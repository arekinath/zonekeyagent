/*
%%
%% Copyright 2023 The University of Queensland
%% Author: Alex Wilson <alex@uq.edu.au>
%%
%% Redistribution and use in source and binary forms, with or without
%% modification, are permitted provided that the following conditions
%% are met:
%% 1. Redistributions of source code must retain the above copyright
%%    notice, this list of conditions and the following disclaimer.
%% 2. Redistributions in binary form must reproduce the above copyright
%%    notice, this list of conditions and the following disclaimer in the
%%    documentation and/or other materials provided with the distribution.
%%
%% THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
%% IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
%% OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
%% IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT,
%% INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
%% NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
%% DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
%% THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
%% (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
%% THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
%%
*/

#include <stdint.h>
#include <assert.h>
#include <stdlib.h>
#include <strings.h>
#include <string.h>
#include <errno.h>
#include <sched.h>
#include <limits.h>

#include <sys/types.h>
#include <zone.h>
#include <sys/zone.h>

#include "erl_nif.h"

enum zka_job_type {
	ZKA_JOB_LIST,
	ZKA_JOB_INFO,
	ZKA_JOB_STOP
};

struct zka_info_job {
	char			 zij_zonename[PATH_MAX];
	zoneid_t		 zij_zid;
};

struct zka_job {
	struct zka_job		*zj_next;
	enum zka_job_type	 zj_type;
	ErlNifEnv		*zj_env;
	ErlNifPid		 zj_pid;
	ERL_NIF_TERM		 zj_ref;
	union {
		struct zka_info_job	zj_info;
	};
};

struct zka_global {
	ErlNifTid	 z_thread;
	ErlNifMutex	*z_jobq_mtx;
	ErlNifCond	*z_jobq_nonempty;
	struct zka_job	*z_jobq_head;
	struct zka_job	*z_jobq_tail;
};

static ERL_NIF_TERM
enif_make_errno(ErlNifEnv *env, const char *func, int eno)
{
	char buf[256];
	strerror_r(eno, buf, sizeof (buf));
	return (enif_make_tuple2(env, enif_make_atom(env, "error"),
	    enif_make_tuple3(env, enif_make_atom(env, func),
	    enif_make_uint(env, eno),
	    enif_make_string(env, buf, ERL_NIF_LATIN1))));
}

static ERL_NIF_TERM
enif_make_err(ErlNifEnv *env, const char *msg)
{
	return (enif_make_tuple2(env, enif_make_atom(env, "error"),
	    enif_make_atom(env, msg)));
}

static ERL_NIF_TERM
enif_make_binstr(ErlNifEnv *env, const char *cstr)
{
	ErlNifBinary bin;
	enif_alloc_binary(strlen(cstr), &bin);
	bcopy(cstr, bin.data, bin.size);
	return (enif_make_binary(env, &bin));
}

static ERL_NIF_TERM
zka_job_list(struct zka_job *job)
{
	zoneid_t *zids = NULL;
	uint_t nzids, i;
	ERL_NIF_TERM *tzids = NULL;
	ERL_NIF_TERM ret;
	ErlNifEnv *env;

	env = job->zj_env;

	if (zone_list(NULL, &nzids) != 0) {
		return (enif_make_errno(env, "zone_list", errno));
	}

	nzids *= 2;
	zids = calloc(nzids, sizeof (zoneid_t));
	tzids = calloc(nzids, sizeof (ERL_NIF_TERM));

	if (zone_list(zids, &nzids) != 0) {
		free(zids);
		free(tzids);
		return (enif_make_errno(env, "zone_list", errno));
	}

	for (i = 0; i < nzids; ++i)
		tzids[i] = enif_make_int(env, zids[i]);

	ret = enif_make_list_from_array(env, tzids, nzids);

	free(tzids);
	free(zids);

	return (enif_make_tuple2(env, enif_make_atom(env, "ok"), ret));
}

static ERL_NIF_TERM
zka_job_info(struct zka_job *job)
{
	ERL_NIF_TERM mapk[5];
	ERL_NIF_TERM mapv[5];
	ERL_NIF_TERM map;
	ERL_NIF_TERM ret;
	uint i = 0;
	int rc;
	char strbuf[PATH_MAX];
	zoneid_t zid;
	zone_status_t st;
	ErlNifEnv *env;

	env = job->zj_env;

	zid = job->zj_info.zij_zid;
	if (job->zj_info.zij_zonename[0] != '\0') {
		zid = getzoneidbyname(job->zj_info.zij_zonename);
		if (zid == -1 && errno == EINVAL)
			return (enif_make_err(env, "not_found"));
		if (zid == -1)
			return (enif_make_errno(env, "getzoneidbyname", errno));
	}

	mapk[i] = enif_make_atom(env, "id");
	mapv[i++] = enif_make_uint(env, (uint)zid);

	if (zone_getattr(zid, ZONE_ATTR_NAME, strbuf, sizeof (strbuf)) < 0)
		return (enif_make_errno(env, "zone_getattr", errno));
	mapk[i] = enif_make_atom(env, "name");
	mapv[i++] = enif_make_binstr(env, strbuf);

	if (zone_getattr(zid, ZONE_ATTR_ROOT, strbuf, sizeof (strbuf)) < 0)
		return (enif_make_errno(env, "zone_getattr", errno));
	mapk[i] = enif_make_atom(env, "root");
	mapv[i++] = enif_make_binstr(env, strbuf);

	if (zone_getattr(zid, ZONE_ATTR_BRAND, strbuf, sizeof (strbuf)) < 0)
		return (enif_make_errno(env, "zone_getattr", errno));
	mapk[i] = enif_make_atom(env, "brand");
	mapv[i++] = enif_make_binstr(env, strbuf);

	if (zone_getattr(zid, ZONE_ATTR_STATUS, &st, sizeof (st)) < 0)
		return (enif_make_errno(env, "zone_getattr", errno));
	mapk[i] = enif_make_atom(env, "status");
	switch (st) {
	case ZONE_IS_UNINITIALIZED:
		mapv[i++] = enif_make_atom(env, "uninitialized");
		break;
	case ZONE_IS_INITIALIZED:
		mapv[i++] = enif_make_atom(env, "initialized");
		break;
	case ZONE_IS_READY:
		mapv[i++] = enif_make_atom(env, "ready");
		break;
	case ZONE_IS_BOOTING:
		mapv[i++] = enif_make_atom(env, "booting");
		break;
	case ZONE_IS_RUNNING:
		mapv[i++] = enif_make_atom(env, "running");
		break;
	case ZONE_IS_SHUTTING_DOWN:
		mapv[i++] = enif_make_atom(env, "shutting_down");
		break;
	case ZONE_IS_EMPTY:
		mapv[i++] = enif_make_atom(env, "empty");
		break;
	case ZONE_IS_DOWN:
		mapv[i++] = enif_make_atom(env, "down");
		break;
	case ZONE_IS_DYING:
		mapv[i++] = enif_make_atom(env, "dying");
		break;
	case ZONE_IS_DEAD:
		mapv[i++] = enif_make_atom(env, "dead");
		break;
	default:
		mapv[i++] = enif_make_atom(env, "unknown");
		break;
	}

	rc = enif_make_map_from_arrays(env, mapk, mapv, i, &map);
	if (!rc)
		return (enif_make_err(env, "map_make_failed"));
	ret = enif_make_tuple2(env, enif_make_atom(env, "ok"), map);

	return (ret);
}

static void *
zka_job_thread(void *arg)
{
	struct zka_global *g = arg;
	struct zka_job *job;
	ERL_NIF_TERM ret;

	enif_mutex_lock(g->z_jobq_mtx);

	while (1) {
		while (g->z_jobq_head == NULL)
			enif_cond_wait(g->z_jobq_nonempty, g->z_jobq_mtx);
		job = g->z_jobq_head;
		g->z_jobq_head = job->zj_next;
		if (job->zj_next == NULL)
			g->z_jobq_tail = NULL;

		enif_mutex_unlock(g->z_jobq_mtx);

		if (job->zj_type == ZKA_JOB_STOP)
			return (NULL);

		switch (job->zj_type) {
		case ZKA_JOB_LIST:
			ret = zka_job_list(job);
			break;
		case ZKA_JOB_INFO:
			ret = zka_job_info(job);
			break;
		default:
			ret = enif_make_tuple2(job->zj_env,
			    enif_make_atom(job->zj_env, "error"),
			    enif_make_atom(job->zj_env, "bad_nif_job_type"));
		}

		ret = enif_make_tuple2(job->zj_env, job->zj_ref, ret);
		enif_send(NULL, &job->zj_pid, job->zj_env, ret);
		enif_free_env(job->zj_env);
		free(job);

		enif_mutex_lock(g->z_jobq_mtx);
	}

	return (NULL);
}

static struct zka_global *nif_global = NULL;

static void
enqueue_job(struct zka_global *g, struct zka_job *j)
{
	j->zj_next = NULL;
	enif_mutex_lock(g->z_jobq_mtx);
	if (g->z_jobq_tail == NULL)
		g->z_jobq_head = j;
	else
		g->z_jobq_tail->zj_next = j;
	g->z_jobq_tail = j;
	enif_mutex_unlock(g->z_jobq_mtx);
	enif_cond_signal(g->z_jobq_nonempty);
}

static struct zka_job *
make_job(ErlNifEnv *env, enum zka_job_type type)
{
	struct zka_job *j;
	j = calloc(1, sizeof (*j));
	j->zj_type = type;
	j->zj_env = enif_alloc_env();
	enif_self(env, &j->zj_pid);
	j->zj_ref = enif_make_ref(j->zj_env);
	return (j);
}

static ERL_NIF_TERM
nif_list_zones(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	struct zka_job *j;
	ERL_NIF_TERM ref;

	j = make_job(env, ZKA_JOB_LIST);
	ref = enif_make_copy(env, j->zj_ref);

	enqueue_job(nif_global, j);

	return (ref);
}

static ERL_NIF_TERM
nif_get_zoneinfo(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	struct zka_job *j;
	ERL_NIF_TERM ref;
	ErlNifBinary bin;

	if (argc != 1)
		return (enif_make_badarg(env));

	j = make_job(env, ZKA_JOB_INFO);

	if (!enif_get_int(env, argv[0], &j->zj_info.zij_zid)) {
		if (!enif_inspect_iolist_as_binary(env, argv[0], &bin) ||
		    (bin.size + 1) >= sizeof (j->zj_info.zij_zonename)) {
			enif_free_env(j->zj_env);
			free(j);
			return (enif_make_badarg(env));
		}
		memcpy(j->zj_info.zij_zonename, bin.data, bin.size);
		j->zj_info.zij_zonename[bin.size] = '\0';
	}

	ref = enif_make_copy(env, j->zj_ref);

	enqueue_job(nif_global, j);

	return (ref);
}

static int
load_cb(ErlNifEnv *env, void **priv_data, ERL_NIF_TERM load_info)
{
	nif_global = calloc(1, sizeof (*nif_global));
	nif_global->z_jobq_mtx = enif_mutex_create("zoneinfo_jobq");
	nif_global->z_jobq_nonempty = enif_cond_create("zi_jobq_nonempty");

	enif_thread_create("zoneinfo_jobs", &nif_global->z_thread,
	    zka_job_thread, nif_global, NULL);

	return (0);
}

static void
unload_cb(ErlNifEnv *env, void *priv_data)
{
	struct zka_job j;

	bzero(&j, sizeof (j));
	j.zj_type = ZKA_JOB_STOP;
	enqueue_job(nif_global, &j);

	enif_thread_join(nif_global->z_thread, NULL);

	enif_mutex_destroy(nif_global->z_jobq_mtx);
	enif_cond_destroy(nif_global->z_jobq_nonempty);

	free(nif_global);
	nif_global = NULL;
}

static ErlNifFunc nif_funcs[] =
{
	{ "get_async", 1,	nif_get_zoneinfo },
	{ "list_async", 0,	nif_list_zones }
};

ERL_NIF_INIT(zka_zoneinfo, nif_funcs, load_cb, NULL, NULL, unload_cb)
