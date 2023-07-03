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
nif_list_zones(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	zoneid_t *zids = NULL;
	uint_t nzids, i;
	ERL_NIF_TERM *tzids = NULL;
	ERL_NIF_TERM ret;

	if (zone_list(NULL, &nzids) != 0)
		return (enif_make_errno(env, "zone_list", errno));

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
nif_get_zoneinfo(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	ErlNifBinary bin;
	ERL_NIF_TERM ret;
	ERL_NIF_TERM mapk[5];
	ERL_NIF_TERM mapv[5];
	ERL_NIF_TERM map;
	uint i = 0;
	int rc;
	char strbuf[PATH_MAX];
	zoneid_t zid;
	zone_status_t st;

	if (argc != 1)
		return (enif_make_badarg(env));

	if (!enif_get_int(env, argv[0], &zid)) {
		if (!enif_inspect_iolist_as_binary(env, argv[0], &bin))
			return (enif_make_err(env, "bad_zone"));

		bcopy(bin.data, strbuf, bin.size);
		strbuf[bin.size] = '\0';

		zid = getzoneidbyname(strbuf);
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

static int
load_cb(ErlNifEnv *env, void **priv_data, ERL_NIF_TERM load_info)
{
	return (0);
}

static void
unload_cb(ErlNifEnv *env, void *priv_data)
{
}

static ErlNifFunc nif_funcs[] =
{
	{ "get", 1,	nif_get_zoneinfo },
	{ "list", 0,	nif_list_zones }
};

ERL_NIF_INIT(zka_zoneinfo, nif_funcs, load_cb, NULL, NULL, unload_cb)
