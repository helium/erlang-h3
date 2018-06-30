/*
 * Copyright 2018 Helium Systems Inc. All Rights Reserved.
 *
 * Licensed under the OpenSSL license (the "License").  You may not use
 * this file except in compliance with the License.  You can obtain a copy
 * in the file LICENSE in the source distribution or at
 * https://www.openssl.org/source/license.html
 */

#include "erl_nif.h"
#include <string.h>
#include <stdbool.h>
#include "h3api.h"

static ERL_NIF_TERM
mk_atom(ErlNifEnv * env, const char * atom)
{
    ERL_NIF_TERM ret;

    if (!enif_make_existing_atom(env, atom, &ret, ERL_NIF_LATIN1))
    {
        return enif_make_atom(env, atom);
    }

    return ret;
}

static ERL_NIF_TERM
erl_num_hexagons(ErlNifEnv * env, int argc, const ERL_NIF_TERM argv[])
{
    int res;
    if (!enif_get_int(env, argv[0], &res)) {
        return enif_make_badarg(env);
    }
    if (res < 0 || res > 15) {
        // invalid resolution
        return enif_make_badarg(env);
    }

    int64_t result = numHexagons(res);
    return enif_make_int64(env, result);
}

static ERL_NIF_TERM
erl_edge_length_meters(ErlNifEnv * env, int argc, const ERL_NIF_TERM argv[])
{
    int res;
    if (!enif_get_int(env, argv[0], &res)) {
        return enif_make_badarg(env);
    }
    if (res < 0 || res > 15) {
        // invalid resolution
        return enif_make_badarg(env);
    }

    double result = edgeLengthM(res);
    return enif_make_double(env, result);
}

static ERL_NIF_TERM
erl_edge_length_kilometers(ErlNifEnv * env, int argc, const ERL_NIF_TERM argv[])
{
    int res;
    if (!enif_get_int(env, argv[0], &res)) {
        return enif_make_badarg(env);
    }
    if (res < 0 || res > 15) {
        // invalid resolution
        return enif_make_badarg(env);
    }

    double result = edgeLengthKm(res);
    return enif_make_double(env, result);
}

static ERL_NIF_TERM
erl_degs_to_rads(ErlNifEnv * env, int argc, const ERL_NIF_TERM argv[])
{
    double res;
    if (!enif_get_double(env, argv[0], &res)) {
        return enif_make_badarg(env);
    }

    double result = degsToRads(res);
    return enif_make_double(env, result);
}

static ERL_NIF_TERM
erl_rads_to_degs(ErlNifEnv * env, int argc, const ERL_NIF_TERM argv[])
{
    double res;
    if (!enif_get_double(env, argv[0], &res)) {
        return enif_make_badarg(env);
    }

    double result = radsToDegs(res);
    return enif_make_double(env, result);
}

static ERL_NIF_TERM
erl_max_k_ring_size(ErlNifEnv * env, int argc, const ERL_NIF_TERM argv[])
{
    int res;
    if (!enif_get_int(env, argv[0], &res)) {
        return enif_make_badarg(env);
    }

    int result = maxKringSize(res);
    return enif_make_int(env, result);
}

static ERL_NIF_TERM
erl_hex_area_km2(ErlNifEnv * env, int argc, const ERL_NIF_TERM argv[])
{
    int res;
    if (!enif_get_int(env, argv[0], &res)) {
        return enif_make_badarg(env);
    }

    double result = hexAreaKm2(res);
    return enif_make_double(env, result);
}

static ERL_NIF_TERM
erl_hex_area_m2(ErlNifEnv * env, int argc, const ERL_NIF_TERM argv[])
{
    int res;
    if (!enif_get_int(env, argv[0], &res)) {
        return enif_make_badarg(env);
    }

    double result = hexAreaM2(res);
    return enif_make_double(env, result);
}

static ERL_NIF_TERM
erl_geo_to_h3(ErlNifEnv * env, int argc, const ERL_NIF_TERM argv[])
{
    const ERL_NIF_TERM *geo;
    int arity;
    if (!enif_is_tuple(env, argv[0]) || !enif_get_tuple(env, argv[0], &arity, &geo) || arity != 2) {
        return enif_make_badarg(env);
    }
    double lat, lon;
    if (!enif_get_double(env, geo[0], &lat) || !enif_get_double(env, geo[1], &lon)) {
        return enif_make_badarg(env);
    }
    int res;
    if (!enif_get_int(env, argv[1], &res)) {
        return enif_make_badarg(env);
    }

    if (res < 0 || res > 15) {
        // invalid resolution
        return enif_make_badarg(env);
    }

    GeoCoord coord;
    coord.lat = lat;
    coord.lon = lon;
    uint64_t result = geoToH3(&coord, res);
    return enif_make_uint64(env, result);
}

static ERL_NIF_TERM
erl_h3_to_geo(ErlNifEnv * env, int argc, const ERL_NIF_TERM argv[])
{
    uint64_t h3idx;
    if (!enif_get_uint64(env, argv[0], &h3idx)) {
        return enif_make_badarg(env);
    }

    if (h3IsValid(h3idx) == 0) {
        return enif_make_badarg(env);
    }

    GeoCoord coord;
    h3ToGeo(h3idx, &coord);
    return enif_make_tuple2(env, enif_make_double(env, coord.lat), enif_make_double(env, coord.lon));
}

static ERL_NIF_TERM
erl_h3_to_string(ErlNifEnv * env, int argc, const ERL_NIF_TERM argv[])
{
    uint64_t h3idx;
    if (!enif_get_uint64(env, argv[0], &h3idx)) {
        return enif_make_badarg(env);
    }

    if (h3IsValid(h3idx) == 0) {
        return enif_make_badarg(env);
    }
    char out[17];
    h3ToString(h3idx, out, 17);
    return enif_make_string(env, out, ERL_NIF_LATIN1);
}



static ErlNifFunc nif_funcs[] = {
    {"num_hexagons", 1, erl_num_hexagons, 0},
    {"edge_length_meters", 1, erl_edge_length_meters, 0},
    {"edge_length_kilometers", 1, erl_edge_length_kilometers, 0},
    {"degs_to_rads", 1, erl_degs_to_rads, 0},
    {"rads_to_degs", 1, erl_rads_to_degs, 0},
    {"max_k_ring_size", 1, erl_max_k_ring_size, 0},
    {"hex_area_km2", 1, erl_hex_area_km2, 0},
    {"hex_area_m2", 1, erl_hex_area_m2, 0},
    {"geo_to_h3", 2, erl_geo_to_h3, 0},
    {"h3_to_geo", 1, erl_h3_to_geo, 0},
    {"h3_to_string", 1, erl_h3_to_string, 0}
    };

static int
load(ErlNifEnv * env, void ** priv_data, ERL_NIF_TERM load_info)
{
    (void)priv_data;
    (void)load_info;
    return 0;
}

ERL_NIF_INIT(h3, nif_funcs, load, NULL, NULL, NULL);
