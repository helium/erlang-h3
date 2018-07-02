/*
 * Copyright 2018 Helium Systems Inc. All Rights Reserved.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *         http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

#include "erl_nif.h"
#include "h3api.h"
#include <stdbool.h>
#include <string.h>

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
    if (!enif_get_int(env, argv[0], &res))
    {
        return enif_make_badarg(env);
    }
    if (res < 0 || res > 15)
    {
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
    if (!enif_get_int(env, argv[0], &res))
    {
        return enif_make_badarg(env);
    }
    if (res < 0 || res > 15)
    {
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
    if (!enif_get_int(env, argv[0], &res))
    {
        return enif_make_badarg(env);
    }
    if (res < 0 || res > 15)
    {
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
    if (!enif_get_double(env, argv[0], &res))
    {
        return enif_make_badarg(env);
    }

    double result = degsToRads(res);
    return enif_make_double(env, result);
}

static ERL_NIF_TERM
erl_rads_to_degs(ErlNifEnv * env, int argc, const ERL_NIF_TERM argv[])
{
    double res;
    if (!enif_get_double(env, argv[0], &res))
    {
        return enif_make_badarg(env);
    }

    double result = radsToDegs(res);
    return enif_make_double(env, result);
}

static ERL_NIF_TERM
erl_hex_area_km2(ErlNifEnv * env, int argc, const ERL_NIF_TERM argv[])
{
    int res;
    if (!enif_get_int(env, argv[0], &res))
    {
        return enif_make_badarg(env);
    }

    double result = hexAreaKm2(res);
    return enif_make_double(env, result);
}

static ERL_NIF_TERM
erl_hex_area_m2(ErlNifEnv * env, int argc, const ERL_NIF_TERM argv[])
{
    int res;
    if (!enif_get_int(env, argv[0], &res))
    {
        return enif_make_badarg(env);
    }

    double result = hexAreaM2(res);
    return enif_make_double(env, result);
}

static ERL_NIF_TERM
erl_geo_to_h3(ErlNifEnv * env, int argc, const ERL_NIF_TERM argv[])
{
    const ERL_NIF_TERM * geo;
    int                  arity;
    if (!enif_is_tuple(env, argv[0])
        || !enif_get_tuple(env, argv[0], &arity, &geo) || arity != 2)
    {
        return enif_make_badarg(env);
    }
    double lat, lon;
    if (!enif_get_double(env, geo[0], &lat)
        || !enif_get_double(env, geo[1], &lon))
    {
        return enif_make_badarg(env);
    }
    int res;
    if (!enif_get_int(env, argv[1], &res))
    {
        return enif_make_badarg(env);
    }

    if (res < 0 || res > 15)
    {
        // invalid resolution
        return enif_make_badarg(env);
    }

    GeoCoord coord;
    coord.lat       = lat;
    coord.lon       = lon;
    uint64_t result = geoToH3(&coord, res);
    return enif_make_uint64(env, result);
}

static ERL_NIF_TERM
erl_h3_to_geo(ErlNifEnv * env, int argc, const ERL_NIF_TERM argv[])
{
    uint64_t h3idx;
    if (!enif_get_uint64(env, argv[0], &h3idx))
    {
        return enif_make_badarg(env);
    }

    if (h3IsValid(h3idx) == 0)
    {
        return enif_make_badarg(env);
    }

    GeoCoord coord;
    h3ToGeo(h3idx, &coord);
    return enif_make_tuple2(env,
                            enif_make_double(env, coord.lat),
                            enif_make_double(env, coord.lon));
}

static ERL_NIF_TERM
erl_h3_to_string(ErlNifEnv * env, int argc, const ERL_NIF_TERM argv[])
{
    uint64_t h3idx;
    if (!enif_get_uint64(env, argv[0], &h3idx))
    {
        return enif_make_badarg(env);
    }

    if (h3IsValid(h3idx) == 0)
    {
        return enif_make_badarg(env);
    }
    char out[17];
    h3ToString(h3idx, out, 17);
    return enif_make_string(env, out, ERL_NIF_LATIN1);
}

static ERL_NIF_TERM
erl_string_to_h3(ErlNifEnv * env, int argc, const ERL_NIF_TERM argv[])
{
    char in[17];
    if (!enif_get_string(env, argv[0], in, 17, ERL_NIF_LATIN1))
    {
        return enif_make_badarg(env);
    }

    uint64_t h3idx = stringToH3(in);

    if (h3IsValid(h3idx) == 0)
    {
        return enif_make_badarg(env);
    }
    return enif_make_uint64(env, h3idx);
}

static ERL_NIF_TERM
erl_get_resolution(ErlNifEnv * env, int argc, const ERL_NIF_TERM argv[])
{
    uint64_t h3idx;
    if (!enif_get_uint64(env, argv[0], &h3idx))
    {
        return enif_make_badarg(env);
    }

    if (h3IsValid(h3idx) == 0)
    {
        return enif_make_badarg(env);
    }

    int result = h3GetResolution(h3idx);
    return enif_make_int(env, result);
}


static ERL_NIF_TERM
erl_get_base_cell(ErlNifEnv * env, int argc, const ERL_NIF_TERM argv[])
{
    uint64_t h3idx;
    if (!enif_get_uint64(env, argv[0], &h3idx))
    {
        return enif_make_badarg(env);
    }

    if (h3IsValid(h3idx) == 0)
    {
        return enif_make_badarg(env);
    }

    int result = h3GetBaseCell(h3idx);
    return enif_make_int(env, result);
}

static ERL_NIF_TERM
erl_is_valid(ErlNifEnv * env, int argc, const ERL_NIF_TERM argv[])
{
    uint64_t h3idx;
    if (!enif_get_uint64(env, argv[0], &h3idx))
    {
        return enif_make_badarg(env);
    }

    if (h3IsValid(h3idx) == 0)
    {
        return mk_atom(env, "false");
    }

    return mk_atom(env, "true");
}


static ERL_NIF_TERM
erl_is_class3(ErlNifEnv * env, int argc, const ERL_NIF_TERM argv[])
{
    uint64_t h3idx;
    if (!enif_get_uint64(env, argv[0], &h3idx))
    {
        return enif_make_badarg(env);
    }

    if (h3IsValid(h3idx) == 0)
    {
        return enif_make_badarg(env);
    }

    if (h3IsResClassIII(h3idx) == 0)
    {
        return mk_atom(env, "false");
    }

    return mk_atom(env, "true");
}


static ERL_NIF_TERM
erl_is_pentagon(ErlNifEnv * env, int argc, const ERL_NIF_TERM argv[])
{
    uint64_t h3idx;
    if (!enif_get_uint64(env, argv[0], &h3idx))
    {
        return enif_make_badarg(env);
    }

    if (h3IsValid(h3idx) == 0)
    {
        return enif_make_badarg(env);
    }

    if (h3IsPentagon(h3idx) == 0)
    {
        return mk_atom(env, "false");
    }

    return mk_atom(env, "true");
}

static ERL_NIF_TERM
erl_parent(ErlNifEnv * env, int argc, const ERL_NIF_TERM argv[])
{
    uint64_t h3idx;
    if (!enif_get_uint64(env, argv[0], &h3idx))
    {
        return enif_make_badarg(env);
    }

    if (h3IsValid(h3idx) == 0)
    {
        return enif_make_badarg(env);
    }

    int res;
    if (!enif_get_int(env, argv[1], &res))
    {
        return enif_make_badarg(env);
    }
    if (res < 0 || res > 15)
    {
        // invalid resolution
        return enif_make_badarg(env);
    }

    if (res >= h3GetResolution(h3idx))
    {
        // asking for a parent with a higher resolution is nonsense
        return enif_make_badarg(env);
    }

    return enif_make_uint64(env, h3ToParent(h3idx, res));
}

static ERL_NIF_TERM
erl_children(ErlNifEnv * env, int argc, const ERL_NIF_TERM argv[])
{
    uint64_t h3idx;
    if (!enif_get_uint64(env, argv[0], &h3idx))
    {
        return enif_make_badarg(env);
    }

    if (h3IsValid(h3idx) == 0)
    {
        return enif_make_badarg(env);
    }

    int res;
    if (!enif_get_int(env, argv[1], &res))
    {
        return enif_make_badarg(env);
    }
    if (res < 0 || res > 15)
    {
        // invalid resolution
        return enif_make_badarg(env);
    }

    if (res <= h3GetResolution(h3idx))
    {
        // asking for children with lower resolution is nonsense
        return enif_make_badarg(env);
    }

    int childcount = maxH3ToChildrenSize(h3idx, res);

    uint64_t * children = calloc(childcount, sizeof(uint64_t));

    if (children == NULL)
    {
        return enif_make_badarg(env);
    }

    h3ToChildren(h3idx, res, children);

    ERL_NIF_TERM list = enif_make_list(env, 0);
    for (int i = childcount - 1; i >= 0; i--)
    {
        list = enif_make_list_cell(env, enif_make_uint64(env, children[i]), list);
    }
    free(children);
    return list;
}

static ERL_NIF_TERM
erl_k_ring(ErlNifEnv * env, int argc, const ERL_NIF_TERM argv[])
{
    uint64_t h3idx;
    if (!enif_get_uint64(env, argv[0], &h3idx))
    {
        return enif_make_badarg(env);
    }

    if (h3IsValid(h3idx) == 0)
    {
        return enif_make_badarg(env);
    }

    int k;
    if (!enif_get_int(env, argv[1], &k))
    {
        return enif_make_badarg(env);
    }
    if (k < 0)
    {
        // invalid k
        return enif_make_badarg(env);
    }

    int kringsize = maxKringSize(k);

    uint64_t * h3indices = calloc(kringsize, sizeof(uint64_t));

    if (h3indices == NULL)
    {
        return enif_make_badarg(env);
    }

    kRing(h3idx, k, h3indices);

    ERL_NIF_TERM list = enif_make_list(env, 0);
    // Output is placed in the provided array in no particular order. Elements
    // of the output array may be left zero, as can happen when crossing a pentagon.
    for (int i = kringsize - 1; i >= 0; i--)
    {
        if (h3indices[i] != 0)
        {
            list = enif_make_list_cell(env,
                                       enif_make_uint64(env, h3indices[i]),
                                       list);
        }
    }
    free(h3indices);
    return list;
}

static ERL_NIF_TERM
erl_max_k_ring_size(ErlNifEnv * env, int argc, const ERL_NIF_TERM argv[])
{
    int k;
    if (!enif_get_int(env, argv[0], &k))
    {
        return enif_make_badarg(env);
    }

    int result = maxKringSize(k);
    return enif_make_int(env, result);
}

static ERL_NIF_TERM
erl_indices_are_neighbors(ErlNifEnv * env, int argc, const ERL_NIF_TERM argv[])
{
    uint64_t h3idx_origin;
    if (!enif_get_uint64(env, argv[0], &h3idx_origin))
    {
        return enif_make_badarg(env);
    }

    uint64_t h3idx_destination;
    if (!enif_get_uint64(env, argv[1], &h3idx_destination))
    {
        return enif_make_badarg(env);
    }

    if (h3IndexesAreNeighbors(h3idx_origin, h3idx_destination) == 0)
    {
        return mk_atom(env, "false");
    }

    return mk_atom(env, "true");
}

static ErlNifFunc nif_funcs[] =
    {{"num_hexagons", 1, erl_num_hexagons, 0},
     {"edge_length_meters", 1, erl_edge_length_meters, 0},
     {"edge_length_kilometers", 1, erl_edge_length_kilometers, 0},
     {"degs_to_rads", 1, erl_degs_to_rads, 0},
     {"rads_to_degs", 1, erl_rads_to_degs, 0},
     {"hex_area_km2", 1, erl_hex_area_km2, 0},
     {"hex_area_m2", 1, erl_hex_area_m2, 0},
     {"from_geo", 2, erl_geo_to_h3, 0},
     {"to_geo", 1, erl_h3_to_geo, 0},
     {"to_string", 1, erl_h3_to_string, 0},
     {"from_string", 1, erl_string_to_h3, 0},
     {"get_resolution", 1, erl_get_resolution, 0},
     {"get_base_cell", 1, erl_get_base_cell, 0},
     {"is_valid", 1, erl_is_valid, 0},
     {"is_class3", 1, erl_is_class3, 0},
     {"is_pentagon", 1, erl_is_pentagon, 0},
     {"parent", 2, erl_parent, 0},
     {"children", 2, erl_children, 0},
     {"k_ring", 2, erl_k_ring, 0},
     {"max_k_ring_size", 1, erl_max_k_ring_size, 0},
     {"indices_are_neighbors", 2, erl_indices_are_neighbors, 0}};

static int
load(ErlNifEnv * env, void ** priv_data, ERL_NIF_TERM load_info)
{
    (void)priv_data;
    (void)load_info;
    return 0;
}

ERL_NIF_INIT(h3, nif_funcs, load, NULL, NULL, NULL);
