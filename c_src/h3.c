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

static ERL_NIF_TERM ATOM_TRUE;
static ERL_NIF_TERM ATOM_FALSE;


static bool
get_h3idx(ErlNifEnv * env, ERL_NIF_TERM term, H3Index * dest)
{
    return enif_get_uint64(env, term, (unsigned long *)dest) && h3IsValid(*dest);
}

static ERL_NIF_TERM
make_h3idx(ErlNifEnv * env, H3Index index)
{
    return enif_make_uint64(env, index);
}

static bool
get_resolution(ErlNifEnv * env, ERL_NIF_TERM term, int * dest)
{
    return enif_get_int(env, term, dest) && *dest >= 0 && *dest <= 15;
}

static bool
get_geo_coord(ErlNifEnv * env, ERL_NIF_TERM term, GeoCoord * dest)
{
    const ERL_NIF_TERM * geo;
    int                  arity;
    if (!enif_is_tuple(env, term) || !enif_get_tuple(env, term, &arity, &geo)
        || arity != 2)
    {
        return false;
    }
    if (!enif_get_double(env, geo[0], &dest->lat)
        || !enif_get_double(env, geo[1], &dest->lon))
    {
        return false;
    }

    return true;
}

static ERL_NIF_TERM
make_geo_coord(ErlNifEnv * env, GeoCoord * coord)
{
    return enif_make_tuple2(env,
                            enif_make_double(env, coord->lat),
                            enif_make_double(env, coord->lon));
}

static bool
get_k(ErlNifEnv * env, ERL_NIF_TERM term, int * dest)
{
    return enif_get_int(env, term, dest) && *dest >= 0;
}


static ERL_NIF_TERM
erl_num_hexagons(ErlNifEnv * env, int argc, const ERL_NIF_TERM argv[])
{
    int res;
    if (!get_resolution(env, argv[0], &res))
    {
        return enif_make_badarg(env);
    }

    int64_t result = numHexagons(res);
    return enif_make_int64(env, result);
}

static ERL_NIF_TERM
erl_edge_length_meters(ErlNifEnv * env, int argc, const ERL_NIF_TERM argv[])
{
    int res;
    if (!get_resolution(env, argv[0], &res))
    {
        return enif_make_badarg(env);
    }

    double result = edgeLengthM(res);
    return enif_make_double(env, result);
}

static ERL_NIF_TERM
erl_edge_length_kilometers(ErlNifEnv * env, int argc, const ERL_NIF_TERM argv[])
{
    int res;
    if (!get_resolution(env, argv[0], &res))
    {
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
    double radians;
    if (!enif_get_double(env, argv[0], &radians))
    {
        return enif_make_badarg(env);
    }

    double result = radsToDegs(radians);
    return enif_make_double(env, result);
}

static ERL_NIF_TERM
erl_hex_area_km2(ErlNifEnv * env, int argc, const ERL_NIF_TERM argv[])
{
    int res;
    if (!get_resolution(env, argv[0], &res))
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
    if (!get_resolution(env, argv[0], &res))
    {
        return enif_make_badarg(env);
    }

    double result = hexAreaM2(res);
    return enif_make_double(env, result);
}

static ERL_NIF_TERM
erl_geo_to_h3(ErlNifEnv * env, int argc, const ERL_NIF_TERM argv[])
{
    GeoCoord coord;
    if (!get_geo_coord(env, argv[0], &coord))
    {
        return enif_make_badarg(env);
    }
    int res;
    if (!get_resolution(env, argv[1], &res))
    {
        return enif_make_badarg(env);
    }

    H3Index result = geoToH3(&coord, res);
    return make_h3idx(env, result);
}

static ERL_NIF_TERM
erl_h3_to_geo(ErlNifEnv * env, int argc, const ERL_NIF_TERM argv[])
{
    H3Index h3idx;
    if (!get_h3idx(env, argv[0], &h3idx))
    {
        return enif_make_badarg(env);
    }

    GeoCoord coord;
    h3ToGeo(h3idx, &coord);
    return make_geo_coord(env, &coord);
}

static ERL_NIF_TERM
erl_h3_to_geo_boundary(ErlNifEnv * env, int argc, const ERL_NIF_TERM argv[])
{
    H3Index h3idx;
    if (!get_h3idx(env, argv[0], &h3idx))
    {
        return enif_make_badarg(env);
    }

    GeoBoundary boundary;
    h3ToGeoBoundary(h3idx, &boundary);

    ERL_NIF_TERM verts[MAX_CELL_BNDRY_VERTS];
    for (int i = 0; i < boundary.numVerts; i++)
    {
        verts[i] = make_geo_coord(env, &boundary.verts[i]);
    }

    return enif_make_list_from_array(env, verts, boundary.numVerts);
}

static ERL_NIF_TERM
erl_h3_to_string(ErlNifEnv * env, int argc, const ERL_NIF_TERM argv[])
{
    H3Index h3idx;
    if (!get_h3idx(env, argv[0], &h3idx))
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

    H3Index h3idx = stringToH3(in);

    if (!h3IsValid(h3idx))
    {
        return enif_make_badarg(env);
    }
    return make_h3idx(env, h3idx);
}

static ERL_NIF_TERM
erl_get_resolution(ErlNifEnv * env, int argc, const ERL_NIF_TERM argv[])
{
    H3Index h3idx;
    if (!get_h3idx(env, argv[0], &h3idx))
    {
        return enif_make_badarg(env);
    }

    int result = h3GetResolution(h3idx);
    return enif_make_int(env, result);
}


static ERL_NIF_TERM
erl_get_base_cell(ErlNifEnv * env, int argc, const ERL_NIF_TERM argv[])
{
    H3Index h3idx;
    if (!get_h3idx(env, argv[0], &h3idx))
    {
        return enif_make_badarg(env);
    }

    int result = h3GetBaseCell(h3idx);
    return enif_make_int(env, result);
}

static ERL_NIF_TERM
erl_is_valid(ErlNifEnv * env, int argc, const ERL_NIF_TERM argv[])
{
    H3Index h3idx;
    return get_h3idx(env, argv[0], &h3idx) ? ATOM_TRUE : ATOM_FALSE;
}


static ERL_NIF_TERM
erl_is_class3(ErlNifEnv * env, int argc, const ERL_NIF_TERM argv[])
{
    H3Index h3idx;
    if (!get_h3idx(env, argv[0], &h3idx))
    {
        return enif_make_badarg(env);
    }

    return h3IsResClassIII(h3idx) ? ATOM_TRUE : ATOM_FALSE;
}


static ERL_NIF_TERM
erl_is_pentagon(ErlNifEnv * env, int argc, const ERL_NIF_TERM argv[])
{
    H3Index h3idx;
    if (!get_h3idx(env, argv[0], &h3idx))
    {
        return enif_make_badarg(env);
    }

    return h3IsPentagon(h3idx) ? ATOM_TRUE : ATOM_FALSE;
}

static ERL_NIF_TERM
erl_parent(ErlNifEnv * env, int argc, const ERL_NIF_TERM argv[])
{
    H3Index h3idx;
    if (!get_h3idx(env, argv[0], &h3idx))
    {
        return enif_make_badarg(env);
    }

    int res;
    if (!get_resolution(env, argv[1], &res))
    {
        return enif_make_badarg(env);
    }

    if (res >= h3GetResolution(h3idx))
    {
        // asking for a parent with a higher resolution is nonsense
        return enif_make_badarg(env);
    }

    return make_h3idx(env, h3ToParent(h3idx, res));
}

static ERL_NIF_TERM
erl_children(ErlNifEnv * env, int argc, const ERL_NIF_TERM argv[])
{
    H3Index h3idx;
    if (!get_h3idx(env, argv[0], &h3idx))
    {
        return enif_make_badarg(env);
    }

    int res;
    if (!get_resolution(env, argv[1], &res))
    {
        return enif_make_badarg(env);
    }

    if (res <= h3GetResolution(h3idx))
    {
        // asking for children with lower resolution is nonsense
        return enif_make_badarg(env);
    }

    int childcount = maxH3ToChildrenSize(h3idx, res);

    H3Index * children = calloc(childcount, sizeof(H3Index));

    if (children == NULL)
    {
        return enif_make_badarg(env);
    }

    h3ToChildren(h3idx, res, children);

    ERL_NIF_TERM list = enif_make_list(env, 0);
    for (int i = childcount - 1; i >= 0; i--)
    {
        list = enif_make_list_cell(env, make_h3idx(env, children[i]), list);
    }
    free(children);
    return list;
}

static ERL_NIF_TERM
erl_k_ring(ErlNifEnv * env, int argc, const ERL_NIF_TERM argv[])
{
    H3Index h3idx;
    if (!get_h3idx(env, argv[0], &h3idx))
    {
        return enif_make_badarg(env);
    }

    int k;
    if (!get_k(env, argv[1], &k))
    {
        return enif_make_badarg(env);
    }

    int       kringsize = maxKringSize(k);
    H3Index * h3indices = calloc(kringsize, sizeof(H3Index));

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
            list = enif_make_list_cell(env, make_h3idx(env, h3indices[i]), list);
        }
    }

    free(h3indices);
    return list;
}

static ERL_NIF_TERM
erl_k_ring_distances(ErlNifEnv * env, int argc, const ERL_NIF_TERM argv[])
{
    H3Index h3idx;
    if (!get_h3idx(env, argv[0], &h3idx))
    {
        return enif_make_badarg(env);
    }

    int k;
    if (!get_k(env, argv[1], &k))
    {
        return enif_make_badarg(env);
    }

    int       kringsize = maxKringSize(k);
    H3Index * h3indices = calloc(kringsize, sizeof(H3Index));

    if (h3indices == NULL)
    {
        return enif_make_badarg(env);
    }

    int * h3distances = calloc(kringsize, sizeof(int));

    if (h3distances == NULL)
    {
        free(h3indices);
        return enif_make_badarg(env);
    }

    kRingDistances(h3idx, k, h3indices, h3distances);

    ERL_NIF_TERM list = enif_make_list(env, 0);
    // Output is placed in the provided array in no particular order. Elements
    // of the output array may be left zero, as can happen when crossing a pentagon.
    for (int i = kringsize - 1; i >= 0; i--)
    {
        if (h3indices[i] != 0)
        {
            ERL_NIF_TERM entry =
                enif_make_tuple2(env,
                                 make_h3idx(env, h3indices[i]),
                                 enif_make_int(env, h3distances[i]));
            list = enif_make_list_cell(env, entry, list);
        }
    }

    free(h3indices);
    free(h3distances);
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

static bool
get_h3indexes(ErlNifEnv * env, ERL_NIF_TERM term, H3Index * dest, unsigned dest_len)
{
    unsigned len;
    if (!enif_get_list_length(env, term, &len) || len > dest_len)
    {
        return false;
    }

    ERL_NIF_TERM head;
    while (enif_get_list_cell(env, term, &head, &term))
    {
        if (!get_h3idx(env, head, dest))
        {
            return false;
        }
        dest += 1;
    }

    return true;
}

static ERL_NIF_TERM
make_h3indexes(ErlNifEnv * env, H3Index * src, unsigned src_len)
{
    ERL_NIF_TERM list = enif_make_list(env, 0);
    for (int i = 0; i < src_len; i++)
    {
        if (src[i] != 0)
        {
            ERL_NIF_TERM entry = make_h3idx(env, src[i]);
            list               = enif_make_list_cell(env, entry, list);
        }
    }

    return list;
}

static ERL_NIF_TERM
erl_compact(ErlNifEnv * env, int argc, const ERL_NIF_TERM argv[])
{
    unsigned len;
    if (!enif_get_list_length(env, argv[0], &len))
    {
        return enif_make_badarg(env);
    }

    H3Index * in_indices = calloc(len, sizeof(H3Index));
    if (in_indices == NULL)
    {
        return enif_make_badarg(env);
    }

    if (!get_h3indexes(env, argv[0], in_indices, len))
    {
        free(in_indices);
        return enif_make_badarg(env);
    }

    H3Index * out_indices = calloc(len, sizeof(H3Index));
    if (out_indices == NULL)
    {
        free(in_indices);
        return enif_make_badarg(env);
    }


    if (!compact(in_indices, out_indices, len))
    {
        free(in_indices);
        free(out_indices);
        return enif_make_badarg(env);
    }

    // Done with in_indices
    free(in_indices);

    ERL_NIF_TERM list = make_h3indexes(env, out_indices, len);

    free(out_indices);
    return list;
}

static ERL_NIF_TERM
erl_uncompact(ErlNifEnv * env, int argc, const ERL_NIF_TERM argv[])
{
    unsigned in_len;
    if (!enif_get_list_length(env, argv[0], &in_len))
    {
        return enif_make_badarg(env);
    }

    int res;
    if (!get_resolution(env, argv[1], &res))
    {
        return enif_make_badarg(env);
    }

    H3Index * in_indices = calloc(in_len, sizeof(H3Index));
    if (in_indices == NULL)
    {
        return enif_make_badarg(env);
    }

    if (!get_h3indexes(env, argv[0], in_indices, in_len))
    {
        free(in_indices);
        return enif_make_badarg(env);
    }

    unsigned  out_len     = maxUncompactSize(in_indices, in_len, res);
    H3Index * out_indices = calloc(out_len, sizeof(H3Index));
    if (out_indices == NULL)
    {
        return enif_make_badarg(env);
    }

    if (!uncompact(in_indices, in_len, out_indices, out_len, res))
    {
        free(in_indices);
        free(out_indices);
        return enif_make_badarg(env);
    }

    // Done with in_indices
    free(in_indices);

    ERL_NIF_TERM list = make_h3indexes(env, out_indices, out_len);

    free(out_indices);
    return list;
}


static ERL_NIF_TERM
erl_indices_are_neighbors(ErlNifEnv * env, int argc, const ERL_NIF_TERM argv[])
{
    H3Index h3idx_origin;
    if (!get_h3idx(env, argv[0], &h3idx_origin))
    {
        return enif_make_badarg(env);
    }

    H3Index h3idx_destination;
    if (!get_h3idx(env, argv[1], &h3idx_destination))
    {
        return enif_make_badarg(env);
    }

    return h3IndexesAreNeighbors(h3idx_origin, h3idx_destination) ? ATOM_TRUE
                                                                  : ATOM_FALSE;
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
     {"to_geo_boundary", 1, erl_h3_to_geo_boundary, 0},
     {"to_string", 1, erl_h3_to_string, 0},
     {"from_string", 1, erl_string_to_h3, 0},
     {"get_resolution", 1, erl_get_resolution, 0},
     {"get_base_cell", 1, erl_get_base_cell, 0},
     {"is_valid", 1, erl_is_valid, 0},
     {"is_class3", 1, erl_is_class3, 0},
     {"is_pentagon", 1, erl_is_pentagon, 0},
     {"parent", 2, erl_parent, 0},
     {"children", 2, erl_children, 0},
     {"compact", 1, erl_compact, 0},
     {"uncompact", 2, erl_uncompact, 0},
     {"k_ring", 2, erl_k_ring, 0},
     {"k_ring_distances", 2, erl_k_ring_distances, 0},
     {"max_k_ring_size", 1, erl_max_k_ring_size, 0},
     {"indices_are_neighbors", 2, erl_indices_are_neighbors, 0}};

#define ATOM(Id, Value)                                                        \
    {                                                                          \
        Id = enif_make_atom(env, Value);                                       \
    }

static int
load(ErlNifEnv * env, void ** priv_data, ERL_NIF_TERM load_info)
{
    (void)priv_data;
    (void)load_info;

    ATOM(ATOM_TRUE, "true");
    ATOM(ATOM_FALSE, "false");

    return 0;
}

ERL_NIF_INIT(h3, nif_funcs, load, NULL, NULL, NULL);
