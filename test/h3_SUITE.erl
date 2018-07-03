-module(h3_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([all/0, init_per_testcase/2, end_per_testcase/2]).
-export([
         degs_to_rads_test/1,
         rads_to_degs_test/1,
         validity_test/1,
         d2r_r2d_roundtrip_test/1,
         r2d_d2r_roundtrip_test/1,
         max_k_ring_size_test/1,
         geo_to_h3_test/1,
         h3_to_geo_test/1,
         h3_to_geo_boundary_test/1,
         hex_area_km2_decreasing_test/1,
         self_not_a_neighbor_test/1,
         h3_of_geo_coord_test/1,
         k_ring_origin_index_test/1,
         k_ring_distance_origin_test/1,
         k_ring_distance_test/1,
         compact_roundtrip_test/1
        ]).

all() ->
    [
     degs_to_rads_test,
     rads_to_degs_test,
     validity_test,
     d2r_r2d_roundtrip_test,
     r2d_d2r_roundtrip_test,
     max_k_ring_size_test,
     geo_to_h3_test,
     h3_to_geo_test,
     hex_area_km2_decreasing_test,
     self_not_a_neighbor_test,
     h3_of_geo_coord_test,
     k_ring_origin_index_test,
     k_ring_distance_origin_test,
     k_ring_distance_test
    ].

init_per_testcase(_, Config) ->
    Resolutions = lists:seq(0, 15),
    ParisIndex = h3:from_geo({h3:degs_to_rads(37.3615593), h3:degs_to_rads(-122.0553238)}, 7),
    SunnyvaleIndex = h3:from_string("89283470c27ffff"),
    [{resolutions, Resolutions},
     {paris_index, ParisIndex},
     {sunnyvale_index, SunnyvaleIndex}
     | Config].

end_per_testcase(_, _Config) ->
    ok.

degs_to_rads_test(_Config) ->
    0.017453292519943295 = h3:degs_to_rads(1.0),
    ok.

rads_to_degs_test(_Config) ->
    57.29577951308232 = h3:rads_to_degs(1.0),
    ok.

validity_test(_Config) ->
    H3 = h3:from_string("845ad1bffffffff"),
    596072861467148287 = H3,
    true = h3:is_valid(H3),
    ok.

d2r_r2d_roundtrip_test(_Config) ->
    40.7128 = h3:rads_to_degs(h3:degs_to_rads(40.7128)),
    ok.

r2d_d2r_roundtrip_test(_Config) ->
    0.7105724077 = h3:degs_to_rads(h3:rads_to_degs(0.7105724077)),
    ok.

max_k_ring_size_test(_Config) ->
    true = h3:max_k_ring_size(3) > 0,
    ok.

geo_to_h3_test(_Config) ->
    Res = h3:from_geo({h3:degs_to_rads(40.689167), h3:degs_to_rads(-74.044444)}, 5),
    "852a1073fffffff" = h3:to_string(Res),
    ok.

h3_to_geo_test(_Config) ->
    {Lat, Long} = h3:to_geo(h3:from_string("845ad1bffffffff")),
    ct:pal("Lat: ~p, Long: ~p", [h3:rads_to_degs(Lat), h3:rads_to_degs(Long)]),
    22.320484717937624 = h3:rads_to_degs(Lat),
    169.72002399032806 = h3:rads_to_degs(Long),
    ok.

h3_to_geo_boundary_test(_Config) ->
    Paris = h3:from_geo({h3:degs_to_rads(48.8566), h3:degs_to_rads(2.3522)}, 9),
    Boundary = h3:to_geo_boundary(Paris),
    [
     {48.858845065, 2.352656618},
     {48.857874118, 2.350662810},
     {48.856107090, 2.350996343},
     {48.855311035, 2.353323544},
     {48.856281965, 2.355317257},
     {48.858048967, 2.354983864}
    ] = Boundary,
    ok.

hex_area_km2_decreasing_test(Config) ->
    Resolutions = proplists:get_value(resolutions, Config),
    Res = lists:map(fun(R) ->
                            h3:hex_area_km2(R)
                    end, lists:reverse(Resolutions)),
    Res = lists:sort(Res),
    ok.

self_not_a_neighbor_test(_Config) ->
    SFH3 = h3:from_geo({0.659966917655, -2.1364398519396}, 9),
    false = h3:indices_are_neighbors(SFH3, SFH3),
    ok.

h3_of_geo_coord_test(_Config) ->
    Paris = h3:from_geo({h3:degs_to_rads(48.8566), h3:degs_to_rads(2.3522)}, 9),
    "891fb466257ffff" = h3:to_string(Paris),
    ok.

k_ring_origin_index_test(Config) ->
    ParisIndex = proplists:get_value(paris_index, Config),
    [OriginIndex] = h3:k_ring(h3:from_geo({h3:degs_to_rads(37.3615593), h3:degs_to_rads(-122.0553238)}, 7), 0),
    ParisIndex = OriginIndex,
    ok.

k_ring_distance_origin_test(Config) ->
    ParisIndex = proplists:get_value(paris_index, Config),
    [{OriginIndex, 0}] = h3:k_ring_distances(ParisIndex, 0),
    OriginIndex = ParisIndex,
    ok.

k_ring_distance_test(Config) ->
    ParisIndex = proplists:get_value(paris_index, Config),
    K = 1,
    %% kring algorithm always returns 7 indices with k=1, in no particular order
    %% 6 vertices of the hexagon and 1 being the origin index
    7 = h3:max_k_ring_size(K),
    7 = length(h3:k_ring_distances(ParisIndex, K)),
    ok.

compact_roundtrip_test(Config) ->
    SunnyvaleIndex = proplists:get_value(sunnyvale_index, Config),
    ExpandedSize = h3:max_k_ring_size(9),
    Expanded = h3:k_ring(SunnyvaleIndex, 9),
    ExpandedSize = length(Expanded),

    Compressed = h3:compact(Expanded),
    73 = length(Compressed),

    Decompressed = h3:uncompact(Compressed, 9),
    ExpandedSize = length(Decompressed),

    ok.
