-module(h3_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([all/0, init_per_testcase/2, end_per_testcase/2]).
-export([
         validity_test/1,
         max_k_ring_size_test/1,
         geo_to_h3_test/1,
         h3_to_geo_test/1,
         h3_to_geo_constrain_test/1,
         h3_to_geo_boundary_test/1,
         hex_area_km2_decreasing_test/1,
         self_not_a_neighbor_test/1,
         h3_edge_test/1,
         h3_of_geo_coord_test/1,
         k_ring_origin_index_test/1,
         k_ring_distance_origin_test/1,
         k_ring_distance_test/1,
         compact_roundtrip_test/1,
         parent_test/1
        ]).

all() ->
    [
     validity_test,
     max_k_ring_size_test,
     geo_to_h3_test,
     h3_to_geo_test,
     h3_to_geo_constrain_test,
     hex_area_km2_decreasing_test,
     self_not_a_neighbor_test,
     h3_edge_test,
     h3_of_geo_coord_test,
     k_ring_origin_index_test,
     k_ring_distance_origin_test,
     k_ring_distance_test,
     compact_roundtrip_test,
     parent_test
    ].

init_per_testcase(_, Config) ->
    Resolutions = lists:seq(0, 15),
    ParisIndex = h3:from_geo({37.3615593, -122.0553238}, 7),
    SunnyvaleIndex = h3:from_string("89283470c27ffff"),
    %% XXX: make this go to a 1000 and feel that RAM burn
    KList = lists:seq(1, 100),
    [{resolutions, Resolutions},
     {paris_index, ParisIndex},
     {sunnyvale_index, SunnyvaleIndex},
     {k_list, KList}
     | Config].

end_per_testcase(_, _Config) ->
    ok.

validity_test(_Config) ->
    H3 = h3:from_string("845ad1bffffffff"),
    596072861467148287 = H3,
    true = h3:is_valid(H3),
    ok.

max_k_ring_size_test(_Config) ->
    true = h3:max_k_ring_size(3) > 0,
    ok.

geo_to_h3_test(_Config) ->
    Res = h3:from_geo({40.689167, -74.044444}, 5),
    "852a1073fffffff" = h3:to_string(Res),
    ok.

h3_to_geo_test(_Config) ->
    {Lat, Lon} = h3:to_geo(h3:from_string("845ad1bffffffff")),
    ct:pal("Lat: ~p, Lon: ~p", [Lat, Lon]),
    {22.320484717937624, 169.72002399032806} = {Lat, Lon},
    ok.

h3_to_geo_constrain_test(_Config) ->
    {Lat, Lon} = h3:to_geo(h3:from_string("87283472bffffff")),
    ct:pal("Lat: ~p, Lon: ~p", [Lat, Lon]),
    {37.35171820183272, -122.05032565263943} = {Lat, Lon},
    ok.

h3_to_geo_boundary_test(_Config) ->
    Paris = h3:from_geo({48.8566, 2.3522}, 9),
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


h3_edge_test(Config) ->
    ParisIndex = proplists:get_value(paris_index, Config), 
    NorthParisIndex = h3:from_geo({37.3715593, -122.0553238}, 7),
    1401326775769759743 = h3:h3_edge(ParisIndex, NorthParisIndex),
    ok.

h3_of_geo_coord_test(_Config) ->
    Paris = h3:from_geo({48.8566, 2.3522}, 9),
    "891fb466257ffff" = h3:to_string(Paris),
    ok.

k_ring_origin_index_test(Config) ->
    ParisIndex = proplists:get_value(paris_index, Config),
    [OriginIndex] = h3:k_ring(h3:from_geo({37.3615593, -122.0553238}, 7), 0),
    ParisIndex = OriginIndex,
    ok.

k_ring_distance_origin_test(Config) ->
    ParisIndex = proplists:get_value(paris_index, Config),
    [{OriginIndex, 0}] = h3:k_ring_distances(ParisIndex, 0),
    OriginIndex = ParisIndex,
    ok.

k_ring_distance_test(Config) ->
    %% TODO: check specific known values possibly
    ParisIndex = proplists:get_value(paris_index, Config),
    KList = proplists:get_value(k_list, Config),
    KRingDistances = [{K, h3:k_ring_distances(ParisIndex, K)} || K <- KList],
    lists:map(fun({K, KRingDistanceList}) ->
                      %% all kringdistancelist must contain the origin index
                      true = lists:member({ParisIndex, 0}, KRingDistanceList),
                      %% their length must be equal to the max allowed at that k
                      true = length(KRingDistanceList) == h3:max_k_ring_size(K),
                      %% each k adds a "layer" of hexagons on top of the base
                      %% so, at k=1, it is the origin index + 6 vertices
                      %% at k=2, it's k*6 vertices + the ones from k=1 ...
                      RingedHexagonsAtK = lists:filter(fun({_, CalcK}) ->
                                                               K == CalcK
                                                       end,
                                                       lists:delete({ParisIndex, 0}, KRingDistanceList)),
                      true = K == length(RingedHexagonsAtK) div 6
              end, KRingDistances),
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

parent_test(Config) ->
    SunnyvaleIndex = proplists:get_value(sunnyvale_index, Config),
    Resolutions = proplists:get_value(resolutions, Config),
    Resolution = h3:get_resolution(SunnyvaleIndex),
    %% check we can get the parent at the same resolution and it's the same index
    SunnyvaleIndex = h3:parent(SunnyvaleIndex, Resolution),
    ?assertError(badarg, h3:parent(SunnyvaleIndex, Resolution+1)),
    [ ?assertNotException(error, badarg, h3:parent(SunnyvaleIndex, R)) || R <- Resolutions, R =< Resolution ],
    [ ?assertError(badarg, h3:parent(SunnyvaleIndex, R)) || R <- Resolutions, R > Resolution ],
    ok.
