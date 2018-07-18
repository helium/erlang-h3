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
         get_unidirectional_edge_test/1,
         h3_of_geo_coord_test/1,
         k_ring_origin_index_test/1,
         k_ring_distance_origin_test/1,
         k_ring_distance_test/1,
         compact_roundtrip_test/1,
         parent_test/1,
         grid_distance_test/1,
         res0_test/1,
         max_polyfill_size_test/1,
         polyfill_test/1,
         set_to_multi_polygon_test/1
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
     get_unidirectional_edge_test,
     h3_of_geo_coord_test,
     k_ring_origin_index_test,
     k_ring_distance_origin_test,
     k_ring_distance_test,
     compact_roundtrip_test,
     parent_test,
     grid_distance_test,
     res0_test,
     max_polyfill_size_test,
     polyfill_test,
     set_to_multi_polygon_test
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
    {37.35171820183272, -122.05032565263944} = {Lat, Lon},
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


get_unidirectional_edge_test(Config) ->
    ParisIndex = proplists:get_value(paris_index, Config),
    NorthParisIndex = h3:from_geo({37.3715593, -122.0553238}, 7),
    1401326775769759743 = h3:get_unidirectional_edge(ParisIndex, NorthParisIndex),
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

grid_distance_test(_Config) ->
    WhiteHouse = h3:from_geo({38.898030, -77.036558}, 8),
    CapitolOneArena = h3:from_geo({38.898663, -77.020803}, 8),
    %% This distance is ~1 mile, which is grid distance = 2 at resolution = 8
    ct:pal("WhiteHouse: ~p, CapitolOneArena: ~p", [WhiteHouse, CapitolOneArena]),
    Distance = h3:grid_distance(WhiteHouse, CapitolOneArena),
    ct:pal("Distance: ~p", [Distance]),
    ?assertEqual(2, Distance),
    ok.

res0_test(_Config) ->
    ?assertEqual(122, length(h3:get_res0_indexes())).

max_polyfill_size_test(_Config) ->
    Polygon = [[
     {48.85903992956009,2.2944849729537964},
     {48.85889875637685,2.2942543029785156},
     {48.85851758679328,2.294098734855652},
     {48.858422293943846,2.293540835380554},
     {48.85826347212501,2.293299436569214},
     {48.85810817919267,2.2935301065444946},
     {48.85800935616668,2.294114828109741},
     {48.85761053268653,2.294265031814575},
     {48.85748347321125,2.2945010662078857},
     {48.85763170923437,2.2947263717651367},
     {48.85803759133688,2.29489803314209},
     {48.85811523797278,2.2954612970352173},
     {48.858274060261955,2.295691967010498},
     {48.858429352679686,2.295445203781128},
     {48.858489351893844,2.2949087619781494},
     {48.85888816837205,2.294742465019226},
     {48.85903992956009,2.2944849729537964}
    ]],
    true = h3:max_polyfill_size(Polygon, 11) > 7,
    ok.

polyfill_test(_Config) ->
    Polygon = [[
     {48.85903992956009,2.2944849729537964},
     {48.85889875637685,2.2942543029785156},
     {48.85851758679328,2.294098734855652},
     {48.858422293943846,2.293540835380554},
     {48.85826347212501,2.293299436569214},
     {48.85810817919267,2.2935301065444946},
     {48.85800935616668,2.294114828109741},
     {48.85761053268653,2.294265031814575},
     {48.85748347321125,2.2945010662078857},
     {48.85763170923437,2.2947263717651367},
     {48.85803759133688,2.29489803314209},
     {48.85811523797278,2.2954612970352173},
     {48.858274060261955,2.295691967010498},
     {48.858429352679686,2.295445203781128},
     {48.858489351893844,2.2949087619781494},
     {48.85888816837205,2.294742465019226},
     {48.85903992956009,2.2944849729537964}
    ]],
    [
     626558103194775551,
     626558103195246591,
     626558103195361279,
     626558103195369471,
     626558103195389951,
     626558103195406335,
     626558103195414527
    ] = lists:usort(h3:polyfill(Polygon, 11)),
    ok.

set_to_multi_polygon_test(_Config) ->
    H3Set = [
     626558103194775551,
     626558103195246591,
     626558103195361279,
     626558103195369471,
     626558103195389951,
     626558103195406335,
     626558103195414527
    ],
    MultiPolygon = h3:set_to_multi_polygon(H3Set),
    [[[
     {48.85903210302723,2.294754240443965},
     {48.85877962638315,2.294802150518519},
     {48.85866603401456,2.295134920160126},
     {48.85841355714403,2.2951828281713653},
     {48.85829996421167,2.2955155938268814},
     {48.858047487114725,2.295563499774844},
     {48.85790860239843,2.295278642923305},
     {48.85802219444174,2.294945878201008},
     {48.857883307720925,2.2946610211526215},
     {48.857630829294166,2.294708930749291},
     {48.85749194090619,2.2944240754268224},
     {48.857605530607586,2.294091308584969},
     {48.85785800958601,2.2940433961323237},
     {48.8579715987236,2.293710625304456},
     {48.8582240774756,2.293662710788451},
     {48.858362966538344,2.2939475699563405},
     {48.858615444512225,2.2938996562330125},
     {48.858754331907825,2.2941845171269284},
     {48.859006809103576,2.2941366041962743},
     {48.85914569483197,2.2944214668161944}
    ]]]= MultiPolygon,
    ok.
