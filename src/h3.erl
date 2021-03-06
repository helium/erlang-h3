-module(h3).

-export([num_hexagons/1,
         edge_length_meters/1,
         edge_length_kilometers/1,
         hex_area_m2/1,
         hex_area_km2/1,
         from_geo/2,
         to_geo/1,
         to_geo_boundary/1,
         to_string/1,
         from_string/1,
         get_resolution/1,
         get_base_cell/1,
         is_valid/1,
         is_class3/1,
         is_pentagon/1,
         parent/2,
         children/2,
         k_ring/2,
         k_ring_distances/2,
         max_k_ring_size/1,
         compact/1,
         uncompact/2,
         indices_are_neighbors/2,
         get_unidirectional_edge/2,
         grid_distance/2,
         get_res0_indexes/0,
         polyfill/2,
         set_to_multi_polygon/1,
         contains/2,
         meminfo/0
        ]).

-on_load(init/0).

-define(APPNAME, h3).
-define(LIBNAME, 'libh3').

-type coord() :: {float(), float()}.
-type h3index() :: non_neg_integer().
-type polygon() :: [[coord(),...],...].
-type resolution() :: 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | 10 | 11 | 12 | 13 | 14 | 15.

-spec num_hexagons(resolution()) -> pos_integer().
num_hexagons(_) ->
    not_loaded(?LINE).

-spec edge_length_meters(resolution()) -> float().
edge_length_meters(_) ->
    not_loaded(?LINE).

-spec edge_length_kilometers(resolution()) -> float().
edge_length_kilometers(_) ->
    not_loaded(?LINE).

-spec hex_area_m2(non_neg_integer()) -> float().
hex_area_m2(_) ->
    not_loaded(?LINE).

-spec hex_area_km2(non_neg_integer()) -> float().
hex_area_km2(_) ->
    not_loaded(?LINE).

%% @doc Indexes the location at the specified resolution. Takes the
%% coordinate in degrees.
-spec from_geo(coord(), resolution()) -> h3index().
from_geo(_, _) ->
    not_loaded(?LINE).

%% @doc Finds the centroid of the index. Returns the coordinate in
%% degrees.
-spec to_geo(h3index()) -> coord().
to_geo(_) ->
    not_loaded(?LINE).

%% @doc Finds the geo boundary of the given index. The retuurned list
%% of coordinates is in degrees.
-spec to_geo_boundary(h3index()) -> [coord()].
to_geo_boundary(_) ->
    not_loaded(?LINE).

%% @doc Converts the given index to its string representation.
-spec to_string(h3index()) -> string().
to_string(_) ->
    not_loaded(?LINE).

%% @doc Converts the string representation to an index.
-spec from_string(string()) -> h3index().
from_string(_) ->
    not_loaded(?LINE).

%% @doc Returns the resolution of the index.
-spec get_resolution(h3index()) -> resolution().
get_resolution(_) ->
    not_loaded(?LINE).

%% @doc Returns the base cell number of the index.
-spec get_base_cell(h3index()) -> h3index().
get_base_cell(_) ->
    not_loaded(?LINE).

%% @doc Returns whethr the given index is valid.
-spec is_valid(h3index()) -> boolean().
is_valid(_) ->
    not_loaded(?LINE).

%% @doc Returns whether the given index has a resolution with Class III orientation.
-spec is_class3(h3index()) -> boolean().
is_class3(_) ->
    not_loaded(?LINE).

%% @doc Returns whether the given index represents a pentagonal cell.
-spec is_pentagon(h3index()) -> boolean().
is_pentagon(_) ->
    not_loaded(?LINE).

%% @doc Returns the parent (coarser) index containing the given Index.
%% Requests for higher resolutions than the the resolution of `Index'
%% are an error.
-spec parent(Index::h3index(), ParentRes::resolution()) -> h3index().
parent(_, _) ->
    not_loaded(?LINE).

%% @doc Returns the children container by the given Index at the given child resolution ChildRes.
-spec children(Index::h3index(), ChildRes::resolution()) -> [h3index(),...].
children(_, _) ->
    not_loaded(?LINE).

%% @doc Produces indices within the given distance K from the given
%% origin Index. k-ring 0 is defined as the origin index, k-ring 1 is
%% defined as k-ring 0 and all neighboring indices, and so on. Output
%% is returned in no particular order.
-spec k_ring(Index::h3index(), K::non_neg_integer()) -> [h3index(),...].
k_ring(_, _) ->
    not_loaded(?LINE).

%% @doc Produces indices and the associated distance within the given
%% distance K from the given origin Index. k-ring 0 is defined as the
%% origin index, k-ring 1 is defined as k-ring 0 and all neighboring
%% indices, and so on. Output is returned in no particular order.
-spec k_ring_distances(Index::h3index(), K::non_neg_integer()) ->
                              [{h3index(), non_neg_integer()},...].
k_ring_distances(_, _) ->
    not_loaded(?LINE).

%% @doc Returns the maximum number of indices that result from the
%% k-ring algorithm with the given K.
-spec max_k_ring_size(K::non_neg_integer()) -> non_neg_integer().
max_k_ring_size(_) ->
    not_loaded(?LINE).

%% @doc Compacts the given list of indexes as best as
%% possible. Returns a list of at most the same length as the input
%% list.
-spec compact([h3index(),...]) -> [h3index(),...].
compact(_) ->
    not_loaded(?LINE).

%% @doc Uncompacts given list of indexes to the given resolution.
-spec uncompact([h3index(),...], resolution()) -> [h3index(),...].
uncompact(_,_) ->
    not_loaded(?LINE).

-spec indices_are_neighbors(h3index(), h3index()) -> boolean().
indices_are_neighbors(_, _) ->
    not_loaded(?LINE).

%% @doc Returns the distance in grid cells between the two
%% indexes. Throws a `badarg' if the distance can not be
%% found. Finding the distance can fail because the two indexes are
%% not comparable (different resolutions), too far apart, or are
%% separated by pentagonal distortion.
-spec grid_distance(h3index(), h3index()) -> integer().
grid_distance(_, _) ->
    not_loaded(?LINE).

%% @doc Returns a unidirectional edge based on the given origin
%% and destination.
-spec get_unidirectional_edge(Origin::h3index(), Destination::h3index()) -> Edge::h3index().
get_unidirectional_edge(_, _) ->
    not_loaded(?LINE).

%% @doc Returns all h3 indexes at resolution 0.
-spec get_res0_indexes() -> [h3index(),...].
get_res0_indexes() ->
    not_loaded(?LINE).

%% @doc Takes a given GeoJSON-like polygon and fills it with the
%% hexagons that are contained by it.
-spec polyfill(polygon(), resolution()) -> [h3index(),...].
polyfill(_, _) ->
    not_loaded(?LINE).

%% @doc Returns a GeoJSON-like MultiPolygon describing the outline(s)
%% of a set of hexagons.
-spec set_to_multi_polygon([h3index(),...]) -> [polygon()].
set_to_multi_polygon(_) ->
    not_loaded(?LINE).

%% @doc Returns a map of nif and libh3 memory allocation statistics.
meminfo() ->
    not_loaded(?LINE).

%% @doc Test if a `Set' of H3 indices completely encompasses a
%%      `Target' H3 index.
%%
%% `Set' can be a list of H3 indices or a binary of serialized
%% little-endian indices.
%%
%% Returns `{true, Index}' if the target is covered as the returned
%% `Index' may be a parent to `Target'. Returns `false' is `Target' is
%% not covered.
-spec contains(Target::h3index(), Set::(binary() | [h3index(),...])) -> false | {true, h3index()}.
contains(_Target, _Set) ->
    not_loaded(?LINE).

init() ->
    SoName = case code:priv_dir(?APPNAME) of
        {error, bad_name} ->
            case filelib:is_dir(filename:join(["..", priv])) of
                true ->
                    filename:join(["..", priv, ?LIBNAME]);
                _ ->
                    filename:join([priv, ?LIBNAME])
            end;
        Dir ->
            filename:join(Dir, ?LIBNAME)
    end,
    erlang:load_nif(SoName, 0).

not_loaded(Line) ->
    erlang:nif_error({not_loaded, [{module, ?MODULE}, {line, Line}]}).
