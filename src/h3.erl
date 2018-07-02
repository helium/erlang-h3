-module(h3).

-export([num_hexagons/1,
         edge_length_meters/1,
         edge_length_kilometers/1,
         degs_to_rads/1,
         rads_to_degs/1,
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
         max_k_ring_size/1,
         indices_are_neighbors/2
        ]).

-on_load(init/0).

-define(APPNAME, h3).
-define(LIBNAME, 'h3').

-type coord() :: {float(), float()}.
-type h3index() :: non_neg_integer().
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

-spec degs_to_rads(float()) -> float().
degs_to_rads(_) ->
    not_loaded(?LINE).

-spec rads_to_degs(float()) -> float().
rads_to_degs(_) ->
    not_loaded(?LINE).

-spec hex_area_m2(non_neg_integer()) -> float().
hex_area_m2(_) ->
    not_loaded(?LINE).

-spec hex_area_km2(non_neg_integer()) -> float().
hex_area_km2(_) ->
    not_loaded(?LINE).

%% @doc Indexes the location at the specified resolution.
-spec from_geo(coord(), resolution()) -> h3index().
from_geo(_, _) ->
    not_loaded(?LINE).

%% @doc Finds the centroid of the index.
-spec to_geo(h3index()) -> coord().
to_geo(_) ->
    not_loaded(?LINE).

%% @doc Finds the geo boundary of the given index.
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

-spec parent(h3index(), resolution()) -> h3index().
parent(_, _) ->
    not_loaded(?LINE).

-spec children(h3index(), resolution()) -> [h3index(),...].
children(_, _) ->
    not_loaded(?LINE).

-spec k_ring(h3index(), non_neg_integer()) -> [h3index(),...].
k_ring(_, _) ->
    not_loaded(?LINE).

-spec max_k_ring_size(non_neg_integer()) -> non_neg_integer().
max_k_ring_size(_) ->
    not_loaded(?LINE).

-spec indices_are_neighbors(h3index(), h3index()) -> boolean().
indices_are_neighbors(_, _) ->
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
