-module(h3).
-export([num_hexagons/1, edge_length_meters/1, edge_length_kilometers/1, geo_to_h3/2]).
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

-spec geo_to_h3(coord(), resolution()) -> h3index().
geo_to_h3(_, _) ->
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
