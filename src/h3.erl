-module(h3).
-export([num_hexagons/1,
         edge_length_meters/1,
         edge_length_kilometers/1,
         degs_to_rads/1
        ]).
-on_load(init/0).

-define(APPNAME, h3).
-define(LIBNAME, 'h3').

num_hexagons(_) ->
    not_loaded(?LINE).

edge_length_meters(_) ->
    not_loaded(?LINE).

edge_length_kilometers(_) ->
    not_loaded(?LINE).

degs_to_rads(_) ->
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
    exit({not_loaded, [{module, ?MODULE}, {line, Line}]}).
