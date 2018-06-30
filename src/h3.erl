-module(h3).
-export([num_hexagons/1,
         edge_length_meters/1,
         edge_length_kilometers/1,
         degs_to_rads/1,
         rads_to_degs/1,
         max_k_ring_size/1,
         hex_area_m2/1,
         hex_area_km2/1
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

rads_to_degs(_) ->
    not_loaded(?LINE).

max_k_ring_size(_) ->
    not_loaded(?LINE).

hex_area_m2(_) ->
    not_loaded(?LINE).

hex_area_km2(_) ->
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
