-module(marray_nif).
-compile(nowarn_export_all).
-compile([export_all]).

-on_load(init/0).

-define(APPNAME, marray_nif).
-define(LIBNAME, marray_nif).

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

marray_new(_Capacity, _DefaultVal) ->
    not_loaded(?LINE).

marray_from_list(_List) ->
    not_loaded(?LINE).

marray_to_list(_Marray) ->
    not_loaded(?LINE).

marray_set(_Marray, _Index, _Val) ->
    not_loaded(?LINE).

marray_get(_Marray, _Index) ->
    not_loaded(?LINE).

marray_swap(_Marray, _IndexI, _IndexJ) ->
    not_loaded(?LINE).

marray_size(_Marray) ->
    not_loaded(?LINE).

marray_sort(_Marray) ->
    not_loaded(?LINE).

marray_reverse(_Marray) ->
    not_loaded(?LINE).
