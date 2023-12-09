%%%-------------------------------------------------------------------
%% @doc marray public API
%% @end
%%%-------------------------------------------------------------------

-module(marray).
-export([
    new/1,
    new/2,
    from_list/1,
    to_list/1,
    set/3,
    get/2,
    swap/3,
    size/1,
    sort/1,
    reverse/1
]).
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

new(Capacity) ->
    new(Capacity, default).

new(Capacity, DefaultVal) ->
    InitVal =
        case DefaultVal of
            default ->
                0;
            _ ->
                DefaultVal
        end,
    marray_nif:marray_new(Capacity, InitVal).

from_list(List) when is_list(List) ->
    marray_nif:marray_from_list(List).

to_list(MArray) ->
    marray_nif:marray_to_list(MArray).

set(MArray, Index, Val) ->
    marray_nif:marray_set(MArray, Index, Val).

get(MArray, Index) ->
    marray_nif:marray_get(MArray, Index).

swap(MArray, IndexI, IndexJ) ->
    marray_nif:marray_swap(MArray, IndexI, IndexJ).

size(MArray) ->
    marray_nif:marray_size(MArray).

sort(MArray) ->
    marray_nif:marray_sort(MArray).

reverse(MArray) ->
    marray_nif:marray_reverse(MArray).

-ifdef(EUNIT).
new_test() ->
    Capacity = 10,
    M = new(Capacity),
    ?assertEqual(marray:size(M), Capacity),
    ?assertEqual(marray:to_list(M), lists:duplicate(Capacity, 0)),
    M1 = new(Capacity, 1),
    ?assertEqual(marray:size(M1), Capacity),
    ?assertEqual(marray:to_list(M1), lists:duplicate(Capacity, 1)).

from_list_test() ->
    List = [1, 2, 3, 4, 5],
    M = from_list(List),
    ?assertEqual(marray:size(M), length(List)),
    ?assertEqual(marray:to_list(M), List).

to_list_test() ->
    List = [1, 2, 3, 4, 5],
    M = from_list(List),
    ?assertEqual(marray:to_list(M), List).

set_test() ->
    List = [1, 2, 3, 4, 5],
    M = from_list(List),
    ?assertEqual(marray:get(M, 0), 1),
    ?assertEqual(marray:get(M, 1), 2),
    ?assertEqual(marray:get(M, 2), 3),
    ?assertEqual(marray:get(M, 3), 4),
    ?assertEqual(marray:get(M, 4), 5),
    marray:set(M, 0, 10),
    marray:set(M, 1, 20),
    marray:set(M, 2, 30),
    marray:set(M, 3, 40),
    marray:set(M, 4, 50),
    ?assertEqual(marray:get(M, 0), 10),
    ?assertEqual(marray:get(M, 1), 20),
    ?assertEqual(marray:get(M, 2), 30),
    ?assertEqual(marray:get(M, 3), 40),
    ?assertEqual(marray:get(M, 4), 50).

swap_test() ->
    List = [1, 2, 3, 4, 5],
    M = from_list(List),
    ?assertEqual(marray:get(M, 0), 1),
    ?assertEqual(marray:get(M, 1), 2),
    ?assertEqual(marray:get(M, 2), 3),
    ?assertEqual(marray:get(M, 3), 4),
    ?assertEqual(marray:get(M, 4), 5),
    marray:swap(M, 0, 4),
    marray:swap(M, 1, 3),
    ?assertEqual(marray:get(M, 0), 5),
    ?assertEqual(marray:get(M, 1), 4),
    ?assertEqual(marray:get(M, 2), 3),
    ?assertEqual(marray:get(M, 3), 2),
    ?assertEqual(marray:get(M, 4), 1).

size_test() ->
    List = [1, 2, 3, 4, 5],
    M = from_list(List),
    ?assertEqual(marray:size(M), length(List)).

sort_test() ->
    List = [5, 4, 3, 2, 1],
    M = from_list(List),
    marray:sort(M),
    ?assertEqual(marray:to_list(M), lists:sort(List)).

reverse_test() ->
    List = [1, 2, 3, 4, 5],
    M = from_list(List),
    marray:reverse(M),
    ?assertEqual(marray:to_list(M), lists:reverse(List)).

-endif.
