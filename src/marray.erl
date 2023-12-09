%%%-------------------------------------------------------------------
%% @doc marray public API
%% @end
%%%-------------------------------------------------------------------

-module(marray).
-export([
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
