# marray

C++ implemented mutable array for BEAM.

## Build

```bash
$ rebar3 compile
```

## Usage

```erlang
M1 = marray:new(10, default).
[0,0,0,0,0,0,0,0,0,0] = marray:to_list(M1).
10 = marray:size(M1).

M2 = marray:from_list([1,2,3,4,5,6,7,8]).
[1,2,3,4,5,6,7,8] = marray:to_list(M2).
8 = marray:size(M2).

ok = marray:set(M1, 2, 3).
[0,0,3,0,0,0,0,0,0,0] = marray:to_list(M1).

{ok, 6} = marray:get(M2, 5).

ok = marray:swap(M2, 0, 7).
[8,2,3,4,5,6,7,1] = marray:to_list(M2).

quicksort(Marray) ->
  R = marray:size(Marray) - 1,
  quicksort(Marray, 0, R).

quicksort(Marray, L, R) when L < R ->
  PivotIndex = partition(Marray, L, R),
  quicksort(Marray, L, PivotIndex - 1),
  quicksort(Marray, PivotIndex + 1, R);
quicksort(Marray, _L, _R) ->
  Marray.

partition(Marray, L, R) ->
  Pivot = marray:get(Marray, R),
  Indices = lists:seq(L, R - 1),
  I = lists:foldr(fun(Index,I) ->
    Val = marray:get(Marray, Index),
    case Val < Pivot of
      true -> 
        marray:swap(Marray,I+1,Index),
        I+1;
      false -> I
    end
  end,L - 1,Indices),
  marray:swap(Marray,I+1, R),
  I+1.

[1,2,3,4,5,6,7,8] = marray:to_list(M2).
```
