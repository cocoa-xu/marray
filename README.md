# marray

C++ implemented mutable array for BEAM.

## Build

```bash
$ rebar3 compile
```

## Quick Benchmark

The following results were obtained from a MacBook Pro with M2 Max.

(unit: mircosecond)

```bash
./benchmark 10 10 100 1000 2000 5000 10000 20000 30000
Benchmarking with array length 10
Marray: avg=125.8, max=162, min=97, stddev=21.59
List:   avg=28.4, max=38, min=24, stddev=3.98

Benchmarking with array length 100
Marray: avg=1890.1, max=2101, min=1630, stddev=153.04
List:   avg=720.0, max=884, min=599, stddev=89.94

Benchmarking with array length 1000
Marray: avg=26974.8, max=32193, min=23739, stddev=2387.03
List:   avg=19484.2, max=22496, min=17313, stddev=1518.84

Benchmarking with array length 2000
Marray: avg=58593.3, max=65042, min=54309, stddev=3446.1
List:   avg=55875.8, max=59970, min=49245, stddev=3112.9

Benchmarking with array length 5000
Marray: avg=168861.7, max=184733, min=155017, stddev=8272.67
List:   avg=224711.5, max=238281, min=214199, stddev=7220.43

Benchmarking with array length 10000
Marray: avg=383278.7, max=409095, min=347485, stddev=20551.45
List:   avg=638803.8, max=694130, min=604800, stddev=24839.1

Benchmarking with array length 20000
Marray: avg=832194.9, max=908544, min=774701, stddev=40717.45
List:   avg=1915756.9, max=2021014, min=1843304, stddev=50501.87

Benchmarking with array length 30000
Marray: avg=1358398.9, max=1487849, min=1248130, stddev=75684.29
List:   avg=3629470.7, max=3944460, min=3324542, stddev=183993.95
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
