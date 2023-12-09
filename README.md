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

[1,2,3,4,5,6,7,8] = marray:to_list(marray:sort(M2)).

[1,4,7] = marray:to_list(marray:stride_view(M2, 3)).
```

## Quick Benchmark

The following results were obtained from a MacBook Pro with M2 Max.

(unit: mircosecond)

```bash
./benchmark 10 10 100 1000 2000 5000 10000 20000 30000
Benchmarking with array length 10
Marray(erlang): avg=160.3, max=201, min=147, stddev=15.01
Marray(c):      avg=0.0, max=0, min=0, stddev=0.0
List:           avg=40.7, max=44, min=39, stddev=1.27

Benchmarking with array length 100
Marray(erlang): avg=2737.5, max=2881, min=2563, stddev=103.44
Marray(c):      avg=1.1, max=2, min=1, stddev=0.3
List:           avg=2.6, max=5, min=2, stddev=0.92

Benchmarking with array length 1000
Marray(erlang): avg=38938.7, max=41431, min=37892, stddev=1180.83
Marray(c):      avg=4.4, max=5, min=4, stddev=0.49
List:           avg=89.4, max=97, min=83, stddev=4.25

Benchmarking with array length 2000
Marray(erlang): avg=86503.9, max=95203, min=83450, stddev=3161.69
Marray(c):      avg=7.5, max=10, min=7, stddev=0.92
List:           avg=232.2, max=248, min=222, stddev=8.39

Benchmarking with array length 5000
Marray(erlang): avg=244410.3, max=281198, min=231632, stddev=13606.75
Marray(c):      avg=18.6, max=24, min=17, stddev=2.01
List:           avg=610.8, max=669, min=566, stddev=28.38

Benchmarking with array length 10000
Marray(erlang): avg=519937.6, max=538865, min=505768, stddev=11287.45
Marray(c):      avg=37.3, max=41, min=34, stddev=2.41
List:           avg=1074.9, max=1176, min=996, stddev=49.79

Benchmarking with array length 20000
Marray(erlang): avg=1115104.8, max=1179568, min=1081238, stddev=26421.23
Marray(c):      avg=71.7, max=90, min=66, stddev=6.47
List:           avg=2201.1, max=2422, min=2024, stddev=109.85

Benchmarking with array length 30000
Marray(erlang): avg=1769281.4, max=2026156, min=1693883, stddev=92459.68
Marray(c):      avg=102.5, max=116, min=99, stddev=4.88
List:           avg=3364.2, max=3959, min=3073, stddev=242.22
```
