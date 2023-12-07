# Run it as: elixir benchmark.exs
Mix.install([:benchee])

path = "./_build/default/lib/marray/ebin"

unless File.exists?(path) do
  raise "marray was not compiled, run rebar3 compile before"
end

Code.prepend_path(path)

l1k = Enum.map(1..10000, fn _ -> :rand.uniform() end)

defmodule QSort do
  def immutable([]),
    do: []

  def immutable([pivot | t]) do
    immutable(for(x <- t, x < pivot, do: x)) ++
      [pivot] ++ immutable(for(x <- t, x >= pivot, do: x))
  end

  def mutable(list) do
    marray = :marray.from_list(list)
    mutable(marray, 0, :marray.size(marray) - 1)
  end

  defp mutable(marray, l, r) when l < r do
    index = partition(marray, l, r)
    mutable(marray, l, index - 1)
    mutable(marray, index + 1, r)
  end

  defp mutable(marray, _l, _r) do
    :marray.to_list(marray)
  end

  defp partition(marray, l, r) do
    pivot = :marray.get(marray, r)
    indices = :lists.seq(l, r - 1)

    i =
      :lists.foldr(
        fn index, i ->
          val = :marray.get(marray, index)

          if val < pivot do
            :marray.swap(marray, i + 1, index)
            i + 1
          else
            i
          end
        end,
        l - 1,
        indices
      )

    :marray.swap(marray, i + 1, r)
    i + 1
  end
end

Benchee.run(
  %{
    ":lists.sort/1" => fn -> :lists.sort(l1k) end,
    "qsort (mutable)" => fn -> QSort.mutable(l1k) end
  },
  time: 5,
  memory_time: 2
)
