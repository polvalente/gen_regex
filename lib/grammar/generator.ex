defmodule GenRegex.Generator do
  @moduledoc """
    Regex generator module and struct. This will be the intermediate step between interpreting parsed ASTs and generating strings.

    For expressions that match an indefinite maximum number of repetitions (e.g a+, a*), the default maximum of 100 will be used
    In the future this will be configurable
  """
  defstruct type: nil,
            min: 1,
            max: 1,
            value: nil

  alias __MODULE__

  @max_reps 100

  def generate(expr), do: generate(expr, nil)

  def generate(list, parent) when is_list(list) do
    list
    |> Enum.map(&(generate(&1, parent)))
    |> Enum.join("")
  end

  # ==================
  # REPETITION CLAUSES
  # ==================

  def generate(%Generator{min: nil} = gen, _), do: generate(%Generator{gen | min: 0}, nil)
  def generate(%Generator{max: nil, min: min} = gen, _) do
    case @max_reps > min do
      true ->
        generate(%Generator{gen | max: @max_reps}, nil)
      false ->
        generate(%Generator{gen | max: min+1}, nil)
    end
  end

  def generate(%Generator{min: min, max: max, type: type} = gen, _)
    when min != 1 and max != 1
  do
    reps = Enum.random(min..max)
    gen_reps("", gen, type, reps)
  end

  # ==================
  #  REGULAR CLAUSES
  # ==================

  def generate(%Generator{type: :word, value: value}, _parent) do
    value
    |> List.wrap()
    |> Enum.map(&generate(&1, :word))
    |> Enum.join("")
  end

  def generate(%Generator{type: :option, value: value} = gen, _parent) do
    value
    |> Enum.random()
    |> generate()
  end

  def generate(%Generator{type: :set, value: value}, _parent) do
    value
    |> Enum.random()
    |> generate(:set)
  end

  def generate(%Generator{type: :negset, value: value}, _parent) do
    value =
      value
      |> Enum.map(&(generate(&1, :negset)))
      |> List.flatten()

    GenRegex.RandomString.all()
    |> String.split("", trim: true)
    |> :lists.subtract(value)
    |> Enum.random()
  end

  def generate(%Generator{type: :range} = gen, :negset), do: generate(gen, :set)
  def generate(%Generator{type: :range, value: value}, :set) do
    value
    |> Enum.to_list()
    |> Enum.map(&(String.Chars.List.to_string([&1])))
    |> List.flatten()
  end

  def generate(%Generator{type: :range, value: value}, _parent) do
    value
    |> Enum.take_random(1)
    |> String.Chars.List.to_string
  end

  def generate(:wildcard, :set), do: "."
  def generate(wildcard, :negset), do: "."
  def generate(%Generator{type: :wildcard}, :set), do: "."
  def generate(%Generator{type: :wildcard}, :negset), do: "."
  def generate(%Generator{type: :wildcard}, _parent) do
    GenRegex.RandomString.generate(1)
  end


  # ==================
  #  CATCH-ALL CLAUSE
  # ==================
  def generate(str, _parent), do: to_string(str)


  # ==================
  # PRIVATE FUNCTIONS
  # ==================

  defp gen_reps(acc, generator, parent, count)
    when count <= 0, do: acc
  defp gen_reps(acc, generator, parent, count),
    do: gen_reps(
      acc <> generate(%Generator{generator | min: 1, max: 1}, parent),
      generator,
      parent,
      count - 1
      )
end
