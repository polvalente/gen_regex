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

  def generate(list) when is_list(list) do
    list
    |> Enum.map(&generate/1)
    |> Enum.join("")
  end
  # ==================
  # REPETITION CLAUSES
  # ==================

  def generate(%Generator{min: nil} = gen), do: generate(%Generator{gen | min: 0})
  def generate(%Generator{max: nil} = gen), do: generate(%Generator{gen | max: @max_reps})
  def generate(%Generator{min: min, max: max} = gen)
    when min != 1 and max != 1
  do
    gen_reps("", gen, Enum.random(min..max))
  end

  # ==================
  #  REGULAR CLAUSES
  # ==================

  def generate(%Generator{type: :word} = gen) do
    gen
    |> Map.get(:value)
    |> List.wrap()
    |> Enum.map(&generate/1)
    |> Enum.join("")
  end

  def generate(%Generator{type: :option} = gen) do
    gen
    |> Map.get(:value)
    |> Enum.random()
    |> generate()
  end


  # ==================
  #  CATCH-ALL CLAUSE
  # ==================
  def generate(str), do: to_string(str)


  # ==================
  # PRIVATE FUNCTIONS
  # ==================

  defp gen_reps(acc, generator, count)
    when count <= 0, do: acc
  defp gen_reps(acc, generator, count),
    do: gen_reps(
      acc + generate(%Generator{generator | min: 1, max: 1}),
      generator,
      count - 1
      )
end
