defmodule GenRegex.Interpreter do
  @moduledoc """
  This is the interpreter module.
  It reduces the parsed structures to string generator nodes, which in turn will be used to generate the final string.
  """

  alias GenRegex.Generator

  def interpret({:word, elems}) do
    result =
      elems
      |> Enum.map(&interpret/1)
      |> Enum.join()
    %Generator{
      type: :word,
      value: result
    }
  end

  def interpret({:option, choices}) do
    result =
      choices
      |> Enum.map(&interpret/1)
      |> List.flatten()

    %Generator{
      type: :option,
      value: result
    }
  end

  def interpret({:choice, choice}),
    do: interpret(choice)

  def interpret({:set, items}),
    do: do_interpret_set(:set, items)

  def interpret({:negset, items}),
    do: do_interpret_set(:negset, items)

  def interpret({:wildcard, :.}),
    do: %Generator{
      type: :wildcard,
      value: nil
    }


  def interpret({:atom, val}), do: to_string(val)

  def interpret({:repexpr, [expr, min, max]}) do
    [result] =
      expr
      |> List.wrap()
      |> Enum.map(&(
        case &1 do
          {:atom, val} ->
            interpret({:word, [{:atom, val}]})
          val -> val
        end)
      )
      |> interpret()

    result
    |> Map.put(:min, min)
    |> Map.put(:max, max)
  end

  def interpret(ast) do
    result =
      ast
      |> Enum.map(&interpret/1)

    result
  end

  defp do_interpret_set(type, items) do
    result =
      items
      |> Enum.uniq()
      |> Enum.map(&interpret/1)
      |> Enum.map(fn item ->
        case item do
          %Generator{type: :wildcard} -> :wildcard
          item -> item
        end
      end)

    %Generator{
      type: type,
      value: result
    }
  end
end
