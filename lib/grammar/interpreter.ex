defmodule GenRegex.Interpreter do
  @moduledoc """
  This is the interpreter module.
  It reduces the parsed structures to string generator nodes, which in turn will be used to generate the final string.
  """

  alias GenRegex.Generator

  defp interpret({:word, [head | tail] = elems}, parent) do
    elems =
      if head == {:atom, :^} and parent == nil do
        tail
      else
        elems
      end

    result =
      elems
      |> Enum.map(&interpret(&1, :word))
      |> Enum.join()

    %Generator{
      type: :word,
      value: result
    }
  end

  defp interpret({:option, choices}, _parent) do
    result =
      choices
      |> Enum.map(&interpret(&1, :option))
      |> List.flatten()

    %Generator{
      type: :option,
      value: result
    }
  end

  defp interpret({:choice, choice}, _parent), do: interpret(choice, :choice)

  defp interpret({:set, items}, _parent), do: do_interpret_set(:set, items)

  defp interpret({:negset, items}, _parent), do: do_interpret_set(:negset, items)

  defp interpret({:wildcard, :.}, _parent),
    do: %Generator{
      type: :wildcard,
      value: nil
    }

  defp interpret({:atom, val}, _parent), do: to_string(val)

  defp interpret({:repexpr, [expr, min, max]}, _parent) do
    min = to_integer(min, :repexpr)
    max = to_integer(max, :repexpr)

    [result] =
      expr
      |> List.wrap()
      |> interpret(:repexpr)

    result
    |> Map.put(:min, min)
    |> Map.put(:max, max)
  end

  defp interpret({:range, val}, :set) do
    [first, last] =
      val
      |> to_string()
      |> String.split("-")
      |> Enum.map(&:binary.decode_unsigned(&1))

    %Generator{
      type: :range,
      value: first..last
    }
  end

  defp interpret({:range, val}, :negset), do: interpret({:range, val}, :set)

  defp interpret({:range, val}, :word), do: to_string(val)

  defp interpret({:range, val}, _) do
    %Generator{
      type: :word,
      value: to_string(val)
    }
  end

  defp interpret(ast, _) when is_number(ast), do: ast
  defp interpret(ast, _) when is_binary(ast), do: ast
  defp interpret(ast, _) when is_nil(ast), do: ast

  defp interpret(ast, parent) do
    result =
      ast
      |> Enum.map(&interpret(&1, parent))

    result
  end

  def interpret(ast), do: interpret(ast, nil)

  defp do_interpret_set(type, items) do
    result =
      items
      |> Enum.uniq()
      |> Enum.map(&interpret(&1, :set))
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

  defp to_integer(nil, _parent), do: nil

  defp to_integer(val, _parent)
       when is_integer(val),
       do: val

  defp to_integer([{:word, elems}], parent) do
    {num, _} =
      elems
      |> Enum.map(&interpret(&1, parent))
      |> Enum.join()
      |> Integer.parse()

    num
  end

  defp to_integer(val, parent) do
    {num, _} =
      val
      |> interpret(parent)
      |> to_string()
      |> Integer.parse()

    num
  end
end
