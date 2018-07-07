defmodule GenRegex.Interpreter do
  @moduledoc """
  This is the interpreter module.
  It reduces the parsed structures to string generator nodes, which in turn will be used to generate the final string.
  """

  alias GenRegex.Generator

  def read(ast, parent \\ nil), do: interpret(ast, parent)

  defp interpret({:word, [head | tail] = elems}, parent) do
    elems =
      if head == {:atom, :^} and parent == nil do
        tail
      else
        elems
      end

    result =
      elems
      |> List.wrap()
      |> Enum.map(&interpret(&1, :word))
      |> Enum.map(fn item ->
        case item do
          %Generator{type: :set, value: value} -> value
          %Generator{type: :negset, value: value} -> value
          item -> item
        end
      end)

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

  defp interpret({:set, items}, :word), do: interpret(items, :word)
  defp interpret({:set, items}, :set), do: interpret(items, :set)
  defp interpret({:set, items}, _parent), do: do_interpret_set(:set, items)

  defp interpret({:negset, items}, :word), do: interpret(items, :word)
  defp interpret({:negset, items}, :negset), do: interpret(items, :negset)
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

  defp interpret({:escape, _seq} = input, parent) do
    {set_type, result} = do_escape(input)

    case parent do
      :set ->
        result
      :negset ->
        result
      :choice ->
        result
      _ ->
        %Generator{
          type: set_type,
          value: result
        }
    end
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

  defp do_interpret_set(type, items) do
    result =
      items
      |> Enum.uniq()
      |> Enum.map(&interpret(&1, :set))
      |> Enum.map(fn item ->
        case item do
          %Generator{type: :wildcard} -> :wildcard
          %Generator{type: :set, value: value} -> value
          item -> item
        end
      end)
      |> List.wrap()
      |> List.flatten()

    %Generator{
      type: type,
      value: result
    }
  end

  defp do_escape({:escape, '\\d'}) do
    {:set,
      %Generator{
        max: 1,
        min: 1,
        type: :set,
        value: [
          %Generator{max: 1, min: 1, type: :range, value: 48..57}
        ]
      }
    }
  end

  defp do_escape({:escape, '\\D'}) do
    {:set, result} = do_escape({:escape, '\\d'})
    {:negset, Map.put(result, :type, :negset)}
  end

  defp do_escape({:escape, '\\w'}) do
    {:set,
      %Generator{
        max: 1,
        min: 1,
        type: :set,
        value: [
          %Generator{max: 1, min: 1, type: :range, value: 48..57},
          %Generator{max: 1, min: 1, type: :range, value: 97..122},
          %Generator{max: 1, min: 1, type: :range, value: 65..90},
          "_"
        ]
      }
    }
  end

  defp do_escape({:escape, '\\W'}) do
    {:set, result} = do_escape({:escape, '\\w'})
    {:negset, Map.put(result, :type, :negset)}
  end

  defp do_escape({:escape, '\\s'}) do
    {:set,
      %Generator{
        max: 1,
        min: 1,
        type: :set,
        value: [" ", "\t", "\r", "\n", "\v", "\f"]
      }
    }
  end

  defp do_escape({:escape, '\\S'}) do
    {:set, result} = do_escape({:escape, '\\s'})
    {:negset, Map.put(result, :type, :negset)}
  end

  defp do_escape({:escape, char}) do
    {:set,
      {
        :atom,
        Macro.unescape_string(to_string(char))
      }
      |> interpret(:escape)
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
