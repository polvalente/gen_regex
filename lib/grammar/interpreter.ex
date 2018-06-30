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

  def interpret({:choice, choice}) do
    interpret(choice)
  end

  def interpret({:set, items}), do: do_interpret_set(:set, items)

  def interpret({:negset, items}) do
    case do_interpret_set(:negset, items) do
      %{type: :wildcard} ->
        %Generator{
          min: 0,
          max: 0,
          type: nil,
          value: nil
        }
      negset ->
        negset
    end
  end

  defp do_interpret_set(type, items) do
    if {:wildcard, :.} in items do
      %Generator{
        type: :wildcard
      }
    else
      result =
        items
        |> Enum.uniq()
        |> Enum.map(&interpret/1)

      %Generator{
        type: type,
        value: result
      }
    end
  end

  def interpret({:atom, val}), do: to_string(val)

  def interpret({:escape, val}) do
    "ESC"
  end

  def interpret(ast) do
    result =
      ast
      |> Enum.map(&interpret/1)

    result
  end
end
