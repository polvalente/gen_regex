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

  def interpret({:set, items}) do
    result =
      items
      |> Enum.uniq()
      |> Enum.map(&interpret/1)

    %Generator{
      type: :set,
      value: result
    }
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
