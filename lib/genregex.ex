defmodule GenRegex do
  @moduledoc """
  This module parses Regex's and generates strings that pass the spec
  """

  @doc """
  Receives a Regex and returns a tokenized version of it
  """

  def generate_from(regexp) do
    regexp
    |> lex()
    |> parse()
    |> interpret()
  end

  def lex(regexp) do
    {:ok, tokens, _} =
      regexp
      |> Regex.source()
      |> to_charlist()
      |> :lexer.string()

    tokens
  end

  def parse(tokens) do
    {:ok, exprs} =
      tokens
      |> :parser.parse()

    exprs
  end

  def interpret([]), do: []
  def interpret([head | tail]) do
    interpret(head) ++ interpret(tail)
  end

  def interpret({:list, elems}) do
    Enum.map(elems, &interpret/1)
  end

  def interpret({:atom, char}) do
    char
  end

  def interpret({:escape, value}) do
    ''
  end

  def interpret({:word, elems}) do
    result = [elems | []]
    |> Enum.map(&interpret/1)
    |> Enum.concat()

    [{:word, result}]
  end

  def interpret({:range, {first, last}}) do
    first = ord(first)
    last = ord(last)

    result = first..last
    |> Enum.to_list()
  end

  def interpret({:set, elems}) do
    result = elems
    |> Enum.uniq()
    |> Enum.map(&interpret/1)
    |> Enum.concat()

    {:set, result}
  end

  def interpret({:setword, word, set}) do
    [{:setword, interpret(word), interpret(set)}]
  end

  def interpret({:wildcard, :.} = arg), do: [arg]

  defp ord(char) do
    char
    |> to_string()
    |> :binary.decode_unsigned()
  end

  defp chr(num) do
    num
    |> :binary.encode_unsigned
    |> to_charlist()
  end
end
