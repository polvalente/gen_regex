defmodule GenRegex do
  @moduledoc """
  This module parses Regex's and generates strings that pass the spec

  For wildcard characters (.), the generated characters will be a subset of printable ASCII chars
  For negated character classes ([^...]), the generated characters will be a subset the possible wildcard values.

  ## Example
      iex> ~r/(bye|hello|hi), (guys|people|all)(!|\()/
      ...> |> GenRegex.generate_from()
      ...> |> Enum.take(3)
      ["bye, all(", "bye, guys(", "hi, guys("]
  """

  alias GenRegex.{
    Generator,
    Interpreter
  }

  def generate_from(regexp) do
    regexp
    |> StreamData.constant()
    |> StreamData.map(&do_generate_from/1)
  end

  defp do_generate_from(regexp) do
    regexp
    |> lex()
    |> parse()
    |> Interpreter.read()
    |> Generator.generate()
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
end
