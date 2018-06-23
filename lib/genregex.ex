defmodule GenRegex do
  @moduledoc """
  This module parses Regex's and generates strings that pass the spec
  """

  @doc """
  Receives a Regex and returns a tokenized version of it
  """
  def lex(regexp) do
    {:ok, tokens, _} =
      regexp
      |> Regex.source()
      |> String.to_charlist()
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
