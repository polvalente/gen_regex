defmodule GenRegex do
  @moduledoc """
  This module parses Regex's and generates strings that pass the spec
  """

  @doc """
  Receives a Regex and returns a tokenized version of it
  """
  def lex(regexp) do
    regexp
    |> Regex.source()
    |> String.to_charlist()
    |> :lexer.string()
  end
end
