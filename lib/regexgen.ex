defmodule Regexgen do
  @moduledoc """
  Documentation for Regexgen.
  """

  @doc """
  Hello world.

  ## Examples

      iex> Regexgen.hello
      :world

  """
  def lex(regexp) do
    regexp
    |> Regex.source()
    |> String.to_charlist()
    |> :lexer.string()
  end
end
