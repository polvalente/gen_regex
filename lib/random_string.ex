defmodule GenRegex.RandomString do
  @moduledoc """
  Random string generator module.

  Taken from ahmadshah/randomizer.ex @ Github gist
  """

  @doc """
  Generate random string based on the given legth. It is also possible to generate certain type of randomise string using the options below:
  * :all - generate alphanumeric random string
  * :alpha - generate nom-numeric random string
  * :numeric - generate numeric random string
  * :upcase - generate upper case non-numeric random string
  * :downcase - generate lower case non-numeric random string
  ## Example
      iex> GenRegex.RandomString.generate(20) //"Je5QaLj982f0Meb0ZBSK"
      iex> GenRegex.RandomString.generate(4, :numeric) //"9821"
  """

  @alphabets "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
  @numbers "0123456789"
  @special "_.!?(){}[]*&%$#@|\\;:,-+=/"

  def generate(length, type \\ :all) do
    alphabets = @alphabets
    numbers = @numbers
    special = @special

    lists =
      cond do
        type == :alpha -> alpha()
        type == :numeric -> numbers()
        type == :upcase -> upcase()
        type == :downcase -> downcase()
        type == :special -> special()
        true -> all()
      end
      |> String.split("", trim: true)

    do_randomizer(length, lists)
  end

  def all, do: @alphabets <> String.downcase(@alphabets) <> @numbers <> @special
  def alpha, do: @alphabets <> String.downcase(@alphabets)
  def numbers, do: @numbers
  def upcase, do: @alphabets
  def downcase, do: String.downcase(@alphabets)
  def special, do: @special

  @doc false
  defp get_range(length) when length > 1, do: (1..length)
  defp get_range(length), do: [1]

  @doc false
  defp do_randomizer(length, lists) do
    get_range(length)
    |> Enum.reduce([], fn(_, acc) -> [Enum.random(lists) | acc] end)
    |> Enum.join("")
  end
end