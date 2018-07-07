defmodule GenRegex.Generator do
  @moduledoc """
    Regex generator module and struct. This will be the intermediate step between interpreting parsed ASTs and generating strings.
  """
  defstruct type: nil,
            min: 1,
            max: 1,
            value: nil

  def generate(list) do
    list
  end
end
