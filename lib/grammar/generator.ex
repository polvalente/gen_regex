defmodule GenRegex.Generator do
  @moduledoc """
    Regex generator struct. This will be the intermediate step between parsing and generating strings.
  """
  defstruct type: nil,
            min: 1,
            max: 1,
            value: nil
end
