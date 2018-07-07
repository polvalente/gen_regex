defmodule GenRegex.GeneratorTest do
  use ExUnit.Case
  use ExUnitProperties

  defmacro checkgen(regex) do
    quote do
      check all word <- GenRegex.generate_from(unquote(regex)), max_runs: 20 do
        assert Regex.match?(unquote(regex), word)
      end
    end
  end

  property "Should generate from word" do
    checkgen(~r/aA0,!@¨/)
  end

  property "Should generate from option" do
    checkgen(~r/(foo)/)
    checkgen(~r/(foo|bar)/)
  end

  property "Should generate from set" do
    checkgen(~r/[foo]/)
    checkgen(~r/[foo.]/)
  end

  property "Should generate from negset" do
    checkgen(~r/[^foo]/)
    checkgen(~r/[^foo.]/)
  end

  property "Should generate from *" do
     checkgen(~r/a*/)
  end

  property "Should generate from +" do
    checkgen(~r/a+/)
  end

  property "Should generate from ?" do
    checkgen(~r/a?/)
  end

  property "Should generate from wildcard" do
    checkgen(~r/./)
  end

  property "Should generate from option+word" do
    checkgen(~r/(first|last)_name/)
  end


  property "Should generate from word+option" do
    checkgen(~r/foo_(bar|baz)/)
  end

  property "Should generate from []*" do
    checkgen(~r/[abc]*/)
  end

  property "Should generate from []+" do
    checkgen(~r/[abc]+/)
  end

  property "Should generate from []?" do
    checkgen(~r/[abc]?/)
  end

  property "Should generate from ()*" do
    checkgen(~r/(abc|def)*/)
  end

  property "Should generate from ()+" do
    checkgen(~r/(abc|def)+/)
  end

  property "Should generate from ()?" do
    checkgen(~r/(abc|def)?/)
  end

  property "Should generate from ([a-zA-Z0-9]+) correctly (set of ranges +)" do
    checkgen(~r/[a-zA-Z0-9]+/)
  end

  property "Should generate from ([^a-zA-Z0-9]+) correctly (negset of ranges +)" do
    checkgen(~r/[^a-zA-Z0-9]+/)
  end

  property "Should generate from range as word outside of set" do
    checkgen(~r/a-zA-Z0-9/)
  end

  # property "Should generate from a{0456} as repexpr (456,456)" do
  #   checkgen(~r/a{0456}/)
  # end

  # property "Should generate from a{4,} as repexpr (4, nil)" do
  #   genexp1 = interpret(~r/a{0456,}/)
  #   genexp2 = interpret(~r/a{0456,   }/)
  # end

  # property "Should generate from a{0123,0456} as repexpr (123,456)" do
  #   checkgen(~r/a{0123, 0456}/)
  # end

  # property "Should generate from ^ as atom in word" do
  #   checkgen(~r/^ab[^aabb^abba]/)
  # end

  # property "Should generate from especial escape sequences" do
  #   digit_expr = interpret(~r/\d/)
  #   ndigit_expr = interpret(~r/\D/)
  #   wspc_expr = interpret(~r/\s/)
  #   nwspc_expr = interpret(~r/\S/)
  #   wrd_expr = interpret(~r/\w/)
  #   nwrd_expr = interpret(~r/\W/)
  # end

  # property "Should generate from regular escape sequences" do
  #   checkgen(~r/\n\r\t\v\f/)
  # end
end
