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

  # test "Should parse []*" do
  #   genexp = interpret(~r/[abc]*/)
  #   assert genexp == [generator(["a", "b", "c"], :set, 0, nil)]
  # end

  # test "Should parse []+" do
  #   genexp = interpret(~r/[abc]+/)
  #   assert genexp == [generator(["a", "b", "c"], :set, 1, nil)]
  # end

  # test "Should parse []?" do
  #   genexp = interpret(~r/[abc]?/)
  #   assert genexp == [generator(["a", "b", "c"], :set, 0, 1)]
  # end

  # test "Should parse ()*" do
  #   genexp = interpret(~r/(abc|def)*/)

  #   assert genexp == [
  #            generator(
  #              [
  #                generator(~w"a b c", :word),
  #                generator(~w"d e f", :word)
  #              ],
  #              :option,
  #              0,
  #              nil
  #            )
  #          ]
  # end

  # test "Should parse ()+" do
  #   genexp = interpret(~r/(abc|def)+/)

  #   assert genexp == [
  #            generator(
  #              [
  #                generator(~w"a b c", :word),
  #                generator(~w"d e f", :word)
  #              ],
  #              :option,
  #              1,
  #              nil
  #            )
  #          ]
  # end

  # test "Should parse ()?" do
  #   genexp = interpret(~r/(abc|def)?/)

  #   assert genexp == [
  #            generator(
  #              [
  #                generator(~w"a b c", :word),
  #                generator(~w"d e f", :word)
  #              ],
  #              :option,
  #              0,
  #              1
  #            )
  #          ]
  # end

  # test "Should parse ([a-zA-Z0-9]+) correctly (set of ranges +)" do
  #   genexp = interpret(~r/[a-zA-Z0-9]+/)

  #   assert genexp == [
  #            generator(
  #              [
  #                generator(97..122, :range),
  #                generator(65..90, :range),
  #                generator(48..57, :range)
  #              ],
  #              :set,
  #              1,
  #              nil
  #            )
  #          ]
  # end

  # test "Should parse ([^a-zA-Z0-9]+) correctly (negset of ranges +)" do
  #   genexp = interpret(~r/[^a-zA-Z0-9]+/)

  #   assert genexp == [
  #            generator(
  #              [
  #                generator(97..122, :range),
  #                generator(65..90, :range),
  #                generator(48..57, :range)
  #              ],
  #              :negset,
  #              1,
  #              nil
  #            )
  #          ]
  # end

  # test "Should generate from range as word outside of set" do
  #   genexp = interpret(~r/a-zA-Z0-9/)
  #   assert genexp == [generator(~w"a-z A-Z 0-9", :word)]
  # end

  # test "Should generate from a{0456} as repexpr (456,456)" do
  #   genexp = interpret(~r/a{0456}/)

  #   assert genexp == [
  #            generator(~w"a", :word, 456, 456)
  #          ]
  # end

  # test "Should generate from a{4,} as repexpr (4, nil)" do
  #   genexp1 = interpret(~r/a{0456,}/)
  #   genexp2 = interpret(~r/a{0456,   }/)

  #   assert genexp1 == [
  #            generator(~w"a", :word, 456, nil)
  #          ]

  #   assert genexp2 == [
  #            generator(~w"a", :word, 456, nil)
  #          ]
  # end

  # test "Should generate from a{0123,0456} as repexpr (123,456)" do
  #   genexp = interpret(~r/a{0123, 0456}/)

  #   assert genexp == [
  #            generator(~w"a", :word, 123, 456)
  #          ]
  # end

  # test "Should generate from ^ as atom in word" do
  #   genexp = interpret(~r/^ab[^aabb^abba]/)

  #   assert genexp == [
  #            generator(~w"a b", :word),
  #            generator(["a", "b", "^"], :negset)
  #          ]
  # end

  # test "Should generate from especial escape sequences" do
  #   digit_expr = interpret(~r/\d/)
  #   ndigit_expr = interpret(~r/\D/)
  #   wspc_expr = interpret(~r/\s/)
  #   nwspc_expr = interpret(~r/\S/)
  #   wrd_expr = interpret(~r/\w/)
  #   nwrd_expr = interpret(~r/\W/)

  #   assert digit_expr ==
  #     [generator([
  #       generator([
  #         generator(48..57, :range)
  #         ],
  #         :set
  #       )],
  #       :word
  #     )]


  #   assert ndigit_expr ==
  #     [generator([
  #       generator([
  #         generator(48..57, :range)
  #         ],
  #         :negset
  #       )],
  #       :word
  #     )]

  #   assert wspc_expr ==
  #   [generator([
  #     generator([" ", "\t", "\r", "\n", "\v", "\f"],
  #       :set
  #     )],
  #     :word
  #   )]

  #   assert nwspc_expr ==
  #     [generator([
  #       generator([" ", "\t", "\r", "\n", "\v", "\f"],
  #         :negset
  #       )],
  #       :word
  #     )]

  #   assert wrd_expr ==
  #   [generator([
  #     generator([
  #       generator(48..57, :range),
  #       generator(97..122, :range),
  #       generator(65..90, :range),
  #       "_"
  #     ],
  #       :set
  #     )],
  #     :word
  #   )]

  #   assert nwrd_expr ==
  #   [generator([
  #     generator([
  #       generator(48..57, :range),
  #       generator(97..122, :range),
  #       generator(65..90, :range),
  #       "_"
  #     ],
  #       :negset
  #     )],
  #     :word
  #   )]
  # end

  # test "Should generate from regular escape sequences" do
  #   genexp = interpret(~r/\n\r\t\v\f/)

  #   assert genexp == [
  #            generator(["\n","\r","\t","\v", "\f"], :word, 1, 1)
  #          ]
  # end
end
