defmodule GenRegex.InterpreterTest do
  use ExUnit.Case

  alias GenRegex.{
    Generator,
    Interpreter
  }

  defmacro interpret(regex) do
    quote do
      unquote(regex)
      |> GenRegex.lex()
      |> GenRegex.parse()
      |> Interpreter.read()
    end
  end

  def generator(val, type \\ nil, min \\ 1, max \\ 1) do
    %Generator{
      type: type,
      min: min,
      max: max,
      value: val
    }
  end

  test "Should interpret word" do
    genexp = interpret(~r/aA0,!@¨/)
    assert genexp == [generator(~w"a A 0 , ! @ ¨", :word)]
  end

  test "Should interpret option" do
    genexp = interpret(~r/(foo)/)
    assert genexp == [generator([generator(~w"f o o", :word)], :option)]

    genexp = interpret(~r/(foo|bar)/)
    assert genexp == [generator([generator(~w"f o o", :word), generator(~w"b a r", :word)], :option)]
  end

  test "Should interpret set" do
    genexp = interpret(~r/[foo]/)
    assert genexp == [generator(["f", "o"], :set)]

    genexp = interpret(~r/[foo.]/)
    assert genexp == [generator(["f", "o", :wildcard], :set)]
  end

  test "Should parse negset" do
    genexp = interpret(~r/[^foo]/)
    assert genexp == [generator(["f", "o"], :negset)]

    genexp = interpret(~r/[^foo.]/)
    assert genexp == [generator(["f", "o", :wildcard], :negset)]
  end

  test "Should parse *" do
    genexp = interpret(~r/a*/)
    assert genexp == [generator(~w"a", :word, 0, nil)]
  end

  test "Should parse +" do
    genexp = interpret(~r/a+/)
    assert genexp == [generator(~w"a", :word, 1, nil)]
  end

  test "Should parse ?" do
    genexp = interpret(~r/a?/)
    assert genexp == [generator(~w"a", :word, 0, 1)]
  end

  test "Should parse wildcard" do
    genexp = interpret(~r/./)
    assert genexp == [generator(nil, :wildcard)]
  end

  test "Should interpret option+word" do
    genexp = interpret(~r/(first|last)_name/)

    assert genexp == [
             generator(
               [
                 generator(~w"f i r s t", :word),
                 generator(~w"l a s t", :word)
               ],
               :option
             ),
             generator(~w"_ n a m e ", :word)
           ]
  end

  test "Should parse word+option" do
    genexp = interpret(~r/foo_(bar|baz)/)

    assert genexp == [
             generator(~w"f o o _", :word),
             generator(
               [
                 generator(~w"b a r", :word),
                 generator(~w"b a z", :word)
               ],
               :option
             )
           ]
  end

  test "Should parse []*" do
    genexp = interpret(~r/[abc]*/)
    assert genexp == [generator(["a", "b", "c"], :set, 0, nil)]
  end

  test "Should parse []+" do
    genexp = interpret(~r/[abc]+/)
    assert genexp == [generator(["a", "b", "c"], :set, 1, nil)]
  end

  test "Should parse []?" do
    genexp = interpret(~r/[abc]?/)
    assert genexp == [generator(["a", "b", "c"], :set, 0, 1)]
  end

  test "Should parse ()*" do
    genexp = interpret(~r/(abc|def)*/)

    assert genexp == [
             generator(
               [
                 generator(~w"a b c", :word),
                 generator(~w"d e f", :word)
               ],
               :option,
               0,
               nil
             )
           ]
  end

  test "Should parse ()+" do
    genexp = interpret(~r/(abc|def)+/)

    assert genexp == [
             generator(
               [
                 generator(~w"a b c", :word),
                 generator(~w"d e f", :word)
               ],
               :option,
               1,
               nil
             )
           ]
  end

  test "Should parse ()?" do
    genexp = interpret(~r/(abc|def)?/)

    assert genexp == [
             generator(
               [
                 generator(~w"a b c", :word),
                 generator(~w"d e f", :word)
               ],
               :option,
               0,
               1
             )
           ]
  end

  test "Should parse ([a-zA-Z0-9]+) correctly (set of ranges +)" do
    genexp = interpret(~r/[a-zA-Z0-9]+/)

    assert genexp == [
             generator(
               [
                 generator(97..122, :range),
                 generator(65..90, :range),
                 generator(48..57, :range)
               ],
               :set,
               1,
               nil
             )
           ]
  end

  test "Should parse ([^a-zA-Z0-9]+) correctly (negset of ranges +)" do
    genexp = interpret(~r/[^a-zA-Z0-9]+/)

    assert genexp == [
             generator(
               [
                 generator(97..122, :range),
                 generator(65..90, :range),
                 generator(48..57, :range)
               ],
               :negset,
               1,
               nil
             )
           ]
  end

  test "Should interpret range as word outside of set" do
    genexp = interpret(~r/a-zA-Z0-9/)
    assert genexp == [generator(~w"a-z A-Z 0-9", :word)]
  end

  test "Should interpret a{0456} as repexpr (456,456)" do
    genexp = interpret(~r/a{0456}/)

    assert genexp == [
             generator(~w"a", :word, 456, 456)
           ]
  end

  test "Should interpret a{4,} as repexpr (4, nil)" do
    genexp1 = interpret(~r/a{0456,}/)
    genexp2 = interpret(~r/a{0456,   }/)

    assert genexp1 == [
             generator(~w"a", :word, 456, nil)
           ]

    assert genexp2 == [
             generator(~w"a", :word, 456, nil)
           ]
  end

  test "Should interpret a{0123,0456} as repexpr (123,456)" do
    genexp = interpret(~r/a{0123, 0456}/)

    assert genexp == [
             generator(~w"a", :word, 123, 456)
           ]
  end

  test "Should interpret ^ as atom in word" do
    genexp = interpret(~r/^ab[^aabb^abba]/)

    assert genexp == [
             generator(~w"a b", :word),
             generator(["a", "b", "^"], :negset)
           ]
  end

  test "Should interpret especial escape sequences" do
    digit_expr = interpret(~r/\d/)
    ndigit_expr = interpret(~r/\D/)
    wspc_expr = interpret(~r/\s/)
    nwspc_expr = interpret(~r/\S/)
    wrd_expr = interpret(~r/\w/)
    nwrd_expr = interpret(~r/\W/)

    assert digit_expr ==
      [generator([
        generator([
          generator(48..57, :range)
          ],
          :set
        )],
        :word
      )]


    assert ndigit_expr ==
      [generator([
        generator([
          generator(48..57, :range)
          ],
          :negset
        )],
        :word
      )]

    assert wspc_expr ==
    [generator([
      generator([" ", "\t", "\r", "\n", "\v", "\f"],
        :set
      )],
      :word
    )]

    assert nwspc_expr ==
      [generator([
        generator([" ", "\t", "\r", "\n", "\v", "\f"],
          :negset
        )],
        :word
      )]

    assert wrd_expr ==
    [generator([
      generator([
        generator(48..57, :range),
        generator(97..122, :range),
        generator(65..90, :range),
        "_"
      ],
        :set
      )],
      :word
    )]

    assert nwrd_expr ==
    [generator([
      generator([
        generator(48..57, :range),
        generator(97..122, :range),
        generator(65..90, :range),
        "_"
      ],
        :negset
      )],
      :word
    )]
  end

  test "Should interpret regular escape sequences" do
    genexp = interpret(~r/\n\r\t\v\f/)

    assert genexp == [
             generator(["\n","\r","\t","\v", "\f"], :word, 1, 1)
           ]
  end
end
