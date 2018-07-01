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
      |> Interpreter.interpret()
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
    assert genexp == [generator("aA0,!@¨", :word)]
  end

  test "Should interpret option" do
    genexp = interpret(~r/(foo)/)
    assert genexp == [generator([generator("foo", :word)], :option)]

    genexp = interpret(~r/(foo|bar)/)
    assert genexp == [generator([generator("foo", :word), generator("bar", :word)], :option)]
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
    assert genexp == [generator("a", :word, 0, nil)]
  end

  test "Should parse +" do
    genexp = interpret(~r/a+/)
    assert genexp == [generator("a", :word, 1, nil)]
  end

  test "Should parse ?" do
    genexp = interpret(~r/a?/)
    assert genexp == [generator("a", :word, 0, 1)]
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
                 generator("first", :word),
                 generator("last", :word)
               ],
               :option
             ),
             generator("_name", :word)
           ]
  end

  test "Should parse word+option" do
    genexp = interpret(~r/foo_(bar|baz)/)

    assert genexp == [
             generator("foo_", :word),
             generator(
               [
                 generator("bar", :word),
                 generator("baz", :word)
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
                 generator("abc", :word),
                 generator("def", :word)
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
                 generator("abc", :word),
                 generator("def", :word)
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
                 generator("abc", :word),
                 generator("def", :word)
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
    assert genexp == [generator("a-zA-Z0-9", :word)]
  end

  test "Should interpret a{0456} as repexpr (456,456)" do
    genexp = interpret(~r/a{0456}/)

    assert genexp == [
             generator("a", :word, 456, 456)
           ]
  end

  test "Should interpret a{4,} as repexpr (4, nil)" do
    genexp1 = interpret(~r/a{0456,}/)
    genexp2 = interpret(~r/a{0456,   }/)

    assert genexp1 == [
             generator("a", :word, 456, nil)
           ]

    assert genexp2 == [
             generator("a", :word, 456, nil)
           ]
  end

  test "Should interpret a{0123,0456} as repexpr (123,456)" do
    genexp = interpret(~r/a{0123, 0456}/)

    assert genexp == [
             generator("a", :word, 123, 456)
           ]
  end

  test "Should parse ^ as atom in word" do
    genexp = interpret(~r/^ab[^aabb^abba]/)

    assert genexp == [
             generator("ab", :word),
             generator(["a", "b", "^"], :negset)
           ]
  end
end
