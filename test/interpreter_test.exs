defmodule GenRegex.InterpreterTest do
  use ExUnit.Case

  alias GenRegex.Generator

  defmacro interpret(regex) do
    quote do
      unquote(regex)
      |> GenRegex.lex()
      |> GenRegex.parse()
      |> GenRegex.Interpreter.interpret()
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
    assert genexp == [generator("aA0,!@¨")]
  end

  test "Should interpret option" do
    genexp = interpret(~r/(foo)/)
    assert genexp == [generator([generator("foo")], :option)]

    genexp = interpret(~r/(foo|bar)/)
    assert genexp == [generator([generator("foo"), generator("bar")], :option)]
  end

  test "Should interpret set" do
    genexp = interpret(~r/[foo]/)
    assert genexp == [generator(["f", "o"], :set)]

    genexp = interpret(~r/[foo.]/)
    assert genexp == [generator(nil, :wildcard)]
  end

  test "Should parse negset" do
    genexp = interpret(~r/[^foo]/)
    assert genexp == [generator(["f", "o"], :negset)]

    genexp = interpret(~r/[^foo.]/)
    assert genexp == [generator(nil, nil, 0, 0)]
  end

  #test "Should parse *" do
  #  genexp = interpret(~r/a*/)
  #end

  #test "Should parse +" do
  #  genexp = interpret(~r/a+/)
  #end

  #test "Should parse ?" do
  #  genexp = interpret(~r/a?/)
  #end

  #test "Should parse wildcard" do
  #  genexp = interpret(~r/./)
  #end

  #test "Should parse option+word" do
  #  genexp = interpret(~r/(first|last)_name/)
  #end

  #test "Should parse word+option" do
  #  genexp = interpret(~r/foo_(bar|baz)/)
  #end

  #test "Should parse []*" do
  #  genexp = interpret(~r/[abc]*/)
  #end

  #test "Should parse []+" do
  #  genexp = interpret(~r/[abc]+/)
  #end

  #test "Should parse []?" do
  #  genexp = interpret(~r/[abc]?/)
  #end

  #test "Should parse ()*" do
  #  genexp = interpret(~r/(abc|def)*/)
  #end

  #test "Should parse ()+" do
  #  genexp = interpret(~r/(abc|def)+/)
  #end

  #test "Should parse ()?" do
  #  genexp = interpret(~r/(abc|def)?/)
  #end

  #test "Should parse (\.[a-zA-Z0-9]+) correctly" do
  #  genexp = interpret(~r/(\.[a-zA-Z0-9]+)/)
  #end

  #test "Should parse a{0456} as repexpr ('04564','0456')" do
  #  genexp = interpret(~r/a{0456}/)
  #end

  #test "Should parse a{4,} as repexpr (4, nil)" do
  #  genexp1 = interpret(~r/a{0456,}/)
  #  genexp2 = interpret(~r/a{0456,   }/)
  #end

  #test "Should parse a{0123,0456} as repexpr (0123,0456)" do
  #  genexp = interpret(~r/a{0123, 0456}/)
  #end

  #test "Should parse ^ as atom in word" do
  #  genexp = interpret(~r/^ab[ab^]/)
  #end
end
