defmodule GenRegex.LexerTest do
  use ExUnit.Case

  test "Should lex characters as atoms" do
    tokens = GenRegex.lex(~r/aA0!/)
    assert tokens == [
      {:atom, 1, 'a'},
      {:atom, 1, 'A'},
      {:atom, 1, '0'},
      {:atom, 1, '!'}
    ]

  end

  test "Should lex brackets as special tokens" do
    tokens = GenRegex.lex(~r/[a]/)
    assert tokens == [
      {:'[', 1},
      {:atom, 1, 'a'},
      {:']', 1}
    ]
  end

  test "Should lex parentheses as special tokens" do
    tokens = GenRegex.lex(~r/(a)/)
    assert tokens == [
      {:'(', 1},
      {:atom, 1, 'a'},
      {:')', 1}
    ]
  end

  test "Should lex | as special token" do
    tokens = GenRegex.lex(~r/a|a/)
    assert tokens == [
      {:atom, 1, 'a'},
      {:'|', 1},
      {:atom, 1, 'a'}
    ]
  end

  test "Should lex ^ as special token" do
    tokens = GenRegex.lex(~r/^a/)
    assert tokens == [
      {:'^', 1},
      {:atom, 1, 'a'}
    ]
  end

  test "Should lex * as special token" do
    tokens = GenRegex.lex(~r/a*/)
    assert tokens == [
      {:atom, 1, 'a'},
      {:'*', 1}
    ]
  end

  test "Should lex + as special token" do
    tokens = GenRegex.lex(~r/a+/)
    assert tokens == [
      {:atom, 1, 'a'},
      {:'+', 1}
    ]
  end

  test "Should lex - as range token" do
    tokens = GenRegex.lex(~r/a-z/)
    assert tokens == [
      {:range, 1, 'a-z'}
    ]
  end

  test "Should lex . as special token" do
    tokens = GenRegex.lex(~r/./)
    assert tokens == [
      {:'.', 1}
    ]
  end

  test "Should lex ? as special token" do
    tokens = GenRegex.lex(~r/a?/)
    assert tokens == [
      {:atom, 1, 'a'},
      {:'?', 1}
    ]
  end

  test "Should lex escape sequence" do
    tokens = GenRegex.lex(~r/\x/)
    assert tokens == [
      {:escape, 1, '\\x'}
    ]
  end

  test "Should lex , as special token" do
    tokens = GenRegex.lex(~r/a,b/)
    assert tokens == [
      {:atom, 1, 'a'},
      {:comma, 1, ','},
      {:atom, 1, 'b'}
    ]
  end
end
