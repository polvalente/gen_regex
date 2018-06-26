defmodule GenRegex.ParserTest do
  use ExUnit.Case

  defmacro parse(regex) do
    quote do
      unquote(regex)
      |> GenRegex.lex()
      |> GenRegex.parse()
    end
  end

  test "Should parse word (with atoms, range, escape)" do
    ast = parse(~r/aA0!@¨\w0-9/)
    assert ast == [
      word: [
        atom: 'a',
        atom: 'A',
        atom: '0',
        atom: '!',
        atom: '@',
        atom: [168],
        escape: '\\w',
        range: {'0', '9'}
      ]
    ]
  end

  test "Should parse list" do
    ast = parse(~r/(foo)/)
    assert ast == [
      list: [
        word: [
          atom: 'f',
          atom: 'o',
          atom: 'o'
        ]
      ]
    ]
  end

  test "Should parse set" do
    ast = parse(~r/[foo.]/)
    assert ast == [
      set: [
        word: [
          atom: 'f',
          atom: 'o',
          atom: 'o'
        ],
        wildcard: :.
      ]
    ]
  end

  test "Should parse *" do
    ast = parse(~r/a*/)
    assert ast == [
      repzero: [
        word: [
          atom: 'a'
        ]
      ]
    ]
  end

  test "Should parse +" do
    ast = parse(~r/a+/)
    assert ast == [
      reponce: [
        word: [
          atom: 'a'
        ]
      ]
    ]
  end

  test "Should parse ?" do
    ast = parse(~r/a?/)
    assert ast == [
      optelem: [
        word: [
          atom: 'a'
        ]
      ]
    ]
  end

  test "Should parse wildcard" do
    ast = parse(~r/./)
    assert ast == [wildcard: :.]
  end

  test "Should parse list+word" do
    ast = parse(~r/(first|last)_name/)
    assert ast == [
      list: [
        word: [
          atom: 'f',
          atom: 'i',
          atom: 'r',
          atom: 's',
          atom: 't'
        ],
        word: [
          atom: 'l',
          atom: 'a',
          atom: 's',
          atom: 't'
        ]
      ],
      word: [
        atom: '_',
        atom: 'n',
        atom: 'a',
        atom: 'm',
        atom: 'e'
      ]
    ]
  end

  test "Should parse word+list" do
    ast = parse(~r/foo_(bar|baz)/)
    assert ast == [
      word: [
        atom: 'f',
        atom: 'o',
        atom: 'o',
        atom: '_'
      ],
      list: [
        word: [
          atom: 'b',
          atom: 'a',
          atom: 'r'
        ],
        word: [
          atom: 'b',
          atom: 'a',
          atom: 'z'
        ]
      ]
    ]
  end

  test "Should parse []*" do
    ast = parse(~r/[abc]*/)
    assert ast == [
      repzero: [
        set: [
          word: [
            atom: 'a',
            atom: 'b',
            atom: 'c'
          ]
        ]
      ]
    ]
  end

  test "Should parse []+" do
    ast = parse(~r/[abc]+/)
    assert ast == [
      reponce: [
        set: [
          word: [
            atom: 'a',
            atom: 'b',
            atom: 'c'
          ]
        ]
      ]
    ]
  end

  test "Should parse []?" do
    ast = parse(~r/[abc]?/)
    assert ast == [
      optelem: [
        set: [
          word: [
            atom: 'a',
            atom: 'b',
            atom: 'c'
          ]
        ]
      ]
    ]
  end

  test "Should parse ()*" do
    ast = parse(~r/(abc|def)*/)
    assert ast == [
      repzero: [
        list: [
          word: [
            atom: 'a',
            atom: 'b',
            atom: 'c'
          ],
          word: [
            atom: 'd',
            atom: 'e',
            atom: 'f'
          ]
        ]
      ]
    ]
  end

  test "Should parse ()+" do
    ast = parse(~r/(abc|def)+/)
    assert ast == [
      reponce: [
        list: [
          word: [
            atom: 'a',
            atom: 'b',
            atom: 'c'
          ],
          word: [
            atom: 'd',
            atom: 'e',
            atom: 'f'
          ]
        ]
      ]
    ]
  end

  test "Should parse ()?" do
    ast = parse(~r/(abc|def)?/)
    assert ast == [
      optelem: [
        list: [
          word: [
            atom: 'a',
            atom: 'b',
            atom: 'c'
          ],
          word: [
            atom: 'd',
            atom: 'e',
            atom: 'f'
          ]
        ]
      ]
    ]
  end
end
