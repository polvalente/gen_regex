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
    ast = parse(~r/aA0!@¨\w[0-9]/)
    assert ast == [
      word: [
        atom: 'a',
        atom: 'A',
        atom: '0',
        atom: '!',
        atom: '@',
        atom: [168],
        escape: '\\w',
      ],
      set: [
        range: {'0', '9'}
      ]
    ]
  end

  test "Should parse option" do
    ast = parse(~r/(foo)/)
    assert ast == [
      option: [
        choice: [
          word: [
            atom: 'f',
            atom: 'o',
            atom: 'o'
          ]
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

  test "Should parse option+word" do
    ast = parse(~r/(first|last)_name/)
    assert ast == [
      option: [
        choice: [
          word: [
            atom: 'f',
            atom: 'i',
            atom: 'r',
            atom: 's',
            atom: 't'
          ]
        ],
        choice: [
          word: [
            atom: 'l',
            atom: 'a',
            atom: 's',
            atom: 't'
          ]
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

  test "Should parse word+option" do
    ast = parse(~r/foo_(bar|baz)/)
    assert ast == [
      word: [
        atom: 'f',
        atom: 'o',
        atom: 'o',
        atom: '_'
      ],
      option: [
        choice: [
          word: [
            atom: 'b',
            atom: 'a',
            atom: 'r'
          ]
        ],
        choice: [
          word: [
            atom: 'b',
            atom: 'a',
            atom: 'z'
          ]
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
        option: [
          choice: [
            word: [
              atom: 'a',
              atom: 'b',
              atom: 'c'
            ]
          ],
          choice: [
            word: [
              atom: 'd',
              atom: 'e',
              atom: 'f'
            ]
          ]
        ]
      ]
    ]
  end

  test "Should parse ()+" do
    ast = parse(~r/(abc|def)+/)
    assert ast == [
      reponce: [
        option: [
          choice: [
            word: [
              atom: 'a',
              atom: 'b',
              atom: 'c'
            ],
          ],
          choice: [
            word: [
              atom: 'd',
              atom: 'e',
              atom: 'f'
            ]
          ]
        ]
      ]
    ]
  end

  test "Should parse ()?" do
    ast = parse(~r/(abc|def)?/)
    assert ast == [
      optelem: [
        option: [
          choice: [
            word: [
              atom: 'a',
              atom: 'b',
              atom: 'c'
            ],
          ],
          choice: [
            word: [
              atom: 'd',
              atom: 'e',
              atom: 'f'
            ]
          ]
        ]
      ]
    ]
  end

  test "Should parse (\.[a-zA-Z0-9]+) correctly" do
    ast = parse(~r/(\.[a-zA-Z0-9]+)/)
    assert ast == [
      option: [
        choice: [
          word: [
            escape: '\\.'
          ],
          reponce: [
            set: [
              range: {'a','z'},
              range: {'A','Z'},
              range: {'0','9'}
            ]
          ]
        ]
      ]
    ]
  end
end
