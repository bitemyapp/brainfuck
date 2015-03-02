# brainfuck

Concise Brainfuck Interpreter using ListZippers in Haskell

Written in under 50 lines of code.

Usage:
------

You could use Cabal to run the executable:

~~~ bash
$ cabal run brainfuck examples/hello.bf

Preprocessing executable 'brainfuck' for brainfuck-0.1.0.0...
Running brainfuck...
Hello World!
~~~

Or run the file manually, if you already have ListZipper installed:

~~~ bash
$ runhaskell src/Main.hs examples/hello.bf
Hello World!
~~~

Details:
--------

Written in a concise manner, ignoring any performance concerns.

Unlike most solutions, this is purely functional.

Unlike some concise solutions, handles nested loops perfectly fine.

Uses two ListZippers, one for the bytes being incremented, and
one for the source code being read.

Additionally, there’s one variation of the program written using
the state monad transformer present in `src/state.hs`.

