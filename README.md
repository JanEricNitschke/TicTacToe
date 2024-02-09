# TicTacToe
Collection of tictactoe games written in different languages.


## TicTacToe-python
Simple tictactoe game with [Python](https://www.python.org/).

Run and test with:
```bash
python tictactoe_python
coverage run -m pytest
coverage report -m
coverage html
```

## TicTacToe-javascript
Simple tictactoe game with [JavaScript](https://www.ecma-international.org/publications-and-standards/standards/ecma-262/).

Hosted [here](https://main.dwbh88xp4gd1m.amplifyapp.com/) on AWS.

## TicTacToe-Cpp
Simple tictactoe game with [C++](https://isocpp.org/std/the-standard).

To build, test and run:
```bash
cmake -S . -B build
cmake --build build
cd build && ctest
cd .. && ./bin/tictactoe
```
## TicTacToe-rust
Simple tictactoe game with [Rust](https://www.rust-lang.org/).

To build, test and run:
```bash
cargo build
cargo test
cargo run
```

## TicTacToe-haskell
Simple tictactoe game with [Haskell](https://www.haskell.org/).

To build, test and run:
```bash
cabal build
cabal test --enable-coverage
cabal run
```

## TicTacToe-go
Simple tictactoe game with [Go](https://go.dev/).

To build, test and run:
```bash
go build
go test -covermode=count -coverprofile=coverage.out
go tool cover -html=coverage.out -o coverage.html
go test -fuzz=FuzzSwapPlayer -fuzztime 30s
go test -fuzz=FuzzGetEmptyCells -fuzztime 30s
go test -fuzz=FuzzRandomMove -fuzztime 30s
go test -fuzz=FuzzMinMax -fuzztime 60s
go run tictactoe_go.go
```


## TicTacToe-Java
Simple tictactoe game with [Java](https://www.java.com/).

To install, test and run:
```bash
mvn package
mvn test
java -jar target/tictactoe-0.1.0.jar
```

## TicTacToe-ruby
Simple tictactoe game with [Ruby](https://www.ruby-lang.org/).

To build and test run:
```bash
gem build tictactoe_ruby.gemspec
bundle install
gem install tictactoe_ruby
bundle exec rake
./bin/tictactoeRuby
```

## TicTacToe-ada
Simple tictactoe game with [Ada](https://ada-lang.io/).

To build and run:
```bash
alr build --validation
./bin/tictactoe_ada.exe
```

## TicTacToe-generic

Simple two player tictactoe game with [Generic](https://github.com/JanEricNitschke/generic-lang).

To run and test:
```bash
generic tictactoe_generic.gen
generic tests/test_tictactoe.gen
```

## TicTacToe-kotlin

Simple tictactoe app with [Kotlin](https://kotlinlang.org/).

To run and test:
```bash
./gradlew build
./gradlew test
./gradlew connectedCheck
```

## TicTacToe-swift

Simple tictactoe game with [Swift](https://www.swift.org/).

To build and test:
```bash
swift build
swift test
```

## TicTacToe-lua

Simple tictactoe game with [Lua](https://www.lua.org/).

To test and run:
```
lua54 tests/test_tictactoe.lua
lua54 tictactoe_lua.lua
```

## TicTacToe-dart

Simple tictactoe app with [Dart](https://dart.dev/) using [Flutter](https://flutter.dev/).
Targeted towards Windows.

To build and test:
```bash
flutter build windows
flutter test
```

## TicTacToe-zig

Simple tictactoe game with [Zig](https://ziglang.org/).

To build and test:
```bash
zig build install
zig build test
```

## TicTacToe-Prolog

Simple tictactoe game with [Prolog](https://en.wikipedia.org/wiki/Prolog).

Specifically [SWI-Prolog](https://www.swi-prolog.org/).

To run and test:
```bash
swipl -g main -t halt tictactoe_prolog.pl
swipl -g run_tests -t halt tictactoe_prolog_test.pl
```

## TicTacToe-Common-Lisp

Simple tictactoe game with [Common Lisp](https://lisp-lang.org/).

In the repl run:
```common-lisp
(load "D:/Programming/Projects/TicTacToe/TicTacToe/tictactoe_common-lisp/tictactoe_common-lisp.asd")
(ql:quickload :tictactoe_common-lisp)
(ql:quickload :fiveam)
```

To compile, run and test:
```bash
sbcl.exe --core C:\\lispstick\\Steel\ Bank\ Common\ Lisp\\1.1.12\\sbcl.core  --eval "(let ((warning (nth-value 1 (compile-file \"src/tictactoe_common-lisp.lisp\")))) (if warning (sb-ext:exit :code 1) (sb-ext:exit)))"
sbcl.exe --core C:\\lispstick\\Steel\ Bank\ Common\ Lisp\\1.1.12\\sbcl.core  --load src/tictactoe_common-lisp.lisp --eval '(progn (tictactoe_common-lisp:play) (sb-ext:quit))'
sbcl.exe --core C:\\lispstick\\Steel\ Bank\ Common\ Lisp\\1.1.12\\sbcl.core  --non-interactive --load run-tests.lisp
```

## TicTacToe-C

Simple tictactoe game with [C](https://www.iso-9899.info/wiki/The_Standard).

AI player settings via command line.

Also creates Python bindings and gives an example of how to use them.

To compile and run:
```bash
make
./bin/tictactoe_c -X 0
```

## TicTacToe-Smalltalk

Simple version with 2 player in [Smalltalk](https://squeak.org/).

To run in Squeak once installed:
```bash
Game new playGame.
```

## TicTacToe-Fortran

Simple version with (currently) 2 players in [Fortran](https://fortran-lang.org/).

To compile and run:
```bash
make
./bin/tictactoe.exe
```

## TicTacToe-scratch
Very simple two player tictactoe game with Scratch.

To play load it on the [Scratch website](https://scratch.mit.edu/)
