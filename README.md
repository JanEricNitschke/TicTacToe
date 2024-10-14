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

## TicTacToe-Assembly

Simple version with 2 players in [x86 Assembly](https://en.wikipedia.org/wiki/X86_assembly_language).
Uses [NASM](https://cs.lmu.edu/~ray/notes/nasmtutorial/).

To compile and run:
```bash
make
```

## TicTacToe-Brainfuck

Simple version with 2 players in [Brainfuck](https://en.wikipedia.org/wiki/Brainfuck).

Useful sites to run and debug:

https://kvbc.github.io/bf-ide/

https://copy.sh/brainfuck/

To run use any brainfuck interpreter.
Example using [brainfuck-exe](https://crates.io/crates/brainfuck-exe):

```bash
cargo install brainfuck-exe
brainfuck -f tictactoe.bf --print-info
```

## TicTacToe-Cobol

Game written in [GnuCobol](https://gnucobol.sourceforge.io/).
No minmax algorithm as highest PC strength as recursion seems annoying.

To compile and run:
```bash
cobc -x tictactoe_cobol.cob
./tictactoe_cobol
```

Depending on the setup first run `./set_env.cmd`.

## TicTacToe-Julia

Version with [Julia](https://julialang.org/).

To test and run:
```bash
julia --project=. test/runtests.jl
julia --project=. test/runexample.jl -X 3 -O 3
```

## TicTacToe-PHP

Version using [PHP](https://www.php.net/).

To test and run:
```bash
phpunit --testdox tests
php tests/runTicTacToe.php -X 4 -O 4
```

## TicTacToe-Nim

Version using [Nim](https://nim-lang.org/).

To compile, test and run:
```bash
nimble build
nimble test
./tictactoe_nim --X=4 --O=4
```

## TicTacToe-C#

Version using [C#](https://learn.microsoft.com/en-us/dotnet/csharp/).

To compile and run:
```bash
dotnet build Tictactoe.Lib
dotnet test Tictactoe.Tests
cd Tictactoe.Prog
dotnet run -- -X 4 -O 4
```

## TicTacToe-Perl

Version using [Perl](https://www.perl.org/).

To  run:
```bash
perl tictactoe.pl -X 4 -O 4
```

## TicTacToe-Nix

Version using [Nix](https://nixos.org/).

This one is only the PC playing against itself with hard coded strength settings.

```bash
nix-instantiate --eval tictactoe.nix --read-write-mode | xargs printf "%b"
```

## TicTacToe-Bash

Version using [Bash](https://www.gnu.org/software/bash/manual/bash.html).

Run with:

```bash
./tictactoe_bash.sh 4 4
```

## TicTacToe-Basic

Version using [Basic](https://www.freebasic.net/).

Compile and run with:
```bash
fbc tictactoe_basic.bas
./tictactoe_basic 4 4
```

## TicTacToe-Clojure

Version using [Clojure](https://clojure.org/).

Run:
```
clj -M tictactoe.clj 4 4
```

## TicTacToe-ClojureScript

Version using [ClojureScript](https://clojurescript.org/).

Run and go to `localhost:9000`:
```
clj -M --main cljs.main --compile tictactoe.core --repl
```

## TicTacToe-CoffeeScript

Version using [CoffeeScript](https://coffeescript.org/).

Run with:
```bash
coffee tictactoe.coffee
```

## TicTacToe-Crystal

Version using [Crystal](https://crystal-lang.org/).

Test and run with:
```bash
crystal spec
crystal run src/tictactoe_crystal.cr -- --X-strength=4 --O-strength=4
```

## TicTacToe-D

Version using [D](https://dlang.org/).

Test and run with:
```
dub test
dub run -- --X-strength=4 --O-strength=4
```

## TicTacToe-Eiffel

Version using [LibertyEiffel](https://www.liberty-eiffel.org/).

Test and run with:
```
se test tests
se c -style_warning -all_check TICTACTOE make -o tictactoe
./tictactoe --x-strength=4 --o-strength=4
se c -style_warning -no_check TICTACTOE make -o tictactoe
./tictactoe --x-strength=5 --o-strength=5
```

# TicTacToe-Elixir

Version using [Elixir](https://elixir-lang.org/).

Test and run with:
```
mix test --no-start
mix run -- --x-strength 4 --o-strength 4
```

# TicTacToe-Elm

Minimal two player version using [Elm](https://elm-lang.org/).

Hosted [here](https://main.d2csemwu9ce7sg.amplifyapp.com/) on AWS.


Compile with:
```
elm make src/Main.elm
```

# TicTacToe-Erlang

Version using [Erlang](https://www.erlang.org/).

Run with:
```
erlc tictactoe.erl
escript tictactoe.escript 4 4
```

## TicTacToe-F#

Version using [F#](https://fsharp.org/).

To compile and run:
```bash
dotnet build
dotnet run -- -X 4 -O 4
```

## TicTacToe-Pascal

Version using [Pascal](https://en.wikipedia.org/wiki/Pascal_(programming_language)), specifically [FreePascal](https://www.freepascal.org/).

To build and run:
```
lazbuild tictactoe.lpi
./tictactoe
```

## TicTacToe-Turnstyle

Version using [Turnstyle](https://jaspervdj.be/turnstyle/).

Written as lambda expression, then compiled with turnstyle `cabal run turnstyle compile -O tictactoe.txt --output tictactoe.png` and finally hand optimized.

Does not handle invalid (out-of-bounds or overwriting) inputs.

Run with:
```
cabal run turnstyle run tictactoe.png
```

## TicTacToe-Tcl

Version using [Tcl](https://www.tcl.tk/).

Run with:
```
tclsh tictactoe.tcl
```

## TicTacToe-Piet

Version using [Piet](https://www.dangermouse.net/esoteric/piet.html).

Developed using [MasterPiets](https://gabriellesc.github.io/piet/index.html), [Pietron](https://github.com/dnek/pietron) and [piet_programming_language](https://github.com/your-diary/piet_programming_language).

Logic of the individual components is defined in [PietLogic.txt](tictactoe_piet/PietLogic.txt).

Run with:
```
piet_programming_language tictactoe.png
```


## TicTacToe-Intercal

Version in [INTERCAL](https://en.wikipedia.org/wiki/INTERCAL), specifically using [C-INTERCAL](https://gitlab.com/esr/intercal).

Adopted from [BoundedBeans](https://esolangs.org/wiki/User:BoundedBeans/INTERCAL_Tic-Tac-Toe).

Compile and run with:
```
ick -Ofb tictactoe.i
./tictactoe
```

## TicTacToe-Shakespeare

Version using [Shakespeare](https://en.wikipedia.org/wiki/Shakespeare_Programming_Language), specifically [shakespearelang](https://shakespearelang.com/1.0/).

Run with:
```
shakespeare run tictactoe.spl
```

## TicTacToe-scratch
Very simple two player tictactoe game with Scratch.

To play load it on the [Scratch website](https://scratch.mit.edu/)

## TicTacToe-Analog-Polarization-WZ

Analog version leveraging the quantum properties of light using polarization filters.

To get and play:
```bash
./get_bachelor_of_science_degree_in_physics
./get_master_of_science_degree_in_physics
./study_polarization_of_WZ_bosons
./get_doctor_rerum_naturalium_in_physics
./have_great_colleagues --Erik --Frank --Lisa --Mareen --Maren --Max --Orçun --Tim
```
