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
Simple tictactoe game with [Javascript](https://www.ecma-international.org/publications-and-standards/standards/ecma-262/).

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

## TicTacToe-scratch
Very simple two player tictactoe game with Scratch

To play load it on the [Scratch website](https://scratch.mit.edu/)
