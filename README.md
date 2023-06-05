# TicTacToe
Collection of tictactoe games written in different langues.


## TicTacToe-python
Simple tictactoe game with Python

Rund and test with:
```bash
python tictactoe_python
coverage run -m pytest
coverage report -m
coverage html
```

## TicTacToe-javascript
Simple tictactoe game with Javascript

Hosted [here](https://main.dwbh88xp4gd1m.amplifyapp.com/) on AWS.

## TicTacToe-Cpp
Simple tictactoe game with C++

To build, test and run:

```bash
cmake -S . -B build
cmake --build build
cd build && ctest
cd .. && ./bin/tictactoe
```
## TicTacToe-rust
Simple tictactoe game with Rust

To build, test and run:
```bash
cargo build
cargo test
cargo run
```

## TicTacToe-haskell
Simple tictactoe game with Haskell

To build, test and run:
```bash
cabal build
cabal test --enable-coverage
cabal run
```

## TicTacToe-go
Simple tictactoe game with Go

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
Simple tictactoe game with Java

To install, test and run:
```bash
mvn package
mvn test
java -jar target/tictactoe_java-0.1.0.jar
```

## TicTacToe-ruby
Simple tictactoe game with Ruby

To build and test run:
```bash
gem build tictactoe_ruby.gemspec
bundle install
gem install tictactoe_ruby
bundle exec rake
./bin/tictactoeRuby
```

## TicTacToe-scratch
Very simple two player tictactoe game with Scratch

To play load it on the [Scratch website](https://scratch.mit.edu/)
