# TicTacToe
Collection of tictactoe games written in different langues.


## TicTacToe-python
Simple tictactoe game with Python

Test with:
```bash
coverage run -m pytest
coverage report -m
coverage html
```

## TicTacToe-javascript
Simple tictactoe game with Javascript

Hosted [here](https://main.dwbh88xp4gd1m.amplifyapp.com/) on AWS.

## TicTacToe-Cpp
Simple tictactoe game with C++

To build and test run:

```bash
cmake -S . -B build
cmake --build build
cd build && ctest
```
## TicTacToe-rust
Simple tictactoe game with Rust

To build and test run:
```bash
cargo build
cargo test
```

## TicTacToe-haskell
Simple tictactoe game with Haskell

To build and test run:
```bash
cabal build
cabal test --enable-coverage
```

## TicTacToe-go
Simple tictactoe game with Go

To build and test run:
```bash
go build
go test -covermode=count -coverprofile=coverage.out
go tool cover -html=coverage.out -o coverage.html
go test -fuzz=FuzzSwapPlayer -fuzztime 30s
go test -fuzz=FuzzGetEmptyCells -fuzztime 30s
go test -fuzz=FuzzRandomMove -fuzztime 30s
go test -fuzz=FuzzMinMax -fuzztime 60s
```

## TicTacToe-ruby
Simple tictactoe game with HRuby

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