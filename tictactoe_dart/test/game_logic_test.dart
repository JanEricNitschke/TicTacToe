// Import the test package and Counter class

import "package:tictactoe_dart/main.dart";
import "package:test/test.dart";
import 'package:flutter/material.dart';

void main() {
  group("EndState", () {
    test("Unary minus works", () {
      expect(-EndState.loss, EndState.win);
      expect(-EndState.draw, EndState.draw);
      expect(-EndState.win, EndState.loss);
    });

    group("relation", () {
      test("less", () {
        expect(EndState.loss < EndState.draw, true);
        expect(EndState.draw < EndState.win, true);
        expect(EndState.loss < EndState.win, true);
      });

      test("less equal", () {
        expect(EndState.loss <= EndState.loss, true);
        expect(EndState.loss <= EndState.draw, true);
        expect(EndState.draw <= EndState.draw, true);
        expect(EndState.draw <= EndState.win, true);
        expect(EndState.loss <= EndState.win, true);
        expect(EndState.win <= EndState.win, true);
      });

      test("greater", () {
        expect(EndState.loss > EndState.draw, false);
        expect(EndState.draw > EndState.win, false);
        expect(EndState.loss > EndState.win, false);
      });

      test("greater equal", () {
        expect(EndState.loss >= EndState.loss, true);
        expect(EndState.loss >= EndState.draw, false);
        expect(EndState.draw >= EndState.draw, true);
        expect(EndState.draw >= EndState.win, false);
        expect(EndState.loss >= EndState.win, false);
        expect(EndState.win >= EndState.win, true);
      });
    });
  });

  group("Player", () {
    test("Swap works", () {
      expect(Player.X.swap(), Player.O);
      expect(Player.O.swap(), Player.X);
    });
  });

  group("Color", () {
    test("Invert works", () {
      expect(const Color.fromARGB(255, 255, 255, 255).invert(),
          const Color.fromARGB(255, 0, 0, 0));
    });
  });

  group("GameState", () {
    late GameState gameState;

    setUp(() {
      gameState = GameState();
    });

    test("Gamestate restart works", () {
      gameState.board = ["1", "2", "3"];
      gameState.winningSpots = {1, 2, 3};
      gameState.player = Player.O;

      gameState.restartGame();

      expect(gameState.board, equals(List.filled(9, "")));
      expect(gameState.winningSpots, equals(<int>[]));
      expect(gameState.player, Player.X);
    });

    test('Empty cells returns cells with "" in them', () {
      gameState.board = ["X", "X", "X", "X", "X", "X", "O", "", ""];
      expect(gameState.emptyCells(), equals([7, 8]));
    });

    group("Make move", () {
      test("Does nothing on taken spot", () {
        gameState.board = ["O", "X", "X", "X", "X", "X", "O", "", ""];
        gameState.makeMove(0);
        expect(gameState.player, Player.X);
        expect(gameState.board[0], "O");
      });

      test("Takes open spot", () {
        gameState.board = ["O", "X", "X", "X", "X", "X", "O", "", ""];
        gameState.makeMove(7);
        expect(gameState.board[7], "X");
      });

      test("Swaps player when not done", () {
        gameState.board = ["", "", "", "", "", "", "", "", ""];
        gameState.makeMove(7);
        expect(gameState.player, Player.O);
      });

      test("Does not swap when game over", () {
        gameState.board = ["O", "X", "X", "X", "X", "X", "O", "", "X"];
        gameState.makeMove(7);
        expect(gameState.player, Player.X);
      });
    });

    group("IsPlayerWin", () {
      test("accepts rows", () {
        gameState.board = ["X", "X", "X", "", "", "", "", "", ""];
        expect(gameState.isPlayerWin(Player.X), true);
        expect(gameState.isPlayerWin(Player.O), false);

        gameState.board = ["", "", "", "O", "O", "O", "", "", ""];
        expect(gameState.isPlayerWin(Player.O), true);
        expect(gameState.isPlayerWin(Player.X), false);

        gameState.board = ["", "", "", "", "", "", "X", "X", "X"];
        expect(gameState.isPlayerWin(Player.X), true);
        expect(gameState.isPlayerWin(Player.O), false);
      });

      test("accepts columns", () {
        gameState.board = ["X", "", "", "X", "", "", "X", "", ""];
        expect(gameState.isPlayerWin(Player.X), true);
        expect(gameState.isPlayerWin(Player.O), false);

        gameState.board = ["", "O", "", "", "O", "", "", "O", ""];
        expect(gameState.isPlayerWin(Player.O), true);
        expect(gameState.isPlayerWin(Player.X), false);

        gameState.board = ["", "", "X", "", "", "X", "", "", "X"];
        expect(gameState.isPlayerWin(Player.X), true);
        expect(gameState.isPlayerWin(Player.O), false);
      });

      test("accepts diagonals", () {
        gameState.board = ["X", "", "", "", "X", "", "", "", "X"];
        expect(gameState.isPlayerWin(Player.X), true);
        expect(gameState.isPlayerWin(Player.O), false);

        gameState.board = ["", "", "O", "", "O", "", "O", "", ""];
        expect(gameState.isPlayerWin(Player.O), true);
        expect(gameState.isPlayerWin(Player.X), false);
      });
    });

    group("game over", () {
      test("does nothing on fail", () {
        gameState.board = List.filled(9, "");
        gameState.playState = PlayState.playing;
        final over = gameState.gameOver();
        expect(gameState.playState, PlayState.playing);
        expect(over, false);
      });

      test("detects draw", () {
        gameState.board = ["X", "X", "O", "O", "O", "X", "X", "O", "X"];
        gameState.playState = PlayState.playing;
        final over = gameState.gameOver();
        expect(gameState.playState, PlayState.draw);
        expect(over, true);
      });

      test("detects win", () {
        gameState.board = ["X", "X", "X", "", "", "", "", "", ""];
        gameState.playState = PlayState.playing;
        final over = gameState.gameOver();
        expect(gameState.playState, PlayState.win);
        expect(over, true);
      });
    });

    group("check wincondition", () {
      test("handles finished condition", () {
        gameState.board = ["X", "X", "", "", "", "", "", "", "X"];
        final status = gameState.checkWincondition([0, 1, 8], Player.X);
        expect(status.done, equals([0, 1, 8]));
        expect(status.open, equals(<int>[]));
      });

      test("handles open condition", () {
        gameState.board = ["", "", "", "", "", "", "", "", "X"];
        final status = gameState.checkWincondition([0, 1, 2], Player.O);
        expect(status.done, equals(<int>[]));
        expect(status.open, equals([0, 1, 2]));
      });

      test("handles partially completed condition", () {
        gameState.board = ["O", "", "", "", "", "", "", "", "X"];
        final status = gameState.checkWincondition([0, 4, 8], Player.O);
        expect(status.done, equals([0]));
        expect(status.open, equals([4]));
      });
    });

    test("winning fields works", () {
      gameState.board = ["X", "O", "O", "X", "O", "O", "X", "X", "X"];
      final doneFields = gameState.winningFields();
      expect(doneFields, equals({0, 3, 6, 7, 8}));
    });

    group("aiMove", () {
      test("swaps player when not done", () {
        gameState.board = ["", "", "", "", "", "", "", "", ""];
        gameState.aiMove();
        expect(gameState.player, Player.O);
      });

      test("does not swap when game over", () {
        gameState.board = ["O", "X", "X", "X", "X", "X", "O", "", "X"];
        gameState.aiMove();
        expect(gameState.player, Player.X);
      });

      group("randomMove", () {
        for (final _ in Iterable.generate(100)) {
          test("makes a valid move", () {
            gameState.board = ["O", "", "X", "", "", "X", "O", "", "X"];
            final doneMove = gameState.randomMove();
            expect(doneMove.endState, EndState.loss);
            expect([1, 3, 4, 7].contains(doneMove.spot), true);
          });
        }
      });

      group("winMove", () {
        test("finds win", () {
          gameState.board = ["X", "X", "", "", "", "", "", "", ""];
          gameState.difficulty = Difficulty.medium;
          final move = gameState.winMove(Player.X);
          expect(move.spot, 2);
          expect(move.endState, EndState.loss);
        });

        for (final _ in Iterable.generate(100)) {
          test("falls back to random", () {
            gameState.board = ["O", "", "X", "", "", "", "", "", ""];
            final doneMove = gameState.winMove(Player.X);
            expect(doneMove.endState, EndState.loss);
            expect([1, 3, 4, 5, 6, 7, 8].contains(doneMove.spot), true);
          });
        }
      });

      group("winBlockMove", () {
        test("finds win", () {
          gameState.board = ["X", "X", "", "", "", "", "", "", ""];
          gameState.difficulty = Difficulty.medium;
          final move = gameState.winBlockMove(Player.X);
          expect(move.spot, 2);
          expect(move.endState, EndState.loss);
        });

        test("finds block", () {
          gameState.board = ["X", "X", "", "", "", "", "", "", ""];
          gameState.difficulty = Difficulty.medium;
          final move = gameState.winBlockMove(Player.O);
          expect(move.spot, 2);
          expect(move.endState, EndState.loss);
        });

        for (final _ in Iterable.generate(100)) {
          test("falls back to random", () {
            gameState.board = ["O", "", "X", "", "", "", "", "", ""];
            final doneMove = gameState.winBlockMove(Player.X);
            expect(doneMove.endState, EndState.loss);
            expect([1, 3, 4, 5, 6, 7, 8].contains(doneMove.spot), true);
          });
        }
      });

      group("minMax", () {
        test("takes open spot", () {
          gameState.board = ["X", "X", "", "O", "X", "O", "X", "O", "O"];
          final bestMove = gameState.minMax(Player.X);
          expect(bestMove.spot, 2);
        });

        test("blocks win", () {
          gameState.board = ["O", "O", "X", "X", "", "O", "", "O", "X"];
          final bestMove = gameState.minMax(Player.X);
          expect(bestMove.spot, 4);
        });

        test("takes win", () {
          gameState.board = ["O", "O", "X", "X", "", "", "", "O", "X"];
          final bestMove = gameState.minMax(Player.O);
          expect(bestMove.spot, 4);
        });

        test("makes best move", () {
          // Correctly block middle
          // Anything else can lead to a loss
          gameState.board = ["O", "", "", "", "", "", "", "", ""];
          final bestMove = gameState.minMax(Player.X);
          expect(bestMove.spot, 4);
          expect(bestMove.endState, EndState.draw);
        });
      });
    });
  });
}
