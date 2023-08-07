import 'package:flutter/material.dart';
import 'package:provider/provider.dart';
import 'package:intl/intl.dart' show toBeginningOfSentenceCase;
import "dart:math";

/// Describes AI strength.
///
/// Used to determine which algorithm to use
/// when making an aiMove.
enum Difficulty { easy, medium, hard, impossible }

/// Describes the state of the game.
///
/// Used to determine whether game board is clickable,
/// description text and control button text and functionality.
enum PlayState { gameModeSelection, waitingForFirstMove, playing, draw, win }

/// Describes game mode.
///
/// Used to determine whether ai should act after human move.
enum GameMode { singlePlayer, twoPlayer }

/// Describes end state of the game for minMax algorithm.
enum EndState {
  loss,
  draw,
  win;

  EndState operator -() {
    if (this == EndState.loss) {
      return EndState.win;
    } else if (this == EndState.win) {
      return EndState.loss;
    }
    return EndState.draw;
  }

  bool operator <(EndState other) {
    return index < other.index;
  }

  bool operator <=(EndState other) {
    return index <= other.index;
  }

  bool operator >(EndState other) {
    return index > other.index;
  }

  bool operator >=(EndState other) {
    return index >= other.index;
  }
}

enum Player {
  X,
  O;

  Player swap() {
    if (this == Player.X) {
      return Player.O;
    }
    return Player.X;
  }
}

extension InvertColor on Color {
  Color invert() {
    final r = 255 - red;
    final g = 255 - green;
    final b = 255 - blue;
    // return Color(0xFFFF0000);
    return Color.fromARGB((opacity * 255).round(), r, g, b);
  }
}

void main() {
  runApp(const TicTacToeApp());
}

class TicTacToeApp extends StatelessWidget {
  const TicTacToeApp({super.key});

  @override
  Widget build(BuildContext context) {
    return ChangeNotifierProvider(
      create: (context) => GameState(),
      child: MaterialApp(
        title: 'TicTacToe-Dart',
        theme: ThemeData(
          useMaterial3: true,
          colorScheme: ColorScheme.fromSeed(
            seedColor: Colors.deepPurple,
            brightness: Brightness.light,
          ),
        ),
        home: const GamePage(),
      ),
    );
  }
}

/// Handles the whole state of the TicTacToe game.
class GameState extends ChangeNotifier {
  /// Tracks ai strength.
  var difficulty = Difficulty.easy;

  /// State that the game is currently in.
  var _playState = PlayState.gameModeSelection;

  /// Sets state and notifies listeners.
  PlayState get playState => _playState;
  set playState(PlayState state) {
    _playState = state;
    notifyListeners();
  }

  /// Tracks the board of the TicTacToe game.
  ///
  /// "" indicates unoccupied spots.
  var board = List.filled(9, "");

  /// Player whose turn it currently is.
  var player = Player.X;

  /// List of index lists that a player has to occupy to win.
  var winConditions = [
    // Rows
    [0, 1, 2],
    [3, 4, 5],
    [6, 7, 8],
    // Cols
    [0, 3, 6],
    [1, 4, 7],
    [2, 5, 8],
    // Diagonals
    [0, 4, 8],
    [2, 4, 6]
  ];
  var gameMode = GameMode.twoPlayer;

  /// If a player has won, this stores the spots that caused the win.
  var winningSpots = <int>{};
  void adjustDifficulty(Difficulty? value) {
    difficulty = value!;
    notifyListeners();
  }

  /// Occupy [spot] for [player] and check if game is over.
  ///
  /// Called when the player clicks a tile of the board.
  /// Returns if [spot] is already occupied.
  /// If not: Captures it for the player and checks if the game is over.
  /// If the game is not over the active [player] is swapped.
  /// Afterwards performs an [aiMove] if [gameMode] is [GameMode.singlePlayer].
  void makeMove(int spot) {
    playState = PlayState.playing;
    if (board[spot] == "") {
      board[spot] = player.name;
      if (!gameOver()) {
        player = player.swap();
        if (gameMode == GameMode.singlePlayer) {
          aiMove();
        }
      }
    }
    notifyListeners();
  }

  /// Performs a random valid move on the [board].
  ({int spot, EndState endState}) randomMove() {
    final empties = emptyCells();
    return (
      spot: empties[Random().nextInt(empties.length)],
      endState: EndState.loss
    );
  }

  /// Returns a list of all empty cells in [board].
  List<int> emptyCells() {
    var empties = <int>[];
    for (final (index, value) in board.indexed) {
      if (value == "") {
        empties.add(index);
      }
    }
    return empties;
  }

  /// Checks if [player] has won the game on the current [board].
  bool isPlayerWin(player) {
    for (final condition in winConditions) {
      final result = checkWincondition(condition, player);
      if (result.done.length == 3) {
        return true;
      }
    }
    return false;
  }

  /// Tries to find a winning move for [player] on the current [board].
  ({int spot, EndState endState})? _winningMove(Player player) {
    for (final condition in winConditions) {
      final status = checkWincondition(condition, player);
      if (status.done.length == 2 && status.open.length == 1) {
        return (spot: status.open[0], endState: EndState.loss);
      }
    }
    return null;
  }

  /// Returns a winning or random move for [player] on the current [board].
  ({int spot, EndState endState}) winMove(Player player) {
    return _winningMove(player) ?? randomMove();
  }

  /// Returns a winning, blocking or random move for [player] on the current [board].
  ({int spot, EndState endState}) winBlockMove(Player player) {
    return _winningMove(player) ?? _winningMove(player.swap()) ?? randomMove();
  }

  /// Returns an optimal move for [player] on the current [board].
  ({int spot, EndState endState}) minMax(Player player) {
    // Game already won
    if (isPlayerWin(player)) {
      return (spot: -1, endState: EndState.win);
    }

    // Game already lost
    if (isPlayerWin(player.swap())) {
      return (spot: -1, endState: EndState.loss);
    }

    final empties = emptyCells();
    // Game drawn
    if (empties.isEmpty) {
      return (spot: -1, endState: EndState.draw);
    }

    // First move, just do random
    if (empties.length == board.length) {
      return randomMove();
    }

    // Recursive cases
    var bestMove = (spot: -1, endState: EndState.loss);
    for (final cell in empties) {
      board[cell] = player.name;
      final currentMove = minMax(player.swap());
      if (-currentMove.endState >= bestMove.endState) {
        bestMove = (spot: cell, endState: -currentMove.endState);
      }
      board[cell] = "";
      if (bestMove.endState == EndState.win) {
        return bestMove;
      }
    }
    return (bestMove.endState >= EndState.draw)
        ? bestMove
        : winBlockMove(player);
  }

  /// Performs an aiMove with the algorithm determined by [difficulty].
  ///
  /// If the game is not over after the move then [player] is swapped.
  void aiMove() {
    var bestMove = switch (difficulty) {
      Difficulty.easy => randomMove(),
      Difficulty.medium => winMove(player),
      Difficulty.hard => winBlockMove(player),
      Difficulty.impossible => minMax(player),
    };
    board[bestMove.spot] = player.name;
    if (!gameOver()) {
      player = player.swap();
    }
    notifyListeners();
  }

  /// Checks if all spots on [board] are occupied.
  bool boardFilled() {
    return !board.any((value) => value == "");
  }

  /// Returns the occupied and open spots for a [condition] and [player].
  ///
  /// Returns the [board] indices in [condition] that are occupied by
  /// [player] as well as those that are unoccupied.
  /// Used to determine winning and blocking moves as well as
  /// highlight winning tiles.
  ({List<int> done, List<int> open}) checkWincondition(
      List<int> condition, Player player) {
    var status = (done: <int>[], open: <int>[]);
    for (final value in condition) {
      if (board[value] == player.name) {
        status.done.add(value);
      } else if (board[value] == "") {
        status.open.add(value);
      }
    }

    return status;
  }

  /// Finds the winning fields for the current [player].
  Set<int> winningFields() {
    var winningFields = <int>{};
    for (final condition in winConditions) {
      var filledFields = checkWincondition(condition, player);
      if (filledFields.done.length == 3) {
        winningFields.addAll(filledFields.done);
      }
    }

    return winningFields;
  }

  /// Checks if the game is over by win or draw.
  bool gameOver() {
    var doneFields = winningFields();
    if (doneFields.isNotEmpty) {
      playState = PlayState.win;
      winningSpots = doneFields;
      return true;
    }
    if (boardFilled()) {
      playState = PlayState.draw;
      return true;
    }
    return false;
  }

  /// Reset the game state to be able to start a new game.
  void restartGame() {
    board = List.filled(9, "");
    winningSpots.clear();
    player = Player.X;
    notifyListeners();
  }
}

class GamePage extends StatefulWidget {
  const GamePage({super.key});
  @override
  State<GamePage> createState() => _GamePageState();
}

class _GamePageState extends State<GamePage> {
  @override
  Widget build(BuildContext context) {
    return LayoutBuilder(builder: (context, constraint) {
      return Scaffold(
        body: Container(
          color: Theme.of(context).colorScheme.inversePrimary,
          child: const Center(
            child: IntrinsicWidth(
              child: Column(
                mainAxisAlignment: MainAxisAlignment.center,
                mainAxisSize: MainAxisSize.max,
                children: [
                  TitleWidget(),
                  GameInfo(),
                  GameBoard(),
                  GameStateSelection(),
                  DifficultySelection()
                ],
              ),
            ),
          ),
        ),
      );
    });
  }
}

/// Widget for choosing the strength of the ai.
class DifficultySelection extends StatelessWidget {
  const DifficultySelection({
    super.key,
  });

  @override
  Widget build(BuildContext context) {
    var appState = context.watch<GameState>();
    final theme = Theme.of(context);
    return Row(
      mainAxisAlignment: MainAxisAlignment.spaceEvenly,
      mainAxisSize: MainAxisSize.max,
      children: [
        Text(
          "Choose an AI difficulty:",
          style: theme.textTheme.bodyMedium!.copyWith(
            color: theme.colorScheme.onPrimaryContainer,
          ),
        ),
        DropdownButton<Difficulty>(
          value: appState.difficulty,
          icon: const Icon(Icons.arrow_downward),
          elevation: 16,
          style: TextStyle(color: theme.colorScheme.onPrimaryContainer),
          underline: Container(
            height: 2,
            color: theme.colorScheme.outline,
          ),
          onChanged: appState.adjustDifficulty,
          items: Difficulty.values
              .map<DropdownMenuItem<Difficulty>>((Difficulty value) {
            return DropdownMenuItem<Difficulty>(
              value: value,
              child: Text(toBeginningOfSentenceCase(value.name)!),
            );
          }).toList(),
        )
      ],
    );
  }
}

/// Widget to control the game state.
///
/// Contains two buttons that modify the GameStates playState
/// depending on its current value.
/// If the game is in [PlayState.gameModeSelection] then they select the game mode.
/// If the game is in [PlayState.waitingForFirstMove] and the game mode is
/// [GameMode.singlePlayer] then the left button performs an aiMove.
/// Otherwise the left button resets the game and the right button
/// goes set the game back to [PlayState.gameModeSelection].
class GameStateSelection extends StatelessWidget {
  const GameStateSelection({
    super.key,
  });

  @override
  Widget build(BuildContext context) {
    var appState = context.watch<GameState>();
    String leftButtonText;
    void Function()? leftButtonFunction;
    String rightButtonText;
    void Function()? rightButtonFunction;
    if (appState.playState == PlayState.gameModeSelection) {
      leftButtonText = "1 Player";
      leftButtonFunction = () {
        appState.gameMode = GameMode.singlePlayer;
        appState.playState = PlayState.waitingForFirstMove;
      };
      rightButtonText = "2 Player";
      rightButtonFunction = () {
        appState.gameMode = GameMode.twoPlayer;
        appState.playState = PlayState.waitingForFirstMove;
      };
    } else {
      if (appState.gameMode == GameMode.singlePlayer &&
          appState.playState == PlayState.waitingForFirstMove) {
        leftButtonText = "AI turn";
        leftButtonFunction = () {
          appState.aiMove();
        };
      } else {
        leftButtonText = "Restart";
        leftButtonFunction = () {
          appState.playState = PlayState.waitingForFirstMove;
          appState.restartGame();
        };
      }
      rightButtonText = "Change game mode";
      rightButtonFunction = () {
        appState.playState = PlayState.gameModeSelection;
        appState.restartGame();
      };
    }

    return Row(
      mainAxisAlignment: MainAxisAlignment.spaceEvenly,
      mainAxisSize: MainAxisSize.max,
      children: [
        ElevatedButton(
          onPressed: leftButtonFunction,
          child: Text(leftButtonText),
        ),
        ElevatedButton(
          onPressed: rightButtonFunction,
          child: Text(rightButtonText),
        ),
      ],
    );
  }
}

/// Widget containing the game board of size [gameSize].
class GameBoard extends StatelessWidget {
  const GameBoard({
    super.key,
  });

  final int gameSize = 3;

  @override
  Widget build(BuildContext context) {
    return Expanded(
      child: Column(
        mainAxisAlignment: MainAxisAlignment.center,
        children: List.generate(gameSize, (row) {
          return Flexible(
            child: Row(
              mainAxisAlignment: MainAxisAlignment.center,
              children: List.generate(gameSize, (col) {
                var spot = 3 * row + col;
                return BoardTile(spot: spot);
              }),
            ),
          );
        }),
      ),
    );
  }
}

/// Widget contain game tiles for each [spot].
///
/// During [PlayState.playing] clicking them causes the active player to make
/// a move there.
class BoardTile extends StatelessWidget {
  const BoardTile({
    super.key,
    required this.spot,
  });

  final int spot;

  @override
  Widget build(BuildContext context) {
    var appState = context.watch<GameState>();
    final theme = Theme.of(context);

    var buttonColor = appState.winningSpots.contains(spot)
        ? theme.colorScheme.primary.invert()
        : theme.colorScheme.primary;

    void Function()? buttonFunction;
    if (appState.playState == PlayState.gameModeSelection) {
      buttonFunction = null;
    } else if (appState.playState == PlayState.win ||
        appState.playState == PlayState.draw) {
      buttonFunction = () {};
    } else {
      // Waiting or playing
      buttonFunction = () {
        appState.makeMove(spot);
      };
    }

    return Flexible(
      child: Padding(
        padding: const EdgeInsets.all(8.0),
        child: AspectRatio(
          aspectRatio: 1,
          child: ElevatedButton(
            onPressed: buttonFunction,
            style: ButtonStyle(
              shape: MaterialStateProperty.all<RoundedRectangleBorder>(
                RoundedRectangleBorder(
                  borderRadius: BorderRadius.circular(20.0),
                ),
              ),
            ),
            child: Text(
              appState.board[spot],
              style: theme.textTheme.displayLarge!.copyWith(
                color: buttonColor,
              ),
            ),
          ),
        ),
      ),
    );
  }
}

/// Simple widget displaying information about the current game state.
class GameInfo extends StatelessWidget {
  const GameInfo({
    super.key,
  });

  @override
  Widget build(BuildContext context) {
    final theme = Theme.of(context);
    var appState = context.watch<GameState>();
    var infoText = switch (appState.playState) {
      PlayState.gameModeSelection => "Select a game mode",
      PlayState.waitingForFirstMove ||
      PlayState.playing =>
        "Player ${appState.player.name}'s turn",
      PlayState.draw => "Draw!",
      PlayState.win => "Player ${appState.player.name} wins!",
    };

    return Text(
      infoText,
      style: theme.textTheme.headlineMedium!.copyWith(
        color: theme.colorScheme.secondary,
      ),
    );
  }
}


/// Simple widget displaying the title of the app.
class TitleWidget extends StatelessWidget {
  const TitleWidget({
    super.key,
  });

  @override
  Widget build(BuildContext context) {
    final theme = Theme.of(context);
    return Text(
      "TicTacToe-Dart",
      style: theme.textTheme.displayLarge!.copyWith(
        color: theme.colorScheme.primary,
      ),
    );
  }
}
