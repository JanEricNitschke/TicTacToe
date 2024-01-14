package tictactoe;

import java.util.Scanner;
import java.util.InputMismatchException;

import tictactoe.game.Board;
import tictactoe.game.HumanPlayer;
import tictactoe.game.AIPlayer;
import tictactoe.game.Marker;

/**
 * Class for playing TicTacToe.
 */
public class TicTacToe {

  /**
   * Game loop of tictactoe.
   * Initializes the board, marker and players.
   * And then lets players make their moves.
   *
   * @param humanPlayer Human player playing the game.
   * @param aiPlayer    Optional AI player as an opponent.
   */
  void playGame(final HumanPlayer humanPlayer, final AIPlayer aiPlayer) {
    Board board = new Board();
    Marker marker = new Marker();
    while (true) {
      if (aiPlayer != null && aiPlayer.getAiMarker() == marker.getMarker()) {
        aiPlayer.makeMove(board, marker);
      } else {
        humanPlayer.makeMove(board, marker);
      }
      if (gameOver(marker, board)) {
        break;
      }
      marker = marker.swapMarker();
    }
    board.showBoard();
  }

  /**
   * Ask the user a yes/no question and return if the answer was yes.
   *
   * @param question Question to ask the user.
   * @param scanner  Scanner to read the user input.
   * @return Whether the user anwered in the affirmative.
   */
  public boolean getUserYesNo(final String question, final Scanner scanner) {
    char answer;
    while (true) {
      System.out.println(question);
      answer = scanner.next().charAt(0);
      if (Character.toUpperCase(answer) == 'Y') {
        return true;
      } else if (Character.toUpperCase(answer) == 'N') {
        return false;
      }

    }
  }

  /**
   * Ask the user if they want to play alone vs an AI.
   *
   * @param scanner Scanner to read the user input.
   * @return Whether the user will play vs an AI.
   */
  boolean getAIOpponentExists(final Scanner scanner) {
    return getUserYesNo("Play alone vs AI?[y/n]", scanner);
  }

  /**
   * Get the character that the AI plays as depending on if it goes first.
   *
   * @param scanner Scanner to read the user input.
   * @return Character that the AI will play as.
   */
  char getAIOpponentStart(final Scanner scanner) {
    if (getUserYesNo("Should the AI make the first move?[y/n]", scanner)) {
      return 'X';
    }
    return 'O';
  }

  /**
   * Get the strength of the ai from the player.
   *
   * @param scanner Scanner to use to get input from player.
   * @return Chosen strength of the AI.
   */
  int getAIOpponentDifficulty(final Scanner scanner) {
    int difficulty;
    String failureString = "ERROR: Input must be a"
        + " valid integer in range [1-4]!";
    System.out.println("AI strength settings:");
    System.out.println("1: Easy");
    System.out.println("2: Medium");
    System.out.println("3: Hard");
    System.out.println("4: Impossible");
    while (true) {
      try {
        System.out.println("How strong should the AI be?[1-4]");
        difficulty = scanner.nextInt();
        if (difficulty > 0 && difficulty < 5) {
          return difficulty;
        }
        System.out.println(failureString);
      } catch (InputMismatchException exception) {
        System.out.println(failureString);
        scanner.next();
      }
    }
  }

  /**
   * Checks whether the game is over.
   * Either due to the current player winning
   * or due to a draw.
   * Prints an appropriate message if the game is finished.
   *
   * @param marker Player that made the last move and can possibly be the winner
   * @param board  Game board currently being played on.
   * @return Whether the game is over or not.
   */
  boolean gameOver(final Marker marker, final Board board) {
    if (board.playerWin(marker)) {
      System.out.println("Player " + marker.getMarker() + " wins the game!");
      return true;
    }
    if (board.boardFull()) {
      System.out.println("Match Draw!");
      return true;
    }
    return false;
  }

  /**
   * Initializes a game and starts the game loop.
   *
   * @param args CL arguments. Not used.
   */
  public static void main(final String[] args) {
    TicTacToe tictactoe = new TicTacToe();
    Scanner inputScanner = new Scanner(System.in);
    HumanPlayer humanPlayer = new HumanPlayer(inputScanner);
    AIPlayer aiPlayer = null;
    if (tictactoe.getAIOpponentExists(inputScanner)) {
      char aiChar = tictactoe.getAIOpponentStart(inputScanner);
      int difficulty = tictactoe.getAIOpponentDifficulty(inputScanner);
      aiPlayer = new AIPlayer(aiChar, difficulty);
    }
    tictactoe.playGame(humanPlayer, aiPlayer);
  }
}
