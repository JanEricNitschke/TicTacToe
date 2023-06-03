package tictactoe_java;

import java.util.Scanner;
import java.util.InputMismatchException;

import tictactoe_java.game.Board;
import tictactoe_java.game.HumanPlayer;
import tictactoe_java.game.AIPlayer;
import tictactoe_java.game.Marker;

/**
 * Class for playing TicTacToe.
 */
public class TicTacToeJava {

  /**
   * Game loop of tictactoe.
   * Initializes the board, marker and players.
   * And then lets players make their moves.y
   */
  void playGame(HumanPlayer humanPlayer, AIPlayer aiPlayer) {
    Board board = new Board();
    Marker marker = new Marker();
    while (true) {
      if (aiPlayer != null && aiPlayer.aiMarker == marker.marker) {
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
  public boolean getUserYesNo(String question, Scanner scanner) {
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
  boolean getAIOpponentExists(Scanner scanner) {
    return getUserYesNo("Play alone vs AI?[y/n]", scanner);
  }

  /**
   * Get the character that the AI plays as depending on if it goes first.
   *
   * @param scanner Scanner to read the user input.
   * @return Character that the AI will play as.
   */
  char getAIOpponentStart(Scanner scanner) {
    if (getUserYesNo("Should the AI make the first move?[y/n]", scanner)) {
      return 'X';
    }
    return 'O';
  }

  int getAIOpponentDifficulty(Scanner scanner) {
    int difficulty;
    String failureString = "ERROR: Input must be a valid integer in range [1-4]!";
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
  boolean gameOver(Marker marker, Board board) {
    if (board.playerWin(marker)) {
      System.out.println("Player " + marker.marker + " wins the game!");
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
  public static void main(String[] args) {
    TicTacToeJava tictactoe = new TicTacToeJava();
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
