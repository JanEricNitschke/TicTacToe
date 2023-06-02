package tictactoe_java;

import java.util.Scanner;

import tictactoe_java.game.Board;
import tictactoe_java.game.HumanPlayer;
import tictactoe_java.game.Marker;


/**
 * Class for playing TicTacToe.
 */
public class TicTacToeJava {

  /**
   * Game loop of tictactoe.
   * Initializes the board, marker and players.
   * And then lets players make their moves.
   */
  void playGame() {
    Scanner inputScanner = new Scanner(System.in);
    Board board = new Board();
    Marker marker = new Marker();
    HumanPlayer humanPlayer = new HumanPlayer(inputScanner);
    while (true) {
      humanPlayer.makeMove(marker, board);
      if (gameOver(marker, board)) {
        break;
      }
      marker = marker.swapMarker();
    }
    board.showBoard();
  }

  /**
   * Checks whether the game is over.
   * Either due to the current player winning
   * or due to a draw.
   * Prints an appropriate message if the game is finished.
   *
   * @param marker Player that made the last move and can possibly be the winner
   * @param board Game board currently being played on.
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
    tictactoe.playGame();
  }
}
