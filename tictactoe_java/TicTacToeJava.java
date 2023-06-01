package tictactoe_java;

import tictactoe_java.game.Board;
import tictactoe_java.game.HumanPlayer;

// Lets do it with a 1D array this time
public class TicTacToeJava {
  public static char swapMarker(char marker) {
    if (marker == 'X') {
      return 'O';
    }
    return 'X';
  }

  void playGame() {
    Board board = new Board();
    char marker = 'X';
    HumanPlayer humanPlayer = new HumanPlayer();
    while (true) {
      humanPlayer.makeMove(marker, board);
      if (gameOver(marker, board)) {
        break;
      }
      marker = swapMarker(marker);
    }
    board.showBoard();
  }

  boolean gameOver(char marker, Board board) {
    if (board.playerWin(marker)) {
      System.out.println("Player "+ marker +" wins the game!");
      return true;
    }
    if (board.boardFull()) {
      System.out.println("Match Draw!");
      return true;
    }
    return false;
  }

  public static void main(String[] args) {
    TicTacToeJava tictactoe = new TicTacToeJava();
    tictactoe.playGame();
  }
}
