package tictactoe_java;

import java.util.InputMismatchException;
import java.util.Scanner;
import tictactoe_java.game.Board;

// Lets do it with a 1D array this time
class TicTacToeJava {
  public static void main(String[] args) {
    Board myBoard = new Board();
    myBoard.showBoard();

    System.out.println("This is my tictactoe package!");

    Scanner myScanner = new Scanner(System.in);
    System.out.println("Play alone? [y/n]");
    String alone = myScanner.nextLine();
    System.out.println("Your answer was: " + alone);

    byte spot = 0;
    boolean validInput = false;
    while (!validInput) {
      try {
        System.out.println("Where to make your next move?");
        spot = myScanner.nextByte();
        validInput = true;
      } catch (InputMismatchException exception) {
        System.out.println("ERROR: Input must be a valid byet integer!");
        myScanner.next();
      }
    }
    System.out.println("Your answer was: " + spot);
    myScanner.close();
  }
}
