package tictactoe_java.game;

import static tictactoe_java.TicTacToeJava.inputScanner;
import java.util.InputMismatchException;

public class HumanPlayer implements Player {
    public void makeMove(char marker, Board board) {
        System.out.println("Player " + marker + " turn.");
        board.showBoard();
        int spot;
        boolean validInput = false;
        while (!validInput) {
            try {
                System.out.println("Where to make your next move?[0-8]");
                spot = inputScanner.nextInt();
                validInput = board.fixSpot(spot, marker);
                System.out.println("Got: " + spot);
                System.out.println("ValidInput?: " + validInput);
            } catch (InputMismatchException exception) {
                System.out.println("ERROR: Input must be a valid integer!");
                inputScanner.next();
            }
        }
    }
}