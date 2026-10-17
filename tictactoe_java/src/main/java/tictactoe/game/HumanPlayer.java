package tictactoe.game;

import java.util.Scanner;
import java.util.InputMismatchException;

/** Class representing a human player. */
public class HumanPlayer implements Player {
    /** Scanner to use to read in values from the user. */
    private final Scanner moveScanner;

    /** Constructor. Set the scanner for taking user input. */
    public HumanPlayer(final Scanner scanner) {
        this.moveScanner = scanner;
    }

    /** Ask the user for input and make a move based on that. */
    public void makeMove(final Board board, final Marker marker) {
        System.out.println("Player " + marker.getMarker() + " turn.");
        board.showBoard();
        int spot;
        boolean validInput = false;
        while (!validInput) {
            try {
                System.out.println("Where to make your next move?[0-8]");
                spot = moveScanner.nextInt();
                validInput = board.fixSpot(spot, marker);
            } catch (InputMismatchException exception) {
                System.out.println("ERROR: Input must be a valid integer!");
                moveScanner.next();
            }
        }
    }
}
