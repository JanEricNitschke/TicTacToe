package tictactoe.game;

import java.util.Scanner;
import java.util.InputMismatchException;

/**
 * Class representing a human player.
 */
public class HumanPlayer implements Player {
    /**
     * Scanner to use to read in values from the user.
     */
    private final Scanner moveScanner;

    /**
     * Constructor. Set the scanner for taking user input.
     *
     * @param scanner Scanner for getting user input.
     */
    public HumanPlayer(final Scanner scanner) {
        this.moveScanner = scanner;
    }

    /**
     * Ask a user for input and make a move based on that.
     * Move had to be valid integer in rane [0-8] that is not yet
     * occupied.
     *
     * @param marker Marker that the player makes a move as.
     * @param board  Game board currently being played on.
     */
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
