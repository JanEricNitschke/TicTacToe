package tictactoe_java.game;


/**
 * Class that holds the char/marker of the currently active player
 */
public class Marker {
    public char marker;

    /**
     * Constructor. Marker always starts as 'X'.
     */
    public Marker() {
        marker = 'X';
    }

    /**
     * Swap the marker field between 'X' and 'O'
     *
     * @return Return the instance after swappings its marker field.
     */
    public Marker swapMarker() {
        marker = swappedMarker();
        return this;
    }

    /**
     * Get the value of the opposite marker.
     *
     * @return Returns the opposite marker between 'X' and 'O'
     */
    public char swappedMarker() {
        if (marker == 'X') {
            return 'O';
        }
        return 'X';
    }
}
