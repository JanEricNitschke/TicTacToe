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

    public Marker(char playerMarker) {
        marker = playerMarker;
    }

    /**
     * Get a marker with a swapped character.
     *
     * @return Return the marker with the swapped character.
     */
    public Marker swapMarker() {
        return new Marker(this.swappedMarker());
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
