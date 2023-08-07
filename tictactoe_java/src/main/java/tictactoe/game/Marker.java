package tictactoe.game;

/**
 * Class that holds the char/marker of the currently active player.
 */
public class Marker {
    /**
     * Char that this marker wraps.
     */
    private final char marker;

    /**
     * @return the marker
     */
    public char getMarker() {
        return marker;
    }

    /**
     * Constructor. Marker always starts as 'X' in normal game.
     */
    public Marker() {
        this.marker = 'X';
    }

    /**
     * Constructor with custom marker.
     * Used for tests.
     * @param playerMarker custom marker.
     */
    public Marker(final char playerMarker) {
        this.marker = playerMarker;
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
     * @return Returns the opposite marker between 'X' and 'O'.
     */
    public char swappedMarker() {
        if (marker == 'X') {
            return 'O';
        }
        return 'X';
    }


}
