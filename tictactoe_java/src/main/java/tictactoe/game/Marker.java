package tictactoe.game;

/** Class that holds the char/marker of the currently active player. */
public class Marker {
    /** Char that this marker wraps. */
    private final char marker;

    /** Constructor. Marker always starts as 'X' in normal game. */
    public Marker() {
        this.marker = 'X';
    }

    /** Constructor with custom marker. Used for tests.
     * @param playerMarker Custom marker.
     */
    public Marker(final char playerMarker) {
        this.marker = playerMarker;
    }

    /**
     * @return the marker
     */
    public char getMarker() {
        return marker;
    }

    /**
     * @return the marker with the swapped character
     */
    public Marker swapMarker() {
        return new Marker(this.swappedMarker());
    }

    /**
     * @return the opposite marker between 'X' and 'O'
     */
    public char swappedMarker() {
        if (marker == 'X') {
            return 'O';
        }
        return 'X';
    }
}
