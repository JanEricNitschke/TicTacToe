package tictactoe.game;

/** Class that holds the char/marker of the currently active player. */
public class Marker {
    /** Char that this marker wraps. */
    private final char marker;

    /** Constructor. Marker always starts as 'X' in normal game. */
    public Marker() {
        this.marker = 'X';
    }

    /** Constructor with custom marker. Used for tests. */
    public Marker(final char playerMarker) {
        this.marker = playerMarker;
    }

    /** Returns the marker. */
    public char getMarker() {
        return marker;
    }

    /** Get a marker with a swapped character. */
    public Marker swapMarker() {
        return new Marker(this.swappedMarker());
    }

    /** Get the value of the opposite marker. */
    public char swappedMarker() {
        if (marker == 'X') {
            return 'O';
        }
        return 'X';
    }
}
