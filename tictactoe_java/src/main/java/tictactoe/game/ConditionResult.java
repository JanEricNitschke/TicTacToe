package tictactoe.game;

import java.util.LinkedList;

/** Holds the result of a condition check: fulfilled and open spots. */
public class ConditionResult {
    /** Number of spots of this condition that have been fulfilled. */
    private int spotsDone;

    /** Spots of the condition that are not occupied by either player. */
    private LinkedList<Integer> spotsOpen;

    /** Initializes spotsDone as 0 and spotsOpen as an empty list. */
    public ConditionResult() {
        this.spotsDone = 0;
        this.spotsOpen = new LinkedList<Integer>();
    }

    /** Returns the spotsDone. */
    public int getSpotsDone() {
        return spotsDone;
    }

    /** Increment spotsDone by the given value. */
    public void addToSpotsDone(final int additional) {
        spotsDone += additional;
    }

    /** Returns the spotsOpen. */
    public LinkedList<Integer> getSpotsOpen() {
        return spotsOpen;
    }
}
