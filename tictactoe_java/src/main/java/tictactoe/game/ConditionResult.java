package tictactoe.game;

import java.util.LinkedList;

/**
 * Class for holding the result of a condition check.
 * Holds the number of spots that are fulfilled by the player for a given
 * condition.
 * As well as the list of spots that are still unoccupied (neither player has
 * claimed.)
 */
public class ConditionResult {
    /**
     * Number of spots of this condition that have been fulfilled.
     */
    private int spotsDone;
    /**
     * Spots of the condition that are not occupied by
     * either player.
     */
    private LinkedList<Integer> spotsOpen;


    /**
     * @return the spotsDone
     */
    public int getSpotsDone() {
        return spotsDone;
    }

    /**
     * @param done the spotsDone to set
     */
    public void setSpotsDone(final int done) {
        this.spotsDone = done;
    }

    /**
     * Increment spotsDone by the given value.
     * @param additional Value to increment spotsDone by.
     */
    public void addToSpotsDone(final int additional) {
        spotsDone += additional;
    }

    /**
     * @return the spotsOpen
     */
    public LinkedList<Integer> getSpotsOpen() {
        return spotsOpen;
    }

    /**
     * @param open the spotsOpen to set
     */
    public void setSpotsOpen(final LinkedList<Integer> open) {
        this.spotsOpen = open;
    }


    /**
     * Constructor initializing the finished spots as 0 and
     * the open spots and an empty linked list.
     */
    public ConditionResult() {
        this.spotsDone = 0;
        this.spotsOpen = new LinkedList<Integer>();
    }
}
