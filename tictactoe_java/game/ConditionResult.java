package tictactoe_java.game;

import java.util.LinkedList;

/**
 * Class for holding the result of a condition check.
 * Holds the number of spots that are fulfilled by the player for a given condition.
 * As well as the list of spots that are still unoccupied (neither player has claimed.)
 */
public class ConditionResult {
    int spotsDone;
    LinkedList<Integer> spotsOpen;

    /**
     * Constructor initializing the finished spots as 0 and
     * the open spots and an empty linked list.
     */
    public ConditionResult() {
        spotsDone = 0;
        spotsOpen = new LinkedList<Integer>();
    }
}
