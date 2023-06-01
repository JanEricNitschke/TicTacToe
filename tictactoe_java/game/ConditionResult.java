package tictactoe_java.game;

import java.util.LinkedList;

public class ConditionResult {
    int spotsDone;
    LinkedList<Integer> spotsOpen;

    public ConditionResult() {
        spotsDone = 0;
        spotsOpen = new LinkedList<Integer>();
    }
}
