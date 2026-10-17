package tictactoe;

import static org.junit.jupiter.api.Assertions.assertEquals;

import org.junit.jupiter.api.Test;

import java.util.LinkedList;

import tictactoe.game.ConditionResult;

public class ConditionResultTest {

    @Test
    public void testConditionResult() {
        ConditionResult conditionResult = new ConditionResult();
        assertEquals(0, conditionResult.getSpotsDone());
        assertEquals(new LinkedList<Integer>(), conditionResult.getSpotsOpen());
    }
}
