package tictactoe_java;

import static org.junit.jupiter.api.Assertions.assertEquals;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

import java.util.LinkedList;

import tictactoe_java.game.ConditionResult;

public class ConditionResultTest {

    @Test
    @DisplayName("Constructor for ConditionResult should work.")
    public void testConditionResult() {
        ConditionResult conditionResult = new ConditionResult();
        assertEquals(0, conditionResult.spotsDone);
        assertEquals(new LinkedList<Integer>(), conditionResult.spotsOpen);
    }
}