package com.example.tictactoe_kotlin.tictactoe

import androidx.compose.runtime.mutableStateListOf
import org.junit.Assert.assertEquals
import org.junit.Assert.assertFalse
import org.junit.Assert.assertTrue
import org.junit.Before
import org.junit.Test

internal class GameTest {

    @Before
    fun setUp() {
        Game.restartGame()
    }

    @Test
    fun swapPlayerTest() {
        assertEquals("X", Game.swapPlayer("O"))
        assertEquals("O", Game.swapPlayer("X"))
    }

    @Test
    fun restartGameTest() {
        Game.gameState.value = GameState.Win
        Game.aiStrength.value = 3
        Game.player.value = "O"
        Game.winningSpots = mutableStateListOf(1, 2, 3)
        Game.aiOpponent.value = true
        Game.restartGame()
        assertEquals(GameState.Playing, Game.gameState.value)
        assertEquals(3, Game.aiStrength.value)
        assertEquals("X", Game.player.value)
        assertEquals(listOf<Int>(), Game.winningSpots.toList())
        assertEquals(true, Game.aiOpponent.value)
    }

    @Test
    fun emptyCellsTest() {
        Game._board = mutableStateListOf("X", "X", "X", "X", "X", "X", "O", "", "")
        assertEquals(mutableListOf(7, 8), Game.emptyCells())
    }

    @Test
    fun aiMoveSwapPlayingTest() {
        Game._board = mutableStateListOf("X", "O", "X", "", "O", "", "O", "X", "X")
        Game.player.value = "O"
        Game.aiStrength.value = 0
        // Swaps when game keeps going
        Game.aiMove()
        assertEquals("X", Game.player.value)
    }

    @Test
    fun makeMoveValidTest() {
        Game.gameState.value = GameState.Playing
        Game._board = mutableStateListOf("X", "", "", "", "", "", "", "", "")
        // Valid move
        assertTrue(Game.makeMove(1))
    }

    @Test
    fun makeMoveTakenTest() {
        Game.gameState.value = GameState.Playing
        Game._board = mutableStateListOf("X", "X", "", "", "", "", "", "", "")
        // Spot taken
        assertFalse(Game.makeMove(1))
    }

    @Test
    fun makeMoveInvalidTest() {
        // Invalid game state
        Game.gameState.value = GameState.Waiting
        Game._board = mutableStateListOf("X", "X", "", "", "", "", "", "", "")
        assertFalse((Game.makeMove(2)))
    }

    @Test
    fun makeMoveOverTest() {
        // Game over
        Game.gameState.value = GameState.Playing
        Game._board = mutableStateListOf("X", "X", "", "", "", "", "", "", "")
        assertFalse(Game.makeMove(2))
    }

    @Test
    fun aiMoveSwapOverTest() {
        Game._board = mutableStateListOf("X", "O", "X", "O", "O", "", "O", "X", "X")
        Game.player.value = "X"
        Game.aiStrength.value = 0
        // Does not when game is over
        Game.aiMove()
        assertEquals("X", Game.player.value)
    }

    @Test
    fun aiMoveWinTest() {
        Game._board = mutableStateListOf("X", "O", "X", "", "O", "", "O", "X", "X")
        Game.player.value = "X"
        Game.aiStrength.value = 1
        Game.aiMove()
        assertEquals(listOf("X", "O", "X", "", "O", "X", "O", "X", "X"), Game._board.toList())
    }

    @Test
    fun aiMoveBlockTest() {
        Game._board = mutableStateListOf("X", "O", "X", "", "O", "", "O", "X", "X")
        Game.player.value = "O"
        Game.aiStrength.value = 2
        Game.aiMove()
        assertEquals(listOf("X", "O", "X", "", "O", "O", "O", "X", "X"), Game._board.toList())
    }

    @Test
    fun aiMoveMinMaxTakesOpenSpotTest() {
        Game.aiStrength.value = 3
        Game._board = mutableStateListOf("X", "X", "", "O", "X", "O", "X", "O", "O")
        Game.player.value = "X"
        Game.aiMove()
        assertEquals(listOf("X", "X", "X", "O", "X", "O", "X", "O", "O"), Game._board.toList())
    }

    @Test
    fun aiMoveMinMaxBlocksWinTest() {
        Game.aiStrength.value = 3
        Game._board = mutableStateListOf("O", "O", "X", "X", "", "O", "", "O", "X")
        Game.player.value = "X"
        Game.aiMove()
        assertEquals(listOf("O", "O", "X", "X", "X", "O", "", "O", "X"), Game._board.toList())
    }

    @Test
    fun aiMoveMinMaxTakesWinTest() {
        Game.aiStrength.value = 3
        Game._board = mutableStateListOf("O", "O", "X", "X", "", "", "", "O", "X")
        Game.player.value = "O"
        Game.aiMove()
        assertEquals(listOf("O", "O", "X", "X", "O", "", "", "O", "X"), Game._board.toList())
    }

    @Test
    fun aiMoveMinMaxBestTest() {
        // Correctly block middle
        // Anything else can lead to a loss
        Game.aiStrength.value = 3
        Game._board = mutableStateListOf("O", "", "", "", "", "", "", "", "")
        Game.player.value = "X"
        Game.aiMove()
        assertEquals(listOf("O", "", "", "", "X", "", "", "", ""), Game._board.toList())
    }
}
