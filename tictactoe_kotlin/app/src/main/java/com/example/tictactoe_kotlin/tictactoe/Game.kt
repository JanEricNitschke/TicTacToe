package com.example.tictactoe_kotlin.tictactoe

import androidx.annotation.VisibleForTesting
import androidx.compose.runtime.mutableStateListOf
import androidx.compose.runtime.mutableStateOf
import androidx.compose.runtime.toMutableStateList

object Game {
    @VisibleForTesting
    internal var _board = mutableStateListOf("", "", "", "", "", "", "", "", "")
    val board: List<String> = _board
    var player = mutableStateOf("X")
    var gameState = mutableStateOf(GameState.Waiting)
    private val winConditions = arrayOf(
        // Rows
        arrayOf(0, 1, 2), arrayOf(3, 4, 5), arrayOf(6, 7, 8),
        // Cols
        arrayOf(0, 3, 6), arrayOf(1, 4, 7), arrayOf(2, 5, 8),
        // Diagonals
        arrayOf(0, 4, 8), arrayOf(2, 4, 6)
    )
    var winningSpots = mutableStateListOf<Int>()
    var aiOpponent = mutableStateOf(false)
    var aiStrength = mutableStateOf(0)

    fun swapPlayer(currentPlayer: String): String {
        if (currentPlayer == "X") {
            return "O"
        }
        return "X"
    }

    fun restartGame() {
        for (index in _board.indices) {
            _board[index] = ""
        }
        gameState.value = GameState.Playing
        winningSpots.clear()
        player.value = "X"
    }

    private fun boardFilled(): Boolean {
        return !_board.any { it == "" }
    }

    private fun checkWinCondition(condition: Array<Int>, currentPlayer: String): ConditionStatus {
        val status = ConditionStatus()
        condition.forEach {
            if (_board[it] == currentPlayer) {
                status.done.add(it)
            } else if (_board[it] == "") {
                status.open.add(it)
            }
        }
        return status
    }

    private fun isPlayerWin(currentPlayer: String): Boolean {
        return winConditions.any { checkWinCondition(it, currentPlayer).done.size == 3 }
    }

    private fun winningFields(): MutableList<Int> {
        val winningFields = mutableListOf<Int>()
        winConditions.forEach {
            val filledFields = checkWinCondition(it, player.value)
            if (filledFields.done.size == 3) {
                winningFields.addAll(filledFields.done)
            }
        }
        return winningFields
    }

    private fun gameOver(): Boolean {
        val winningFields = winningFields()
        if (winningFields.size > 0) {
            gameState.value = GameState.Win
            winningSpots = winningFields.toMutableStateList()
            return true
        }
        if (boardFilled()) {
            gameState.value = GameState.Draw
            return true
        }
        return false
    }

    private fun randomMove(): Move {
        return Move(emptyCells().random(), -1)
    }

    fun emptyCells(): MutableList<Int> {
        val empties = mutableListOf<Int>()
        _board.forEachIndexed { index, value ->
            if (value == "") {
                empties.add(index)
            }
        }
        return empties
    }


    private fun winningMove(currentPlayer: String): Move? {
        winConditions.forEach {
            val status = checkWinCondition(it, currentPlayer)
            if (status.done.size == 2 && status.open.size == 1) {
                return Move(status.open[0], -1)
            }
        }
        return null
    }

    private fun winMove(currentPlayer: String): Move {
        return winningMove(currentPlayer) ?: randomMove()
    }

    private fun winBlockMove(currentPlayer: String): Move {
        return winningMove(currentPlayer) ?: winningMove(swapPlayer(currentPlayer)) ?: randomMove()
    }

    private fun minMax(currentPlayer: String, board: MutableList<String>): Move {
        // Game already won
        if (isPlayerWin(currentPlayer)) {
            return Move(0, 1)
        }
        // Game already lost
        if (isPlayerWin(swapPlayer(currentPlayer))) {
            return Move(0, -1)
        }
        // Game drawn
        if (boardFilled()) {
            return Move(0, 0)
        }
        // New game
        val empties = emptyCells()
        if (empties.size == 9) {
            return randomMove()
        }

        var bestMove = Move(-1, -1)
        empties.forEach {
            _board[it] = currentPlayer
            val currentMove = minMax(swapPlayer(currentPlayer), board)
            if (-currentMove.endState >= bestMove.endState) {
                bestMove = Move(it, -currentMove.endState)
            }
            _board[it] = ""
            if (bestMove.endState == 1) {
                return bestMove
            }
        }
        return if (bestMove.endState >= 0) bestMove else winBlockMove(currentPlayer)
    }

    fun aiMove() {
        val bestMove: Move = when (aiStrength.value) {
            0 -> randomMove()
            1 -> winMove(player.value)
            2 -> winBlockMove(player.value)
            3 -> minMax(player.value, _board.toMutableList())
            else -> randomMove()
        }
        _board[bestMove.spot] = player.value
        if (gameOver()) {
            return
        }
        player.value = swapPlayer(player.value)
    }

    fun makeMove(spot: Int): Boolean {
        if (gameState.value == GameState.Playing && _board[spot] == "") {
            _board[spot] = player.value
            if (gameOver()) {
                return false
            }
            return true
        }
        return false
    }
}
