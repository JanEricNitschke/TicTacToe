package com.example.tictactoe_kotlin

import android.os.Bundle
import androidx.activity.ComponentActivity
import androidx.activity.compose.setContent
import androidx.compose.foundation.background
import androidx.compose.foundation.clickable
import androidx.compose.foundation.layout.Arrangement
import androidx.compose.foundation.layout.Box
import androidx.compose.foundation.layout.Column
import androidx.compose.foundation.layout.Row
import androidx.compose.foundation.layout.aspectRatio
import androidx.compose.foundation.layout.fillMaxSize
import androidx.compose.foundation.layout.fillMaxWidth
import androidx.compose.foundation.layout.padding
import androidx.compose.foundation.shape.RoundedCornerShape
import androidx.compose.material3.Button
import androidx.compose.material3.ButtonDefaults
import androidx.compose.material3.DropdownMenu
import androidx.compose.material3.DropdownMenuItem
import androidx.compose.material3.Surface
import androidx.compose.material3.Text
import androidx.compose.runtime.Composable
import androidx.compose.runtime.NonSkippableComposable
import androidx.compose.runtime.getValue
import androidx.compose.runtime.mutableStateOf
import androidx.compose.runtime.remember
import androidx.compose.runtime.setValue
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.platform.testTag
import androidx.compose.ui.text.style.TextAlign
import androidx.compose.ui.tooling.preview.Preview
import androidx.compose.ui.unit.dp
import androidx.compose.ui.unit.sp
import com.example.tictactoe_kotlin.tictactoe.Game
import com.example.tictactoe_kotlin.tictactoe.GameState
import com.example.tictactoe_kotlin.ui.theme.Tictactoe_kotlinTheme

class MainActivity : ComponentActivity() {
    override fun onCreate(savedInstanceState: Bundle?) {
        super.onCreate(savedInstanceState)
        setContent {
            Tictactoe_kotlinTheme {
                // A surface container using the 'background' color from the theme
                Surface(
                    modifier = Modifier.fillMaxSize(), color = Color(0xFF232F3E)
                ) {
                    TicTacToe()
                }
            }
        }
    }
}

@NonSkippableComposable
@Composable
fun GameBoard(
    board: List<String>,
    onClick: (Int) -> (() -> Unit),
    textColor: Color,
    backgroundColor: Color,
    modifier: Modifier = Modifier
) {
    var spot: Int
    Column(Modifier.padding(8.dp)) {
        for (i in 0..2) {
            Row {
                for (j in 0..2) {
                    spot = 3 * i + j
                    Button(
                        shape = RoundedCornerShape(10.dp),
                        colors = ButtonDefaults.buttonColors(
                            containerColor = backgroundColor,
                            contentColor = if (spot in Game.winningSpots) Color(0xFFFF0000) else textColor
                        ),
                        onClick = onClick(spot),
                        enabled = Game.gameState.value != GameState.Waiting,
                        modifier = modifier
                            .weight(1f)
                            .aspectRatio(1f)
                            .padding(4.dp)
                            .testTag("Button$spot")
                    ) {
                        Text(text = board[spot], fontSize = 50.sp)
                    }
                }
            }
        }
    }
}

@Composable
fun Controls(
    leftControlButtonText: String,
    rightControlButtonTest: String,
    leftButtonFunction: () -> Unit,
    rightButtonFunction: () -> Unit,
    textColor: Color,
    backgroundColor: Color,
    modifier: Modifier = Modifier
) {
    Row(
        horizontalArrangement = Arrangement.SpaceEvenly,
        modifier = modifier
            .fillMaxWidth()
            .padding(8.dp)
    ) {
        Button(
            onClick = leftButtonFunction,
            shape = RoundedCornerShape(14.dp),
            colors = ButtonDefaults.buttonColors(
                containerColor = backgroundColor, contentColor = textColor
            ),
        ) {
            Text(text = leftControlButtonText, color = textColor)
        }
        Button(
            onClick = rightButtonFunction,
            shape = RoundedCornerShape(14.dp),
            colors = ButtonDefaults.buttonColors(
                containerColor = backgroundColor, contentColor = textColor
            ),
        ) {
            Text(text = rightControlButtonTest, color = textColor)
        }
    }
}

@Composable
fun AIDropdown(textColor: Color, backgroundColor: Color, modifier: Modifier = Modifier) {
    var expanded by remember { mutableStateOf(false) }
    val items = listOf("Easy", "Medium", "Hard", "Impossible")
    Box(modifier.padding(horizontal = 4.dp)) {
        Text(
            items[Game.aiStrength.value],
            color = textColor,
            modifier = Modifier
                .fillMaxWidth()
                .clickable(onClick = { expanded = true })
                .background(color = backgroundColor, shape = RoundedCornerShape(6.dp)),
            textAlign = TextAlign.Center
        )
        DropdownMenu(
            expanded = expanded,
            onDismissRequest = { expanded = false },
            modifier = Modifier.fillMaxWidth()
        ) {
            items.forEachIndexed { index, difficulty ->
                DropdownMenuItem(text = {
                    Text(text = difficulty, color = textColor)
                }, onClick = {
                    Game.aiStrength.value = index
                    expanded = false
                })
            }
        }
    }
}

@Composable
fun Difficulty(textColor: Color, backgroundColor: Color, modifier: Modifier = Modifier) {
    Row(
        horizontalArrangement = Arrangement.SpaceEvenly,
        modifier = Modifier
            .fillMaxWidth()
            .padding(8.dp)
    ) {
        Text(text = "Choose an AI difficulty:", color = textColor)
        AIDropdown(textColor, backgroundColor, modifier = modifier)
    }
}


@Composable
fun TicTacToe(modifier: Modifier = Modifier) {
    val titleColor = Color(0xFF722007)
    val uiColor = Color(0xFFFF9900)
    val elementBackgroundColor = Color(0xFFF8F8F8)

    ButtonDefaults.buttonColors(containerColor = elementBackgroundColor, contentColor = uiColor)
    var headerText by remember {
        mutableStateOf("Select a game mode")
    }

    var leftControlButtonText by remember {
        mutableStateOf("1 Player")
    }
    var leftButtonFunction by remember {
        mutableStateOf({
            Game.gameState.value = GameState.Playing
            Game.aiOpponent.value = true
        })
    }

    var rightControlButtonText by remember {
        mutableStateOf("2 Player")
    }
    var rightButtonFunction by remember {
        mutableStateOf({
            Game.gameState.value = GameState.Playing
            Game.aiOpponent.value = false
        })
    }

    when (Game.gameState.value) {
        GameState.Waiting -> {
            headerText = "Select a game mode"
            leftControlButtonText = "1 Player"
            rightControlButtonText = "2 Player"
            leftButtonFunction = {
                Game.gameState.value = GameState.Playing
                Game.aiOpponent.value = true
            }
            rightButtonFunction = {
                Game.gameState.value = GameState.Playing
                Game.aiOpponent.value = false
            }
        }

        GameState.Playing -> {
            headerText = "Player ${Game.player.value}'s turn"
            if (Game.emptyCells().size == 9 && Game.aiOpponent.value) {
                leftControlButtonText = "AI turn"
                leftButtonFunction = { Game.aiMove() }
            } else {
                leftControlButtonText = "Restart"
                leftButtonFunction = { Game.restartGame() }
            }
            rightControlButtonText = "Change game mode"
            rightButtonFunction = { Game.restartGame(); Game.gameState.value = GameState.Waiting }
        }
        // Buttons have the same functionality for win, draw and playing
        // No need to add that to win/draw as they are remembered and those
        // states can only be reached from playing.
        GameState.Draw -> {
            headerText = "Draw!"
        }

        GameState.Win -> {
            headerText = "Player ${Game.player.value} wins!"
        }
    }


    Column(horizontalAlignment = Alignment.CenterHorizontally) {
        Text(
            text = "TicTacToe-Kotlin",
            color = titleColor,
            textAlign = TextAlign.Center,
            fontSize = 45.sp
        )
        Text(text = headerText, color = uiColor, fontSize = 25.sp)
        GameBoard(
            board = Game.board, onClick = {
                {
                    if (Game.makeMove(it)) {
                        Game.player.value = Game.swapPlayer(Game.player.value)
                        if (Game.aiOpponent.value) {
                            Game.aiMove()
                        }
                    }
                }
            }, textColor = uiColor, backgroundColor = elementBackgroundColor, modifier = modifier
        )
        Controls(
            leftControlButtonText,
            rightControlButtonText,
            leftButtonFunction,
            rightButtonFunction,
            textColor = uiColor,
            backgroundColor = elementBackgroundColor,
            modifier = modifier
        )
        Difficulty(
            textColor = uiColor, backgroundColor = elementBackgroundColor, modifier = modifier
        )
    }
}


@Preview(showBackground = true, backgroundColor = 0xFF232F3E, showSystemUi = true)
@Composable
fun TicTacToePreview() {
    Tictactoe_kotlinTheme {
        TicTacToe()
    }
}
