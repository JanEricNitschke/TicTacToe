package com.example.tictactoe_kotlin

import androidx.compose.foundation.layout.fillMaxSize
import androidx.compose.material3.Surface
import androidx.compose.ui.Modifier
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.test.assertTextEquals
import androidx.compose.ui.test.hasTestTag
import androidx.compose.ui.test.junit4.createComposeRule
import androidx.compose.ui.test.onNodeWithText
import androidx.compose.ui.test.performClick
import androidx.test.ext.junit.runners.AndroidJUnit4
import androidx.test.platform.app.InstrumentationRegistry
import com.example.tictactoe_kotlin.ui.theme.Tictactoe_kotlinTheme
import org.junit.Assert.*
import org.junit.Rule
import org.junit.Test
import org.junit.runner.RunWith

/**
 * Instrumented test, which will execute on an Android device.
 *
 * See [testing documentation](http://d.android.com/tools/testing).
 */
@RunWith(AndroidJUnit4::class)
class TicTacToeTest {

    @get:Rule
    val composeTestRule = createComposeRule()

    @Test
    fun useAppContext() {
        // Context of the app under test.
        val appContext = InstrumentationRegistry.getInstrumentation().targetContext
        assertEquals("com.example.tictactoe_kotlin", appContext.packageName)
    }

    @Test
    fun clickFieldTest() {
        composeTestRule.setContent {
            Tictactoe_kotlinTheme {
                // A surface container using the 'background' color from the theme
                Surface(
                    modifier = Modifier.fillMaxSize(), color = Color(0xFF232F3E)
                ) {
                    TicTacToe()
                }
            }
        }
        composeTestRule.onNodeWithText("2 Player").assertExists()
        composeTestRule.onNodeWithText("2 Player").performClick()
        composeTestRule.onNodeWithText("Player X's turn").assertExists()
        composeTestRule.onNode(hasTestTag("Button7")).assertExists()
        composeTestRule.onNode(hasTestTag("Button7")).performClick()
        composeTestRule.onNode((hasTestTag("Button7"))).assertTextEquals("X")
        composeTestRule.onNodeWithText("Player O's turn").assertExists()
    }
}
