// @ts-check

/**
 * Contains the current state of the board
 */
let board = [
  ["", "", ""],
  ["", "", ""],
  ["", "", ""],
];

/**
 * Contains the player that is currently active
 */
let player = "X";

/**
 * Game status to handle whether buttons should be enabled
 * and what there text and functionality should be
 */
let gameStatus = "paused";

/**
 * Whether it is a 1 or 2 player game
 */
let gameMode = "";

/**
 * Current difficulty setting for the AI
 */
let difficultySetting = 4;

/* eslint-disable no-unused-vars */
/**
 * Propagates the difficulty whenever the player changes
 * it in the select element.
 *
 * @param { Event & { target: HTMLSelectElement } } event -
 *     triggered by changing difficulty on the select.
 */
function handleSelectChange(event) {
  difficultySetting = parseInt(event.target.value);
}
/* eslint-enable no-unused-vars */

/**
 * Swap the player between X and O
 *
 * @param {string} curPlayer - Currently active player
 * @return {string} Returns the previously inactive player.
 */
function getOtherPlayer(curPlayer) {
  if (curPlayer === "X") {
    return "O";
  }
  return "X";
}

/**
 * Swap the current player and change the text
 */
function swapPlayer() {
  player = getOtherPlayer(player);
  const message = document.getElementById("message");
  if (message === null) {
    throw new Error("Could not find message field.");
  }
  message.innerHTML = "Player " + player + "'s turn";
}

/**
 * Check whether the given player wins given the passed state.
 *
 * @param {string} curPlayer - Currently active player ('X' or 'O')
 * @param {Array.<string[]>} state - Current board state.
 * @return {Array.<number[]> | false} The list of winning cells
 *     (as (x, y) tuple). or 'false' if no win was found.
 */
function isPlayerWin(curPlayer, state) {
  let win = false;
  const n = state.length;
  let fields;
  // checking rows
  for (let i = 0; i < n; i++) {
    win = true;
    fields = [];
    for (let j = 0; j < n; j++) {
      fields.push([i, j]);
      if (state[i][j] != curPlayer) {
        win = false;
        break;
      }
    }
    if (win) {
      return fields;
    }
  }

  // checking columns
  for (let i = 0; i < n; i++) {
    win = true;
    fields = [];
    for (let j = 0; j < n; j++) {
      fields.push([j, i]);
      if (state[j][i] != curPlayer) {
        win = false;
        break;
      }
    }
    if (win) {
      return fields;
    }
  }

  // checking diagonals
  win = true;
  fields = [];
  for (let i = 0; i < n; i++) {
    fields.push([i, i]);
    if (state[i][i] != curPlayer) {
      win = false;
      break;
    }
  }
  if (win) {
    return fields;
  }

  win = true;
  fields = [];
  for (let i = 0; i < n; i++) {
    fields.push([i, n - i - 1]);
    if (state[i][n - i - 1] != curPlayer) {
      win = false;
      break;
    }
  }
  if (win) {
    return fields;
  }
  return false;
}

/**
 * Grab all empty cells from the current board
 *
 * @param {Array.<string[]>} state - Current board state.
 * @return {Array.<number[]>} The list of empty cells (as (x, y) tuple).
 */
function emptyCells(state) {
  const emptyCells = [];
  for (let row = 0; row < state.length; row++) {
    for (let col = 0; col < state.length; col++) {
      if (state[row][col] === "") {
        emptyCells.push([row, col]);
      }
    }
  }
  return emptyCells;
}

/**
 * Try to perform the given move
 *
 * @param {number} x - x coordinate of the attempted move
 * @param {number} y - y coordinate of the attempted move
 * @return {boolean} Whether the move was valid (and thus performed).
 */
function setMove(x, y) {
  if (board[x][y] === "") {
    board[x][y] = player;
    return true;
  }
  return false;
}

/**
 * Color the given fields red.
 * Used to highlight the winning line.
 *
 * @param {Array.<number[]>} fields List of cells (as (x, y) tuples)
 *     to color red.
 */
function colorRed(fields) {
  for (const field of fields) {
    const row = field[0];
    const col = field[1];
    const cell = document.getElementById(row.toString() + col.toString());
    if (cell === null) {
      throw new Error(`Could not find button for coordinates ${[row, col]}`);
    }
    cell.style.color = "#ff0000";
  }
}

/**
 * Check if the game is over
 *
 * @param {Array.<string[]>} state - Current board state.
 * @return {boolean} Whether the game is over by draw or win.
 */
function checkEndState(state) {
  const fields = isPlayerWin(player, state);
  if (fields) {
    const message = document.getElementById("message");
    if (message === null) {
      throw new Error("Could not find message field.");
    }
    message.innerHTML = "Player " + player + " wins!";

    gameStatus = player + " won";
    colorRed(fields);
    return true;
  }
  if (emptyCells(state).length === 0) {
    const message = document.getElementById("message");
    if (message === null) {
      throw new Error("Could not find message field.");
    }
    message.innerHTML = "Draw!";

    gameStatus = "Draw";
    return true;
  }
  return false;
}

/* eslint-disable no-unused-vars */
/**
 * Function that is called when any field is clicked.
 *
 * @param {HTMLButtonElement} field - Button that was clicked to make a move.
 * @return {undefined} Does not return anything.
 */
function clickedField(field) {
  // If the game is currently not live
  // but waiting for settings to be chosen
  // then shake the message
  // (This could also be handled by just disabling the fields
  // until all settings are chosen)
  if (gameStatus != "ongoing") {
    const message = document.getElementById("message");
    if (message === null) {
      throw new Error("Could not find message field.");
    }
    message.style.animation = "";
    message.offsetHeight; /* trigger reflow */
    message.style.animation = "shake 0.5s linear 4";
    return;
  }
  // Get the coordinates of the field
  const x = field.id.split("")[0];
  const y = field.id.split("")[1];
  // Check if the move is valid
  const move = setMove(parseInt(x), parseInt(y));
  if (move == true) {
    // Perform the move by changing the text
    // and andjusting the button
    field.innerHTML = player;
    const restartButton = document.getElementById("button1");
    if (
      restartButton === null ||
      !(restartButton instanceof HTMLButtonElement)
    ) {
      throw new Error("Could not find button1.");
    }
    restartButton.disabled = true;
    restartButton.innerHTML = "Restart";

    // If the game is over return
    if (checkEndState(board)) {
      restartButton.disabled = false;
      return;
    }
    // If not swap the active player
    swapPlayer();
    // If it is a 1 person game perform the AI move
    if (gameMode == "1Player") {
      aiTurn();
    }
  }
}

/**
 * Restart the game.
 */
function restart() {
  // Reset the game status
  gameStatus = "ongoing";
  // Board
  board = [
    ["", "", ""],
    ["", "", ""],
    ["", "", ""],
  ];
  // and player
  player = "X";
  // Reset the contents of the cells
  for (let row = 0; row < board.length; row++) {
    for (let col = 0; col < board.length; col++) {
      const cell = document.getElementById(row.toString() + col.toString());
      if (cell === null) {
        throw new Error(`Could not find button for coordinates ${[row, col]}`);
      }
      cell.innerHTML = "";
      cell.style.color = "#FF9900";
    }
  }
  // and the message
  const message = document.getElementById("message");
  if (message === null) {
    throw new Error("Could not find message field.");
  }
  message.innerHTML = "Player X's turn";
}
/* eslint-disable no-unused-vars */

//
/**
 * Takes a board state and returns the optimal move
 * for the given player
 *
 * @param {string} curPlayer
 * @param {Array.<string[]>} state - Current board state
 * @return {number[]} Optimal move given the conditions as a tuple
 *     (x, y, endState) with (x, y) being the move coordinates as
 *     endState being the endState after that move and following
 *     perfect play from both player.
 */
function minmax(curPlayer, state) {
  // Base cases
  let best = [-1, -1, -1];
  // Current player has won
  if (isPlayerWin(curPlayer, state)) {
    best[2] = 1;
    return best;
  }
  // Current player has lost
  if (isPlayerWin(getOtherPlayer(curPlayer), state)) {
    best[2] = -1;
    return best;
  }
  const empties = emptyCells(state);
  // Game is over in a draw
  if (empties.length === 0) {
    best[2] = 0;
    return best;
  }
  // First move is random to increase
  // replayability
  if (empties.length === 9) {
    best = [Math.random() * 3, Math.random() * 3, 0];
    return best;
  }
  // Recursively apply minmax algorithm
  // to find best move
  empties.forEach(function (field) {
    const x = field[0];
    const y = field[1];
    state[x][y] = curPlayer;
    const score = minmax(getOtherPlayer(curPlayer), state);
    if (-score[2] >= best[2]) {
      best = [x, y, -score[2]];
    }
    state[x][y] = "";
  });
  return best;
}

/**
 * Perform a random valid move
 *
 * @param {Array.<string[]>} state - Current board state.
 * @return {number[]} Returns an array [x, y, 0] with x, y being the coordinates
 *     of the random move.
 */
function randomMove(state) {
  // Get all empty cells
  const empties = emptyCells(state);
  // Build a valid move from a random one
  const move = empties[Math.floor(Math.random() * empties.length)];
  return [move[0], move[1], 0];
}

/**
 * Try to get a blocking move
 *
 * A blocking move is one that prevents and opponent from finishing
 * a line
 * So just check which move would do that for the opponent
 *
 * @param {string} curPlayer - Currently active player.
 * @param {Array.<string[]>} state - Current board state.
 * @return {number[] | null} Returns an array [x, y, 0] with x, y
 *     being the coordinates of the blocking move or 'null' if none exists.
 */
function getBlockingMove(curPlayer, state) {
  return getWinningMove(getOtherPlayer(curPlayer), state);
}

/**
 * Try to get a blocking move
 *
 * @param {string} curPlayer - Currently active player.
 * @param {Array.<string[]>} state - Current board state.
 * @return {number[] | null} Returns an array [x, y, 0] with x, y
 *     being the coordinates of the blocking move or 'null' if none exists.
 */
function getWinningMove(curPlayer, state) {
  // Build a map with all the lines that win
  // Have to use strings because lists compare
  // by reference not value
  const winConditions = new Map([
    ["row0", new Set([`0_0`, `0_1`, `0_2`])],
    ["row1", new Set([`1_0`, `1_1`, `1_2`])],
    ["row2", new Set([`2_0`, `2_1`, `2_2`])],
    ["col0", new Set([`0_0`, `1_0`, `2_0`])],
    ["col1", new Set([`0_1`, `1_1`, `2_1`])],
    ["col2", new Set([`0_2`, `1_2`, `2_2`])],
    ["diag", new Set([`0_0`, `1_1`, `2_2`])],
    ["antidiag", new Set([`0_2`, `1_1`, `2_0`])],
  ]);
  // Go over all cells
  for (let row = 0; row < state.length; row++) {
    for (let col = 0; col < state.length; col++) {
      // If the current player occupies it then
      // that reduces the number of needed values in that line
      // by one
      const rowSet = winConditions.get("row" + row.toString());
      const colSet = winConditions.get("col" + col.toString());
      const antiDiagSet = winConditions.get("antidiag");
      const diagSet = winConditions.get("diag");
      if (
        rowSet == undefined ||
        colSet == undefined ||
        diagSet == undefined ||
        antiDiagSet == undefined
      ) {
        return null;
      }

      if (state[row][col] == curPlayer) {
        if (rowSet.has(`${row}_${col}`)) {
          rowSet.delete(`${row}_${col}`);
        }
        if (colSet.has(`${row}_${col}`)) {
          colSet.delete(`${row}_${col}`);
        }
        if (row == col && diagSet.has(`${row}_${col}`)) {
          diagSet.delete(`${row}_${col}`);
        }
        if (row == state.length - 1 - col && antiDiagSet.has(`${row}_${col}`)) {
          antiDiagSet.delete(`${row}_${col}`);
        }
      }
      // If the opponent is in the cell then all lines that contain
      // it are useless
      if (state[row][col] == getOtherPlayer(curPlayer)) {
        rowSet.clear();
        colSet.clear();
        if (row == col) {
          diagSet.clear();
        }
        if (row == state.length - 1 - col) {
          antiDiagSet.clear();
        }
      }
    }
  }

  // Check if any line requires exactly one more move
  for (const winCondition of winConditions.values()) {
    if (winCondition.size == 1) {
      const coords = winCondition.values().next().value.split("_");
      return [coords[0], coords[1], 0];
    }
  }
  // If there is no winning move return null
  return null;
}

/**
 * Get a winning or random move.
 *
 * @param {string} curPlayer - Currently active player.
 * @param {Array.<string[]>} state - Current board state.
 * @return {number[]} - Array [x, y, 0] with x, y
 *     being the coordinates move.
 */
function winMove(curPlayer, state) {
  const winningMove = getWinningMove(curPlayer, state);
  if (winningMove != null) {
    return winningMove;
  }
  // If there is no winning move perform a random one
  return randomMove(state);
}

/**
 * Get a winning, blocking or random move.
 *
 * @param {string} curPlayer - Currently active player.
 * @param {Array.<string[]>} state - Current board state.
 * @return {number[]} - Array [x, y, 0] with x, y
 *     being the coordinates move.
 */
function blockWinMove(curPlayer, state) {
  const winningMove = getWinningMove(curPlayer, state);
  if (winningMove != null) {
    return winningMove;
  }
  const blockingMove = getBlockingMove(curPlayer, state);
  if (blockingMove != null) {
    return blockingMove;
  }
  // If neither is possible do a random one
  return randomMove(state);
}

/**
 * Functionality for an AI turn
 *
 * @return {undefined} - Returns nothing. Only early exit.
 */
function aiTurn() {
  let best = null;
  // Get a move according to the difficulty setting
  switch (difficultySetting) {
    case 1:
      best = randomMove(board);
      break;
    case 2:
      best = winMove(player, board);
      break;
    case 3:
      best = blockWinMove(player, board);
      break;
    default:
      best = minmax(player, board);
      if (best[2] == -1) {
        best = blockWinMove(player, board);
      }
  }
  const row = best[0];
  const col = best[1];
  // Apply the move and check if the game is over
  if (setMove(row, col)) {
    const field = document.getElementById(row.toString() + col.toString());
    if (field === null) {
      throw new Error(`Could not find button for coordinates ${[row, col]}`);
    }
    field.innerHTML = player;

    if (checkEndState(board)) {
      const restartButton = document.getElementById("button1");
      if (
        restartButton === null ||
        !(restartButton instanceof HTMLButtonElement)
      ) {
        throw new Error("Could not find button1.");
      }
      restartButton.disabled = false;
      return;
    }
    // swap the player back
    swapPlayer();
  }
}

/**
 * Handle state and functionality of the first button
 *
 * @param {HTMLButtonElement} button - Left button.
 */
function button1click(button) {
  // At the start of the game it is used to set
  // the game mode to one player mode
  // In that case it should change
  // to initiate an AI turn
  if (button.innerHTML == "1 Player") {
    gameMode = "1Player";
    changeElementsForGame();
    button.innerHTML = "AI Turn";
  }
  // If the game is over it functions as restart button
  else if (button.innerHTML == "Restart") {
    restart();
    if (gameMode === "1Player") {
      button.innerHTML = "AI Turn";
    }
  }
  // Using it to start the AI turn
  // changes it to a restart button
  else {
    // AI start
    aiTurn();
    button.innerHTML = "Restart";
    button.disabled = true;
  }
}

/**
 * Set all the settings as they should be for a live game
 */
function changeElementsForGame() {
  // Game is now ongoing
  gameStatus = "ongoing";
  // Button1 should handle restart
  const button1 = document.getElementById("button1");
  if (button1 === null || !(button1 instanceof HTMLButtonElement)) {
    throw new Error("Could not find button1.");
  }
  button1.innerHTML = "Restart";
  // Button two should handle game mode change
  const button2 = document.getElementById("button2");
  if (button2 === null || !(button2 instanceof HTMLButtonElement)) {
    throw new Error("Could not find button1.");
  }
  button2.innerHTML = "Change game mode";
  // And the message should display which players turn it is
  const message = document.getElementById("message");
  if (message === null) {
    throw new Error("Could not find message field.");
  }
  message.innerHTML = "Player " + player + "'s turn";
}

/**
 * Handles state and functionality for the second button
 *
 * @param {HTMLButtonElement} button - Right button
 */
function button2click(button) {
  // At the start it handles sets two player mode
  if (button.innerHTML == "2 Players") {
    changeElementsForGame();
    gameMode = "2Players";
  }
  // Otherwise it is used to pause the game and change the mode
  else if (button.innerHTML == "Change game mode") {
    restart();
    gameStatus = "paused";
    button.innerHTML = "2 Players";
    const button1 = document.getElementById("button1");
    if (button1 === null || !(button1 instanceof HTMLButtonElement)) {
      throw new Error("Could not find button1.");
    }
    button1.innerHTML = "1 Player";
    button1.disabled = false;
    const message = document.getElementById("message");
    if (message === null) {
      throw new Error("Could not find message field.");
    }
    message.innerHTML = "Select game mode";
    gameMode = "";
  }
}
