// Contains the current state of the board
let board = [["", "", ""], ["", "", ""], ["", "", ""]];
// Contains the player that is currently active
let player = "X";
// Game status to handle whether buttons should be enabled
// and what there text and functionality should be
let game_status = "paused";
// Whether it is a 1 or 2 player game
let game_mode = "";
// Current difficulty setting for the AI
let difficulty_setting = 4;

// Propagates the difficulty whenever the player changes
// it in the select element.
function handleSelectChange(event) {
    var selectElement = event.target;

    difficulty_setting = parseInt(selectElement.value);
}

// Swap the player between X and O
function get_other_player(cur_player) {
    if (cur_player === "X") {
        cur_player = "O";
    }
    else {
        cur_player = "X";
    }
    return cur_player;
}

// Swap the current player and change the text
function swap_player() {
    player = get_other_player(player);
    message = document.getElementById("message");
    message.innerHTML = "Player " + player + "'s turn";
}

// Check whether the given player wins
// given the passed state
function is_player_win(cur_player, state) {
    let win = false;
    const n = state.length;
    let fields;
    // checking rows
    for (let i = 0; i < n; i++) {
        win = true;
        fields = [];
        for (let j = 0; j < n; j++) {
            fields.push([i, j]);
            if (state[i][j] != cur_player) {
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
            if (state[j][i] != cur_player) {
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
        if (state[i][i] != cur_player) {
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
        if (state[i][n - i - 1] != cur_player) {
            win = false;
            break;
        }
    }
    if (win) {
        return fields;
    }
    return false;
}

// Grab all empty cells from the current board
function empty_cells(state) {
    let empty_cells = [];
    for (let row = 0; row < state.length; row++) {
        for (let col = 0; col < state.length; col++) {
            if (state[row][col] === "")
                empty_cells.push([row, col]);
        }
    }
    return empty_cells;
}

// Try to perform the given move
function setMove(x, y) {
    if (board[x][y] === "") {
        board[x][y] = player;
        return true;
    }
    return false;
}

// Color the given fields red
// Used to highlight the winning line
function color_red(fields) {
    for (const field of fields) {
        row = field[0];
        col = field[1];
        cell = document.getElementById(row.toString() + col.toString());
        console.log(cell, row.toString() + col.toString());
        cell.style.color = "#ff0000";
    }
}

// Check if the game is over
function checkEndState(state) {
    fields = is_player_win(player, state);
    if (fields) {
        message = document.getElementById("message");
        message.innerHTML = "Player " + player + " wins!";
        game_status = player + " won";
        console.log(fields);
        color_red(fields);
        return true;
    }
    if (empty_cells(state).length === 0) {
        message = document.getElementById("message");
        message.innerHTML = "Draw!";
        game_status = "Draw";
        return true;
    }
    return false;
}

// Function when any field is clicked
function clickedField(field) {
    // If the game is currently not live
    // but waiting for settings to be chosen
    // then shake the message
    // (This could also be handled by just diabling the fields
    // until all settings are chosen)
    if (game_status != "ongoing") {
        document.getElementById('message').style.animation = "";
        document.getElementById('message').offsetHeight; /* trigger reflow */
        document.getElementById('message').style.animation = "shake 0.5s linear 4";
        return;
    }
    // Get the coordinates of the field
    let x = field.id.split("")[0];
    let y = field.id.split("")[1];
    // Check if the move is valid
    let move = setMove(x, y);
    if (move == true) {
        // Perform the move by changing the text
        // and andjusting the button
        field.innerHTML = player;
        restartButton = document.getElementById("button1");
        restartButton.disabled = true;
        restartButton.innerHTML = "Restart";
        // If the game is over return
        if (checkEndState(board)) {
            restartButton.disabled = false;
            return;
        }
        // If not swap the active player
        swap_player();
        // If it is a 1 person game perform the AI move
        if (game_mode == "1Player") {
            ai_turn();
        }
    }

}
// Restart the game
function restart() {
    // Reset the game status
    game_status = "ongoing";
    // Board
    board = [["", "", ""], ["", "", ""], ["", "", ""]];
    // and player
    player = "X";
    // Reset the contents of the cells
    for (let row = 0; row < board.length; row++) {
        for (let col = 0; col < board.length; col++) {
            cell = document.getElementById(row.toString() + col.toString());
            cell.innerHTML = "";
            cell.style.color = "#FF9900";
        }
    }
    // and the message
    message = document.getElementById("message");
    message.innerHTML = "Player X's turn";
}

// Takes a board state and returns the coordinates of the optimal move for the given player
function minmax(cur_player, state) {
    // Base cases
    let best = [-1, -1, -1];
    // Current player has won
    if (is_player_win(cur_player, state)) {
        best[2] = 1;
        return best;
    }
    // Current player has lost
    if (is_player_win(get_other_player(cur_player), state)) {
        best[2] = -1;
        return best;
    }
    const empties = empty_cells(state);
    // Game is over in a draw
    if (empties.length === 0) {
        best[2] = 0;
        return best;
    }
    // First move is random to increase
    // replayability
    if (empties.length === 9) {
        best = [parseInt(Math.random() * 3), parseInt(Math.random() * 3), 0];
        return best;
    }
    // Recursively apply minmax algorithm
    // to find best move
    empties.forEach(function (field) {
        var x = field[0];
        var y = field[1];
        state[x][y] = cur_player;
        let score = minmax(get_other_player(cur_player), state);
        if (-score[2] >= best[2]) {
            best = [x, y, -score[2]];
        }
        state[x][y] = "";
    });
    return best;
}

// Perform a random valid move
function random_move(_, state) {
    // Get all empty cells
    const empties = empty_cells(state);
    // Build a valid move from a random one
    move = empties[Math.floor(Math.random() * empties.length)];
    return [move[0], move[1], 0];
}

// Try to get a blocking move
function get_blocking_move(cur_player, state) {
    // A blocking move is one that prevents and opponent from finishing
    // a line
    // So just check which move would do that for the opponent
    return get_winning_move(get_other_player(cur_player), state);
}

// Try to get a winning move
function get_winning_move(cur_player, state) {
    // Build a map with all the lines that win
    // Have to use strings because lists compare
    // by reference not value
    let win_conditions = new Map([
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
            if (state[row][col] == cur_player) {
                if (win_conditions.get("row" + row.toString()).has(`${row}_${col}`)) {
                    win_conditions.get("row" + row.toString()).delete(`${row}_${col}`);
                }
                if (win_conditions.get("col" + col.toString()).has(`${row}_${col}`)) {
                    win_conditions.get("col" + col.toString()).delete(`${row}_${col}`);
                }
                if (row == col && win_conditions.get("diag").has(`${row}_${col}`)) {
                    win_conditions.get("diag").delete(`${row}_${col}`);
                }
                if (row == (state.length - 1 - col) && win_conditions.get("antidiag").has(`${row}_${col}`)) {
                    win_conditions.get("antidiag").delete(`${row}_${col}`);
                }
            }
            // If the opponent is in the cell then all lines that contain
            // it are useless
            if (state[row][col] == get_other_player(cur_player)) {
                win_conditions.get("row" + row.toString()).clear();
                win_conditions.get("col" + col.toString()).clear();
                if (row == col) {
                    win_conditions.get("diag").clear();
                }
                if (row == (state.length - 1 - col)) {
                    win_conditions.get("antidiag").clear();
                }
            }
        }
    }

    // Check if any line requires exactly one more move
    for (let win_condition of win_conditions.values()) {
        if (win_condition.size == 1) {
            const coords = win_condition.values().next().value.split("_");
            return [coords[0], coords[1], 0];
        }
    }
    // If there is no winning move return null
    return null;
}

// Try to perform a winning move
function win_move(cur_player, state) {
    let winning_move = get_winning_move(cur_player, state);
    if (winning_move != null) {
        return winning_move;
    }
    // If there is no winning move perform a random one
    return random_move(cur_player, state);
}

// Try to perform a winning or blocking move
function block_win_move(cur_player, state) {
    let winning_move = get_winning_move(cur_player, state);
    if (winning_move != null) {
        return winning_move;
    }
    let blocking_move = get_blocking_move(cur_player, state);
    if (blocking_move != null) {
        return blocking_move;
    }
    // If neither is possible do a random one
    return random_move(cur_player, state);
}

// Functionality for an AI turn
function ai_turn() {
    let best = null;
    // Get a move according to the difficulty setting
    switch (difficulty_setting) {
        case 1:
            best = random_move(player, board);
            break;
        case 2:
            best = win_move(player, board);
            break;
        case 3:
            best = block_win_move(player, board);
            break;
        default:
            best = minmax(player, board);
            if (best[2] == -1) {
                best = block_win_move(player, board);
            }
    }
    const row = best[0];
    const col = best[1];
    // Apply the move and check if the game is over
    if (setMove(row, col)) {
        field = document.getElementById(row.toString() + col.toString());
        field.innerHTML = player;
        if (checkEndState(board)) {
            restartButton.disabled = false;
            return;
        }
        // swap the player back
        swap_player();
    }
}

// Handle state and functionality of the first button
function button1click(button) {
    // At the start of the game it is used to set
    // the game mode to one player mode
    // In that case it should change
    // to initiate an AI turn
    if (button.innerHTML == "1 Player") {
        game_mode = "1Player";
        changeElementsForGame();
        button.innerHTML = "AI Turn";
    }
    // If the game is over it functions as restart button
    else if (button.innerHTML == "Restart") {
        restart();
        if (game_mode === "1Player") {
            button.innerHTML = "AI Turn";
        }

    }
    // Using it to start the AI turn
    // changes it to a restart button
    else { // AI start
        ai_turn();
        button.innerHTML = "Restart";
        button.disabled = true;
    }
}

// Set all the settings as they should be for a live game
function changeElementsForGame() {
    // Game is now ongoing
    game_status = "ongoing";
    // Button1 should handle restart
    button1 = message = document.getElementById("button1");
    button1.innerHTML = "Restart";
    // Button two should handle game mode change
    button2 = message = document.getElementById("button2");
    button2.innerHTML = "Change game mode";
    // And the message should display which players turn it is
    message = document.getElementById("message");
    message.innerHTML = "Player " + player + "'s turn";

}

// Handles state and functionality for the second button
function button2click(button) {
    // At the start it handles sets two player mode
    if (button.innerHTML == "2 Players") {
        changeElementsForGame();
        game_mode = "2Players";
    }
    // Otherwise it is used to pause the game and change the mode
    else if (button.innerHTML == "Change game mode") {
        restart();
        game_status = "paused";
        button.innerHTML = "2 Players";
        button1 = message = document.getElementById("button1");
        button1.innerHTML = "1 Player";
        button1.disabled = false;
        message = document.getElementById("message");
        message.innerHTML = "Select game mode";
        game_mode = "";
    }
}
