let board = [["", "", ""], ["", "", ""], ["", "", ""]];
let player = "X";
let game_status = "paused";
let game_mode = "";

function get_other_player(cur_player) {
    if (cur_player === "X") {
        cur_player = "O";
    }
    else {
        cur_player = "X";
    }
    return cur_player;
}

function swap_player() {
    player = get_other_player(player);
    message = document.getElementById("message");
    message.innerHTML = "Player " + player + "'s turn";
}

function is_player_win(cur_player, state) {
    let win = false;
    const n = state.length;
    // checking rows
    for (let row of state) {
        win = true;
        for (let value of row) {
            if (value != cur_player) {
                win = false;
                break;
            }
        }
        if (win) {
            return win;
        }
    }

    // checking columns
    for (let i = 0; i < n; i++) {
        win = true;
        for (let j = 0; j < n; j++) {
            if (state[j][i] != cur_player) {
                win = false;
                break;
            }
        }
        if (win) {
            return win;
        }
    }

    // checking diagonals
    win = true;
    for (let i = 0; i < n; i++) {
        if (state[i][i] != cur_player) {
            win = false;
            break;
        }
    }
    if (win) {
        return win;
    }

    win = true;
    for (let i = 0; i < n; i++) {
        if (state[i][n - i - 1] != cur_player) {
            win = false;
            break;
        }
    }
    if (win) {
        return win;
    }
    return false;
}

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

function setMove(x, y) {
    if (board[x][y] === "") {
        board[x][y] = player;
        return true;
    }
    return false;
}

function checkEndState(state) {
    if (is_player_win(player, state)) {
        message = document.getElementById("message");
        message.innerHTML = "Player " + player + " wins!";
        game_status = player + " won";
        return 1;
    }
    if (empty_cells(state).length === 0) {
        message = document.getElementById("message");
        message.innerHTML = "Draw!";
        game_status = "Draw";
        return 0;
    }
    return -1;
}

function clickedField(field) {
    if (game_status != "ongoing") {
        document.getElementById('message').style.animation = "";
        document.getElementById('message').offsetHeight; /* trigger reflow */
        document.getElementById('message').style.animation = "shake 0.5s linear 4";
        return;
    }
    let x = field.id.split("")[0];
    let y = field.id.split("")[1];
    let move = setMove(x, y);
    if (move == true) {
        field.innerHTML = player;
        restartButton = document.getElementById("button1");
        restartButton.disabled = true;
        restartButton.innerHTML = "Restart";
        if (checkEndState(board) != -1) {
            restartButton.disabled = false;
            return;
        }
        swap_player();
    }
    if (game_mode == "1Player") {
        ai_turn();
    }
}

function restart() {
    game_status = "ongoing";
    board = [["", "", ""], ["", "", ""], ["", "", ""]];
    player = "X";
    for (let row = 0; row < board.length; row++) {
        for (let col = 0; col < board.length; col++) {
            cell = document.getElementById(row.toString() + col.toString());
            cell.innerHTML = "";
            message = document.getElementById("message");
            message.innerHTML = "Player X's turn";
        }
    }
}

function minmax(cur_player, state) {
    let best = [-1, -1, -1];
    if (is_player_win(cur_player, state)) {
        best[2] = 1;
        return best;
    }
    if (is_player_win(get_other_player(cur_player), state)) {
        best[2] = -1;
        return best;
    }
    const empties = empty_cells(state);
    if (empties.length === 0) {
        best[2] = 0;
        return best;
    }
    if (empties.length === 9) {
        best = [parseInt(Math.random() * 3), parseInt(Math.random() * 3), 0];
        return best;
    }
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

function ai_turn() {
    best = minmax(player, board);
    const row = best[0];
    const col = best[1];
    if (setMove(row, col)) {
        field = document.getElementById(row.toString() + col.toString());
        field.innerHTML = player;
        if (checkEndState(board) != -1) {
            restartButton.disabled = false;
            return;
        }
        swap_player();
    }
}

function button1click(button) {
    if (button.innerHTML == "1 Player") {
        game_mode = "1Player";
        changeElementsForGame();
        button.innerHTML = "AI Turn";
    }
    else if (button.innerHTML == "Restart") {
        restart();
        if (game_mode === "1Player") {
            button.innerHTML = "AI Turn";
        }

    }
    else { // AI start
        ai_turn();
        button.innerHTML = "Restart";
        button.disabled = true;
    }
}

function changeElementsForGame() {
    game_status = "ongoing";
    button1 = message = document.getElementById("button1");
    button1.innerHTML = "AI Turn";
    button1.innerHTML = "Restart";
    button2 = message = document.getElementById("button2");
    button2.innerHTML = "Change game mode";
    message = document.getElementById("message");
    message.innerHTML = "Player " + player + "'s turn";

}

function button2click(button) {
    if (button.innerHTML == "2 Players") {
        changeElementsForGame();
        game_mode = "2Players";
    }
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