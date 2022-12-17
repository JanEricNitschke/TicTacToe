let board = [["", "", ""], ["", "", ""], ["", "", ""]];
let player = "X";
let game_status = "paused";
let game_mode = "";
let difficulty_setting = 4;

function handleSelectChange(event) {
    var selectElement = event.target;

    difficulty_setting = selectElement.value;
}

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

function color_red(fields) {
    for (const field of fields) {
        row = field[0];
        col = field[1];
        cell = document.getElementById(row.toString() + col.toString());
        console.log(cell, row.toString() + col.toString());
        cell.style.color = "#ff0000";
    }
}

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
        if (checkEndState(board)) {
            restartButton.disabled = false;
            return;
        }
        swap_player();
        if (game_mode == "1Player") {
            ai_turn();
        }
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
            cell.style.color = "#FF9900";
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

function random_move(_, state) {
    const empties = empty_cells(state);
    move = empties[Math.floor(Math.random() * empties.length)];
    return [move[0], move[1], 0];
}

function get_blocking_move(cur_player, state) {
    return get_winning_move(get_other_player(cur_player), state);
}

function get_winning_move(cur_player, state) {
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
    for (let row = 0; row < state.length; row++) {
        for (let col = 0; col < state.length; col++) {
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

    for (let win_condition of win_conditions.values()) {
        if (win_condition.size == 1) {
            const coords = win_condition.values().next().value.split("_");
            return [coords[0], coords[1], 0];
        }
    }
    return null;
}

function win_move(cur_player, state) {
    let winning_move = get_winning_move(cur_player, state);
    if (winning_move != null) {
        return winning_move;
    }
    return random_move(cur_player, state);
}

function block_win_move(cur_player, state) {
    let winning_move = get_winning_move(cur_player, state);
    if (winning_move != null) {
        return winning_move;
    }
    let blocking_move = get_blocking_move(cur_player, state);
    if (blocking_move != null) {
        return blocking_move;
    }
    return random_move(cur_player, state);
}

function ai_turn() {
    let best = null;
    if (difficulty_setting == 1) {
        best = random_move(player, board);
    }
    else if (difficulty_setting == 2) {
        best = win_move(player, board);
    }
    else if (difficulty_setting == 3) {
        best = block_win_move(player, board);
    }
    else {
        best = minmax(player, board);
    }
    const row = best[0];
    const col = best[1];
    if (setMove(row, col)) {
        field = document.getElementById(row.toString() + col.toString());
        field.innerHTML = player;
        if (checkEndState(board)) {
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