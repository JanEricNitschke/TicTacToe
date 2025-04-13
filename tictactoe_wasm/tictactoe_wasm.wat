(module
  ;; Memory: 9 cells, 1 byte each
  (memory (export "memory") 1)

  ;; 0 = empty, 1 = X, 2 = O
  ;; 0 for X turn, 1 for O turn
  (global $current_player_turn  (mut i32) (i32.const 0))

  ;; Global to track game status
  ;; 0 for ongoing, 1 for X win, 2 for O win, 3 for draw
  (global $game_status (mut i32) (i32.const 0))

  ;; Exported function: place a move
  (func (export "play") (param $cell i32) (result i32)
    ;; Check if the game is done
    (if (i32.ne (global.get $game_status) (i32.const 0))
      (then
        ;; Game is done, no move allowed
        (return (i32.const 0))
      )
    )

    ;; Game still ongoing, check if cell is empty
    ;; Load cell value
    (i32.load8_u (local.get $cell))
    ;; Check if empty
    (i32.eqz)
    (if
      ;; Result type on the stack after the if
      (result i32)
      (then
        ;; Write current player
        (i32.store8 (local.get $cell)
          (i32.add (global.get $current_player_turn ) (i32.const 1))
        )
        ;; Switch turn
        (global.set $current_player_turn
          (i32.xor (global.get $current_player_turn ) (i32.const 1))
        )
        ;; Success
        (i32.const 1)
      )
      (else
        ;; Fail (cell occupied)
        (i32.const 0)
      )
    )
  )

    ;; Helper to check a triple
  (func $check_triple (param $cell1 i32) (param $cell2 i32) (param $cell3 i32) (result i32)
    (local $cell_value i32)
    (local.set $cell_value (i32.load8_u (local.get $cell1)))
    (if (result i32)
      (i32.and
        (local.get $cell_value)
        (i32.and
          (i32.eq (local.get $cell_value) (i32.load8_u (local.get $cell2)))
          (i32.eq (local.get $cell_value) (i32.load8_u (local.get $cell3)))
        )
      )
      (then (local.get $cell_value))
      (else (i32.const 0))
    )
  )

  ;; check_winner returns: 0 (ongoing), 1 (X wins), 2 (O wins)
  (func $check_winner (result i32)
    (local $result i32)
    (block $exit
      (local.set $result (call $check_triple (i32.const 0) (i32.const 1) (i32.const 2)))
      (br_if $exit (local.get $result))
      (local.set $result (call $check_triple (i32.const 3) (i32.const 4) (i32.const 5)))
      (br_if $exit (local.get $result))
      (local.set $result (call $check_triple (i32.const 6) (i32.const 7) (i32.const 8)))
      (br_if $exit (local.get $result))
      (local.set $result (call $check_triple (i32.const 0) (i32.const 3) (i32.const 6)))
      (br_if $exit (local.get $result))
      (local.set $result (call $check_triple (i32.const 1) (i32.const 4) (i32.const 7)))
      (br_if $exit (local.get $result))
      (local.set $result (call $check_triple (i32.const 2) (i32.const 5) (i32.const 8)))
      (br_if $exit (local.get $result))
      (local.set $result (call $check_triple (i32.const 0) (i32.const 4) (i32.const 8)))
      (br_if $exit (local.get $result))
      (local.set $result (call $check_triple (i32.const 2) (i32.const 4) (i32.const 6)))
      (br_if $exit (local.get $result))
      (local.set $result (i32.const 0)) ;; No winner
    )
    (local.get $result)
  )

  ;; check_draw returns: 1 (draw), 0 (not draw)
  (func $check_draw (result i32)
    (local $cell_index i32)
    (local $cell_value i32)
    (local.set $cell_index (i32.const 0))
    (loop $loop
      ;; Read each cell
      (local.set $cell_value (i32.load8_u (local.get $cell_index)))

      ;; If any cell is empty, not a draw
      (if (i32.eqz (local.get $cell_value))
        (then (return (i32.const 0)))
      )

      (local.set $cell_index (i32.add (local.get $cell_index) (i32.const 1)))
      (br_if $loop (i32.lt_u (local.get $cell_index) (i32.const 9)))
    )

    ;; All cells are filled, return draw
    (return (i32.const 1))
  )

  ;; check_end combines winner check and draw check
  (func (export "check_end") (result i32)
    (local $result i32)
    ;; If game is already over, return the result immediately
    (if (i32.ne (global.get $game_status) (i32.const 0))
      (then
        (return (global.get $game_status))
      )
    )

    ;; Check for winner
    (call $check_winner)
    (if (i32.ne (local.tee $result) (i32.const 0))
      (then
        (global.set $game_status (local.get $result))
        (return (local.get $result))
      )
    )

    ;; Check for draw
    (call $check_draw)
    (if (i32.eq (local.tee $result) (i32.const 1))
      (then
        ;; 3 means draw
        (global.set $game_status (i32.const 3))
        (return (i32.const 3))
      )
    )

    ;; Game is ongoing
    (i32.const 0)
  )
)
