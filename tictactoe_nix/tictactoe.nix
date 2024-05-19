with (import <nixpkgs> { }).lib;
with builtins;

let
  # Function to create an empty board
  emptyBoard = [ "-" "-" "-" "-" "-" "-" "-" "-" "-" ];

  # Function to print the board in a readable format
  printBoard = board: ''
    ${concatStringsSep " " (take 3 board)}
    ${concatStringsSep " " (take 3 (drop 3 board))}
    ${concatStringsSep " " (drop 6 board)}
  '';

  # Function to check if the board is full
  isFull = board: all (c: c != "-") board;

  swapPlayer = player: if player == "X" then "O" else "X";

  # Function to check if the current player has won
  checkWin = { board, player }:
    let
      winningCombinations = [
        [ 0 1 2 ]
        [ 3 4 5 ]
        [ 6 7 8 ] # Rows
        [ 0 3 6 ]
        [ 1 4 7 ]
        [ 2 5 8 ] # Columns
        [ 0 4 8 ]
        [ 2 4 6 ] # Diagonals
      ];
      checkCombination = combination:
        all (index: elemAt board index == player) combination;
    in
    any checkCombination winningCombinations;


  # Function to find the first empty cell index
  findEmptyIndex = board:
    let
      checkIndex = index:
        if index == length board then
          -1
        else if elemAt board index == "-" then
          index
        else
          checkIndex (index + 1);
    in
    checkIndex 0;

  # Function to find the index for a winning move (if possible)
  trywinningMoveIndex = { board, player }:
    let
      winningCombinations = [
        [ 0 1 2 ]
        [ 3 4 5 ]
        [ 6 7 8 ] # Rows
        [ 0 3 6 ]
        [ 1 4 7 ]
        [ 2 5 8 ] # Columns
        [ 0 4 8 ]
        [ 2 4 6 ] # Diagonals
      ];

      findEmptyInCombination = combination:
        let
          emptyCells = filter (index: elemAt board index == "-") combination;
        in
        if length emptyCells == 1 then
          elemAt emptyCells 0
        else
          -1;

      checkCombination = combination:
        let
          emptyIndex = findEmptyInCombination combination;
        in
        if emptyIndex != -1 then
          all (index: elemAt board index == player || elemAt board index == "-") combination
        else
          false;

      validCombinations = filter (combination: checkCombination combination) winningCombinations;

      winningMove =
        if length validCombinations > 0 then
          findEmptyInCombination (head validCombinations)
        else
          -1;
    in
    winningMove;


  winningMoveIndex = { board, player }:
    let
      winningMove = trywinningMoveIndex { board = board; player = player; };
    in
    if winningMove != -1 then
      winningMove
    else
      findEmptyIndex board;


  winningBlockingMoveIndex = { board, player }:
    let
      winningMove = trywinningMoveIndex { board = board; player = player; };
      blockingMove = trywinningMoveIndex { board = board; player = swapPlayer player; };
    in
    if winningMove != -1 then
      winningMove
    else if blockingMove != -1 then
      blockingMove
    else
      findEmptyIndex board;


  # Function to find the index for a perfect move
  perfectMoveIndex = { board, player }: (perfectMove { board = board; player = player; }).index;

  placeMove = { board, player, index }:
    take index board ++ [ player ] ++ drop (index + 1) board;

  minimum = list:
    foldl (a: b: if a < b then a else b) (head list) (tail list);

  perfectMove = { board, player }:
    if checkWin { board = board; player = player; } then
      { index = -1; score = 1; }
    else if checkWin { board = board; player = swapPlayer player; } then
      { index = -1; score = -1; }
    else if isFull board then
      { index = -1; score = 0; }
    else
      let
        emptyIndexes = filter (index: elemAt board index == "-") [ 0 1 2 3 4 5 6 7 8 ];
        newBoards = map (index: placeMove { board = board; player = player; index = index; }) emptyIndexes;
        scores = map (newBoard: (perfectMove { board = newBoard; player = swapPlayer player; }).score) newBoards;
        bestScore = minimum scores;
        bestIndex = elemAt emptyIndexes (lists.findFirstIndex (score: score == bestScore) 0 scores);
      in
      { index = bestIndex; score = -bestScore; };



  # Function to make a move
  makeMove = { board, player, strategy }:
    let
      emptyIndex =
        if strategy == 0 then findEmptyIndex board
        else if strategy == 1 then winningMoveIndex { board = board; player = player; }
        else if strategy == 2 then winningBlockingMoveIndex { board = board; player = player; }
        else if strategy == 3 then perfectMoveIndex { board = board; player = player; }
        else -1;

      newBoard =
        if emptyIndex != -1 then
          take emptyIndex board ++ [ player ] ++ drop (emptyIndex + 1) board
        else
          board;
    in
    newBoard;

  # Function to play the game (loop until the board is full or the counter reaches 10)
  playGame = { board, player, strategyX, strategyO }:
    let
      newBoard = makeMove { board = board; player = player; strategy = if player == "X" then strategyX else strategyO; };
      nextPlayer = swapPlayer player;
      playerWon = checkWin { board = newBoard; player = player; };
    in
    if isFull newBoard || playerWon then
      ''
        ${printBoard newBoard}
        ${if playerWon then "Player " + player + " wins!" else "Draw!"}
      ''
    else
      ''
        ${printBoard newBoard}
        ${playGame { board = newBoard; player = nextPlayer; strategyX = strategyX; strategyO = strategyO; }}
      '';

  # Initial game state
  initialBoard = emptyBoard;
  initialPlayer = "X";
  initialStrategyX = 2; # Default strategy for player X
  initialStrategyO = 3; # Default strategy for player O

in
playGame { board = initialBoard; player = initialPlayer; strategyX = initialStrategyX; strategyO = initialStrategyO; }
