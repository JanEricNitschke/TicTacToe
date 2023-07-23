import Foundation

struct ConditionResult: Equatable {
  var done: [Int]
  var open: [Int]

  init(done: [Int] = [], open: [Int] = []) {
    self.done = done
    self.open = open
  }
}

struct Move: Equatable {
  var spot: Int
  var endState: Int
  init(spot: Int, endState: Int) {
    self.spot = spot
    self.endState = endState
  }
}

public class TicTacToe {
  var board = ["0", "1", "2", "3", "4", "5", "6", "7", "8"]
  var winConditions = [
    [0, 1, 2], [3, 4, 5], [6, 7, 8],  // Rows
    [0, 3, 6], [1, 4, 7], [2, 5, 8],  // Cols
    [0, 4, 8], [2, 4, 6],  // Diagonals
  ]
  var aiOpponent = false
  var aiMarker = "X"
  var aiStrength = 0
  var playerMarkers: Set = ["X", "O"]

  public init() {}

  func swapPlayer(_ player: String) -> String {
    if player == "X" {
      return "O"
    }
    return "X"
  }

  public func showBoard() {
    let lineSeparator = "---------------"
    let sideLength = 3
    print(lineSeparator)

    for row in 0...2 {
      for col in 0...2 {
        print("| \(board[(row * sideLength) + col]) |", terminator: "")
      }
      print("")

      print(lineSeparator)
    }
  }

  func isBoardFilled() -> Bool {
    return board.allSatisfy { playerMarkers.contains($0) }
  }

  func checkWinCondition(condition: [Int], player: String) -> ConditionResult {
    var result = ConditionResult()
    condition.forEach {
      if board[$0] == player {
        result.done.append($0)
      } else if !playerMarkers.contains(board[$0]) {
        result.open.append($0)
      }
    }
    return result
  }

  func isPlayerWin(player: String) -> Bool {
    return winConditions.contains {
      checkWinCondition(condition: $0, player: player).done.count == 3
    }
  }

  func fixSpot(spot: Int, player: String) -> Bool {
    if !(0...8 ~= spot) {
      print("ERROR: Spot has to be in range [0-8]!")
      return false
    }
    if playerMarkers.contains(board[spot]) {
      print("ERROR: Spot \(spot) is already occupied")
      return false
    }
    board[spot] = player
    return true
  }

  func playerTurn(player: String) {
    while true {
      print("Player \(player) turn.")
      showBoard()
      print("Where to make your next move? [0-8]")
      let spot = Int(readLine() ?? "")
      if spot == nil {
        print("ERROR: Input must be a valid integer!")
        continue
      }
      if fixSpot(spot: spot!, player: player) {
        break
      }
    }
  }

  func emptyCells() -> [Int] {
    var empties: [Int] = []
    board.enumerated().forEach { (index, value) in
      if !playerMarkers.contains(value) {
        empties.append(index)
      }
    }
    return empties
  }

  func randomMove() -> Move {
    // randomMove should never be called on a full board
    return Move(spot: emptyCells().randomElement()!, endState: 0)
  }

  func winningMove(player: String) -> Move? {
    for condition in winConditions {
      let result = checkWinCondition(condition: condition, player: player)
      if result.done.count == 2 && result.open.count == 1 {
        return Move(spot: result.open[0], endState: -1)
      }
    }
    return nil
  }

  func winMove(player: String) -> Move {
    return winningMove(player: player) ?? randomMove()
  }

  func blockWinMove(player: String) -> Move {
    return winningMove(player: player) ?? winningMove(player: swapPlayer(player)) ?? randomMove()
  }

  func minMax(player: String) -> Move {
    // Already won
    if isPlayerWin(player: player) {
      return Move(spot: -1, endState: 1)
    }

    // Already lost
    if isPlayerWin(player: swapPlayer(player)) {
      return Move(spot: -1, endState: -1)
    }

    let empties = emptyCells()

    // Already drawn
    if empties.isEmpty {
      return Move(spot: -1, endState: 0)
    }

    // New game.
    // Make random move for diversity and speed
    if empties.count == 9 {
      return randomMove()
    }

    // Recursive minmax
    var bestMove = Move(spot: -1, endState: -1)
    empties.forEach {
      board[$0] = player
      let currentMove = minMax(player: swapPlayer(player))
      if -currentMove.endState >= bestMove.endState {
        bestMove = Move(spot: $0, endState: -currentMove.endState)
      }
      board[$0] = String($0)
    }
    return bestMove
  }

  func aiTurn(player: String) {
    print("AI turn as \(player).")
    showBoard()
    var aiMove: Move
    switch aiStrength {
    case 1: aiMove = randomMove()
    case 2: aiMove = winMove(player: player)
    case 3: aiMove = blockWinMove(player: player)
    default: aiMove = minMax(player: player)
    }
    board[aiMove.spot] = player
    Thread.sleep(forTimeInterval: 1)
  }

  func getYesNo(question: String) -> Bool {
    while true {
      print(question)
      let respone = readLine() ?? ""
      if ["Y", "y"].contains(respone) {
        return true
      }
      if ["N", "n"].contains(respone) {
        return false
      }
    }
  }

  func getAiOpponnent() {
    aiOpponent = getYesNo(question: "Play alone vs AI? [y/n]")
  }

  func getAiStart() {
    if getYesNo(question: "Should the AI make the first move? [y/n]") {
      aiMarker = "X"
    } else {
      aiMarker = "O"
    }
  }

  func getAiStrength() {
    print("AI strength settings:")
    print("1: Easy")
    print("2: Medium")
    print("3: Hard")
    print("4: Impossible")
    while !(1...4 ~= aiStrength) {
      print("How strong should the AI be? [1 - 4]")
      let response = Int(readLine() ?? "")
      if response != nil {
        aiStrength = response!
      } else {
        print("Bad choice")
      }
    }
  }

  public func initializeGame() {
    getAiOpponnent()
    if aiOpponent {
      getAiStart()
      getAiStrength()
    }

  }

  public func play() {
    var player = "X"
    while true {
      if aiOpponent && player == aiMarker {
        aiTurn(player: player)
      } else {
        playerTurn(player: player)
      }
      if isPlayerWin(player: player) {
        print("Player \(player) wins the game!")
        break
      }

      if isBoardFilled() {
        print("Match drawn!")
        break
      }

      player = swapPlayer(player)
    }
    showBoard()
  }
}
