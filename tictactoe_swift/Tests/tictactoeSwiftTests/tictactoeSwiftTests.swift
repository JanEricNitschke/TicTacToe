import XCTest

@testable import tictactoeSwiftLib

final class tictactoe_swiftTests: XCTestCase {

  var tictactoe = TicTacToe()

  override func setUp() {
    tictactoe = TicTacToe()
    // This is the setUp() instance method.
    // XCTest calls it before each test method.
    // Set up any synchronous per-test state here.
  }

  func testSwapPlayer() throws {
    XCTAssertEqual("X", tictactoe.swapPlayer("O"))
    XCTAssertEqual("O", tictactoe.swapPlayer("X"))
    // XCTest Documenation
    // https://developer.apple.com/documentation/xctest

    // Defining Test Cases and Test Methods
    // https://developer.apple.com/documentation/xctest/defining_test_cases_and_test_methods
  }

  func testIsBoardFilled() {
    XCTAssertFalse(tictactoe.isBoardFilled())
    tictactoe.board = ["X", "X", "X", "O", "O", "O", "O", "X", "X"]
    XCTAssertTrue(tictactoe.isBoardFilled())
  }

  func testCheckWinCondition() {
    tictactoe.board = ["X", "X", "2", "3", "4", "5", "6", "7", "X"]
    XCTAssertEqual(
      tictactoe.checkWinCondition(condition: [0, 1, 2], player: "X"),
      ConditionResult(done: [0, 1], open: [2]))
    XCTAssertEqual(
      tictactoe.checkWinCondition(
        condition: [0, 1, 8], player: "X"), ConditionResult(done: [0, 1, 8], open: []))
    XCTAssertEqual(
      tictactoe.checkWinCondition(condition: [0, 1, 8], player: "O"),
      ConditionResult(done: [], open: []))
  }

  func testisPlayerWinRow() {
    tictactoe.board = ["X", "X", "X", "3", "4", "5", "6", "7", "X"]
    XCTAssertFalse(tictactoe.isPlayerWin(player: "O"))
    XCTAssertTrue(tictactoe.isPlayerWin(player: "X"))
  }

  func testisPlayerWinCol() {
    tictactoe.board = ["0", "O", "X", "3", "O", "5", "6", "O", "X"]
    XCTAssertFalse(tictactoe.isPlayerWin(player: "X"))
    XCTAssertTrue(tictactoe.isPlayerWin(player: "O"))
  }
  func testisPlayerWinDiag() {
    tictactoe.board = ["0", "1", "X", "3", "X", "5", "X", "O", "O"]
    XCTAssertFalse(tictactoe.isPlayerWin(player: "O"))
    XCTAssertTrue(tictactoe.isPlayerWin(player: "X"))
  }

  func testFixSpot() {
    tictactoe.board = ["0", "1", "X", "3", "X", "5", "X", "O", "O"]
    XCTAssertTrue(tictactoe.fixSpot(spot: 0, player: "X"))
    // Now occupied
    XCTAssertFalse(tictactoe.fixSpot(spot: 0, player: "X"))
    // Too small
    XCTAssertFalse(tictactoe.fixSpot(spot: -3, player: "O"))
    // Too big
    XCTAssertFalse(tictactoe.fixSpot(spot: 13, player: "O"))
  }

  func testRandomMove() {
    tictactoe.board = ["0", "1", "X", "3", "X", "5", "X", "O", "O"]
    for _ in 0..<100 {
      let move = tictactoe.randomMove()
      XCTAssertTrue([0, 1, 3, 5].contains(move.spot))
    }
  }

  func testWinMove() {
    tictactoe.board = ["X", "1", "O", "3", "X", "5", "6", "7", "8"]
    for _ in 0..<100 {
      let move = tictactoe.winMove(player: "X")
      XCTAssertEqual(8, move.spot)
    }
  }

  func testBlockWinMove() {
    tictactoe.board = ["X", "1", "O", "3", "X", "5", "6", "7", "8"]
    for _ in 0..<100 {
      let winMove = tictactoe.blockWinMove(player: "X")
      XCTAssertEqual(8, winMove.spot)
      let blockMove = tictactoe.blockWinMove(player: "O")
      XCTAssertEqual(8, blockMove.spot)
    }
  }

  func testMinMaxBaseCases() {
    tictactoe.board = ["X", "X", "X", "X", "X", "X", "X", "X", "X"]
    XCTAssertEqual(Move(spot: -1, endState: 1), tictactoe.minMax(player: "X"))
    XCTAssertEqual(Move(spot: -1, endState: -1), tictactoe.minMax(player: "O"))
    tictactoe.board = ["X", "O", "X", "X", "O", "O", "O", "X", "X"]
    XCTAssertEqual(Move(spot: -1, endState: 0), tictactoe.minMax(player: "X"))
    XCTAssertEqual(Move(spot: -1, endState: 0), tictactoe.minMax(player: "O"))
  }

  func testMinMaxTakesOpenSpot() {
    tictactoe.board = ["X", "X", "-", "O", "X", "O", "X", "O", "O"]
    XCTAssertEqual(Move(spot: 2, endState: 1), tictactoe.minMax(player: "X"))
    XCTAssertEqual(Move(spot: 2, endState: 1), tictactoe.minMax(player: "O"))
  }

  func testMinMaxBlocksWin() {
    tictactoe.board = ["O", "O", "X", "X", "-", "O", "-", "O", "X"]
    XCTAssertEqual(Move(spot: 4, endState: 0), tictactoe.minMax(player: "X"))
  }

  func testMinMaxTakesWin() {
    tictactoe.board = ["O", "O", "X", "X", "-", "-", "-", "O", "X"]
    XCTAssertEqual(Move(spot: 4, endState: 1), tictactoe.minMax(player: "O"))
  }

  func testMinMaxBest() {
    // Correctly block middle
    // Anything else can lead to a loss
    tictactoe.board = ["X", "-", "-", "-", "-", "-", "-", "-", "-"]
    XCTAssertEqual(Move(spot: 4, endState: 0), tictactoe.minMax(player: "O"))
  }
}
