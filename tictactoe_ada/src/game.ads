with Ada.Containers.Vectors;
with Ada.Containers; use Ada.Containers;

--  @summary
--  Logic required for a game of TicTacToe.

--  @description
--  This package provides routines needed for
--  playing a game of TicTacToe. This includes getting
--  input from the user. Managing the board and checking
--  for win conditions. Also includes functionality
--  for playing alone vs the PC.

package Game is
   --  Valid entries that a game board can have.
   --  Number characters for empty spots and 'X' and 'O' for taken spots.
   type Array_Entry is ('1', '2', '3', '4', '5', '6', '7', '8', '9', 'X', 'O');
   --  Subtype for valid player characters.
   subtype Player_Char is Array_Entry range 'X' .. 'O';
   subtype Board_Index is Integer range 1 .. 9;
   subtype Rows is Board_Index range 1 .. 3;
   subtype Columns is Board_Index range 1 .. 3;
   type Board is array (Board_Index) of Array_Entry;
   type Condition is array (1 .. 3) of Board_Index;
   subtype Strength is Integer range 1 .. 4;
   Spot_Filled_Except : exception;

   package Spot_Vectors is new Ada.Containers.Vectors
     (Index_Type => Board_Index, Element_Type => Board_Index);

   use Spot_Vectors;

   --  A Record containing the results of checking a specific win condition.
   --  @field Spots_Taken Number of spots of the win condition already taken
   --   by the player.
   --  @field Spots_Open Vector of the spots that are not occupied at all.
   type Condition_Status is record
      Spots_Taken : Integer range 0 .. 9 := 0;
      Spots_Open  : Vector;
   end record;

   --  A move representing a possible AI move with position and game outcome.
   --  @field Spot Index of the spot the Move is made on.
   --  @field Ent_State The expected outcome of the game. Loss, Draw, Win.
   type Move is record
      Spot      : Board_Index;
      End_State : Integer range -1 .. 1 := -1;
   end record;

   procedure Show_Board (Game_Board : Board);
   --  Procedure for printing the board in human readable format.
   --  @param Game_Board The board to show.
   function Swap_Player (Player : Player_Char) return Player_Char;
   --  Procedure for Swapping between player characters.
   --  @param Player The current player to swap.
   procedure Fix_Spot
     (Player : Player_Char; Spot : Board_Index; Game_Board : in out Board);
   --  Procedure for fixing a spot for a player. Checks if spot is taken.
   --  @param Player The player to make the move.
   --  @param Spot The place to make the move at.
   --  @param Game_Board The board to make the move on.
   --  @exception Spot_Filled_Except raised
   --     if The spot is already occupied.
   procedure Clear_Spot (Spot : Board_Index; Game_Board : in out Board);
   --  Procedure for clearing a spot and assign the Integer char back.
   --  @param Spot Spot to clear and reassign.
   --  @ Game_Board Board to clear the spot on.
   function Board_Full (Game_Board : Board) return Boolean;
   --  Checks if the board is full. Indicates a draw if done after win check.
   --  @param Game_Board Board to check whether it is full.
   --  @return True if the board is full.
   function Check_Condition
     (Win_Condition : Condition; Player : Player_Char; Game_Board : Board)
      return Condition_Status;
   --  Checks how many spots of a particular win condition a player has taken.
   --  @param Win_Condition The condition to check.
   --  @param Player The player to check how many spots they have taken.
   --  @param Game_Board The board to check the condition on.
   --  @return The result of the check with how many spots are taken and
   --   which are still open.
   function Player_Win
     (Player : Player_Char; Game_Board : Board) return Boolean;
   --  Checks if a specific player has won the game
   --  @param Player The player to check for whether they have won.
   --  @param Game_Board the board to check if the player has won.
   --  @param True if the player has won.
   function Game_Over
     (Player : Player_Char; Game_Board : Board) return Boolean;
   --  Checks whether the game is over.
   --  @param Player The player that could have potentially won the game.
   --  @param Game_Board the board to check.
   --  @return True if the game is finished by draw or win.

   function Get_Player_Yes_No (Question : String) return Boolean;
   --  Get player input to a yes/no question.
   --  @param Question The question to ask the player.
   --  @return True if the player answered in the affirmative.
   function Get_AI_Opponent_Exists return Boolean;
   --  Ask the player if they want to play vs an AI.
   --  @return True if they want to play vs the AI.
   function Get_AI_Opponent_Start return Player_Char;
   --  Ask the player if the AI should make the first move.
   --  @return True if the AI should make the first move.
   function Get_AI_Opponent_Strength return Strength;
   --  Ask the player for the strength the AI should have.
   --  @return The strength the AI should have.

   function Random_Move (Game_Board : Board) return Move;
   --  Make a random valid move on the board.
   --  @param Game_Board The board to make the move on.
   --  @return A random valid move on the board.
   function Get_Winning_Move
     (Player : Player_Char; Game_Board : Board) return Move;
   --  Get a winning move if possible.
   --  @param Player Player to make the move as.
   --  @param Game_Board Board to make the move on.
   --  @return Potentially the winning move.
   --   If None exists the Endstate is set to -1
   function Winning_Move
     (Player : Player_Char; Game_Board : Board) return Move;
   --  Make a winning or random move.
   --  @param Player Player to make the move as.
   --  @param Game_Board Board to make the move on.
   --  @return Winning or random move.
   function Blocking_Winning_Move
     (Player : Player_Char; Game_Board : Board) return Move;
   --  Make a winning, blocking or random move.
   --  @param Player Player to make the move as.
   --  @param Game_Board Board to make the move on.
   --  @return Winning, blocking or random move.
   function Minmax
     (Player : Player_Char; Game_Board : in out Board) return Move;
   --  Make the best possible move.
   --  @param Player Player to make the move as.
   --  @param Game_Board Board to make the move on.
   --  @return Best possible move.

   procedure Player_Turn (Player : Player_Char; Game_Board : in out Board);
   --  Procedure to facilitate a player turn.
   --  @param Player The player to make a move.
   --  @param Game_Board The board to take the turn on.
   procedure AI_Turn
     (Player : Player_Char; Game_Board : in out Board; AI_Strength : Strength);
   --  Procedure to facilitate an AI turn.
   --  @param Player The player to make the move as.
   --  @param Game_Board The board to take the turn on.
   --  @param AI_Strength The strength to use for the move.
end Game;
