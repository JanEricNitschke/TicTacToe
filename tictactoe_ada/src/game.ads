with Ada.Containers.Vectors;
with Ada.Containers; use Ada.Containers;

package Game is
   type Array_Entry is ('1', '2', '3', '4', '5', '6', '7', '8', '9', 'X', 'O');
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

   type Condition_Status is record
      Spots_Taken  : Integer range 0 .. 9 := 0;
      Spots_Open : Vector;
   end record;

   type Move is record
      Spot      : Board_Index;
      End_State : Integer range -1 .. 1 := -1;
   end record;

   procedure Show_Board (Game_Board : Board);
   function Swap_Player (Player : Player_Char) return Player_Char;
   procedure Fix_Spot
     (Player : Player_Char; Spot : Board_Index; Game_Board : in out Board);
   procedure Clear_Spot (Spot : Board_Index; Game_Board : in out Board);
   function Board_Full (Game_Board : Board) return Boolean;
   function Check_Condition
     (Win_Condition : Condition; Player : Player_Char; Game_Board : Board)
      return Condition_Status;
   function Player_Win
     (Player : Player_Char; Game_Board : Board) return Boolean;
   function Game_Over
     (Player : Player_Char; Game_Board : Board) return Boolean;

   function Get_Player_Yes_No (Question : String) return Boolean;
   function Get_AI_Opponent_Exists return Boolean;
   function Get_AI_Opponent_Start return Player_Char;
   function Get_AI_Opponent_Strength return Strength;

   function Random_Move (Game_Board : Board) return Move;
   function Get_Winning_Move
     (Player : Player_Char; Game_Board : Board) return Move;
   function Winning_Move
     (Player : Player_Char; Game_Board : Board) return Move;
   function Blocking_Winning_Move
     (Player : Player_Char; Game_Board : Board) return Move;
   function Minmax
     (Player : Player_Char; Game_Board : in out Board) return Move;

   procedure Player_Turn (Player : Player_Char; Game_Board : in out Board);
   procedure AI_Turn
     (Player : Player_Char; Game_Board : in out Board; AI_Strength : Strength);
end Game;
