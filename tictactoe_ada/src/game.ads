package Game is
   type Array_Entry is ('X', 'O', '1', '2', '3', '4', '5', '6', '7', '8', '9');
   subtype Player_Char is Array_Entry range 'X' .. 'O';
   --  subtype Open_Spot is Array_Entries range '1' .. '9';
   subtype Board_Index is Integer range 1 .. 9;
   subtype Rows is Board_Index range 1 .. 3;
   subtype Columns is Board_Index range 1 .. 3;
   type Board is array (Board_Index) of Array_Entry;
   type Condition is array (1 .. 3) of Board_Index;
   procedure Show_Board (Game_Board : Board);
   function Swap_Player (Player : Player_Char) return Player_Char;
   procedure Fix_Spot
     (Player : Player_Char; Spot : Board_Index; Game_Board : in out Board);
   procedure Player_Turn (Player : Player_Char; Game_Board : in out Board);
   function Board_Full (Game_Board : Board) return Boolean;
   function Check_Condition
     (Win_Condition : Condition; Player : Player_Char; Game_Board : Board)
      return Boolean;
   function Player_Win
     (Player : Player_Char; Game_Board : Board) return Boolean;
   function Game_Over
     (Player : Player_Char; Game_Board : Board) return Boolean;
end Game;
