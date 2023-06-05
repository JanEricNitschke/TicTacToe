with Ada.Text_IO;         use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

package body Game is
   Win_Conditions : constant array (1 .. 8) of Condition :=
     ((1, 2, 3), (4, 5, 6), (7, 8, 9), (1, 4, 7), (2, 5, 8), (3, 6, 9),
      (1, 5, 9), (3, 5, 7));

   procedure Show_Board (Game_Board : Board) is
      Line_Seperator : constant String := "---------------------";
   begin
      Put_Line (Line_Seperator);
      for Row in Rows loop
         for Column in Columns loop
            Put
              ("| " &
               Array_Entry'Image (Game_Board (((Row - 1) * 3) + Column)) &
               " |");
         end loop;
         New_Line;
         Put_Line (Line_Seperator);
      end loop;
   end Show_Board;

   function Swap_Player (Player : Player_Char) return Player_Char is
   begin
      if Player = 'X' then
         return 'O';
      end if;
      return 'X';
   end Swap_Player;

   Spot : Board_Index;
   Spot_Filled_Except : exception;

   procedure Fix_Spot
     (Player : Player_Char; Spot : Board_Index; Game_Board : in out Board)
   is
   begin
      if Game_Board (Spot) in Player_Char then
         raise Spot_Filled_Except;
      end if;
      Game_Board (Spot) := Player;
   end Fix_Spot;

   procedure Player_Turn (Player : Player_Char; Game_Board : in out Board) is
   begin
      loop
         Put_Line ("Player " & Player_Char'Image (Player) & "turn.");
         Show_Board (Game_Board);
         begin
            Get (Spot);
            Fix_Spot (Player, Spot, Game_Board);
            exit;
         exception
            when Spot_Filled_Except =>
               Put_Line ("Spot already filled");
            when Data_Error         =>
               Put_Line ("Spot has to be a valid integer in range 1..9");
               Skip_Line;
            when Constraint_Error   =>
               Put_Line ("Spot has to be a valid integer in range 1..9");
         end;
      end loop;
   end Player_Turn;

   function Board_Full (Game_Board : Board) return Boolean is
   begin
      for Content of Game_Board loop
         if Content not in Player_Char then
            return False;
         end if;
      end loop;
      return True;
   end Board_Full;

   function Check_Condition
     (Win_Condition : Condition; Player : Player_Char; Game_Board : Board)
      return Boolean
   is
   begin
      for Spot of Win_Condition loop
         if Game_Board (Spot) /= Player then
            return False;
         end if;
      end loop;
      return True;
   end Check_Condition;

   function Player_Win
     (Player : Player_Char; Game_Board : Board) return Boolean
   is
   begin
      for Win_Condition of Win_Conditions loop
         if Check_Condition (Win_Condition, Player, Game_Board) then
            return True;
         end if;
      end loop;
      return False;
   end Player_Win;

   function Game_Over (Player : Player_Char; Game_Board : Board) return Boolean
   is
   begin
      if Player_Win (Player, Game_Board) then
         Put_Line
           ("Player " & Player_Char'Image (Player) & " has won the game!");
         return True;
      end if;
      if Board_Full (Game_Board) then
         Put_Line ("Match Drawn!");
         return True;
      end if;
      return False;
   end Game_Over;

end Game;
