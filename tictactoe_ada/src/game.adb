with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Text_IO;         use Ada.Text_IO;
with Ada.Numerics.Discrete_Random;

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

   procedure Fix_Spot
     (Player : Player_Char; Spot : Board_Index; Game_Board : in out Board)
   is
   begin
      if Game_Board (Spot) in Player_Char then
         raise Spot_Filled_Except;
      end if;
      Game_Board (Spot) := Player;
   end Fix_Spot;

   procedure Clear_Spot (Spot : Board_Index; Game_Board : in out Board) is
   begin
      Game_Board (Spot) := Array_Entry'Val (Spot - 1);
   end Clear_Spot;

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
      return Condition_Status
   is
      Status      : Condition_Status;
      Open_Vector : Vector;
   begin
      Status := (0, Open_Vector);
      for Spot of Win_Condition loop
         if Game_Board (Spot) = Player then
            --  Occupied by the player
            Status.Spots_Taken := Status.Spots_Taken + 1;
         elsif Game_Board (Spot) not in Player_Char then
            --  Not occupied by any player
            Status.Spots_Open.Append (Spot);
         end if;
      end loop;
      return Status;
   end Check_Condition;

   function Player_Win
     (Player : Player_Char; Game_Board : Board) return Boolean
   is
   begin
      for Win_Condition of Win_Conditions loop
         if Check_Condition (Win_Condition, Player, Game_Board).Spots_Taken = 3
         then
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

   function Get_Player_Yes_No (Question : String) return Boolean is
      type Answer is (y, n);
      User_Answer : Answer;
      package Answer_IO is new Enumeration_IO (Answer);
   begin
      loop
         begin
            Put_Line (Question);
            Answer_IO.Get (User_Answer);
            if User_Answer = y then
               return True;
            end if;
            return False;
         exception
            when Data_Error =>
               Put_Line ("Invalid input.");
               Skip_Line;
         end;
      end loop;
   end Get_Player_Yes_No;

   function Get_AI_Opponent_Exists return Boolean is
   begin
      return Get_Player_Yes_No ("Play alone vs AI?[y/n]");
   end Get_AI_Opponent_Exists;

   function Get_AI_Opponent_Start return Player_Char is
   begin
      if Get_Player_Yes_No ("Should the AI make the first move?[y/n]") then
         return 'X';
      end if;
      return 'O';
   end Get_AI_Opponent_Start;

   function Get_AI_Opponent_Strength return Strength is
      AI_Strength : Strength;
   begin
      Put_Line ("AI strength settings:");
      Put_Line ("1: Easy");
      Put_Line ("2: Medium");
      Put_Line ("3: Hard");
      Put_Line ("4: Impossible");
      loop
         begin
            Put_Line ("How strong should the AI be?[1-4]");
            Get (AI_Strength);
            return AI_Strength;
         exception
            when Data_Error       =>
               Put_Line
                 ("ERROR: Input must be a valid integer in range [1-4]!");
               Skip_Line;
            when Constraint_Error =>
               Put_Line
                 ("ERROR: Input must be a valid integer in range [1-4]!");
         end;
      end loop;
   end Get_AI_Opponent_Strength;

   procedure Player_Turn (Player : Player_Char; Game_Board : in out Board) is
      Spot : Board_Index;
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

   function Open_Spots (Game_Board : Board) return Vector is
      Free_Spots : Vector;
   begin
      for Spot in Game_Board'Range loop
         if Game_Board (Spot) not in Player_Char then
            Free_Spots.Append (Spot);
         end if;
      end loop;
      return Free_Spots;
   end Open_Spots;

   function Random_Move (Game_Board : Board) return Move is

      Free_Spots : Vector;

   begin
      Free_Spots := Open_Spots (Game_Board);
      declare
         subtype Rand_Range is Natural range 1 .. Integer (Free_Spots.Length);
         package Rand_Spot is new Ada.Numerics.Discrete_Random (Rand_Range);
         use Rand_Spot;
         Gen : Generator;
      begin
         Reset (Gen);
         return (Free_Spots (Random (Gen)), -1);
      end;
   end Random_Move;

   function Get_Winning_Move
     (Player : Player_Char; Game_Board : Board) return Move
   is
      Status : Condition_Status;
   begin
      for Win_Condition of Win_Conditions loop
         Status := Check_Condition (Win_Condition, Player, Game_Board);
         if Status.Spots_Taken = 2 and then Status.Spots_Open.Length = 1 then
            return (Status.Spots_Open.First_Element, 1);
         end if;
      end loop;
      return (1, -1);
   end Get_Winning_Move;

   function Winning_Move (Player : Player_Char; Game_Board : Board) return Move
   is
      Winning_Move : Move;
   begin
      Winning_Move := Get_Winning_Move (Player, Game_Board);
      if Winning_Move.End_State = 1 then
         return Winning_Move;
      end if;
      return Random_Move (Game_Board);
   end Winning_Move;

   function Blocking_Winning_Move
     (Player : Player_Char; Game_Board : Board) return Move
   is
      Best_Move : Move;
   begin
      Best_Move := Get_Winning_Move (Player, Game_Board);
      if Best_Move.End_State = 1 then
         return Best_Move;
      end if;
      Best_Move := Get_Winning_Move (Swap_Player (Player), Game_Board);
      --  Blocking move
      if Best_Move.End_State = 1 then
         return Best_Move;
      end if;
      return Random_Move (Game_Board);
   end Blocking_Winning_Move;

   function Minmax
     (Player : Player_Char; Game_Board : in out Board) return Move
   is
      Best_Move    : Move;
      Current_Move : Move;
      Free_Spots   : Vector;
   begin
      if Player_Win (Player, Game_Board) then
         return (1, 1);
      end if;
      if Player_Win (Swap_Player (Player), Game_Board) then
         return (1, -1);
      end if;
      Free_Spots := Open_Spots (Game_Board);
      if Integer (Free_Spots.Length) = 1 then
         return (1, 0);
      end if;
      if Integer (Free_Spots.Length) = 9 then
         return Random_Move (Game_Board);
      end if;

      for Spot of Free_Spots loop
         Game_Board (Spot) := Player;
         Current_Move      := Minmax (Swap_Player (Player), Game_Board);
         if -Current_Move.End_State >= Best_Move.End_State then
            Best_Move := (Spot, -Current_Move.End_State);
         end if;
         Clear_Spot (Spot, Game_Board);
      end loop;
      return Best_Move;
   end Minmax;

   procedure AI_Turn
     (Player : Player_Char; Game_Board : in out Board; AI_Strength : Strength)
   is
      Best_Move : Move;
   begin
      Put_Line ("AI turn as " & Player_Char'Image (Player));
      Show_Board (Game_Board);
      case AI_Strength is
         when 1 =>
            Best_Move := Random_Move (Game_Board);
         when 2 =>
            Best_Move := Winning_Move (Player, Game_Board);
         when 3 =>
            Best_Move := Blocking_Winning_Move (Player, Game_Board);
         when 4 =>
            Best_Move := Minmax (Player, Game_Board);
      end case;
      Fix_Spot (Player, Best_Move.Spot, Game_Board);
      delay 1.0;
   end AI_Turn;

end Game;
