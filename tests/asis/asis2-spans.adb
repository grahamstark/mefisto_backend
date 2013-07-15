-------------------------------------------------------------------------------
--
--  This unit is part of the @Asis2@ ASIS secondary library.
--
--  <STRONG>Copyright (c) 2002, 2003 by Thomas Wolf.</STRONG>
--  <BLOCKQUOTE>
--    AdaBrowse is free software; you can redistribute it and/or modify it
--    under the terms of the  GNU General Public License as published by the
--    Free Software  Foundation; either version 2, or (at your option) any
--    later version. AdaBrowse is distributed in the hope that it will be
--    useful, but <EM>without any warranty</EM>; without even the implied
--    warranty of <EM>merchantability or fitness for a particular purpose.</EM>
--    See the GNU General Public License for  more details. You should have
--    received a copy of the GNU General Public License with this distribution,
--    see file "<A HREF="GPL.txt">GPL.txt</A>". If not, write to the Free
--    Software Foundation, 59 Temple Place - Suite 330, Boston, MA 02111-1307,
--    USA.
--  </BLOCKQUOTE>
--  <BLOCKQUOTE>
--    As a special exception from the GPL, if other files instantiate generics
--    from this unit, or you link this unit with other files to produce an
--    executable, this unit does not by itself cause the resulting executable
--    to be covered by the GPL. This exception does not however invalidate any
--    other reasons why the executable file might be covered by the GPL.
--  </BLOCKQUOTE>
--
--  <AUTHOR>
--    Thomas Wolf  (TW) <E_MAIL>
--  </AUTHOR>
--
--  <PURPOSE>
--    Utility routines working on text spans.
--  </PURPOSE>
--
--  <HISTORY>
--    02-FEB-2002   TW  First release as part of @AdaBrowse@.
--    20-MAR-2002   TW  Added @Get_Span@.
--    24-APR-2002   TW  Changed 'Find_Comment' and 'Expand_Comment' such that
--                      they catch trailing comments, too.
--    18-JUL-2003   TW  Renamed from @AD.Spans@ to @Asis2.Spans@, changed the
--                      license, renamed @Get@ to @Find@, added the @As_Word@
--                      and @In_Comment_Too@ parameters to @Find@ and
--                      @Through@, and added extensive comments, and removed
--                      (and re- implemented) things formerly imported from
--                      my @Util@ or @GAL@ subsystems because I wanted to keep
--                      the @Asis2@ independent of those other libraries, and
--                      also because I need to properly handle @Wide_String@s
--                      here.
--  </HISTORY>
-------------------------------------------------------------------------------

pragma License (Modified_GPL);

with Ada.Strings.Wide_Fixed;
with Ada.Strings.Wide_Maps.Wide_Constants;

with Asis.Compilation_Units;
with Asis.Elements;
with Asis.Exceptions;
with Asis.Text;

with Asis2.Text;

package body Asis2.Spans is

   package ASF renames Ada.Strings.Wide_Fixed;
   package ASM renames Ada.Strings.Wide_Maps;
   package ASC renames Ada.Strings.Wide_Maps.Wide_Constants;

   use Asis2.Text;

   ----------------------------------------------------------------------------

   function Is_Nil
     (Pos : in Position)
     return Boolean
   is
   begin
      return Pos = Nil_Position;
   end Is_Nil;

   ----------------------------------------------------------------------------

   function "<"  (Left, Right : in Position) return Boolean
   is
      use Asis.Text;
   begin
      if Left.Line = Right.Line then
         return Left.Column < Right.Column;
      else
         return Left.Line < Right.Line;
      end if;
   end "<";

   function "<=" (Left, Right : in Position) return Boolean
   is
   begin
      return not (Right < Left);
   end "<=";

   function ">"  (Left, Right : in Position) return Boolean
   is
   begin
      return Right < Left;
   end ">";

   function ">=" (Left, Right : in Position) return Boolean
   is
   begin
      return not (Left < Right);
   end ">=";

   ----------------------------------------------------------------------------

   function Start (Span : in Asis.Text.Span) return Position
   is
   begin
      if Asis.Text.Is_Nil (Span) then
         return Nil_Position;
      else
         return Position'(Span.First_Line, Span.First_Column);
      end if;
   end Start;

   function Stop  (Span : in Asis.Text.Span) return Position
   is
   begin
      if Asis.Text.Is_Nil (Span) then
         return Nil_Position;
      else
         return Position'(Span.Last_Line, Span.Last_Column);
      end if;
   end Stop;

   ----------------------------------------------------------------------------

   procedure Set_Start
     (Span : in out Asis.Text.Span;
      Pos  : in     Position)
   is
   begin
      if Pos = Nil_Position then
         Span.First_Line   := 1;
         Span.First_Column := 1;
      else
         Span.First_Line   := Pos.Line;
         Span.First_Column := Pos.Column;
      end if;
   end Set_Start;

   procedure Set_Stop
     (Span : in out Asis.Text.Span;
      Pos  : in     Position)
   is
   begin
      Span.Last_Line   := Pos.Line;
      Span.Last_Column := Pos.Column;
   end Set_Stop;

   ----------------------------------------------------------------------------

   function Get_Checked_Span
     (Element : in Asis.Element)
     return Asis.Text.Span
   is
   begin
      return Asis.Text.Element_Span (Element);
   exception
      when Asis.Exceptions.ASIS_Failed =>
         return Asis.Text.Nil_Span;
   end Get_Checked_Span;

   ----------------------------------------------------------------------------

   procedure Search
     (Element      : in     Asis.Element;
      Span         : in out Asis.Text.Span;
      Pattern      : in     Wide_String;
      Direction    : in     Ada.Strings.Direction := Ada.Strings.Forward;
      From         : in     Position              := Nil_Position;
      Words_Only   : in     Boolean               := True;
      Comments_Too : in     Boolean               := False)
   is
      use type Ada.Strings.Direction;

      function Is_Word
        (Source      : in Wide_String;
         Start, Stop : in Natural)
        return Boolean;
      pragma Inline (Is_Word);

      function Is_Word
        (Source      : in Wide_String;
         Start, Stop : in Natural)
        return Boolean
      is
         Dash : constant Wide_Character := '_';
      begin
         if not Words_Only then return True; end if;
         declare
            Blank_Left : constant Boolean :=
              Start <= Source'First or else
              (Source (Start - 1) /= Dash and then
               not ASM.Is_In (Source (Start - 1), ASC.Alphanumeric_Set));
            Blank_Right : constant Boolean :=
              Stop >= Source'Last or else
              (Source (Stop + 1) /= Dash and then
               not ASM.Is_In (Source (Stop + 1), ASC.Alphanumeric_Set));
         begin
            return Blank_Left and Blank_Right;
         end;
      end Is_Word;

      function Find_Forward
        (Source    : in Wide_String;
         Pattern   : in Wide_String)
        return Natural
      is
         I      : Natural          := 0;
         J      : Natural          := Source'First;
         Length : constant Natural := Pattern'Length;

      begin
         while J + Length - 1 <= Source'Last loop
            I := ASF.Index (Source (J .. Source'Last), Pattern);
            exit when
              I = 0 or else
              Is_Word (Source (J ..  Source'Last), I, I + Length - 1);
            I := 0;
            J := J + Length + 1;
         end loop;
         return I;
      end Find_Forward;

      function Find_Backward
        (Source    : in Wide_String;
         Pattern   : in Wide_String)
        return Natural
      is
         I      : Natural          := 0;
         J      : Integer          := Source'Last;
         Length : constant Natural := Pattern'Length;
      begin
         while J >= Source'First + Length - 1 loop
            I := ASF.Index (Source (Source'First .. J), Pattern,
                            Ada.Strings.Backward);
            exit when
              I = 0 or else
              Is_Word (Source (Source'First .. J), I, I + Length - 1);
            I := 0;
            J := J - Length - 1;
         end loop;
         return I;
      end Find_Backward;

      function Get_Line
        (Line : in Asis.Text.Line)
        return Wide_String
      is
      begin
         if Comments_Too then
            return Asis.Text.Line_Image (Line);
         else
            return Asis.Text.Non_Comment_Image (Line);
         end if;
      end Get_Line;

   begin
      if Pattern'Last < Pattern'First or else Asis.Text.Is_Nil (Span) then
         Span := Asis.Text.Nil_Span; return;
      end if;
      if From /= Nil_Position then
         if Direction = Ada.Strings.Forward then
            Set_Start (Span, From);
         else
            Set_Stop (Span, From);
         end if;
      end if;
      declare
         Lines : constant Asis.Text.Line_List  :=
           Asis.Text.Lines (Element, Span);
         Col   : Asis.Text.Character_Position;
         Line  : Asis.Text.Line_Number         := Span.First_Line;
         Pat   : constant Wide_String          := To_Lower (Pattern);
      begin
         if Direction = Ada.Strings.Forward then
            for I in Lines'Range loop
               Col := Find_Forward (To_Lower (Get_Line (Lines (I))), Pat);
               if Col > 0 then
                  Line := I; exit;
               end if;
            end loop;
         else
            for I in reverse Lines'Range loop
               Col := Find_Backward (To_Lower (Get_Line (Lines (I))), Pat);
               if Col > 0 then
                  Line := I; exit;
               end if;
            end loop;
         end if;
         if Col > 0 then
            Span := Asis.Text.Span'(First_Line   => Line,
                                    First_Column => Col,
                                    Last_Line    => Line,
                                    Last_Column  => Col + Pattern'Length - 1);
         else
            Span := Asis.Text.Nil_Span;
         end if;
      end;
   end Search;

   ----------------------------------------------------------------------------

   function Get_Span
     (Element : in Asis.Element)
     return Asis.Text.Span
   is
      Result : Asis.Text.Span := Get_Checked_Span (Element);
   begin
      if not Asis.Text.Is_Nil (Result) then
         --  Now expand the span to encompass the "private" of private library
         --  unit declarations! (Yes, I know that this is not quite correct
         --  in terms of the Ada syntax. But it is useful, because it allows
         --  me to treat private library unit declarations just like public
         --  ones. In fact, I think that's the way it should be, even in ASIS.)
         declare
            use Asis.Compilation_Units;
            use Asis.Elements;
            use Asis;

            Unit : constant Asis.Compilation_Unit :=
              Enclosing_Compilation_Unit (Element);

         begin
            if Is_Equal (Element, Unit_Declaration (Unit)) then
               if Unit_Class (Unit) = A_Private_Declaration then
                  declare
                     Span    : Asis.Text.Span                    := Result;
                     --  ASIS-for-GNAT-3.16a (and probably 3.15p, too!) has a
                     --  bug here; the "private" isn't included either! Hence
                     --  we use the unit declaration's span and set the start
                     --  to (1, 1)! Reported as C530-002.
                     Clauses : constant Asis.Context_Clause_List :=
                       Context_Clause_Elements (Unit, True);
                  begin
                     Set_Start (Span, Position'(1, 1));
                     --  Restrict the span such that we only search up to the
                     --  end of the context clauses.
                     if Clauses'Length > 0 then
                        declare
                           Stop_At : constant Asis.Text.Span :=
                             Get_Checked_Span (Clauses (Clauses'Last));
                        begin
                           if not Asis.Text.Is_Nil (Stop_At) then
                              Set_Start (Span, Stop (Stop_At));
                           end if;
                        end;
                     end if;
                     Search (Element, Span, "private", Ada.Strings.Backward,
                             Start (Result));
                     if not Asis.Text.Is_Nil (Span) then
                        Set_Start (Result, Start (Span));
                     end if;
                  end;
               end if; --  private?
            else
               --  Ok, it's not a unit declaration. Now there is another
               --  problem with private types with discriminants. ASIS-for-GNAT
               --  3.16a only searches up to the next semicolon to determine
               --  the end of the span, but one needs to search to the first
               --  semicolon following the keyword "private"!
               --  This corrects bug C602-001 in ASIS-for-GNAT 3.16a.
               if Declaration_Kind (Element) = A_Private_Type_Declaration then
                  declare
                     Span    :          Asis.Text.Span := Result;
                     Stop_At : constant Asis.Text.Span :=
                       Get_Span (Enclosing_Element (Element));
                  begin
                     Set_Stop (Span, Stop (Stop_At));
                     Search (Element, Span, "private", Ada.Strings.Forward,
                             Start (Span));
                     if not Asis.Text.Is_Nil (Span) then
                        declare
                           From : constant Position := Stop (Span);
                        begin
                           Set_Stop (Span, Stop (Stop_At));
                           Search (Element, Span, ";", Ada.Strings.Forward,
                                   From, False);
                           if not Asis.Text.Is_Nil (Span) then
                              Set_Stop (Result, Stop (Span));
                           end if;
                        end;
                     end if;
                  end;
               elsif Definition_Kind (Element) = A_Private_Type_Definition then
                  --  Set the span to the span of the "private" keyword. First
                  --  get the *declaration's* span using a recursive call:
                  declare
                     Span : Asis.Text.Span :=
                       Get_Span (Enclosing_Element (Element));
                  begin
                     Search (Element, Span, "private", Ada.Strings.Backward,
                             Stop (Span));
                     if not Asis.Text.Is_Nil (Span) then
                        Result := Span;
                     end if;
                  end;
               end if;
            end if;
         end;
      end if;
      return Result;
   end Get_Span;

   ----------------------------------------------------------------------------

   function Find
     (Element         : in Asis.Element;
      Pattern         : in Wide_String;
      Direction       : in Ada.Strings.Direction := Ada.Strings.Forward;
      From            : in Position              := Nil_Position;
      As_Word         : in Boolean               := True;
      In_Comments_Too : in Boolean               := False)
     return Asis.Text.Span
   is
      Result : Asis.Text.Span := Get_Span (Element);
   begin
      Search
        (Element, Result, Pattern, Direction, From, As_Word, In_Comments_Too);
      return Result;
   end Find;

   function Through
     (Element         : in Asis.Element;
      Pattern         : in Wide_String;
      Direction       : in Ada.Strings.Direction := Ada.Strings.Forward;
      From            : in Position              := Nil_Position;
      As_Word         : in Boolean               := True;
      In_Comments_Too : in Boolean               := False)
     return Asis.Text.Span
   is
      Found_At : Asis.Text.Span :=
        Find (Element, Pattern, Direction, From, As_Word, In_Comments_Too);

      use type Ada.Strings.Direction;

   begin
      if not Asis.Text.Is_Nil (Found_At) then
         if Direction = Ada.Strings.Forward then
            Set_Start (Found_At, Start (Get_Span (Element)));
         else
            Set_Stop  (Found_At, Stop (Get_Span (Element)));
         end if;
      end if;
      return Found_At;
   end Through;

   ----------------------------------------------------------------------------

   function Find_Comment
     (Element   : in Asis.Element;
      From      : in Asis.Text.Line_Number_Positive;
      Direction : in Ada.Strings.Direction := Ada.Strings.Forward)
     return Position
   is

      use Asis.Text;

      use type Ada.Strings.Direction;

      Result     : Position              := Nil_Position;
      Check_Line : Asis.Text.Line_Number := From;
   begin
      if Direction = Ada.Strings.Backward then
         while Check_Line > 1 loop
            Check_Line := Check_Line - 1;
            declare
               Lines : constant Line_List :=
                 Asis.Text.Lines (Element, Check_Line, Check_Line);
            begin
               if not Is_Nil (Lines (Lines'First)) then
                  exit when
                    Trim (Non_Comment_Image (Lines (Lines'First)))'Length > 0;
                  if
                     Trim (Comment_Image (Lines (Lines'First)))'Length > 0
                  then
                     --  We do have a comment on that line!
                     Result := (Check_Line, Length (Lines (Lines'First)));
                     exit;
                  end if;
               end if;
            end;
         end loop;
      else
         declare
            Maximum : constant Position    :=
              Stop (Compilation_Span (Element));
            First   : constant Line_Number := Check_Line;
         begin
            while Check_Line < Maximum.Line loop
               declare
                  Lines : constant Line_List :=
                    Asis.Text.Lines (Element, Check_Line, Check_Line);
               begin
                  if not Is_Nil (Lines (Lines'First)) then
                     exit when
                       Check_Line > First
                       and then
                       Trim (Non_Comment_Image (Lines (Lines'First)))'Length
                       > 0;
                     if
                        Trim (Comment_Image (Lines (Lines'First)))'Length > 0
                     then
                        --  We do have a comment on that line!
                        Result := (Check_Line, 1);
                        exit;
                     end if;
                  end if;
               end;
               Check_Line := Check_Line + 1;
            end loop;
         end;
      end if;
      return Result;
   end Find_Comment;

   ----------------------------------------------------------------------------

   function Expand_Comment
     (Element   : in Asis.Element;
      From      : in Position;
      Direction : in Ada.Strings.Direction := Ada.Strings.Forward)
     return Position
   is

      use Asis.Text;

      use type Ada.Strings.Direction;

      Result  : Position;
      Check   : Position := From;

   begin
      if Is_Nil (Check) then raise Program_Error; end if;
      if Direction = Ada.Strings.Backward then
         Result := (Check.Line, 1);
         while Check.Line > 1 loop
            Check.Line := Check.Line - 1;
            declare
               Lines : constant Line_List :=
                 Asis.Text.Lines (Element, Check.Line, Check.Line);
            begin
               exit when
                 Is_Nil (Lines (Lines'First))
                 or else
                 Trim (Non_Comment_Image (Lines (Lines'First)))'Length > 0
                 or else
                 Trim (Comment_Image (Lines (Lines'First)))'Length = 0;
            end;
            Result.Line := Check.Line;
         end loop;
      else
         declare
            Maximum : constant Position    :=
              Stop (Compilation_Span (Element));
            First   : constant Line_Number := Check.Line;
         begin
            Result := Nil_Position;
            while Check.Line <= Maximum.Line loop
               declare
                  Lines : constant Line_List :=
                    Asis.Text.Lines (Element, Check.Line, Check.Line);
               begin
                  exit when
                    Is_Nil (Lines (Lines'First))
                    or else
                    (Check.Line > First and then
                     Trim
                       (Non_Comment_Image (Lines (Lines'First)))'Length > 0);
                  exit when
                    Trim (Comment_Image (Lines (Lines'First)))'Length = 0;
                  Result := (Check.Line, Length (Lines (Lines'First)));
               end;
               Check.Line := Check.Line + 1;
            end loop;
         end;
      end if;
      return Result;
   end Expand_Comment;

end Asis2.Spans;
