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
--    18-JUL-2003   TW  Renamed from @AD.Spans@ to @Asis2.Spans@, changed the
--                      license, renamed @Get@ to @Find@, added the @As_Word@
--                      and @In_Comment_Too@ parameters to @Find@ and
--                      @Through@, and added extensive comments.
--  </HISTORY>
-------------------------------------------------------------------------------

pragma License (Modified_GPL);

with Asis.Text;

with Ada.Strings;

package Asis2.Spans is

   pragma Elaborate_Body;

   ----------------------------------------------------------------------------

   type Position is
      record
         Line   : Asis.Text.Line_Number        := 0;
         Column : Asis.Text.Character_Position := 0;
      end record;
   --  A type to express source positions. An @Asis.Text.Span@ can be seen
   --  as consisting of two @Position@s, a @Start@ position and a @Stop@
   --  position.

   Nil_Position : constant Position := (0, 0);

   function Is_Nil (Pos : in Position) return Boolean;
   --  Returns <CODE>Pos = Nil_Position</CODE>.

   function "<"  (Left, Right : in Position) return Boolean;
   function "<=" (Left, Right : in Position) return Boolean;
   function ">"  (Left, Right : in Position) return Boolean;
   function ">=" (Left, Right : in Position) return Boolean;
   --  Position comparisons.

   function Start (Span : in Asis.Text.Span) return Position;
   --  Returns the start position of @Span@, or a @Nil_Position@ if
   --  <CODE>Asis.Text.Is_Nil (Span)</CODE>.

   function Stop  (Span : in Asis.Text.Span) return Position;
   --  Returns the stop position of @Span@, or a @Nil_Position@ if
   --  <CODE>Asis.Text.Is_Nil (Span)</CODE>.

   procedure Set_Start
     (Span : in out Asis.Text.Span;
      Pos  : in     Position);
   --  Sets the start position of @Span@ to @Pos@. If
   --  <CODE>Is_Nil (Pos)</CODE>, the start position is set to @(1,1)@.

   procedure Set_Stop
     (Span : in out Asis.Text.Span;
      Pos  : in     Position);
   --  Sets the stop position of @Span@ to @Pos@.

   ----------------------------------------------------------------------------

   function Find
     (Element         : in Asis.Element;
      Pattern         : in Wide_String;
      Direction       : in Ada.Strings.Direction := Ada.Strings.Forward;
      From            : in Position              := Nil_Position;
      As_Word         : in Boolean               := True;
      In_Comments_Too : in Boolean               := False)
     return Asis.Text.Span;
   --  Searches within the @Element@'s text span as returned by @Get_Span@
   --  for the @Pattern@, starting from position @From@ going in the given
   --  @Direction@.
   --
   --  Returns an @Asis.Text.Nil_Span@ if (assume that
   --  <CODE>Region := Get_Span (Element)</CODE>):
   --  <UL>
   --    <LI>@Pattern@ is empty.
   --    <LI><CODE>Asis.Text.Is_Nil (Region)</CODE>.
   --    <LI><CODE>From >= Stop (Region) <B>and</B>
   --        Direction = Forward</CODE>.
   --    <LI><CODE>From <= Start (Region) <B>and</B>
   --        Direction = Backward</CODE>.
   --    <LI>The @Pattern@ cannot be found.
   --  </UL>
   --
   --  If @As_Word@ is @True@, only matches of @Pattern@ as whole words are
   --  considered matches. A "whole word" match is assumed it left and right
   --  of it are either the line's ends, or a non-alphanumeric character
   --  other than the underscore.
   --
   --  If @In_Comments_Too@ is @True@, the @Pattern@ is searched using the
   --  plain @Asis.Text.Line_Image@, otherwise @Asis.Text.Non_Comment_Image@
   --  is used.
   --
   --  If <CODE>Is_Nil (From)</CODE>, searching starts at
   --  <CODE>Start (Region)</CODE> if <CODE>Direction = Forward</CODE>, and at
   --  <CODE>Stop (Region)</CODE> otherwise.
   --
   --  Searching for the @Pattern@ is always case-insensitive, i.e. using
   --  the @Ada.Strings.Wide_Maps.Wide_Constants.Lower_Case_Map@.

   function Through
     (Element         : in Asis.Element;
      Pattern         : in Wide_String;
      Direction       : in Ada.Strings.Direction := Ada.Strings.Forward;
      From            : in Position              := Nil_Position;
      As_Word         : in Boolean               := True;
      In_Comments_Too : in Boolean               := False)
     return Asis.Text.Span;
   --  As @Find@ above, but if a match is found, returns a span from the
   --  beginning of the @Element@'s span up to the end of the match if
   --  <CODE>Direction = Forward</CODE>, or a span from the beginning of
   --  the match to the end of the @Element@'s span if <CODE>Direction =
   --  Backward</CODE>.

   function Find_Comment
     (Element   : in Asis.Element;
      From      : in Asis.Text.Line_Number_Positive;
      Direction : in Ada.Strings.Direction := Ada.Strings.Forward)
     return Position;
   --  Searches for a comment starting on line @From@ going in the
   --  given @Direction@. If no comment is found, returns @Nil_Position@.
   --
   --  If @Direction@ is @Forward@, the search stops at the end of the
   --  @Asis.Text.Compilation_Span@ of the @Element@, and returns a @Position@
   --  with <CODE>Column = 1</CODE> and @Line@ the line number of the comment
   --  line found. (I.e. the position is at the beginning of the comment line,
   --  but not necessarily at the beginning of the comment!)
   --
   --  if @Direction@ is @Backward@, the search stops at @Position@ @(1,1)@
   --  and returns a @Position@ with @Line@ the line number of the comment
   --  line found and @Column@ the last character of that line. (I.e., the
   --  position is at the <EM>end</EM> of the comment line.)
   --
   --  In both cases, the search stops if a line containing something else but
   --  a comment is found. Note that searching @Forward@ can start on a line
   --  containing a trailing comment; in this case, the function returns
   --  <CODE>(From, 1)</CODE>. When searching backwards, trailing comments
   --  are <EM>not</EM> included.

   function Expand_Comment
     (Element   : in Asis.Element;
      From      : in Position;
      Direction : in Ada.Strings.Direction := Ada.Strings.Forward)
     return Position;
   --  Expands a comment found by @Find_Comment@ in the given direction. The
   --  function returns a position that hgas @Column@ set as for @Find_Comment@
   --  and line the last comment line following from the given @Position@ in
   --  the given @Direction@. Searching stops when an empty line is found, or
   --  a line containing some non-comment is found.

   function Get_Span
     (Element : in Asis.Element)
     return Asis.Text.Span;
   --  Always use this instead of @Asis.Text.Element_Span@, which has been
   --  observed to fail sometimes in ASIS-for-GNAT 3.14p. (E.g., on record
   --  component declarations of the form "<CODE>X : Integer'Base;</CODE>").
   --
   --  Also adjusts the span of private library unit declarations to encompass
   --  the "private" (Bug C530-002 in ASIS-for-GNAT 3.16a), and corrects the
   --  span of private type declarations with more than one discriminant,
   --  which also comes out wrong in ASIS-for-GNAT 3.16a (Bug C602-001).
   --
   --  If no span can be determined, returns an @Asis.Text.Nil_Span@.

end Asis2.Spans;
