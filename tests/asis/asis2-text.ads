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
--    Commonly useful operations on @Wide_String@.
--  </PURPOSE>
--
--  <HISTORY>
--    18-JUL-2003   TW  Initial version.
--  </HISTORY>
-------------------------------------------------------------------------------

pragma License (Modified_GPL);

with Ada.Characters.Handling;
with Ada.Characters.Latin_1;
with Ada.Strings.Wide_Maps.Wide_Constants;

package Asis2.Text is

   pragma Elaborate_Body;

   function To_Lower
     (Source : in Wide_String)
     return Wide_String;
   --  Applies @Ada.Strings.Wide_Maps.Wide_Constants.Lower_Map@ to @Source@.

   function To_Upper
     (Source : in Wide_String)
     return Wide_String;
   --  Applies @Ada.Strings.Wide_Maps.Wide_Constants.Upper_Map@ to @Source@.

   function To_Basic
     (Source : in Wide_String)
     return Wide_String;
   --  Applies @Ada.Strings.Wide_Maps.Wide_Constants.Basic_Map@ to @Source@.

   function To_Mixed
     (Source : in Wide_String)
     return Wide_String;
   --  Applies @Ada.Strings.Wide_Maps.Wide_Constants.Upper_Map@ the first
   --  element in @Source@ and to all elements in source following a white
   --  space, punctuation, or the underscore.

   White_Space : constant Ada.Strings.Wide_Maps.Wide_Character_Set;

   function Trim
     (Source : in Wide_String)
     return Wide_String;
   --  Trims all leading or trailing characters in @White_Space@ from @Source@.

private

   use type Ada.Strings.Wide_Maps.Wide_Character_Set;

   White_Space : constant Ada.Strings.Wide_Maps.Wide_Character_Set :=
     Ada.Strings.Wide_Maps.Wide_Constants.Control_Set or
     Ada.Strings.Wide_Maps.To_Set
       (Ada.Characters.Handling.To_Wide_String
          (' ' & Ada.Characters.Latin_1.NBSP));

end Asis2.Text;
