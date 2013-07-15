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

with Ada.Strings.Wide_Fixed;

package body Asis2.Text is

   package ASF renames Ada.Strings.Wide_Fixed;
   package ASM renames Ada.Strings.Wide_Maps;
   package ASC renames Ada.Strings.Wide_Maps.Wide_Constants;

   ----------------------------------------------------------------------------

   function To_Lower
     (Source : in Wide_String)
     return Wide_String
   is
   begin
      return ASF.Translate (Source, ASC.Lower_Case_Map);
   end To_Lower;

   function To_Upper
     (Source : in Wide_String)
     return Wide_String
   is
   begin
      return ASF.Translate (Source, ASC.Upper_Case_Map);
   end To_Upper;

   function To_Basic
     (Source : in Wide_String)
     return Wide_String
   is
   begin
      return ASF.Translate (Source, ASC.Basic_Map);
   end To_Basic;

   ----------------------------------------------------------------------------

   function To_Mixed
     (Source : in Wide_String)
     return Wide_String
   is
      Result    : Wide_String (1 .. Source'Length);
      Alpha_Num : Boolean     := False;
      J         : Natural     := 1;

      use ASC, ASM;

   begin
      for I in Source'Range loop
         if not Alpha_Num then
            Result (J) := Value (Upper_Case_Map, Source (I));
         else
            Result (J) := Value (Lower_Case_Map, Source (I));
         end if;
         Alpha_Num := Is_In (Source (I), Alphanumeric_Set);
         J := J + 1;
      end loop;
      return Result;
   end To_Mixed;

   ----------------------------------------------------------------------------

   function Trim
     (Source : in Wide_String)
     return Wide_String
   is
   begin
      return ASF.Trim (Source, White_Space, White_Space);
   end Trim;

end Asis2.Text;
