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
--    Utility routines for container elements, i.e. packages and task or
--    protected declarations or definitions.
--  </PURPOSE>
--
--  <HISTORY>
--    04-JUN-2003   TW  First release as part of @AdaBrowse@.
--    18-JUL-2003   TW  Created from operations in @AD.Queries@.
--  </HISTORY>
-------------------------------------------------------------------------------

pragma License (Modified_GPL);

with Ada.Characters.Handling;
with Ada.Exceptions;

with Asis.Declarations;
with Asis.Definitions;
with Asis.Elements;
with Asis.Exceptions;

package body Asis2.Container_Elements is

   Package_Name : constant String := "Asis2.Container_Elements";

   package ACH renames Ada.Characters.Handling;

   use Asis;
   use Asis.Declarations;
   use Asis.Definitions;
   use Asis.Elements;

   function Has_Private
     (Element : in Asis.Element)
     return Boolean
   is
   begin
      case Declaration_Kind (Element) is
         when A_Package_Declaration |
              A_Generic_Package_Declaration =>
            return Asis.Declarations.Is_Private_Present (Element);
         when A_Task_Type_Declaration |
              A_Protected_Type_Declaration =>
            return Asis.Definitions.Is_Private_Present
                     (Type_Declaration_View (Element));
         when A_Single_Task_Declaration |
              A_Single_Protected_Declaration =>
            return Asis.Definitions.Is_Private_Present
                     (Object_Declaration_View (Element));
         when Not_A_Declaration =>
            case Definition_Kind (Element) is
               when A_Task_Definition |
                    A_Protected_Definition =>
                  return Asis.Definitions.Is_Private_Present (Element);
               when others =>
                  null;
            end case;
         when others =>
            null;
      end case;
      return False;
   end Has_Private;

   function Visible_Items
     (Element         : in Asis.Element;
      Include_Pragmas : in Boolean := False)
     return Asis.Declarative_Item_List
   is
   begin
      case Declaration_Kind (Element) is
         when A_Package_Declaration |
              A_Generic_Package_Declaration =>
            return Visible_Part_Declarative_Items (Element, Include_Pragmas);
         when A_Task_Type_Declaration |
              A_Protected_Type_Declaration =>
            return Visible_Part_Items
                     (Type_Declaration_View (Element), Include_Pragmas);
         when A_Single_Task_Declaration |
              A_Single_Protected_Declaration =>
            return Visible_Part_Items
                     (Object_Declaration_View (Element), Include_Pragmas);
         when Not_A_Declaration =>
            case Definition_Kind (Element) is
               when A_Task_Definition |
                    A_Protected_Definition =>
                  return Visible_Part_Items (Element, Include_Pragmas);
               when others =>
                  null;
            end case;
         when others =>
            null;
      end case;
      Ada.Exceptions.Raise_Exception
        (Asis.Exceptions.ASIS_Inappropriate_Element'Identity,
         "This element is not allowed in " &
         Package_Name & ".Visible_Items: " &
         ACH.To_String (Debug_Image (Element)));
      return Asis.Nil_Element_List;
   end Visible_Items;

   function Private_Items
     (Element         : in Asis.Element;
      Include_Pragmas : in Boolean := False)
     return Asis.Declarative_Item_List
   is
   begin
      case Declaration_Kind (Element) is
         when A_Package_Declaration |
              A_Generic_Package_Declaration =>
            return Private_Part_Declarative_Items (Element, Include_Pragmas);
         when A_Task_Type_Declaration |
              A_Protected_Type_Declaration =>
            return Private_Part_Items
                     (Type_Declaration_View (Element), Include_Pragmas);
         when A_Single_Task_Declaration |
              A_Single_Protected_Declaration =>
            return Private_Part_Items
                     (Object_Declaration_View (Element), Include_Pragmas);
         when Not_A_Declaration =>
            case Definition_Kind (Element) is
               when A_Task_Definition |
                    A_Protected_Definition =>
                  return Private_Part_Items (Element, Include_Pragmas);
               when others =>
                  null;
            end case;
         when others =>
            null;
      end case;
      Ada.Exceptions.Raise_Exception
        (Asis.Exceptions.ASIS_Inappropriate_Element'Identity,
         "This element is not allowed in " &
         Package_Name & ".Private_Items: " &
         ACH.To_String (Debug_Image (Element)));
      return Asis.Nil_Element_List;
   end Private_Items;

end Asis2.Container_Elements;
