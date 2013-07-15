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

with Asis;

package Asis2.Container_Elements is

   pragma Elaborate_Body;

   function Has_Private
     (Element : in Asis.Element)
     return Boolean;
   --  Returns @True@ if @Element@ has an explicit private part. This is a
   --  wrapper around @Asis.Declarations.Is_Private_Present@ and
   --  @Asis.Definitions.Is_Private_Present@. Returns @False@ if no explicit
   --  @private@ keyword is present, or the @Element@ isn't appropriate.
   --
   --  Appropriate elements:
   --
   --  Declaration_Kinds :
   --     A_Package_Declaration
   --     A_Generic_Package_Declaration
   --     A_Task_Type_Declaration
   --     A_Single_Task_Declaration
   --     A_Protected_Type_Declaration
   --     A_Single_Protected_Declaration
   --
   --  Definition_Kinds :
   --     A_Task_Definition
   --     A_Protected_Definition

   function Visible_Items
     (Element         : in Asis.Element;
      Include_Pragmas : in Boolean := False)
     return Asis.Declarative_Item_List;
   --  Returns the list of items declared in the visible part of a (generic)
   --  package, task (type), or protected type or object. This is a wrapper
   --  around @Asis.Declarations.Visible_Part_Declarative_Items@ and
   --  @Asis.Definitions.Visible_Part_Items@.
   --
   --  Appropriate elements:
   --
   --  Declaration_Kinds :
   --     A_Package_Declaration
   --     A_Generic_Package_Declaration
   --     A_Task_Type_Declaration
   --     A_Single_Task_Declaration
   --     A_Protected_Type_Declaration
   --     A_Single_Protected_Declaration
   --
   --  Definition_Kinds :
   --     A_Task_Definition
   --     A_Protected_Definition

   function Private_Items
     (Element         : in Asis.Element;
      Include_Pragmas : in Boolean := False)
     return Asis.Declarative_Item_List;
   --  Returns the list of items declared in the private part of a (generic)
   --  package, task (type), or protected type or object. This is a wrapper
   --  around @Asis.Declarations.Private_Part_Declarative_Items@ and
   --  @Asis.Definitions.Private_Part_Items@.
   --
   --  Appropriate elements:
   --
   --  Declaration_Kinds :
   --     A_Package_Declaration
   --     A_Generic_Package_Declaration
   --     A_Task_Type_Declaration
   --     A_Single_Task_Declaration
   --     A_Protected_Type_Declaration
   --     A_Single_Protected_Declaration
   --
   --  Definition_Kinds :
   --     A_Task_Definition
   --     A_Protected_Definition

end Asis2.Container_Elements;
