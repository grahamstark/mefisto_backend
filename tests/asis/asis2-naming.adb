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
--    Utility routines operating on naming expressions and defining names.
--  </PURPOSE>
--
--  <HISTORY>
--    08-JUL-2003   TW  Last release as part of @AdaBrowse@.
--    18-JUL-2003   TW  Created from operations in @AD.Queries@.
--  </HISTORY>
-------------------------------------------------------------------------------

pragma License (Modified_GPL);

with Ada.Exceptions;

with Asis.Compilation_Units;
with Asis.Declarations;
with Asis.Elements;
with Asis.Exceptions;
with Asis.Expressions;

with Asis2.Declarations;
with Asis2.Text;

package body Asis2.Naming is

   Package_Name : constant String := "Asis2.Naming";

   use Asis;
   use Asis.Declarations;
   use Asis.Elements;
   use Asis.Expressions;

   ----------------------------------------------------------------------------

   function Get_Name
     (Decl : in Asis.Declaration)
     return Asis.Defining_Name
   is
      Names : constant Name_List := Asis.Declarations.Names (Decl);
   begin
      return Names (Names'First);
   end Get_Name;

   ----------------------------------------------------------------------------

   procedure Verify_Name_Definition
     (Def  : in out Asis.Defining_Name;
      Name : in     Asis.Expression)
   is
   begin
      if Is_Nil (Def) or else Is_Nil (Name) then
         Def := Nil_Element;
         return;
      end if;
      --  Asis sometimes returns the wrong name.
      if Defining_Name_Kind (Def) = A_Defining_Expanded_Name then
         Def := Defining_Selector (Def);
      end if;
      declare
         Original : constant Wide_String :=
           Asis2.Text.To_Lower (Name_Image (Name));
         Def_Name : constant Wide_String :=
           Asis2.Text.To_Lower (Defining_Name_Image (Def));
      begin
         if Original /= Def_Name then
            --  Other differences (besides casing).
            Def := Nil_Element;
         end if;
      end;
   end Verify_Name_Definition;

   ----------------------------------------------------------------------------

   function Name_Expression_Image
     (Name : in Asis.Expression)
     return Wide_String
   is
      E : constant Asis.Expression_Kinds := Expression_Kind (Name);
   begin
      case E is
         when A_Selected_Component =>
            return Name_Expression_Image (Prefix (Name)) & "." &
                   Name_Expression_Image (Selector (Name));
         when An_Identifier |
              An_Enumeration_Literal =>
            declare
               Def : Asis.Defining_Name :=
                 Asis2.Declarations.Name_Definition (Name);
            begin
               Verify_Name_Definition (Def, Name);
               if not Is_Nil (Def) then
                  return Defining_Name_Image (Def);
               else
                  --  No definition (or multiple ones, or a wrong one) found:
                  --  use the expression's image instead.
                  return Name_Image (Name);
               end if;
            end;
         when A_Character_Literal |
              An_Operator_Symbol =>
            return Name_Image (Name);
         when An_Attribute_Reference =>
            return Name_Expression_Image (Prefix (Name)) & "'" &
                   Asis2.Text.To_Mixed
                     (Name_Image (Attribute_Designator_Identifier (Name)));
         when others =>
            Ada.Exceptions.Raise_Exception
              (Asis.Exceptions.ASIS_Inappropriate_Element'Identity,
               "Unexpected expression kind " & Expression_Kinds'Image (E) &
               " in " & Package_Name & ".Name_Expression_Image.");
            return "";
      end case;
   end Name_Expression_Image;

   ----------------------------------------------------------------------------

   function Name_Definition_Image
     (Name : in Asis.Defining_Name)
     return Wide_String
   is
   begin
      if Defining_Name_Kind (Name) = A_Defining_Expanded_Name then
         return Name_Expression_Image (Defining_Prefix (Name)) & "." &
                Defining_Name_Image (Defining_Selector (Name));
      else
         return Defining_Name_Image (Name);
      end if;
   end Name_Definition_Image;

   ----------------------------------------------------------------------------

   function Get_Single_Name
     (Decl : in Asis.Declaration)
     return Wide_String
   is
   begin
      if Declaration_Kind (Decl) = Not_A_Declaration then
         return "";
      end if;
      declare
         Names : constant Name_List := Asis.Declarations.Names (Decl);
      begin
         if Names'Last /= Names'First then return ""; end if;
         return Name_Definition_Image (Names (Names'First));
      end;
   end Get_Single_Name;

   ----------------------------------------------------------------------------

   function Full_Unit_Name
     (Unit : in Asis.Compilation_Unit)
     return Wide_String
   is
      --  ASIS-for-GNAT 3.14p has a problem with unit names for children
      --  that are generic instantiations: it returns only the last
      --  component. Try to correct that.
      --
      --  Also, ASIS-for-GNAT uses the program text to construct that unit
      --  name, which is consistent with the encouragement given by the ASIS
      --  standard. We, however, want to have capitalization as in the parent
      --  units declarations!

      use Asis.Compilation_Units;

      Parent    : constant Compilation_Unit :=
        Corresponding_Parent_Declaration (Unit);
      --  Nil if Unit is Standard. A_Nonexistent_Declaration if parent unit is
      --  not in context. Standard if Unit is a root unit.

      Name      : Asis.Defining_Name := Get_Name (Unit_Declaration (Unit));

   begin
      if Is_Nil (Parent) or else
         Unit_Kind (Parent) = A_Nonexistent_Declaration
      then
         --  Standard, or something went wrong: fall back to the standard ASIS
         --  implementation.
         return Unit_Full_Name (Unit);
      end if;
      if Is_Nil (Corresponding_Parent_Declaration (Parent)) then
         --  Grandparent is nil, therefore Parent is Standard: a root unit.
         return Name_Definition_Image (Name);
      else
         --  A child unit
         if Defining_Name_Kind (Name) = A_Defining_Expanded_Name then
            Name := Defining_Selector (Name);
         end if;
         return Full_Unit_Name (Parent) & "." &
                Name_Definition_Image (Name);
      end if;
   end Full_Unit_Name;

   ----------------------------------------------------------------------------

   function Container_Name
     (Element : in Asis.Element)
     return Wide_String
   is
      Unit : constant Asis.Declaration :=
        Unit_Declaration (Enclosing_Compilation_Unit (Element));

      Dot  : constant Wide_String := ".";

      function Parent
        (Unit    : in Asis.Declaration;
         Element : in Asis.Element)
        return Wide_String
      is
         D : Asis.Declaration :=
           Asis2.Declarations.Enclosing_Declaration (Element);
      begin --  Parent
         --  Skip enclosing declarations until we hit a package declaration,
         --  a task or a PO, or a subprogram declaration.
         loop
            if Is_Nil (D) then return ""; end if;
            case Declaration_Kind (D) is
               when A_Single_Task_Declaration |
                    A_Single_Protected_Declaration |
                    A_Task_Type_Declaration |
                    A_Protected_Type_Declaration |
                    A_Task_Body_Declaration |
                    A_Protected_Body_Declaration |
                    An_Entry_Declaration |
                    An_Entry_Body_Declaration =>
                  --  All the things that cannot be a compilation unit:
                  return Parent (Unit, D) & Dot & Get_Single_Name (D);

               when A_Package_Declaration |
                    A_Package_Body_Declaration |
                    A_Package_Renaming_Declaration |
                    A_Generic_Package_Declaration |
                    A_Generic_Procedure_Declaration |
                    A_Generic_Function_Declaration |
                    A_Generic_Package_Renaming_Declaration |
                    A_Generic_Procedure_Renaming_Declaration |
                    A_Generic_Function_Renaming_Declaration |
                    A_Procedure_Declaration |
                    A_Procedure_Body_Declaration |
                    A_Procedure_Renaming_Declaration |
                    A_Function_Declaration |
                    A_Function_Body_Declaration |
                    A_Function_Renaming_Declaration |
                    A_Package_Instantiation |
                    A_Procedure_Instantiation |
                    A_Function_Instantiation |
                    A_Formal_Procedure_Declaration |
                    A_Formal_Function_Declaration =>
                  --  All the things that *can* be a compilation unit:
                  if Is_Equal (Unit, D) then
                     return Full_Unit_Name (Enclosing_Compilation_Unit (D));
                  else
                     return Parent (Unit, D) & Dot & Get_Single_Name (D);
                  end if;
               when others =>
                  D := Asis2.Declarations.Enclosing_Declaration (D);
            end case;
         end loop;
         return "";
      end Parent;

   begin --  Container_Name;
      if Is_Equal (Unit, Element) then return ""; end if;
      return Parent (Unit, Element);
   end Container_Name;

   ----------------------------------------------------------------------------

   function Fully_Qualified_Name
     (Name : in Asis.Defining_Name)
     return Wide_String
   is
      Decl      : constant Asis.Declaration      :=
        Asis2.Declarations.Enclosing_Declaration (Name);
      Unit      : constant Asis.Compilation_Unit :=
        Enclosing_Compilation_Unit (Decl);
      Unit_Decl : constant Asis.Declaration      :=
        Unit_Declaration (Unit);
   begin
      if Is_Equal (Unit_Decl, Decl) then
         return Full_Unit_Name (Unit);
      end if;
      --  It's not a unit itself...
      declare
         Container : constant Wide_String := Container_Name (Decl);
      begin
         if Container'Length = 0 then
            return Name_Definition_Image (Name);
         else
            return Container & "." & Name_Definition_Image (Name);
         end if;
      end;
   end Fully_Qualified_Name;

end Asis2.Naming;
