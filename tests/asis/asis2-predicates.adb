-------------------------------------------------------------------------------
--
--  This unit is part of the @Asis2@ ASIS secondary library.
--
--  <STRONG>Copyright (c) 2003 by Thomas Wolf.</STRONG>
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
--     Thomas Wolf  (TW) <E_MAIL>
--  </AUTHOR>
--
--  <PURPOSE>
--    Useful predicates on @Asis.Element@. All predicates return @False@ if
--    called with inappropriate element kinds.
--
--    Whereever the following descriptions specify "a declaration of", this
--    also allows "a defining name in a declaration of".
--
--    Wherever the following descriptions specify "a declaration of a type" or
--    " a type declaration", this also allows "a type definition" of such a
--    type.
--
--    Mentions of "type" include generic formal types, "variable" includes
--    generic formal "in out" objects, and so on.
--
--    If @Element@ is an @Expression@, the predicates on types are also
--    applicable, they refer to the type of the expression. If the @Expression@
--    is a name (identifier, operator, enumeration literal, or selected
--    component), they refer to the referenced defining name.
--  </PURPOSE>
--
--  <HISTORY>
--   05-JUN-2003   TW  Initial version.
--   08-JUL-2003   TW  Added 'Is_Package'; changed 'Unique_Name' to really
--                     return the fully qualified name.
--   18-JUL-2003   TW  Removed the string operations, put into the @Asis2@
--                     library, changed the license.
--  </HISTORY>
-------------------------------------------------------------------------------

pragma License (Modified_GPL);

with Asis.Compilation_Units;
with Asis.Definitions;
with Asis.Declarations;
with Asis.Elements;
with Asis.Expressions;

with Asis2.Container_Elements;
with Asis2.Declarations;
with Asis2.Naming;
with Asis2.Text;

package body Asis2.Predicates is

   use Asis;
   use Asis.Definitions;
   use Asis.Declarations;
   use Asis.Elements;
   use Asis.Expressions;

   function Decl
     (Element : in Asis.Element)
     return Asis.Element
   is
   begin
      case Element_Kind (Element) is
         when An_Expression =>
            case Expression_Kind (Element) is
               when An_Identifier |
                    An_Enumeration_Literal |
                    An_Operator_Symbol |
                    A_Character_Literal |
                    A_Selected_Component =>
                  return Asis2.Declarations.Name_Declaration (Element);
               when others =>
                  return Corresponding_Expression_Type (Element);
            end case;
         when A_Defining_Name | A_Definition =>
            return Asis2.Declarations.Enclosing_Declaration (Element);
         when A_Declaration =>
            return Element;
         when others =>
            return Nil_Element;
      end case;
   end Decl;

   function Def
     (Decl : in Asis.Declaration)
     return Asis.Definition
   is
      D : constant Declaration_Kinds := Declaration_Kind (Decl);
   begin
      if D in A_Type_Declaration or else D = A_Subtype_Declaration then
         if D /= An_Incomplete_Type_Declaration then
            return Type_Declaration_View (Decl);
         else
            return Nil_Element;
         end if;
      elsif D in An_Object_Declaration then
         return Object_Declaration_View (Decl);
      end if;
      return Nil_Element;
   end Def;

   function Type_Of
     (D : in Asis.Declaration)
     return Asis.Declaration
   is
   begin
      case Declaration_Kind (D) is
         when A_Type_Declaration |
              A_Subtype_Declaration =>
            return D;
         when A_Variable_Declaration |
              A_Constant_Declaration |
              A_Deferred_Constant_Declaration |
              An_Integer_Number_Declaration |
              A_Real_Number_Declaration =>
            return Type_Of (Decl (Asis.Definitions.Subtype_Mark (Def (D))));
         when A_Function_Declaration |
              A_Function_Renaming_Declaration |
              A_Formal_Function_Declaration |
              A_Generic_Function_Declaration =>
            return Type_Of (Decl (Result_Profile (D)));
         when A_Function_Instantiation =>
            return Type_Of (Corresponding_Declaration (D));
         when A_Formal_Object_Declaration =>
            return Type_Of (Decl (Declaration_Subtype_Mark (Def (D))));
         when others =>
            null;
      end case;
      return Nil_Element;
   end Type_Of;

   function Base
     (D : in Asis.Declaration)
     return Asis.Declaration
   is
      Type_Decl : constant Declaration := Type_Of (D);
   begin
      case Declaration_Kind (Type_Decl) is
         when A_Type_Declaration |
              A_Subtype_Declaration =>
            return Corresponding_First_Subtype (Type_Decl);
         when others =>
            null;
      end case;
      return Nil_Element;
   end Base;

   function Root
     (D : in Asis.Declaration)
     return Asis.Declaration
   is
      Type_Decl : constant Declaration := Type_Of (D);
   begin
      case Declaration_Kind (Type_Decl) is
         when A_Subtype_Declaration =>
            return Root (Base (Type_Decl));
         when others =>
            declare
               Def : constant Asis.Definition :=
                 Asis2.Predicates.Def (Type_Decl);
            begin
               if Is_Nil (Def) then return Nil_Element; end if;
               case Definition_Kind (Def) is
                  when A_Type_Definition =>
                     case Type_Kind (Def) is
                        when A_Derived_Type_Definition |
                             A_Derived_Record_Extension_Definition =>
                           return Corresponding_Root_Type (Def);
                        when others =>
                           return Type_Decl;
                     end case;
                  when A_Formal_Type_Definition =>
                     if Formal_Type_Kind (Def) =
                        A_Formal_Derived_Type_Definition
                     then
                        return
                          Root (Decl (Asis.Definitions.Subtype_Mark (Def)));
                     else
                        return Type_Decl;
                     end if;
                  when others =>
                     return Type_Decl;
               end case;
            end;
      end case;
   end Root;

   ----------------------------------------------------------------------------
   --  Units

   function Is_Private
     (Element : in Asis.Element)
     return Boolean
   is
   begin
      if Is_Nil (Element) then return False; end if;
      declare
         D : constant Asis.Declaration := Decl (Element);
      begin
         if Is_Nil (D) then return False; end if;
         if Is_Unit (D) then
            case Asis.Compilation_Units.Unit_Class
                   (Enclosing_Compilation_Unit (D))
            is
               when A_Private_Declaration |
                    A_Private_Body =>
                  return True;
               when others =>
                  null;
            end case;
         else
            declare
               Items : constant Asis.Element_List :=
                 Asis2.Container_Elements.Private_Items
                   (Asis2.Declarations.Enclosing_Declaration (D));
            begin
               for I in Items'Range loop
                  if Is_Equal (D, Items (I)) then return True; end if;
               end loop;
            end;
         end if;
      end;
      return False;
   end Is_Private;

   function Is_Separate
     (Element : in Asis.Element)
     return Boolean
   is
   begin
      if Is_Nil (Element) then return False; end if;
      declare
         D : constant Asis.Declaration := Decl (Element);
      begin
         if Is_Nil (D) then return False; end if;
         if Is_Unit (D) then
            if Asis.Compilation_Units.Unit_Class
                 (Enclosing_Compilation_Unit (D)) = A_Separate_Body
            then
               return True;
            end if;
         else
            if Declaration_Kind (D) in A_Body_Stub then
               return True;
            end if;
         end if;
      end;
      return False;
   end Is_Separate;

   function Is_Unit
     (Element : in Asis.Element)
     return Boolean
   is
   begin
      if Is_Nil (Element) then return False; end if;
      return
        Is_Equal (Decl (Element),
                  Unit_Declaration (Enclosing_Compilation_Unit (Element)));
   end Is_Unit;

   function Is_Child
     (Element : in Asis.Element)
     return Boolean
   is
   begin
      if Is_Unit (Element) then
         declare
            use Asis.Compilation_Units;
            Parent : constant Asis.Compilation_Unit :=
              Corresponding_Parent_Declaration
                (Enclosing_Compilation_Unit (Element));
         begin
            if Is_Nil (Parent) then
               --  Standard
               return False;
            end if;
            --  If the grandparent is nil, the parent is Standard, and we *do*
            --  have a root unit.
            return not Is_Nil (Corresponding_Parent_Declaration (Parent));
         end;
      end if;
      return False;
   end Is_Child;

   ----------------------------------------------------------------------------
   --  Items

   function Is_Constant
     (Element : in Asis.Element)
     return Boolean
   is
   begin
      if Is_Nil (Element) then return False; end if;
      declare
         D : constant Asis.Declaration := Decl (Element);
      begin
         case Declaration_Kind (D) is
            when A_Constant_Declaration |
                 A_Deferred_Constant_Declaration |
                 A_Real_Number_Declaration |
                 An_Integer_Number_Declaration =>
               return True;
            when A_Formal_Object_Declaration =>
               return Mode_Kind (D) <= An_In_Mode;
            when others =>
               null;
         end case;
      end;
      return False;
   end Is_Constant;

   function Is_Variable
     (Element : in Asis.Element)
     return Boolean
   is
   begin
      if Is_Nil (Element) then return False; end if;
      declare
         D : constant Asis.Declaration := Decl (Element);
      begin
         case Declaration_Kind (D) is
            when A_Variable_Declaration |
                 A_Single_Task_Declaration |
                 A_Single_Protected_Declaration =>
               return True;
            when A_Formal_Object_Declaration =>
               return Mode_Kind (D) >= An_Out_Mode;
            when others =>
               null;
         end case;
      end;
      return False;
   end Is_Variable;

   function Is_Package
     (Element : in Asis.Element)
     return Boolean
   is
   begin
      if Is_Nil (Element) then return False; end if;
      declare
         D : constant Asis.Declaration := Decl (Element);
      begin
         case Declaration_Kind (D) is
            when A_Package_Declaration |
                 A_Generic_Package_Declaration |
                 A_Generic_Package_Renaming_Declaration |
                 A_Package_Instantiation |
                 A_Package_Renaming_Declaration |
                 A_Formal_Package_Declaration |
                 A_Formal_Package_Declaration_With_Box =>
               return True;
            when others =>
               return False;
         end case;
      end;
   end Is_Package;

   function Is_Type
     (Element : in Asis.Element)
     return Boolean
   is
   begin
      if Is_Nil (Element) then return False; end if;
      declare
         D : constant Asis.Declaration := Decl (Element);
      begin
         case Declaration_Kind (D) is
            when A_Formal_Type_Declaration =>
               return True;
            when others =>
               return Declaration_Kind (D) in A_Type_Declaration;
         end case;
      end;
   end Is_Type;

   function Is_Subtype
     (Element : in Asis.Element)
     return Boolean
   is
   begin
      return Declaration_Kind (Decl (Element)) = A_Subtype_Declaration;
   end Is_Subtype;

   --  Returns @True@ if @Element@ is a subtype declaration.

   function Is_Procedure
     (Element : in Asis.Element)
     return Boolean
   is
   begin
      case Declaration_Kind (Decl (Element)) is
         when A_Procedure_Declaration |
              A_Generic_Procedure_Declaration |
              A_Procedure_Renaming_Declaration |
              A_Generic_Procedure_Renaming_Declaration |
              A_Procedure_Body_Declaration |
              A_Procedure_Body_Stub |
              A_Procedure_Instantiation =>
            return True;
         when A_Formal_Procedure_Declaration =>
            return True;
         when others =>
            return False;
      end case;
   end Is_Procedure;

   function Is_Function
     (Element : in Asis.Element)
     return Boolean
   is
   begin
      case Declaration_Kind (Decl (Element)) is
         when A_Function_Declaration |
              A_Generic_Function_Declaration |
              A_Function_Renaming_Declaration |
              A_Generic_Function_Renaming_Declaration |
              A_Function_Body_Declaration |
              A_Function_Body_Stub |
              A_Function_Instantiation =>
            return True;
         when A_Formal_Function_Declaration =>
            return True;
         when others =>
            return False;
      end case;
   end Is_Function;

   function Is_Subprogram
     (Element : in Asis.Element)
     return Boolean
   is
   begin
      return Is_Procedure (Element) or else Is_Function (Element);
   end Is_Subprogram;

   function Is_Entry
     (Element : in Asis.Element)
     return Boolean
   is
   begin
      case Declaration_Kind (Decl (Element)) is
         when An_Entry_Declaration |
              An_Entry_Body_Declaration =>
            return True;
         when others =>
            return False;
      end case;
   end Is_Entry;

   ----------------------------------------------------------------------------
   --  Types, Variables, and Constants. See RM 3.2

   function Is_Elementary
     (Element : in Asis.Element)
     return Boolean
   is
      D : constant Definition := Def (Root (Decl (Element)));
   begin
      case Definition_Kind (D) is
         when Not_A_Definition =>
            return False;
         when A_Task_Definition |
              A_Protected_Definition |
              A_Tagged_Private_Type_Definition |
              A_Private_Extension_Definition =>
            return False;
         when A_Private_Type_Definition =>
            return False;
         when A_Type_Definition =>
            case Type_Kind (D) is
               when An_Unconstrained_Array_Definition |
                    A_Constrained_Array_Definition |
                    A_Record_Type_Definition =>
                  return False;
               when others =>
                  null;
            end case;
         when A_Formal_Type_Definition =>
            case Formal_Type_Kind (D) is
               when A_Formal_Discrete_Type_Definition ..
                    A_Formal_Decimal_Fixed_Point_Definition =>
                  return True;
               when others =>
                  return False;
            end case;
         when others =>
            null;
      end case;
      return True;
   end Is_Elementary;

   function Is_Scalar
     (Element : in Asis.Element)
     return Boolean
   is
      subtype Scalar_Type is
        Type_Kinds range An_Enumeration_Type_Definition ..
                         A_Decimal_Fixed_Point_Definition;

      D : constant Definition := Def (Root (Decl (Element)));

   begin
      case Definition_Kind (D) is
         when A_Formal_Type_Definition =>
            case Formal_Type_Kind (D) is
               when A_Formal_Discrete_Type_Definition ..
                    A_Formal_Decimal_Fixed_Point_Definition =>
                  return True;
               when others =>
                  return False;
            end case;
         when others =>
            return Type_Kind (D) in Scalar_Type;
      end case;
   end Is_Scalar;

   function Is_Discrete
     (Element : in Asis.Element)
     return Boolean
   is
      D : constant Definition := Def (Root (Decl (Element)));
   begin
      case Definition_Kind (D) is
         when A_Formal_Type_Definition =>
            case Formal_Type_Kind (D) is
               when A_Formal_Discrete_Type_Definition ..
                    A_Formal_Modular_Type_Definition =>
                  return True;
               when others =>
                  return False;
            end case;
         when others =>
            case Type_Kind (D) is
               when An_Enumeration_Type_Definition |
                    A_Signed_Integer_Type_Definition |
                    A_Modular_Type_Definition =>
                  return True;
               when A_Root_Type_Definition =>
                  case Root_Type_Kind (D) is
                     when A_Root_Integer_Definition |
                          A_Universal_Integer_Definition =>
                        return True;
                     when others =>
                        null;
                  end case;
               when others =>
                  null;
            end case;
            return False;
      end case;
   end Is_Discrete;

   function Is_Enumeration
     (Element : in Asis.Element)
     return Boolean
   is
      D : constant Definition := Def (Root (Decl (Element)));
   begin
      return Type_Kind (D) = An_Enumeration_Type_Definition;
   end Is_Enumeration;

   --  Returns @True@ if @Element@ is either the declaration of an enumeration
   --  type or a variable declaration whose type is an enumeration type.
   --  Includes subtypes and derived types, also includes character and boolean
   --  types.

   function Is_Integral
     (Element : in Asis.Element)
     return Boolean
   is
      D : constant Definition := Def (Root (Decl (Element)));
   begin
      case Definition_Kind (D) is
         when A_Formal_Type_Definition =>
            case Formal_Type_Kind (D) is
               when A_Formal_Signed_Integer_Type_Definition |
                    A_Formal_Modular_Type_Definition =>
                  return True;
               when others =>
                  null;
            end case;
         when others =>
            case Type_Kind (D) is
               when A_Signed_Integer_Type_Definition |
                    A_Modular_Type_Definition =>
                  return True;
               when A_Root_Type_Definition =>
                  case Root_Type_Kind (D) is
                     when A_Root_Integer_Definition |
                          A_Universal_Integer_Definition =>
                        return True;
                     when others =>
                        null;
                  end case;
               when others =>
                  null;
            end case;
      end case;
      return False;
   end Is_Integral;

   function Is_Signed
     (Element : in Asis.Element)
     return Boolean
   is
      D : constant Definition := Def (Root (Decl (Element)));
   begin
      case Definition_Kind (D) is
         when A_Formal_Type_Definition =>
            return Formal_Type_Kind (D) =
                   A_Formal_Signed_Integer_Type_Definition;
         when others =>
            case Type_Kind (D) is
               when A_Signed_Integer_Type_Definition =>
                  return True;
               when A_Root_Type_Definition =>
                  case Root_Type_Kind (D) is
                     when A_Root_Integer_Definition |
                          A_Universal_Integer_Definition =>
                        return True;
                     when others =>
                        null;
                  end case;
               when others =>
                  null;
            end case;
      end case;
      return False;
   end Is_Signed;

   function Is_Modular
     (Element : in Asis.Element)
     return Boolean
   is
      D : constant Definition := Def (Root (Decl (Element)));
   begin
      return Type_Kind (D) = A_Modular_Type_Definition or else
             Formal_Type_Kind (D) = A_Formal_Modular_Type_Definition;
   end Is_Modular;

   function Is_Real
     (Element : in Asis.Element)
     return Boolean
   is
      subtype Formal_Reals is Formal_Type_Kinds range
        A_Formal_Floating_Point_Definition ..
        A_Formal_Decimal_Fixed_Point_Definition;

      D : constant Definition := Def (Root (Decl (Element)));
   begin
      case Definition_Kind (D) is
         when A_Formal_Type_Definition =>
            return Formal_Type_Kind (D) in Formal_Reals;
         when others =>
            case Type_Kind (D) is
               when A_Floating_Point_Definition |
                    An_Ordinary_Fixed_Point_Definition |
                    A_Decimal_Fixed_Point_Definition =>
                  return True;
               when A_Root_Type_Definition =>
                  case Root_Type_Kind (D) is
                     when A_Root_Real_Definition |
                          A_Universal_Real_Definition  |
                          A_Universal_Fixed_Definition =>
                        return True;
                     when others =>
                        null;
                  end case;
               when others =>
                  null;
            end case;
      end case;
      return False;
   end Is_Real;

   function Is_Float
     (Element : in Asis.Element)
     return Boolean
   is
      D : constant Definition := Def (Root (Decl (Element)));
   begin
      case Definition_Kind (D) is
         when A_Formal_Type_Definition =>
            return Formal_Type_Kind (D) =
                   A_Formal_Floating_Point_Definition;
         when others =>
            case Type_Kind (D) is
               when A_Floating_Point_Definition =>
                  return True;
               when A_Root_Type_Definition =>
                  case Root_Type_Kind (D) is
                     when A_Root_Real_Definition |
                          A_Universal_Real_Definition =>
                        return True;
                     when others =>
                        null;
                  end case;
               when others =>
                  null;
            end case;
      end case;
      return False;
   end Is_Float;

   function Is_Fixed
     (Element : in Asis.Element)
     return Boolean
   is
      D : constant Definition := Def (Root (Decl (Element)));
   begin
      case Definition_Kind (D) is
         when A_Formal_Type_Definition =>
            case Formal_Type_Kind (D) is
               when A_Formal_Ordinary_Fixed_Point_Definition ..
                    A_Formal_Decimal_Fixed_Point_Definition =>
                  return True;
               when others =>
                  null;
            end case;
         when others =>
            case Type_Kind (D) is
               when An_Ordinary_Fixed_Point_Definition |
                    A_Decimal_Fixed_Point_Definition =>
                  return True;
               when A_Root_Type_Definition =>
                  return Root_Type_Kind (D) = A_Universal_Fixed_Definition;
               when others =>
                  null;
            end case;
      end case;
      return False;
   end Is_Fixed;

   function Is_Ordinary_Fixed
     (Element : in Asis.Element)
     return Boolean
   is
      D : constant Definition := Def (Root (Decl (Element)));
   begin
      return Type_Kind (D) = An_Ordinary_Fixed_Point_Definition or else
             Formal_Type_Kind (D) =
             A_Formal_Ordinary_Fixed_Point_Definition;
   end Is_Ordinary_Fixed;

   function Is_Decimal_Fixed
     (Element : in Asis.Element)
     return Boolean
   is
      D : constant Definition := Def (Root (Decl (Element)));
   begin
      return Type_Kind (D) = A_Decimal_Fixed_Point_Definition or else
             Formal_Type_Kind (D) =
             A_Formal_Decimal_Fixed_Point_Definition;
   end Is_Decimal_Fixed;

   function Is_Numeric
     (Element : in Asis.Element)
     return Boolean
   is
   begin
      return Is_Real (Element) or else Is_Integral (Element);
   end Is_Numeric;

   function Is_Access
     (Element : in Asis.Element)
     return Boolean
   is
      D : constant Definition := Def (Root (Decl (Element)));
   begin
      return Type_Kind (D) = An_Access_Type_Definition;
   end Is_Access;

   function Is_Access_To_Object
     (Element : in Asis.Element)
     return Boolean
   is
      D : constant Definition := Def (Root (Decl (Element)));
   begin
      return Access_Type_Kind (D) in Asis.Access_To_Object_Definition;
   end Is_Access_To_Object;

   function Is_Access_To_Subprogram
     (Element : in Asis.Element)
     return Boolean
   is
      D : constant Definition := Def (Root (Decl (Element)));
   begin
      return Access_Type_Kind (D) in Asis.Access_To_Subprogram_Definition;
   end Is_Access_To_Subprogram;

   function Is_Composite
     (Element : in Asis.Element)
     return Boolean
   is
      D : constant Definition := Def (Root (Decl (Element)));
   begin
      case Definition_Kind (D) is
         when Not_A_Definition =>
            --  Because of this case, we cannot just return
            --  not Is_Elementary.
            return False;
         when A_Task_Definition |
              A_Protected_Definition |
              A_Tagged_Private_Type_Definition |
              A_Private_Extension_Definition =>
            return True;
         when A_Private_Type_Definition =>
            --  This is also a case where composite /= not
            --  elementary.
            return False;
         when A_Type_Definition =>
            case Type_Kind (D) is
               when An_Unconstrained_Array_Definition |
                    A_Constrained_Array_Definition |
                    A_Record_Type_Definition =>
                  return True;
               when others =>
                  null;
            end case;
         when others =>
            null;
      end case;
      return False;
   end Is_Composite;

   function Is_Array
     (Element : in Asis.Element)
     return Boolean
   is
      D : constant Definition := Def (Root (Decl (Element)));
   begin
      case Type_Kind (D) is
         when An_Unconstrained_Array_Definition |
              A_Constrained_Array_Definition =>
            return True;
         when others =>
            return False;
      end case;
   end Is_Array;

   function Is_Record
     (Element : in Asis.Element)
     return Boolean
   is
      D : constant Definition := Def (Root (Decl (Element)));
   begin
      case Definition_Kind (D) is
         when A_Tagged_Private_Type_Definition |
              A_Private_Extension_Definition =>
            return True;
         when A_Type_Definition =>
            case Type_Kind (D) is
               when A_Record_Type_Definition |
                    A_Derived_Record_Extension_Definition |
                    A_Tagged_Record_Type_Definition =>
                  return True;
               when others =>
                  null;
            end case;
         when others =>
            null;
      end case;
      return False;
   end Is_Record;

   function Is_Tagged
     (Element : in Asis.Element)
     return Boolean
   is
      D : constant Definition := Def (Root (Decl (Element)));
   begin
      case Definition_Kind (D) is
         when A_Tagged_Private_Type_Definition |
              A_Private_Extension_Definition =>
            return True;
         when A_Type_Definition =>
            case Type_Kind (D) is
               when A_Derived_Record_Extension_Definition |
                    A_Tagged_Record_Type_Definition =>
                  return True;
               when others =>
                  null;
            end case;
         when others =>
            null;
      end case;
      return False;
   end Is_Tagged;

   function Is_Task
     (Element : in Asis.Element)
     return Boolean
   is
   begin
      declare
         D : constant Asis.Declaration := Root (Decl (Element));
      begin
         case Declaration_Kind (D) is
            when A_Task_Type_Declaration |
                 A_Single_Task_Declaration =>
               return True;
            when others =>
               return Definition_Kind (Def (D)) = A_Task_Definition;
         end case;
      end;
   end Is_Task;

   function Is_Protected
     (Element : in Asis.Element)
     return Boolean
   is
      Elem_Decl : Asis.Declaration := Decl (Element);
   begin
      if Is_Subprogram (Elem_Decl) or else Is_Entry (Elem_Decl) then
         --  It is protected if it is a protected subprogram, i.e. the
         --  enclosing declaration (if any) is a protected type or object.
         Elem_Decl := Asis2.Declarations.Enclosing_Declaration (Elem_Decl);
      end if;
      declare
         D : constant Asis.Declaration := Root (Elem_Decl);
      begin
         case Declaration_Kind (D) is
            when A_Protected_Type_Declaration |
                 A_Single_Protected_Declaration =>
               return True;
            when others =>
               return Definition_Kind (Def (D)) = A_Protected_Definition;
         end case;
      end;
   end Is_Protected;

   function Is_Limited
     (Element : in Asis.Element)
     return Boolean
   is
   begin
      if Is_Task (Element) or else Is_Protected (Element) then
         return True;
      end if;
      declare
         D : constant Asis.Declaration := Root (Decl (Element));
      begin
         case Trait_Kind (D) is
            when A_Limited_Trait |
                 A_Limited_Private_Trait =>
               return True;
            when others =>
               null;
         end case;
      end;
      return False;
   end Is_Limited;

   function Is_Class_Wide
     (Element : in Asis.Element)
     return Boolean
   is

      function Class_Wide_Subtype
        (D : in Definition)
        return Boolean
      is
         Expr : Expression := Asis.Definitions.Subtype_Mark (D);
      begin
         while Expression_Kind (Expr) = An_Attribute_Reference loop
            if Attribute_Kind (Expr) = A_Class_Attribute then
               return True;
            else
               Expr := Prefix (Expr);
            end if;
         end loop;
         return Is_Class_Wide (Expr);
      end Class_Wide_Subtype;

      D : constant Asis.Declaration := Decl (Element);

   begin
      case Declaration_Kind (D) is
         when A_Subtype_Declaration =>
            return Class_Wide_Subtype (Type_Declaration_View (D));
         when A_Variable_Declaration |
              A_Constant_Declaration |
              A_Deferred_Constant_Declaration =>
            declare
               Def : constant Definition := Object_Declaration_View (D);
            begin
               if Definition_Kind (Def) = A_Subtype_Indication then
                  return Class_Wide_Subtype (Def);
               end if;
            end;
         when A_Formal_Object_Declaration =>
            return Attribute_Kind (Declaration_Subtype_Mark (D)) =
                   A_Class_Attribute;
         when others =>
            null;
      end case;
      return False;
   end Is_Class_Wide;

   function Is_Controlled
     (Element : in Asis.Element)
     return Boolean
   is
      D : Asis.Declaration := Root (Decl (Element));
   begin
      --  It's controlled if the enclosing package of the root type's
      --  declaration is Ada.Finalization, or if it is a private type
      --  who's full view is controlled.
      if Declaration_Kind (D) = A_Private_Type_Declaration then
         D := (Corresponding_Type_Declaration (D));
      end if;
      declare
         Name : constant Wide_String :=
           Asis2.Text.To_Lower
             (Asis2.Naming.Full_Unit_Name (Enclosing_Compilation_Unit (D)));
      begin
         return Name = "ada.finalization";
      end;
   end Is_Controlled;

   function Is_Private_Type
     (Element : in Asis.Element)
     return Boolean
   is
      D : constant Definition := Base (Decl (Element));
   begin
      case Declaration_Kind (D) is
         when A_Private_Type_Declaration |
              A_Private_Extension_Declaration =>
            return True;
         when others =>
            null;
      end case;
      return False;
   end Is_Private_Type;

   function Is_Incomplete
     (Element : in Asis.Element)
     return Boolean
   is
      D : constant Declaration := Decl (Element);
   begin
      case Declaration_Kind (D) is
         when An_Incomplete_Type_Declaration |
              A_Deferred_Constant_Declaration =>
            return True;
         when others =>
            return False;
      end case;
   end Is_Incomplete;

   function Is_Aliased
     (Element : in Asis.Element)
     return Boolean
   is
   begin
      return Trait_Kind (Decl (Element)) = An_Aliased_Trait;
   end Is_Aliased;

   ----------------------------------------------------------------------------
   --  Generics, renamings, and other stuff.

   function Is_Exception
     (Element : in Asis.Element)
     return Boolean
   is
      D : constant Declaration := Decl (Element);
   begin
      case Declaration_Kind (D) is
         when An_Exception_Declaration |
              An_Exception_Renaming_Declaration =>
            return True;
         when others =>
            return False;
      end case;
   end Is_Exception;

   function Is_Renaming
     (Element : in Asis.Element)
     return Boolean
   is
      D : constant Declaration := Decl (Element);
   begin
      return Declaration_Kind (D) in Asis.A_Renaming_Declaration;
   end Is_Renaming;

   function Is_Generic
     (Element : in Asis.Element)
     return Boolean
   is
      D : constant Declaration := Decl (Element);
   begin
      return Declaration_Kind (D) in Asis.A_Generic_Declaration;
   end Is_Generic;

   function Is_Generic_Formal
     (Element : in Asis.Element)
     return Boolean
   is
      D : constant Declaration := Decl (Element);
   begin
      return Declaration_Kind (D) in Asis.A_Formal_Declaration;
   end Is_Generic_Formal;

   function Is_Instance
     (Element : in Asis.Element)
     return Boolean
   is
      D : constant Declaration := Decl (Element);
   begin
      return Declaration_Kind (D) in Asis.A_Generic_Instantiation;
   end Is_Instance;

   function Is_Abstract
     (Element : in Asis.Element)
     return Boolean
   is
   begin
      return Trait_Kind (Decl (Element)) >= An_Abstract_Trait;
   end Is_Abstract;

   function Is_Pragma
     (Element : in Asis.Element)
     return Boolean
   is
   begin
      return Element_Kind (Element) = A_Pragma;
   end Is_Pragma;

   function Is_Clause
     (Element : in Asis.Element)
     return Boolean
   is
   begin
      return Clause_Kind (Element) = A_Representation_Clause;
   end Is_Clause;

end Asis2.Predicates;
