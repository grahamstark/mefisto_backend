------------------------------------------------------------------------------
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
--    05-JUN-2003   TW  Last release as part of @AdaBrowse@.
--    18-JUL-2003   TW  Created from operations in @AD.Queries@.
--  </HISTORY>
-------------------------------------------------------------------------------

pragma License (Modified_GPL);

with Asis.Declarations;
with Asis.Definitions;
with Asis.Elements;
with Asis.Exceptions;
with Asis.Expressions;

package body Asis2.Declarations is

   use Asis;
   use Asis.Declarations;
   use Asis.Definitions;
   use Asis.Elements;
   use Asis.Expressions;

   ----------------------------------------------------------------------------

   function References
     (Expr : in Asis.Expression;
      Decl : in Asis.Declaration)
     return Boolean
   is
   begin
      case Expression_Kind (Expr) is
         when An_Identifier =>
            return
              Is_Equal (Corresponding_Name_Declaration (Expr), Decl);
         when A_Selected_Component =>
            return
              Is_Equal (Corresponding_Name_Declaration  (Selector (Expr)),
                        Decl);
         when others =>
            return False;
      end case;
   end References;

   ----------------------------------------------------------------------------

   function Name_Definition
     (Expr : in Asis.Expression)
     return Asis.Defining_Name
   is

      function Get_Def
        (Expr : in Asis.Expression)
        return Asis.Defining_Name
      is
      begin
         if Expression_Kind (Expr) = An_Operator_Symbol then
            declare
               Outer : Asis.Element := Enclosing_Element (Expr);
            begin
               while Expression_Kind (Outer) = A_Parenthesized_Expression loop
                  Outer := Enclosing_Element (Outer);
               end loop;
               if Element_Kind (Outer) = An_Association and then
                  Association_Kind (Outer) = A_Pragma_Argument_Association
               then
                  --  ASIS-for-GNAT 3.14p has a severe bug in function
                  --  'Corresponding_Name_Definition': it doesn't return at
                  --  all if a name refers to several definitions, and some of
                  --  those are predefined entities. Hence this work-around:
                  --  if we have an operator symbol in a pragma, we always
                  --  return a Nil_Element, for it may reference predefined
                  --  operators.
                  --
                  --  'Corresponding_Name_Definition_List' has the same bug.
                  return Nil_Element;
               end if;
            end;
         end if;
         declare
            Result : constant Defining_Name :=
              Corresponding_Name_Definition (Expr);
         begin
            --  ASIS 2.0.R for GNAT 3.14p sometimes returns a defining expanded
            --  name here. We don't want that: we clearly ask for the defining
            --  identifier of the selector, and that should obviously be the
            --  defining selector!
            if Defining_Name_Kind (Result) = A_Defining_Expanded_Name then
               return Defining_Selector (Result);
            else
               return Result;
            end if;
         end;
         --  Asis 2.0.R for GNAT 3.13p occasionally returns the wrong
         --  definition here. An example is Test.Gen.Err, where it
         --  returns the name definition from the declaration of the
         --  generic child package "Test.Gen" instead of from the
         --  declaration of package "Test" for the identifier "Test" at
         --  the beginning of the defining expanded name "Test.Gen.Err".
         --
         --  I have no work-around for this!
         --  This error is *not* corrected in the 3.14p version.
         --
         --  Note that we have to handle this failure explicitly at the
         --  call site!
      exception
         when Asis.Exceptions.ASIS_Inappropriate_Element |
              Asis.Exceptions.ASIS_Failed =>
            --  Asis sometimes crashes here... observed for generic children
            --  of generic packages that have non-generic parent packages.
            --  An example is GAL.ADT.Lists.Iterators: in the package
            --  defining name, Asis 2.0.R for GNAT 3.13p crashes on "GAL"
            --  and "ADT" (internal error: no entity set), which are the
            --  non-generic parents of the generic package "Lists".
            --
            --  This error seems to be corrected in the 3.14p version.
            --
            --  Note that we may also legally get an exception here, e.g. if
            --  the expression denotes the subprogram name in a dispatching
            --  call.
            return Nil_Element;
      end Get_Def;

   begin
      if Expression_Kind (Expr) = A_Selected_Component then
         return Get_Def (Selector (Expr));
      else
         return Get_Def (Expr);
      end if;
   end Name_Definition;

   ----------------------------------------------------------------------------

   function Name_Declaration
     (Expr : in Asis.Expression)
     return Asis.Declaration
   is
      Result : Asis.Defining_Name := Name_Definition (Expr);
   begin
      if not Is_Nil (Result) then
         Result := Enclosing_Declaration (Result);
      end if;
      return Result;
   end Name_Declaration;

   ----------------------------------------------------------------------------

   function Enclosing_Declaration
     (Element : in Asis.Element)
     return Asis.Declaration
   is
      Result : Asis.Element := Enclosing_Element (Element);
   begin
      while not Is_Nil (Result) and then
            Declaration_Kind (Result) = Not_A_Declaration
      loop
         Result := Enclosing_Element (Result);
      end loop;
      return Result;
   end Enclosing_Declaration;

   ----------------------------------------------------------------------------

   function Real_Declaration
     (Decl : in Asis.Declaration)
     return Asis.Declaration
   is
   begin
      if Declaration_Kind (Decl) = An_Enumeration_Literal_Specification then
         if Declaration_Origin (Decl) /= An_Implicit_Inherited_Declaration then
            return Decl;
         end if;
         --  Ok, we have an implicitly inherited enumeration literal: go find
         --  its real declaration.
         declare
            True_Decl : Asis.Element := Decl;
         begin
            --  Use a loop to find the type declaration: I'm not sure whether
            --  the list (of enumeration literal specifications) is represented
            --  somewhere explicitly. With a loop, we'll just skip that if it
            --  is there somewhere.
            while not Is_Nil (True_Decl) loop
               case Declaration_Kind (True_Decl) is
                  when An_Ordinary_Type_Declaration =>
                     exit when
                       Type_Kind (Type_Declaration_View (True_Decl)) =
                       A_Derived_Type_Definition;
                  when others =>
                     null;
               end case;
               True_Decl := Enclosing_Element (True_Decl);
            end loop;
            if Is_Nil (True_Decl) then return Nil_Element; end if;
            --  Now 'True_Decl' is the (derived) type definition that inherits
            --  the enumeration literal. Go unwind type derivations now:
            True_Decl :=
              Corresponding_Root_Type (Type_Declaration_View (True_Decl));
            if not Is_Nil (True_Decl) and then
               Type_Kind (Type_Declaration_View (True_Decl)) =
               An_Enumeration_Type_Definition
            then
               --  Now 'True_Decl' is the enumeration type declaration from
               --  which the literal was inherited in the first place. Get
               --  the list of literals, and try to find the one that
               --  corresponds to 'Decl'.
               declare
                  Literals : constant Asis.Declaration_List :=
                    Enumeration_Literal_Declarations
                      (Type_Declaration_View (True_Decl));
                  Image    : constant Wide_String :=
                    Defining_Name_Image (Names (Decl) (1));
               begin
                  for I in Literals'Range loop
                     if Defining_Name_Image (Names (Literals (I)) (1)) =
                        Image
                     then
                        return Literals (I);
                     end if;
                  end loop;
               end;
            end if;
            --  Nothing found.
            return Nil_Element;
         end;
      else
         --  Not an enumeration literal specification.
         return Corresponding_Declaration (Decl);
      end if;
   end Real_Declaration;

end Asis2.Declarations;
