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

with Asis;

package Asis2.Predicates is

   pragma Elaborate_Body;

   ----------------------------------------------------------------------------
   --  Units

   function Is_Private
     (Element : in Asis.Element)
     return Boolean;
   --  Returns @True@ if @Element@ is a declaration occurring in the private
   --  part of a (generic) package declaration, task or protected type
   --  declaration, or a single task or protected object declaration. Also
   --  returns @True@ if <CODE>Is_Unit (Element)</CODE> and it is a private
   --  library unit.

   function Is_Separate
     (Element : in Asis.Element)
     return Boolean;
   --  Returns @True@ for separate body declarations and stubs.

   function Is_Unit
     (Element : in Asis.Element)
     return Boolean;
   --  Returns @True@ if @Element@ is the declaration of a compilation unit
   --  (spec or body).

   function Is_Child
     (Element : in Asis.Element)
     return Boolean;
   --  Returns @True@ if <CODE>Is_Unit (Element)</CODE> and it is not a
   --  root library unit.

   ----------------------------------------------------------------------------
   --  Items

   function Is_Package
     (Element : in Asis.Element)
     return Boolean;
   --  Returns @True@ for all kinds of package declarations.

   function Is_Constant
     (Element : in Asis.Element)
     return Boolean;
   --  Returns @True@ for constant and named number declarations.

   function Is_Variable
     (Element : in Asis.Element)
     return Boolean;
   --  Returns @True@ for variable declarations and single task or protected
   --  object declarations.

   function Is_Type
     (Element : in Asis.Element)
     return Boolean;
   --  Returns @True@ if @Element@ is a type declaration.

   function Is_Subtype
     (Element : in Asis.Element)
     return Boolean;
   --  Returns @True@ if @Element@ is a subtype declaration.

   function Is_Procedure
     (Element : in Asis.Element)
     return Boolean;

   function Is_Function
     (Element : in Asis.Element)
     return Boolean;

   function Is_Subprogram
     (Element : in Asis.Element)
     return Boolean;
   --  <CODE>Is_Procedure (Element) <STRONG>or</STRONG>
   --  Is_Function (Element)</CODE>.

   function Is_Entry
     (Element : in Asis.Element)
     return Boolean;

   function Is_Pragma
     (Element : in Asis.Element)
     return Boolean;

   function Is_Clause
     (Element : in Asis.Element)
     return Boolean;

   ----------------------------------------------------------------------------
   --  Types, Variables, and Constants. See RM 3.2

   function Is_Elementary
     (Element : in Asis.Element)
     return Boolean;

   function Is_Scalar
     (Element : in Asis.Element)
     return Boolean;

   function Is_Discrete
     (Element : in Asis.Element)
     return Boolean;

   function Is_Enumeration
     (Element : in Asis.Element)
     return Boolean;
   --  Returns @True@ if @Element@ is either the declaration of an enumeration
   --  type or a variable declaration whose type is an enumeration type.
   --  Includes subtypes and derived types, also includes character and boolean
   --  types.

   function Is_Integral
     (Element : in Asis.Element)
     return Boolean;
   --  Returns @True@ if @Element@ is either the declaration of an integer
   --  type or a variable declaration whose type is an integer type.
   --  Includes subtypes and derived types.

   function Is_Signed
     (Element : in Asis.Element)
     return Boolean;

   function Is_Modular
     (Element : in Asis.Element)
     return Boolean;
   --  Returns @True@ if @Element@ is either the declaration of a modular
   --  type or a variable declaration whose type is a modular type.
   --  Includes subtypes and derived types.

   function Is_Real
     (Element : in Asis.Element)
     return Boolean;

   function Is_Float
     (Element : in Asis.Element)
     return Boolean;
   --  Returns @True@ if @Element@ is either the declaration of a modular
   --  type or a variable declaration whose type is a modular type.
   --  Includes subtypes and derived types.

   function Is_Fixed
     (Element : in Asis.Element)
     return Boolean;
   --  Returns @True@ if @Element@ is either the declaration of a modular
   --  type or a variable declaration whose type is a modular type.
   --  Includes subtypes and derived types.

   function Is_Ordinary_Fixed
     (Element : in Asis.Element)
     return Boolean;

   function Is_Decimal_Fixed
     (Element : in Asis.Element)
     return Boolean;

   function Is_Numeric
     (Element : in Asis.Element)
     return Boolean;

   function Is_Access
     (Element : in Asis.Element)
     return Boolean;
   --  <CODE>Is_Access_To_Object <STRONG>or</STRONG>
   --  Is_Access_To_Subprogram</CODE>.

   function Is_Access_To_Object
     (Element : in Asis.Element)
     return Boolean;
   --  Returns @True@ if @Element@ is either the declaration of an access
   --  type or a variable declaration whose type is an access type. Includes
   --  subtypes and derived types.

   function Is_Access_To_Subprogram
     (Element : in Asis.Element)
     return Boolean;
   --  Returns @True@ if @Element@ is either the declaration of an access
   --  type or a variable declaration whose type is an access type. Includes
   --  subtypes and derived types.

   function Is_Composite
     (Element : in Asis.Element)
     return Boolean;

   function Is_Array
     (Element : in Asis.Element)
     return Boolean;
   --  Returns @True@ if @Element@ is either the declaration of an array
   --  type or a variable declaration whose type is an array type. Includes
   --  subtypes and derived types.

   function Is_Record
     (Element : in Asis.Element)
     return Boolean;
   --  Returns @True@ if @Element@ is either the declaration of a record
   --  type or a variable declaration whose type is a record type. Includes
   --  subtypes and derived types.

   function Is_Tagged
     (Element : in Asis.Element)
     return Boolean;
   --  Returns @True@ if @Element@ is either the declaration of a tagged type
   --  or a variable declaration whose type is tagged.
   --
   --  This includes types derived from tagged types, but not subtypes of
   --  tagged types!

   function Is_Task
     (Element : in Asis.Element)
     return Boolean;
   --  Returns @True@ if @Element@ is either a task type declaration, a single
   --  task declaration, or a defining name in a variable declaration whose
   --  type is a task type. Includes subtypes and derived types.

   function Is_Protected
     (Element : in Asis.Element)
     return Boolean;
   --  Returns @True@ if @Element@ is either a protected type declaration, a
   --  protected object declaration, or a defining name in a variable
   --  declaration whose type is a protected type. Includes subtypes and
   --  derived types.

   function Is_Limited
     (Element : in Asis.Element)
     return Boolean;
   --  Returns @True@ if @Element@ is either the declaration of a limited type
   --  or a variable declaration whose type is limited. Includes subtypes and
   --  derived types.

   function Is_Class_Wide
     (Element : in Asis.Element)
     return Boolean;
   --  Returns @True@ if @Element@ is either the declaration of a class-wide
   --  type or a variable declaration whose type is class-wide. Includes
   --  subtypes.

   function Is_Controlled
     (Element : in Asis.Element)
     return Boolean;
   --  Definition: a <EM>controlled type</EM> is a type derived from one of
   --  the two types declared in @Ada.Finalization@.
   --
   --  Returns @True@ if @Element@ is either the declaration of a controlled
   --  type or a variable declaration whose type is controlled. Includes
   --  subtypes.

   function Is_Private_Type
     (Element : in Asis.Element)
     return Boolean;
   --  Returns @True@ if @Element@ is either the declaration of a private
   --  type or a variable declaration whose type is private.

   function Is_Incomplete
     (Element : in Asis.Element)
     return Boolean;
   --  Returns @True@ if @Element@ is the declaration of an incomplete type
   --  or a deferred constant.

   function Is_Aliased
     (Element : in Asis.Element)
     return Boolean;

   ----------------------------------------------------------------------------
   --  Generics, renamings, and other stuff.

   function Is_Exception
     (Element : in Asis.Element)
     return Boolean;

   function Is_Renaming
     (Element : in Asis.Element)
     return Boolean;

   function Is_Generic
     (Element : in Asis.Element)
     return Boolean;
   --  Returns @True@ for generic subprogram and package declarations, their
   --  bodies, and generic formal types and objects.

   function Is_Generic_Formal
     (Element : in Asis.Element)
     return Boolean;
   --  Returns @True@ for all generic formals.

   function Is_Instance
     (Element : in Asis.Element)
     return Boolean;
   --  Returns @True@ for all instantiations of generic subprograms or
   --  packages.

   function Is_Abstract
     (Element : in Asis.Element)
     return Boolean;
   --  Returns @True@ for all abstract types and subprograms.

   function Decl
     (Element : in Asis.Element)
     return Asis.Element;

   function Def
     (Decl : in Asis.Declaration)
     return Asis.Definition;
     
   function Type_Of
     (D : in Asis.Declaration)
     return Asis.Declaration;
     
   function Base
     (D : in Asis.Declaration)
     return Asis.Declaration;
     
   function Root
     (D : in Asis.Declaration)
     return Asis.Declaration;
   
end Asis2.Predicates;
