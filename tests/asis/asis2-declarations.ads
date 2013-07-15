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

with Asis;

package Asis2.Declarations is

   function References
     (Expr : in Asis.Expression;
      Decl : in Asis.Declaration)
     return Boolean;
   --  Returns True if 'Expr' is @An_Identifier@ or @A_Selected_Component@, and
   --  that name refers to 'Decl'.

   function Name_Definition
     (Expr : in Asis.Expression)
     return Asis.Defining_Name;
   --  Appropriate expression kinds:
   --
   --    All those for @Asis.Expressions.Corresponding_Name_Definition@ plus
   --    @A_Selected_Component@, for which it returns the name definition of
   --    the selector.
   --
   --  Returns @Nil_Element@ if @Expr@ refers to more than one definition.

   function Name_Declaration
     (Expr : in Asis.Expression)
     return Asis.Declaration;
   --  Synonym to <CODE>Enclosing_Declaration (Name_Definition (Expr));</CODE>.

   function Enclosing_Declaration
     (Element : in Asis.Element)
     return Asis.Declaration;
   --  Returns the declaration that contains @Element@.

   function Real_Declaration
     (Decl : in Asis.Declaration)
     return Asis.Declaration;
   --  Appropriate declaration kinds:
   --
   --    All those for @Asis.Declarations.Corresponding_Declaration@ plus
   --    @An_Enumeration_Literal_Specification@, for which, if the enumeration
   --    literal is implicitly inherited, it returns the explicit declaration
   --    from which the enumeration literal was inherited, if such an explicit
   --    declaration exists. If none exists, returns @Nil_Element@.
   --
   --  For all other declaration kinds, identical to function
   --  @Asis.Declarations.Corresponding_Declaration@.

end Asis2.Declarations;
