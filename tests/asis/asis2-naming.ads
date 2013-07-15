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

with Asis;

package Asis2.Naming is

   pragma Elaborate_Body;

   procedure Verify_Name_Definition
     (Def  : in out Asis.Defining_Name;
      Name : in     Asis.Expression);
   --  Needed to correct bugs in ASIS-for-GNAT, which sometimes gets confused
   --  and finds the wrong name definitions when given an expression. @Def@
   --  and @Name@ should be simple names. If their images are equal or differ
   --  only in casing, nothing is changed; otherwise, @Def@ is set to a
   --  @Nil_Element@.

   function Name_Expression_Image
     (Name : in Asis.Expression)
     return Wide_String;
   --  As @Name_Image@, but also accepts @A_Selected_Component@, and uses,
   --  whenever possible, the corresponding @Defining_Name_Image@ to construct
   --  the image instead of the program text of the expression. If @Name@ is
   --  @A_Selected_Component@, the resulting image will be a single line (i.e.
   --  no weird things between name components like comments, line breaks, and
   --  so on); individual name components will be separated by periods.
   --
   --  Also accepts @An_Attribute_Reference@, provided its ultimate prefix is
   --  one of the above.

   function Name_Definition_Image
     (Name : in Asis.Defining_Name)
     return Wide_String;
   --  As @Defining_Name_Image@, but also accepts @A_Defining_Expanded_Name@,
   --  for which it will return the normalized image (i.e., no line breaks,
   --  comments, and other weird things; all one line, components separated
   --  by periods).

   function Get_Name
     (Decl : in Asis.Declaration)
     return Asis.Defining_Name;
   --  Returns the first defining name from a declaration.

   function Get_Single_Name
     (Decl : in Asis.Declaration)
     return Wide_String;
   --  Returns the defining name image of <CODE>Get_Name (Decl)</CODE> if the
   --  declaration declares only one name, or the empty string otherwise.

   function Full_Unit_Name
     (Unit : in Asis.Compilation_Unit)
     return Wide_String;
   --  A replacement for <CODE>Asis.Compilation_Units.Full_Name</CODE>, which
   --  is buggy in ASIS-for-GNAT 3.14p: it returns only the last name component
   --  for children that are generic instantiations. (At least for subprograms,
   --  I didn't test it for package instances that are children of some other
   --  package.)
   --
   --  This function corrects that error. If the compilation unit's defining
   --  name is @A_Defining_Expanded_Name@ (i.e., it's a child unit), the
   --  prefixes are returned using (if possible) the capitalization used in
   --  the defining names of the parent units.

   function Container_Name
     (Element : in Asis.Element)
     return Wide_String;
   --  Returns the full name of the enclosing declaration of @Element@. All
   --  element kinds are appropriate. Returns an empty string for compilation
   --  unit declarations.

   function Fully_Qualified_Name
     (Name : in Asis.Defining_Name)
     return Wide_String;
   --  Returns the fully qualified name, i.e. the @Full_Unit_Name@ if @Name@
   --  is the defining name of a compilation unit, and the @Container_Name@
   --  followed by a dot and the @Name_Definition_Image@ of @Name@ otherwise.
   --
   --  All @Defining_Name_Kinds@ are appropriate.

end Asis2.Naming;
