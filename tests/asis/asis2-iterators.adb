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
--    Thomas Wolf  (TW) <E_MAIL>
--  </AUTHOR>
--
--  <PURPOSE>
--    An analogue to @Asis.Iterator.Traverse_Element@ that ensures that
--    @Post_Operation@s are called for "open" @Pre_Operation@s.
--  </PURPOSE>
--
--  <HISTORY>
--    11-JUL-2003   TW  Initial version.
--    14-JUL-2003   TW  Ensured that at level zero, we do return
--                      @Skip_Siblings@ when appropriate.
--                        Added @Traverse_Levels@, @Traverse_List@,
--                      @Traverse_Unit@, and @Traverse_Unit_Elements@.
--  </HISTORY>
-------------------------------------------------------------------------------

pragma License (Modified_GPL);

with Ada.Exceptions;

with Asis.Compilation_Units;
with Asis.Elements;
with Asis.Exceptions;
with Asis.Iterator;

package body Asis2.Iterators is

   use Asis;

   Package_Name : constant String := "Asis.Correct_Iterate";

   --  generic
   --     type State_Information (<>) is limited private;
   --
   --     with procedure Pre_Operation
   --            (Element : in     Asis.Element;
   --             Depth   : in     Asis.ASIS_Positive;
   --             Control : in out Control_Traversal;
   --             State   : in out State_Information) is <>;
   --
   --     with procedure Post_Operation
   --            (Element : in     Asis.Element;
   --             Depth   : in     Asis.ASIS_Positive;
   --             Control : in out Control_Traversal;
   --             State   : in out State_Information) is <>;
   --
   procedure Traverse_Levels
     (Element : in     Asis.Element;
      Control : in out Control_Traversal;
      State   : in out State_Information)
   is

      Level : Asis.ASIS_Natural := 0;
      --  We count the nesting level in order to not reset a Skip_Siblings
      --  or Terminate_Current_And_Siblings to Continue_Traversal on top level.
      --  On top-level, we want to pass out these two values, because they
      --  may be useful for deciding whether or not to continue traversing
      --  in a list of elements.
      --
      --  Note that although this iterator is based on the standard one (which
      --  doesn't invoke Post when Abandon_Children of Abandon_Siblings is set
      --  in Pre), we can still count correctly because we know where we are.

      procedure Pre
        (Element      : in     Asis.Element;
         Asis_Control : in out Asis.Traverse_Control;
         Real_Control : in out Control_Traversal);

      procedure Post
        (Element      : in     Asis.Element;
         Asis_Control : in out Asis.Traverse_Control;
         Real_Control : in out Control_Traversal);

      procedure Traverse is
         new Asis.Iterator.Traverse_Element
               (State_Information => Control_Traversal,
                Pre_Operation     => Pre,
                Post_Operation    => Post);

      procedure Pre
        (Element      : in     Asis.Element;
         Asis_Control : in out Asis.Traverse_Control;
         Real_Control : in out Control_Traversal)
      is
         use type Asis.Traverse_Control;
      begin
         Level := Level + 1;
         --  Real_Control = Continue_Traversal and Asis_Control = Continue
         Pre_Operation (Element, Level, Real_Control, State);
         case Real_Control is
            when Continue_Traversal =>
               --  Post will decrement the level.
               Asis_Control := Continue;
            when Terminate_Current =>
               --  Do *not* call Post!
               Asis_Control := Abandon_Children;
               Level := Level - 1;
               Real_Control := Continue_Traversal;
            when Terminate_Current_And_Siblings =>
               --  Do *not* call Post!
               Asis_Control := Abandon_Siblings;
               Level := Level - 1;
               if Level > 0 then
                  Real_Control := Continue_Traversal;
               else
                  Real_Control := Skip_Siblings;
               end if;
            when Skip_Children |
                 Skip_Siblings |
                 Unwind_Traversal |
                 Stop_Traversal =>
               --  Post will decrement the level.
               Post (Element, Asis_Control, Real_Control);
               --  If Post weakened the condition, make sure that we
               --  skip at least the children.
               if Asis_Control = Continue then
                  Asis_Control := Abandon_Children;
               end if;
            when Terminate_Traversal =>
               Asis_Control := Terminate_Immediately;
               --  We don't care about the level anymore.
         end case;
         --  Real_Control in
         --    (Continue_Traversal, Unwind_Traversal, Terminate_Traversal)
         --  or
         --  Level = 0 and Real_Control = Skip_Siblings
      end Pre;

      procedure Post
        (Element      : in     Asis.Element;
         Asis_Control : in out Asis.Traverse_Control;
         Real_Control : in out Control_Traversal)
      is
      begin
         --  Real_Control not in
         --    (Terminate_Current, Terminate_Current_And_Siblings,
         --     Terminate_Traversal)
         --  and
         --  Asis_Control = Continue
         Post_Operation (Element, Level, Real_Control, State);
         Level := Level - 1;
         case Real_Control is
            when Continue_Traversal =>
               Asis_Control := Continue;
            when Skip_Children | Terminate_Current =>
               Asis_Control := Abandon_Children;
               Real_Control := Continue_Traversal;
            when Skip_Siblings | Terminate_Current_And_Siblings =>
               Asis_Control := Abandon_Siblings;
               if Level > 0 then
                  Real_Control := Continue_Traversal;
               else
                  Real_Control := Skip_Siblings;
               end if;
            when Unwind_Traversal =>
               Asis_Control := Abandon_Siblings;
            when Stop_Traversal | Terminate_Traversal =>
               Asis_Control := Terminate_Immediately;
               Real_Control := Terminate_Traversal;
         end case;
         --  Real_Control in
         --    (Continue_Traversal, Unwind_Traversal, Terminate_Traversal)
         --  or
         --  Level = 0 and Real_Control = Skip_Siblings
      end Post;

      Asis_Control : Asis.Traverse_Control := Continue;

   begin
      if Control /= Continue_Traversal or else
         Asis.Elements.Is_Nil (Element)
      then
         return;
      end if;
      --  Real_Control = Continue_Traversal and Asis_Control = Continue
      Traverse (Element, Asis_Control, Control);
      --  Real_Control in
      --    (Continue_Traversal, Skip_Siblings, Unwind_Traversal,
      --     Terminate_Traversal)
   end Traverse_Levels;

   --  generic
   --     type State_Information (<>) is limited private;
   --
   --     with procedure Pre_Operation
   --                      (Element : in     Asis.Element;
   --                       Control : in out Control_Traversal;
   --                       State   : in out State_Information) is <>;
   --
   --     with procedure Post_Operation
   --                      (Element : in     Asis.Element;
   --                       Control : in out Control_Traversal;
   --                       State   : in out State_Information) is <>;
   --
   procedure Traverse_Element
     (Element : in     Asis.Element;
      Control : in out Control_Traversal;
      State   : in out State_Information)
   is
      procedure Pre
        (Element : in     Asis.Element;
         Depth   : in     Asis.ASIS_Positive;
         Control : in out Control_Traversal;
         State   : in out State_Information)
      is
         pragma Warnings (Off, Depth); --  silence -gnatwf
      begin
         Pre_Operation (Element, Control, State);
      end Pre;

      procedure Post
        (Element : in     Asis.Element;
         Depth   : in     Asis.ASIS_Positive;
         Control : in out Control_Traversal;
         State   : in out State_Information)
      is
         pragma Warnings (Off, Depth); --  silence -gnatwf
      begin
         Post_Operation (Element, Control, State);
      end Post;

      procedure Traverse is
         new Traverse_Levels (State_Information, Pre, Post);

   begin
      Traverse (Element, Control, State);
   end Traverse_Element;

   --  generic
   --     type State_Information (<>) is limited private;
   --
   --     with procedure Process_Element
   --            (Element : in     Asis.Element;
   --             Control : in out Control_Traversal;
   --             State   : in out State_Information) is <>;
   --
   procedure Traverse_List
     (Elements : in     Asis.Element_List;
      Control  : in out Control_Traversal;
      State    : in out State_Information)
   is
   begin
      for I in Elements'Range loop
         Process_Element (Elements (I), Control, State);
         exit when Control /= Continue_Traversal;
      end loop;
   end Traverse_List;

   --  generic
   --     type State_Information (<>) is limited private;
   --
   --     with procedure Process_Element
   --            (Element : in     Asis.Element;
   --             Control : in out Control_Traversal;
   --             State   : in out State_Information) is <>;
   --
   procedure Traverse_Unit_Elements
     (Element         : in     Asis.Compilation_Unit;
      Control         : in out Control_Traversal;
      State           : in out State_Information;
      Include_Pragmas : in     Boolean := False)
   is
      use Asis.Elements;
      use Asis.Compilation_Units;
   begin
      if Control /= Continue_Traversal or else Is_Nil (Element) then
         return;
      end if;
      declare
         Kind : constant Unit_Kinds := Unit_Kind (Element);
      begin
         if Kind not in A_Procedure .. A_Protected_Body_Subunit then
            Ada.Exceptions.Raise_Exception
              (Asis.Exceptions.ASIS_Inappropriate_Compilation_Unit'Identity,
               Package_Name & ".Traverse_Unit: Unexpected unit kind " &
               Unit_Kinds'Image (Kind));
         end if;
      end;
      --  Control = Continue_Traversal
      Process_Unit :
      declare
         procedure Process_List is
            new Traverse_List (State_Information, Process_Element);
      begin
         if Include_Pragmas then
            Process_List
              (Configuration_Pragmas (Enclosing_Context (Element)),
               Control, State);

            if Control >= Unwind_Traversal then return; end if;
            --  Control in (Continue_Traversal, Skip_Siblings)
            Control := Continue_Traversal;

            Process_List
              (Compilation_Pragmas (Element), Control, State);

            if Control >= Unwind_Traversal then return; end if;
            --  Control in (Continue_Traversal, Skip_Siblings)
            Control := Continue_Traversal;
         end if; --  Include_Pragmas

         Process_List
           (Context_Clause_Elements (Compilation_Unit => Element,
                                     Include_Pragmas  => True),
            Control, State);

         if Control >= Unwind_Traversal then return; end if;
         --  Control in (Continue_Traversal, Skip_Siblings)
         Control := Continue_Traversal;

         Process_Element (Unit_Declaration (Element), Control, State);
      end Process_Unit;
      --  Control in
      --    (Continue_Traversal, Skip_Siblings, Unwind_Traversal,
      --     Terminate_Traversal)
   end Traverse_Unit_Elements;

   --  generic
   --     type State_Information (<>) is limited private;
   --
   --     with procedure Pre_Operation
   --                      (Element : in     Asis.Element;
   --                       Control : in out Control_Traversal;
   --                       State   : in out State_Information) is <>;
   --
   --     with procedure Post_Operation
   --                      (Element : in     Asis.Element;
   --                       Control : in out Control_Traversal;
   --                       State   : in out State_Information) is <>;
   --
   procedure Traverse_Unit
     (Element         : in     Asis.Compilation_Unit;
      Control         : in out Control_Traversal;
      State           : in out State_Information;
      Include_Pragmas : in     Boolean := False)
   is
      procedure Process_Element is
         new Traverse_Element (State_Information);

      procedure Traverse is
         new Traverse_Unit_Elements (State_Information, Process_Element);

   begin
      Traverse (Element, Control, State, Include_Pragmas);
   end Traverse_Unit;

end Asis2.Iterators;

