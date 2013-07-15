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
--    14-JUL-2003   TW  Added @Traverse_Levels@, @Traverse_List@,
--                      @Traverse_Unit@, and @Traverse_Unit_Elements@.
--  </HISTORY>
-------------------------------------------------------------------------------

pragma License (Modified_GPL);

with Asis;

package Asis2.Iterators is

   pragma Elaborate_Body;

   type Control_Traversal is
     (Continue_Traversal,             --  = Asis.Continue.
      Terminate_Current,              --  = Asis.Abandon_Children.
      Skip_Children,                  --  = Asis.Abandon_Children + Post.
      Terminate_Current_And_Siblings, --  = Asis.Abandon_Siblings.
      Skip_Siblings,                  --  = Asis.Abandon_Siblings + Post.
      Unwind_Traversal,               --  = "sticky" Skip_Siblings.
      Stop_Traversal,                 --  = Asis.Terminate_Immediately + Post.
      Terminate_Traversal             --  = Asis.Terminate_Immediately.
     );
   --  The names of these enumeration values have been chosen deliberately
   --  such that they do not clash with the literals declared for the type
   --  @Asis.Traverse_Control@.
   --
   --  <DL>
   --    <DT>@Continue_Traversal@
   --    <DD>Like @Asis.Continue@. Keep going with normal depth-first
   --        traversal.
   --        <BR><BR>
   --    <DT>@Terminate_Current@
   --    <DD>Exactly like @Asis.Abandon_Children@, i.e. if set in the
   --        @Pre_Operation@, the corresponding @Post_Operation@ is
   --        <STRONG><EM>not</EM></STRONG> called!
   --        <BR><BR>
   --    <DT>@Skip_Children@
   --    <DD>Similar to @Asis.Abandon_Children@. If set in a @Pre_Operation@,
   --        skips the children of the current element and then invokes the
   --        @Post_Operation@ of the current element with @Control@ =
   --        @Skip_Children@.
   --        <BR><BR>
   --        If @Control@ is @Skip_Siblings@ after a @Post_Operation@,
   --        this is equivalent to @Continue_Traversal@.
   --        <BR><BR>
   --    <DT>@Terminate_Current_And_Siblings@
   --    <DD>Exactly like @Asis.Abandon_Siblings@, i.e. if set in the
   --        @Pre_Operation@, the corresponding @Post_Operation@ is
   --        <STRONG><EM>not</EM></STRONG> called!
   --        <BR><BR>
   --    <DT>@Skip_Siblings@
   --    <DD>Similar to @Asis.Abandon_Siblings@. If set in a @Pre_Operation@,
   --        skips the children of the current element and then invokes the
   --        @Post_Operation@ of the current element with @Control@ =
   --        @Skip_Siblings@.
   --        <BR><BR>
   --        If @Control@ is @Skip_Siblings@ after a @Post_Operation@,
   --        traversal skips the remaining sibling elements and picks up
   --        again at the @Post_Operation@ of the parent element with
   --        @Control@ = @Continue_Traversal@.
   --        <BR><BR>
   --    <DT>@Unwind_Traversal@
   --    <DD>Stops traversing and unwinds, calling all the @Post_Operations@
   --        for which the corresponding @Pre_Operations@ had already been
   --        called (and the corresponding @Post_Operation@ has <EM>not</EM>
   --        yet been called).
   --        <BR><BR>
   --        If set in @Pre_Operation@, skips the children of the current
   --        element and then invokes the @Post_Operation@ of the current
   --        element with @Control@ = @Unwind_Traversal@.
   --        <BR><BR>
   --        If @Control@ is @Unwind_Traversal@ after a @Post_Operation@,
   --        traversal skips the remaining sibling elements and picks up
   --        again at the @Post_Operation@ of the parent element with
   --        @Control@ = @Unwind_Traversal@.
   --        <BR><BR>
   --        (@Unwind_Traversal@ is a "sticky" @Skip_Siblings@.)
   --        <BR><BR>
   --    <DT>@Stop_Traversal@
   --    <DD>Similar to @Asis.Terminate_Immediately@, but if called in a
   --        @Pre_Operation@, skips the current element's children and then
   --        <EM>does</EM> call the corresponding @Post_Operation@ with
   --        @Control@ = @Stop_Traversal@.
   --        <BR><BR>
   --        If @Control@ is @Stop_Traversal@ after a @Post_Operation@,
   --        this is equivalent to @Terminate_Traversal@.
   --        <BR><BR>
   --        This is like @Terminate_Traversal@, but using the corresponding
   --        @Post_Operation@ as a "last rites" handler that may override
   --        the decision to stop traversing.
   --        <BR><BR>
   --        Setting @Stop_Traversal@ in a @Pre_Operation@ is a short-hand
   --        for setting @Unwind_Traversal@ in the @Pre_Operation@ and then
   --        setting @Terminate_Traversal@ in the corresponding
   --        @Post_Operation@.
   --        <BR><BR>
   --    <DT>@Terminate_Traversal@
   --    <DD>Like @Asis.Terminate_Immediately@. Immediately and irrevokably
   --        stops traversing. No operation will be called anymore.
   --  </DL>
   --
   --  The difference to the standard ASIS iterator scheme is that this
   --  iterator will always call the @Post_Operation@ if the corresponding
   --  @Pre_Operation@ has been called (unless @Control@ = @Stop_Traversal@),
   --  whereas the standard iterator does <EM>not</EM> call the
   --  @Post_Operation@ of an element if @Control@ is set in that
   --  element's @Pre_Operation@ to @Abandon_Children@ or @Abandon_Siblings@.
   --
   --  Using @Terminate_Current@ or @Terminate_Current_And_Siblings@, the
   --  original behavior of the standard @Asis.Iterator.Traverse_Element@ is
   --  also available.

   generic
      type State_Information (<>) is limited private;

      with procedure Pre_Operation
                       (Element : in     Asis.Element;
                        Control : in out Control_Traversal;
                        State   : in out State_Information) is <>;

      with procedure Post_Operation
                       (Element : in     Asis.Element;
                        Control : in out Control_Traversal;
                        State   : in out State_Information) is <>;

   procedure Traverse_Element
     (Element : in     Asis.Element;
      Control : in out Control_Traversal;
      State   : in out State_Information);
   --  Does a depth-first traversal of the element tree rooted at @Element@.
   --  Calls the @Pre_Operation@ for each element in the tree before
   --  descending further, and the @Post_Operation@ after having processed
   --  all descendents.
   --
   --  Traversal can be fine-tuned by setting @Control@ as appropriate.
   --
   --  The @State@ is passed on untouched to the @Pre_Operation@s and the
   --  @Post_Operations@.
   --
   --  Note that you can instantiate this iterator for any type, even an
   --  unconstrained type.
   --
   --  Upon return, @Control@ is one of @Continue_Traversal@, @Skip_Siblings@,
   --  @Unwind_Traversal@, or @Terminate_Traversal@. (Especially
   --  @Skip_Siblings@ is useful when traversing an element list explicitly.)
   --
   --  The @Pre_Operation@ is always called with @Control@ =
   --  @Continue_Traversal@.
   --
   --  The @Post_Operation@ is called with @Control@ one of
   --  @Continue_Traversal@, @Skip_Children@, @Skip_Siblings@,
   --  @Unwind_Traversal@, or @Stop_Traversal@.

   generic
      type State_Information (<>) is limited private;

      with procedure Pre_Operation
             (Element : in     Asis.Element;
              Depth   : in     Asis.ASIS_Positive;
              Control : in out Control_Traversal;
              State   : in out State_Information) is <>;

      with procedure Post_Operation
             (Element : in     Asis.Element;
              Depth   : in     Asis.ASIS_Positive;
              Control : in out Control_Traversal;
              State   : in out State_Information) is <>;

   procedure Traverse_Levels
     (Element : in     Asis.Element;
      Control : in out Control_Traversal;
      State   : in out State_Information);
   --  Like @Traverse_Element@, but also maintains a depth count.
   --  @Depth@ = @1@ signifies top-level, i.e. the invocations of
   --  @Pre_Operation@ or @Post_Operation@ for the actual parameter
   --  for @Element@.

   generic
      type State_Information (<>) is limited private;

      with procedure Process_Element
             (Element : in     Asis.Element;
              Control : in out Control_Traversal;
              State   : in out State_Information) is <>;

   procedure Traverse_List
     (Elements : in     Asis.Element_List;
      Control  : in out Control_Traversal;
      State    : in out State_Information);
   --  Equivalent to the code
   --  <PRE>
   --    for I in Elements'Range loop
   --      Process_Element (Elements (I), Control, State);
   --      exit when Control /= Continue_Traversal;
   --    end loop;
   --  </PRE>
   --
   --  Note that setting @Control@ to @Skip_Siblings@ or
   --  @Terminate_Current_And_Siblings@ on an element skips the remaining
   --  elements in the list.

   generic
      type State_Information (<>) is limited private;

      with procedure Pre_Operation
             (Element : in     Asis.Element;
              Control : in out Control_Traversal;
              State   : in out State_Information) is <>;

      with procedure Post_Operation
             (Element : in     Asis.Element;
              Control : in out Control_Traversal;
              State   : in out State_Information) is <>;

   procedure Traverse_Unit
     (Element         : in     Asis.Compilation_Unit;
      Control         : in out Control_Traversal;
      State           : in out State_Information;
      Include_Pragmas : in     Boolean := False);
   --  Slightly generalized version of @Traverse_Element@. Traverses the
   --  <EM>unit</EM>, i.e. the configuration pragmas, compilation pragmas,
   --  context clauses (and pragmas), and then the unit declaration (in that
   --  order). The semantics of @Control@ is as for @Traverse_Element@.
   --
   --  Setting @Control@ to @Skip_Siblings@ in the @Pre_Operation@ or the
   --  @Post_Operation@ of a configuration pragmas makes @Traverse_Unit@
   --  skip the remaining configuration pragmas and continue with the first
   --  compilation pragma. Setting @Skip_Siblings@ on a compilation pragma
   --  skips the remaining compilation pragmas and continues with the first
   --  context clause; and setting it on a context clause skips the remaining
   --  context clauses and continues with the unit declaration element.
   --
   --  If @Include_Pragmas@ is @False@, configuration and compilation pragmas
   --  are not traversed at all, only the context clauses and the unit element
   --  are. If @Include_Pragmas@ is @True@, the pragmas <EM>are</EM> traversed.
   --  Pragmas within the context clauses always are included in the traversal.

   generic
      type State_Information (<>) is limited private;

      with procedure Process_Element
             (Element : in     Asis.Element;
              Control : in out Control_Traversal;
              State   : in out State_Information) is <>;

   procedure Traverse_Unit_Elements
     (Element         : in     Asis.Compilation_Unit;
      Control         : in out Control_Traversal;
      State           : in out State_Information;
      Include_Pragmas : in     Boolean := False);
   --  Like @Traverse_Unit@, but using @Process_Element@ (which could e.g. be
   --  an instantiation of @Traverse_Element@ of @Traverse_Levels@) to process
   --  the child elements of the unit.

end Asis2.Iterators;

