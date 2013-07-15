------------------------------------------------------------------------------
--                                                                          --
--                    ASIS APPLICATION TEMPLATE COMPONENTS                  --
--                                                                          --
--           A C T U A L S _ F O R _ T R A V E R S I N G . P R E _ O P      --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--            Copyright (c) 2000, Free Software Foundation, Inc.            --
--                                                                          --
-- ASIS  Application  Templates are  free software; you can redistribute it --
-- and/or  modify it under  terms  of the  GNU  General  Public  License as --
-- published by the Free Software Foundation; either version 2, or (at your --
-- option) any later version. ASIS Application Templates are distributed in --
-- the hope that they will be useful, but  WITHOUT  ANY  WARRANTY; without  --
-- even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR --
-- PURPOSE. See the GNU General Public License for more details. You should --
-- have  received a copy of the GNU General Public License distributed with --
-- distributed  with  GNAT;  see  file  COPYING. If not, write to the Free  --
-- Software Foundation, 59 Temple Place - Suite 330, Boston, MA 02111-1307, --
-- USA.                                                                     --
--                                                                          --
-- ASIS Application Templates were developed and are now maintained by Ada  --
-- Core Technologies Inc (http://www.gnat.com).                             --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Wide_Text_IO;
with Ada.Text_IO;

with Ada.Characters.Handling;
with Ada.Exceptions;
with Asis.Definitions;
with Asis.Exceptions;
with Asis.Declarations;
with Asis.Errors;
with Asis.Implementation;
with Asis.Elements;
with Asis.Text;

with Asis2.Predicates;
with Asis2.Declarations;

separate (Actuals_For_Traversing)


procedure Pre_Op( 
   element :        Asis.Element;
   control : in out Asis.Traverse_Control;
   state   : in out Traversal_State ) is    

   procedure Print_Element_Info( name : Wide_String; element : Asis.Element ) is
      use Ada.Wide_Text_IO;
      
      text                      : constant Asis.Program_Text := Asis.Text.Element_Image( element );
      definition_kind           : constant Asis.Definition_Kinds := Asis.Elements.Definition_Kind( element );
      type_kind                 : constant Asis.Type_Kinds := Asis.Elements.Type_Kind( element );
      argument_kind             : constant Asis.Element_Kinds := Asis.Elements.Element_Kind( element );
   begin
      Put( "Dump of " & name );
      Put( "| text " & text );
      Put( "| definition_kind " & Asis.Definition_Kinds'Wide_Image( definition_kind ));
      Put( "| type_kind " & Asis.Type_Kinds'Wide_Image( type_kind ));
      Put_Line( "argument_kind " & Asis.Element_Kinds'Wide_Image( argument_kind ));
   end Print_Element_Info;
  
   use Ada.Wide_Text_IO;
   use Asis.Text;
   use type Asis.Type_Kinds;
   use type Asis.Definition_Kinds;
   use type Asis.Declaration_Kinds;
   use type Asis.Element_Kinds;
   
   definition_kind           : Asis.Definition_Kinds;
   type_kind                 : Asis.Type_Kinds;
   argument_kind             : Asis.Element_Kinds;
   text                      : constant Asis.Program_Text := Element_Image( element );
begin
   -- Put_Line( "Pre Op for element " & text );
   argument_kind := Asis.Elements.Element_Kind( element );
   definition_kind := Asis.Elements.Definition_Kind( element );
   type_kind := Asis.Elements.Type_Kind( element );
   case argument_kind is
      when Asis.A_Declaration => 
         null;
         -- Ada.Text_IO.Put_Line( "Found a declaration." & Asis.Element_Kinds'Image( argument_kind ));
         
      when Others =>
         null;
         -- Ada.Text_IO.Put_Line( "Found something else: " & Asis.Element_Kinds'Image( argument_kind ));
   end case;
   if( definition_kind = Asis.A_Record_Definition ) then
      declare
         enclosing : Asis.Element;
         record_element : Asis.Element;
         record_components : constant Asis.Record_Component_List := Asis.Definitions.Record_Components( element );
      begin
         Print_Element_Info( "Record Declaration ", element );
         Put_Line( "starting loop" );
         for i in record_components'Range loop
            record_element := record_components( i );
            
            Print_Element_Info( "record element ", record_element );
            enclosing := 
               Asis.Elements.Enclosing_Element( record_element );
            declare
               definition_kind : constant Asis.Definition_Kinds := Asis.Elements.Definition_Kind( record_element );
               type_kind       : constant Asis.Type_Kinds := Asis.Elements.Type_Kind( record_element );
               argument_kind   : constant Asis.Element_Kinds := Asis.Elements.Element_Kind( record_element );
               declaration_kind : constant Asis.Declaration_Kinds := Asis.Elements.Declaration_Kind( record_element );
               -- begin
               --  if( argument_kind = Asis.A_Declaration ) then
               --   declare
               name_list      : constant Asis.Defining_Name_List := Asis.Declarations.Names( record_element );
               -- type_kind : constant Asis.Type_Kinds := Asis.Elements.Type_Kind( record_element );
               dec            : constant Asis.Element := Asis2.Predicates.Decl( record_element );
               def            : constant Asis.Definition := Asis2.Predicates.Def( dec );
               obj_dec_view   : constant Asis.Definition := Asis.Declarations.Object_Declaration_View( dec );
               obj_dec_kind   : constant Asis.Declaration_Kinds := Asis.Elements.Declaration_Kind( obj_dec_view );
               obj_type_kind  : constant Asis.Type_Kinds := Asis.Elements.Type_Kind( obj_dec_view );
               initialization_expression : Asis.Expression;
               -- type        : constant Asis.Declaration := Asis2.Predicates.Type_Of( def );
               -- base        : constant  Asis.Declaration := Asis2.Predicates.Base( type );
               -- type_structure : constant Asis.Declaration := Asis.Definitions.Corresponding_Type_Structure( def );
               -- root_type      : constant Asis.Declaration := Asis.Definitions.Corresponding_Root_Type( def );
               -- type_kind : constant Asis.Type_Kinds := Asis.Elements.Type_Kind( type );
               begin
                  -- if( argument_kind = Asis.A_Type_Declaration ) then
                  --   Put_Line( "a type declaration" );
                  -- end if;
                  Print_Element_Info( "dec ", dec );
                  Print_Element_Info( "def ", def );
                  Put_Line( "Obj declaration_kind |" & Asis.Declaration_Kinds'Wide_Image( obj_dec_kind ));
                  Put_Line( "Obj Type Kind |" & Asis.Type_Kinds'Wide_Image( obj_type_kind ));
                  case obj_type_kind is
                     when Asis.A_Derived_Type_Definition =>
                     declare
                        corresponding_root_type : constant Asis.Declaration := 
                           Asis.Definitions.Corresponding_Root_Type( obj_dec_view ); -- A_Derived_Type_Definition
                     begin
                         Print_Element_Info( "corresponding_root_type ", corresponding_root_type );
                     end;
                     when 
                        Asis.A_Derived_Record_Extension_Definition |
                        Asis.A_Record_Type_Definition |
                        Asis.A_Tagged_Record_Type_Definition =>
                        declare
                           rd :  constant Asis.Declaration := 
                              Asis.Definitions.Record_Definition( obj_dec_view );
                        begin
                           Print_Element_Info( "rd ", rd );
                        end;
                     when others =>
                        null;
                  end case;
                  initialization_expression :=  Asis.Declarations.Initialization_Expression( dec );
                  
                  Print_Element_Info( "obj_dec_view ", obj_dec_view );
                  Print_Element_Info( "initialization_expression ", initialization_expression );
                  -- Print_Element_Info( "type structure ", type_structure );
                  --Put_Line( "record element name |" & Asis.Declarations.Defining_Name_Image( Name_List( 1 )) & "| ");
                  -- Put_Line( Asis.Declarations.Defining_Name_Image( Asis2.Declarations.Name_Definition( record_element )));
                  --Print_Element_Info( "type structure ", type_structure );
                  --Print_Element_Info( "type structure ", type_structure );
                  Put_Line( " ============================ " );
               --   end;
               -- end if;
            end;
         end loop;
      end;
   end if;
exception

   when Ex : Asis.Exceptions.ASIS_Inappropriate_Context          |
             Asis.Exceptions.ASIS_Inappropriate_Container        |
             Asis.Exceptions.ASIS_Inappropriate_Compilation_Unit |
             Asis.Exceptions.ASIS_Inappropriate_Element          |
             Asis.Exceptions.ASIS_Inappropriate_Line             |
             Asis.Exceptions.ASIS_Inappropriate_Line_Number      |
             Asis.Exceptions.ASIS_Failed                         =>

      Put ("Pre_Op : ASIS exception (");

      Put (Ada.Characters.Handling.To_Wide_String (
              Ada.Exceptions.Exception_Name (Ex)));

      Put (") is raised");
      New_Line;

      Put ("ASIS Error Status is ");

      Put
        (Asis.Errors.Error_Kinds'Wide_Image (Asis.Implementation.Status));

      New_Line;

      Put ("ASIS Diagnosis is ");
      New_Line;
      Put (Asis.Implementation.Diagnosis);
      New_Line;

      Asis.Implementation.Set_Status;

   when Ex : others =>

      Put ("Pre_Op : ");

      Put (Ada.Characters.Handling.To_Wide_String (
              Ada.Exceptions.Exception_Name (Ex)));

      Put (" is raised (");

      Put (Ada.Characters.Handling.To_Wide_String (
              Ada.Exceptions.Exception_Information (Ex)));

      Put (")");
      New_Line;

end Pre_Op;
