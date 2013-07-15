

with Ada.Characters.Handling;
with Ada.Characters;
with Ada.Command_Line;
with Ada.Exceptions;
with Ada.Wide_Text_IO;
with Asis.Text;
with Asis;
with Asis.Ada_Environments;
with Asis.Compilation_Units;
with Asis.Errors;
with Asis.Exceptions;
with Asis.Elements;
with Asis.Implementation;
with Asis.Set_Get;

with Context_Processing;
with Unit_Processing;

procedure Asis_Tests is

   use Asis.Set_Get; -- comp_unit equality test hides in here for some reason
   use Ada.Wide_Text_IO;
   use Ada.Characters.Handling;
   
   function Get_Target return Wide_String is
   begin
      if( Ada.Command_Line.Argument_Count > 0 ) then
         return To_Wide_String(Ada.Command_Line.Argument( 1 ));
      else
         return "type_tests"; -- "eu.web.settings";
      end if;
   end Get_Target;
   
   my_context_name           : constant Wide_String := "Mefisto";
   -- -T search bin/ for tree files: see user guide p 28, reference 9.
   my_context_parameters     : constant Wide_String := "-Tbin/ -CA -FM"; 
   initialization_parameters : constant Wide_String := "";
   finalization_parameters   : constant Wide_String := "";
   target                    : constant Wide_String := Get_Target;

   comp_unit                 : Asis.Compilation_Unit;
   my_context                : Asis.Context;  --  3.5
   decl                      : Asis.Declaration;
begin
   Asis.Implementation.Initialize( initialization_parameters );  
   Asis.Ada_Environments.Associate( The_Context => my_context, 
                                    Name        => my_context_name,
                                    Parameters  => my_context_parameters ); --  8.3
   Asis.Ada_Environments.Open( my_context );                                --  8.4
  
   -- Context_Processing.Process_Context( The_Context => my_context,
   --                                     Trace       => true );
   comp_unit := Asis.Compilation_Units.Library_Unit_Declaration( target, my_context );  -- 10.8
   decl := Asis.Elements.Unit_Declaration( comp_unit );
   
   Put_Line( "Full Name " & Asis.Compilation_Units.Unit_Full_Name( comp_unit ));
   Put_Line( "Text Name " & Asis.Compilation_Units.Text_Name( comp_unit ));
   Put_Line( "Declaration Element Image " & Asis.Text.Element_Image( decl ));
   if( comp_unit = Asis.Nil_Compilation_Unit )then
      Put_Line( target & "  was null, fool." );
   else
      Put_Line( "processing " );
      Unit_Processing.Process_Unit( comp_unit );
   end if;
   
   Asis.Ada_Environments.Close( my_context );                 --  8.5
   Asis.Ada_Environments.Dissociate( my_context );            --  8.6
   Asis.Implementation.Finalize( finalization_parameters );   --  6.8
exception
   --  The exception handling in this driver is somewhat redundant and may
   --  need some reconsidering when using this driver in real ASIS tools
   when Ex : Asis.Exceptions.ASIS_Inappropriate_Context          |
             Asis.Exceptions.ASIS_Inappropriate_Container        |
             Asis.Exceptions.ASIS_Inappropriate_Compilation_Unit |
             Asis.Exceptions.ASIS_Inappropriate_Element          |
             Asis.Exceptions.ASIS_Inappropriate_Line             |
             Asis.Exceptions.ASIS_Inappropriate_Line_Number      |
             Asis.Exceptions.ASIS_Failed                         =>

      Put ("ASIS exception (");
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

      Put (Ada.Characters.Handling.To_Wide_String (
              Ada.Exceptions.Exception_Name (Ex)));
      Put (" is raised (");
      Put (Ada.Characters.Handling.To_Wide_String (
              Ada.Exceptions.Exception_Information (Ex)));
      Put (")");
      New_Line;
   
end Asis_Tests;
