--
--  $Author: graham_s $
--  $Date: 2010-02-15 13:40:48 +0000 (Mon, 15 Feb 2010) $
--  $Revision: 8644 $
--

with Ada.Text_IO;

with EU.BE.Globals;
with EU.BE.Parameter_System_Declarations;

with EU.BE.Household.Web_IO;
with EU.BE.I81N;
with EU.BE.Users.IO;
with EU.BE.Users;
with EU.BE;
-- with EU.BE.Model.Results;
with EU.BE.Model.Settings;
with EU.BE.Model.Settings;
with EU.BE.Output;
with EU.BE.Model;
with EU.Web.Settings;
with EU;
with EU.BE.Model.Runner.Web;

with AUnit.Test_Cases;
with AUnit.Assertions;

with Text_Buffer;
with Text_Utils;
   
package body EU.BE.Model.Runner.Tests is

   use AUnit.Assertions;
   use AUnit.Test_Cases.Registration;
   use Ada.Text_IO;
   use Text_Utils;
   
   package users                renames EU.BE.Users;
   package globals              renames EU.BE.Globals;
   subtype Param_Buff           is EU.BE.Parameter_System_Declarations.BE_Buffer;
    
   package ms renames EU.BE.Model.Settings;
   
   model_sett   : ms.Model_Settings;
   param_buffer : Param_Buff;

   procedure Test_Basic_Run( t : in out AUnit.Test_Cases.Test_Case'Class ) is
      use EU.BE.Output;
      use EU.BE.Users;
      use EU.BE.Users.IO;
      use EU.BE.Model.Settings;
      sep            : constant String := EU.Web.Settings.Dir_Separator;
      uno            : constant := 1;
      nstr           : constant String := Positive'Image( uno );
      username       : constant Unbounded_String := TuS( "test_user" ) & nstr( 2 .. nstr'Length );
      filename       : constant Unbounded_String := model_sett.Working_Root & sep & username & sep & USER_FILE_NAME;
      user           : constant User_Type :=  Read_User( TS( filename ));    
      run_number_str : constant String := Get_New_Run_Id;
      output_dir     : Unbounded_String;
      model_sett_2   : ms.Model_Settings := model_sett.Copy;
      state          : State_Rec;
      output         : Outputs_Rec;
   begin
      model_sett_2.Set_Dir_Separator( EU.Web.Settings.Dir_Separator( 1 ));
      model_sett_2.Set_Run_Id( run_number_str );
      model_sett_2.Set_Users_Directory( username );
      Create_User_Files( model_sett_2.Working_Root, EU.Web.Settings.Dir_Separator, user );
      Put_Line( "starting create; username " & TS( username ) & "| run number |" & run_number_str );
      output_dir := Create_Directories_For_Run( 
         model_sett_2.Working_Root, 
         SEP, 
         user, 
         run_number_str );
      Put_Line( "make output dir as |" & TS( output_dir ) & "|" );
      globals.Create_Parameter_Files( model_sett_2, param_buffer );
      Run_Direct( model_sett_2, output, state ); 
      Assert( state.error_code = 1, "run shpuld return 1 was " & Integer'Image( state.error_code ));
   end Test_Basic_Run;
   
   procedure Test_Write_Control( t : in out AUnit.Test_Cases.Test_Case'Class ) is
   begin
      Write_EU_Config_File( model_sett );    
   end Test_Write_Control;
   
   
   procedure Test_Settings(  t : in out AUnit.Test_Cases.Test_Case'Class ) is
      model_sett_2 : ms.Model_Settings := model_sett.Copy;
   begin
      
      model_sett_2.Set_Users_Directory( "graham_s" );
      model_sett_2.Set_Run_Id( "run1" );
      model_sett.Set_Dir_Separator( '/' );

      Put_Line( "model_sett_2.Year = " & model_sett_2.Year'Img );
      Put_Line( "model_sett_2.Run_Id = " & model_sett_2.Run_Id );
      
      Put_Line( "model_sett_2.Model_Root = " & model_sett_2.Model_Root );
      
      Put_Line( "model_sett_2.Working_Root = " & model_sett_2.Working_Root );
      Put_Line( "model_sett_2.Qualified_Users_Directory = " & model_sett_2.Qualified_Users_Directory );
      Put_Line( "model_sett_2.Qualified_Run_Directory = " & model_sett_2.Qualified_Run_Directory );
      Put_Line( "model_sett_2.Qualified_Output_Directory = " & model_sett_2.Qualified_Output_Directory );
      
      Put_Line( "model_sett_2.Invocation_Script = " & model_sett_2.Invocation_Script );
      Put_Line( "model_sett_2.Datafile_Directory = " & model_sett_2.Datafile_Directory );
      Put_Line( "model_sett_2.Log_File_Name = " & model_sett_2.Qualified_Log_File_Name );
      Put_Line( "model_sett_2.Monitor_File_Name = " & model_sett_2.Qualified_Monitor_File_Name );
      Put_Line( "model_sett_2.main_configuration_file_name = " & model_sett_2.Qualified_Main_Configuration_File_Name );
      Put_Line( "model_sett_2.Log_Warnings = " & model_sett_2.Log_Warnings'Img );
      Put_Line( "model_sett_2.Log_Runtime = " & model_sett_2.Log_Runtime'Img );
      Put_Line( "model_sett_2.Dataset_Name = " & model_sett_2.Dataset_Name );
      Put_Line( "model_sett_2.Spine_File_Name = " & model_sett_2.Spine_File_Name );
      Put_Line( "model_sett_2.System_Name = " & model_sett_2.System_Name );
      Put_Line( "model_sett_2.Output_Directory = " & model_sett_2.Qualified_Output_Directory );
      Put_Line( "model_sett_2.Euromod_Version = " & model_sett_2.Euromod_Version );
      Put_Line( "model_sett_2.Parameter_Directory = " & model_sett_2.Qualified_Parameter_Directory );
      Put_Line( "model_sett_2.control_file_name = " & model_sett_2.Control_File_Name );
   end Test_Settings;
   
   procedure Test_Monitor(  t : in out AUnit.Test_Cases.Test_Case'Class ) is
      st1 , st2 : State_Rec;
   begin
      Read_EM_Monitor_File( "etc/monitor.txt", st1 );
      Put_Line( "Positive'Image(Phase_Type'Value( households )) " & Positive'Image(Phase_Type'Pos( households )));
      Assert( st1.module = 213, "module should be 213 was " & Natural'Image( st1.module ));
      Assert( st1.phase = non_households, " st1.phase should be non_households was " & Phase_Type'Image( st1.phase ));
      Assert( st1.household = 2000, "household should be 2000; was " &  Natural'Image( st1.household ));
      Assert( st1.health = normal, "health should be normal was " &  Health_Type'Image( st1.health ));
      Assert( st1.error_code = 0, "error should be 0; was " & Integer'Image( st1.error_code ));
      Monitor( model_sett, st2 );
      Put_Line( "model_sett.monitor_file " & model_sett.Qualified_Monitor_File_Name );
      Assert( st2.module = 210, "2 module should be 210 was " & Natural'Image( st2.module ));
      Assert( st2.phase = eu_run_complete, " st2.phase should be eu_run_complete was " & Phase_Type'Image( st2.phase ));
      Assert( st2.household = 0, "st2 household should be 0; was " &  Natural'Image( st2.household ));
      Assert( st2.health = normal, "health should be normal was " &  Health_Type'Image( st2.health ));
      Assert( st2.error_code = 0, "error should be 0; was " & Integer'Image( st2.error_code ));
      
      -- 4 2000 213
   end Test_Monitor;
   
   procedure Test_Create_Parameter_Files(  t : in out AUnit.Test_Cases.Test_Case'Class ) is
      use EU.BE.Users;
      use EU.BE.Users.IO;
      use EU.BE.Model.Settings;
      sep        : constant String := EU.Web.Settings.Dir_Separator;
      uno        : constant := 1;
      nstr       : constant String := Positive'Image( uno );
      username   : constant Unbounded_String := TuS( "test_user" ) & nstr( 2 .. nstr'Length );
      filename   : constant Unbounded_String := model_sett.Working_Root & sep & username & sep & USER_FILE_NAME;
      user       : constant User_Type :=  Read_User( TS( filename ));    
      run_number_str : constant String :=  Get_New_Run_Id;
      output_dir : Unbounded_String;
   begin
      model_sett.Set_Run_Id( run_number_str );
      output_dir := Create_Directories_For_Run( model_sett.Working_Root, SEP, user, run_number_str );
      Put_Line( "starting create; username " & TS( username ) & "| run number |" & run_number_str );
      globals.Create_Parameter_Files( model_sett, param_buffer );
   end Test_Create_Parameter_Files;
   
   procedure Test_Load_Past_Runs(  t : in out AUnit.Test_Cases.Test_Case'Class ) is
      r : Run_Results_Record;
      l : Run_Results_List;
      
   begin
      model_sett.Set_Users_Directory( "test_user13" );
      l := Get_Previous_Runs( model_sett );
      for p in 1 .. Natural( l.Length ) loop
         Put_Line( To_String( l.Element( p )));
      end loop;
   end Test_Load_Past_Runs; 
   
   --------------------
   -- Register_Tests --
   --------------------

   procedure Register_Tests( t : in out Test_Case ) is
   begin 
      Register_Routine( T, Test_Write_Control'Access, "Test_Write_Control" );
      Register_Routine( T, Test_Settings'Access, "Test_Settings" );
      Register_Routine( T, Test_Create_Parameter_Files'Access, "Test_Create_Parameter_Files;" );
      Register_Routine( T, Test_Basic_Run'Access, "Test_Basic_Run" );
      Register_Routine( T, Test_Monitor'Access, "Test_Monitor" );
      Register_Routine( T, Test_Load_Past_Runs'Access, "Test_Load Past Runs" );
   end Register_Tests;

   procedure Set_Up (T : in out Test_Case) is
   begin
      EU.Web.Settings.Read_Web_Settings( "etc/web_settings.txt" );
      Put_Line( "read OK" );
      EU.BE.I81N.Load_Translations;
      model_sett := ms.Read_Model_Settings( "etc/be_settings.txt" );
      model_sett.Set_Run_Id( "run1" );
      model_sett.Set_Users_Directory( "graham_s" );
      model_sett.Set_Dir_Separator( '/' );
      
      param_buffer  := globals.Get_Loaded_Input_Buffer( EU.BE.I81N.en );
   end Set_Up;

   ----------
   -- Name --
   ----------
   function Name ( T : Test_Case ) return Message_String is
   begin
      return Format( "EU.BE.Household.IO.Tests" );
   end Name;
   
   
end EU.BE.Model.Runner.Tests;
