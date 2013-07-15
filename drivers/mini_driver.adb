with Ada.Calendar;
with Ada.Calendar.Formatting;

with Gnat.Expect;

with Ada.Command_Line;
with Ada.Directories;
with Ada.IO_Exceptions;
with Ada.Strings.Unbounded;
with Ada.Characters.Handling;
with Ada.Text_IO;

with Gnat.OS_Lib;
with System;

with EU.BE.Model.Runner;
with EU.Web.Settings;
with EU.BE.Model.Settings;

with Text_Utils;
with Web_Utils;

procedure Mini_Driver is
  
   use Ada.Strings.Unbounded;
   use Ada.Text_IO;
   use Text_Utils;
   use Ada.Calendar;
   use EU.BE.Model.Settings;
   
   package osl renames Gnat.OS_Lib;
   package runner renames EU.BE.Model.Runner;
   package expect renames Gnat.Expect;

   default_settings : Model_Settings;
   default_descriptor : expect.Process_Descriptor;

   MAX_RESERVED_TIME : constant Duration := 60.0;
   
   settings        : Model_Settings;
   locked          : Boolean := True;
   proc_descriptor : expect.Process_Descriptor;
   state           : State_Rec := NULL_STATE_REC;
   reserved_from   : Time := DEFAULT_TIME;
   
   
   function Job_Is_Running return Boolean is
   use type expect.Process_Id;
   begin
      return state.phase in currency .. eu_output or state.pid > 0;
   end Job_Is_Running;
   
   procedure Update_State is
      use Ada.Calendar;
      sep                : constant String := EU.Web.Settings.Dir_Separator; 
      report_file_name   : constant String := settings.Qualified_Output_Directory & SEP & "eu_dump.txt";
   begin
      state.pid := Integer( proc_descriptor.Get_Pid );
      -- expect ..    
      if( state.pid > 0 )then
         runner.Monitor( settings, state );
         case state.phase is
         when not_started => null;
         when starting .. eu_output =>
            if( state.start_time > DEFAULT_TIME )then
               state.execution_time := Ada.Calendar.Clock - state.start_time;
            end if;
         when eu_run_complete .. mefisto_complete =>
            proc_descriptor := default_descriptor;
         end case;
      else
         state.phase := not_started;
      end if;
      state.pid := Integer( proc_descriptor.Get_Pid );
   end  Update_State;

   procedure Run_Direct is 
      use System;
      use Gnat.OS_Lib;
      use type Process_Id;
      
      args : Argument_List( 1 .. 1 );
      success : Boolean;
   begin
      args( 1 ) := new String'( settings.Qualified_Main_Configuration_File_Name );
      Put_Line( "Run_Direct " & "script is " & settings.invocation_script & " config is |" & settings.Qualified_Main_Configuration_File_Name & "| ");
      state.health := normal;
      state.phase := starting;
      Spawn(
         Program_Name => settings.invocation_script, 
         Args         => args, 
         Output_File  => "f1",
         Return_Code  => state.pid,
         Success      => success,
         Err_to_Out   => True );
         state.start_time := Ada.Calendar.Clock;
      -- delay 1.0;
      if( state.pid = -1 )then 
         state.error_code := -1;
         state.health := in_error;
      end if;
      Put_Line(  "Run Direct " & " spawned; pid = " & state.pid'Img & " error code " & Integer'Image( state.error_code ));
      Free( args( 1 ));
      Put_Line( "Run Direct " & " state.error_code = " & Integer'Image( state.error_code ) & " health " & state.health'Img );
      loop
         Update_State;
         delay 1.0;
         Put_Line( "phase is " & state.phase'Img & " module " & state.module'Img );
         exit when not Job_Is_Running;
      end loop;
   end Run_Direct;

   
   -- 
   -- procedure Run_Detached is 
      -- use System;
      -- use Gnat.OS_Lib;
      -- use type Process_Id;
      -- 
      -- args : Argument_List( 1 .. 1 );
   -- begin
      -- args( 1 ) := new String'( settings.Qualified_Main_Configuration_File_Name );
      -- Put_Line( "Run_Detached " & "script is " & settings.invocation_script & " config is |" & settings.Qualified_Main_Configuration_File_Name & "| ");
      -- state.health := normal;
      -- state.phase := starting;
      -- expect.Non_Blocking_Spawn(
         -- descriptor  => proc_descriptor,
         -- command     => settings.invocation_script, 
         -- args        => args, 
         -- buffer_size => 4096,
         -- err_to_out  => True );
         -- state.start_time := Ada.Calendar.Clock;
      -- -- delay 1.0;
      -- if( state.pid = -1 )then 
         -- state.error_code := -1;
         -- state.health := in_error;
      -- end if;
      -- Put_Line(  "Run Direct " & " spawned; pid = " & state.pid'Img & " error code " & Integer'Image( state.error_code ));
      -- Free( args( 1 ));
      -- Put_Line( "Run Direct " & " state.error_code = " & Integer'Image( state.error_code ) & " health " & state.health'Img );
      -- loop
         -- Update_State;
         -- delay 1.0;
         -- Put_Line( "phase is " & state.phase'Img & " module " & state.module'Img );
         -- exit when not Job_Is_Running;
      -- end loop;
   -- end Run_Detached;
-- 
   procedure Initialise_Settings is

   begin
      Put_Line( "Add_All_Targets globals entered " );
      if( Ada.Command_Line.Argument_Count > 1 ) then
         default_settings := Read_Model_Settings( Ada.Command_Line.Argument( 1 ));
         EU.Web.Settings.Read_Web_Settings( Ada.Command_Line.Argument( 2 ));
         default_settings.Set_Dir_Separator( EU.Web.Settings.Dir_Separator( 1 )); 
         settings := default_settings;
         settings.Set_Users_Directory(  Ada.Command_Line.Argument( 3 ));
         settings.Set_Run_Id( Ada.Command_Line.Argument( 4 ));
         Put_Line( "reading parameters OK; sep is " &  EU.Web.Settings.Dir_Separator( 1 ));
      else
         Put_Line( "Call with <default model settings> <web settings> <user> <run>" );
      end if;
   end Initialise_Settings;
   
   
   task type Model_Runner_Task is
      entry Start;
      entry Stop;
   end Model_Runner_Task;
   
   task body Model_Runner_Task is
    
   begin
   		accept Start do
   			Put_Line( "START" );	
   		end Start;
   		
		-- loop
			if( not locked )then
				locked := True;
				Put_Line( "RUNNING" );
				Run_Direct;
				-- locked := False;
				delay 2.0;
			end if;
		-- end loop;
	   
		accept Stop do
   			Put_Line( "Stop" );	
	   end Stop;
	   
   end Model_Runner_Task;
   
 
   p : Model_Runner_Task;

begin
   Initialise_Settings;
   locked := False;
   p.Start;
   
   p.Stop;
   -- Run_Detached;
   -- Run_Direct;
   loop
      Put_Line( "MAIN THREAD - HELLO! " & locked'Img );
      delay 2.0;
   end loop;
end Mini_Driver;
