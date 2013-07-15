
with Ada.Calendar.Formatting;
with Ada.Directories;
with Ada.Exceptions;
with Ada.IO_Exceptions;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;
with Ada.Text_IO;

with Gnat.OS_Lib;
with System;

with EU.BE.Globals;
with EU.BE.Household.Web_IO;
with EU.BE.I81N;
with EU.BE.Results.IO;
with EU.BE.Results;
with EU.Web.Settings;

with EU_Logger;

with Text_Utils;
with Utils;

package body EU.BE.Model.Runner is

   package osl renames Gnat.OS_Lib;
   use Ada.Text_IO;
   use Ada.Exceptions;
   use Ada.Strings.Unbounded;
   use EU_Logger;
   
   procedure Log( ctl : Model_Settings; message : String ) is
   begin
      EU_Logger.Log( EU_Logger.runner, ctl.run_id & " : " & ctl.base_users_directory & " : " & message );
   end Log;

   procedure Log( target_file : String; message : String ) is
   begin
      EU_Logger.Log( EU_Logger.runner, target_file & " : " & message );
   end Log;
   
   
   function Get_New_Run_Id return String is
   begin
      return Text_Utils.Censor_String( "run_" & Ada.Calendar.Formatting.Image( Ada.Calendar.Clock ) & "_run" );
   end Get_New_Run_Id;
 
   procedure Read_Log_File( log_file_name : String; state : in out State_Rec ) is
      use Text_Utils;
      use Ada.Directories;
   begin
      state.health := normal;
      if( Exists( Log_file_name ))then
         state.message := Read_Whole_File( Log_file_name );
         if( Index( state.message, "Error" ) > 0 )then
            -- if( Length( state.message ) > 0 )then
            state.health := in_error;
            state.phase := mefisto_complete;
         end if;
      end if;
   end Read_Log_File;
   
   function To_String( r : Run_Results_Record ) return String is
   use Text_Utils;
      s : Unbounded_String;
   begin
      s := s & "Time : " & Ada.Calendar.Formatting.Image( r.date ) & LINE_BREAK;
      s := s & "Success: " & Boolean'Image( r.success ) & LINE_BREAK;
      s := s & "Run Number: " & r.run_number & LINE_BREAK;
      return TS( s );
   end To_String;
   
   function Compare_Run_Results( r1, r2 : Run_Results_Record ) return boolean is
   use Ada.Calendar;
   begin
      return ( r1.date < r2.date );
   end Compare_Run_Results;
   
   package Job_Sorter is new Run_Results_Package.Generic_Sorting( "<" => Compare_Run_Results ); 
   
   
   function Get_Previous_Runs( ctl : in Model_Settings ) return Run_Results_List is
   use Ada.Directories;
   use Ada.Text_IO;
   use Text_Utils;

      l : Run_Results_List;
      local_ctl : Model_Settings := Ctl.copy;
      
      procedure Find_Entry( directory_entry : Directory_Entry_Type ) is
         file  : File_Type;
         basename    : constant String := Base_Name( Simple_Name( directory_entry ));
         fullname    : constant String := Full_Name( directory_entry );
         r           : Run_Results_Record;
      begin
         local_ctl.Set_Run_Id( basename );
         Put_line( "looking for logfile |" & ctl.Qualified_Log_File_Name & "|" );
         r.date := Modification_Time( fullname );
         r.directory := TuS( basename );
         Read_Log_File( ctl.Qualified_Log_File_Name, r.state );
         if( r.state.health = normal )then
           if( Exists( local_ctl.Qualified_Monitor_File_Name ))then
               Read_EM_Monitor_File( local_ctl.Qualified_Monitor_File_Name, r.state );
            end if;
         end if;
         Put_Line( "basename |" & basename & 
            "| Qualified_Monitor_File_Name " & local_ctl.Qualified_Monitor_File_Name & 
            "| local_ctl.Qualified_Log_Name=|" &  local_ctl.Qualified_Log_File_Name );
         r.run_number :=TuS( basename );
         r.success := ( r.state.health = normal ) and ( r.state.phase = mefisto_complete );
         l.Append( r );
      exception
         when others => Put_Line( "failed to open " & basename );
      end Find_Entry;
      
   begin
      Put_Line( "Past_Runs: looking for dir |" & local_ctl.Qualified_Users_Directory & "|" );
      if( not Exists( local_ctl.Qualified_Users_Directory ))then
         return l;
      end if;
      Search( local_ctl.Qualified_Users_Directory, "run_*", ( Directory => True, others => False ), Find_Entry'Access );
      Job_Sorter.Sort( l );
      return l;
      exception
         when Ada.IO_Exceptions.Name_Error => null; -- don't care if there's no such directory.
   end Get_Previous_Runs;
   
   function Last_Successful_Run( run_results : Run_Results_List ) return Natural is
      l : Natural := Natural( run_results.length );
   begin
      for i in reverse 1 .. l loop
         if( run_results.Element( i ).success )then
            return i;
         end if;
      end loop;
      return 0;
   end Last_Successful_Run;

   procedure Write_EM_Monitor_File( file_name : String; state : State_Rec ) is
      use Ada.Text_IO;
      package AIO is new Ada.Text_IO.Integer_IO ( Natural );
      f  : File_Type;
   begin
      Create( f , Out_File, file_name );
      Put_Line( f, "Step HHCount Module" );
      AIO.Put( f, Phase_Type'Pos( state.phase ) );
      AIO.Put( f, state.household );
      AIO.Put( f, state.module );
      New_Line( f );
      Close( f );
   end  Write_EM_Monitor_File;

   procedure Load_Output( ctl : in Model_Settings; output : in out Outputs_Rec ) is
   use EU.BE.Results;
      hh_level_results_post       : Detailed_Results_List;
      personal_level_results_post : Detailed_Results_List;
      
   begin
      Log( ctl, "Load_Output entered" );
      EU.BE.Results.IO.Read_To_List( 
         ctl.Qualified_Output_File_Name, 
         personal_level_results_post, 
         hh_level_results_post );
      Log( ctl, "EU.BE.Results.IO.Read_To_List OK" );
      Initialise_Outputs( output );
      Log( ctl, "Initialise_Outputs OK" );
      
      Create_Outputs( 
         EU.BE.Globals.Get_Households,
         EU.BE.Globals.Get_HH_Level_List_Pre, 
         hh_level_results_post, 
         EU.BE.Globals.Get_Personal_Level_Results_Pre,
         personal_level_results_post,
         ctl,
         output );
      Log( ctl, "Create_Outputs OK" );
      if( ctl.Create_Zip_File )then
         Log( ctl, "Creating Zip File |" & ctl.Qualified_Output_Directory & "run_output.zip|" );
         Log( ctl, "of directory |" & ctl.Qualified_Run_Directory & "|" );
         Utils.Zip_Directory( ctl.Qualified_Output_Directory, ctl.Qualified_Output_Directory & "run_output.zip" );
      end if;
      Log( ctl, "run output ended" );
   end Load_Output;
   
   procedure Run_Direct( ctl : in Model_Settings; output : in out Outputs_Rec; state : out State_Rec ) is 
      use System;
      use Ada.Strings.Unbounded;
      use Ada.Text_IO;
      use Gnat.OS_Lib;
      use EU.BE.Results;
      args                        : osl.Argument_List( 1 .. 1 );
      success                     : Boolean := False;
      sep                         : constant String := EU.Web.Settings.Dir_Separator;
      report_file_name            : constant String := ctl.Qualified_Output_Directory & SEP & "eu_dump.txt";
   begin
      args( 1 ) := new String'( ctl.Qualified_Main_Configuration_File_Name );
      Log( ctl, "set args 1 to " & ctl.Qualified_Main_Configuration_File_Name );
      Log( ctl, "script is " & ctl.invocation_script );
      Log( ctl, "logfile is " & ctl.Qualified_Log_File_Name );
      state.health := normal;
      osl.Spawn( ctl.invocation_script, 
                 args, 
                 report_file_name, 
                 success, 
                 state.error_code );
      Log( EU_Logger.runner, "spawned; success = " & Boolean'Image( success ) & " error code " & Integer'Image( state.error_code ));
      if( not success ) and then ( state.error_code = 0 ) then
         state.error_code := -1;
      end if;
      if( state.error_code < 0 )then
         state.health := in_error;
      end if;
      Free( args( 1 ));
      Log( ctl, "state.error_code = " & Integer'Image( state.error_code ) & " health " & state.health'Img );
		state.message := Text_Utils.Read_Whole_File( report_file_name );
      if( state.health /= in_error )then
         Log( ctl, "starting to run output" );
         state.household := 0;
         state.module := 0;
         state.phase := mefisto_output;
         Write_EM_Monitor_File( ctl.Qualified_Monitor_File_Name, state );
         Load_Output( ctl, output );
         Log( ctl, "returned from output" );
         -- Ada.Directories.Delete_File( report_file_name );
         state.phase := mefisto_complete;
      else
      	Log( ctl, "output not produced; run failed with message " & Text_Utils.TS( state.message ));
      end if;
      Log( ctl, "returning from Run_Direct; state is " & state.phase'Img );
      Write_EM_Monitor_File( ctl.Qualified_Monitor_File_Name, state );
   end Run_Direct;
   
   procedure Read_EM_Monitor_File( file_name : String; state : in out State_Rec ) is
      use Ada.Text_IO;
      package AIO is new Ada.Text_IO.Integer_IO ( Natural );
      f  : File_Type;
      s  : String( 1..120 );
      i, last  : Natural;
   begin
      state.monitor_read_error := False;
      Open( f , In_File, file_name, "shared=yes" );
      Get_Line( f, s, last );
      if( s( 1 .. last ) = "Step HHCount Module" )then
         AIO.Get( f, i );
         state.phase := Phase_Type'Val( i );
         AIO.Get( f, state.household );
         AIO.Get( f, state.module );
         state.error_code := 0;
      else
         Log( file_name, "header line read wrong was " & s );
         state.error_code := -1;
      end if;
      Close( f );
      -- Log( file_name, "returning normally with module " & state.module'Img & " household " & state.household'Img );
      exception
      when exp : Ada.IO_Exceptions.Use_Error =>
         Log( file_name, "Read_EM_Monitor_File exception: use error " & Exception_Information( exp ));
         state.monitor_read_error := True;
      when exp : ADA.IO_EXCEPTIONS.END_ERROR =>    
         Log( file_name, "Read_EM_Monitor_File exception end error; attempting close " & Exception_Information( exp ));
         state.monitor_read_error := True;
         Close( f );
      when exp : others =>
         Log( file_name, "Read_EM_Monitor_File exception other error " & Exception_Information( exp ));
         state.monitor_read_error := True;
   end  Read_EM_Monitor_File;
   
   procedure Monitor( ctl : in Model_Settings; state : in out State_Rec ) is
      use Text_Utils;
      use Ada.Directories;
      MAX_TRIES : constant := 5;
      tries : Natural := 0;
      backup : State_Rec := state;
   begin
      -- Log( ctl, "monitor entering; looking for file " & ctl.Qualified_Monitor_File_Name );
      Read_Log_File( ctl.Qualified_Log_File_Name, state );
      if( state.health = normal )then
          loop
            if( Exists( ctl.Qualified_Monitor_File_Name ))then
               -- Log( ctl, "monitor exists" );
               Read_EM_Monitor_File( ctl.Qualified_Monitor_File_Name, state );
               -- Log( ctl, "Read_EM_Monitor_File " & 
               --         state.error_code'Img & 
               --          "state.monitor_read_error=" & 
               --          state.monitor_read_error'Img );
               if( state.error_code >= 0 ) and ( not state.monitor_read_error )then
                  -- Log( ctl, "Monitor: read ok; returning with state phase" & state.phase'Img );
                  exit;
               end if;
            else
               Log( ctl, "Monitor doesn't exist after " & tries'Img & " tries "  );
            end if;
            delay 0.1;
            tries := tries + 1;
            if( tries = MAX_TRIES )then
               state.health := in_error;
               state.message := TuS( "Reading monitor file failed after " & Natural'Image( MAX_TRIES ) & " attempts" );
               exit;
            end if;
         end loop;
         if( state.health = in_error )then
            backup.message := state.message;
            backup.health := state.health;
            backup.error_code := state.error_code;
            state := backup;
         end if;
      end if;
   end Monitor;
   
end EU.BE.Model.Runner;
