with Ada.Calendar;
with Ada.Calendar.Formatting;

with Ada.Command_Line;
with Ada.Directories;
with Ada.IO_Exceptions;
with Ada.Strings.Unbounded;
with Ada.Characters.Handling;
with Ada.Text_IO;
with Gnat.Expect;

with Gnat.OS_Lib;
with System;

with AWS.Parameters;

with EU.BE.Model.Runner;
with EU.Web.Settings;

with Text_Utils;
with Web_Utils;

package body EU.BE.Mini_Runner is
  
   use Ada.Strings.Unbounded;
   use Ada.Text_IO;
   use Text_Utils;
   use Ada.Calendar;
   
   package osl renames Gnat.OS_Lib;
   package runner renames EU.BE.Model.Runner;
   package expect renames Gnat.Expect;

   default_settings : Model_Settings;
   default_descriptor : expect.Process_Descriptor;

   MAX_RESERVED_TIME : constant Duration := 60.0;
   
   settings        : Model_Settings;
   locked          : Boolean := False;
   proc_descriptor : expect.Process_Descriptor;
   state           : State_Rec := NULL_STATE_REC;
   reserved_from   : Time := DEFAULT_TIME;
   
   procedure Reserve is
   begin
      reserved_from := Clock;
   end Reserve;

   procedure Unreserve is
   begin
      reserved_from := Default_Time;
   end unreserve;

   function Is_Reserved return Boolean is
      how_long_reserved : Duration;
   begin
      if( reserved_from = Default_Time )then
         return False;
      end if;
      how_long_reserved := Clock - reserved_from;
      Put_Line( "Is_Reserved how long = " & how_long_reserved'Img & " reserved_from " & Ada.Calendar.Formatting.Image( reserved_from )); 
      if( how_long_reserved > MAX_RESERVED_TIME )then
         Unreserve;
         Put_Line( "Is_Reserved; unreserving" );
         return False;
      else
         return True;
      end if;
   end Is_Reserved;
   
   function Make_URL( 
      server_name : String;
      port        : Positive;
      action      : Action_Type;
      user        : String := "";
      run         : String := "" ) return String is
   use Ada.Characters.Handling;
      pstr : constant String := Positive'Image( port )( 2 .. Positive'Image( port )'Last );
      url : Unbounded_String := TuS( "http://" & server_name & ":" & pstr & "/" & To_Lower( action'Img ));
   begin
      if( action in Full_Queries )then
         url := url & "?rid=" & run & "&uid=" & user; 
      end if;
      return TS( url );
   end Make_URL;
   
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
   
   function Job_Is_Running return Boolean is
   use type expect.Process_Id;
   begin
      return state.phase in currency .. eu_output or state.pid > 0;
   end Job_Is_Running;
   
   procedure Record_Death( 
      descriptor : expect.Process_Descriptor'Class;
      str        : String ) is
   begin
      state.pid := 0;
   end Record_Death;
   
   procedure Run_Direct is 
      use System;
      use Gnat.OS_Lib;
      use type Process_Id;
      
      args : Argument_List( 1 .. 1 );
   begin
      args( 1 ) := new String'( settings.Qualified_Main_Configuration_File_Name );
      Put_Line( "Run_Direct port " & port'Img & "script is " & settings.invocation_script & " config is |" & settings.Qualified_Main_Configuration_File_Name & "| ");
      state.health := normal;
      state.phase := starting;
      expect.Non_Blocking_Spawn(
         descriptor  => proc_descriptor,
         command     => settings.invocation_script, 
         args        => args, 
         buffer_size => 40960,
         err_to_out  => True );
         state.start_time := Ada.Calendar.Clock;
      -- delay 1.0;
      Update_State;
      if( state.pid = -1 )then 
         state.error_code := -1;
         state.health := in_error;
      end if;
      Put_Line(  "Run Direct  port " & port'Img & " spawned; pid = " & state.pid'Img & " error code " & Integer'Image( state.error_code ));
      Free( args( 1 ));
      Put_Line( "Run Direct  port " & port'Img & " state.error_code = " & Integer'Image( state.error_code ) & " health " & state.health'Img );
   end Run_Direct;

   procedure Make_Settings_From_Request( request : in AWS.Status.Data ) is
      params : constant AWS.Parameters.List := AWS.Status.Parameters (Request);
      user_id_str  : String := AWS.Parameters.Get( params, "uid" );
      run_id_str   : String := AWS.Parameters.Get( params, "rid" );
   begin
      Put_Line( "Make_Settings_From_Request got user_id as |" & user_id_str & "| rid=|" & run_id_str & "| ");
      settings.Set_Run_Id( run_id_str  );
      settings.Set_Users_Directory( user_id_str  );
   end Make_Settings_From_Request;
   
   function Alive_Callback( request : in AWS.Status.Data ) return AWS.Response.Data is
   begin
   	return AWS.Response.Build( "text/plain", ALIVE_STR ); 
   end Alive_Callback;
   
   function Reserve_Callback( request : in AWS.Status.Data ) return AWS.Response.Data is
      ok : Boolean := False;
   begin
      Put_Line( "Reserving server " & port'Img );
      if( not Is_Reserved )then
         Reserve;
         ok := True;
      end if;
   	return AWS.Response.Build( "text/plain", Censor_String( Boolean'Image( ok ))); 
   end Reserve_Callback;
   
   function Unreserve_Callback( request : in AWS.Status.Data ) return AWS.Response.Data is
   begin
      Put_Line( "Unreserving server " & port'Img );
      Unreserve;
   	return AWS.Response.Build( "text/plain", "true" ); 
   end Unreserve_Callback;
   
   function Free_Callback( request : in AWS.Status.Data ) return AWS.Response.Data is
      free : constant Boolean := not Job_Is_Running and not Is_Reserved;
      free_img : constant String := Censor_String( Boolean'Image( free ));
   begin
      Put_Line( "Free check server " & port'Img & " Job_Is_Running = " & Job_Is_Running'Img );
      Put_Line( "Free check server " & port'Img & " Is_Reserved = " & Is_Reserved'Img );
      Put_Line( "Free check server " & port'Img & " free = " & free'Img );
      
      if( free )then
         Reserve;
      end if;
   	return AWS.Response.Build( "text/plain", free_img ); 
   end Free_Callback;

   function Halt_Callback( request : in AWS.Status.Data ) return AWS.Response.Data is
      state_string : Unbounded_String := TuS( "STAT" );
   begin
      Update_State;
      if( not Job_Is_Running )then
         return AWS.Response.Build( "text/plain", NOT_RUNNING_STR );
      end if;
      if( locked )then
         return AWS.Response.Build( "text/plain", LOCKED_STR );
      end if;
      Put_Line( "Halt_Callback; port " & port'Img & " pid to kill " & state.pid'Img );
      locked := True;
      Make_Settings_From_Request( request );
      expect.Close( proc_descriptor, state.exit_state );
      proc_descriptor := default_descriptor;
      Update_State;
      state_string := State_To_Parameters( state );
      locked := False;
      return AWS.Response.Build( "text/plain", state_string ); 
   end Halt_Callback;
   
   function Status_Callback( request : in AWS.Status.Data ) return AWS.Response.Data is
      state_string : Unbounded_String := TuS( "STAT" );
   begin
      if( locked )then
         return AWS.Response.Build( "text/plain", LOCKED_STR );
      end if;
      locked := True;
      Put_Line( "Status_Callback; port " & port'Img & " locked" );
      Make_Settings_From_Request( request );
      Update_State;
      state_string := State_To_Parameters( state );
      locked := False;         
      Put_Line( "Status_Callback; port " & port'Img & " unlocked " );
      return AWS.Response.Build( "text/plain", state_string ); 
   end Status_Callback;
   
   function Run_Callback( request : in AWS.Status.Data ) return AWS.Response.Data is
      state_string : Unbounded_String := TuS( "RUN" );
   begin
      if( locked )then
         return AWS.Response.Build( "text/plain", LOCKED_STR );
      end if;
      locked := True;
      Put_Line( "Run_Callback; port " & port'Img & " pre-initial update; pid is now = " & state.pid'Img & " phase is " & state.phase'Img );
      Update_State;
      Put_Line( "Run_Callback; ; port " & port'Img & " post-initial update; pid is now = " & state.pid'Img & " phase is " & state.phase'Img );
      Make_Settings_From_Request( request );
      if( Job_Is_Running )then
         state_string := State_To_Parameters( state ) & "&already_running=True";
      else
         Put_Line( "Run_Callback; Nothing Running; starting run " );
         settings := default_settings;
         Make_Settings_From_Request( request );
         state := NULL_STATE_REC;
         state.phase:= starting;
         Run_Direct;
         Update_State;
         state_string := State_To_Parameters( state ) & "&already_running=False";
      end if;
      locked := False;
      Put_Line( "Run_Callback; ; port " & port'Img & " unlocked; pid is now = " & state.pid'Img & " phase is " & state.phase'Img );
      return AWS.Response.Build( "text/plain", state_string ); 
   end Run_Callback;

   procedure Initialise_Settings is
   begin
      Put_Line( "Add_All_Targets globals entered " );
      if( Ada.Command_Line.Argument_Count > 1 ) then
         default_settings := Read_Model_Settings( Ada.Command_Line.Argument( 1 ));
         EU.Web.Settings.Read_Web_Settings( Ada.Command_Line.Argument( 2 ));
         default_settings.Set_Dir_Separator( EU.Web.Settings.Dir_Separator( 1 )); 
         settings := default_settings;
         port := Positive'Value( Ada.Command_Line.Argument( 3 ));
         Put_Line( "reading parameters OK; sep is " &  EU.Web.Settings.Dir_Separator( 1 ));
      else
         Put_Line( "Call with <default model settings> <web settings> <port>" );
      end if;
   end Initialise_Settings;

end EU.BE.Mini_Runner;
