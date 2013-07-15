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

--
-- Tasking version of the Miniserver backend, unsing a single execution task
-- which executes a single blocking run evocation at a time.
-- There's also a much simpler non-blocking version which works well on Linux but hangs in Windows.
-- Uses Ada tasking and a protected object which you can read about here:
-- http://en.wikibooks.org/wiki/Ada_Programming/Tasking
--
package body EU.BE.Mini_Runner is
  
   use Ada.Strings.Unbounded;
   use Ada.Text_IO;
   use Text_Utils;
   use Ada.Calendar;
   
   package osl renames Gnat.OS_Lib;
   package runner renames EU.BE.Model.Runner;
   package expect renames Gnat.Expect;

   default_settings : Model_Settings;
   
   MAX_RESERVED_TIME : constant Duration := 60.0;
   locked          : Boolean := False;
   reserved_from   : Time := DEFAULT_TIME;
   
   protected body Job_Submission_Type is   

      function Get_State return State_Rec is
      begin
         return state;
      end Get_State;
      
      procedure Mark_Job_End is
      begin
         server_state := server_free;
      end Mark_Job_End;
      
      
      procedure Set_State( new_state : State_Rec ) is
      begin
         state := new_state;
      end Set_State;
      
      function Get_Settings return Model_Settings is
      begin
         return settings;
      end Get_Settings;
      
      entry Seize when not locked is
      begin
         locked := True;
      end Seize;
      
      procedure Release is
      begin
         locked := False;
      end Release;
      
      procedure Update_Settings( new_settings : Model_Settings ) is
      begin
         settings := new_settings;
      end Update_Settings;
      
      procedure Enque( run_settings : Model_Settings; run_state : State_Rec ) is
      begin
          settings := run_settings;
          state := run_state;
          server_state := job_needs_done;
      end Enque;
     
      procedure Deque( run_settings : out Model_Settings; run_state : out State_Rec ) is
      begin
          run_settings := settings;
          run_state := state;
          server_state := job_is_running;
      end Deque;

      function Get_Server_State return Server_State_Type is
      begin
         return server_state;
      end Get_Server_State;
     
   end Job_Submission_Type;
   
   job_submission : Job_Submission_Type;
   
   --
   -- Reserving a miniserver: if a minserver is marked as free but the job hasn't been
   -- submitted yet, 
   --
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
      settings           : constant Model_Settings := job_submission.Get_Settings;
      sep                : constant String := EU.Web.Settings.Dir_Separator; 
      report_file_name   : constant String := settings.Qualified_Output_Directory & SEP & "eu_dump.txt";
      state              : State_Rec := job_submission.Get_State;
   begin
      -- state.pid := Integer( proc_descriptor.Get_Pid );
      -- expect ..    
      case job_submission.Get_Server_State is
      when job_is_running =>
         runner.Monitor( settings, state );
         case state.phase is
         when not_started => null;
         when starting .. eu_output =>
            if( state.start_time > DEFAULT_TIME )then
               state.execution_time := Ada.Calendar.Clock - state.start_time;
            end if;
         when eu_run_complete .. mefisto_complete =>
            -- server_state := server_free;
            job_submission.Mark_Job_End;
         end case;
      when job_needs_done =>
         state.phase := not_started;
      when server_free =>
         null;
      end case;
      job_submission.Set_State( state );
   end  Update_State;

   procedure Run_Direct( settings : Model_Settings; state : in out State_Rec ) is 
   use System;
   use Gnat.OS_Lib;
      args : Argument_List( 1 .. 1 );
      success : Boolean;
      sep                         : constant String := EU.Web.Settings.Dir_Separator;
      report_file_name            : constant String := settings.Qualified_Output_Directory & SEP & "eu_dump.txt";
   begin
      args( 1 ) := new String'( settings.Qualified_Main_Configuration_File_Name );
      Put_Line( "Run_Direct " & "script is " & settings.invocation_script & " config is |" & settings.Qualified_Main_Configuration_File_Name & "| ");
      state.health := normal;
      state.phase := starting;
      Spawn(
         Program_Name => settings.invocation_script, 
         Args         => args, 
         Output_File  => report_file_name,
         Return_Code  => state.pid,
         Success      => success,
         Err_to_Out   => True );
      -- delay 1.0;
      if( state.pid = -1 )then 
         state.error_code := -1;
         state.health := in_error;
      else
         state.household := 0;
         state.module := 0;
         state.phase := eu_run_complete;
      end if;
      Put_Line(  "Run Direct " & " spawned; pid = " & state.pid'Img & " error code " & Integer'Image( state.error_code ));
      Free( args( 1 ));
      Put_Line( "Run Direct " & " state.error_code = " & Integer'Image( state.error_code ) & " health " & state.health'Img );
   end Run_Direct;

   procedure Update_Settings_From_Request( request : in AWS.Status.Data ) is
      params : constant AWS.Parameters.List := AWS.Status.Parameters( request );
      user_id_str  : String := AWS.Parameters.Get( params, "uid" );
      run_id_str   : String := AWS.Parameters.Get( params, "rid" );
      settings       : Model_Settings := job_submission.Get_Settings;
   begin
      Put_Line( "Update_Settings_From_Request got user_id as |" & user_id_str & "| rid=|" & run_id_str & "| ");
      settings.Set_Run_Id( run_id_str  );
      settings.Set_Users_Directory( user_id_str  );
      job_submission.Update_Settings( settings );
      Put_Line( "Update_Settings_From_Request exiting OK ");
   end Update_Settings_From_Request;
   
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
      free : constant Boolean := job_submission.Get_Server_State = server_free and not Is_Reserved;
      free_img : constant String := Censor_String( Boolean'Image( free ));
   begin
      Put_Line( "Free check server " & port'Img & " server_state = " & job_submission.Get_Server_State'Img );
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
      if( job_submission.Get_Server_State = server_free )then
         return AWS.Response.Build( "text/plain", NOT_RUNNING_STR );
      end if;
      if( locked )then
         return AWS.Response.Build( "text/plain", LOCKED_STR );
      end if;
      Put_Line( "Halt_Callback; port " & port'Img  );
      locked := True;
      Update_Settings_From_Request( request );
      job_submission.Mark_Job_End;
      -- expect.Close( proc_descriptor, state.exit_state );
      -- proc_descriptor := default_descriptor;
      Update_State;
      state_string := State_To_Parameters( job_submission.Get_State );
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
      Update_Settings_From_Request( request );
      Update_State;
      state_string := State_To_Parameters( job_submission.Get_State );
      locked := False;         
      Put_Line( "Status_Callback; port " & port'Img & " unlocked " );
      return AWS.Response.Build( "text/plain", state_string ); 
   end Status_Callback;
   
   function Run_Callback( request : in AWS.Status.Data ) return AWS.Response.Data is
      state_string : Unbounded_String := TuS( "RUN" );
      state : State_Rec := job_submission.Get_State;
      settings : Model_Settings;
   begin
      if( locked )then
         return AWS.Response.Build( "text/plain", LOCKED_STR );
      end if;
      locked := True;
      Put_Line( "Run_Callback; port " & port'Img & " pre-initial update; pid is now = " & state.pid'Img & " phase is " & state.phase'Img );
      Update_State;
      Put_Line( "Run_Callback; ; port " & port'Img & " post-initial update; pid is now = " & state.pid'Img & " phase is " & state.phase'Img );
      case job_submission.Get_Server_State is
      when job_is_running | job_needs_done =>      
         state_string := State_To_Parameters( state ) & "&already_running=True";
      when server_free =>
         Put_Line( "Run_Callback; Nothing Running; starting run." );
         settings := default_settings;
         Update_Settings_From_Request( request );
         state := NULL_STATE_REC;
         state.phase:= starting;
         state.start_time := Clock;
         state.execution_time := 0.0;
         Put_Line( "Converting state to string." );
         state_string := State_To_Parameters( state ) & "&already_running=False";
         Put_Line( "Made State String as " & TS( state_string ));
         Put_Line( "Enquing job." );
         job_submission.Enque( settings, state );
      end case;
      locked := False;
      Put_Line( "Run_Callback; ; port " & port'Img & " unlocked; pid is now = " & state.pid'Img & " phase is " & state.phase'Img );
      return AWS.Response.Build( "text/plain", state_string ); 
   end Run_Callback;

   procedure Initialise_Settings is
   begin
      Put_Line( "Initialise Settings entered " );
      if( Ada.Command_Line.Argument_Count > 1 ) then
         default_settings := Read_Model_Settings( Ada.Command_Line.Argument( 1 ));
         EU.Web.Settings.Read_Web_Settings( Ada.Command_Line.Argument( 2 ));
         default_settings.Set_Dir_Separator( EU.Web.Settings.Dir_Separator( 1 )); 
         job_submission.Update_Settings( default_settings );
         port := Positive'Value( Ada.Command_Line.Argument( 3 ));
         Put_Line( "reading parameters OK; sep is " &  EU.Web.Settings.Dir_Separator( 1 ));
      else
         Put_Line( "Call with <default model settings> <web settings> <port>" );
      end if;
   end Initialise_Settings;
   
   --
   -- This creates a a single runner task which polls for a 
   -- queued job and executes it directly when it finds one.
   -- 
   task type Model_Runner_Task_Type is
   
   end Model_Runner_Task_Type;
   
   task body Model_Runner_Task_Type is
    
   begin
      loop
         delay 1.0;
         if job_submission.Get_Server_State = job_needs_done then
            declare
               settings : Model_Settings;
               state     : State_Rec;
            begin
               job_submission.Deque( settings, state );
               Put_Line( "TASK LOOP; RUN DEQUED" );
               Run_Direct( settings, state );
               job_submission.Set_State( state );
               job_submission.Mark_Job_End;
               Put_Line( "TASK LOOP; JOB COMPLETED" );
            end;
         end if;
      end loop;
   end Model_Runner_Task_Type;
   
   model_runner_task : Model_Runner_Task_Type;
   
end EU.BE.Mini_Runner;
