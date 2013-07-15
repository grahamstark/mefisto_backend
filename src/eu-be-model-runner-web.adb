with AWS.Client;
with AWS.URL;
with AWS.Response;

with Ada.Strings.Unbounded;
with Ada.Text_IO;

with EU.BE.Globals;
with EU.BE.Mini_Runner;
with EU.BE.Output;
with EU.BE.Parameter_System_Declarations;

with EU.Web.Settings;

with EU_Logger;

with Text_Utils;
with Utils;
--
-- FIXME abort somehow
--

package body EU.BE.Model.Runner.Web is

   use Ada.Strings.Unbounded;
   use Text_Utils;
   use Ada.Text_IO;
   
   package Output_Session       renames globals.Output_Session_Package;
   package Run_State_Session    renames globals.Run_State_Session_Package;
   package Run_Settings_Session renames globals.Run_Settings_Session_Package;
   package Buffer_Session       renames globals.Buffer_Session_Package;

   package globals renames EU.BE.Globals;
   package mini    renames EU.BE.Mini_Runner;

   procedure Log( s : String ) is
   begin
      EU_Logger.Log( EU_Logger.queueing, s );
   end Log;
   
   procedure Unreserve( port : Positive ) is
   use AWS.Client;
   use AWS.URL;
   use AWS.Response;
      url : constant String := mini.Make_URL(
         EU.Web.Settings.Mini_URL,
         port,
         mini.unreserve );
      data : AWS.Response.Data;
   begin
      Log( "unreserving; url is " & url );
      data := AWS.Client.Get( url );
      Log( "unreserving; response is " & Message_Body( data ));
   end Unreserve;
   
   
   procedure Run_Miniserver( 
      ctl : in Model_Settings; 
      output : in out Outputs_Rec; 
      state : out State_Rec; 
      port : Positive;
      session_id : AWS.Session.Id ) is 
   use AWS.Client;
   use AWS.URL;
   use AWS.Response;
   use Ada.Calendar;
      MAX_MONITOR_CALLS : constant := 100;
      run_url : constant String := mini.Make_URL(
         EU.Web.Settings.Mini_URL,
         port,
         mini.run,
         ctl.Base_Users_Directory,
         ctl.Run_Id );
      status_url : constant String := mini.Make_URL(
         EU.Web.Settings.Mini_URL,
         port,
         mini.status,
         ctl.Base_Users_Directory,
         ctl.Run_Id );
      halt_url : constant String := mini.Make_URL(
         EU.Web.Settings.Mini_URL,
         port,
         mini.halt,
         ctl.Base_Users_Directory,
         ctl.Run_Id );
      data : AWS.Response.Data;
      message : Unbounded_String;
      elapsed : Duration;
      tries   : Natural := 0;      
   begin
   		if( ctl.Run_Id = "" or ctl.Base_Users_Directory = "" )then
   			return;
   		end if;
      Log( "ctl.Base_Users_Directory |" & ctl.Base_Users_Directory & "| ctl.Run_Id |" & ctl.Run_Id );
      Log( "Run_Miniserver; run_url=|" & run_url & "| " );
      Log( "Run_Miniserver; status_url=|" & status_url & "| " );
      Log( "Run_Miniserver; halt_url=|" & halt_url & "| " );
      data := AWS.Client.Get( run_url );
      message := Message_Body( data );
      Log( "Run_Miniserver; got run message as " & TS( message ));
      state := Load_State_From_URL( message );
      Log( "after run submission; got initial state as " & TS( State_To_Parameters( state )));
      Run_State_Session.Set( session_id, globals.SESSION_RUN_STATE, state );   

      loop
         Log( "Entering monitoring loop; tries = " & tries'Img & " port " & port'Img );
         data := AWS.Client.Get( status_url );
         message := Message_Body( data );
         Log( "monitor; got monitor message as " & TS( message ) & " tries = " & tries'Img );
         state := Load_State_From_URL( message );
         Run_State_Session.Set( session_id, globals.SESSION_RUN_STATE, state );   
         Log( "monitor; made state as " & TS( State_To_Parameters( state )));
         tries := tries + 1;
         delay 2.0;
         exit when tries > MAX_MONITOR_CALLS or state.phase > Running_Phase'Last;
      end loop;
            
      if( state.phase in Running_Phase )then
         Log( "error retries exceeeded" );
         data := AWS.Client.Get( halt_url );
         message := Message_Body( data );
         state := Load_State_From_URL( message );
         state.health := in_error;
         Run_State_Session.Set( session_id, globals.SESSION_RUN_STATE, state );   
      end if;
      
      if( state.health /= in_error )then
         Log(  "starting to run output" );
         state.household := 0;
         state.module := 0;
         state.phase := mefisto_output;
         Run_State_Session.Set( session_id, globals.SESSION_RUN_STATE, state );   
         Write_EM_Monitor_File( ctl.Qualified_Monitor_File_Name, state );
         Load_Output( ctl, output );
         Log(  "returned from output" );
         -- Ada.Directories.Delete_File( report_file_name );
         state.phase := mefisto_complete;
      else
      	Log( "output not produced; run failed with message " & Text_Utils.TS( state.message ));
      end if;
      Log(  "returning from Run_Miniserver; state is " & state.phase'Img );
      Write_EM_Monitor_File( ctl.Qualified_Monitor_File_Name, state );
      Run_State_Session.Set( session_id, globals.SESSION_RUN_STATE, state );   
   end Run_Miniserver;

   
   
   type Run_Entry_Type is record
      run_settings : Model_Settings;
      session_id   : AWS.Session.Id;
   end record;
   
   package Job_Queue_Package is new Ada.Containers.Vectors(
      Index_Type => Positive,
      Element_Type => Run_Entry_Type );      
   
   subtype Job_List is Job_Queue_Package.Vector;
   
   protected type Job_Queue_Type is
      
       entry Seize;
       procedure Release;
      
      function Find_Free_Server return Integer;
      procedure Deque( rc : out Run_Entry_Type );
      function Size return Natural;
      procedure Register_Worker( wc : in out Natural );
      procedure Enque( 
            session_id : AWS.Session.Id; 
            run_settings : Model_Settings );
   private      
      locked          : Boolean := False;
      queue           : Job_List;
      job_count       : Natural := 0;
      worker_count    : Natural := 0;
   end Job_Queue_Type;   
   
   protected body Job_Queue_Type is   

      entry Seize when not locked is
      begin
         locked := True;
      end Seize;
      
      procedure Release is
      begin
         locked := False;
      end Release;
      
      
      function Find_Free_Server return Integer is
      use AWS.Client;
      use AWS.URL;
      use AWS.Response;
         port : Positive := EU.Web.Settings.First_Mini_Port;
         n    : Positive := EU.Web.Settings.Num_Mini_Servers;
      begin
         for i in 1 .. n loop
            port := port + 1;
            declare
               url : constant String := mini.Make_URL(
                  EU.Web.Settings.Mini_URL,
                  port,
                  mini.free );
               data : AWS.Response.Data;
            begin
               Log( "Checking url |" & url );
               data := AWS.Client.Get( url );
               declare
                  resp : constant String := Message_Body( data );
               begin
                  Log( "got data as |" & resp & "| " ); 
                  if( resp = "true" )then
                     Log( "found free port as " & port'Img );
                     return port;
                  end if;
               end;
            end;
         end loop;
         return -1;
      end Find_Free_Server;
      
      procedure Deque( rc : out Run_Entry_Type ) is
      begin
         rc := ( Get_Null_Model_Settings, AWS.Session.No_Session );
         if( Size > 0 ) then
            rc := queue.Element( 1 );
            queue.Delete( 1 );
            job_count := job_count + 1;
         end if;
      end Deque;
      
      procedure Register_Worker( wc : in out Natural ) is
      begin
         Log( "Worker count " & Natural'Image( worker_count ));
         worker_count := worker_count + 1;
         wc := worker_count;
      end Register_Worker;
      
      function Size return Natural is 
      begin
         return Natural( queue.Length );
      end Size;
      
      procedure Enque( 
            session_id : AWS.Session.Id; 
            run_settings : Model_Settings ) is
         rc :  Run_Entry_Type := ( run_settings => run_settings, session_id=>session_id );
      begin
         Log( "enqueing job with session id |" & AWS.Session.Image( session_id ) & "|" );
         queue.Append( rc );     
      end Enque;
      
   end Job_Queue_Type;
    
   job_queue : Job_Queue_Type;
   
   procedure Submit_Run( 
      session_id : AWS.Session.Id; 
      run_settings : Model_Settings ) is
   begin
      job_queue.Enque( session_id, run_settings );
   end Submit_Run;
   
   task type Model_Runner_Task_Type is
       -- 
      -- For why this pragma:
      -- See: http://gcc.gnu.org/onlinedocs/gcc-3.3.6/gnat_rm/Storage_005fSize-Clauses.html
      -- and: http://coding.derkeiler.com/Archive/Ada/comp.lang.ada/2005-05/msg00281.html
      -- 
    -- pragma Storage_Size( Utils.Default_Stack_Size );
     -- entry Start( qno : Positive );
 
   end Model_Runner_Task_Type;
   
   task body Model_Runner_Task_Type is
      use EU.BE.Output;
      --
      -- FIXME: this is wrong because is presupposes that the user's session is
      -- still there when the job runs. We need need to store everything we need
      -- in the Regime_And_Control record and use it all from there.
      -- Not fixing it now, though.
      -- Also, we have out_dir from the session and   working_directory from the RandC record
      -- which are (almost) the same thing.
      -- 
      rc              : Run_Entry_Type; 
      worker_number   : Natural  := 0;
      miniserver_port : Integer := -1;
  begin
      job_queue.Register_Worker( worker_number );
      loop
         -- Log( "got port number as " & miniserver_port'Img );
         if( job_queue.Size > 0 ) then 
            job_queue.seize;
            miniserver_port := job_queue.Find_Free_Server;
            job_queue.Release;
            if( miniserver_port > EU.Web.Settings.First_Mini_Port ) and ( job_queue.Size > 0 )then
               Log( "job allocated to worker " & Natural'Image( worker_number ) & 
                    " server " & Integer'Image( miniserver_port ) & 
                    " Queue Size is " & job_queue.Size'Img );
               job_queue.Deque( rc );  
               declare
                  output : Outputs_Rec; 
                  state : State_Rec := Run_State_Session.Get( 
                     rc.session_id, 
                     globals.SESSION_RUN_STATE );
               begin
                  state.queued := False;
                  Log( "dequed job with id " & AWS.Session.Image( rc.session_id ));
                  state.phase := starting;
                  Run_State_Session.Set( 
                     rc.session_id, 
                     globals.SESSION_RUN_STATE, state );   
                  -- Run_Direct( rc.run_settings, output, state );  
                  Run_Miniserver( rc.run_settings, output, state, miniserver_port, rc.session_id );  
                  globals.Output_Session_Package.Set( 
                     rc.session_id, 
                     globals.SESSION_OUTPUTS, 
                     output );
                  state.phase := mefisto_complete;
                  Unreserve( miniserver_port );
                  Run_State_Session.Set( 
                     rc.session_id, 
                     globals.SESSION_RUN_STATE, state );   
               end;
               Log( Natural'Image( worker_number ) & " : job completed and queue freed"  );
            end if; -- found a free port
         end if;    -- has an available job
         miniserver_port := -1;
         delay 1.0;
      end loop; -- forever
   end Model_Runner_Task_Type;
   
   -- this declares 15 tasks available to run jobs
   subtype Running_Jobs_Range is positive Range 1 .. 15;
   type Runner_Array_Type is array( Running_Jobs_Range ) of Model_Runner_Task_Type;
   runner_array : Runner_Array_Type;
   
   function Monitor_To_HTML( state : State_Rec; lang : Languages ) return Unbounded_String is
   use EU.BE.I81N.Translations;
      MAX_MODULES      : constant := 281;
      PERSON_COUNT     : constant := 7_719.0;
      
      function Make_Modules_Table( current_module : Natural ) return Unbounded_String is
         num_modules : constant Rate := Rate( MAX_MODULES );
         s : Unbounded_String := TuS("<table width='100%' cellspacing='1'><tr class='modulebox' >" );
         class : Unbounded_String := TuS( "module_done" );
       begin  
         for m in 1 .. MAX_MODULES loop
            -- too big! show every 3rd
            if(( m mod 4 ) = 0 )then
               if( m = current_module ) or (( m + 1 ) = current_module ) or (( m + 2 ) = current_module ) or (( m + 3 ) = current_module ) then
                  s := s & "<td width='1px' class='module_active'></td>";       
                  class := TuS( "module_todo" );
               else
                  s := s & "<td width=1px' class='" & class & "'></td>";
               end if;
            end if;
         end loop;
         s := s  & "<td>" & Format( current_module ) & "&nbsp;" & Lookup( "done", lang ) & "</td></tr></table>";
         return s;
       end Make_Modules_Table;
    
       --
       -- no longer used
       --
      function Make_Household_Progress_Table( pct_done : Natural ) return Unbounded_String is
         use Text_Utils;
         
         s : Unbounded_String;
         pct_to_do : Integer;
      begin
         pct_to_do := 100 - pct_done;
         s := s &"<table width='100%' border='0' cellspacing='0'  class='progressBar'>";
         s := s &"<tr>";
         if( pct_done > 0 ) then
            s := s & "<td width='" & Format(pct_done) & "%' class='households_done'>&nbsp;</td>";
            s := s & "<td width='" & Format(pct_to_do) & "%' class='households_todo'>&nbsp;</td>";
         else
            s := s & "<td>&nbsp;</td>";
         end if;
         s := s & "</tr>";
         s := s & "</table>";
         return s;
      end Make_Household_Progress_Table;
      
      s                : Unbounded_String;
      modules_table    : Unbounded_String;
      households_table : Unbounded_String;
      pct_done         : Natural := 0;
   begin
      s := s & "<table width='290' class='statusTable' >" & LINE_BREAK;
      s := s &"     <tr>" & LINE_BREAK;
      case state.phase is
         when not_started => 
               s := s & "<tr><td align='middle'><h3>" &
               Lookup( "job_in_queue_message", lang ) &
               "</h3></td></tr>" & LINE_BREAK;
         when starting | currency => 
               s := s & "<tr><td align='middle'><h3>" &
               Lookup( "em_starting_message", lang ) &
               "</h3></td></tr>" & LINE_BREAK;
         when households | non_households => 
              pct_done := Natural( 100.0 * Amount( state.household )/PERSON_COUNT );
              modules_table := Make_Modules_Table( state.module );
              -- Log( " make modules table as " & TS( modules_table ) & " module no " & state.module'Img );
              households_table := Make_Household_Progress_Table( pct_done );
              s := s &"<tr><th>" & Lookup( "Module", lang ) & 
              "</th></tr>" & LINE_BREAK;
              -- <th colspan='2'>" & Lookup( "Households Processed", lang ) & "</th>
              s := s & "<tr>" & LINE_BREAK;
              s := s & "<td >" & modules_table & "</td>";
              -- s := s &"<td width='49%'>" & households_table & "</td>";
              -- s := s & "<td width='16%'>" & Web_Format( state.household, lang ) & " (";
              -- s := s & Web_Format( pct_done, lang ) & "%) " & Lookup( "completed", lang ) & "</td>";
              s := s & "</tr>" & LINE_BREAK;
         when eu_output | eu_run_complete => 
               s := s & "<tr><td align='middle'><h3>" &
               Lookup( "em_output_message", lang ) &
               "</h3></td></tr>" & LINE_BREAK;
               pct_done := 100;
         when mefisto_output => 
               s := s & "<tr><td align='middle'><h3>" &
               Lookup( "mf_output_message", lang ) &
               "</h3></td></tr>" & LINE_BREAK;
               pct_done := 100;
         when mefisto_complete => 
               s := s & "<tr><td align='middle'><h3>" &
               Lookup( "job_complete_message", lang ) &
               "</h3></td></tr>" & LINE_BREAK;
               pct_done := 100;
      end case;
      s := s &"     </tr>" & LINE_BREAK;
      s := s &"</table>" & LINE_BREAK;
      if( state.health = in_error ) or ( state.monitor_read_error ) or ( state.error_code < 0 )then
         s := s & "<div class='error'>" & 
              Lookup( "Error encountered", lang ) & 
              "<pre>" & state.message & "</pre>" &
              
              "<p>state.health : <b>" & 
              state.health'Img & "</b></p><p> state.monitor_read_error: <b>" &  state.monitor_read_error'Img & 
              "</b></p><p>state.error_code: <b>" & state.error_code'Img & "</b></p></div>" & LINE_BREAK;
      end if;
      return s;
   end  Monitor_To_HTML;  
   
   function Get_State_Of_Run_As_HTML( 
      run_state : State_Rec;
      lang      : Languages ) return Unbounded_String is
      s : Unbounded_String;
   begin
      -- 
      -- 
      s := Monitor_To_HTML( run_state, lang );
      if( run_state.health = in_error )then
         s := s & "<script>" & LINE_BREAK; 
         s := s & "      clearTimeout( PeriodicalTimer );" & LINE_BREAK;
         s := s & "      $( '#run_submit_button' ).disabled=false;" & LINE_BREAK;
         s := s & "</script>" & LINE_BREAK;
      elsif( run_state.phase in Running_Phase )then
         s := s & "<script>" & LINE_BREAK; 
         -- send back javascript to disable the output link while running.
         s := s & "      $( '#modelNavTop' ).tabs('option','disabled', [3]);" & LINE_BREAK;
         s := s & "</script>" & LINE_BREAK;
      elsif( run_state.phase = mefisto_complete )then
         Log( "complete; sending switch off code " );
         --
         -- code to stop the update callback then switch focus to the output page when using jquery tabs
         --
         s := s & "<script>" & LINE_BREAK; 
         s := s & "      updater.stop();"  & LINE_BREAK; 
         s := s & "      $( '#run_submit_button' ).disabled=false;" & LINE_BREAK;
         s := s & "      $( '#modelNavTop' ).tabs('option','disabled', []);" & LINE_BREAK;
         s := s & "      $( '#modelNavTop' ).tabs( 'select' , 3 );" & LINE_BREAK;
         s := s & "</script>" & LINE_BREAK;
      end if;
      return s;
   end Get_State_Of_Run_As_HTML;   
   
end EU.BE.Model.Runner.Web;
