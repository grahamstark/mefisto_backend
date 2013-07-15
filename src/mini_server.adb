with Ada.Text_IO;
with Ada.Strings.Unbounded;

with AWS.Config.Set; 
with AWS.Config; 
with AWS.Dispatchers.Callback;
with AWS.Mime;
with AWS.Server.Log;
with AWS.Server;
with AWS.Services.Dispatchers.URI;
with AWS.Services.Page_Server;
with AWS.Services;

with EU.BE.Mini_Runner;

with EU.BE.Model.Settings;
with EU.BE.Mini_Runner;
with EU.Web.Settings;

with Text_Utils;
with Web_Utils;
with Utils;

--
-- 
-- This is the driver for a single Mefisto runner. Called with the name of a user and a run, which should already
-- be created. Just runs the Euromod instance and doesn't bother with creating output, etc. which is left 
-- to the calling program once the run is complete.
-- Use a call to Status to check run progress 
--  possible actions are 
--       free - check if free and reserve for a run if so
--       run  - run the model using the supplied username and run_id
--       status - check the status of a run
--       reserve - reserve a server (not used?)
--       unreserve - remove a reservation
--       halt - kill a job (actually doesn't kill a Euromod process in the live version, but sets all the internal state back to the 'free' state.
--
-- e.g
--      localhost:[PORT]/free 
--      localhost:[PORT]/status?rid=[some_run_id]&uid=[some_user_id]
--      localhost:[PORT]/run?rid=[some_run_id]&uid=[some_user_id]
-- 
procedure Mini_Server is
   
    
   use AWS.Services;
   use AWS.Config;
   use AWS.Config.Set;
   use Ada.Text_IO;
   
   package minirun renames EU.BE.Mini_Runner;
   
   my_server     : AWS.Server.HTTP;
   my_dispatcher : AWS.Services.Dispatchers.URI.Handler;
   my_config     : AWS.Config.Object := AWS.Config.Get_Current;
   
   package awsc renames AWS.Config; 
   
   use Text_Utils;
   use Ada.Strings.Unbounded;
    
   default_handler : AWS.Dispatchers.Callback.Handler;

begin
   
   minirun.Initialise_Settings; 
   
   declare
      SEP : constant String := EU.Web.Settings.Dir_Separator;
      port_id : constant String := Positive'Image( minirun.port );
   begin      
   
      default_handler := AWS.Dispatchers.Callback.Create( minirun.Status_Callback'Access );
      
      Dispatchers.URI.Register_Default_Callback( my_dispatcher, default_handler );
      
      Put_Line("Mini Server!!!!! port" & port_id );
       
      awsc.Set.Server_Name( my_config, "Mefisto/Motiff Mini Server; port " & port_id );
      awsc.Set.Server_Port( my_config, minirun.port );
      awsc.Set.WWW_Root( my_config, EU.Web.Settings.Physical_Root & "web" );
      awsc.Set.Session( my_config, False );
      awsc.Set.Max_Connection( my_config,  10 );
      awsc.Set.Accept_Queue_Size( my_config, 10 );
      awsc.Set.Free_Slots_Keep_Alive_Limit(my_config, 10 );
      
      Dispatchers.URI.Register_Regexp( my_dispatcher, "/alive.*", minirun.Alive_Callback'Access );
      Dispatchers.URI.Register_Regexp( my_dispatcher, "/run.*", minirun.Run_Callback'Access );
      Dispatchers.URI.Register_Regexp( my_dispatcher, "/halt.*", minirun.Halt_Callback'Access );
      Dispatchers.URI.Register_Regexp( my_dispatcher, "/free.*", minirun.Free_Callback'Access );
      Dispatchers.URI.Register_Regexp( my_dispatcher, "/reserve.*", minirun.Reserve_Callback'Access );
      Dispatchers.URI.Register_Regexp( my_dispatcher, "/unreserve.*", minirun.Unreserve_Callback'Access );
   end;
   
   AWS.Server.Log.Start( 
      Web_Server => my_server,
      Auto_Flush => True );
   AWS.Server.Log.Start_Error( 
      Web_Server => my_server );
   AWS.Server.Start( 
      my_server,
      Dispatcher => my_dispatcher,
      Config     => my_config );
   
   AWS.Server.Wait( AWS.Server.forever );
   AWS.Server.Shutdown( my_server );
   
end Mini_Server;
