with AWS.Status;
with AWS.Response;
with EU.BE.Model.Settings;

-- 
-- This is the backend code for a single Mefisto runner. 
-- Called with the name of a user and a run, which should already
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
-- the Make_URL procedure can construct one of these strings for you.
-- 
package EU.BE.Mini_Runner is

   use EU.BE.Model.Settings;
   
   type Action_Type is ( status, run, halt, free, alive, reserve, unreserve );
   subtype Full_Queries is Action_Type range status .. halt;

   --
   -- Status of the server. The request should be
   -- constructed using the Make_URL function below
   --
   -- returns 'LOCKED' if the server is locked for some reason, or otherwise a state string
   -- which can be decoded into a State_Rec using the function 
   -- 
   function Status_Callback( Request : in AWS.Status.Data ) return AWS.Response.Data;
   
   --
   -- Run the model. The request and response is as above.
   --
   function Run_Callback( Request : in AWS.Status.Data ) return AWS.Response.Data;

   --
   -- Check if the server is free, and if so reserve it. Request data is ignored.
   -- response is 'true' for free, 'false' otherwise.
   --
   function Free_Callback( Request : in AWS.Status.Data ) return AWS.Response.Data;
   
   --
   -- halt is only really possible in the detached runner version, but this can be
   -- used to mark a server as free if a job has times out somehow.
   -- returns a state string
   function Halt_Callback( Request : in AWS.Status.Data ) return AWS.Response.Data;
   
   --
   -- returns the string 'ALIVE' and otherwise does nothing. Used by keep-alive monitors
   --
   function Alive_Callback( request : in AWS.Status.Data ) return AWS.Response.Data;

   --
   -- Reserving a miniserver: if a minserver is marked as free but the job hasn't been
   -- submitted yet. Possible responses are 'true' for success or 'false'
   --
   function Reserve_Callback( request : in AWS.Status.Data ) return AWS.Response.Data;
   function Unreserve_Callback( request : in AWS.Status.Data ) return AWS.Response.Data;
   
   procedure Initialise_Settings;
   
   --
   -- convenience method to make one of the URLS above
   --
   function Make_URL( 
      server_name : String;
      port        : Positive;
      action      : Action_Type;
      user        : String := "";
      run         : String := "" ) return String;
   

   --
   -- possible one word replies from mini server
   --
   LOCKED_STR      : constant String := "LOCKED";
   NOT_RUNNING_STR : constant String := "NOT_RUNNING";
   ALIVE_STR       : constant String := "ALIVE";

   port            : Positive;
   
private

   type Server_State_Type is ( server_free, job_is_running, job_needs_done );
   
   --
   -- for unit testing.
   --
   protected type Job_Submission_Type is
      
      entry Seize;
      procedure Release;
      
      procedure Enque( run_settings : Model_Settings; run_state : State_Rec );
      function Get_Server_State return Server_State_Type;
      procedure Deque( run_settings : out Model_Settings; run_state : out State_Rec );
      function Get_Settings return Model_Settings;
      function Get_State return State_Rec;
      procedure Set_State( new_state : State_Rec );
      procedure Mark_Job_End;
      procedure Update_Settings( new_settings : Model_Settings );
      
   private   
      
      locked          : Boolean := False;
      settings        : Model_Settings;
      state           : State_Rec := NULL_STATE_REC;
      server_state    : Server_State_Type := server_free;
      
   end Job_Submission_Type;   

end EU.BE.Mini_Runner;
