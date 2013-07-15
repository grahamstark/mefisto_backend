with Logger;

package EU_Logger is

   type Loggable_Modules is ( globals, callbacks, parameter_handlers, queueing, output, poverty, input_rendering, runner, users );

	package E_Logger is new Logger( Loggable_Modules );
	
	procedure Log( which : Loggable_Modules; s : String ) renames E_Logger.Log;
	procedure Add_Target( which : Loggable_Modules ) renames E_Logger.Add_Target;
	procedure Flush renames E_Logger.Flush;
	procedure Set_Output( name : String ) renames E_Logger.Set_Output;
	procedure Add_All_Targets renames E_Logger.Add_All_Targets;
	procedure Remove_Target( which : Loggable_Modules ) renames E_Logger.Remove_Target;
	procedure Clear_All_Targets renames E_Logger.Clear_All_Targets;
	
end EU_Logger;

