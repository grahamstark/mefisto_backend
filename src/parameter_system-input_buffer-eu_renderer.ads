with Ada.Strings.Unbounded;
with Templates_Parser;
with Text_Utils;
with EU.BE.Users;
generic

package Parameter_System.Input_Buffer.EU_Renderer is
   
   use Templates_Parser;
   use Text_Utils;
   
   procedure Create_HTML_Inputs( 
      html_cells       : in out Templates_Parser.Vector_Tag;
      buff             : Buffer;
      base_sys         : Parameter_System_Rec; 
      sys              : Parameter_System_Rec; 
      parameter_prefix : Unbounded_String );
      
   type HTML_Kind is ( html5, xhtml1 );
   
   type Ajax_Action_Type is ( 
      insert_above, 
      insert_below, 
      delete, 
      save, 
      copy,
      error_check );

   procedure Set_HTML_Type( t : HTML_Kind );
    
   --
   -- FIXME private!
   --
   function Make_Validator_Code( 
      key : Unbounded_String; 
      param : Parameter_Rec ) return Unbounded_String;
   
   --
   -- testing only!
   --
   function Make_Ajax_Call_Indexed( 
      action          : Ajax_Action_Type;
      key             : Unbounded_String;
      row             : Natural;
      lang            : Languages;
      ajax_target_key : Unbounded_String) return Unbounded_String;

      
   function Make_Parameter_Menu(
      base_sys            : Parameter_System_Rec; 
      path                : Unbounded_String_List;
      prefix              : Unbounded_String;      
      lang                : Languages;
      user                : EU.BE.Users.User_Type ) return Unbounded_String;   
      
   function Make_Indexed_Block(
      complete_sys      : Parameter_System_Rec;
      key               : Unbounded_String;
      buff              : Buffer;
      ajax_target_key   : Unbounded_String ) return Unbounded_String;
      
   function Create_Input_Page(
      buff                : Buffer;
      model_menu          : Unbounded_String;
      base_sys            : Parameter_System_Rec; 
      sys                 : Parameter_System_Rec;
      parameter_prefix    : Unbounded_String;
      main_error_message  : Unbounded_String;
      job_is_running      : Boolean;
      user                : EU.BE.Users.User_Type;
      extra_translations  : Templates_Parser.Translate_Set ) return Unbounded_String;

   function Create_Wrapper_Input_Page(
      model_menu          : Unbounded_String;
      user                : EU.BE.Users.User_Type;
      extra_translations  : Templates_Parser.Translate_Set ) return Unbounded_String;
      
   function Create_Inner_Input_Page(
      buff                : Buffer;
      base_sys            : Parameter_System_Rec; 
      sys                 : Parameter_System_Rec;
      parameter_prefix    : Unbounded_String;
      error_count         : Natural;
      job_is_running      : Boolean;
      user                : EU.BE.Users.User_Type;
      extra_translations  : Templates_Parser.Translate_Set ) return Unbounded_String;
      
   function Create_Euromod_Parameter_Mappings( buff : Buffer ) return Templates_Parser.Translate_Set;
   
   --
   -- Specific variable handlers
   --
   procedure Switch_Payment_For_UB_1( buff : in out Buffer );
   
end Parameter_System.Input_Buffer.EU_Renderer;
