with Ada.Text_IO;

package body EU.Web.Settings is

   use Ada.Strings.Unbounded;
   
   this_log_file_dir             : Unbounded_String := To_Unbounded_String("");
   this_Mefisto_Root             : Unbounded_String := To_Unbounded_String("");
   this_Motyff_Root              : Unbounded_String := To_Unbounded_String("");
   this_physical_root            : Unbounded_String := To_Unbounded_String("");
   this_work_dir                 : Unbounded_String := To_Unbounded_String("");
   this_template_components_path : Unbounded_String := To_Unbounded_String("");
   this_port                     : Positive := 80;
   this_charts_driver_script     : Unbounded_String := To_Unbounded_String("");
   this_language_components_path : Unbounded_String := To_Unbounded_String("");
   this_dir_separator            : Unbounded_String := To_Unbounded_String("");
   this_xml_file                 : Unbounded_String := To_Unbounded_String("");
   this_charts_url               : Unbounded_String := To_Unbounded_String("");

   this_first_mini_port          : Positive := 20000;
   this_num_mini_servers         : Positive := 15;
   this_mini_url                 : Unbounded_String := To_Unbounded_String("localhost");
   
   
   procedure Read_Web_Settings( filename : String ) is
      use EU_Key_Value_IO;
      use Ada.Text_IO;
      file : Ada.Text_IO.File_Type;  
   begin
      Open( file, In_File, filename );
      this_physical_root := read( file, "physical_root" );
      this_mefisto_root := read( file, "mefisto_root" );
      this_motyff_root := read( file, "motyff_root" );
      this_log_file_dir := read( file, "log_file_dir" );
      this_work_dir := read( file, "work_dir" );
      this_template_components_path := read( file, "template_components_path" );
      this_port := read( file, "port" );
      this_charts_driver_script := read( file, "charts_driver_script" );
      this_language_components_path := read( file, "language_components_path" );
      this_dir_separator := read( file, "dir_separator" );
      this_xml_file := read( file, "xml_file" );
      this_charts_url := read( file, "charts_url" );
      this_first_mini_port := Read( file, "first_mini_port" );
      this_num_mini_servers := Read( file, "num_mini_servers" );
      this_mini_url := Read( file, "mini_url" );      
      Close( file );
   end Read_Web_Settings;
   
   function Language_Components_Path return String is
   begin
     return To_String( this_physical_root & this_dir_separator & this_language_components_path );
   end  Language_Components_Path;
   
   function Dir_Separator return String is
   begin
      return To_String( this_dir_separator );
   end Dir_Separator;
   
   function Log_File_Dir return String is
   begin
      return To_String( this_physical_root & this_dir_separator & this_Log_File_Dir );
   end Log_File_Dir;
    
   function Mefisto_Root return String is 
   begin 
      return To_String( this_Mefisto_Root ); 
   end Mefisto_Root;

   function Motyff_Root return String is 
   begin 
      return To_String( this_Motyff_Root ); 
   end Motyff_Root;

   function Physical_Root return String is 
   begin 
      return To_String( this_physical_root ); 
   end Physical_Root;

   function Work_Dir return String is 
   begin 
      return To_String( this_physical_root & this_dir_separator & this_work_dir ); 
   end Work_Dir;
   
   function Template_Components_Path return String is 
   begin 
      return To_String( this_physical_root & this_dir_separator & this_template_components_path ); 
   end Template_Components_Path;

   function Port return Positive is 
   begin 
      return this_port;
   end Port;
    
   function Charts_Driver_Script return String is 
   begin 
      return To_String( this_physical_root & this_dir_separator & this_charts_driver_script ); 
   end Charts_Driver_Script;

   function XML_File return String is 
   begin 
      return To_String( this_physical_root & this_dir_separator & this_xml_file ); 
   end XML_File;
    
   function Charts_URL return String is 
   begin 
      return To_String( this_charts_url ); 
   end Charts_URL;
   function First_Mini_Port return Positive is
   begin
      return this_first_mini_port;
   end First_Mini_Port;
   
   function Num_Mini_Servers return Positive is
   begin
      return this_num_mini_servers;
   end Num_Mini_Servers;
   
   function Mini_URL return String is
   begin
      return To_String( this_mini_url );
   end Mini_URL;

   
 
   
end EU.Web.Settings;
