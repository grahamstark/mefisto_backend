with Ada.Strings.Unbounded;

package EU.Web.Settings is
   
   procedure Read_Web_Settings( filename : String );
   
   function Log_File_Dir return String;
   function Physical_Root return String; 
   function Mefisto_Root return String; 
   function Motyff_Root return String; 
   function Work_Dir return String; 
   function Template_Components_Path return String;
   function Port return Positive; 
   function Charts_Driver_Script return String; 
   function Language_Components_Path return String;
   function Dir_Separator return String;
   function XML_File return String;
   function Charts_URL return String; 
   
   function First_Mini_Port return Positive;
   function Num_Mini_Servers return Positive;
   function Mini_URL return String;
   
end EU.Web.Settings;
