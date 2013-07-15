with EU.BE.I81N;

with EU.BE.Light_Parameters;

with Parameter_System.Input_Buffer.EU_Renderer;
with Parameter_System.Input_Buffer.Utils;
with Parameter_System.Input_Buffer;
with Parameter_System.XML;
with Parameter_System;

package EU.BE.Parameter_System_Declarations is  
   
   use EU.BE.Light_Parameters;

   --
   -- packages for the input buffer system
   --
   package BE_Parameter_System is new Parameter_System( 
      Float_Type   => Amount, 
      Counter_Type => Counter_Type, 
      Languages    => EU.BE.I81N.Languages );
      
   package BE_Parameter_System_XML is new BE_Parameter_System.XML;
   
   procedure Log_Params( s : String );            

   --
   -- FIXME needing uprate here is really crappy design.
   --
   procedure Uprate( 
            c    : in out Amount; 
            mult : Amount; 
            rec  : BE_Parameter_System.Parameter_Rec; 
            which_operation : Integer := 0 );
            
   package BE_Parameter_System_IO is new BE_Parameter_System.Input_Buffer(
      Uprate,
      EU.BE.I81N.Web_Format,
      EU.BE.I81N.Web_Format,
      EU.BE.I81N.Web_Format,
      EU.BE.I81N.Web_Validate,
      EU.BE.I81N.Web_Validate,
      EU.BE.I81N.Translations.Lookup
   );
      
   subtype BE_Buffer is BE_Parameter_System_IO.Buffer;
   
   package BE_Utils is new BE_Parameter_System_IO.Utils;
   package BE_Renderer is new BE_Parameter_System_IO.EU_Renderer;
  
   procedure Map_To_Parameters( light : Light_Params; params : in out BE_Buffer ); 
   

end EU.BE.Parameter_System_Declarations;

