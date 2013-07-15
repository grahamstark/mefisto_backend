with EU.BE.I81N;
with Templates_Parser;

package EU.BE.Results.Web_IO is

   use EU.BE.I81N;
   use Templates_Parser;
   
   function To_HTML( pers : in Summary_Record; lang : Languages; postfix : String := "" ) return String;
   function To_HTML( pers : in Detailed_Record; lang : Languages; postfix : String := "" ) return String;
   function To_Translate_Set( pers : in Detailed_Record; lang : Languages; postfix : String ) return Translate_Set;
   function To_Translate_Set( pers : in Summary_Record; lang : Languages; postfix : String ) return Translate_Set;
   
end  EU.BE.Results.Web_IO;
