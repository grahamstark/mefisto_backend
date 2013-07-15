with EU.BE.I81N;

--
-- This converts a SILC houshold or person to a simple html representation. 
-- Used in the example generator routines.
-- 
package EU.BE.Household.Web_IO is
   
   use EU.BE.I81N;
   
   function To_HTML( hh : in Household_Rec; lang : Languages ) return String;
   function To_HTML( pers : in Person_Rec; lang : Languages ) return String;
    
end EU.BE.Household.Web_IO;
