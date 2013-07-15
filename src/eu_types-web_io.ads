with EU.BE.I81N;

package EU_Types.Web_IO is

   use EU.BE.I81N;

   function Web_Format( i : Marital_status_type; lang : Languages ) return String;
   function Web_Format( i : Education_current_status_type; lang : Languages ) return String;
   function Web_Format( i : Education_highest_status_type; lang : Languages ) return String;
   function Web_Format( i : Consensual_union_type; lang : Languages ) return String; 
   function Web_Format( i : Gender_type; lang : Languages ) return String;
   function Web_Format( i : Citizenship_type; lang : Languages ) return String; 
   function Web_Format( i : Tenure_type; lang : Languages ) return String;
   function Web_Format( i : Occupation_Isco_1_Digit; lang : Languages ) return String;
      
end EU_Types.Web_IO;
