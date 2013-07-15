with GNAT.String_Split;

package EU.Conversion_Utils is
   
   use GNAT.String_Split;
   function Convert( slices : Slice_Set; which : Natural ) return Amount;
   function Convert( slices : Slice_Set; which : Natural ) return Integer;
   function Convert( slices : Slice_Set; which : Natural ) return Boolean;
   
   function TDA_Tokenize( s : String ) return Slice_Set;
   function Get_String( slices : Slice_Set; which : Natural ) return String;
 
end EU.Conversion_Utils;
