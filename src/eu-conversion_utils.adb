with Ada.Characters.Latin_1;
with Ada.Strings.Fixed;
with Ada.Strings;
with Ada.Characters.Handling; 
with Format_Utils;
          

package body EU.Conversion_Utils is
   use Ada.Strings.Fixed;
   use Ada.Strings;
 
 package Local_Format_Utils is new 
      Format_Utils( 
         Counter_Type => Counter_Type, 
         Float_Type => Rate,
         Default_Separator => ' ',
         Default_Radix_Mark => ',',
         Amount_Picture => 
            Ada.Text_IO.Editing.To_Picture( "-ZZZ_ZZZ_ZZZ_ZZZ_ZZZ_ZZ9") ); -- comma seperator

   use Local_Format_Utils;

   function Get_String( slices : Slice_Set; which : Natural ) return String is
      wn : Slice_Number := Slice_Number( which ); 
   begin
      return Trim(Slice( slices , wn ), Both );
   end Get_String;

   function Convert( slices : Slice_Set; which : Natural ) return Amount is
      wn : Slice_Number := Slice_Number( which );
      r  : Amount;
   begin
      if( Slice( slices, wn )'Length = 0 ) or ( Slice( slices, wn ) = " " )then 
         return MISSING_R; 
      end if;
      r := Lenient_Convert( Slice( slices , wn ));
      if(( r = -1.0 ) or ( r = -2.0 ))then 
         r := MISSING_R; 
      end if;
      return r;
   end Convert;
   
   function Convert( slices : Slice_Set; which : Natural ) return Integer is
      i : Integer;
      wn : Slice_Number := Slice_Number( which ); 
   begin
      if( Slice( slices, wn )'Length = 0 ) or ( Slice( slices, wn ) = " " )then 
         return MISSING; 
      end if;
      i := Integer'Value( Trim(Slice( slices , wn ), Both ));
      if( i = -1 ) then i := MISSING; end if;
      return i;
   end Convert;
   
   function Convert( slices : Slice_Set; which : Natural ) return Boolean is
      use Ada.Characters.Handling;
      wn : Slice_Number := Slice_Number( which ); 
      b : Boolean := False;
   begin 
      if( Slice( slices, wn )'Length = 0 ) or ( Slice( slices, wn ) = " " )then 
         return False; 
      end if;
      declare
         s : String := To_Upper( Trim( Slice( slices , wn ), Both ));
       begin
         b := ( s = "1" )  or ( s = "TRUE" ) or ( s = "YES" );
       end;
       return b;
   end Convert;
   
   function TDA_Tokenize( s : String ) return Slice_Set is
      use Ada.Characters.Latin_1;
      use Ada.Strings.Fixed;
      use Ada.Strings;
      
      DELIMS : constant String := "," & HT & LF & CR;
      slices : Slice_Set;
      last   : Natural := s'Last;
   begin
      if( s(last) = LF ) then last := last - 1; end if;
      Create( slices, Trim( s( 1 .. last ), Both ), DELIMS, Single );
      return slices;
   end TDA_Tokenize;

end EU.Conversion_Utils;
