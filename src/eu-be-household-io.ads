with Ada.Text_IO;
with Ada.Containers.Vectors;
with Ada.Strings.Unbounded;
with Ada.Direct_IO;
with Text_Buffer;

--
-- This contains code to read and write complete households from the SILC dataset.
-- and allocate people to benefit units.
--
-- TODO/FIXME
-- It really needs to be rewritten to read and write in ASCII rather than binary
-- as there is no real performance gain (data is read once & then kept in memory)
-- and it would save us some 32/64 bit agnony. Benefit unit code is redundant.
-- 
package EU.BE.Household.IO is

   use Ada.Text_IO;
   use Ada.Strings.Unbounded;
   use Text_Buffer;
   
   type EU_Data_Set is tagged private;
   
   function Load_Binary( file_name : String ) return EU_Data_Set;
  
   procedure Write_Household( hh : in Household_Rec; file : File_Type );
   procedure Write_Household_Header( file : File_Type );
   
   function Read_Person( line : Unbounded_String ) return Person_Rec;
   
   function Read_Household( data : EU_Data_Set; which : Household_Count ) return Household_Rec;
   
   procedure Create_Binary_Dataset( text_file_name : String; binary_file_name : String; flemish_only : Boolean );
   
   function Number_Of_Households( data : EU_Data_Set ) return Household_Count; 

   function Number_Of_People( data : EU_Data_Set ) return Positive; 
   
   procedure Allocate_Household_To_Personal( hh : in out Household_Rec );
   
   procedure Allocate_Personal_To_Household( hh : in out Household_Rec );
   
private
   
   package Household_Data_Package is new Ada.Containers.Vectors( Element_Type => Household_Data, Index_Type => Positive );  
   package Person_Data_Package is new Ada.Containers.Vectors( Element_Type => Person_Rec, Index_Type => Positive );  
   
   subtype Household_List is Household_Data_Package.Vector;
   subtype Person_List is Person_Data_Package.Vector;

   type EU_Data_Set is tagged record
      people : Person_List;
      households : Household_List;
   end record;

end EU.BE.Household.IO;
