--
-- Some basic type definitions, moved out of the household defintions to aviod cross-dependencies
-- see also BE_Base_Model_Types
--
package BE_Types is

   type Nuts_level_1_type is (  
      brussels,
      flanders,
      wallonia );
    
   type Age_Band is ( age_0_15, age_16_18, age_19_21, age_22_40, age_41_60, age_61_65, age_over_65 );
   subtype Adult_Age_Band is Age_Band range age_16_18 .. Age_Band'Last;
   
   function Get_Age_Band( age : Natural ) return Age_Band;
   
   function Value( i : Nuts_level_1_type ) return Integer;
   function Convert( i : String ) return Nuts_level_1_type;

   subtype Household_Count is Integer range 0 .. 6_000; -- actually 5,860
   subtype Deciles         is Integer range 1 .. 10;
   subtype Quintiles       is Integer range 1 .. 5;
  
end BE_Types;
