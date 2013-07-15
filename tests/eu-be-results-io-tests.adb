--
--  $Author: graham_s $
--  $Date: 2010-02-15 13:40:48 +0000 (Mon, 15 Feb 2010) $
--  $Revision: 8644 $
--
with EU;
with EU.BE.Model;
with EU.BE.Model.Runner;
with EU.BE.Model.Settings;
-- with EU.BE.Model.Results;
with EU.BE;
with EU.Web.Settings;

with EU.BE.Household.Web_IO;
with EU.BE.I81N;
with Ada.Text_IO;

with AUnit.Test_Cases;
with AUnit.Assertions;
with Text_Buffer;
   
package body EU.BE.Results.IO.Tests is

   use AUnit.Assertions;
   use AUnit.Test_Cases.Registration;
   use Ada.Text_IO;
   package ms renames EU.BE.Model.Settings;
   
   model_sett : ms.Model_Settings;
   
   
   procedure Test_Read_Results(  T : in out AUnit.Test_Cases.Test_Case'Class ) is
      use Text_Buffer;
      use Detailed_Results_Package;
      
      detailed_buff : Buffer;
      detailed : Detailed_Record;
      agg  : Detailed_Record;
      personal_list : Detailed_Results_List;
      agg_list  : Detailed_Results_List;
   begin
      detailed_buff := Load( model_sett.Qualified_Output_File_Name );

      Assert( detailed_buff.Num_Lines = 14217, "should be 14217 (inc header) lines in ../../model/output/be_2008_ext_std.csv; was " & 
         Positive'Image( detailed_buff.Num_Lines ));
      for l in 2 .. detailed_buff.Num_Lines loop
         Put_Line( "on line " & l'Img );
         detailed :=  Read_Detailed( detailed_buff.Get_Line( l ));
         Put_Line( "Read OK" );
         case l is
            when 2 => 
               Assert( detailed.identifier_household = 1, "r1 detailed.identifier_household should be 1 was " & detailed.identifier_household'Img );
               Assert( detailed.taxable_income_something_mq = 1303.78	, "r1 taxable_income_something_mq should be 1303.78 was" & Format(  detailed.taxable_income_something_mq ));
               Assert( detailed.demographic_age = 70, "age rec 1 should be 70 was " & detailed.demographic_age'Img );
            when 7719 => 
               Assert( detailed.identifier_household = 9746, "last: detailed.identifier_household should be 9746 was " & detailed.identifier_household'Img );
               Assert( detailed.taxable_income_something_mq = 0.00, "r6 taxable_income_something_mq should be 0.00 was " & Format(  detailed.taxable_income_something_mq ));
               Assert( detailed.demographic_age = 21, "age rec 6 should be 21 was " & detailed.demographic_age'Img );
            when others => null;
         end case;
         Add_To_List( personal_list, detailed );
         Add_To_List( agg_list, detailed, true );
      end loop;
      Assert( Positive( agg_list.Length ) = 3125, "accumulate aggs should give 3125; was " & agg_list.Length'Img );
      Assert( Positive( personal_list.Length ) = 14215, "accumulate aggs should give 7719; was " & personal_list.Length'Img );
      agg := agg_list.Element( 4 );
      Assert( agg.taxable_income_something_mq = 8_930.20, "agg.taxable_income_something_mq for hh last (pos4) should be 8,930.20 was " & agg.taxable_income_something_mq'Img );
      agg := agg_list.Element( 3125 );
      Assert( agg.taxable_income_something_mq = 12_759.02, "agg.taxable_income_something_mq for hh last (last) should be 12.759.02 was " & agg.taxable_income_something_mq'Img );
    end Test_Read_Results;
  
   procedure Test_Read_Results_Old(  T : in out AUnit.Test_Cases.Test_Case'Class ) is
      use Text_Buffer;
      use Detailed_Results_Package;
      
      buff : Buffer;
      detailed_buff : Buffer;
      summary : Summary_Record;
      detailed : Detailed_Record;
      agg  : Detailed_Record;
      personal_list : Detailed_Results_List;
      agg_list  : Detailed_Results_List;
   begin
      buff := Load( model_sett.Qualified_Output_Directory & "" );
      detailed_buff := Load( model_sett.Qualified_Output_Directory & "be_2008_ext_std.csv" );
      Put_Line( "lines in buffer " & Positive'Image( buff.Num_Lines ));
      Assert( buff.Num_Lines = 5861, "should be 5861 (inc header) lines in ../../model/output/be_2008_std_hh.csv; was " & 
         Positive'Image( buff.Num_Lines ));
      Assert( detailed_buff.Num_Lines = 7719, "should be 7719 (inc header) lines in ../../model/output/be_2008_ext_std.csv; was " & 
         Positive'Image( detailed_buff.Num_Lines ));
      for l in 2 .. buff.Num_Lines loop
         Put_Line( "on line " & l'Img );
         summary :=  Read_Summary( buff.Get_Line( l ));  
      end loop;
      for l in 2 .. detailed_buff.Num_Lines loop
         Put_Line( "on line " & l'Img );
         detailed :=  Read_Detailed( detailed_buff.Get_Line( l ));
         Put_Line( "Read OK" );
         case l is
            when 2 => 
               Assert( detailed.identifier_household = 1, "r1 detailed.identifier_household should be 1 was " & detailed.identifier_household'Img );
               Assert( detailed.taxable_income_something_mq = 1411.21	, "r1 taxable_income_something_mq should be 1411.21 was " & Format(  detailed.taxable_income_something_mq ));
               Assert( detailed.demographic_age = 70, "age rec 1 should be 70 was " & detailed.demographic_age'Img );
            when 7719 => 
               Assert( detailed.identifier_household = 9746, "last: detailed.identifier_household should be 2 was " & detailed.identifier_household'Img );
               Assert( detailed.taxable_income_something_mq = 0.00, "r6 taxable_income_something_mq should be 0.00 was " & Format(  detailed.taxable_income_something_mq ));
               Assert( detailed.demographic_age = 21, "age rec 6 should be 54 was " & detailed.demographic_age'Img );
            when others => null;
         end case;
         Add_To_List( personal_list, detailed );
         Add_To_List( agg_list, detailed, true );
      end loop;
      Assert( Positive( agg_list.Length ) = 3125, "accumulate aggs should give 5861; was " & agg_list.Length'Img );
      Assert( Positive( personal_list.Length ) = 14215, "accumulate aggs should give 7719; was " & personal_list.Length'Img );
      agg := agg_list.Element( 4 );
      Assert( agg.taxable_income_something_mq = 8691.5, "agg.taxable_income_something_mq for hh 6 (pos4) should be 8691.5 was " & agg.taxable_income_something_mq'Img );
      agg := agg_list.Element( 3125 );
      Assert( agg.taxable_income_something_mq = 12_759.02, "agg.taxable_income_something_mq for hh 6 (last) should be 12.759.02 was " & agg.taxable_income_something_mq'Img );
    end Test_Read_Results_Old;

   --------------------
   -- Register_Tests --
   --------------------

   procedure Register_Tests( t : in out Test_Case ) is
   begin 
        Register_Routine( T, Test_Read_Results'Access, "Test _Read_Results" );
   end Register_Tests;

   procedure Set_Up (T : in out Test_Case) is
   begin
      EU.Web.Settings.Read_Web_Settings( "etc/web_settings.txt" );
      Put_Line( "read OK" );
      EU.BE.I81N.Load_Translations;
      model_sett := ms.Read_Model_Settings( "etc/be_settings.txt" );
      model_sett.Set_Run_Id( "graham_s/run1/" );
   end Set_Up;

   ----------
   -- Name --
   ----------
   function Name ( T : Test_Case ) return Message_String is
   begin
      return Format( "EU.BE.Results.IO.Tests" );
   end Name;

end EU.BE.Results.IO.Tests;
