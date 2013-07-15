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
   
package body EU.BE.Household.IO.Tests is

   use AUnit.Assertions;
   use AUnit.Test_Cases.Registration;
   use Ada.Text_IO;
   package ms renames EU.BE.Model.Settings;
   
   model_sett : ms.Model_Settings;

   procedure Test_Create_Dataset( T : in out AUnit.Test_Cases.Test_Case'Class ) is
   begin
      Put_Line( "Test_Create_Dataset" );
      Put_Line( "Opening " & model_sett.Datafile_Directory & "/" & model_sett.Dataset_Name );
      Put_Line( "Writing to " & model_sett.Datafile_Directory & "/data_index.bin" );
      Create_Binary_Dataset( text_file_name => model_sett.Datafile_Directory & "/" & model_sett.Dataset_Name & ".txt",
                    binary_file_name        => model_sett.Datafile_Directory &  "/" & model_sett.Dataset_Name,
                    flemish_only            => True );
   end Test_Create_Dataset;

   procedure Read_Data is
      ds : EU_Data_Set;
      hh : Household_Rec;
   begin
      Put_Line( "Read_Data" );
      Put_Line( "Read_Data: reading from " & model_sett.Datafile_Directory & "/" & model_sett.Dataset_Name );
      ds := Load_Binary( model_sett.Datafile_Directory & "/" & model_sett.Dataset_Name );
      Put_Line( "testing " & ds.Number_Of_Households'Img & " Households " );
      Assert( ds.Number_Of_Households = 3125, "should be 3125 households; was " & ds.Number_Of_Households'Img );
      for hhref in 1 .. ds.Number_Of_Households loop
         
         hh := ds.Read_Household( hhref );
         if( hhref = 4 ) then
            Assert( hh.num_people = 4, "Num People(1) = 4 but was " &  hh.num_people'Img );
            Assert( hh.people(1).demographic_age = 41, "Age of 1st should be 41 but was " & hh.people(1).demographic_age'Img );
            Assert( Nearly_Equal(hh.expenditure_housing_cost_mortgage_payment_interest_and_capital_mortgage_interest, 2.0*2689.575, 0.01 ), 
               "hh.expenditure_housing_cost_mortgage_payment_interest_and_capital_mortgage_interest should be 1,524.23 was " &
                  Format( hh.expenditure_housing_cost_mortgage_payment_interest_and_capital_mortgage_interest ));
            Assert( Nearly_Equal(hh.people(1).expenditure_housing_cost_mortgage_payment_interest_and_capital_mortgage_interest, 2689.575, 0.01 ), 
               "hh.people(1).expenditure_housing_cost_mortgage_payment_interest_and_capital_mortgage_interest should be 762.11 was " &
                  Format( hh.people(1).expenditure_housing_cost_mortgage_payment_interest_and_capital_mortgage_interest ));
            Assert( Nearly_Equal( hh.people(2).expenditure_housing_cost_mortgage_payment_interest_and_capital_mortgage_interest, 2689.575, 0.01 ), 
               "hh.people(3).expenditure_housing_cost_mortgage_payment_interest_and_capital_mortgage_interest should be 762.11 was " &
                  Format( hh.people(2).expenditure_housing_cost_mortgage_payment_interest_and_capital_mortgage_interest ));
            Assert( Nearly_Equal( hh.people(3).expenditure_housing_cost_mortgage_payment_interest_and_capital_mortgage_interest, 0.0, 0.01 ), 
               "hh.people(3).expenditure_housing_cost_mortgage_payment_interest_and_capital_mortgage_interest should be 0.0 was " &
                  Format( hh.people(3).expenditure_housing_cost_mortgage_payment_interest_and_capital_mortgage_interest ));
                  
         elsif( hhref = 2 ) then
            Assert( hh.num_people = 1, "Num People(2) = 1 but was " &  hh.num_people'Img );
            Assert( hh.people(1).demographic_age = 80, "Age of 2st(2) should be 80 but was " & hh.people(1).demographic_age'Img );
         end if;
         if( hhref < 10 )then
            Put_Line( EU.BE.Household.Web_IO.To_HTML( hh.people( 2 ), EU.BE.I81N.en ));
            Put_Line( EU.BE.Household.Web_IO.To_HTML( hh.people( 2 ), EU.BE.I81N.nl ));
         end if;
         Put_Line( "HH[" & hhref'Img &" ] read " );
      end loop;
   end Read_Data;
   
   procedure Test_Read_Data( T : in out AUnit.Test_Cases.Test_Case'Class ) is
   begin
      Put_Line( "Test_Read_Data" );
      Read_Data;
   end Test_Read_Data;
   
   procedure Test_Write_Data( t : in out AUnit.Test_Cases.Test_Case'Class ) is
      f : File_Type;
      fname : constant String := model_sett.Datafile_Directory & "/created_v1";
      ds : EU_Data_Set;
      hh : Household_Rec;
   begin
      Put_Line( "Test_Write_Data" );
      Create( f, Out_File, fname & ".txt" );
      ds := Load_Binary( model_sett.Datafile_Directory & "/" & model_sett.Dataset_Name );
      Assert( ds.Number_Of_Households = 3125, " load of ds didn't return 3125 household was: " & ds.Number_Of_Households'Img );
      Assert( ds.Number_Of_People = 7719, " load of ds didn't return 7719 people was: " & ds.Number_Of_People'Img );
      Put_Line( "Test_Write_Data:After Load_Binary; read from  " &
         model_sett.Datafile_Directory & "/" & model_sett.Dataset_Name );
      Write_Household_Header( f );
      for hhref in 1 .. ds.Number_Of_Households loop
         hh := ds.Read_Household( hhref );
         Put_Line( "Test_Write_Data: write hh " & hhref'Img & " num people " & hh.num_people'Img );
         Write_Household( hh, f );
      end loop;
      Close( f );
      Put_Line( "Test_Write_Data:Before Create_Binary_Dataset; csv written to " & fname & ".txt");
      Create_Binary_Dataset( 
         text_file_name   => fname & ".txt",
         binary_file_name => fname,
         flemish_only     => True );
      Put_Line( "Test_Write_Data:Before Read_Data" );
      -- Read_Data( "created_v1" );
   end Test_Write_Data;
   
   procedure Test_Write_Data_With_Reallaoc( t : in out AUnit.Test_Cases.Test_Case'Class ) is
      f : File_Type;
      fname : constant String := model_sett.Datafile_Directory & "/created_v2";
      ds : EU_Data_Set;
      hh : Household_Rec;
   begin
      Put_Line( "Test_Write_Data_With_Reallaoc" );
      Create( f, Out_File, fname & ".csv" );
      ds := Load_Binary( model_sett.Datafile_Directory & "/" & model_sett.Dataset_Name );
      Assert( ds.Number_Of_Households = 3125, " load of ds didn't return 2 household was: " & ds.Number_Of_Households'Img );
      Assert( ds.Number_Of_People = 7719, " load of ds didn't return 6 people was: " & ds.Number_Of_People'Img );
      Put_Line( "Test_Write_Data:After Load_Binary; read from  " &
         model_sett.Datafile_Directory & "/" & model_sett.Dataset_Name );
      Write_Household_Header( f );
      for hhref in 1 .. ds.Number_Of_Households loop
         hh := ds.Read_Household( hhref );
         Put_Line( "Test_Write_Data: write hh " & hhref'Img & " num people " & hh.num_people'Img );
         Allocate_Household_To_Personal( hh );
         Write_Household( hh, f );
      end loop;
      Close( f );
      Put_Line( "Test_Write_Data:Before Create_Binary_Dataset; csv written to " & fname & ".csv");
      Create_Binary_Dataset( 
         text_file_name   => fname & ".csv",
         binary_file_name => fname,
         flemish_only     => True );
      Put_Line( "Test_Write_Data:Before Read_Data" );
      -- ead_Data( "created_v1" );
   end Test_Write_Data_With_Reallaoc;
   
  

   --------------------
   -- Register_Tests --
   --------------------

   procedure Register_Tests( t : in out Test_Case ) is
   begin 
        Register_Routine( T, Test_Create_Dataset'Access, "Test _Create_Dataset" );
        Register_Routine( T, Test_Read_Data'Access, "Test _Read_Data" );
        Register_Routine( T, Test_Write_Data'Access, "Test _Write_Data" );
        Register_Routine( T, Test_Write_Data_With_Reallaoc'Access, "Test_Write_Data_With_Reallaoc" );
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
      return Format( "EU.BE.Household.IO.Tests" );
   end Name;

end EU.BE.Household.IO.Tests;
