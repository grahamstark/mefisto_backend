with Ada.Text_IO.Unbounded_IO;
with GNAT.String_Split;
with EU.Conversion_Utils;
with Text_Buffer;


package body EU.BE.Household.IO is

   package Household_IO is new Ada.Direct_IO( Household_Data );
   package Person_IO is new Ada.Direct_IO( Person_Rec );
   
   function Read_Household( data : EU_Data_Set; which : Household_Count ) return Household_Rec is
      hh : Household_Rec;
      hhd : Household_Data;
      pos : Positive;
   begin
      hhd := data.households.Element( which );
      
      Household_Data( hh ) := hhd;
      -- Put_Line( "hh.num_people " & hh.num_people'Img );
      -- Put_Line( "hh.first_person_record " & hh.first_person_record'Img );
      for pno in 1 .. hh.num_people loop
         pos := pno + hh.first_person_record - 1;
         hh.people( pno ) := data.people.Element( pos );
      end loop;
      return hh;
   end Read_Household;

   function Number_Of_Households( data : EU_Data_Set ) return Household_Count is
   begin
      return Household_Count( Household_Data_Package.Length( data.households ));
   end Number_Of_Households;
   
   function Number_Of_People( data : EU_Data_Set ) return Positive is
   begin
      return Positive( Person_Data_Package.Length( data.people ));
   end Number_Of_People;
 
   function Load_Binary( file_name : String ) return EU_Data_Set is
      ds : EU_Data_Set;
      household_file : Household_IO.File_Type;
      person_file : Person_IO.File_Type;
      hh_data   : Household_Data;
      person    : Person_Rec;
      pno       : Natural := 0;
      hno       : Natural := 0;
   begin
      Household_IO.Open( household_file, Household_IO.In_File, file_name & ".hhd" );
      loop
         hno := hno + 1;
         Put_Line( "on hno " & hno'Img );
         
         exit when Household_IO.End_Of_File( household_file );
         Household_IO.Read( household_file, hh_data );
         ds.households.Append( hh_data ); 
      end loop;
      Household_IO.Close( household_file );
      
      Person_IO.Open( person_file, Person_IO.In_File, file_name & ".ped" );
      loop
         pno := pno + 1;
         Put_Line( "on pno " & pno'Img );
         exit when Person_IO.End_Of_File( person_file );
         Person_IO.Read( person_file, person );
         ds.people.Append( person ); 
      end loop;
      Person_IO.Close( person_file );
      return ds;
   end Load_Binary;
   
   function Format( b : Boolean ) return String is
   begin
      if( b ) then return "1"; else return "0"; end if;
   end Format;

   procedure Allocate_Personal_To_Household( hh : in out Household_Rec ) is
   begin
      hh.assets_main_residence_number_of_rooms :=  hh.people(1).assets_main_residence_number_of_rooms;
      hh.assets_main_residence_tenure := hh.people(1).assets_main_residence_tenure;
      hh.expenditure_housing_cost := 0.0;
      hh.expenditure_housing_cost_rent := 0.0;
      hh.expenditure_housing_cost_mortgage_payment_interest_and_capital_mortgage_interest := 0.0;
      for  pno in 1 .. hh.num_people loop 
         hh.expenditure_housing_cost := hh.expenditure_housing_cost + 
            hh.people( pno ).expenditure_housing_cost;
         hh.expenditure_housing_cost_rent := hh.expenditure_housing_cost_rent + 
            hh.people( pno ).expenditure_housing_cost_rent;
         hh.expenditure_housing_cost_mortgage_payment_interest_and_capital_mortgage_interest := 
            hh.expenditure_housing_cost_mortgage_payment_interest_and_capital_mortgage_interest + 
            hh.people( pno ).expenditure_housing_cost_mortgage_payment_interest_and_capital_mortgage_interest;
         if( hh.people( pno ).expenditure_housing_cost_mortgage_payment_interest_and_capital_mortgage_interest > 0.0 ) or
           ( hh.people( pno ).expenditure_housing_cost_rent > 0.0 ) or
           ( hh.people( pno ).expenditure_housing_cost > 0.0 ) then
           hh.people( pno ).responsible_for_housing_costs := True;
         end if;
      end loop;
   end Allocate_Personal_To_Household;

   
   
   procedure Create_Binary_Dataset( text_file_name : String; binary_file_name : String; flemish_only : Boolean ) is
      lines : Text_Buffer.Buffer;
      household_file : Household_IO.File_Type;
      person_file : Person_IO.File_Type;
      line : Unbounded_String;
      person               : Person_Rec;
      hh                   : Household_Rec;
      last_household       : Integer := -1;
   begin
      Put_Line( "Create_Binary_Dataset; reading from " & text_file_name & " and writing to " & binary_file_name );
      lines := Text_Buffer.Load( text_file_name ); 
      Household_IO.Create( household_file, Household_IO.Out_File, binary_file_name & ".hhd" );
      Person_IO.Create( person_file, Person_IO.Out_File, binary_file_name & ".ped" );
      Put_Line( "Files Opended OK" );
      for l in 2 .. lines.Num_Lines loop
         line := lines.Get_Line( l );
         person := Read_Person( line );
         Person_IO.Write( person_file, person );
         if( last_household /= person.identifier_household ) then
            if( last_household /= -1 ) then
                  Allocate_Personal_To_Household( hh );
                  if(( not flemish_only ) or ( hh.people( 1 ).demographic_region_nuts_level_1 = flanders ))then
                     Household_IO.Write( household_file, Household_Data(hh) );
                  end if;
                  -- Put_Line( "writing h.first_person_record of " & hh.first_person_record'Img & " num people " &  hh.num_people'Img );
                  hh.num_people := 0;
            end if;
            hh.first_person_record := l - 1;
            last_household := person.identifier_household;
         end if;
         hh.num_people := hh.num_people + 1;
         hh.people( hh.num_people ) := person;                           
      end loop;
      if(( not flemish_only ) or ( hh.people( 1 ).demographic_region_nuts_level_1 = flanders ))then
         Household_IO.Write( household_file, Household_Data(hh) );
         Put_Line( "writing h.first_person_record of " & hh.first_person_record'Img & " num people " &  hh.num_people'Img );
      end if;
      Household_IO.Close( household_file );
      Person_IO.Close( person_file );
   end Create_Binary_Dataset;
   
   function Read_Person( line : Unbounded_String ) return Person_Rec is
      use GNAT.String_Split;
      use EU.Conversion_Utils;
      use EU_Types;
      use BE_Types;
      person   : Person_Rec;
      slices   : Slice_Set;
   begin
      slices := TDA_Tokenize( To_String( line ));
      
      -- ======== AUTOGENERATED STARTS
      -- person.identifier_household := Convert( slices, 1 );
      -- person.identifier_person := Convert( slices, 2 );
      -- person.identifier_father := Convert( slices, 3 );
      -- person.identifier_mother := Convert( slices, 4 );
      -- person.identifier_partner := Convert( slices, 5 );
      -- person.original_household_id := Convert( slices, 6 );
      -- person.original_person_id := Convert( slices, 7 );
      -- person.demographic_age := Convert( slices, 8 );
      -- person.demographic_gender := Convert( Get_String( slices, 9 ));
      -- person.demographic_marital_status := Convert( Get_String( slices, 10 ));
      -- person.demographic_citizenship := Convert( Get_String( slices, 11 ));
      -- person.demographic_disability := Convert( slices, 12 );
      -- person.demographic_date_of_interview := Convert( slices, 13 );
      -- person.demographic_weight := Convert( slices, 14 );
      -- person.demographic_sample_units_main_or_basic := Convert( slices, 15 );
      -- person.demographic_sample_units_01 := Convert( slices, 16 );
      -- person.demographic_sample_units_02 := Convert( slices, 17 );
      -- person.demographic_country := Convert( slices, 18 );
      -- person.demographic_education_current_status := Convert( Get_String( slices, 19 ));
      -- person.demographic_education_highest_status := Convert( Get_String( slices, 20 ));
      -- person.demographic_education_number_of_years := Convert( slices, 21 );
      -- person.demographic_education_when_achieved_highest_status := Convert( slices, 22 );
      -- person.demographic_region_nuts_level_1 := Convert( Get_String( slices, 23 ));
      -- person.demographic_consensual_union := Convert( Get_String( slices, 24 ));
      -- person.labour_market_economic_status := Convert( slices, 25 );
      -- person.labour_market_civil_servant := Convert( slices, 26 );
      -- person.labour_market_firm_size := Convert( slices, 27 );
      -- person.labour_market_industry_nace := Convert( slices, 28 );
      -- person.labour_market_industry_nace_detailed_industry := Convert( slices, 29 );
      -- person.labour_market_in_work_months_per_year := Convert( slices, 30 );
      -- person.labour_market_hours_worked_per_week := Convert( slices, 31 );
      -- person.labour_market_in_work_work_history_length_of_time_in_months := Convert( slices, 32 );
      -- person.labour_market_occupation_isco_1_digit := Convert( Get_String( slices, 33 ));
      -- person.labour_market_pensioner_months_per_year := Convert( slices, 34 );
      -- person.labour_market_self_employed := Convert( slices, 35 );
      -- person.labour_market_unemployed_months_per_year := Convert( slices, 36 );
      -- person.labour_market_out_of_work_actively_seeking := Convert( slices, 37 );
      -- person.income_imputed_value_wage_or_salary := Convert( slices, 38 );
      -- person.income_employment := Convert( slices, 39 );
      -- person.in_kind_fringe_benefit := Convert( slices, 40 );
      -- person.income_private_pension := Convert( slices, 41 );
      -- person.income_self_employment := Convert( slices, 42 );
      -- person.benefit_education := Convert( slices, 43 );
      -- person.benefit_unemployment := Convert( slices, 44 );
      -- person.pension_health := Convert( slices, 45 );
      -- person.pension_disability := Convert( slices, 46 );
      -- person.pension_old_age := Convert( slices, 47 );
      -- person.pension_survivors := Convert( slices, 48 );
      -- person.in_kind_imputed_value_housing := Convert( slices, 49 );
      -- person.income_investment := Convert( slices, 50 );
      -- person.income_property_rent := Convert( slices, 51 );
      -- person.income_property := Convert( slices, 52 );
      -- person.income_private_transfers := Convert( slices, 53 );
      -- person.benefit_social_assistance := Convert( slices, 54 );
      -- person.benefit_housing := Convert( slices, 55 );
      -- person.tax_property_tax := Convert( slices, 56 );
      -- person.income_other := Convert( slices, 57 );
      -- person.income_disposable := Convert( slices, 58 );
      -- person.tax_repayments_or_receipts := Convert( slices, 59 );
      -- person.benefit_child_main_or_basic := Convert( slices, 60 );
      -- person.benefit_child_birth_or_adoption := Convert( slices, 61 );
      -- person.benefit_child := Convert( slices, 62 );
      -- person.benefit_maternity := Convert( slices, 63 );
      -- person.benefit_family_parental_leave := Convert( slices, 64 );
      -- -- person.benefit_family := Convert( slices, 65 );
      -- person.income_employment_months_per_year := Convert( slices, 65 );
      -- person.in_kind_fringe_benefit_months_per_year := Convert( slices, 66 );
      -- person.income_self_employment_months_per_year := Convert( slices, 67 );
      -- person.benefit_unemployment_months_per_year := Convert( slices, 68 );
      -- person.pension_disability_months_per_year := Convert( slices, 69 );
      -- person.pension_health_months_per_year := Convert( slices, 70 );
      -- person.pension_old_age_months_per_year := Convert( slices, 71 );
      -- person.pension_survivors_months_per_year := Convert( slices, 72 );
      -- person.tax_income_tax_and_sics := Convert( slices, 73 );
      -- person.benefit_early_retirement := Convert( slices, 74 );
      -- person.benefit_early_retirement_early_retirement_pension_months_per_year := Convert( slices, 75 );
      -- person.assets_main_residence_number_of_rooms := Convert( slices, 76 );
      -- person.assets_main_residence_tenure := Convert( Get_String( slices, 77 ));
      -- person.assets_financial_capital := Convert( slices, 78 );
      -- person.expenditure_maintenance_payment := Convert( slices, 79 );
      -- person.expenditure_private_pension_voluntary := Convert( slices, 80 );
      -- person.expenditure_housing_cost := Convert( slices, 81 );
      -- person.expenditure_housing_cost_rent := Convert( slices, 82 );
      -- person.expenditure_housing_cost_mortgage_payment_interest_and_capital_mortgage_interest := Convert( slices, 83 );
      
      person.identifier_household := Convert( slices, 1 );
      person.identifier_person := Convert( slices, 2 );
      person.identifier_father := Convert( slices, 3 );
      person.identifier_mother := Convert( slices, 4 );
      person.identifier_partner := Convert( slices, 5 );
      person.idorighh := Convert( slices, 6 );
      person.idorigperson := Convert( slices, 7 );
      person.demographic_age := Convert( slices, 8 );
      person.demographic_gender := Convert( Get_String( slices, 9 ));
      person.demographic_marital_status := Convert( Get_String( slices, 10 ));
      person.demographic_citizenship := Convert( Get_String( slices, 11 ));
      person.demographic_disability := Convert( slices, 12 );
      person.demographic_date_of_interview := Convert( slices, 13 );
      person.demographic_weight := Convert( slices, 14 );
      person.demographic_sample_units_main_or_basic := Convert( slices, 15 );
      person.demographic_sample_units_01 := Convert( slices, 16 );
      person.demographic_sample_units_02 := Convert( slices, 17 );
      person.demographic_country := Convert( slices, 18 );
      person.demographic_education_current_status := Convert( Get_String( slices, 19 ));
      person.demographic_education_highest_status := Convert( Get_String( slices, 20 ));
      person.demographic_education_number_of_years := Convert( slices, 21 );
      person.demographic_education_when_achieved_highest_status := Convert( slices, 22 );
      person.demographic_region_nuts_level_1 := Convert( Get_String( slices, 23 ));
      person.demographic_consensual_union := Convert( Get_String( slices, 24 ));
      person.labour_market_economic_status := Convert( slices, 25 );
      person.labour_market_civil_servant := Convert( slices, 26 );
      person.labour_market_firm_size := Convert( slices, 27 );
      person.labour_market_industry_nace := Convert( slices, 28 );
      person.labour_market_industry_nace_detailed_industry := Convert( slices, 29 );
      person.labour_market_in_work_months_per_year := Convert( slices, 30 );
      person.labour_market_hours_worked_per_week := Convert( slices, 31 );
      person.labour_market_in_work_work_history_length_of_time_in_months := Convert( slices, 32 );
      person.labour_market_occupation_isco_1_digit := Convert( Get_String( slices, 33 ));
      person.labour_market_pensioner_months_per_year := Convert( slices, 34 );
      person.labour_market_self_employed := Convert( slices, 35 );
      person.labour_market_unemployed_months_per_year := Convert( slices, 36 );
      person.labour_market_out_of_work_actively_seeking := Convert( slices, 37 );
      person.income_imputed_value_wage_or_salary := Convert( slices, 38 );
      person.income_employment := Convert( slices, 39 );
      person.in_kind_fringe_benefit := Convert( slices, 40 );
      person.income_private_pension := Convert( slices, 41 );
      person.income_self_employment := Convert( slices, 42 );
      person.benefit_education := Convert( slices, 43 );
      person.benefit_unemployment := Convert( slices, 44 );
      person.pension_health := Convert( slices, 45 );
      person.pension_disability := Convert( slices, 46 );
      person.pension_old_age := Convert( slices, 47 );
      person.pension_survivors := Convert( slices, 48 );
      person.in_kind_imputed_value_housing := Convert( slices, 49 );
      person.income_investment := Convert( slices, 50 );
      person.income_property_rent := Convert( slices, 51 );
      person.income_property := Convert( slices, 52 );
      person.income_private_transfers := Convert( slices, 53 );
      person.benefit_social_assistance := Convert( slices, 54 );
      person.benefit_housing := Convert( slices, 55 );
      person.tax_property_tax := Convert( slices, 56 );
      person.income_other := Convert( slices, 57 );
      person.income_disposable := Convert( slices, 58 );
      person.tax_repayments_or_receipts := Convert( slices, 59 );
      person.benefit_child_main_or_basic := Convert( slices, 60 );
      person.benefit_child_birth_or_adoption := Convert( slices, 61 );
      person.benefit_child := Convert( slices, 62 );
      person.benefit_maternity := Convert( slices, 63 );
      person.benefit_family_parental_leave := Convert( slices, 64 );
      person.in_kind_housing_owned_occupied := Convert( slices, 65 );
      person.income_employment_months_per_year := Convert( slices, 66 );
      person.in_kind_fringe_benefit_months_per_year := Convert( slices, 67 );
      person.income_self_employment_months_per_year := Convert( slices, 68 );
      person.benefit_unemployment_months_per_year := Convert( slices, 69 );
      person.pension_disability_months_per_year := Convert( slices, 70 );
      person.pension_health_months_per_year := Convert( slices, 71 );
      person.pension_old_age_months_per_year := Convert( slices, 72 );
      person.pension_survivors_months_per_year := Convert( slices, 73 );
      person.tax_income_tax_and_sics := Convert( slices, 74 );
      person.benefit_early_retirement := Convert( slices, 75 );
      person.benefit_early_retirement_early_retirement_pension_months_per_year := Convert( slices, 76 );
      person.assets_main_residence_number_of_rooms := Convert( slices, 77 );
      person.assets_main_residence_tenure := Convert( Get_String( slices, 78 ));
      person.assets_financial_capital := Convert( slices, 79 );
      person.expenditure_maintenance_payment := Convert( slices, 80 );
      person.expenditure_private_pension_voluntary := Convert( slices, 81 );
      person.expenditure_housing_cost := Convert( slices, 82 );
      person.expenditure_housing_cost_rent := Convert( slices, 83 );
      person.expenditure_housing_cost_mortgage_payment_interest_and_capital_mortgage_interest := Convert( slices, 84 );
      person.expenditure_housing_cost_mortgage_payment_interest_and_capital_mortgage_capital := Convert( slices, 85 );
      person.assets_mortgage_year_of_loan := Convert( slices, 86 );
      person.assets_mortgage_loan_value := Convert( slices, 87 );      
      
      
      return person;
   end Read_Person;

   procedure Allocate_Household_To_Personal( hh : in out Household_Rec ) is
      num_hresp : Amount := 0.0;
   begin
      for pno in 1 .. hh.num_people loop
         hh.people( pno ).assets_main_residence_number_of_rooms := hh.assets_main_residence_number_of_rooms;
         hh.people( pno ).assets_main_residence_tenure := hh.assets_main_residence_tenure;

         if( hh.people( pno ).responsible_for_housing_costs ) then
            num_hresp := num_hresp + 1.0;
         end if;
      end loop;
      if( num_hresp = 0.0 ) then
         hh.people(1).expenditure_housing_cost := hh.expenditure_housing_cost;
         hh.people(1).expenditure_housing_cost_rent := hh.expenditure_housing_cost_rent;
         hh.people(1).expenditure_housing_cost_mortgage_payment_interest_and_capital_mortgage_interest :=
            hh.expenditure_housing_cost_mortgage_payment_interest_and_capital_mortgage_interest; 
      else
         for pno in 1 .. hh.num_people loop
            if( hh.people( pno ).responsible_for_housing_costs ) then
               hh.people( pno ).expenditure_housing_cost := hh.expenditure_housing_cost / num_hresp;
               hh.people( pno ).expenditure_housing_cost_rent := hh.expenditure_housing_cost_rent / num_hresp;
               hh.people( pno ).expenditure_housing_cost_mortgage_payment_interest_and_capital_mortgage_interest :=
                  hh.expenditure_housing_cost_mortgage_payment_interest_and_capital_mortgage_interest / num_hresp;
            end if;
         end loop;
      end if;
   end Allocate_Household_To_Personal;
   
   procedure Write_Household_Header( file : File_Type ) is
   begin
      Put_Line( file, "idhh,idperson,idfather,idmother,idpartner,idorighh,idorigperson,dag,dgn,dms,dcz,ddi,ddt,dwt,dcu,dct,dec,deh,dey,dew,drgn1,lcs,lfs,lin,lindi,liwmy,lhw,liwwh,loc,lpemy,lse,lunmy,lowas,les,lyrmy,yivwg,yem,yemmy,kfb,ypp,yse,bed,bun,bunmy,phl,pdi,poa,psu,kivho,yiy,ypr,ypt,bfa,bsa,bho,yot,yds,tad,bma,tscee,tscse,tscer,tin,tis,byr,amrrm,amrtn,afc,xmp,xhc,xhcrt,xhcmomi" );
   end Write_Household_Header;
   
   procedure Write_Household( hh : in Household_Rec; file : File_Type ) is
      SEP : constant String := ",";
      person : Person_Rec;
   begin
      
      for pno in 1 .. hh.num_people loop
         person := hh.people( pno );
         -- =========== AUTOGENERATED STARTS ============

         -- Put( file, Format( person.identifier_household ));
         -- Put( file, Format( person.identifier_person ));
         -- Put( file, Format( person.identifier_father ));
         -- Put( file, Format( person.identifier_mother ));
         -- Put( file, Format( person.identifier_partner ));
         -- Put( file, Format( person.original_household_id ));
         -- Put( file, Format( person.original_person_id ));
         -- Put( file, Format( person.demographic_age ));
         -- Put( file, Format( Value( person.demographic_gender ) ));
         -- Put( file, Format( Value( person.demographic_marital_status ) ));
         -- Put( file, Format( Value( person.demographic_citizenship ) ));
         -- Put( file, Format( person.demographic_disability ));
         -- Put( file, Format( person.demographic_date_of_interview ));
         -- Put( file, Format( person.demographic_weight ));
         -- Put( file, Format( person.demographic_sample_units_main_or_basic ));
         -- Put( file, Format( person.demographic_sample_units_01 ));
         -- Put( file, Format( person.demographic_sample_units_02 ));
         -- Put( file, Format( person.demographic_country ));
         -- Put( file, Format( Value( person.demographic_education_current_status ) ));
         -- Put( file, Format( Value( person.demographic_education_highest_status ) ));
         -- Put( file, Format( person.demographic_education_number_of_years ));
         -- Put( file, Format( person.demographic_education_when_achieved_highest_status ));
         -- Put( file, Format( Value( person.demographic_region_nuts_level_1 ) ));
         -- Put( file, Format( Value( person.demographic_consensual_union ) ));
         -- Put( file, Format( person.labour_market_economic_status ));
         -- Put( file, Format( person.labour_market_civil_servant ));
         -- Put( file, Format( person.labour_market_firm_size ));
         -- Put( file, Format( person.labour_market_industry_nace ));
         -- Put( file, Format( person.labour_market_industry_nace_detailed_industry ));
         -- Put( file, Format( person.labour_market_in_work_months_per_year ));
         -- Put( file, Format( person.labour_market_hours_worked_per_week ));
         -- Put( file, Format( person.labour_market_in_work_work_history_length_of_time_in_months ));
         -- Put( file, person.labour_market_occupation_isco_1_digit'Img );
         -- Put( file, Format( person.labour_market_pensioner_months_per_year ));
         -- Put( file, Format( person.labour_market_self_employed ));
         -- Put( file, Format( person.labour_market_unemployed_months_per_year ));
         -- Put( file, Format( person.labour_market_out_of_work_actively_seeking ));
         -- Put( file, Format( person.income_imputed_value_wage_or_salary ));
         -- Put( file, Format( person.income_employment ));
         -- Put( file, Format( person.in_kind_fringe_benefit ));
         -- Put( file, Format( person.income_private_pension ));
         -- Put( file, Format( person.income_self_employment ));
         -- Put( file, Format( person.benefit_education ));
         -- Put( file, Format( person.benefit_unemployment ));
         -- Put( file, Format( person.pension_health ));
         -- Put( file, Format( person.pension_disability ));
         -- Put( file, Format( person.pension_old_age ));
         -- Put( file, Format( person.pension_survivors ));
         -- Put( file, Format( person.in_kind_imputed_value_housing ));
         -- Put( file, Format( person.income_investment ));
         -- Put( file, Format( person.income_property_rent ));
         -- Put( file, Format( person.income_property ));
         -- Put( file, Format( person.income_private_transfers ));
         -- Put( file, Format( person.benefit_social_assistance ));
         -- Put( file, Format( person.benefit_housing ));
         -- Put( file, Format( person.tax_property_tax ));
         -- Put( file, Format( person.income_other ));
         -- Put( file, Format( person.income_disposable ));
         -- Put( file, Format( person.tax_repayments_or_receipts ));
         -- Put( file, Format( person.benefit_child_main_or_basic ));
         -- Put( file, Format( person.benefit_child_birth_or_adoption ));
         -- Put( file, Format( person.benefit_child ));
         -- Put( file, Format( person.benefit_maternity ));
         -- Put( file, Format( person.benefit_family_parental_leave ));
         -- Put( file, Format( person.benefit_family ));
         -- Put( file, Format( person.income_employment_months_per_year ));
         -- Put( file, Format( person.in_kind_fringe_benefit_months_per_year ));
         -- Put( file, Format( person.income_self_employment_months_per_year ));
         -- Put( file, Format( person.benefit_unemployment_months_per_year ));
         -- Put( file, Format( person.pension_disability_months_per_year ));
         -- Put( file, Format( person.pension_health_months_per_year ));
         -- Put( file, Format( person.pension_old_age_months_per_year ));
         -- Put( file, Format( person.pension_survivors_months_per_year ));
         -- Put( file, Format( person.tax_income_tax_and_sics ));
         -- Put( file, Format( person.benefit_early_retirement ));
         -- Put( file, Format( person.benefit_early_retirement_early_retirement_pension_months_per_year ));
         -- Put( file, Format( person.assets_main_residence_number_of_rooms ));
         -- Put( file, Format( Value( person.assets_main_residence_tenure ) ));
         -- Put( file, Format( person.assets_financial_capital ));
         -- Put( file, Format( person.expenditure_maintenance_payment ));
         -- Put( file, Format( person.expenditure_private_pension_voluntary ));
         -- Put( file, Format( person.expenditure_housing_cost ));
         -- Put( file, Format( person.expenditure_housing_cost_rent ));
         -- Put( file, Format( person.expenditure_housing_cost_mortgage_payment_interest_and_capital_mortgage_interest ));         
            Put( file, Format( person.identifier_household ));
            Put( file, Format( person.identifier_person ));
            Put( file, Format( person.identifier_father ));
            Put( file, Format( person.identifier_mother ));
            Put( file, Format( person.identifier_partner ));
            Put( file, Format( person.idorighh ));
            Put( file, Format( person.idorigperson ));
            Put( file, Format( person.demographic_age ));
            Put( file, Format( Value( person.demographic_gender ) ));
            Put( file, Format( Value( person.demographic_marital_status ) ));
            Put( file, Format( Value( person.demographic_citizenship ) ));
            Put( file, Format( person.demographic_disability ));
            Put( file, Format( person.demographic_date_of_interview ));
            Put( file, Format( person.demographic_weight ));
            Put( file, Format( person.demographic_sample_units_main_or_basic ));
            Put( file, Format( person.demographic_sample_units_01 ));
            Put( file, Format( person.demographic_sample_units_02 ));
            Put( file, Format( person.demographic_country ));
            Put( file, Format( Value( person.demographic_education_current_status ) ));
            Put( file, Format( Value( person.demographic_education_highest_status ) ));
            Put( file, Format( person.demographic_education_number_of_years ));
            Put( file, Format( person.demographic_education_when_achieved_highest_status ));
            Put( file, Format( Value( person.demographic_region_nuts_level_1 ) ));
            Put( file, Format( Value( person.demographic_consensual_union ) ));
            Put( file, Format( person.labour_market_economic_status ));
            Put( file, Boolean'Image( person.labour_market_civil_servant ));
            Put( file, Format( person.labour_market_firm_size ));
            Put( file, Format( person.labour_market_industry_nace ));
            Put( file, Format( person.labour_market_industry_nace_detailed_industry ));
            Put( file, Format( person.labour_market_in_work_months_per_year ));
            Put( file, Format( person.labour_market_hours_worked_per_week ));
            Put( file, Format( person.labour_market_in_work_work_history_length_of_time_in_months ));
            Put( file, Occupation_Isco_1_Digit'Image( person.labour_market_occupation_isco_1_digit ));
            Put( file, Format( person.labour_market_pensioner_months_per_year ));
            Put( file, Format( person.labour_market_self_employed ));
            Put( file, Format( person.labour_market_unemployed_months_per_year ));
            Put( file, Format( person.labour_market_out_of_work_actively_seeking ));
            Put( file, Format( person.income_imputed_value_wage_or_salary ));
            Put( file, Format( person.income_employment ));
            Put( file, Format( person.in_kind_fringe_benefit ));
            Put( file, Format( person.income_private_pension ));
            Put( file, Format( person.income_self_employment ));
            Put( file, Format( person.benefit_education ));
            Put( file, Format( person.benefit_unemployment ));
            Put( file, Format( person.pension_health ));
            Put( file, Format( person.pension_disability ));
            Put( file, Format( person.pension_old_age ));
            Put( file, Format( person.pension_survivors ));
            Put( file, Format( person.in_kind_imputed_value_housing ));
            Put( file, Format( person.income_investment ));
            Put( file, Format( person.income_property_rent ));
            Put( file, Format( person.income_property ));
            Put( file, Format( person.income_private_transfers ));
            Put( file, Format( person.benefit_social_assistance ));
            Put( file, Format( person.benefit_housing ));
            Put( file, Format( person.tax_property_tax ));
            Put( file, Format( person.income_other ));
            Put( file, Format( person.income_disposable ));
            Put( file, Format( person.tax_repayments_or_receipts ));
            Put( file, Format( person.benefit_child_main_or_basic ));
            Put( file, Format( person.benefit_child_birth_or_adoption ));
            Put( file, Format( person.benefit_child ));
            Put( file, Format( person.benefit_maternity ));
            Put( file, Format( person.benefit_family_parental_leave ));
            Put( file, Format( person.in_kind_housing_owned_occupied ));
            Put( file, Format( person.income_employment_months_per_year ));
            Put( file, Format( person.in_kind_fringe_benefit_months_per_year ));
            Put( file, Format( person.income_self_employment_months_per_year ));
            Put( file, Format( person.benefit_unemployment_months_per_year ));
            Put( file, Format( person.pension_disability_months_per_year ));
            Put( file, Format( person.pension_health_months_per_year ));
            Put( file, Format( person.pension_old_age_months_per_year ));
            Put( file, Format( person.pension_survivors_months_per_year ));
            Put( file, Format( person.tax_income_tax_and_sics ));
            Put( file, Format( person.benefit_early_retirement ));
            Put( file, Format( person.benefit_early_retirement_early_retirement_pension_months_per_year ));
            Put( file, Format( person.assets_main_residence_number_of_rooms ));
            Put( file, Tenure_Type'Image( person.assets_main_residence_tenure ));
            Put( file, Format( person.assets_financial_capital ));
            Put( file, Format( person.expenditure_maintenance_payment ));
            Put( file, Format( person.expenditure_private_pension_voluntary ));
            Put( file, Format( person.expenditure_housing_cost ));
            Put( file, Format( person.expenditure_housing_cost_rent ));
            Put( file, Format( person.expenditure_housing_cost_mortgage_payment_interest_and_capital_mortgage_interest ));
            Put( file, Format( person.expenditure_housing_cost_mortgage_payment_interest_and_capital_mortgage_capital ));
            Put( file, Format( person.assets_mortgage_year_of_loan ));
            Put( file, Format( person.assets_mortgage_loan_value ));      
         -- =========== AUTOGENERATED ENDS ============
      end loop;

      -- .
      -- o Capital and property income must be shared equally between the oldest
      -- household member and his/her partner (the underlying assumption being
      -- that
      -- in the case of three generation households it would be probably the
      -- oldest
      -- couple who would retain this kind of income). (TBD)
      -- o Any monetary variable related to housing at the household level in the
      -- original dataset (housing allowances, imputed rent, housing cost) must
      -- be
      -- assigned to the person(s) responsible for the accommodation. If there
      -- are
      -- two of such person then the amount is shared equally between them.
      -- o Any non-monetary variable at the household level in the original
      -- dataset
      -- should be assigned to all the persons in the household.
      --
   
      
   end Write_Household;

end EU.BE.Household.IO;
