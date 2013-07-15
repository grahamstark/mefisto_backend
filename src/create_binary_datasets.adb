
with EU.BE.Household.IO;
with EU.BE.Model.Settings;
with EU.BE.Results.IO;
with EU.BE.Results;
with EU.Web.Settings;
with EU.BE.Output;
with EU.BE.Globals;
with EU.BE.Parameter_System_Declarations;

with Ada.Strings.Unbounded;
with Ada.Direct_IO;
with Ada.Text_IO;
with Ada.Assertions;
with Ada.Command_Line;

with BE_Types;
with Text_Buffer;
with Inequality_Generator;
with BE_Base_Model_Types;

procedure Create_Binary_Datasets is

   use Ada.Assertions;
   use Ada.Strings.Unbounded;
   use Ada.Text_IO;
   use BE_Types;
   use BE_Base_Model_Types;
   use EU.BE.Household.IO;
   use EU.BE.Household;
   use EU.BE.Results;
   
   package Household_IO is new Ada.Direct_IO( Household_Data );
   package Person_IO is new Ada.Direct_IO( Person_Rec );
   package Be_Inequality is new Inequality_Generator( Amount=>Amount, Rate=>Rate );
   package globals renames EU.BE.Globals;
   use Be_Inequality;
   
   num_people_in_sample     : Positive;
   num_households_in_sample : Positive;

   equiv_hh_incomes     : Quantile_List;
   equiv_person_incomes : Quantile_List;
   hh_incomes           : Quantile_List;
   person_incomes       : Quantile_List;
  
   person_income_rank       : Rank_List;
   household_income_rank        : Rank_List;
   equiv_person_income_rank : Rank_List;
   equiv_household_income_rank  : Rank_List;
   
   num_people : Amount := 0.0;
   num_households : Amount := 0.0;

   function XTile( cumulative_population : Amount; n : Positive; total_population : Amount ) return Positive is
      ii : Positive;
   begin
      if( cumulative_population = 0.0 )then
         ii := 1;
      elsif( cumulative_population = total_population )then
         ii := n;
      else
         Put_Line( " cumulative_population " & cumulative_population'Img );
         Put_Line( " n " & n'Img );
         Put_Line( " total_population " & total_population'Img );
         ii := Positive( Amount'Ceiling( cumulative_population * Amount( n ) / total_population ));
         Assert( ii <= n, " XTile should always be <= " & n'Img & " was " & ii'Img &
                         " cumulative_population " & cumulative_population'Img & 
                         " total_population " & total_population'Img );
                         
      end if;
      return ii;
   end XTile;
   
   procedure Create_Binary_Dataset( 
      text_file_name : String; 
      binary_file_name : String; 
      flemish_only : Boolean; 
      results : Detailed_Results_List;
      add_derived : Boolean ) is
      lines             : Text_Buffer.Buffer;
      household_file    : Household_IO.File_Type;
      person_file       : Person_IO.File_Type;
      line              : Unbounded_String;
      person            : Person_Rec;
      household         : Household_Rec;
      last_household    : Integer := -1;
      result            : Detailed_Record;
      hhref             : Natural := 0;
      hh_rank           : Ranking_Rec;
      equiv_hh_rank     : Ranking_Rec;
      equiv_pers_rank   : Ranking_Rec;
      ii                : Integer;
   begin
      Put_Line( "Create_Binary_Dataset; reading from " & text_file_name & " and writing to " & binary_file_name );
      lines := Text_Buffer.Load( text_file_name ); 
      Household_IO.Create( household_file, Household_IO.Out_File, binary_file_name & ".hhd" );
      Person_IO.Create( person_file, Person_IO.Out_File, binary_file_name & ".ped" );
      Put_Line( "Files Opended OK" );
      for l in 2 .. lines.Num_Lines loop
         line := lines.Get_Line( l );
         person := Read_Person( line );
         if( add_derived )then
            equiv_pers_rank := equiv_person_income_rank.Element( l-1 );
            Assert( equiv_pers_rank.index = person.identifier_household, 
               "equiv_pers_rank.index /= household.identifier_household; hhref=|" & Integer'Image( hhref ) &
               "| equiv_pers_rank.index = " & Integer'Image( equiv_pers_rank.index ) & 
               "| person = |" & Integer'Image( person.identifier_household ));
            person.equivalent_income_quintile := XTile( equiv_pers_rank.cumulative_population, 5, num_people );
            person.equivalent_income_decile := XTile( equiv_pers_rank.cumulative_population, 10, num_people );
            person.equivalent_income_rank := equiv_pers_rank.rank;
            person.net_income_rank := equiv_pers_rank.rank;
         end if;
         Person_IO.Write( person_file, person );
            
         if( last_household /= person.identifier_household ) then
            if( last_household /= -1 ) then
                  EU.BE.Household.IO.Allocate_Personal_To_Household( household );
                  if(( not flemish_only ) or ( household.people( 1 ).demographic_region_nuts_level_1 = flanders ))then
                     hhref := hhref + 1;
                     if( add_derived )then
                        result := Results.Element( hhref );
                        equiv_hh_rank := equiv_household_income_rank.Element( hhref );
                        hh_rank := household_income_rank.Element( hhref );
                        Assert( equiv_hh_rank.index = household.people( 1 ).identifier_household, 
                           "equiv_hh_rank.index /= household.identifier_household; hhref=|" & Integer'Image( hhref ) &
                           "| equiv_hh_rank.index = " & Integer'Image( equiv_hh_rank.index ) & 
                           "| household.identifier_household = |" & Integer'Image( household.people( 1 ).identifier_household ));
                           
                        Put_Line( "result.identifier_household " & Positive'Image( result.identifier_household ));
                        Put_Line( "num_people_in_sample=|" & Positive'Image( num_people_in_sample ) &  "| num_households_in_sample " & Positive'Image( num_households_in_sample ));
                        Put_Line( "rank people=|" & Positive'Image( equiv_pers_rank.rank ) &  "| rank_households " & Positive'Image( equiv_hh_rank.rank ));
                        
                        Assert( result.identifier_household = household.people( 1 ).identifier_household, 
                           "result.identifier_household /= household.identifier_household; hhref=" & Integer'Image( hhref ) &
                           "result.identifier_household = " & Integer'Image( result.identifier_household ) & 
                           "household.identifier_household = " & Integer'Image( household.people( 1 ).identifier_household ));
                        for p in 1 .. household.Num_People loop
                           household.people( p ).equivalent_income := result.standard_disposable_income/household.Get_Equivalence_Scale;
                        end loop;
                        household.equivalent_income := result.standard_disposable_income/household.Get_Equivalence_Scale;
                        ii := Natural( Amount'Ceiling( equiv_hh_rank.cumulative_population * 5.0 / num_households ));
                        Put_Line( "equiv_hh_rank.cumulative_population " &  equiv_hh_rank.cumulative_population'Img & 
                                  "num_households " & num_households'Img & 
                                  "  Amount'Ceiling( equiv_hh_rank.cumulative_population * 5.0 / num_households )) = " & ii'img );
                        
                        household.equivalent_income_quintile := XTile( equiv_hh_rank.cumulative_population, 5, num_households );
                        household.equivalent_income_decile := XTile( equiv_hh_rank.cumulative_population, 10, num_households );
                        household.net_income_quintile := XTile( hh_rank.cumulative_population, 5, num_households );
                        household.net_income_decile := XTile( hh_rank.cumulative_population, 10, num_households );
                     end if;
                     Household_IO.Write( household_file, Household_Data( household ));
                  end if;
                  -- Put_Line( "writing h.first_person_record of " & household.first_person_record'Img & " num people " &  household.num_people'Img );
                  household.num_people := 0;
            end if;
            household.first_person_record := l - 1;
            last_household := person.identifier_household;
         end if;
         household.num_people := household.num_people + 1;
         household.people( household.num_people ) := person;                           
      end loop;
      if(( not flemish_only ) or ( household.people( 1 ).demographic_region_nuts_level_1 = flanders ))then
         Household_IO.Write( household_file, Household_Data( household ) );
         Put_Line( "writing h.first_person_record of " & household.first_person_record'Img & " num people " &  household.num_people'Img );
      end if;
      Household_IO.Close( household_file );
      Person_IO.Close( person_file );
   end Create_Binary_Dataset;
   
   procedure Create_Rankings( results : Detailed_Results_List ) is
      use Quantile_Package;
      
      
      quant       : Quantile;
      result      : Detailed_Record;
      num_results : constant Positive := Positive( results.Length );
      household   : Household_Rec;
      households  : EU_Data_Set := globals.Get_Households;
      eq_scale    : Amount;
      n           : Positive := 1;
   begin
      --
      -- household level
      -- 
      for i in 1 .. num_results loop
         household := households.Read_Household( i );
         result := results.Element( i );
         quant.index := result.identifier_household;
         Put_Line( "result.identifier_household " & Positive'Image( result.identifier_household ));
         Assert( result.identifier_household = household.people( 1 ).identifier_household, 
            "result.identifier_household /= household.identifier_household; hhref=" & Integer'Image( i ) &
            "result.identifier_household = " & Integer'Image( result.identifier_household ) & 
            "household.identifier_household = " & Integer'Image( household.people( 1 ).identifier_household ));
         quant.population := result.demographic_weight;
         
         num_people :=  num_people +  quant.population * Amount( household.Num_People );
         num_households := num_households +  quant.population;

         quant.income :=  result.standard_disposable_income;
         if( quant.income <= 0.0 )then
            Put_Line( "ZERO INCOME FOR HH " & Positive'Image( result.identifier_household ));
            quant.income := 0.01;
         end if;
         hh_incomes.Append( quant );
         
         quant.income := result.standard_disposable_income / household.Get_Equivalence_Scale;
         if( quant.income <= 0.0 )then
            Put_Line( "ZERO EQUIVALISED INCOME FOR HH " & Positive'Image( result.identifier_household ));
            quant.income := 0.01;
         end if;
         equiv_hh_incomes.Append( quant );
         
         -- quant.income := result.standard_disposable_income / household.Get_Equivalence_Scale;
         for p in 1 .. household.Num_People loop
             equiv_person_incomes.Append( quant );     
         end loop;
         
      end loop;
      
      -- person_income_rank       := Be_Inequality.Rank_By_Income( 
      household_income_rank        := Be_Inequality.Rank_By_Income( equiv_hh_incomes );
      equiv_person_income_rank     := Be_Inequality.Rank_By_Income( equiv_person_incomes );
      equiv_household_income_rank  := Be_Inequality.Rank_By_Income( equiv_hh_incomes );
      Put_Line( "incomes =" );
      for i in 1 .. Natural( equiv_hh_incomes.Length ) loop
         Put_Line( i'Img & equiv_hh_incomes.Element( i ).index'Img & " " & equiv_hh_incomes.Element( i ).income'Img );
      end loop;
      Put_Line( "ranks =" );
      for i in 1 .. Natural(equiv_household_income_rank.Length) loop
         Put_Line( i'Img & equiv_household_income_rank.Element(i).index'Img & " " & equiv_household_income_rank.Element(i).Rank'Img );
      end loop;
      Put_Line( "pers incomes =" );
      for i in 1 .. Natural( equiv_person_incomes.Length ) loop
         Put_Line( i'Img & equiv_person_incomes.Element( i ).index'Img & " " & equiv_person_incomes.Element( i ).income'Img );
      end loop;
      Put_Line( "pers ranks =" );
      for i in 1 .. Natural(equiv_person_income_rank.Length) loop
         Put_Line( i'Img & equiv_person_income_rank.Element(i).index'Img & " " & equiv_person_income_rank.Element(i).Rank'Img );
      end loop;
   end Create_Rankings;
  
   package ms renames EU.BE.Model.Settings;
   use EU.BE.Results;
   
   model_sett        : ms.Model_Settings;
   username          : constant String := "default";
   run_name          : constant String := "default";
   hh_level_list_pre : Detailed_Results_List;
   add_derived       : Boolean := True;   
   
begin
   if( Ada.Command_Line.Argument_Count = 3 ) then -- windows case
      add_derived := Boolean'Value( Ada.Command_Line.Argument( 3 ));
   elsif( Ada.Command_Line.Argument_Count = 1 ) then -- unix case
      add_derived := Boolean'Value( Ada.Command_Line.Argument( 1 ));
   end if;
   EU.BE.Globals.Initialise_Settings_And_Datasets(
      include_base_results => True,
      load_household_data  => add_derived ); 
   model_sett := globals.Get_Default_Model_Settings ;
   hh_level_list_pre  := EU.BE.Globals.Get_HH_Level_List_Pre;
	Put_Line( "Test_Create_Dataset" );
	Put_Line( "Opening " & model_sett.Datafile_Directory & "/" & model_sett.Dataset_Name );
	Put_Line( "Writing to " & model_sett.Datafile_Directory & "/data_index.bin" );
	if( add_derived )then
	   Create_Rankings( hh_level_list_pre );
	end if;
	if( add_derived )then
      num_people_in_sample        := globals.Get_Households.Number_Of_People;
      num_households_in_sample    := globals.Get_Households.Number_Of_Households;
   end if;
   Create_Binary_Dataset( 
          text_file_name   => model_sett.Datafile_Directory & "/" & model_sett.Dataset_Name & ".txt",
          binary_file_name => model_sett.Datafile_Directory &  "/" & model_sett.Dataset_Name,
          flemish_only     => True,
          results          => hh_level_list_pre,
          add_derived      => add_derived );
end Create_Binary_Datasets;
