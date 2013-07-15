with Ada.Text_IO;

package body EU_Types is

   use Ada.Text_IO;
   
   function Value( i : Tenure_type ) return Integer is
    begin
         case i is
             when owned_on_mortgage => return 1;
             when owned_outright => return 2;
             when rented => return 3;
             when reduced_rented => return 4;
             when social_rented => return 5;
             when free => return 6;
             when other => return 7;
         end case;
    end Value;
    
    function Convert( i : String ) return Occupation_Isco_1_Digit is
    begin
       if( i = "1" )then
          return legislators_senior_officials_and_managers;
       elsif( i = "2" )then
          return professionals;
       elsif( i = "3" )then
          return technicians_and_associate_professionals;
      elsif( i = "4" )then
         return clerks;
      elsif( i = "5" )then
         return service_workers_and_shop_and_market_sales_workers;
      elsif( i = "6" )then
         return skilled_agricultural_and_fishery_workers;
      elsif( i = "7" )then
         return craft_and_related_trades_workers;
      elsif( i = "8" )then
         return plant_and_machine_operators_and_assemblers;
      elsif( i = "9" )then
         return elementary_occupations;
      elsif( i = "10" )then
         return armed_forces;
      else
         Put_Line( "found unmatched|" & i & "| as Occupation_Isco_1_Digit" );
         return no_occupation;
      end if;
    end Convert;


    function Convert( i : String ) return Tenure_type is
    begin
         if i = "1" then
            return owned_on_mortgage;
         elsif i = "2" then
            return owned_outright;
         elsif i = "3" then
            return rented;
         elsif i = "4" then
            return reduced_rented;
         elsif i = "5" then
            return social_rented;
         elsif i = "6" then
            return free;
         elsif i = "7" then
            return other;
        end if;
    end Convert;

    function Value( i : Marital_status_type ) return Integer is
    begin
         case i is
             when single => return 1;
             when married => return 2;
             when separated => return 3;
             when divorced => return 4;
             when widowed => return 5;
         end case;
    end Value;

    function Convert( i : String ) return Marital_status_type is
    begin
         if i = "1" then
            return single;
         elsif i = "2" then
            return married;
         elsif i = "3" then
            return separated;
         elsif i = "4" then
            return divorced;
         elsif i = "5" then
            return widowed;
        end if;
    end Convert;


    function Value( i : Education_current_status_type ) return Integer is
    begin
         case i is
             when not_in_education => return 0;
             when pre_primary => return 1;
             when primary => return 2;
             when lower_secondary => return 3;
             when upper_secondary => return 4;
             when post_secondary => return 5;
             when tertiary => return 6;
         end case;
    end Value;

    function Convert( i : String ) return Education_current_status_type is
    begin
         if i = "0" then
            return not_in_education;
         elsif i = "1" then
            return pre_primary;
         elsif i = "2" then
            return primary;
         elsif i = "3" then
            return lower_secondary;
         elsif i = "4" then
            return upper_secondary;
         elsif i = "5" then
            return post_secondary;
         elsif i = "6" then
            return tertiary;
        end if;
    end Convert;


    function Value( i : Education_highest_status_type ) return Integer is
    begin
         case i is
             when not_completed_primary => return 0;
             when primary => return 1;
             when lower_secondary => return 2;
             when upper_secondary => return 3;
             when post_secondary => return 4;
             when tertiary => return 5;
             when this_country => return 1;
             when other_eu => return 2;
             when other => return 3;
         end case;
    end Value;

    function Convert( i : String ) return Education_highest_status_type is
    begin
         if i = "0" then
            return not_completed_primary;
         elsif i = "1" then
            return primary;
         elsif i = "2" then
            return lower_secondary;
         elsif i = "3" then
            return upper_secondary;
         elsif i = "4" then
            return post_secondary;
         elsif i = "5" then
            return tertiary;
         elsif i = "1" then
            return this_country;
         elsif i = "2" then
            return other_eu;
         elsif i = "3" then
            return other;
        end if;
    end Convert;


    function Value( i : Consensual_union_type ) return Integer is
    begin
         case i is
             when no => return 0;
             when yes_on_a_legal_basis => return 1;
         end case;
    end Value;

    function Convert( i : String ) return Consensual_union_type is
    begin
         if i = "0" then
            return no;
         elsif i = "1" then
            return yes_on_a_legal_basis;
        end if;
    end Convert;


    function Value( i : Gender_type ) return Integer is
    begin
         case i is
             when female => return 0;
             when male => return 1;
         end case;
    end Value;

    function Convert( i : String ) return Gender_type is
    begin
         if i = "0" then
            return female;
         elsif i = "1" then
            return male;
        end if;
    end Convert;

    function Value( i : Citizenship_type ) return Integer is
    begin
         case i is
             when this_country => return 1;
             when other_eu => return 2;
             when other => return 3;
         end case;
    end Value;

    function Convert( i : String ) return Citizenship_type is
    begin
         if i = "1" then
            return this_country;
         elsif i = "2" then
            return other_eu;
         elsif i = "3" then
            return other;
        end if;
    end Convert;
    
end EU_Types;
