package body EU_Types.Web_IO is

   use EU.BE.I81N.Translations;

   function Web_Format( i : Tenure_type; lang : Languages ) return String is
   begin
      case i is
          when owned_on_mortgage => return Lookup( "owned_on_mortgage", lang );
          when owned_outright => return Lookup( "owned_outright", lang );
          when rented => return Lookup( "rented", lang );
          when reduced_rented => return Lookup( "reduced_rented", lang );
          when social_rented => return Lookup( "social_rented", lang );
          when free => return Lookup( "free", lang );
          when other => return Lookup( "other", lang );
      end case;
   end Web_Format;
   
   function Web_Format( i : Marital_status_type; lang : Languages ) return String is
   begin
      case i is
          when single => return Lookup( "single", lang );
          when married => return Lookup( "married", lang );
          when separated => return Lookup( "separated", lang );
          when divorced => return Lookup( "divorced", lang );
          when widowed => return Lookup( "widowed", lang );
      end case;
   end Web_Format;

   function Web_Format( i : Education_current_status_type; lang : Languages ) return String is
   begin
      case i is
          when not_in_education => return Lookup( "not_in_education", lang );
          when pre_primary => return Lookup( "pre_primary", lang );
          when primary => return Lookup( "primary", lang );
          when lower_secondary => return Lookup( "lower_secondary", lang );
          when upper_secondary => return Lookup( "upper_secondary", lang );
          when post_secondary => return Lookup( "post_secondary", lang );
          when tertiary => return Lookup( "tertiary", lang );
      end case;
   end Web_Format;

   function Web_Format( i : Education_highest_status_type; lang : Languages ) return String is
   begin
      case i is
          when not_completed_primary => return Lookup( "not_completed_primary", lang );
          when primary => return Lookup( "primary", lang );
          when lower_secondary => return Lookup( "lower_secondary", lang );
          when upper_secondary => return Lookup( "upper_secondary", lang );
          when post_secondary => return Lookup( "post_secondary", lang );
          when tertiary => return Lookup( "tertiary", lang );
          when this_country => return Lookup( "this_country", lang );
          when other_eu => return Lookup( "other_eu", lang );
          when other => return Lookup( "other", lang );
      end case;
   end Web_Format;

   function Web_Format( i : Consensual_union_type; lang : Languages ) return String is
   begin
      case i is
          when no => return Lookup( "no", lang );
          when yes_on_a_legal_basis => return Lookup( "yes_on_a_legal_basis", lang );
      end case;
   end Web_Format;

   function Web_Format( i : Gender_type; lang : Languages ) return String is
   begin
      case i is
          when female => return Lookup( "female", lang );
          when male => return Lookup( "male", lang );
          when others => return "?";
      end case;
   end Web_Format;
   
   function Web_Format( i : Citizenship_type; lang : Languages ) return String is
   begin
      case i is
          when this_country => return Lookup( "this_country", lang );
          when other_eu => return Lookup( "other_eu", lang );
          when other => return Lookup( "other", lang );
      end case;
   end Web_Format;
   
   function Web_Format( i : Occupation_Isco_1_Digit; lang : Languages ) return String is
   begin
      case i is   
         when legislators_senior_officials_and_managers => return Lookup( "legislators_senior_officials_and_managers", lang );
         when professionals => return Lookup( "professionals", lang );
         when technicians_and_associate_professionals => return Lookup( "technicians_and_associate_professionals", lang );
         when clerks => return Lookup( "clerks", lang );
         when service_workers_and_shop_and_market_sales_workers => return Lookup( "service_workers_and_shop_and_market_sales_workers", lang );
         when skilled_agricultural_and_fishery_workers => return Lookup( "skilled_agricultural_and_fishery_workers", lang );
         when craft_and_related_trades_workers => return Lookup( "craft_and_related_trades_workers", lang );
         when plant_and_machine_operators_and_assemblers => return Lookup( "plant_and_machine_operators_and_assemblers", lang );
         when elementary_occupations => return Lookup( "elementary_occupations", lang );
         when armed_forces => return Lookup( "armed_forces", lang );
         when no_occupation => return Lookup( "no_occupation", lang );
      end case;
   end Web_Format;
    
end EU_Types.Web_IO;
