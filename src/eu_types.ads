package EU_Types is

   MISSING   : constant       := -1;
   MISSING_R : constant       := -1.0;

   subtype Age_Range is Natural range 0 .. 150;
   
   subtype Person_Count is Natural range 0 .. 20;
   subtype Person_Range is Person_Count range 1 .. Person_Count'Last;
   subtype Family_Unit_Count is Natural range 0 .. 20;
   subtype Family_Unit_Range is Person_Count range 1 .. Person_Count'Last;
    
   subtype System_Number is Positive range 1 .. 2;
   PRE   : constant Positive := System_Number'First;
   POST  : constant Positive := System_Number'Last;
   
   type Tenure_type is (  
      owned_on_mortgage,
      owned_outright,
      rented,
      reduced_rented,
      social_rented,
      free,
      other );
   function Value( i : Tenure_type ) return Integer;
   function Convert( i : String ) return Tenure_type;
   
   
   type Marital_status_type is (  
      single,
      married,
      separated,
      divorced,
      widowed );
   function Value( i : Marital_status_type ) return Integer;
   function Convert( i : String ) return Marital_status_type;
   
   type Education_current_status_type is (  
      not_in_education,
      pre_primary,
      primary,
      lower_secondary,
      upper_secondary,
      post_secondary,
      tertiary );
   function Value( i : Education_current_status_type ) return Integer;
   function Convert( i : String ) return Education_current_status_type;
   
   type Education_highest_status_type is (  
      not_completed_primary,
      primary,
      lower_secondary,
      upper_secondary,
      post_secondary,
      tertiary,
      this_country,
      other_eu,
      other );
   function Value( i : Education_highest_status_type ) return Integer;
   function Convert( i : String ) return Education_highest_status_type;
   
   type Consensual_union_type is (  
      no,
      yes_on_a_legal_basis );
   function Value( i : Consensual_union_type ) return Integer;
   function Convert( i : String ) return Consensual_union_type;
   
   type Gender_type is (  
      female,
      male );
   function Value( i : Gender_type ) return Integer;
   function Convert( i : String ) return Gender_type;
   
   type Citizenship_type is (  
      this_country,
      other_eu,
      other );
   function Value( i : Citizenship_type ) return Integer;
   function Convert( i : String ) return Citizenship_type;
   
   type Occupation_Isco_1_Digit is (
   
      legislators_senior_officials_and_managers,
      professionals,
      technicians_and_associate_professionals,
      clerks,
      service_workers_and_shop_and_market_sales_workers,
      skilled_agricultural_and_fishery_workers,
      craft_and_related_trades_workers,
      plant_and_machine_operators_and_assemblers,
      elementary_occupations,
      armed_forces,
      no_occupation );
      
   function Convert( i : String ) return Occupation_Isco_1_Digit; 
      
   
end EU_Types;
