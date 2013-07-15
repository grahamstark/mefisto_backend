package EU.BE.Light_Parameters is

   type Reform_Packages is ( flat_tax, regional_tax_credit, child_care, minimum_income_protection );
   
   type Light_Params is record
      which_package : Reform_Packages := flat_tax;
      base_rate : Rate := 15.0; exemption : Amount := 12_500.0;
      basic_amount : Amount;
      basic_tax_credit : Amount;
      income_support_per_single_person : Amount;
   end record;
   
   MISSING_PARAMS : constant Light_Params := ( 
      which_package => flat_tax, 
      base_rate    => 15.0, 
      exemption    => 12_500.0,
      basic_amount => 300.0,
      basic_tax_credit => 1_000.0,
      income_support_per_single_person => 1725.0 );
 
end EU.BE.Light_Parameters; 

