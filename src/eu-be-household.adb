package body EU.BE.Household is

   function Oldest_Person( hh : Household_Rec ) return Person_Range is
        aop : Person_Range := Person_Range'First;
        max_age : Integer := -99; 
   begin
        for pno in 1 .. hh.num_people loop
           if( hh.people( pno ).demographic_age > max_age )then
              max_age := hh.people( pno ).demographic_age;
              aop := pno;
           end if;
        end loop;
        return aop;
   end Oldest_Person;
   
   function Head_Of_Household( hh : Household_Rec ) return Person_Range renames Oldest_Person;
   
   --
   -- Or 0 if no spouse
   --
   function Spouse_Of( hh : Household_Rec; person_num : Person_Range ) return Person_Count is
      idp : constant Person_Count := Person_Count( hh.people( person_num ).identifier_partner );
   begin
        if( idp = 0 )then
           return idp;
        end if;
        for pno in 1 .. hh.num_people loop
           if( hh.people( pno ).identifier_person = idp )then
              return pno;
           end if;
        end loop;
        return Person_Count'First; 
   end Spouse_Of;
   
   function Get_Equivalence_Scale( hh : Household_Rec ) return Amount is
      e : Amount := 1.0;
   begin
      for pno in 2 .. hh.num_people loop
         if( hh.people( pno ).demographic_age < 14 )then
            e := e + 0.3;
         else
            e := e + 0.5;
         end if;
      end loop;
      return e;
   end Get_Equivalence_Scale;

end EU.BE.Household;

