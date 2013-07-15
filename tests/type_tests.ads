package Type_Tests is
   
   type F1 is new Float;
   type E1 is (ta,tb,tc);
   type S10 is new String(1 .. 10);
   subtype F1S is F1 range 1.0 .. 100.0;
   
   type Record_A is record
      a : integer;
      b : F1;
      c : E1 := tc;
   end record;
   
   type Record_B is record
      d : Record_A;
      e : S10;
      f : F1S := 10.0;
   end record;

end Type_Tests;
