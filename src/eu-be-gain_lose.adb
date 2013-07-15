package Body EU.BE.Gain_Lose is

   procedure Increment_Cell_Breakdowns( 
      current_breakdown : in out Aggregate_Cell_Breakdown; 
      new_breakdown     : Cell_Breakdown;
      weight            : Amount ) is
   begin
      current_breakdown.tenure( new_breakdown.tenure ) := 
         current_breakdown.tenure( new_breakdown.tenure ) + weight;
       -- FIXME etc
    end Increment_Cell_Breakdowns;
   

end EU.BE.Gain_Lose;
