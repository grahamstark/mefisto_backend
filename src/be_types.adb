package body BE_Types is

    function Value( i : Nuts_level_1_type ) return Integer is
    begin
         case i is
             when brussels => return 1;
             when flanders => return 2;
             when wallonia => return 3;
         end case;
    end Value;
    
    function Get_Age_Band( age : Natural ) return Age_Band is
       ar : Age_Band;
    begin
      case age is
         when  0 .. 15 => ar := age_0_15;
         when 16 .. 18 => ar := age_16_18;
         when 19 .. 21 => ar := age_19_21;
         when 22 .. 40 => ar := age_22_40;
         when 41 .. 60 => ar := age_41_60;
         when 61 .. 65 => ar := age_61_65;
         when others   => ar := age_over_65;
      end case;
      return ar;
    end Get_Age_Band;

    function Convert( i : String ) return Nuts_level_1_type is
    begin
         if i = "1" then
            return brussels;
         elsif i = "2" then
            return flanders;
         elsif i = "3" then
            return wallonia;
        end if;
    end Convert;

end BE_Types;
