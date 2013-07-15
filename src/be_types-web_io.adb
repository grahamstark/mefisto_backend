package body BE_Types.Web_IO is
 
   use EU.BE.I81N.Translations;
  
   function Web_Format( i : Nuts_level_1_type; lang : Languages ) return String is
   begin
      case i is
          when brussels => return Lookup( "brussels", lang );
          when flanders => return Lookup( "flanders", lang );
          when wallonia => return Lookup( "wallonia", lang );
      end case;
   end Web_Format;

end BE_Types.Web_IO;
