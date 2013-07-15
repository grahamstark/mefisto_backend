with EU.Web.Settings;
with Text_Utils;
with Ada.Text_IO;
with Utils;
with Templates_Parser;


package body EU.BE.I81N is

   package euw renames  EU.Web.Settings;
   
   function GL_Class( i : Amount; up_is_good : Boolean ) return String is
   begin
      if( i > 0.001 ) then
         if( up_is_good )then
            return "improvement";
         else
            return "worseing";
         end if;
      elsif ( i < -0.001 ) then
         if( up_is_good )then
            return "worseing";
         else
            return "improvement";
         end if;
      end if;
      return "neutral";      
   end GL_Class;
      
   function GL_Format( i : Integer; up_is_good : Boolean; lang : Languages ) return String is
   use Text_Utils;
      s : Unbounded_String;
   begin
      s := s & "<span class='total_cost_value " & GL_Class( Amount( i ), up_is_good ) & "'>";
      if( i > 0 )then
         s := s & "&nbsp;" & "+" & "&nbsp;";
      end if;
      s := s & Web_Format( i, lang );
      s := s & "</span>";
      return TS( s );
   end GL_Format;
      
   function GL_Format( i : Amount; up_is_good : Boolean; lang : Languages ) return String is
   use Text_Utils;
     s : Unbounded_String;
   begin
      s := s & "<span class='total_cost_value " & GL_Class( i, up_is_good ) & "'>";
      if( i > 0.0 )then
         s := s & "&nbsp;" & "+" & "&nbsp;";
      end if;
      s := s & Web_Format( i, lang );
      s := s & "</span>";
      return TS( s );
   end GL_Format;
   
   function Lang_Str( lang : Languages ) return String is
   begin
      case lang is
         when en => return "en";
         when nl => return "nl";
      end case;
   end Lang_Str;
   
   package NL_Format_Utils is new 
      Format_Utils( 
         Counter_Type => Counter_Type, 
         Float_Type => Amount,
         Default_Separator => ' ',
         Default_Radix_Mark => ',',
         Amount_Picture => 
            Ada.Text_IO.Editing.To_Picture( "-ZZZ_ZZZ_ZZZ_ZZZ_ZZZ_ZZ9.99") ); -- comma seperator

   package EN_Format_Utils is new 
      Format_Utils( 
         Counter_Type => Counter_Type, 
         Float_Type => Amount,
         Default_Separator => ',',
         Default_Radix_Mark => '.',
         Amount_Picture => 
            Ada.Text_IO.Editing.To_Picture( "-ZZZ_ZZZ_ZZZ_ZZZ_ZZZ_ZZ9.99") ); -- comma seperator
      -- type Languages is ( en, nl );
      -- package Translations is new Translate_Utils( Languages => Languages );
      
    function Web_Format( c : Amount; lang : Languages ) return String is
    begin
      -- if( c = MISSING_R )then
      --    return Translations.Lookup( "missing_acro", lang );
      -- end if;
      case lang is
         when en => return EN_Format_Utils.Format_With_Commas( c );
         when nl => return NL_Format_Utils.Format_With_Commas( c );
      end case;
    end Web_Format;
    
    function Web_Format( c : Integer; lang : Languages ) return String is
    begin
      -- if( c = MISSING )then
      --    return Translations.Lookup( "missing_acro", lang );
      -- end if;
      case lang is
         when en => return EN_Format_Utils.Format_With_Commas( c );
         when nl => return NL_Format_Utils.Format_With_Commas( c );
      end case;
    end Web_Format;
 
   function Web_Format( b : Boolean; lang : Languages ) return String is
   begin
      if( b ) then
         return Translations.Lookup( "Yes", lang );
      else 
         return Translations.Lookup( "No", lang );
      end if;
   end Web_Format;
   
   function Create_Message(
      lang     : Languages;
      min      : Amount;
      max      : Amount ) return Unbounded_String is
      use Utils;
      use Templates_Parser;
      min_max_table : Translate_Set;
      message : Unbounded_String;
   begin
      if( min = 0.0 ) and ( max = Amount'Last )then
        message := To_Unbounded_String( Translations.Lookup( "format_positive", lang ));
      elsif( min > Amount'First ) and ( max = Amount'Last )then
        Insert( min_max_table, Templates_Parser.Assoc( "MIN", Web_Format( min, lang )));
        declare
           templ : String := Translations.Lookup( "format_min_only", lang );
        begin
           message := To_Unbounded_String( Translate( templ, min_max_table )); 
        end;
      elsif( min = Amount'First ) and ( max < Amount'Last )then
        Insert( min_max_table, Templates_Parser.Assoc( "MAX", Web_Format( max, lang )));
        declare
           templ : String := Translations.Lookup( "format_max_only", lang );
        begin
           message := To_Unbounded_String( Translate( templ, min_max_table )); 
        end;
      elsif( min > Amount'First ) and ( max < Amount'Last )then
        declare
           templ : String := Translations.Lookup( "format_min_and_max", lang );
        begin
           Insert( min_max_table, Templates_Parser.Assoc( "MIN", Web_Format( min, lang )));
           Insert( min_max_table, Templates_Parser.Assoc( "MAX", Web_Format( max, lang )));
           message := To_Unbounded_String( Translate( templ, min_max_table )); 
        end;
      else
         message := To_Unbounded_String( Translations.Lookup( "format_not_valid_number", lang ));
      end if;
      return message;
   end Create_Message;

   function Create_Message(
      lang     : Languages;
      min      : Integer;
      max      : Integer ) return Unbounded_String is
      use Utils;
      use Templates_Parser;
      min_max_table : Translate_Set;
      message : Unbounded_String;
   begin
      if( min = 0 ) and ( max = Integer'Last )then
        message := To_Unbounded_String( Translations.Lookup( "format_positive", lang ));
      elsif( min > Integer'First ) and ( max = Integer'Last )then
        Insert( min_max_table, Templates_Parser.Assoc( "MIN", Web_Format( min, lang )));
        declare
           templ : String := Translations.Lookup( "format_min_only", lang );
        begin
           message := To_Unbounded_String( Translate( templ, min_max_table )); 
        end;
      elsif( min = Integer'First ) and ( max < Integer'Last )then
        Insert( min_max_table, Templates_Parser.Assoc( "MAX", Web_Format( max, lang )));
        declare
           templ : String := Translations.Lookup( "format_max_only", lang );
        begin
           message := To_Unbounded_String( Translate( templ, min_max_table )); 
        end;
      elsif( min > Integer'First ) and ( max < Integer'Last )then
        declare
           templ : String := Translations.Lookup( "format_min_and_max", lang );
        begin
           Insert( min_max_table, Templates_Parser.Assoc( "MIN", Web_Format( min, lang )));
           Insert( min_max_table, Templates_Parser.Assoc( "MAX", Web_Format( max, lang )));
           message := To_Unbounded_String( Translate( templ, min_max_table )); 
        end;
      else
         message := To_Unbounded_String( Translations.Lookup( "format_not_valid_number", lang ));
      end if;
      return message;
   end Create_Message;

   
   function Is_Valid_Number( s : String; lang : Languages; is_integer : Boolean := False ) return Boolean is
   begin
      case lang is
         when en => return EN_Format_Utils.Validate_String_As_Number( s, is_integer );
         when nl => return NL_Format_Utils.Validate_String_As_Number( s, is_integer );
      end case;
   end Is_Valid_Number;

   procedure Web_Validate(
      input_str : String;
      lang     : Languages;
      val      : out Amount;
      message  : out Unbounded_String;
      error    : out Utils.Error_Type;
      min      : Amount := Amount'First;
      max      : Amount := Amount'Last ) is
      use Utils;
      use Text_Utils;
   begin
      message := To_Unbounded_String( "" );
      error  := No_Error;
      val := Amount'Value( input_str );
      
      -- hacked max / min checks with the 999999s a basic guard 
      -- for hacked upper/lower limits
      if( min >= -9999999.9 ) and then ( val < min ) then
            error := Out_Of_Range_Error;
            message := Create_Message( lang, min, max );   
      elsif( max <= 9999999.9 ) and then ( val > max ) then
            error := Out_Of_Range_Error;
            message := Create_Message( lang, min, max );   
      end if;
   exception
      when Others => 
         error := Format_Error;
         case lang is 
         when en =>
            if( Is_Valid_Number( input_str, en )) then
               declare
                  s : String := Delete_All_Instances( input_str, "," );
               begin
                  Ada.Text_IO.Put_Line( "made s as |" & s & "|" );
                  EN_Format_Utils.Validate( s, val, message, error, min, max );
               end;
            end if;
         when nl =>
            if( Is_Valid_Number( input_str, nl )) then
               declare
                  s : String := Replace_In_String(
                                 Delete_All_Instances( input_str, " " ), ",", "." );
               begin
                  NL_Format_Utils.Validate( s, val, message, error, min, max );
               end;
            end if;
         end case;
         if( error = Format_Error ) then
            val := R_VALIDATE_ERROR;
         end if;
         if( error /= No_Error ) then
            message := Create_Message( lang, min, max );
          else
            message := To_Unbounded_String( "" );
         end if;
   end Web_Validate;
   
   procedure Web_Validate(
      input_str : String;
      lang     : Languages;
      val      : out Integer;
      message  : out Unbounded_String;
      error    : out Utils.Error_Type;
      min      : Integer := Integer'First;
      max      : Integer:= Integer'Last ) is
      use Utils;
      use Text_Utils;
    begin
      message := To_Unbounded_String( "" );
      error  := No_Error;
      val := Integer'Value( input_str );
      if( val < min ) or ( val > max ) then
         error := Out_Of_Range_Error;
         message := Create_Message( lang, min, max );   
      end if;
   exception
      when Others => 
         error := Format_Error;
         case lang is 
         when en =>
            if( Is_Valid_Number( input_str, en, True )) then
               declare
                  s : String := Delete_All_Instances( input_str, "," );
               begin
                  EN_Format_Utils.Validate( s, val, message, error, min, max );
               end;
            end if;
         when nl =>
            if( Is_Valid_Number( input_str, nl, True )) then
               declare
                  s : String := Delete_All_Instances( input_str, " " );
               begin
                  NL_Format_Utils.Validate( s, val, message, error, min, max );
               end;
            end if;
         end case;
         if( error = Format_Error ) then
            val := VALIDATE_ERROR;
         end if;
         if( error /= No_Error ) then
            message := Create_Message( lang, min, max );
          else
            message := To_Unbounded_String( "" );
         end if;
   end Web_Validate;
   
   function Lang_File_Name( fname : String; lang : Languages ) return String is
      use Ada.Text_IO;
      lang_str : constant String := Text_Utils.Censor_String( Languages'Image( lang ));
      f_name : constant String := 
         euw.Language_Components_Path & 
         euw.Dir_Separator & 
         lang_str & 
         euw.Dir_Separator & 
         fname &
         ".txt";
   begin
      Put_Line( "Opening " & f_name );
      return f_name; 
   end  Lang_File_Name;
   
   procedure Load_Translations is
   begin
      for lang in Languages'Range loop
         Translations.Load( Lang_File_Name( "data_translations", lang ), lang );
         Translations.Load( Lang_File_Name( "enum_translations", lang ), lang );
         Translations.Load( Lang_File_Name( "manual_translations", lang ), lang );
         Translations.Load( Lang_File_Name( "summary_output_translations", lang ), lang );
         Translations.Load( Lang_File_Name( "full_output_translations", lang ), lang );
      end loop;
   end Load_Translations;

end EU.BE.I81N;
