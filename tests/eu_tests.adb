--
--  $Author: graham_s $
--  $Date: 2010-02-15 13:40:48 +0000 (Mon, 15 Feb 2010) $
--  $Revision: 8644 $
--
with EU;
with EU.BE.Model;
with EU.BE.Model.Runner;
with EU.BE.Model.Settings;
-- with EU.BE.Model.Results;
with EU.BE;
with EU.Web.Settings;
with EU.BE.Household;
with EU.BE.Household.IO;
with EU.BE.Household.Web_IO;
with EU.BE.Results;
with EU.BE.Results.Web_IO;
with EU.BE.Results.IO;
with EU.BE.I81N;
with EU.BE.Output.Web_IO; 
with Ada.Text_IO;
with Text_Utils;
with Utils;
with AUnit.Test_Cases;
with AUnit.Assertions;
with BE_Base_Model_Types;
with GNAT.Regpat;
with Ada.Containers;


package body EU_Tests is

   use AUnit.Assertions;
   use AUnit.Test_Cases.Registration;
   use Ada.Text_IO;
   use EU.BE;
   use BE_Base_Model_Types;
   
   procedure Test_Split_String( T : in out AUnit.Test_Cases.Test_Case'Class ) is
      use Text_Utils;
      use Ada.Containers;
      use Text_Utils.Unbounded_String_Vector_Package;
      s1 : String := "This is a test";
      l  : Unbounded_String_List := Split( s1, ' ');
      s2 : String := "  This is a test  ";
      l2 : Unbounded_String_List := Split( s2, ' ');
   begin
      Put_Line( "Test_Split_String" );
      Assert( l.Length = 4, "1 length should be 4 was " & l.Length'Img );
      Assert( To_String( l.Element( 4 )) = "test", "1 element 4 should be 'test' was |" & To_String( l.Element( 4 )) & "|" );
      Assert( To_String( l.Element( 1 )) = "This", "1 element 1 should be 'This' was |" & To_String( l.Element( 1 )) & "|");
      Assert( l2.Length = 4, " 2 length should be 4 was " & l.Length'Img );
      Assert( To_String( l2.Element( 4 )) = "test", "2 element 4 should be 'test' was |" & To_String( l2.Element( 4 )) & "|");
      Assert( To_String( l2.Element( 1 )) = "This", "2 element 1 should be 'This' was |" & To_String( l2.Element( 1 )) & "|");
      
   end Test_Split_String;
   
   procedure Test_Read_Settings( T : in out AUnit.Test_Cases.Test_Case'Class ) is
      use  EU.BE.Model.Settings;
      model_sett : Model_Settings;
   begin
      Put_Line( "Test_Read_Settings" );
      Put_Line( "web_sett.Log_File_Dir " & EU.Web.Settings.Log_File_Dir );
      -- bin/EMMain params/emconfig_linux.csv
      model_sett := Read_Model_Settings( "etc/be_settings.txt" );
      model_sett.Set_Run_Id( "run1" );
      model_sett.Set_Users_Directory( "graham_s" );
      model_sett.Set_Dir_Separator( '/' );
      Put_Line( "model settings read OK" );
      Put_Line( "model_sett.Log_File_Name " & model_sett.Qualified_Log_File_Name );
      model_sett.Write_EU_Config_File; 
   end Test_Read_Settings;
   
   
   procedure Test_Translations( T : in out AUnit.Test_Cases.Test_Case'Class ) is
      use EU.BE.I81N.Translations;
   begin
      Put_Line( "Test_Translations" );
      for lang in EU.BE.I81N.Languages loop
         Put_Line(EU.BE.I81N.Languages'Image( lang ) & " " & "yes=" & Lookup( "yes", lang ));
         Put_Line(EU.BE.I81N.Languages'Image( lang ) & " " & "not_in_education=" & Lookup( "not_in_education", lang ));
         Put_Line(EU.BE.I81N.Languages'Image( lang ) & " " & "demographic_consensual_union=" & Lookup( "demographic_consensual_union", lang ));
         Put_Line(EU.BE.I81N.Languages'Image( lang ) & " " & "XXX=" & Lookup( "XXX", lang ));
    end loop;
   end Test_Translations;
   
   procedure Test_Templates( T : in out AUnit.Test_Cases.Test_Case'Class ) is
   begin
      null;
   end Test_Templates;
   
   procedure Test_Str_Editing( T : in out AUnit.Test_Cases.Test_Case'Class ) is
      s : constant String := Text_Utils.Delete_All_Instances( ",,,FRED,FRED,,FRED,,,", "," );
      sc : constant String := "FREDFREDFRED";
      s3 : String := "FREDFREDFRED";
   begin
      Put_Line( "Test_Str_Editing" );
      Put_Line( "Test_Str_Editing: got s as " & s );
      if( s3 = sc )then
         Put_Line( s );
      end if;
      -- Assert( s = "FREDFREDFRED", "should be FREDFREDFRED but was " & s );
   end Test_Str_Editing;
   
   procedure Test_Number_Regep( T : in out AUnit.Test_Cases.Test_Case'Class ) is
      THOUSANDS : constant Character := ',';
      DECIMAL : constant Character := '.';
      use GNAT.Regpat;
      pos : Natural;
           
      N_RE : constant String := "^ *((\d+)|(\d{1,3})(\"&THOUSANDS&"\d{3})*)(\"&DECIMAL&"\d{2,})* *$";
      re : constant Pattern_Matcher := Compile( N_RE );
      D_RE_S : constant String := "^ *((\d+)|(\d{1,3})(\"&" "&"\d{3})*)(\"&","&"\d{2,})* *$";
      d_re : constant Pattern_Matcher := Compile( D_RE_S );
   begin
      Put_Line( "Test_Number_Regep" );
      Put_Line( "final re is |"&N_RE&"| " );
      pos := Match( re, "x12345" );      
      Assert( pos = 0, "#1 shouldn't match matched at " & Natural'Image(pos) );
      pos := Match( re, "12345" );
      Assert( pos = 1, "#2 should match at 1 matched at " & Natural'Image(pos) );
      pos := Match( re, "12,345" );
      Assert( pos = 1, "#3 should match at 1 matched at " & Natural'Image(pos) );
      pos := Match( re, "   12,345   " );
      Assert( pos = 1, "#4 should match at 1 matched at " & Natural'Image(pos) );
      pos := Match( re, "   111,112,345.2345678   " );
      Assert( pos = 1, "#5 should match at 1 matched at " & Natural'Image(pos) );
      pos := Match( re, "   111x112,345.2345678   " );
      Assert( pos = 0, "#6 should match at 0 matched at " & Natural'Image(pos) );

      pos := Match( d_re, "x12345" );      
      Assert( pos = 0, "#de_1 shouldn't match matched at " & Natural'Image(pos) );
      pos := Match( d_re, "12345" );
      Assert( pos = 1, "#de_2 should match at 1 matched at " & Natural'Image(pos) );
      pos := Match( d_re, "12 345" );
      Assert( pos = 1, "#de_3 should match at 1 matched at " & Natural'Image(pos) );
      pos := Match( d_re, "   12 345   " );
      Assert( pos = 1, "#de_4 should match at 1 matched at " & Natural'Image(pos) );
      pos := Match( d_re, "   111 112 345,2345678   " );
      Assert( pos = 1, "#de_5 should match at 1 matched at " & Natural'Image(pos) );
      pos := Match( d_re, "   111x112 345,2345678   " );
      Assert( pos = 0, "#de_6 should match at 0 matched at " & Natural'Image(pos) );
      
   end Test_Number_Regep;
   
   procedure Test_Number_Regep_Final( T : in out AUnit.Test_Cases.Test_Case'Class ) is
      use EU.BE.I81N;
         match : Boolean;
   begin
      Put_Line( "Test_Number_Regep_Final" );
      match := Is_Valid_Number( "x12345", en );    
      Assert( not match, "#1 shouldn't match matched at " );
      match := Is_Valid_Number( "12345", en );
      Assert( match, "#2 should match at 1 matched at " );
      match := Is_Valid_Number( "12,345", en );
      Assert( match, "#3 should match at 1 matched at " );
      match := Is_Valid_Number( "   12,345   ", en );
      Assert( match, "#4 should match at 1 matched at " );
      match := Is_Valid_Number( "   111,112,345.2345678   ", en );
      Assert( match, "#5 should match at 1 matched at " );
      match := Is_Valid_Number( "   111x112,345.2345678   ", en );
      Assert( not match, "#6 should match at 0 matched at " );

      match := Is_Valid_Number( "x12345", nl );
      Assert( not match, "#de_1 shouldn't match matched at " );
      match := Is_Valid_Number( "12345", nl );
      Assert( match, "#de_2 should match at 1 matched at " );
      match := Is_Valid_Number( "12 345", nl );
      Assert( match, "#de_3 should match at 1 matched at " );
      match := Is_Valid_Number( "   12 345   ", nl );
      Assert( match, "#de_4 should match at 1 matched at " );
      match := Is_Valid_Number( "   111 112 345,2345678   ", nl );
      Assert( match, "#de_5 should match at 1 matched at " );
      match := Is_Valid_Number( "   111x112 345,2345678   ", nl );
      Assert( not match, "#de_6 should match at 0 matched at " );
      
   end Test_Number_Regep_Final;
   
   procedure Test_Validation( T : in out AUnit.Test_Cases.Test_Case'Class ) is
      use Utils;
      use EU.BE.I81N;
      v : Amount;
      message : Unbounded_String;
      error : Error_Type;
      -- No_Error, Format_Error, Out_Of_Range_Error, Other_Error
   begin
   
      Put_Line( "Test_Validation" );
      Web_Validate( "12345", en, v, message, error );
      Assert( error = No_Error, "t1: Error should be no error was " & Error_Type'Image( error ));
      Assert( message = To_Unbounded_String( "" ), "t1: message should be '' was " & To_String( message ));
      Assert( v = 12345.0, "t1: v should be 12345.0; was " & Amount'Image( v ));
      
      Web_Validate( "12345", nl, v, message, error );
      Assert( error = no_error, "t2: Error should be no error was " & Error_Type'Image( error ));
      Assert( message = To_Unbounded_String( "" ), "t2: message should be '' was " & To_String( message ));
      Assert( v = 12345.0, "t2: v should be 12345.0; was " & Amount'Image( v ));
      
      Web_Validate( "12345", en, v, message, error, 0.0, 100.0 );
      Assert( error = Out_Of_Range_Error, "t3: Error should be Out_Of_Range_Error was " & Error_Type'Image( error ));
      Assert( message = To_Unbounded_String( "This should be a number between 0.00 and 100.00" ), "t3: message should be 'This should be a number between 0.0 and 100.0' was " & To_String( message ));
      Assert( v = 12345.0, "t3: v should be 12345.0; was " & Amount'Image( v ));
      
      Web_Validate( "12345", nl, v, message, error, 0.0, 100.0 );
      Assert( error = Out_Of_Range_Error, "t4: Error should be Out_Of_Range_Error was " & Error_Type'Image( error ));
      Assert( message = To_Unbounded_String( "Dit moet een getal tussen 0,00 been 100,00" ), "t4: message should be 'Dit moet een getal tussen 0.0 Been 100.0' was " & To_String( message ));
      Assert( v = 12345.0, "t4: v should be 12345.0; was " & Amount'Image( v ));

      Web_Validate( "12345,00", en, v, message, error );
      Assert( error = Format_Error, "t5: Error should be Format_Error was " & Error_Type'Image( error ));
      Assert( message = To_Unbounded_String( "This is not a valid number" ), "t5: message should be 'This is not a valid number' was " & To_String( message ));
      Assert( v = R_VALIDATE_ERROR, "t5: v should be Amount'First; was " & Web_Format( v, en ));

      Web_Validate( "12345,00", nl, v, message, error );
      Assert( error = No_Error, "t6: Error should be No_Error was " & Error_Type'Image( error ));
      Assert( message = To_Unbounded_String( "" ), "t6: message should be '' was " & To_String( message ));
      Assert( v = 12345.0, "t6: v should be 12345.0; was " & Web_Format( v, en ));

      Web_Validate( "xxx12345,x00", en, v, message, error );
      Assert( error = Format_Error, "t7: Error should be Format_Error was " & Error_Type'Image( error ));
      Assert( message = To_Unbounded_String( "This is not a valid number" ), "t7: message should be 'This is not a valid number' was " & To_String( message ));
      Put_Line( "v = " & v'Img );
      Assert( v = R_VALIDATE_ERROR, "t7: v should be Amount'First; was " & Web_Format( v, en ));
      
      Web_Validate( "12345,x00", nl, v, message, error );
      Assert( error = Format_Error, "t8: Error should be Format_Error was " & Error_Type'Image( error ));
      Assert( message = To_Unbounded_String( "Dit is geen geldig nummer" ), "t8: message should be 'This is not a valid number' was " & To_String( message ));
      Assert( v = R_VALIDATE_ERROR, "t8: v should be Amount'First; was " & Web_Format( v, en ));
      
      Web_Validate( "123,450.00", en, v, message, error );
      Assert( v = 123_450.0, "t8: v should be 123_450.0; was " & Web_Format( v, en ));
      Assert( error = No_Error, "t8: Error should be no error was " & Error_Type'Image( error ));
      Assert( message = To_Unbounded_String( "" ), "t8: message should be '' was " & To_String( message ));

      Web_Validate( "123,450.00", nl, v, message, error );
      Assert( v = R_VALIDATE_ERROR, "t9: v should be R_VALIDATE_ERROR; was " & Web_Format( v, en ));
      Assert( error = Format_Error, "t9: Error should be Format_Error was " & Error_Type'Image( error ));
      Assert( message = To_Unbounded_String( "Dit is geen geldig nummer" ), "t9: message should be 'This is not a valid number' was " & To_String( message ));

      Web_Validate( "      123,450.00    ", en, v, message, error );
      Assert( v = 123_450.0, "t10: v should be 123_450.0; was " & Web_Format( v, en ));
      Assert( error = No_Error, "t10: Error should be no error was " & Error_Type'Image( error ));
      Assert( message = To_Unbounded_String( "" ), "t10: message should be '' was " & To_String( message ));

      Web_Validate( "      -123,450.00    ", en, v, message, error );
      Assert( v = -123_450.0, "t11: v should be -123_450.0; was " & Web_Format( v, en ));
      Assert( error = No_Error, "t11: Error should be no error was " & Error_Type'Image( error ));
      Assert( message = To_Unbounded_String( "" ), "t11: message should be '' was " & To_String( message ));

   end Test_Validation;
   
   procedure Test_Validation_Int( T : in out AUnit.Test_Cases.Test_Case'Class ) is
      use Utils;
      use EU.BE.I81N;
      v : Integer;
      message : Unbounded_String;
      error : Error_Type;
      -- No_Error, Format_Error, Out_Of_Range_Error, Other_Error
   begin
   
      Put_Line( "Test_Validation_Int" );
      Web_Validate( "12345", en, v, message, error );
      Assert( error = No_Error, "t1: Error should be no error was " & Error_Type'Image( error ));
      Assert( message = To_Unbounded_String( "" ), "t1: message should be '' was " & To_String( message ));
      Assert( v = 12345, "t1: v should be 12345; was " & Integer'Image( v ));
      
      Web_Validate( "12345", nl, v, message, error );
      Assert( error = no_error, "t2: Error should be no error was " & Error_Type'Image( error ));
      Assert( message = To_Unbounded_String( "" ), "t2: message should be '' was " & To_String( message ));
      Assert( v = 12345, "t2: v should be 12345; was " & Integer'Image( v ));
      
      Web_Validate( "12345", en, v, message, error, 0, 100 );
      Assert( error = Out_Of_Range_Error, "t3: Error should be Out_Of_Range_Error was " & Error_Type'Image( error ));
      Assert( message = To_Unbounded_String( "This should be a number between 0 and 100" ), "t3: message should be 'This should be a number between 0 and ' was " & To_String( message ));
      Assert( v = 12345, "t3: v should be 12345; was " & Integer'Image( v ));
      
      Web_Validate( "12345", nl, v, message, error, 0, 100 );
      Assert( error = Out_Of_Range_Error, "t4: Error should be Out_Of_Range_Error was " & Error_Type'Image( error ));
      Assert( message = To_Unbounded_String( "Dit moet een getal tussen 0 been 100" ), "t4: message should be 'Dit moet een getal tussen 0 Been 100' was " & To_String( message ));
      Assert( v = 12345, "t4: v should be 12345; was " & Integer'Image( v ));
      
      Web_Validate( "12345", nl, v, message, error );
      Assert( error = No_Error, "t6: Error should be No_Error was " & Error_Type'Image( error ));
      Assert( message = To_Unbounded_String( "" ), "t6: message should be '' was " & To_String( message ));
      Assert( v = 12345, "t6: v should be 12345; was " & Web_Format( v, en ));

      Web_Validate( "xxx12345,", en, v, message, error );
      Assert( error = Format_Error, "t7: Error should be Format_Error was " & Error_Type'Image( error ));
      Assert( message = To_Unbounded_String( "This is not a valid number" ), "t7: message should be 'This is not a valid number' was " & To_String( message ));
      Put_Line( "v = " & v'Img );
      Assert( v = VALIDATE_ERROR, "t7: v should be Integer'First; was " & Web_Format( v, en ));
      
      Web_Validate( "12345,", nl, v, message, error );
      Assert( error = Format_Error, "t8: Error should be Format_Error was " & Error_Type'Image( error ));
      Assert( message = To_Unbounded_String( "Dit is geen geldig nummer" ), "t8: message should be 'This is not a valid number' was " & To_String( message ));
      Assert( v = VALIDATE_ERROR, "t8: v should be Integer'First; was " & Web_Format( v, en ));
      
      Web_Validate( "123,450", en, v, message, error );
      Assert( v = 123_450, "t8: v should be 123_450; was " & Web_Format( v, en ));
      Assert( error = No_Error, "t8: Error should be no error was " & Error_Type'Image( error ));
      Assert( message = To_Unbounded_String( "" ), "t8: message should be '' was " & To_String( message ));

      Web_Validate( "123,450", nl, v, message, error );
      Assert( v = VALIDATE_ERROR, "t9: v should be VALIDATE_ERROR; was " & Web_Format( v, en ));
      Assert( error = Format_Error, "t9: Error should be Format_Error was " & Error_Type'Image( error ));
      Assert( message = To_Unbounded_String( "Dit is geen geldig nummer" ), "t9: message should be 'This is not a valid number' was " & To_String( message ));

      Web_Validate( "      123,450    ", en, v, message, error );
      Assert( v = 123_450, "t10: v should be 123_450; was " & Web_Format( v, en ));
      Assert( error = No_Error, "t10: Error should be no error was " & Error_Type'Image( error ));
      Assert( message = To_Unbounded_String( "" ), "t10: message should be '' was " & To_String( message ));

      Web_Validate( "      -123,450    ", en, v, message, error );
      Assert( v = -123_450, "t11: v should be -123_450; was " & Web_Format( v, en ));
      Assert( error = No_Error, "t11: Error should be no error was " & Error_Type'Image( error ));
      Assert( message = To_Unbounded_String( "" ), "t11: message should be '' was " & To_String( message ));

   end Test_Validation_Int;

   --------------------
   -- Register_Tests --
   --------------------

   procedure Register_Tests( t : in out Test_Case ) is
   begin 
       Register_Routine( T, Test_Read_Settings'Access, "Test _Read_Settings" );
       Register_Routine( T, Test_Translations'Access, "Test _Translation" );
       Register_Routine( T, Test_Templates'Access, "Test _Templates" );
       Register_Routine( T, Test_Str_Editing'Access, "Test _Str_Editing" );
       Register_Routine( T, Test_Validation'Access, "Test _Validation" );
       Register_Routine( T, Test_Validation_Int'Access, "Test _Validation_Int" );
       Register_Routine( T, Test_Number_Regep'Access, "Test_Number_Regep" );
       Register_Routine( T, Test_Number_Regep_Final'Access, "Test_Number_Regep_Final" );
       Register_Routine( T, Test_Split_String'Access, "Test_Split_String" );
  end Register_Tests;

   procedure Set_Up (T : in out Test_Case) is
   begin
      Put_Line( "eu_tests:: Set_Up" );
      EU.Web.Settings.Read_Web_Settings( "etc/web_settings.txt" );
      Put_Line( "read OK" );
      EU.BE.I81N.Load_Translations;
   end Set_Up;

   ----------
   -- Name --
   ----------
   function Name ( T : Test_Case ) return Message_String is
   begin
      return Format( " EU Tests." );
   end Name;

end EU_Tests;
