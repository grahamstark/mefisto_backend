with AUnit.Assertions;
with AUnit.Test_Cases;

with Ada.Exceptions;
with Ada.Strings.Unbounded.Text_IO;
with Ada.Text_IO;

with BE_Base_Model_Types;

with DOM.Core.Attrs;
with DOM.Core.Documents;    
with DOM.Core.Nodes;
with DOM.Core;        
with DOM.Readers; 
with Sax.Readers;     

with EU.BE.Globals;
with EU.BE.Parameter_System_Declarations;

with EU.BE.I81N;
with EU.BE.Users;
with EU.Web.Settings;

with Input_Sources.File;
with Keyed_Text_Buffer;
with Line_Extractor;
with Text_Utils;
with Web_Utils;
with XML_Utils.Conversions;
with XML_Utils;

with Parameter_System.Input_Buffer.EU_Renderer;
with Parameter_System.Input_Buffer.Utils;
with Parameter_System.Input_Buffer;
with Parameter_System.Iterator;
with Parameter_System.XML;
with Parameter_System;
with Parameter_System_IO_Commons;
with Templates_Parser;


package body DOM_And_Parameter_Tests is

   use AUnit.Assertions;
   use AUnit.Test_Cases.Registration;
   use Ada.Text_IO;
   use BE_Base_Model_Types;
   use Text_Utils;
   use Parameter_System_IO_Commons;
   
   type African_Langs is ( en, pt, fr ); 
   
   DELIMITER : constant Character := '.';
   
   package NA_Ps is new Parameter_System( 
      Float_Type=>Amount, 
      Counter_Type=>Counter_Type, 
      Languages => African_Langs ); -- EU.BE.I81N.Languages 
   package NA_Ps_X is new NA_Ps.XML;
   
   package globals renames EU.BE.Globals;
   package be_params renames EU.BE.Parameter_System_Declarations;
         
   extra_translations : Templates_Parser.Translate_Set;

   -- package BE_Ps is new Parameter_System( 
      -- Float_Type=>Amount, 
      -- Counter_Type=>Counter_Type, 
      -- Languages => EU.BE.I81N.Languages );
   -- package globals.BE_Parameter_System_XML is new BE_Ps.XML;
   -- 
   -- procedure Uprate( 
               -- c    : in out Amount; 
               -- mult : Amount; 
               -- rec  : BE_Ps.Parameter_Rec; 
               -- which_operation : Integer := 0 ) is
   -- use BE_PS;
   -- begin
      -- case rec.logical_type is
         -- -- need a rooker wise thing here
         -- when any_kind | tax_allowance |
              -- tax_band |  benefit | poverty_line =>
                 -- c := c * mult;
         -- when others => null;
      -- end case;
   -- end Uprate;
      -- 
    -- 
   -- 
   -- package BE_IO is new BE_Ps.Input_Buffer(
      -- Uprate,
      -- EU.BE.I81N.Web_Format,
      -- EU.BE.I81N.Web_Format,
      -- EU.BE.I81N.Web_Format,
      -- EU.BE.I81N.Web_Validate,
      -- EU.BE.I81N.Web_Validate,
      -- EU.BE.I81N.Translations.Lookup
   -- );
   -- package BE_Utils is new BE_IO.Utils;
   -- package BE_Renderer is new BE_IO.EU_Renderer;
   -- 
   

   procedure Test_Parameters_IO( t : in out AUnit.Test_Cases.Test_Case'Class ) is
      
      use be_params.BE_Parameter_System_IO;
      use be_params.BE_Parameter_System;
      use be_params.BE_Parameter_System_XML;
      use EU.BE.I81N;
      use XML_Utils;
      
      -- doc : domc.Document;
      sys : Parameter_System_Rec;
   begin
       Put_Line( "Test_Parameters_IO" );
       sys := globals.Get_BE_Parameter_System;
       be_params.BE_Utils.Write_Parameter_File_Skeleton( "tmp/be_mefisto_skel.prm", sys );	   
   end Test_Parameters_IO;
   
    procedure Test_Javascript( t : in out AUnit.Test_Cases.Test_Case'Class ) is
      
      use be_params.BE_Parameter_System_IO;
      use be_params.BE_Parameter_System;
      use be_params.BE_Parameter_System_XML;
      use EU.BE.I81N;
      use XML_Utils;
      use Ada.Strings.Unbounded.Text_IO;
      use Text_IO;
      doc   : domc.Document;
      sys   : Parameter_System_Rec;
      param : Parameter_Rec;
      n     : Natural;
      s     : Unbounded_String;
      js    : Unbounded_String; 
   begin
       Put_Line( "Test_Javascript" );
       sys := globals.Get_BE_Parameter_System;
       n := Natural(sys.parameters.Length);
       for i in 1 .. n loop
         param := sys.parameters.Element( i );
         s := be_params.BE_Renderer.Make_Validator_Code( To_Unbounded_String("id.fred.joe"), param );
         Put( "param " );Put_Line( s );
       end loop;
       js := be_params.BE_Renderer.Make_Ajax_Call_Indexed( 
               be_params.BE_Renderer.insert_below, 
               TuS( "test.test2.arrays" ),
               5,
               en,
               TuS( "this.is.the.target.key" ));

       Put_Line( js );
   end Test_Javascript;
   
   
   procedure Test_Uprate_Parameters( t : in out AUnit.Test_Cases.Test_Case'Class ) is
      use be_params.BE_Parameter_System_IO;
      use be_params.BE_Parameter_System;
      use be_params.BE_Parameter_System_XML;
      use EU.BE.I81N;
      use XML_Utils;
      use EU.BE;
      doc : domc.Document;
      sys : Parameter_System_Rec;
      use Keyed_Text_Buffer;
      defaults : Text_Buffer := globals.Get_Default_Params;
      input_buffer : be_params.BE_Buffer;
      s1, s2 : Unbounded_String := Null_Unbounded_String;
      nb : Natural;
      array_target : constant Unbounded_String := TuS( "be.income_tax.rate_bands" );
      v  : Amount;
      
      target : Unbounded_String := TuS("be.benefits.unemployment_benefits.unemployment_benefit_after_regular_employment.non_disabled_with_family.more_than_12_months_unemployed.maximum_unemployment_benefit" );
   begin
      --
      -- FIXME this has the default BE rb size (5) wired in 
      -- FIXME convert to assertions
      Put_Line( "Test_Load_Parameters" );
      s1 := defaults.element( s2 & "be.benefits.unemployment_benefits.unemployment_benefit_after_regular_employment.cohabitating_non_disabled.zero_6_months_unemployed.replacement_rate" );
      Assert( s1 = s2 &"60", 
         "be.benefits.unemployment_benefits.unemployment_benefit_after_regular_employment.cohabitating_non_disabled.zero_6_months_unemployed.replacement_rate; should be 60; was " & To_String( s1 ));
      doc := Get_Doc( EU.Web.Settings.XML_File, True );
      
      sys := globals.Get_BE_Parameter_System;
      input_buffer := globals.Get_Loaded_Input_Buffer( en ); 
      v := input_buffer.Get( target );
      Put_Line( "be.benefits.unemployment_benefits.unemployment_benefit_after_regular_employment.non_disabled_with_family.more_than_12_months_unemployed.maximum_unemployment_benefit Before: " & BE_Format_Utils.Format( v ));

      for i in 1 .. 5 loop
         declare
            r : BE_Base_Model_Types.Rate := input_buffer.Get( Tus( "be.income_tax.rate_bands" ), actual_value, i, TuS( "rate" )); 
            b : Amount := input_buffer.Get( Tus( "be.income_tax.rate_bands" ), actual_value, i, TuS( "band" ) ) ;
         begin
            Put_Line( "BEFORE: rate[ "& i'Img & "] = " &  BE_Format_Utils.Format( r ));            
            Put_Line( "BEFORE: band[ "& i'Img & "] = " &  BE_Format_Utils.Format( b ));            
         end;
      end loop;
      

      be_params.BE_Parameter_System_IO.Operate( input_buffer, Null_Unbounded_String, 1.1 );
      v := input_buffer.Get( target );
      Put_Line( "be.benefits.unemployment_benefits.unemployment_benefit_after_regular_employment.non_disabled_with_family.more_than_12_months_unemployed.maximum_unemployment_benefit After: " & BE_Format_Utils.Format( v ));
      
      for i in 1 .. 5 loop
         declare
            r : BE_Base_Model_Types.Rate := input_buffer.Get( Tus( "be.income_tax.rate_bands" ), actual_value, i, TuS( "rate" )); 
            b : Amount := input_buffer.Get( Tus( "be.income_tax.rate_bands" ), actual_value, i, TuS( "band" ) ) ;
         begin
            Put_Line( "AFTER: rate[ "& i'Img & "] = " &  BE_Format_Utils.Format( r ));            
            Put_Line( "AFTER: band[ "& i'Img & "] = " &  BE_Format_Utils.Format( b ));            
         end;
      end loop;
      
      be_params.BE_Parameter_System_IO.Operate( input_buffer, TuS( "be.benefits.income_support" ), 10.0 );
         
      for i in 1 .. 5 loop
         declare
            r : BE_Base_Model_Types.Rate := input_buffer.Get( Tus( "be.income_tax.rate_bands" ), actual_value, i, TuS( "rate" )); 
            b : Amount := input_buffer.Get( Tus( "be.income_tax.rate_bands" ), actual_value, i, TuS( "band" ) ) ;
         begin
            Put_Line( "AFTER 10 * income supp: rate[ "& i'Img & "] = " &  BE_Format_Utils.Format( r ));            
            Put_Line( "AFTER 10 * income supp: band[ "& i'Img & "] = " &  BE_Format_Utils.Format( b ));            
         end;
      end loop;
      
      v := input_buffer.Get( TuS( "be.benefits.income_support.child_benefits.age_supplements_first_child.children_aged_18_25" ));
      Put_Line( "AFTER *10 :be.benefits.income_support.child_benefits.age_supplements_first_child.children_aged_18_25 " & BE_Format_Utils.Format( v ));
      
   end Test_Uprate_Parameters;

      
      
   procedure Test_Load_Parameters( t : in out AUnit.Test_Cases.Test_Case'Class ) is
      use be_params.BE_Parameter_System_IO;
      use be_params.BE_Parameter_System;
      use be_params.BE_Parameter_System_XML;
      use EU.BE.I81N;
      use XML_Utils;
      
      doc : domc.Document;
      sys : Parameter_System_Rec;
      use Keyed_Text_Buffer;
      defaults : Text_Buffer := globals.Get_Default_Params;
      input_buffer : be_params.BE_Buffer;
      s1, s2 : Unbounded_String := Null_Unbounded_String;
      nb : Natural;
      array_target : constant Unbounded_String := TuS( "be.income_tax.rates_and_bands.rate_bands" );
      v  : Amount;
   begin
      --
      -- FIXME this has the default BE rb size (5) wired in 
      --
      Put_Line( "Test_Load_Parameters" );
      s1 := defaults.element( s2 & "be.benefits.unemployment_benefits.unemployment_benefit_after_regular_employment.cohabitating_non_disabled.zero_6_months_unemployed.replacement_rate" );
      Assert( s1 = s2 &"60", 
         "be.benefits.unemployment_benefits.unemployment_benefit_after_regular_employment.cohabitating_non_disabled.zero_6_months_unemployed.replacement_rate; should be 60; was " & To_String( s1 ));
      doc := Get_Doc( EU.Web.Settings.XML_File, True );
      
      sys := be_params.BE_Parameter_System_XML.Load( doc );
      input_buffer := globals.Get_Loaded_Input_Buffer( en ); 
     
      
      nb := input_buffer.Get_Current_Collection_Size( array_target );
      Assert( nb = 5, "be.benefits.rate_bands should be 5; was " & Natural'Image( nb ));
      input_buffer.Add( array_target, 4 );
      nb := input_buffer.Get_Current_Collection_Size( array_target );
      Assert( nb = 6, "be.benefits.rate_bands should be 6 after insert; was " & Natural'Image( nb ));
      input_buffer.Delete( array_target, 4 );
      nb := input_buffer.Get_Current_Collection_Size( array_target );
      Assert( nb = 5, "be.benefits.rate_bands should be 5 after delete; was " & Natural'Image( nb ));
      for i in 1 .. 35 loop
            input_buffer.Add( array_target, 4 );
            nb := input_buffer.Get_Current_Collection_Size( array_target );
            if( i <= 3 )then
               Assert( nb = i + 5, "loop be.benefits.rate_bands should be " & Natural'Image(i+10) &" after insert; was " & Natural'Image( nb ));
            end if;
      end loop;
      for i in 1 .. 60 loop
         input_buffer.Delete( array_target, 1 );
         nb := input_buffer.Get_Current_Collection_Size( array_target );
         if( i < 8 ) then
            Assert( nb = 8-i, "loop be.benefits.rate_bands should be " & Natural'Image(8-i) &" after insert; was " & Natural'Image( nb ));
         else             
            Assert( nb = 1, "loop be.benefits.rate_bands should be 1 after delete; was " & Natural'Image( nb ));
         end if;
      end loop;
      

      
         -- if( prefix /= Null_Unbounded_String ) and then ( Index( skey, TS(prefix )) = 1 )then
         --   return;   
         -- end if;

      
   end Test_Load_Parameters;   
   
   procedure Test_XPath( t : in out AUnit.Test_Cases.Test_Case'Class ) is
      use XML_Utils;
      use domc;
      doc : domc.Document;
      parameter_systems   : domc.Node_List;
      parameters : domc.Node_List;
      ps         : domc.Node;
      path1       : String := NA_Ps_X.Make_X_Path_From_Path( "/this/is/a/test" );
      path2       : String := NA_Ps_X.Make_X_Path_From_Path( "thisx/isx/ax/testx" );
      path3       : String := NA_Ps_X.Make_X_Path_From_Path( "/this/is/a/test/", True );
      path4       : String := NA_Ps_X.Make_X_Path_From_Path( "thisx/isx/ax/testx/", True );
   begin
      Put_Line( "Test_XPath" );
      Put_Line( "path_1 |" & path1 );
      Put_Line( "path_2 |" & path2 );
      Put_Line( "path_3 |" & path3 );
      Put_Line( "path_4 |" & path4 );
      
      doc := Get_Doc( "etc/na.xml", True );
      Assert( doc /= null, "doc should not be null" );      
      parameters := xp.XPath_Query( doc, "//ParameterSystem[@name='na']/ ParameterSystem[@name='quickies']/Parameter" ); 
      Assert( dnodes.Length( parameters ) > 0, "parameters should have some members" );
      for pno in 1 .. dnodes.Length( parameters ) loop
          ps := dnodes.Item( parameters, pno - 1 );
          Put_Line( "@instanceName=" & Get_Attr_Value( ps, "instanceName" ));         
      end loop;
   end Test_XPath;

   
   procedure Test_Parameters_BE( t : in out AUnit.Test_Cases.Test_Case'Class ) is
      use XML_Utils;
      use be_params.BE_Parameter_System;
      use be_params.BE_Parameter_System_XML;
      use EU.BE.I81N;
      use be_params.BE_Parameter_System.Parameter_Search;
      -- doc : domc.Document;
      pr : Parameter_Rec; 
      q_path : constant String := "be" & DELIMITER & "income_tax";
      ps : Parameter_System_Rec;
      income_tax : Parameter_System_Rec;
      full : Parameter_System_Rec;
      allow : Parameter_Rec;
      allowances : Parameter_System_Rec;
   begin
      Put_Line( "Test_Parameters_BE" );
      ps := globals.Get_BE_Parameter_System;
      income_tax := ps.Get( q_path );
      allowances := ps.Get(  q_path & ".allowances" );
      full := ps.Get( "be" );
      Put_Line( income_tax.Description( description, nl ));
      Assert( income_tax.instance_name = To_Unbounded_String( "income_tax" ), "income_tax.instance_name should be 'income_tax'; was |" & To_String( income_tax.instance_name ));
      Assert( full = ps, "ps and full should be identical where not " );
      allow := Get_Parameter( allowances, "base_tax_allowance" );
      Assert( allow.Description( label, en ) = "Base tax allowance", "allow.Description( label, en ) /= 'Base tax allowance'; was " & allow.Description( label, en ));
      Put_Line( "allow.Description( label, en ) |" & allow.Description( label, en ) & "|" );    
      -- Assert( income_tax.Get_Extra( "sheet" ) = "tin_be", "income_tax.Get_Extra( 'sheet' ) should be 'tin_be' was " & income_tax.Get_Extra( "page" ));
   end Test_Parameters_BE;

   procedure Test_Create_Menus_BE( t : in out AUnit.Test_Cases.Test_Case'Class ) is
      use be_params.BE_Parameter_System;
      use be_params.BE_Parameter_System_XML;
      use EU.BE.I81N;
      use EU.BE.Users;
      use Text_IO;
      q_path : constant String := "be" & DELIMITER & "income_tax" & DELIMITER & "rates_and_bands";
      param_sys : Parameter_System_Rec := globals.Get_BE_Parameter_System;
      user    : constant User_Type := globals.Validate( TuS( "test_user12" ), TuS("some password" ));
      input_buffer : be_params.BE_Buffer;
      path : Unbounded_String_List;
      prefix : Unbounded_String := TuS( "/mefisto/parameters_page/" );
      menu : Unbounded_String;
      path2 : Unbounded_String_List := Split( "/benefits/income_support/child_benefits/age_supplements_first_child", '/' );
      f : File_Type;
      
      path3 : Unbounded_String_List := Split( "/benefits/income_support/", '/' );
      depth : Natural := 0;
      
      URI : constant String := "/mefisto/parameters_page";
      URI2 : constant String := "/mefisto/parameters_page/income_tax/refundable_tax_credit_for_children";
      actual_path : Unbounded_String_List := Split( URI, '/' );
      actual_path_str : Unbounded_String;
   begin
      Put_Line( "Test_Create_Menus_BE" );
      Create( f, Out_File, "web/test_create_menus_be.html" );
      menu := be_params.BE_Renderer.Make_Parameter_Menu( param_sys, path, prefix, user.lang, user );
      Put_Line( f, TS( menu ));
      menu := be_params.BE_Renderer.Make_Parameter_Menu( param_sys, path2, prefix, user.lang, user );
      Put_Line( f, TS( menu ));
      param_sys.Complete_Path_To_Left( path3, depth );
      Put_Line( f, Join( path3, '/' ));
      
      actual_path.Delete( 1, 2 );
      depth := 0;
      param_sys.Complete_Path_To_Left( actual_path, depth );
      actual_path_str := TuS( Join( actual_path, '/' ));
      Put_Line( f, "path_str from URI=" & TS( actual_path_str ));
      menu := be_params.BE_Renderer.Make_Parameter_Menu( 
         param_sys, 
         actual_path, 
         prefix, 
         user.lang, 
         user );
      Put_Line( f, TS( menu ));
      
      actual_path := Split( URI2, '/' );
      actual_path.Delete( 1, 2 );
      depth := 0;
      param_sys.Complete_Path_To_Left( actual_path, depth );
      actual_path_str := TuS( Join( actual_path, '/' ));
      Put_Line( f, "path_str from URI2=" & TS( actual_path_str ));
      menu := be_params.BE_Renderer.Make_Parameter_Menu( 
         param_sys, 
         actual_path, 
         prefix, 
         user.lang, 
         user );
      Put_Line( f, TS( menu ));

      Close( f );
      
   end Test_Create_Menus_BE;
   
   procedure Test_Create_Inputs_BE( t : in out AUnit.Test_Cases.Test_Case'Class ) is
      use XML_Utils;
      use be_params.BE_Parameter_System;
      use be_params.BE_Parameter_System_XML;
      use EU.BE.I81N;
      use EU.BE.Users;
      use Keyed_Text_Buffer;
      use Text_IO;
      -- doc : domc.Document;
      pr : Parameter_Rec; 
      q_path : constant String := "be" & DELIMITER & "income_tax" & DELIMITER & "rates_and_bands";
      ps : Parameter_System_Rec := globals.Get_BE_Parameter_System;
      income_tax : Parameter_System_Rec;
      full_sys : Parameter_System_Rec;
      pension : Parameter_Rec;
      user    : constant User_Type := globals.Validate( TuS( "test_user12" ), TuS("some password" ));
      defaults : Text_Buffer := globals.Get_Default_Params;
      input_buffer : be_params.BE_Buffer;
      input_page : Unbounded_String;
      f : File_Type;
   begin
      Create( f, Out_File, "web/test_create_inputs_be.html" );
      Put_Line( "Test_Create_Inputs_BE" );
      -- doc := Get_Doc( EU.Web.Settings.XML_File, True );
      ps := globals.Get_BE_Parameter_System;
      income_tax := ps.Get( q_path );
      full_sys := ps.Get( "be" );
      input_buffer := globals.Get_Loaded_Input_Buffer( en );
      Put_Line( income_tax.Description( description, nl ));
      input_page := be_params.BE_Renderer.Create_Input_Page(
         buff                => input_buffer,
         model_menu          => TuS( "MODEL MENU HERE" ),
         base_sys            => full_sys, 
         sys                 => income_tax, 
         parameter_prefix    => TuS( q_path ),
         main_error_message  => Null_Unbounded_String,
         job_is_running      => False,
         user                => User,
         extra_translations  => extra_translations );
      Put_Line( f, TS( input_page ));
      
      Close( f );
   end Test_Create_Inputs_BE;
   
   procedure Test_Parameters( t : in out AUnit.Test_Cases.Test_Case'Class ) is
      use XML_Utils;
      use NA_Ps;
      use NA_Ps_X;

      function Get( this : Parameter_System_Rec; name : String ) return Parameter_Rec is
         use Ada.Exceptions; 
         use Parameter_Rec_Package;
         p : Parameter_Rec;
         ub_name : constant Unbounded_String := To_Unbounded_String( name );
         n : Natural := Natural(this.parameters.Length);
      begin
         for i in 1 .. n loop
            p := this.parameters.Element( i );
            if( p.instance_name = ub_name )then
               return p;
            end if;
         end loop;
         Raise_Exception( Param_Exception'Identity, 
            "Parameter : Get: parameter with name |" & name & "| was not found " );
      end Get;
      
      doc : domc.Document;
      pr : Parameter_Rec; 
      q_path : constant String := "na" & DELIMITER & "quickies";
      ps : Parameter_System_Rec;
      quickies : Parameter_System_Rec;
      full : Parameter_System_Rec;
      pension : Parameter_Rec;
   begin
      Put_Line( "Test_Parameters" );
      doc := Get_Doc( "etc/na.xml", True );
      ps := Load( doc );
      quickies := ps.Get( q_path );
      full := ps.Get( "na" );
      Assert( quickies.Description( label, en ) = "Quick changes", "quickies.Description( label, en ) = 'Quick changes'; was " & quickies.Description( label, en ));
      Assert( quickies.instance_name = To_Unbounded_String( "quickies" ), "quickies.instance_name should be 'quickies'; was |" & To_String( quickies.instance_name ));
      Assert( full = ps, "ps and full should be identical where not " ); 
      Assert( quickies.Get_Extra( "page" ) = "page2", "quickies.Get_Extra( 'page' ) should be 'page2' was " & quickies.Get_Extra( "page" ));
      pension := Get( quickies, "pension" );
      Assert( pension.Get_Extra( "row" ) = "row22",  "pension.getExtra( 'row' ) /= 'row22' & was |" & pension.Get_Extra( "row" ));
   end Test_Parameters;
   
   
   procedure Test_Parameters_XML( t : in out AUnit.Test_Cases.Test_Case'Class ) is
      use XML_Utils;
      use NA_Ps;
      use NA_Ps_X;
      
      doc : domc.Document;
      pr : Parameter_Rec; 
      path : constant String := Make_X_Path_From_Path( "" & DELIMITER & "na" & DELIMITER & "quickies" & DELIMITER & "standardrateofvat", True );
      sys_path : constant String := Make_X_Path_From_Path( "na" );
      ps : Parameter_System_Rec;
   begin
      Put_Line( "Test_Parameters_XML" );
      Put_Line( "searching for param " & path );
      doc := Get_Doc( "etc/na.xml", True );
      Put_Line( "doc loaded OK" );
      pr := Load( path, doc );
      Put_Line( "parameters loaded OK" );
      Assert( pr.Description( description, en ) = "Change the standard Rate of VAT to this.", 
         "pr.description( description, en ) should be 'Change the standard Rate of VAT to this.' was |" & 
         pr.Description( description, en ));
      Put_Line( "pr.Description( label, pt ) " & pr.Description( label, pt ));
      Put_Line( "pr.Description( tooltip, fr ) " & pr.Description( tooltip, fr ));
      ps := Load( sys_path, doc );
      Put_Line( "ps.Description( label, pt ) " & ps.Description( label, pt ));
      Put_Line( "ps.Description( tooltip, fr ) " & ps.Description( tooltip, fr ));
      Put_Line( "ps.Description( description, en ) " & ps.Description( description, en ));
      Put_Line( "ps.Description( tooltip, en ) " & ps.Description( tooltip, en ));
     
   end Test_Parameters_XML;
   
   procedure Test_XML_Utils( t : in out AUnit.Test_Cases.Test_Case'Class ) is
      use XML_Utils;
      
      doc : domc.Document;
      parameter_systems   : domc.Node_List;
      ps : domc.Node;
   begin
      Put_Line( "Test_XML_Utils" );
      doc := Get_Doc( "etc/na.xml", True );
      parameter_systems := docs.Get_Elements_By_Tag_Name( doc, "ParameterSystem" );
      for pno in 1 .. dnodes.Length( parameter_systems ) loop
          ps := dnodes.Item( parameter_systems, pno - 1 );
          Put_Line( "@name=" & Get_Attr_Value( ps, "name" ));
          
      end loop;      
   end Test_XML_Utils;
   
   procedure Test_Mini_Load( t : in out AUnit.Test_Cases.Test_Case'Class ) is
      use be_params.BE_Parameter_System;
      use be_params.BE_Parameter_System_XML;
      use XML_Utils;
      doc : domc.Document;
      sys_path : constant String := Make_X_Path_From_Path( "be" );
      ps : Parameter_System_Rec;
      p  : Parameter_Rec;
   begin
      doc := Get_Doc( "etc/be_mefisto_mini_test_version.xml", True ); 
      ps := Load( sys_path, doc );
      p := ps.parameters.Element( 1 );
      Assert( p.edit_info.onchange = TuS( "alert( 'BOO' );" ), "on change should be |alert( 'BOO' );| was |" & TS( p.edit_info.onchange ) & "|" );
   end Test_Mini_Load;
   
   procedure Test_Basic_XML( t : in out AUnit.Test_Cases.Test_Case'Class ) is
   
      use Input_Sources.File;
      use Sax.Readers;
      use DOM.Readers;
      
      use DOM.Core;
      
      use DOM.Core.Documents;
      use DOM.Core.Nodes;
      use DOM.Core.Attrs;
      
      input  : File_Input;
      reader : Tree_Reader;
      doc    : Document;
      parameter_systems   : Node_List;
      ps     : Node;
      name     : Attr;
   begin
      Put_Line( "Test_Basic_XML" );
        
      Set_Public_Id( input, "ParameterSystem NA" );
      
      Open( "etc/na.xml", input );
      
      Set_Feature( reader, Validation_Feature, True );
      Set_Feature( reader, Namespace_Feature, False );
      
      Parse( reader, input );
      Close( input );
      
      doc := Get_Tree( reader );
      
      parameter_systems := Get_Elements_By_Tag_Name( doc, "ParameterSystem" );
      for pno in 1 .. Length( parameter_systems ) loop
         ps := Item( parameter_systems, pno - 1 );
         name := Get_Named_Item( Attributes( ps ), "name" );
         Put_Line( "Value of |" & Value( name ) & "|" ); 
      end loop;

      
      Free( reader );
      
   end Test_Basic_XML;
   
   procedure Test_Create_Euromod_Parameter_Mappings( t : in out AUnit.Test_Cases.Test_Case'Class ) is
      use Templates_Parser;
      use EU.BE.I81N;
      input_buffer : be_params.BE_Buffer := globals.Get_Loaded_Input_Buffer( en );
      translations : Translate_Set;
   begin
      Put_Line( "Test_Create_Euromod_Parameter_Mappings" );
      translations := be_params.BE_Renderer.Create_Euromod_Parameter_Mappings( input_buffer );
      Web_Utils.Dump_Translations( translations );
   end Test_Create_Euromod_Parameter_Mappings;
   
   procedure Test_Parse_Keys( t : in out AUnit.Test_Cases.Test_Case'Class ) is
      key : Unbounded_String;
      s   : Unbounded_String := Null_Unbounded_String;
      is_indexed_key  : Boolean;
      base_key        : Unbounded_String;
      index           : Natural;
      postfix         : Unbounded_String;
   begin
      Put_Line( "Test_Parse_Keys" );
      Line_Extractor.Parse_Indexed_Key( TuS( "HEHE" ), is_indexed_key, base_key, index, postfix );
      Assert( not is_indexed_key, "HEHE should not be indexed key but was" );
      Line_Extractor.Parse_Indexed_Key( TuS( "FRED[ 222 ].jojo" ), is_indexed_key, base_key, index, postfix );
      Assert( is_indexed_key, " 'FRED[ 222 ].jojo' should be indexed key but wasn't" );
      Assert( base_key = TuS( "FRED" ), " 'FRED[ 222 ].jojo' should be have base key 'FRED' but was |" & TS( base_key ));
      Assert( postfix = TuS( "jojo" ), " 'FRED[ 222 ].jojo' should be have postfix 'jojo' but was |" & TS( postfix ));
      Assert( index = 222, " 'FRED[ 222 ].jojo' should be have index=222 but was |" & Natural'Image( index ));
      Line_Extractor.Parse_Indexed_Key( TuS( "FRED[ xxx ].jojo" ), is_indexed_key, base_key, index, postfix );
      Assert( not is_indexed_key, " 'FRED[ xxx ].jojo' shouldn't be indexed key but wasn" );
   end Test_Parse_Keys;
   
   procedure Register_Tests( t : in out Test_Case ) is
   begin 
        Register_Routine( T, Test_Basic_XML'Access, "Test Basic XML" );
        Register_Routine( T, Test_XML_Utils'Access, "Test XML Utils" );
        Register_Routine( T, Test_XPath'Access, "Test XPath" );
        Register_Routine( T, Test_Parameters_XML'Access, "Test Parameters_XML" );
        Register_Routine( T, Test_Parameters'Access, "Test Parameters" );
        Register_Routine( T, Test_Parameters_BE'Access, "Test Parameters BE" );
        Register_Routine( T, Test_Parameters_IO'Access, "Test Parameter IO" );
        Register_Routine( T, Test_Load_Parameters'Access, "Test Load Parameters" );
        Register_Routine( T, Test_Parse_Keys'Access, "Test Parse Keys" );
        Register_Routine( T, Test_Javascript'Access, "Test Javascript" );
        Register_Routine( T, Test_Create_Inputs_BE'Access, "Test_Create_Inputs_BE" );
        Register_Routine( T, Test_Uprate_Parameters'Access, "Test Uprate Parameters" );
        Register_Routine( T, Test_Create_Menus_BE'Access, "Test Create Menus" );
        Register_Routine( T, Test_Create_Euromod_Parameter_Mappings'Access, "Test Create Euromod Parameter Mappings" );
        Register_Routine( T, Test_Mini_Load'Access, "Test Mini Load" );
   end Register_Tests;

   procedure Set_Up (T : in out Test_Case) is
      use Templates_Parser;
   begin
      be_params.BE_Parameter_System.Set_Delimiter( DELIMITER );
      NA_Ps.Set_Delimiter( DELIMITER );
      Line_Extractor.Set_Delimiter( DELIMITER );
      Insert( extra_translations, Assoc( "LANG", EU.BE.I81N.Lang_Str( EU.BE.I81N.en )));
      Insert( extra_translations, Assoc( "TEMPLATE_ROOT",  EU.Web.Settings.template_components_path ));
      Insert( extra_translations, Assoc( "SEP", EU.Web.Settings.Dir_Separator ));
      Insert( extra_translations, Assoc( "ROOT", EU.Web.Settings.Mefisto_Root ));
      Insert( extra_translations, Assoc( "JOB_IS_RUNNING", False ));
      Insert( extra_translations, Assoc( "USERNAME", "USERNAME" ));
      Insert( extra_translations, Assoc( "RANDOM_STRING", Utils.Random_String ));
      Insert( extra_translations, Assoc( "URI", "SOME URI" ));
      Insert( extra_translations, Assoc( "PAGE-TITLE", "" ));
   end Set_Up;

   ----------
   -- Name --
   ----------
   function Name ( T : Test_Case ) return Message_String is
   begin
      return Format( "DOM and Parameter Tests" );
   end Name;


end DOM_And_Parameter_Tests;
