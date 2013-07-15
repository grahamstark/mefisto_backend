with Ada.Text_IO;
with Ada.Strings.Unbounded.Text_IO;
with Ada.Exceptions;

with AUnit.Test_Cases;
with AUnit.Assertions;

with GNAT.MD5;

with EU.BE.I81N;
with EU.BE.Users;
with EU.BE.Users.IO;
with EU.BE.Globals;
with EU.BE.Parameter_System_Declarations;

with EU.BE.Model.Settings;
with EU.BE.Model.Runner;
with EU.Web.Settings;
with BE_Base_Model_Types;
with Text_Utils;
with Strings_Edit.Float_Edit;
with T_Utils;
with T_Utils.Standard_Chart_Generator;
with General_Chart_Constants;
with Inequality_Generator;
with Inequality_Generator.Web_IO;

package body Web_Tests is

   use AUnit.Assertions;
   use AUnit.Test_Cases.Registration;
   use Ada.Text_IO;
   use BE_Base_Model_Types;
   use Text_Utils;
   use General_Chart_Constants;
   use EU.BE.I81N;

   package globals              renames EU.BE.Globals;
   
   subtype Param_Buff           is EU.BE.Parameter_System_Declarations.BE_Buffer;
   
   type Test_Items is ( item1, item2, item3, item4, item5 );
   
   function PP( t : Test_Items ) return String is
   begin
      return Test_Items'Image( t )( 2 .. Test_Items'Image( t )'Length );
   end PP;
   
   package TT is new T_Utils( Test_Items, Rate, Amount, Counter_Type );
   
   package TT_Chart is new TT.Standard_Chart_Generator;
   
   function Is_Sensible_Graph( sys : Pre_Or_Post; ctype : Chart_Type  ) return Boolean is
      go : Boolean := True;
   begin
      if( ctype = time_series )then
         go := False;
      else  
         case sys is
         when pre | post =>
             if( ctype = radar ) or ( ctype = polar )then
                go := False;
             end if;
         when others => go := True;
         end case;
      end if;
       return go;
   end Is_Sensible_Graph;
  
   package iqg is new Inequality_Generator( Rate=>Rate, Amount=>Amount );
   package iqg_Web_IO is new iqg.Web_IO;
   
   function Get_Test_Quantiles return iqg.Quantile_List is
      quant : iqg.Quantile;
      quant_list : iqg.Quantile_List;
      bins  :  iqg.Quantile_List;
   begin
      quant := ( index=>1, population=>1_145_008.0,income=>2954.35);
      iqg.Quantile_Package.append( quant_list, quant );
      iqg.Quantile_Package.append( quant_list, ( index=>2, population=>1274868.0, income=>9680.00 ));
      iqg.Quantile_Package.append( quant_list, ( index=>3, population=>1489169.0, income=>18586.47 ));
      iqg.Quantile_Package.append( quant_list, ( index=>4, population=>1309984.0, income=>22810.09 ));
      iqg.Quantile_Package.append( quant_list, ( index=>5, population=>1227877.0, income=>27624.27 ));
      iqg.Quantile_Package.append( quant_list, ( index=>6, population=>1333681.0, income=>3671.367 ));
      iqg.Quantile_Package.append( quant_list, ( index=>7, population=>3136635.0, income=>110401.13 ));
      iqg.Quantile_Package.append( quant_list, ( index=>8, population=>3619401.0, income=>162869.10 ));
      iqg.Quantile_Package.append( quant_list, ( index=>9, population=>3105688.0, income=>170061.28 ));
      iqg.Quantile_Package.append( quant_list, ( index=>10, population=>3252768.0, income=>217926.70 ));
      iqg.Quantile_Package.append( quant_list, ( index=>11, population=>3383398.0, income=>291369.02 ));
      iqg.Quantile_Package.append( quant_list, ( index=>12, population=>3126897.0, income=>420418.97 ));
      iqg.Quantile_Package.append( quant_list, ( index=>13, population=>207672.0, income=>68629.44 ));
      iqg.Quantile_Package.append( quant_list, ( index=>14, population=>49031.0, income=>32752.28 ));
      iqg.Quantile_Package.append( quant_list, ( index=>15, population=>13820.0, income=>18659.36 ));
      iqg.Quantile_Package.append( quant_list, ( index=>16, population=>5249.0, income=>15461.40 ));
      iqg.Quantile_Package.append( quant_list, ( index=>17, population=>1247.0, income=>8458.45 ));
      iqg.Quantile_Package.append( quant_list, ( index=>18, population=>686.0, income=>14801.38 ));
      iqg.Sort_By_Income( quant_list );
      -- bins := iqg.Binify( quant_list, 6 );
      return quant_list;
   end Get_Test_Quantiles;

   procedure Chart_Tests( t : in out AUnit.Test_Cases.Test_Case'Class ) is
   use TT;
   use TT_Chart;
   use iqg;
   use iqg_Web_IO;
      h           : Unbounded_String;
      DATA1       : constant TT.Amount_Array := ( -2.1, 10_000.3, 30.5, 40.7, -20.9 );
      DATA2       : constant TT.Amount_Array := ( 12.8, -10.6, 130.4, -40_012.2, 20.0 );
      NN_DATA1    : constant TT.Amount_Array := ( 12.8, 10.6, 20.4, 12.2, 20.0 );
      NN_DATA2    : constant TT.Amount_Array := ( 2.1, 10.3, 30.5, 40.7, 20.9 );
      LL_DATA1    : constant TT.Amount_Array := ( 0.2, 0.4, 0.6, 0.8, 1.0 );
      LL_DATA2    : constant TT.Amount_Array := ( 0.1, 0.3, 0.4, 0.9, 1.0 );
      QUANTILES   : constant Quantile_List := iqg.Binify( Get_Test_Quantiles, 10 );
      IQ_MEASURES : constant Inequality_Array := Generate( Get_Test_Quantiles );
      PLOT_URL    : constant String := "http://localhost:8080/mfplot/plotter";
   begin
      h := h & "<html><head><title>XX</title></head></body><table>";
      h := h & To_String( QUANTILES, IQ_MEASURES, QUANTILES, IQ_MEASURES, "Dump of test lorenz", en );
      h := h & "<pre>" & LINE_BREAK;
      h := h & To_String( IQ_MEASURES ) & LINE_BREAK;
      h := h & "</pre>" & LINE_BREAK;
      Systems:
      for sys in Pre_Or_Post loop
         declare
            sys_str : constant String := Censor_String( Pre_Or_Post'Image( sys ));
         begin
            Chart_Types:
            for ctype in Chart_Type loop
               declare
                  type_str  : constant String := Censor_String( Chart_Type'Image( ctype ));
               begin
                  Chart_Sizes:
                  for size in Chart_Size loop
                     declare
                        size_str  : constant String := Censor_String( Chart_Size'Image( size ));
                     begin
                        Styles:
                        for style in Chart_Style loop
                           if( Is_Sensible_Graph( sys, ctype ))then
                              declare
                                 style_str : constant String := Censor_String( Chart_Style'Image( style ));
                                 title : constant String := sys_str & " : " & type_str;
                                 subtitle : constant String :=  size_str & " : " & style_str;   
                                 url : Unbounded_String;
                                 d1 , d2 : TT.Amount_Array;
                              begin
                                 case ctype is
                                 when bar => 
                                    d1 := DATA1;
                                    d2 := DATA2;
                                 when lorenz =>
                                    d1 := LL_DATA1;
                                    d2 := LL_DATA2;
                                 when others =>
                                    d1 := NN_DATA1;
                                    d2 := NN_DATA2;
                                 end case;  
                                 h := h & "<tr><td>" & LINE_BREAK;
                                 h := h & "<p>" & title & " <em>" & subtitle & "</em></p>" & LINE_BREAK;
                                 if( ctype /= lorenz )then
                                    url := Make_Univariate_Chart(
                                        PLOT_URL,
                                        title,
                                        subtitle,
                                        d1,
                                        "x-axis label",
                                        "y-axis label",
                                        PP'Access,
                                        ctype,
                                        size,
                                        sys,
                                        style,
                                        d2 );
                                 else
                                    url := Make_Standard_Chart(
                                        PLOT_URL,
                                        QUANTILES,
                                        QUANTILES,
                                        title,
                                        size,
                                        en );
                                    
                                 end if;
                                 h := h & "<img src='" & url & "' alt='" & title & " : " & subtitle & "' />"  & LINE_BREAK;
                                 h := h & "<pre>" & url & LINE_BREAK & "</pre>" & LINE_BREAK;
                                 h := h & "</td></tr>" & LINE_BREAK;    
                              end;
                           end if;
                        end loop Styles;
                     end;
                  end loop Chart_Sizes;
               end;
            end loop Chart_Types;
         end;
      end loop Systems;
      h := h & "</table></body></html>" & LINE_BREAK;
      Write_Whole_File( "chart_dumps.html", h );
   end Chart_Tests;
   
   package Local_Float_Edit is new Strings_Edit.Float_Edit( Number => Float );
  --                             value_and_error.input_field_text := Local_Format(  value_and_error.rval, param_desc.edit_info );               

   function Format_For_Input_Field( f : Float; length : Integer; prec : Integer ) return String is
      buff : String( 1 .. 30 ) := ( others => ' ' );
      pos  : Integer := 1;
   begin
      Put_Line( "edit.length = " & length'Img & " edit.prec " & prec'Img );
      Local_Float_Edit.Put( 
         Destination => buff,
         Pointer     => pos,
         Value       => f,
         Field       => 30,
         AbsSmall    => prec );
        return Censor_String( buff );
   end Format_For_Input_Field;
   
   procedure FE_Tests( t : in out AUnit.Test_Cases.Test_Case'Class ) is
   begin
      Put_Line( "100.0 10 2       =" & Format_For_Input_Field( 100.0, 10, -2 ));
      Put_Line( "100_000.0, 10, 2 =" & Format_For_Input_Field( 100_000.0, 10, -2 ));
      Put_Line( "1.0, 10, 2       =" & Format_For_Input_Field( 1.0, 10, -2 ));
      Put_Line( "1.12345, 10, 2   =" & Format_For_Input_Field( 1.12345, 10, -2 ));
      Put_Line( "1.12345, 10, 0   =" & Format_For_Input_Field( 1.12345, 10, 0 ));
      Put_Line( "100.0 10 2       =" & Format_For_Input_Field( 100.0, 2, -10 ));
      Put_Line( "100_000.0, 10, 2 =" & Format_For_Input_Field( 100_000.0, 2, -10 ));
      Put_Line( "1.0, 10, 2       =" & Format_For_Input_Field( 1.0, 2, -10 ));
      Put_Line( "1.12345, 10, 2   =" & Format_For_Input_Field( 1.12345, 2, -10 ));
      Put_Line( "1.12345, 10, 0   =" & Format_For_Input_Field( 1.12345, 2, -10 ));
      
   end FE_Tests;
   
   
   procedure User_Tests( t : in out AUnit.Test_Cases.Test_Case'Class ) is
      use EU.BE.Users;
      use EU.BE.Users.IO;
      use EU.BE.Model.Settings;
      user : User_Type;
      settings : Model_Settings := EU.BE.Globals.Get_Default_Model_Settings;
      sep : constant String := EU.Web.Settings.Dir_Separator;
      working_dir : Unbounded_String;
   begin
      for uno in 1 .. 20 loop
         declare
            nstr : String := Positive'Image( uno );
            username : Unbounded_String := TuS( "test_user" ) & nstr( 2 .. nstr'Length );
            run_num : Positive;
         begin
            user.username := username;
            user.description  := TuS( "A TEST USER" );
            user.password := TuS( GNAT.MD5.Digest( "some password" ));
            Create_User_Files( settings.Working_Root, sep, user );
            for rn in 1 .. 20 loop
               declare
                  run_num_str : String := EU.BE.Model.Runner.Get_New_Run_Id;
               begin
                  user.work_dir := Create_Directories_For_Run( settings.Working_Root, sep, user, run_num_str );
               end;
            end loop;
            Assert( run_num = 20, "run num should always finish on 20 but was " & Positive'Image( run_num ));
            Create_User_Files( settings.Working_Root, sep, user );            
         end;
         
      end loop;
      -- Assert( not is_indexed_key, " 'FRED[ xxx ].jojo' shouldn't be indexed key but wasn" );
      for uno in 1 .. 20 loop
         declare
            default_pwd : constant Unbounded_String := TuS( GNAT.MD5.Digest( "some password" ));
            tmp_user : User_Type;
            nstr : String := Positive'Image( uno );
            username : Unbounded_String := TuS( "test_user" ) & nstr( 2 .. nstr'Length );
            filename : Unbounded_String := settings.Working_Root & sep & username & sep & USER_FILE_NAME;
            run_num : Positive;
            param_buffer  : Param_Buff;
         begin
            tmp_user :=  Read_User( TS( filename ));
            Assert( tmp_user.username = username, "username mismatch; should be " & TS( username ) & "| was " & TS( tmp_user.username ));
            Assert( tmp_user.password = default_pwd, "password mismatch; should be " & TS( default_pwd ) & "| was " & TS( tmp_user.password ));
            -- run_num :=  Get_Next_Run_Number( settings.Working_Root, sep, user );
            param_buffer := globals.Get_Loaded_Input_Buffer( tmp_user.lang );
            -- Assert( run_num = 21, "run num should be 21 on re-read but was " & Positive'Image( run_num ));
         end;
      end loop;
   end User_Tests;
   
   procedure Register_Tests( t : in out Test_Case ) is
   begin 
      Register_Routine( T, Chart_Tests'Access, "Chart tests" );
      Register_Routine( T, User_Tests'Access, "Test Users" );
      Register_Routine( T, FE_Tests'Access, "Floating point tests" );
   end Register_Tests;

   procedure Set_Up (T : in out Test_Case) is
   begin
      null;
   end Set_Up;

   ----------
   -- Name --
   ----------
   function Name ( T : Test_Case ) return Message_String is
   begin
      return Format( "Web Tests" );
   end Name;


end Web_Tests;
