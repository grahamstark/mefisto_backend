with Ada.Containers.Hashed_Sets;

with Costs_Tabulator.Web_IO;
with EU.Web.Settings;
with Google_Chart_Constants;
with Inequality_Generator.Web_IO;
with Poverty_Tabulator.Web_IO;
with Poverty_Tabulator;
with Tabulator.Web_IO;
with Templates_Parser;
with Web_Utils;
with EU.BE.Main_Menu;

package body EU.BE.Output.Web_IO is
   
   use Translations;
   package euws renames EU.Web.Settings; 
   
   function Get_Summary_Table( 
      outputs        : Outputs_Rec;
      lang           : Languages        := Languages'First ) return Unbounded_String is
   use Templates_Parser;
       translations : Translate_Set;
   begin
      
      Insert( translations, 
         Assoc( "GAIN-LOSE-SUMMARY", 
                EU.BE.Output.Gain_Lose_Web_IO.Get_Summary( outputs, lang )));
      Insert( translations, 
         Assoc( "POVERTY-SUMMARY", 
                EU.BE.Output.Poverty_Web_IO.Get_Summary( outputs, lang )));
      Insert( translations, 
         Assoc( "INEQUALITY-SUMMARY", 
                EU.BE.Output.Inequality_Web_IO.Get_Summary( outputs, lang )));
      Insert( translations, 
         Assoc( "BUDGET-SUMMARY", 
                EU.BE.Output.Budget_Web_IO.Get_Summary( outputs, lang )));
      return Web_Utils.Parse_Template( TuS( euws.template_components_path ) & 
         euws.Dir_Separator & "summary_output", translations );      
      
   end Get_Summary_Table;
   
   function Get_Poverty_Control_Section( 
      sysno          : System_Number      := 1;
      breakdown      : Disaggregated_Breakdown_Target   := by_decile;
      do_differences : Boolean            := False;
      lang           : Languages := Languages'First ) return Unbounded_String is
      
      package Breakdown_Target_T is new T_Utils( T => Disaggregated_Breakdown_Target, Rate_Type => Rate, Amount_Type => Amount, Counter_Type => Counter_Type );
      package Breakdown_Target_T_Web_IO is new Breakdown_Target_T.Web_IO;
   
      package System_Number_T is new T_Utils( T => System_Number, Rate_Type => Rate, Amount_Type => Amount, Counter_Type => Counter_Type );
      package System_Number_T_Web_IO is new System_Number_T.Web_IO;
      
      function Pretty_Print( breakdown : Disaggregated_Breakdown_Target ) return String is
      begin
         return Lookup( Censor_String( Disaggregated_Breakdown_Target'Image( breakdown )), lang);
      end Pretty_Print;
      
      function Pretty_Print( sysno : System_Number ) return String is
      begin
          case sysno is
             when 1 => return Lookup( "Pre System", lang );
             when 2 => return Lookup( "Post System", lang );
          end case;
       end Pretty_Print;

      s : Unbounded_String;
      checkbox : Unbounded_String;
      dd : Boolean := do_differences;
      
       
   begin
      s := s & "<table class='control_section'>" & LINE_BREAK;
      
      s := s & "   <tr><td><label for='breakdown'>"  & Lookup( "Break Down By?", lang ) & "</label></td><td>" & LINE_BREAK;
      s := s & Breakdown_Target_T_Web_IO.Make_Select( 
        "breakdown",
         breakdown, Pretty_Print'Access );
      s := s & "   </td></tr>" & LINE_BREAK;

      s := s & "   <tr><td><label for='sysno'>"  & Lookup( "System to display?", lang ) & "</label></td><td>" & LINE_BREAK;
      s := s & System_Number_T_Web_IO.Make_Select( 
         "sysno",
         sysno, Pretty_Print'Access);
      s := s & "   </td></tr>" & LINE_BREAK;
      
      s := s & "   <tr><td><label>"  & Lookup( "Print differences from base?", lang ) & "</label></td><td>" & LINE_BREAK;
      BE_HTML.Make_One_Input( "do_differences", checkbox, dd, False );
      s := s & checkbox;
      s := s & "   </td></tr>" & LINE_BREAK;
      
      s := s & "  <tr><td>" & LINE_BREAK;
      s := s & "     <input id='output_submit_button' type='submit' value='" & Lookup( "Redraw", lang ) & "' name='Redraw'  onclick='submitOutputForm( ""poverty_page"" );return false;' />" & LINE_BREAK;
      s := s & "  </td></tr>" & LINE_BREAK;
      s := s & "</table>" & LINE_BREAK;
      return s;
   end Get_Poverty_Control_Section;

   
   function Get_Budget_Control_Section(
      sysno          : System_Number      := 1;
      breakdown      : Disaggregated_Breakdown_Target   := by_decile;
      print_counts   : Boolean            := False;
      do_differences : Boolean            := False;
      lang           : Languages          := Languages'First) return Unbounded_String is
         
      package Breakdown_Target_T is new T_Utils( T => Disaggregated_Breakdown_Target, Rate_Type => Rate, Amount_Type => Amount, Counter_Type => Counter_Type );
      package Breakdown_Target_T_Web_IO is new Breakdown_Target_T.Web_IO;
      
      package System_Number_T is new T_Utils( T => System_Number, Rate_Type => Rate, Amount_Type => Amount, Counter_Type => Counter_Type );
      package System_Number_T_Web_IO is new System_Number_T.Web_IO;

      function Pretty_Print( breakdown : Disaggregated_Breakdown_Target ) return String is
      begin
         return Lookup( Censor_String( Disaggregated_Breakdown_Target'Image( breakdown )), lang );
      end Pretty_Print;
      
      function Pretty_Print( sysno : System_Number ) return String is
      begin
          case sysno is
             when 1 => return Lookup( "Pre System", lang );
             when 2 => return Lookup( "Post System", lang );
          end case;
       end Pretty_Print;

      s : Unbounded_String;
      checkbox : Unbounded_String;
      pc : Boolean := print_counts;
      dd : Boolean := do_differences;
      
   begin
      s := s & "<table class='control_section'>" & LINE_BREAK;
      
      s := s & "   <tr><td><label for='breakdown'>"  & Lookup( "Break Down By?", lang ) & "</label></td><td>" & LINE_BREAK;
      s := s & Breakdown_Target_T_Web_IO.Make_Select( 
         "breakdown", breakdown, Pretty_Print'Access );
      s := s & "   </td></tr>" & LINE_BREAK;

      s := s & "   <tr><td><label for='sysno'>"  & Lookup( "System to display?", lang ) & "</label></td><td>" & LINE_BREAK;
      s := s & System_Number_T_Web_IO.Make_Select( 
         "sysno", sysno, Pretty_Print'Access);
      s := s & "   </td></tr>" & LINE_BREAK;
      
      s := s & "   <tr><td><label for='print_counts'>"  & Lookup( "Print caseloads?", lang ) & "</label></td><td>" & LINE_BREAK;
      BE_HTML.Make_One_Input( "print_counts", checkbox, pc, False );
      s := s & checkbox;
      s := s & "   </td></tr>" & LINE_BREAK;

      s := s & "   <tr><td><label for='do_differences'>"  & Lookup( "Print differences from base?", lang ) & "</label></td><td>" & LINE_BREAK;
      BE_HTML.Make_One_Input( "do_differences", checkbox, dd, False );
      s := s & checkbox;
      s := s & "   </td></tr>" & LINE_BREAK;
      
      s := s & "  <tr><td>" & LINE_BREAK;
      s := s & "     <input id='output_submit_button' type='submit' value='" & Lookup( "Redraw", lang ) & "' name='Redraw' onclick='submitOutputForm( ""budget_page"" );return false;' />" & LINE_BREAK;
      s := s & "  </td></tr>" & LINE_BREAK;
      s := s & "</table>" & LINE_BREAK;
      return s;
   end Get_Budget_Control_Section;

   function Get_Inequality_Control_Section(
      breakdown     : Breakdown_Target   := no_breakdown;
      lang          : Languages := Languages'First ) return Unbounded_String is
        
      package Breakdown_Target_T is new T_Utils( T => Breakdown_Target, Rate_Type => Rate, Amount_Type => Amount, Counter_Type => Counter_Type );
      package Breakdown_Target_T_Web_IO is new Breakdown_Target_T.Web_IO;
      function Pretty_Print is new Translations.Lookup_V( T=>Breakdown_Target );

      function Pretty_Print( v : Breakdown_Target ) return String is
      begin
         return Pretty_Print( v, lang );
      end  Pretty_Print;
        
      s : Unbounded_String;
   begin
      s := s & "<table class='control_section'>" & LINE_BREAK;
      s := s & "   <tr><td><label for='breakdown'>"  & Lookup( "Break Down By?", lang ) & "</label></td><td>" & LINE_BREAK;
      s := s & Breakdown_Target_T_Web_IO.Make_Select( 
         "breakdown", breakdown, Pretty_Print'Access);
      s := s & "   </td></tr>" & LINE_BREAK;
      s := s & "  <tr><td>" & LINE_BREAK;
      s := s & "     <input id='output_submit_button' type='submit' value='" & Lookup( "Redraw", lang ) & "' name='Redraw' onclick='submitOutputForm( ""inequality_page"" );return false;' />" & LINE_BREAK;
      s := s & "  </td></tr>" & LINE_BREAK;
      s := s & "</table>" & LINE_BREAK;
      return s;
   end Get_Inequality_Control_Section;

   function Get_Gain_Lose_Control_Section(
      breakdown        : Disaggregated_Breakdown_Target   := by_decile;
      comp_cell        : Compare_Cell       := current_cell;
      cell_op          : Cell_Compare_Type  := counter;
      value_To_Use     : Summary_Items_Type := disposable_income;
      lang             : Languages := Languages'First;
      advanced_version : Boolean := False ) return Unbounded_String is
        
      package Summary_Items_Type_T is new T_Utils( T => Summary_Items_Type, Rate_Type => Rate, Amount_Type => Amount, Counter_Type => Counter_Type );
      package Summary_Items_Type_T_Web_IO is new Summary_Items_Type_T.Web_IO;
      function Pretty_Print is new Translations.Lookup_V( T=> Summary_Items_Type );
      
      package Cell_Compare_Type_T is new T_Utils( T => Cell_Compare_Type, Rate_Type => Rate, Amount_Type => Amount, Counter_Type => Counter_Type );
      package Cell_Compare_Type_T_Web_IO is new Cell_Compare_Type_T.Web_IO;
      function Pretty_Print is new Translations.Lookup_V( T=> Cell_Compare_Type );
      
      package Breakdown_Target_T is new T_Utils( T => Disaggregated_Breakdown_Target, Rate_Type => Rate, Amount_Type => Amount, Counter_Type => Counter_Type );
      package Breakdown_Target_T_Web_IO is new Breakdown_Target_T.Web_IO;
      
      function Pretty_Print( breakdown : Disaggregated_Breakdown_Target ) return String is
      begin
         return Lookup( Censor_String( Disaggregated_Breakdown_Target'Image( breakdown )), lang);
      end Pretty_Print;

      package Compare_Cell_T is new T_Utils( T => Compare_Cell, Rate_Type => Rate, Amount_Type => Amount, Counter_Type => Counter_Type );
      package Compare_Cell_T_Web_IO is new Compare_Cell_T.Web_IO;
      function Pretty_Print is new Translations.Lookup_V( T=> Compare_Cell );   
         
       -- 
      function Pretty_Print( v : Compare_Cell ) return String is
      begin
         return Pretty_Print( v, lang );
      end  Pretty_Print;
      
      function Pretty_Print( v : Cell_Compare_Type ) return String is
      begin
         return Pretty_Print( v, lang );
      end  Pretty_Print;
      
      function Pretty_Print( v : Summary_Items_Type ) return String is
      begin
         return Pretty_Print( v, lang );
      end  Pretty_Print;
        
      s : Unbounded_String;
   begin
      s := s & "<table class='control_section'>" & LINE_BREAK;
      
      s := s & "   <tr><td>"  & Lookup( "Break Down By?", lang ) & "</td><td>" & LINE_BREAK;
      
      s := s & Breakdown_Target_T_Web_IO.Make_Select( 
         "breakdown", breakdown, Pretty_Print'Access);
     
      s := s & "   </td></tr>" & LINE_BREAK;
      
      s := s & "   <tr><td>" & Lookup( "Express table as?", lang ) & "</td><td>" & LINE_BREAK;
      s := s & Compare_Cell_T_Web_IO.Make_Select( 
         "comp_cell", comp_cell, Pretty_Print'Access);
      s := s & "   </td></tr>" & LINE_BREAK;

      if( advanced_version )then
         s := s & "   <tr><td>"& Lookup( "Item shown in cells", lang ) & "</td><td>"  & LINE_BREAK;
         s := s & Summary_Items_Type_T_Web_IO.Make_Select( 
            "value_to_use", value_to_use, Pretty_Print'Access );
         s := s & "   </td></tr>" & LINE_BREAK;

         s := s & "  <tr><td>" & Lookup( "Compare ", lang ) & "</td><td>"  &  LINE_BREAK;
         s := s & Cell_Compare_Type_T_Web_IO.Make_Select( 
            "cell_op", cell_op, Pretty_Print'Access );
         
         s := s & "  </td></tr>" & LINE_BREAK;
      end if;
      s := s & "  <tr><td>" & LINE_BREAK;
      s := s & "     <input id='output_submit_button' type='submit' value='" & Lookup( "Redraw", lang ) & "' name='Redraw'  onclick='submitOutputForm( ""gain_lose_page"" );return false;'/>" & LINE_BREAK;
      s := s & "  </td></tr>" & LINE_BREAK;
      s := s & "</table>" & LINE_BREAK;
      return s;
   end Get_Gain_Lose_Control_Section;
     
   
   function Get_Output_Menu( 
      which_page : Output_Page_Type;
      lang       : Languages ) return Unbounded_String is
      s : Unbounded_String;
   begin
      s := s & "<div id='output_menu'>" & LINE_BREAK;
      s := s & "  <ul>" & LINE_BREAK; 
      for b in Output_Page_Type loop
      declare
         k : String := Censor_String( Output_Page_Type'Image( b ));
         text : String := Lookup( k, lang );
      begin
         -- if( b /= which_page )then
         s := s & "      <li><a href='/mefisto/output_page/" & k & "/' title='output_target'>" & text & "</a></li>" & LINE_BREAK;
         --  else
         --     s := s & "      <li class='on'>" & text & "</li>" & LINE_BREAK;
         --  end if;
      end;
      end loop;
      s := s & "   </ul>"& LINE_BREAK;
      s := s & "</div>"& LINE_BREAK;
      return s;  
   end Get_Output_Menu;
    
   function Get_Output_Page(  translations    : Translate_Set;
                              which_page      : Output_Page_Type;
                              control_section : Unbounded_String;
                              gallery         : Unbounded_String;
                              content         : Unbounded_String;
                              lang            : Languages ) return Unbounded_String is
   use Templates_Parser;
   use EU.BE.Main_Menu;
       full_translations    : Translate_Set := translations; 
       menu : Unbounded_String;
   begin
      menu := Get_Output_Menu( which_page, lang ) & LINE_BREAK;
      Insert( full_translations, Assoc( "TEMPLATE_ROOT", euws.template_components_path ));
      Insert( full_translations, Assoc( "LANG", Lang_Str( lang )));
      Insert( full_translations, Assoc( "ROOT", euws.Mefisto_Root ));
      Insert( full_translations, Assoc( "SEP", euws.Dir_Separator ));
      Insert( full_translations, Assoc( "MODEL-MENU", menu ));
      Insert( full_translations, Assoc( "GALLERY-SECTION", gallery ));
      Insert( full_translations, Assoc( "CONTENT-SECTION", content ));
      Insert( full_translations, Assoc( "IS-OUTPUT-PAGE", True ));
      Insert( full_translations, Assoc( "CONTROL-SECTION", control_section ));
      Insert( full_translations, Assoc( "WHICH_PAGE", Censor_String( Output_Page_Type'Image( which_page )))); 
      return Web_Utils.Parse_Template( TuS( euws.template_components_path ) & 
         euws.Dir_Separator & "output", translations );      
   end Get_Output_Page;

end  EU.BE.Output.Web_IO;
