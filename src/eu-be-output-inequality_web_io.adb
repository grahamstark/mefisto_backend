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
with General_Chart_Constants;

package body EU.BE.Output.Inequality_Web_IO is
   
   use Translations;
   use General_Chart_Constants;
   
   package Be_Inequality_Web_IO is new 
      Be_Inequality.Web_IO;
   
   package euws renames EU.Web.Settings; 
   
   function Get_Summary(
      outputs       : Outputs_Rec; 
      lang          : Languages          := Languages'First ) return Unbounded_String is
      s : Unbounded_String;
   use Templates_Parser;
       translations : Translate_Set;
       chart        : Unbounded_String;
       gini_change  : constant Amount := 100.0 * (
          outputs.ineq( 2 ).inequality_measures( be_inequality.gini ) - 
          outputs.ineq( 1 ).inequality_measures( be_inequality.gini ));
       igc         : constant Integer := Integer( gini_change );
   begin
      if( Natural( outputs.ineq( 1 ).lorenz.Length ) = 0 )then
         return TuS( "" );
      end if;
      
      if( gini_change > 0.01 )then
         Insert( translations, Assoc( "GINI-ARROW", "up-bad" ));
      elsif( gini_change < -0.01 )then
         Insert( translations, Assoc( "GINI-ARROW", "down-good" ));
      else
         Insert( translations, Assoc( "GINI-ARROW", "nc" ));
      end if;
      chart := Be_Inequality_Web_IO.Make_Standard_Chart(
                  euws.Charts_URL,
                  outputs.ineq( 1 ).lorenz, 
                  outputs.ineq( 2 ).lorenz, 
                  "Lorenz Curve",
                  medium,
                  lang );
                
      Insert( translations, Assoc( "MORE", Lookup( "more", lang ))); 
      Insert( translations, Assoc( "TITLE", Lookup( "Inequality", lang )));
      Insert( translations, Assoc( "CHART", chart ));
      Insert( translations, Assoc( "GINI-CHANGE-STR", Lookup( "Change In Gini", lang )));
      Insert( translations, Assoc( "GINI-CHANGE", GL_Format( gini_change, False, lang )));
      
      return Web_Utils.Parse_Template( TuS( euws.template_components_path ) & 
         euws.Dir_Separator & "inequality_summary", translations );      
   end Get_Summary;
  
   
   function Get_Gallery_Table_Header( lang : Languages ) return Unbounded_String is
      t : Unbounded_String;
   begin
      t := t & "            <table class='datatable' width='100%'>" & LINE_BREAK;
      t := t & "              <thead>" & LINE_BREAK;
      t := t & "                <tr>" & LINE_BREAK;
      t := t & "                  <td width='20%' align='center' valign='middle'>" & LINE_BREAK;
      t := t & "                  </td>" & LINE_BREAK;
      t := t & "                  <td width='20%' align='center'>" & LINE_BREAK;
      t := t & "                     " & Lookup( "Pre", lang ) & LINE_BREAK;
      t := t & "                  </td>" & LINE_BREAK;
      t := t & "                  <td width='20%' align='center'>" & LINE_BREAK;
      t := t & "                     " & Lookup( "Post", lang ) & LINE_BREAK;
      t := t & "                  </td>" & LINE_BREAK;
      t := t & "                  <td width='20%' align='center'>" & LINE_BREAK;
      t := t & "                     " & Lookup( "Change", lang ) & LINE_BREAK;
      t := t & "                  </td>" & LINE_BREAK;
      t := t & "                  <td width='20%' align='center'></td>" & LINE_BREAK;
      t := t & "                </tr>" & LINE_BREAK;
      t := t & "              </thead>" & LINE_BREAK;
      t := t & "              <tbody>" & LINE_BREAK;
      
      return t;
   end Get_Gallery_Table_Header;
   
   function Create_Chart_Popup_Link(
      breakdown     : Breakdown_Target ;
      col           : String;
      size          : Chart_Size;
      title         : String;
      thumbnail_url : Unbounded_String ) return Unbounded_String is
      u : Unbounded_String;
   begin
      u := u & "<a class='picture_thumbnail' href='/mefisto/output_page/inequality_page/single_chart_popup?";
      u := u & "col=" & Censor_String( col );
      u := u & "&amp;size=" & Censor_String( Chart_Size'Image( size ));
      u := u & "&amp;breakdown=" & Censor_String( Breakdown_Target'Image( breakdown ));
      u := u & "' ";
      u := u & "onclick=""ExampleWindow( this.href, '" & title & "', '" & title & "', '' ); return false;"" ";
      u := u & "onfocus='this.blur()'>";
      u := u & "<img src='" & thumbnail_url & "' alt='" & title & "' /></a>"; 
      return u;
   end Create_Chart_Popup_Link;
   
   function Create_Chart(
      outputs      : Outputs_Rec; 
      breakdown    : Breakdown_Target   := no_breakdown;
      col          : String;
      size         : Chart_Size;
      lang         : Languages := Languages'First) return Unbounded_String is
   begin
      case breakdown is
         when by_tenure =>
            declare
               tt : Tenure_Type := Tenure_Type'Value( col );
            begin
               if( Natural( outputs.ineq_by_tenure( 1 )( tt ).lorenz.Length ) > 0 )then
                  return Be_Inequality_Web_IO.Make_Standard_Chart( 
                        euws.Charts_URL,
                        outputs.ineq_by_tenure( 1 )( tt ).lorenz, 
                        outputs.ineq_by_tenure( 2 )( tt ).lorenz,
                        Censor_String( Tenure_Type'Image( tt )),
                        size,
                        lang );
               end if;
            end;
         when by_decile =>
            declare
               dec : Deciles := Deciles'Value( col );
            begin
               if( Natural( outputs.ineq_by_decile( 1 )( dec ).lorenz.Length ) > 0 )then
                  return Be_Inequality_Web_IO.Make_Standard_Chart( 
                     euws.Charts_URL,
                     outputs.ineq_by_decile( 1 )( dec ).lorenz, 
                     outputs.ineq_by_decile( 2 )( dec ).lorenz,
                     Censor_String( Deciles'Image( dec )),
                     size,
                     lang );
               end if;
            end;
         when by_age_of_head =>
            declare
               tt : Adult_Age_Band := Adult_Age_Band'Value( col );
            begin
               if( Natural( outputs.ineq_by_age_band( 1 )( tt ).lorenz.Length ) > 0 )then
                  return Be_Inequality_Web_IO.Make_Standard_Chart( 
                        euws.Charts_URL,
                        outputs.ineq_by_age_band( 1 )( tt ).lorenz, 
                        outputs.ineq_by_age_band( 2 )( tt ).lorenz,
                        Censor_String( Adult_Age_Band'Image( tt )),
                        size,
                        lang );
               end if;
            end;
         when by_occupation_of_head =>
            declare
               tt : Occupation_Isco_1_Digit:= Occupation_Isco_1_Digit'Value( col );
            begin
               if( Natural( outputs.ineq_by_occupation( 1 )( tt ).lorenz.Length ) > 0 )then
                  return Be_Inequality_Web_IO.Make_Standard_Chart( 
                        euws.Charts_URL,
                        outputs.ineq_by_occupation( 1 )( tt ).lorenz, 
                        outputs.ineq_by_occupation( 2 )( tt ).lorenz,
                        Censor_String( Occupation_Isco_1_Digit'Image( tt )),
                        size,
                        lang );
               end if;
            end;
         when no_breakdown =>
            return Be_Inequality_Web_IO.Make_Standard_Chart( 
                  euws.Charts_URL,
                  outputs.ineq( 1 ).lorenz, 
                  outputs.ineq( 2 ).lorenz, 
                  "Whole Population",
                  size,
                  lang );
     end case;
     return TuS( "" );
   end Create_Chart;

   type G_Array is array( 1 .. 3 ) of Amount;
   
   function Extract_Ginis(
      outputs      : Outputs_Rec; 
      breakdown    : Breakdown_Target   := no_breakdown;
      col          : String ) return G_Array is
         ga : G_Array := ( others => 0.0 );
   begin
      case breakdown is
         when by_tenure =>
            declare
               tt : Tenure_Type := Tenure_Type'Value( col );
            begin
               if( Natural( outputs.ineq_by_tenure( 1 )( tt ).lorenz.Length ) > 0 )then
                  ga( 1 ) := outputs.ineq_by_tenure( 1 )( tt ).gini;
                  ga( 2 ) := outputs.ineq_by_tenure( 2 )( tt ).gini;
                  ga( 3 ) := ga(2) - ga( 1 ); 
               end if;
            end;
         when by_decile =>
            declare
               dec : Deciles := Deciles'Value( col );
            begin
               if( Natural( outputs.ineq_by_decile( 1 )( dec ).lorenz.Length ) > 0 )then
                  ga( 1 ) := outputs.ineq_by_decile( 1 )( dec ).gini;
                  ga( 2 ) := outputs.ineq_by_decile( 2 )( dec ).gini;
                  ga( 3 ) := ga(2) - ga( 1 ); 
               end if;
            end;
         when by_age_of_head =>
            declare
               tt : Adult_Age_Band := Adult_Age_Band'Value( col );
            begin
               if( Natural( outputs.ineq_by_age_band( 1 )( tt ).lorenz.Length ) > 0 )then
                  ga( 1 ) := outputs.ineq_by_age_band( 1 )( tt ).gini;
                  ga( 2 ) := outputs.ineq_by_age_band( 2 )( tt ).gini;
                  ga( 3 ) := ga(2) - ga( 1 ); 
               end if;
            end;
         when by_occupation_of_head =>
            declare
               tt : Occupation_Isco_1_Digit:= Occupation_Isco_1_Digit'Value( col );
            begin
               if( Natural( outputs.ineq_by_occupation( 1 )( tt ).lorenz.Length ) > 0 )then
                  ga( 1 ) := outputs.ineq_by_occupation( 1 )( tt ).gini;
                  ga( 2 ) := outputs.ineq_by_occupation( 2 )( tt ).gini;
                  ga( 3 ) := ga(2) - ga( 1 ); 
               end if;
            end;
         when no_breakdown =>
                  ga( 1 ) := outputs.ineq( 1 ).gini;
                  ga( 2 ) := outputs.ineq( 2 ).gini;
                  ga( 3 ) := ga(2) - ga( 1 ); 
     end case;
     return ga;
   end Extract_Ginis;

   
   function Get_Large_Inequality_Chart_Page(
      outputs        : Outputs_Rec; 
      breakdown      : Breakdown_Target;
      col            : String;
      lang           : Languages          := Languages'First ) return Unbounded_String is
   use Templates_Parser;
      chart : Unbounded_String := Create_Chart( 
                  outputs, 
                  breakdown, 
                  col, 
                  large, 
                  lang );
      translations : Translate_Set;
   begin
      Insert( translations, Assoc( "CHART", chart ));
      Insert( translations, Assoc( "TEMPLATE_ROOT", euws.template_components_path ));
      Insert( translations, Assoc( "LANG", Lang_Str( lang )));
      Insert( translations, Assoc( "ROOT", euws.Mefisto_Root ));
      Insert( translations, Assoc( "SEP", euws.Dir_Separator ));
      return TuS( Web_Utils.Parse_Template( 
            EU.Web.Settings.template_components_path & 
            EU.Web.Settings.dir_separator & "chart_popup", 
            translations ));      
   end Get_Large_Inequality_Chart_Page;
   
   function Create_One_Row( 
      title           : String;
      label           : String;
      outputs         : Outputs_Rec; 
      breakdown       : Breakdown_Target   := no_breakdown;
      col             : String;
      lang            : Languages := Languages'First) return Unbounded_String is
      tr              : Unbounded_String;
      ginis           : G_Array := Extract_Ginis( outputs, breakdown, col );
      thumbnail_chart : Unbounded_String := Create_Chart( outputs, breakdown, col, thumb, lang );
   begin
      tr := tr & "<tr class='tableRowEven'>" & LINE_BREAK;
      tr := tr & "<th valign='middle' align='right'>" & label & "</th>";
      tr := tr & "<td valign='middle' align='right'>" & Web_Format( ginis( 1 ), lang ) & "</td>";
      tr := tr & "<td valign='middle' align='right'>" & Web_Format( ginis( 2 ), lang ) & "</td>";
      tr := tr & "<td valign='middle' align='right'>" & Gl_Format( ginis( 3 ), False, lang ) & "</td>";
      tr := tr & "<td>" & Create_Chart_Popup_Link( breakdown, col, large, title, thumbnail_chart ) & "</td>";
      tr := tr & "</tr>" & LINE_BREAK;
      return tr;
   end Create_One_Row;
   
      
   function Get_Inequality_Chart_Grid(
      outputs      : Outputs_Rec; 
      lang         : Languages := Languages'First ) return Unbounded_String is
   use Templates_Parser;
      translations : Translate_Set;
      table        : Unbounded_String;
      title        : Unbounded_String;
      table_list   : Vector_Tag;
      link_list    : Vector_Tag;
      title_list   : Vector_Tag;
   begin
      for breakdown  in no_breakdown .. no_breakdown loop 
         table := Get_Gallery_Table_Header( lang );
         case breakdown is
            -- when by_tenure =>
               -- for tt in Tenure_Type loop
                  -- if( Natural( outputs.ineq_by_tenure( 1 )( tt ).lorenz.Length ) > 0 )then
                     -- title := TuS( "" ) & Lookup( "Income Distribution Effects", lang ) & "&nbsp;:&nbsp;"
                       -- & Lookup( Censor_String( Tenure_Type'Image( tt )), lang )
                       -- & " (" & Lookup( "Lorenz Curves", lang ) & ")";
                     -- table := table & Create_One_Row( 
                        -- TS( title ),
                        -- Lookup( Censor_String( Tenure_Type'Image( tt )), lang ),
                        -- outputs,
                        -- breakdown,
                        -- Tenure_Type'Image( tt ),
                        -- lang );
                  -- end if;
               -- end loop;
            -- when by_decile =>
               -- for dec in Deciles loop
                  -- if( Natural( outputs.ineq_by_decile( 1 )( dec ).lorenz.Length ) > 0 )then
                     -- title := TuS( "" ) & Lookup( "Income Distribution Effects", lang ) & "&nbsp;:&nbsp;"
                       -- & Lookup( Censor_String( Deciles'Image( dec )), lang )
                       -- & " (" & Lookup( "Lorenz Curves", lang ) & ")";
                     -- table := table & Create_One_Row( 
                        -- TS( title ),
                        -- Lookup( Censor_String( Deciles'Image( dec )), lang ),
                        -- outputs,
                        -- breakdown,
                        -- Deciles'Image( dec ),
                        -- lang );
                  -- end if;
               -- end loop;
            -- when by_age_of_head =>
               -- for tt in Adult_Age_Band loop
                  -- if( Natural( outputs.ineq_by_age_band( 1 )( tt ).lorenz.Length ) > 0 )then
                     -- title := TuS( "" ) & Lookup( "Income Distribution Effects", lang ) & "&nbsp;:&nbsp;"
                       -- & Lookup( Censor_String( Adult_Age_Band'Image( tt )), lang )
                       -- & " (" & Lookup( "Lorenz Curves", lang ) & ")";
                     -- table := table & Create_One_Row( 
                        -- TS( title ),
                        -- Lookup( Censor_String( Adult_Age_Band'Image( tt )), lang ),
                        -- outputs,
                        -- breakdown,
                        -- Adult_Age_Band'Image( tt ),
                        -- lang );
                  -- end if;
               -- end loop;
            -- when by_occupation_of_head =>               
               -- for tt in Occupation_Isco_1_Digit loop
                  -- if( Natural( outputs.ineq_by_occupation( 1 )( tt ).lorenz.Length ) > 0 )then
                     -- title := TuS( "" ) & Lookup( "Income Distribution Effects", lang ) & "&nbsp;:&nbsp;"
                       -- & Lookup( Censor_String( Occupation_Isco_1_Digit'Image( tt )), lang )
                       -- & " (" & Lookup( "Lorenz Curves", lang ) & ")";
                     -- table := table & Create_One_Row( 
                        -- TS( title ),
                        -- Lookup( Censor_String( Occupation_Isco_1_Digit'Image( tt )), lang ),
                        -- outputs,
                        -- breakdown,
                        -- Occupation_Isco_1_Digit'Image( tt ),
                        -- lang );
                  -- end if;
               -- end loop;
                 
            when no_breakdown =>
                  title := TuS( "" ) & Lookup( "Income Distribution Effects", lang ) & "&nbsp;"
                    & Lookup( "Whole Population", lang )
                    & "(" & Lookup( "Lorenz Curve", lang )
                    & "&nbsp;%)";
                  table := table & Create_One_Row( 
                        TS( title ),
                        Lookup( "Whole Population", lang ),
                        outputs,
                        breakdown,
                        Lookup( "Overall", lang ),
                        lang );
         end case;
         table := table & "              </tbody>" & LINE_BREAK;
         table := table & "      </table>" & LINE_BREAK;
         title_list := title_list & Lookup( Censor_String( Breakdown_Target'Image( breakdown )), lang );
         link_list := link_list & Censor_String( Breakdown_Target'Image( breakdown ));
         table_list := table_list & table;
      end loop;
      Insert( translations, Assoc( "PAGE-TITLE", Lookup( "Chart Gallery", lang )));
      Insert( translations, Assoc( "INTRO_TEXT",  Lookup( "gini_explanation", lang )));
      Insert( translations, Assoc( "TABLES", table_list ));
      Insert( translations, Assoc( "LINKS", link_list ));
      Insert( translations, Assoc( "TITLES", title_list ));
      Insert( translations, Assoc( "TEMPLATE_ROOT", euws.template_components_path ));
      Insert( translations, Assoc( "LANG", Lang_Str( lang )));
      Insert( translations, Assoc( "ROOT", euws.Mefisto_Root ));
      Insert( translations, Assoc( "SEP", euws.Dir_Separator ));
      Insert( translations, Assoc( "BEFORE", Lookup( "Before Reform", lang  )));
      Insert( translations, Assoc( "AFTER", Lookup( "After Reform", lang  )));
      Insert( translations, Assoc( "GINI", Lookup( "inequality_effects", lang  )));
      return TuS( Web_Utils.Parse_Template( 
         EU.Web.Settings.template_components_path & 
         EU.Web.Settings.dir_separator & "inequality_gallery", 
         translations ));      
   end Get_Inequality_Chart_Grid;
      
   function Get_Inequality_Table(
      outputs      : Outputs_Rec; 
      breakdown    : Breakdown_Target   := no_breakdown;
      lang         : Languages := Languages'First ) return Unbounded_String is
         ts : Unbounded_String;
   begin
      if( breakdown = no_breakdown )then
         ts := ts & "   <h3>" & Lookup( "Aggregate Gini Coefficient", lang ) & "</h3>" & LINE_BREAK;
      end if;
      ts := ts & "<table class='datatable' cellpadding='8' >" & LINE_BREAK;
      
      if( breakdown /= no_breakdown )then
         ts := ts & "   <caption>" & Lookup( "Gini Coefficients by ", lang ) & Lookup( censor_string( Breakdown_Target'Image( breakdown )), lang ) & "</caption>" & LINE_BREAK;
      end if;
      ts := ts & "   <thead>" & LINE_BREAK;
      ts := ts & "      <tr><th></th><th>" & Lookup( "Pre", lang ) & "</th><th>" & Lookup( "Post", lang ) & "</th></tr>" & LINE_BREAK;
      ts := ts & "   </thead>" & LINE_BREAK;

      ts := ts & "   <tbody>" & LINE_BREAK;
      case breakdown is
         when by_tenure =>
            for tt in Tenure_Type loop
               ts := ts & "      " & Be_Inequality_Web_IO.To_String( 
                  outputs.ineq_by_tenure( 1 )( tt ).inequality_measures, 
                  outputs.ineq_by_tenure( 2 )( tt ).inequality_measures, 
                  Censor_String( Tenure_Type'Image( tt )), 
                  lang );       
            end loop;
         when by_decile =>
            for tt in Deciles loop
               ts := ts & "      " & 
                  Be_Inequality_Web_IO.To_String( 
                     outputs.ineq_by_decile( 1 )( tt ).inequality_measures, 
                     outputs.ineq_by_decile( 2 )( tt ).inequality_measures, 
                     Censor_String( Deciles'Image( tt )), 
                  lang );       
            end loop;
         when by_age_of_head =>
            for tt in Adult_Age_Band loop
               ts := ts & "      " & 
                  Be_Inequality_Web_IO.To_String( 
                     outputs.ineq_by_age_band( 1 )( tt ).inequality_measures, 
                     outputs.ineq_by_age_band( 2 )( tt ).inequality_measures, 
                     Censor_String( Adult_Age_Band'Image( tt )), 
                  lang );       
            end loop;
         when by_occupation_of_head =>
            for tt in Occupation_Isco_1_Digit loop
               ts := ts & "      " & 
                  Be_Inequality_Web_IO.To_String( 
                     outputs.ineq_by_occupation( 1 )( tt ).inequality_measures, 
                     outputs.ineq_by_occupation( 2 )( tt ).inequality_measures, 
                     Censor_String( Occupation_Isco_1_Digit'Image( tt )), 
                  lang );       
            end loop;
         when no_breakdown =>
               ts := ts  & "      " & Be_Inequality_Web_IO.To_String( 
                  outputs.ineq( 1 ).inequality_measures, 
                  outputs.ineq( 2 ).inequality_measures, 
                  Lookup( "Overall", lang ), 
                  lang ); 
      end case;
      ts := ts & "   </tbody>" & LINE_BREAK;
      ts := ts & "</table>" & LINE_BREAK;
      return ts;
   end Get_Inequality_Table;
   

end  EU.BE.Output.Inequality_Web_IO;
