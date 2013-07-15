with Ada.Containers.Hashed_Sets;

with Costs_Tabulator.Web_IO;
with EU.Web.Settings;
with General_Chart_Constants;
with Inequality_Generator.Web_IO;
with Poverty_Tabulator.Web_IO;
with Poverty_Tabulator;
with Tabulator.Web_IO;
with Templates_Parser;
with Web_Utils;
with EU.BE.Main_Menu;

package body EU.BE.Output.Poverty_Web_IO is
   
   use Translations;
 
 
   package Poverty_By_Tenure_Web_IO is new 
      Poverty_By_Tenure_Package.Web_IO;
   package Poverty_By_Decile_Web_IO is new 
      Poverty_By_Decile_Package.Web_IO;
   package Poverty_By_Age_Band_Web_IO is new 
      Poverty_By_Age_Band_Package.Web_IO;
   package Poverty_By_Occupation_Web_IO is new 
      Poverty_By_Occupation_Package.Web_IO;
   
   package euws renames EU.Web.Settings; 

   function Get_Summary(
      outputs       : Outputs_Rec; 
      lang          : Languages          := Languages'First ) return Unbounded_String is
   use Templates_Parser;
   use Tabulator_Commons;
      diff              : Amount;
      row_str           : Unbounded_String;
      translations      : Translate_Set;
      sum_lines         : Vector_Tag;
      change_in_poverty : constant Amount :=
         outputs.poverty_by_decile(2).totals( people_in_poverty ) - 
         outputs.poverty_by_decile(1).totals( people_in_poverty );
      icp : constant Integer := Integer( change_in_poverty );
      iv1, iv2, idiff : Integer;
   begin
      Insert( translations, Assoc( "MORE", Lookup( "more", lang ))); 
      Insert( translations, Assoc( "TITLE", Lookup( "Poverty", lang )));
      Insert( translations, Assoc( "PRE", Lookup( "Before", lang )));
      Insert( translations, Assoc( "POST",  Lookup( "After", lang )));
      Insert( translations, Assoc( "DIFFERENCE", Lookup( "Change", lang )));
      Insert( translations, Assoc( "CHANGE-IN-POVERTY", GL_Format( icp, False, lang )));
      if icp > 1 then
         Insert( translations, Assoc( "POVERTY-ARROW", "up-bad" ));
      elsif icp < -1 then
         Insert( translations, Assoc( "POVERTY-ARROW", "down-good" ));  
      else
         Insert( translations, Assoc( "POVERTY-ARROW", "nc" ));
      end if;
      Insert( translations, Assoc( "GOOD-BAD-CLASS", GL_Class( Amount( icp ), False )));
      Insert( translations, Assoc( "CHANGE-IN-POVERTY-STR", Lookup( "Change in Numbers of People in Poverty", lang )));
      for p in people_in_poverty .. poverty_gap_per_poor_person loop
        diff := outputs.poverty_by_decile( 2 ).totals( p ) -
                outputs.poverty_by_decile( 1 ).totals( p );
        row_str := TuS( "<tr>" ) & 
        "<th>" & Lookup( Censor_String( Poverty_Elements'Image( p )), lang ) & "</th>";
        if( p = poverty_rate ) or ( p = poverty_gap_per_poor_person )then -- changes in rates formatted as 2dp floats
           row_str := row_str &
            "<td>" & Web_Format( outputs.poverty_by_decile( 1 ).totals( p ), lang ) & "</td>" &
            "<td>" & Web_Format( outputs.poverty_by_decile( 2 ).totals( p ), lang ) & "</td>" &
            "<td>" & GL_Format( diff, False, lang ) & "</td></tr>";
        else -- everything else as integers
           if( p = poverty_gap_total )then
              iv1 := Integer( outputs.poverty_by_decile( 1 ).totals( p )/1_000_000.0);
              iv2 := Integer( outputs.poverty_by_decile( 2 ).totals( p )/1_000_000.0);
              idiff := iv2 - iv1;
           else
              iv1 := Integer( outputs.poverty_by_decile( 1 ).totals( p ));
              iv2 := Integer( outputs.poverty_by_decile( 2 ).totals( p ));
              idiff := Integer( diff );
           end if;
           row_str := row_str &
            "<td>" & Web_Format( iv1, lang ) & "</td>" &
            "<td>" & Web_Format( iv2, lang ) & "</td>" &
            "<td>" & GL_Format( idiff, False, lang ) & "</td></tr>";
        end if;
        sum_lines := sum_lines & row_str; 
         
      end loop;
      Insert( translations, Assoc( "BUD-ROWS", sum_lines ));
      return Web_Utils.Parse_Template( TuS( euws.template_components_path ) & 
         euws.Dir_Separator & "poverty_summary", translations );      
   end Get_Summary;
   
   function Get_Gallery_Table_Header( lang : Languages ) return Unbounded_String is
      t : Unbounded_String;
      title      : Unbounded_String;
   begin
      t := t & "            <table class='summary_table'>" & LINE_BREAK;
      t := t & "              <thead>" & LINE_BREAK;
      t := t & "                <tr>" & LINE_BREAK;
      for col in people_in_poverty .. poverty_gap_per_poor_person loop
         title := TuS( "" ) & Lookup( Censor_String( Poverty_Elements'Image( col )), lang );

         t := t & "                  <td align='center' valign='middle'>" & LINE_BREAK;
         t := t & Lookup( Censor_String( Poverty_Elements'Image( col )), lang );
         t := t & "                  </td>" & LINE_BREAK;
      end loop;
      t := t & "                </tr>" & LINE_BREAK;
      t := t & "              </thead>" & LINE_BREAK;
      return t;
   end Get_Gallery_Table_Header;
   
   function Create_Chart_Popup_Link(
      breakdown     : Breakdown_Target ;
      col           : Poverty_Elements;
      size          : Chart_Size;
      title         : String;
      thumbnail_url : Unbounded_String ) return Unbounded_String is
      u : Unbounded_String;
   begin
      u := u & "<a class='picture_thumbnail' href='/mefisto/output_page/poverty_page/single_chart_popup?";
      u := u & "col=" & Censor_String( Poverty_Elements'Image( col ));
      u := u & "&amp;size=" & Censor_String( Chart_Size'Image( size ));
      u := u & "&amp;breakdown=" & Censor_String( Breakdown_Target'Image( breakdown ));
      u := u & "' ";
      u := u & "onclick=""ExampleWindow( this.href, '" & title & "', '" & title & "', '' ); return false;"" ";
      u := u & "onfocus='this.blur()'>";
      u := u & "<img src='" & thumbnail_url & "' alt='" & title & "' /></a>"; 
      return u;
   end Create_Chart_Popup_Link;

   function Create_Chart(
      outputs       : Outputs_Rec; 
      breakdown     : Breakdown_Target   := no_breakdown;
      col           : Poverty_Elements;
      size          : Chart_Size;
      lang          : Languages          := Languages'First ) return Unbounded_String is
         title      : Unbounded_String;
   begin
      title := TuS( "" ) & Lookup( Censor_String( Poverty_Elements'Image( col )), lang );
      case breakdown is
         when by_tenure =>
            return Poverty_By_Tenure_Web_IO.Make_Chart( 
               pre_tab       => outputs.poverty_by_tenure( 1 ), 
               post_tab      => outputs.poverty_by_tenure( 2 ), 
               title         => TS( title ),
               subtitle      => "",
               chart_type    => General_Chart_Constants.bar,
               
               x_axis_title  => Lookup( "Tenure", lang ),
               y_axis_title  => Lookup( col'Img, lang ),
               
               col           => col,
               chart_size    => size,
               lang          => lang );
         when by_decile =>
            return Poverty_By_Decile_Web_IO.Make_Chart( 
               pre_tab       => outputs.poverty_by_Decile( 1 ), 
               post_tab      => outputs.poverty_by_Decile( 2 ), 
               title         => TS( title ),
               subtitle      => "",
               chart_type    => General_Chart_Constants.bar,
               x_axis_title  => Lookup( "Decile", lang ),
               y_axis_title  => Lookup( col'Img, lang ),
               col           => col,
               chart_size    => size,
               lang=>lang );
         when by_age_of_head =>
            return Poverty_By_Age_Band_Web_IO.Make_Chart( 
               pre_tab       => outputs.poverty_by_age_band( 1 ), 
               post_tab      => outputs.poverty_by_age_band( 2 ), 
               title         => TS( title ),
               subtitle      => "",
               chart_type    => General_Chart_Constants.bar,
               col           => col,
               x_axis_title  => Lookup( "Age Band", lang ),
               y_axis_title  => Lookup( col'Img, lang ),
               chart_size    => size,
               lang=>lang );
         when by_occupation_of_head =>
            return Poverty_By_Occupation_Web_IO.Make_Chart( 
               pre_tab       => outputs.poverty_by_occupation( 1 ), 
               post_tab      => outputs.poverty_by_occupation( 2 ), 
               title         => TS( title ),
               subtitle      => "",
               chart_type    => General_Chart_Constants.bar,
               col           => col,
               x_axis_title  => Lookup( "Occupation", lang ),
               y_axis_title  => Lookup( col'Img, lang ),
               chart_size    => size,
               lang=>lang );
         when no_breakdown => return TuS( "" );
      end case;
   end Create_Chart;
   
  function Get_Large_Poverty_Chart_Page(
      outputs        : Outputs_Rec; 
      breakdown      : Disaggregated_Breakdown_Target   := by_decile;
      col            : Poverty_Elements;
      lang           : Languages          := Languages'First ) return Unbounded_String is
   use Templates_Parser;
         chart : Unbounded_String := Create_Chart( 
            outputs, breakdown, col, large, lang );
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
   end Get_Large_Poverty_Chart_Page;
   
   function Get_Poverty_Chart_Popup_Link(
      outputs       : Outputs_Rec; 
      breakdown     : Breakdown_Target;
      lang          : Languages := Languages'First ) return Unbounded_String is
   use Templates_Parser;
      table     : Unbounded_String;
      thumbnail : Unbounded_String;
      title     : Unbounded_String := TuS( Lookup( Censor_String( Breakdown_Target'Image( breakdown )), lang ));
      initial   : constant String := Censor_String( Breakdown_Target'Image( breakdown ));
   begin
      table := Get_Gallery_Table_Header( lang );
      table := table & "              <tbody>" & LINE_BREAK;
      table := table & "                 <tr class='tableRowEven'>" & LINE_BREAK;
      for col in people_in_poverty .. poverty_gap_per_poor_person loop
         table := table & "                 <td>" & LINE_BREAK;
         table := table & "                     <a class='picture_thumbnail' href='/mefisto/output_page/poverty_page/gallery_popup/#" & initial & "' ";
         table := table & " onclick=""GalleryWindow( this.href, '" & title & "', '800', '200' ); return false;"" onfocus='this.blur()'>";
         thumbnail := Create_Chart( 
               outputs, 
               breakdown, 
               col,
               thumb,
               lang );
         table := table & "<img src='" & thumbnail & "' /></a>" & LINE_BREAK;
         table := table & "                 </td>" & LINE_BREAK;
      end loop;     
      table := table & "                 </tr>" & LINE_BREAK;
      table := table & "              </tbody>" & LINE_BREAK;
      table := table & "          </table>" & LINE_BREAK;
      
      return table;
   end Get_Poverty_Chart_Popup_Link;

   
   function Get_Poverty_Chart_Grid(
      outputs       : Outputs_Rec; 
      lang          : Languages          := Languages'First ) return Unbounded_String is
   use Templates_Parser;
      translations : Translate_Set;
      title      : Unbounded_String;
      table      : Unbounded_String;
      thumbnail  : Unbounded_String;
      table_list : Vector_Tag;
      link_list  : Vector_Tag;
      title_list : Vector_Tag;
   begin
      breakdowns:
      for breakdown in by_decile .. Breakdown_Target'Last loop
         link_list := link_list & Censor_String( Breakdown_Target'Image( breakdown ));
         title_list := title_list & Lookup( Censor_String( Breakdown_Target'Image( breakdown )), lang );
         table := Get_Gallery_Table_Header( lang );
         table := table & "              <tbody>" & LINE_BREAK;
         table := table & "                 <tr class='tableRowEven'>" & LINE_BREAK;
         cols:
         for col in people_in_poverty .. poverty_gap_per_poor_person loop
            title := TuS( "" ) & Lookup( Censor_String( Poverty_Elements'Image( col )), lang ) & " ";
            thumbnail := Create_Chart( 
                  outputs, 
                  breakdown, 
                  col,
                  thumb,
                  lang );
            table := table & "                    <td>";
            table := table & Create_Chart_Popup_Link(
               breakdown, 
               col,
               large,
               TS( title ),
               thumbnail );
            table := table & "</td>" & LINE_BREAK;
         end loop cols;
         table := table & "                 </tr>" & LINE_BREAK;
         table := table & "              </tbody>" & LINE_BREAK;
         table := table & "          </table>" & LINE_BREAK;
         table_list := table_list & table;
      end loop breakdowns;
      Insert( translations, Assoc( "PAGE-TITLE", Lookup( "Chart Gallery", lang )));
      Insert( translations, Assoc( "INTRO_TEXT", Lookup( "household_poverty_explanation", lang )));
      Insert( translations, Assoc( "TABLES", table_list ));
      Insert( translations, Assoc( "LINKS", link_list ));
      Insert( translations, Assoc( "TITLES", title_list ));
      Insert( translations, Assoc( "TEMPLATE_ROOT", euws.template_components_path ));
      Insert( translations, Assoc( "LANG", Lang_Str( lang )));
      Insert( translations, Assoc( "ROOT", euws.Mefisto_Root ));
      Insert( translations, Assoc( "SEP", euws.Dir_Separator ));
      Insert( translations, Assoc( "BEFORE", Lookup( "Before Reform", lang  )));
      Insert( translations, Assoc( "AFTER", Lookup( "After Reform", lang  )));
      Insert( translations, Assoc( "IS-EXAMPLE-PAGE", True ));
      return TuS( Web_Utils.Parse_Template( 
            EU.Web.Settings.template_components_path & 
            EU.Web.Settings.dir_separator & "output_gallery_popup", 
            translations ));      
   end Get_Poverty_Chart_Grid;
   
   function Get_Poverty_Table( 
      outputs        : Outputs_Rec; 
      sysno          : System_Number    := 1;
      breakdown      : Disaggregated_Breakdown_Target := by_decile;
      do_differences : Boolean          := False;
      lang           : Languages        := Languages'First ) return Unbounded_String is
    begin
      case breakdown is
         when by_tenure =>
            declare
               tab     : Poverty_By_Tenure_Package.Table_Type;
               povline : Amount;
            begin
               if( do_differences )then
                  tab := Poverty_By_Tenure_Package.Difference( 
                     outputs.poverty_by_tenure( 2 ), 
                     outputs.poverty_by_tenure( 1 ));
                     povline := outputs.poverty_line_per_person( 2 ) - outputs.poverty_line_per_person( 1 );
               else
                  tab := outputs.poverty_by_tenure( sysno );
                  povline := outputs.poverty_line_per_person( sysno );
               end if;
               return Poverty_By_Tenure_Web_IO.To_String( 
                  tab,
                  povline,
                  Lookup( "By Tenure Type", lang ),
                  Lookup( "Poverty broken down by tenure type (money amounts in &euro; per month)",lang ),
                  lang );
            end;
         when by_decile =>
            declare
               tab     : Poverty_By_Decile_Package.Table_Type;
               povline : Amount;
            begin
               if( do_differences )then
                  tab := Poverty_By_Decile_Package.Difference( 
                     outputs.poverty_by_Decile( 2 ), 
                     outputs.poverty_by_Decile( 1 ));
                     povline := outputs.poverty_line_per_person( 2 ) - outputs.poverty_line_per_person( 1 );
               else
                  tab := outputs.poverty_by_Decile( sysno );
                  povline := outputs.poverty_line_per_person( sysno );
               end if;
               return Poverty_By_Decile_Web_IO.To_String( 
                  tab,
                  povline,
                  Lookup( "By Decile", lang ),
                  Lookup( "Poverty broken down by Decile (money amounts in &euro; per month)",lang ),
                  lang );
            end;
         when by_age_of_head =>
            declare
               tab     : Poverty_By_Age_Band_Package.Table_Type;
               povline : Amount;
            begin
               if( do_differences )then
                  tab := Poverty_By_Age_Band_Package.Difference( 
                     outputs.poverty_by_age_band( 2 ), 
                     outputs.poverty_by_age_band( 1 ));
                     povline := outputs.poverty_line_per_person( 2 ) - outputs.poverty_line_per_person( 1 );
               else
                  tab := outputs.poverty_by_age_band( sysno );
                  povline := outputs.poverty_line_per_person( sysno );
               end if;
               return Poverty_By_Age_Band_Web_IO.To_String( 
                  tab,
                  povline,
                  Lookup( "By age of the head of the household", lang ),
                  Lookup( "Poverty broken down by age of head of household (money amounts in &euro; per month)",lang ),
                  lang );
            end;
         when by_occupation_of_head =>
            declare
               tab     : Poverty_By_Occupation_Package.Table_Type;
               povline : Amount;
            begin
               if( do_differences )then
                  tab := Poverty_By_Occupation_Package.Difference( 
                     outputs.poverty_by_occupation( 2 ), 
                     outputs.poverty_by_occupation( 1 ));
                     povline := outputs.poverty_line_per_person( 2 ) - outputs.poverty_line_per_person( 1 );
               else
                  tab := outputs.poverty_by_occupation( sysno );
                  povline := outputs.poverty_line_per_person( sysno );
               end if;
               return Poverty_By_Occupation_Web_IO.To_String( 
                  tab,
                  povline,
                  Lookup( "By occupation", lang ),
                  Lookup( "Poverty broken down by occupation of the head of the household(money amounts in &euro; per month)",lang ),
                  lang );
            end;
        end case;
    end Get_Poverty_Table;   

end  EU.BE.Output.Poverty_Web_IO;
