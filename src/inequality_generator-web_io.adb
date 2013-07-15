--
-- copyright(c) 2009 Graham Stark/ Virtual Worlds (graham.stark@virtual-worlds.biz)
--
-- ////////////////////////////////
--
-- This is free software; you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation; either version 3, or (at your option)
-- any later version.
-- 
-- It is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
-- 
-- You should have received a copy of the GNU General Public License
-- along with this software; see the file docs/gpl_v3.  If not, write to
-- the Free Software Foundation, Inc., 51 Franklin Street,
-- Boston, MA 02110-1301, USA.
-- 
-- /////////////////////////////
pragma License( Modified_GPL );
--
-- FIXME : Generic on Language somehow.
--
with IO_Commons;
with Text_Utils;
with BE_Base_Model_Types;
with AWS.URL;
with Standard_Colours;
with Colours;
with Text_IO;

package body Inequality_Generator.Web_IO is
   use Text_IO;
   use IO_Commons;
   use Text_Utils;
   use Translations;
   use BE_Base_Model_Types;
   use Standard_Colours;
   use Colours;
   
   function To_String( 
      ql        : Quantile_List;
      total     : Amount;
      is_income : Boolean ) return Unbounded_String is
      s : Unbounded_String;
      q      : Quantile;
      pl     : Natural := Natural( ql.Length );
      m      : Amount;
   begin
      for i in 1 .. pl loop
         q := ql.Element( i );
         if( is_income )then
            m := q.income/total;
         else
            m :=q.population/total;
         end if;
         if( i = 1 ) and then ( m /= 0.0 )then
            s := s & "0.0,"; -- force an initial zero if there isn't one in the data
         end if;
         if( m /= 1.0 )then
            s := s & Amount'Image( m )( 2 .. Amount'Image( m )'Length );
         else
            s := s & "1.0"; -- java charts barf on Ada's representation of 1.0  as "1.00000E 00"
         end if;
         if( i < pl )then
            s := s & ",";
         end if;
      end loop;
      return s;
   end To_String;
   
   function Make_Standard_Chart(
      plotter_url   : String;
      pre           : Quantile_List;
      post          : Quantile_List;      
      title         : String;
      size          : Chart_Size;
      lang          : Languages ) return Unbounded_String is

      pl          : constant Natural := Natural( pre.Length );
      LAST_PRE_Q  : constant Quantile := pre.Element( pl );
      LAST_POST_Q : constant Quantile := post.Element( pl );
      TYPE_STR    : constant String := "lorenz";
      SIZE_STR    : constant String := Censor_String( Chart_Size'Image( size ));
      STYLE_STR   : constant String := "normal";
      SYS_STR     : constant String := "both";
      PRE_COL     : constant String := To_String( STD_COLOURS( vw_red ));
      POST_COL    : constant String := To_String( STD_COLOURS( vw_blue ));
      url : Unbounded_String := TuS( plotter_url & "?" ) & 
          "type=" & TYPE_STR & 
          "&amp;size=" & SIZE_STR & 
          "&amp;style=" & STYLE_STR & 
          "&amp;pre_or_post=" & SYS_STR & 
          "&amp;num_series=2";
    begin
      
      url := url & "&amp;da_pre_1=" & To_String( pre, LAST_PRE_Q.population, False ); 
      url := url & "&amp;col_pre_1=" & PRE_COL;       
      url := url & "&amp;da_pre_2=" & To_String( pre, LAST_PRE_Q.income, True ); 
      url := url & "&amp;col_pre_2=" & PRE_COL;       
                                                 
      url := url & "&amp;da_post_1=" & To_String( post, LAST_POST_Q.population, False ); 
      url := url & "&amp;col_post_1=" & POST_COL;     
      url := url & "&amp;da_post_2=" & To_String( post, LAST_POST_Q.income, True ); 
      url := url & "&amp;col_post_2=" & POST_COL;
      if( size /= thumb )then
         declare
            titles : constant String :=  AWS.URL.Encode( Lookup( "Income Distribution Effects", lang ) 
                 & " " 
                 & Lookup( title, lang )
                 & "(" & Lookup( "Lorenz Curve", lang )
                 & ")" );
            xtitle : constant String :=  AWS.URL.Encode( Lookup( "Population Share", lang ));
            ytitle : constant String :=  AWS.URL.Encode( Lookup( "Income Share", lang ));
         begin
            url := url & "&amp;title=" & AWS.URL.Encode( titles );
            url := url & "&amp;xlabel=" & AWS.URL.Encode( xtitle );
            url := url & "&amp;ylabel=" & AWS.URL.Encode( ytitle );
         end;
      end if;
      return url;   
   end Make_Standard_Chart;
      
   function Make_Chart(
      pre           : Quantile_List;
      pre_measures  : Inequality_Array;
      post          : Quantile_List;      
      post_measures : Inequality_Array;      
      title         : String;
      is_thumbnail  : Boolean;
      lang          : Languages ) return Unbounded_String is

      url : Unbounded_String := TuS( "http://chart.apis.google.com/chart?cht=lxy&amp;" );
      series : Unbounded_String := TuS( "" );
      q      : Quantile;
      pl     : Natural := Natural( pre.Length );
   
   function Fmt( t : Amount ) return String is
      type Little_D is delta 0.0001 range -10.0 .. 10.0; 
      s : String := Little_D'Image( Little_D( t ));
   begin
      return s( 2 .. s'Length );
   end Fmt;
   
         
   begin
      if( pl = 0 )then
         return TuS( "" );
      end if;
      if( not is_thumbnail )then
         url := url & "chs=520x370&amp;";
      else
         url := url & "chs=75x75&amp;";
      end if;
      url := url & "chd=t:0,1|0,1|"; -- diagonals
      -- series 1
      series := TuS( "0," );
      for i in 1 .. pl loop
         q := pre.Element( i );
         series := series & Fmt( q.population/Amount(pre_measures( Total_Population )));
         if( i < pl )then
            series := series & ",";
         end if;
      end loop;
      series := series & "|0,";
      for i in 1 .. pl loop
         q := pre.Element( i );
         series := series &  Fmt( q.income/Amount( pre_measures( Total_Income )));
         if( i < pl )then
            series := series & ",";
         end if;
      end loop;
      series := series & "|0,";
      for i in 1 .. pl loop
         q := post.Element( i );
         series := series & Fmt( q.population/Amount( post_measures( Total_Population )));
         if( i < pl )then
            series := series & ",";
         end if;
      end loop;
      series := series & "|0,";
      for i in 1 .. pl loop
         q := post.Element( i );
         series := series & Fmt( q.income/Amount( post_measures( Total_Income ))); -- GOT to be a better way
         if( i < pl )then
            series := series & ",";
         end if;
      end loop;
      url := url & series & "&amp;chco=bbbbbb,FF2222,20629e&amp;chds=0,1";
      if( not is_thumbnail )then
         declare
            titles : constant String :=  AWS.URL.Encode( Lookup( "Income Distribution Effects", lang ) 
                 & " " 
                 & Lookup( title, lang )
                 & "(" & Lookup( "Lorenz Curve", lang )
                 & ")" );
            xtitle : constant String :=  AWS.URL.Encode( Lookup( "Population Share", lang ));
            ytitle : constant String :=  AWS.URL.Encode( Lookup( "Income Share", lang ));
         begin
            url := url & "&amp;chtt=" & titles;
            url := url & "&amp;chts=224422,10&amp;";
            url := url & "chxl=1:||" & xtitle & "|3:||" & ytitle;
            url := url & "&amp;chxt=x,x,y,y"; 
         end;
      end if;
      return url;   
    end Make_Chart;

    function To_String(
       pre_lorenz   : Quantile_List;
       pre_ineq     : Inequality_Array;
       post_lorenz  : Quantile_List;  
       post_ineq    : Inequality_Array;
       label         : String;
       lang          : Languages ) return Unbounded_String is
          
      function Fmt( a : Amount ) return String is
      begin
         return Web_Format( BE_Base_Model_Types.Amount( a ), lang );
      end Fmt;          
          
      s : Unbounded_String;
      pl     : Natural := Natural( pre_lorenz.Length );
      pre_q : Quantile;
      post_q : Quantile;
    begin
       s := s & "<table class='datatable'>" & LINE_BREAK;
       s := s & "<caption>" & Lookup( label, lang ) & "<caption>" & LINE_BREAK;
       s := s & "<thead><tr><th rowspan='2'>" & Lookup( "Population", lang ) & "</th> ";
       s := s & "<th colspan='2'>" & Lookup( "Income", lang ) & "</th></tr>";
       s := s & "<tr><th>" & Lookup( "Pre", lang ) & "</th> ";
       s := s & "<th>" & Lookup( "Post", lang ) & "</th> ";
       s := s & "</tr></thead>" & LINE_BREAK;
       s := s & "<tbody>" & LINE_BREAK;
       for i in 1 .. pl loop
          pre_q := pre_lorenz.Element( i );
          post_q := post_lorenz.Element( i );
          if( ( i mod 2 ) = 0 )then
             s := s & "<tr class='tableRowOdd'>";
          else
             s := s & "<tr class='tableRowEven'>";
          end if;
          s := s & "<td>" & Fmt( pre_q.population ) & "</td>";
          s := s & "<td>" & Fmt( pre_q.income ) & "</td>";
          s := s & "<td>" & Fmt( post_q.income ) & "</td>";
          s := s & "</tr>" & LINE_BREAK;    
       end loop;
       s := s & To_String( pre_ineq, post_ineq, "Gini Coefficients", lang ) & LINE_BREAK;
       s := s & "</tbody>" & LINE_BREAK;
       s := s & "</table>" & LINE_BREAK;
       return s;
    end To_String;
     
    
    
       -- 
       -- 
       -- href='http://chart.apis.google.com/chart?cht=lxy&amp;chs=370x370&amp;chd=t:0.0000,1.000|0.0000,1.000|0.0000,0.0507,0.1044,0.1509,0.2002,0.2514,0.3019,0.3572,0.4012,0.4538,0.5011,0.5503,0.6093,0.6502,0.7013,0.7507,0.8006,0.8534,0.9067,1.000|0.0001,0.0108,0.0237,0.0357,0.0576,0.0694,0.0890,0.0976,0.1166,0.1550,0.1651,0.2128,0.2435,0.2653,0.3187,0.3772,0.4502,0.5380,0.6233,1.000|0.0000,0.0507,0.1044,0.1509,0.2002,0.2514,0.3019,0.3572,0.4012,0.4538,0.5011,0.5503,0.6093,0.6502,0.7013,0.7507,0.8006,0.8534,0.9067,1.000|0.0001,0.0108,0.0237,0.0357,0.0576,0.0694,0.0890,0.0976,0.1166,0.1550,0.1651,0.2128,0.2435,0.2653,0.3187,0.3772,0.4502,0.5380,0.6233,1.000&amp;chco=bbbbbb,FF2222,20629e&amp;chds=0.0000,1.000&amp;chtt=Income+Distribution+Effects+by+Dodoma+(Gini+coefficient,+%)&amp;chts=224422,10&amp;chxl=1:||Population Share|3:||Income Share&amp;chxt=x,x,y,y' title='Income Distribution Effects by Dodoma (Gini coefficient, %)'>
       -- 
       -- 
       -- <img src='http://chart.apis.google.com/chart?cht=lxy&amp;chs=75x75&amp;chd=t:0.0000,1.000|0.0000,1.000|0.0000,0.0507,0.1044,0.1509,0.2002,0.2514,0.3019,0.3572,0.4012,0.4538,0.5011,0.5503,0.6093,0.6502,0.7013,0.7507,0.8006,0.8534,0.9067,1.000|0.0001,0.0108,0.0237,0.0357,0.0576,0.0694,0.0890,0.0976,0.1166,0.1550,0.1651,0.2128,0.2435,0.2653,0.3187,0.3772,0.4502,0.5380,0.6233,1.000|0.0000,0.0507,0.1044,0.1509,0.2002,0.2514,0.3019,0.3572,0.4012,0.4538,0.5011,0.5503,0.6093,0.6502,0.7013,0.7507,0.8006,0.8534,0.9067,1.000|0.0001,0.0108,0.0237,0.0357,0.0576,0.0694,0.0890,0.0976,0.1166,0.1550,0.1651,0.2128,0.2435,0.2653,0.3187,0.3772,0.4502,0.5380,0.6233,1.000&amp;chco=bbbbbb,FF2222,20629e&amp;chds=0.0000,1.000' alt='Income Distribution Effects by Dodoma (Gini coefficient, %)' />    </a>
-- <div class='caption'>Income Distribution Effects by Dodoma (Gini coefficient, %)</div>
-- </li>
      

    
   function To_String(
         pre          : Inequality_Array;
         post         : Inequality_Array;      
         label        : String;
         lang         : Languages ) return Unbounded_String is
            
      function Fmt( a : Rate ) return String is
      begin
         return Web_Format( BE_Base_Model_Types.Amount( a ), lang );
      end Fmt;
      
      t : Unbounded_String;
    begin
      t := t & "<tr class='overallTotals'>" & 
         "<th>&nbsp;" & Lookup( label, lang ) & "</th>" & 
         "<td>&nbsp;" & Fmt( pre( gini ) * 100.0 )     & "</td>" & 
         "<td>&nbsp;" & Fmt( post( gini ) * 100.0 )    & "</td>" &
         "</tr>" & LINE_BREAK;
      return t;
   end  To_String;


end Inequality_Generator.Web_IO;
