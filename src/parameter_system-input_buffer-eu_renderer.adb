with AWS.Parameters;

with Ada.Characters.Handling;
with Ada.Exceptions;
with Ada.Strings.Fixed;
with Ada.Strings;
with Ada.Text_IO;

with EU.Web.Settings;
with EU_Logger;
with HTML_Utils;
with Line_Extractor;
with Strings_Edit.Float_Edit;
with Templates_Parser;
with Text_Utils;
with Web_Utils;


package body Parameter_System.Input_Buffer.EU_Renderer is

   use Text_Utils;
   use Ada.Text_IO;

   INDENT : constant Unbounded_String := TuS( "        " );
   
   
   HACK_ROOT : constant String := "/mefisto/";
   JAVA_IMAGES_PATH  : constant String := "/images/java_graphs/";
   INSERT_BEFORE_IMG : constant String := JAVA_IMAGES_PATH & "table/RowInsertBefore24.gif";
   INSERT_AFTER_IMG  : constant String := JAVA_IMAGES_PATH & "table/RowInsertAfter24.gif";
   DELETE_IMG        : constant String := JAVA_IMAGES_PATH & "table/RowDelete24.gif";
   SAVE_IMG          : constant String := JAVA_IMAGES_PATH & "general/Save24.gif";
   COPY_IMG          : constant String := JAVA_IMAGES_PATH & "general/Copy24.gif";
   PASTE_IMG         : constant String := JAVA_IMAGES_PATH & "general/Paste24.gif";
   HELP_IMG          : constant String := "/images/" & "help.png";
   
   html_type : HTML_Kind;
  
   package euws renames EU.Web.Settings; 

   package Local_HTML_Utils is new HTML_Utils(
      Rate=>Float_Type, 
      Counter_Type=>Counter_Type
   );

   procedure Log( s : String ) is
   begin
      EU_Logger.Log( EU_Logger.input_rendering, s );
   end Log;

   
   --
   -- Dmitry's formatting package
   --
   package Local_Float_Edit is new Strings_Edit.Float_Edit( Number => Float_Type );

   function Format_For_Input_Field( f : Float_Type; edit : Edit_Info_Rec ) return Unbounded_String is
      MAX_SAFE_FLOAT     : constant := 9999999.99;
      MAX_SAFE_FLOAT_USTR : constant Unbounded_String := TuS( "999999999.99" ); 
      -- note this is bigger than the corresponding number, since the back-end may
      -- be using a smaller precision than euromod and we need a bif number for upper limits.
      -- REALLY need to work out how to 
      -- fix this properly
      MIN_SAFE_FLOAT     : constant := -9999999.99;
      MIN_SAFE_FLOAT_USTR : constant Unbounded_String := TuS( "-999999999.99" );
      buff : String( 1 .. 30 );
      pos  : Integer := 1;
   begin
      Put_Line( "edit.length = " & edit.length'Img & " edit.prec " & edit.prec'Img );
      --
      -- FIXME horrible hack to ensure euromod doesn't get numbers formatted with
      -- exponents, which kills it
      -- 
      if( f >= MAX_SAFE_FLOAT )then 
         return MAX_SAFE_FLOAT_USTR;
      elsif( f <= MIN_SAFE_FLOAT )then
         return MIN_SAFE_FLOAT_USTR;
      end if;
      Local_Float_Edit.Put( 
         Destination => buff,
         Pointer     => pos,
         Value       => f,
         Field       => buff'Length,
         AbsSmall    => -(edit.prec ) );
      return TuS( Ada.Strings.Fixed.Trim( buff, Ada.Strings.Both ));
   end Format_For_Input_Field;
   
   function Format_For_Input_Field( i : Integer; edit : Edit_Info_Rec ) return Unbounded_String is
      s : String := Integer'Image( i );
   begin
      return TuS( s( 1 .. s'Length ));
   end Format_For_Input_Field;
   
   function Format_Field_For_Euromod( v_and_e : Value_And_Error; param : Parameter_Rec ) return Unbounded_String is
      rval : Float_Type;
      edit_info : Edit_Info_Rec := param.edit_info;
   begin
      case v_and_e.dtype is
         when real_type       =>
            rval := v_and_e.rval;
            if( param.logical_type = rate )then
               edit_info.prec := edit_info.prec + 2;
               rval := rval / 100.0; -- rates from percentages to levels
            end if;
            return Format_For_Input_Field( rval, edit_info );
         when integer_type    => return Format_For_Input_Field( v_and_e.ival, edit_info );
         when enumerated_type => return v_and_e.eval;
         when boolean_type    => return Format_For_Input_Field( Boolean'Pos( v_and_e.bval ),edit_info );
         when string_type     => return v_and_e.sval;
         when date_type       => return TuS( "NOT IMPLEMENTED YET" );
         when decimal_type    => return Format_For_Input_Field( Float_Type( v_and_e.dval ),  edit_info );
      end case;
   end Format_Field_For_Euromod;     
   
   --
   -- FIXME. this is HORRIBLE; better of iterating through the parameter systems than via this map
   --
   function Create_Euromod_Parameter_Mappings( buff : Buffer ) return Templates_Parser.Translate_Set is
      use Templates_Parser;
      use Utils;
      translations   : Translate_Set;
       
      
      procedure Add_To_Translations( c : Complete_Param_And_Value_Maps_Package.Cursor ) is
      use Complete_Param_And_Value_Maps_Package;
         cpvr  : Complete_Param_And_Value_Rec := Element( c );
         pos_in_reference : Positive;

         procedure Add_Map_Of_Arrays( c : Value_And_Error_Map_Package.Cursor ) is
         use Value_And_Error_Map_Package;
            vande : Value_And_Error_Vector := Element( c );
            key   : Unbounded_String;
            value : Unbounded_String; 
            param : Parameter_Rec;
            CURR_SIZE : constant Natural := Natural( vande.Length );
         begin
            param := cpvr.system_desc.parameters.Element( pos_in_reference );
            Put_Line( "reference_desc.maximum_size = " & cpvr.reference_desc.maximum_size'Img & "Size of this collection (cpvr.current_size):" & cpvr.current_size'Img );
            for i in 1 .. cpvr.reference_desc.maximum_size  loop
               if( i <= cpvr.current_size ) and then
                 ( vande.Element( i ).error = No_Error )then
                  value := Format_Field_For_Euromod( 
                     vande.Element( i ).all,
                     param );
               else
                  value := TuS( "999999999.999" );
               end if;
               key := TuS( param.Get_Extra( "param_name" ));
               Put_Line( "Key : " & TS( key ) & "value " & TS( value ) & " loop count " & i'Img );
               key := key & Integer'Image( i )( 2 .. Integer'Image( i )'Length );
               Insert( translations, Assoc( TS( key ), value ));
            end loop;
            pos_in_reference := pos_in_reference  + 1;
         end Add_Map_Of_Arrays;

         key   : Unbounded_String;
         value : Unbounded_String;
      begin
         Put_Line( "Add_To_Translations; on collection type " & cpvr.etype'Img );
         case cpvr.etype is
         when single        => 
            if( cpvr.val.error = No_Error )then
               key := TuS( cpvr.param_desc.Get_Extra( "param_name" ));
               Put_Line( "Add_To_Translations: parameter " & 
                  TS( cpvr.param_desc.instance_name ) & " got key as " & TS( key ));
               if( key /= Null_Unbounded_String )then
                  value := Format_Field_For_Euromod( cpvr.val.all, cpvr.param_desc );
                  Insert( translations, Assoc( TS( key ), value ));
               end if;
            end if;
         when single_array  => 
            for i in 1 .. Natural( cpvr.vallist.Length ) loop
               if( cpvr.vallist.Element( i ).error = No_Error )then
                  key := TuS( cpvr.param_desc.Get_Extra( "param_name" ));
                  if( key /= Null_Unbounded_String )then
                     value := Format_Field_For_Euromod( cpvr.vallist.Element( i ).all, cpvr.array_param_desc );
                     key := key & Integer'Image( i )( 2 .. Integer'Image( i )'Length );
                     Insert( translations, Assoc( TS( key ), value ));
                  end if;
               end if;
            end loop;
         when map_of_arrays => 
            pos_in_reference := 1;
            cpvr.valmap.Iterate( Add_Map_Of_Arrays'Access );
         end case;
      end Add_To_Translations;
      
   begin
      buff.params.Iterate( Add_To_Translations'Access ); 
      return translations;
   end Create_Euromod_Parameter_Mappings;


   procedure Make_Full_Input_Menus( 
      sys              : Parameter_System_Rec; 
      path             : Unbounded_String_List;
      menu_string      : in out Unbounded_String;
      depth            : Positive;
      is_first_element : in out Boolean;
      lang             : Languages;
      prefix           : Unbounded_String ) is
   use Parameter_System_Rec_Package;
   
      function Path_To_URL( l : Unbounded_String_List ) return Unbounded_String is
      -- local, simpler version of this
      use Unbounded_String_Vector_Package;
         s : Unbounded_String := prefix;
         c : Unbounded_String_Vector_Package.Cursor := First( l ); 
      begin
         loop
            s := s & Element( c ) & "/";
            exit when c = Last( l );
            Next( c );
         end loop;
         return s;
      end Path_To_URL;
      
      num_subsystems    : constant Natural := Natural( sys.parameter_systems.Length );
      num_parameters    : constant Natural := 
         Natural( sys.parameters.Length ) + Natural( sys.parameter_system_references.Length );
      
      path_so_far       : Unbounded_String_List := path;
      class             : Unbounded_String;
      url               : Unbounded_String;
      labelstr          : String := sys.Description( label, lang  );
      link              : Unbounded_String;
      openclosed        : Unbounded_String := Null_Unbounded_String;
      spacer            : Unbounded_String := depth * INDENT;
   begin
      
      if( depth > 1 )then -- skip "sys"
         if( not is_first_element )then
            if( num_subsystems > 0 )then
               openclosed := TuS( " class='closed' " );
            end if;
         else
            is_first_element := False;
         end if;
         path_so_far.Append( sys.instance_name );
         url := Path_To_URL( path_so_far );
         if( num_subsystems > 0 )then
            class := TuS( "folder" );
         else
            class := TuS( "file" );
         end if;
         
         Log( "on element " & TS( sys.instance_name ) & " got path as " & To_String( path_so_far ));
         
         if( num_parameters > 0 )then
            link := TuS( "<a href=""javascript:loadForm( '" ) & url & "' );"">" & labelstr & "</a>";
         else
            link := TuS( labelstr );
         end if;
         menu_string := menu_string & spacer & "<li "& openclosed & "><span class='" & class & "'>" & link & "</span>";
      end if;
      if( num_subsystems > 0 )then
         if( depth > 1 )then
            menu_string :=  menu_string & LINE_BREAK & spacer & indent & "<ul>" & LINE_BREAK;
         end if;
         for sno in 1 .. num_subsystems loop
            declare
               current_sys : Parameter_System_Rec := sys.parameter_systems.element( sno ).all;
            begin
               if( current_sys.instance_name /= Null_Unbounded_String )then
                  Make_Full_Input_Menus( 
                     current_sys, 
                     path_so_far, 
                     menu_string,
                     depth + 1,
                     is_first_element,
                     lang, 
                     prefix );
               end if;
            end;
         end loop;
         if( depth > 1 )then
            menu_string := spacer & indent & menu_string & "</ul>" & LINE_BREAK;
            menu_string := spacer & menu_string & "</li>" & LINE_BREAK;
         end if;
      else
         if( depth > 1 )then
            menu_string := menu_string & "</li>" & LINE_BREAK;
         end if;
      end if;
   end Make_Full_Input_Menus;

   
   procedure Make_Input_Menus( 
      sys            : Parameter_System_Rec; 
      path           : in out Unbounded_String_List;
      menu_string    : in out Unbounded_String;
      user           : EU.BE.Users.User_Type;
      lang           : Languages;
      depth          : in out Natural;
      prefix         : Unbounded_String ) is
    use Parameter_System_Rec_Package;
    use Unbounded_String_Vector_Package;
         num_subsystems : constant Natural := Natural( sys.parameter_systems.Length );
    begin
      if( num_subsystems > 0 )then
         declare
            list           : Unbounded_String;
            selected_sys   : Parameter_System_Rec;
            this_element_name : Unbounded_String := TuS( "XX" );
            all_but_last        : Unbounded_String_List;
            path_length    : constant Natural := Natural( path.Length );
         begin
            -- path is (say) income_tax | allowances
            --
            -- if the path stretches down this far, use this element 
            --
            --
            -- the elements in the path up to this point, if any
            --
            for i in 1 .. depth loop
               if( i <= path_length )then
                  all_but_last.Append( path.Element( i ));
               end if;
            end loop;
            depth := depth + 1;
            if( num_subsystems > 0 )then
               selected_sys :=  sys.parameter_systems.element( 1 ).all;  
               if( path_length >= depth ) and ( path_length > 0 )then
                  this_element_name := path.Element( depth );
               else
                  this_element_name := selected_sys.instance_name;
               end if;
               list := list & "<ul>" & LINE_BREAK;
               
               for sno in 1 .. num_subsystems loop
                  declare
                     current_sys : Parameter_System_Rec := sys.parameter_systems.element( sno ).all;
                     labelstr : String := current_sys.Description( label, lang  );
                     elem  : Unbounded_String;
                     url   : Unbounded_String;
                     path_to_left :  Unbounded_String_List;
                     full_path_to_left :  Unbounded_String_List;
                  begin
                     if( current_sys.instance_name = Null_Unbounded_String )then
                        null; -- no instance parameter systems, e.g. ratebands
                     elsif( current_sys.instance_name = this_element_name )then
                        selected_sys := current_sys;
                        elem := elem & "    <li class='on'>" & labelstr & "</li>" & LINE_BREAK;
                     else
                        Make_Path_To_Left( current_sys, path_to_left );
                        full_path_to_left.Append( all_but_last );
                        full_path_to_left.Append( path_to_left );
                        url := Path_To_URL( prefix, full_path_to_left );
                        elem := elem & "    <li><a " & url & ">" & labelstr & "</a></li>" & LINE_BREAK;
                     end if;
                     list := list & elem;
                  end;
               end loop;
               list := list & "</ul>" & LINE_BREAK;
               menu_string := menu_string & list;
            end if;
            if(( depth - 1 ) > path_length )then
               path.append( selected_sys.instance_name );   
            end if;
            Make_Input_Menus( selected_sys, path, menu_string, user, lang, depth, prefix ); 
         end;
       end if;
    end Make_Input_Menus;
   
   function Make_Parameter_Menu(
      base_sys            : Parameter_System_Rec; 
      path                : Unbounded_String_List;
      prefix              : Unbounded_String;
      lang                : Languages;
      user                : EU.BE.Users.User_Type ) return Unbounded_String is  
      menu : Unbounded_String;
      lists : Unbounded_String ;
      depth : Natural := 0;
      locpath : Unbounded_String_List;
      is_first_element : Boolean := True;
   begin
      Make_Full_Input_Menus( 
         base_sys, 
         locpath,
         lists,
         1,
         is_first_element,
         lang,
         prefix );
     menu := menu &
      TuS( "<div id='treenavigation' class='designreformblock treenavigation' style='width: 200px;'>" ) &
      LINE_BREAK & 
      TuS( "<ul id='browser' class='filetree' >" ) &
      LINE_BREAK & 
      lists & 
      LINE_BREAK &
      TuS( "</ul>" ) &
      LINE_BREAK & 
      TuS( "</div>" ) &
      LINE_BREAK; 
      
      -- Make_Input_Menus( base_sys, locpath, lists, user, lang, depth, prefix );
      -- menu := menu & "<div id='modelNavSub'>" & LINE_BREAK & lists & LINE_BREAK &  "</div>" & LINE_BREAK;
      return menu;
   end Make_Parameter_Menu;

   --
    
   function Contains_Value( table : AWS.Parameters.List; value : String ) return Boolean is
      table_size : Integer := AWS.Parameters.Count (table);
   begin
      for i in  1 .. table_size loop
          if (AWS.Parameters.Get( table, i ).Value = value) then            
            return True;
         end if;
      end loop;
      return False;
   end Contains_Value;
   
   

   function Reverse_Table_Lookup( table : AWS.Parameters.List; value : String ) return String is
      table_size : Integer := AWS.Parameters.Count( table );
   begin
      for i in  1 .. table_size loop
          if( AWS.Parameters.Get( table, i ).Value = value ) then
            return AWS.Parameters.Get( table, i ).Name;
         end if;
      end loop;
      return "";
   end Reverse_Table_Lookup;
  
   function Units_String( ltype : Logical_Kind; units : Units_Kind; lang : Languages ) return String is
   begin
      case ltype is
         when any_kind | quickies | rates_and_bands  => return "";
         when rate                     => return "&nbsp;<span class='units'>(%)</span>";
         when tax_allowance | tax_band => return "&nbsp;<span class='units'>(&euro; " & Lookup( "per year", lang ) & ")</span>"; 
         when benefit | poverty_line   => 
            if( units = day ) then
               return "&nbsp;<span class='units'>(&euro; " & Lookup( "per day", lang ) & ")</span>"; 
            else
               return "&nbsp;<span class='units'>(&euro; " & Lookup( "per month", lang ) & ")</span>";
            end if;
         when age_limit                => return "&nbsp;<span class='units'>(" & Lookup( "years", lang ) & ")</span>"; 
      end case;
   end Units_String;
   
   procedure Set_HTML_Type( t : HTML_Kind ) is
   begin
      html_type := t;
   end Set_HTML_Type;

   function Make_Radio
     (varname           : Unbounded_String;
      selected          : Unbounded_String;
      enum              : Enumerated_Type_Rec;
      lang              : Languages;
      class             : String := "";
      extras            : String := "";
      help              : String := "")
      return              Unbounded_String is
      use Enumerated_Element_Package;
      n : Positive := Positive( Length( enum.values ));
      s : Unbounded_String :=  Null_Unbounded_String;
   begin
      for i in 1 .. n loop
         s := s & "<input type='radio name='" & varname & "' id='" & varname & "'";
         if (class /= "") then
            s := s & " class='" & class & "' ";
         end if;
         if (extras /= "") then
            s := s & " " & extras;
         end if;
         if (help /= "") then
            s := s & " alt='" & help & "' ";
         end if;
         declare
            ev : Enum_Value_Rec := enum.values.Element( i );
         begin
            if( selected = ev.name ) or ( selected = Null_Unbounded_String and ev.is_default ) then
               s := s & " default = 'default' ";
            end if;
            s := s & " value='" & ev.name & "' />";
            if( ev.text( lang ) /= Null_Unbounded_String )then
               s := s & ev.text( lang );
            else
               s := s & ev.text( Languages'First );
            end if;
         end;
      end loop;
      return s;
   end Make_Radio;
 
   function Make_Checkbox
     (varname : Unbounded_String;
      checked : Boolean;
      class   : Unbounded_String := Null_Unbounded_String;
      help    : Unbounded_String := Null_Unbounded_String;
      extras  : Unbounded_String := Null_Unbounded_String )
      return    Unbounded_String
   is
      s : Unbounded_String := Null_Unbounded_String;
   begin
      s := s & "<input type='checkbox' name='" & varname & "' ";
      if (class /= Null_Unbounded_String) then
         s := s & "class='" & class & "' ";
      end if;
      if (extras /= Null_Unbounded_String) then
         s := s & extras;
      end if;
      if (help /= Null_Unbounded_String) then
         s := s & " alt='" & help & "' ";
      end if;
      if (checked) then
         s := s & " checked='checked' ";
      end if;
      s := s & " />";
      return s;
   end Make_Checkbox;
   
   function Make_Validator_Code( key : Unbounded_String; param : Parameter_Rec ) return Unbounded_String is
      s : Unbounded_String := """" & key & """: { required: true, ";   
   begin
      
      -- case logical_type is
         -- when any_kind, 
         -- when rate =>
         -- when tax_allowance =>
         -- when tax_band =>
         -- when benefit =>
         -- when age_limit =>
         -- when quickies =>
         -- when poverty_line =>
         -- when rates_and_bands =>   
      -- end case;
      if( param.edit_info.treat_as_percentage )then
            s := s & "min: 0,";
            s := s & "max: 100,";
      else
         if( param.edit_info.min /= Float_Type'First )then
            s := s & "min: " & Float_Type'Image( param.edit_info.min ) & ",";
         end if;
         if( param.edit_info.max /= Float_Type'Last )then
            s := s & "max: " & Float_Type'Image( param.edit_info.max ) & ",";
         end if;
      end if;
      s := s & "number: true }"; -- FIXME allow for Dutch style xxx xxx,00
      return s;
   end Make_Validator_Code;
   
   function Make_Info_Box( 
      key : Unbounded_String;
      text : Unbounded_String ) return Unbounded_String is
         s : Unbounded_String;
   begin
      s := s & "<div class='info' id='" & key & "_help' >" & LINE_BREAK;
      s := s & text & LINE_BREAK;
      s := s & "</div>" & LINE_BREAK;
      return s;
   end Make_Info_Box;
   
   function Make_Select
     (varname           : Unbounded_String;
      selected          : Unbounded_String;
      enum              : Enumerated_Type_Rec;
      lang              : Languages;
      class             : Unbounded_String := Null_Unbounded_String;
      extras            : Unbounded_String := Null_Unbounded_String;
      help              : Unbounded_String := Null_Unbounded_String)
      return              Unbounded_String
      
   is
      use Enumerated_Element_Package;
      n : Positive := Positive( Length( enum.values ));
      s : Unbounded_String := Null_Unbounded_String & 
         "<select" & " name='" & varname & "' id='" & varname & "'";
   begin
      if( class /= Null_Unbounded_String )then
         s := s & "class='" & class & "' ";
      end if;
      if (extras /= Null_Unbounded_String) then
         s := s & extras;
      end if;
      if (help /= Null_Unbounded_String) then
         s := s & " alt='" & help & "' ";
      end if;

      s := s & ">";
      
      for i in 1 .. n loop
         s := s & "<option ";
         declare
            ev : Enum_Value_Rec := enum.values.Element( i );
         begin
            if( selected = ev.name ) or ( selected = Null_Unbounded_String and ev.is_default ) then
               s := s & " selected = 'selected' ";
            end if;
            s := s &
              " value='" & ev.name & "'>";
            if( ev.text( lang ) /= Null_Unbounded_String )then
               s := s & ev.text( lang );
            else
               s := s & ev.text( Languages'First );
            end if;
            s := s & "</option>";
         end;
      end loop;
      s := s & "</select>";
      return s;
   end Make_Select;

   function Make_Input
     (varname    : Unbounded_String;
      value      : Unbounded_String;
      class      : Unbounded_String  := Null_Unbounded_String;
      size       : Integer           := 12;
      help       : Unbounded_String  := Null_Unbounded_String;
      error_text : Unbounded_String  := Null_Unbounded_String;
      extras     : Unbounded_String  := Null_Unbounded_String )
      return       Unbounded_String
   is
      s : Unbounded_String :=
         To_Unbounded_String
           ("<input type='text' size='" & 
             Censor_String(Integer'Image(size)) & 
            "' name='" ) & varname & "' id='" & varname & "' ";
   begin
      if (class /= Null_Unbounded_String) then
         s := s & "class='" & class & "' ";
      end if;
      if (extras /= Null_Unbounded_String) then
         s := s & extras;
      end if;
      if (help /= Null_Unbounded_String) then
         s := s & " alt='" & help & "' ";
      end if;
      s := s & " value='" & value & "' ";
      s := s & " />";
      if( error_text /= Null_Unbounded_String )then
         s := s & "<br/>";
         s := s & "<span class='input_error_message'>" & error_text & "</span>";
      end if;
      return s;
   end Make_Input;

   
   function Get_Display_Priority( param : Parameter_Rec ) return Float_Type is
      priority :  Float_Type := 1.0;
   begin
      if( param.logical_type = quickies )then
         priority := 100.0;
      else
         declare 
            pstring : String := param.Get_Extra( "priority" );
         begin
            if( pstring /= "" )then
               priority := Local_Format_Utils.Lenient_Convert( pstring );                       
            end if;
         end;
      end if;
      return priority;
   end Get_Display_Priority;
   
   function Get_Class( val_and_err  : Value_And_Error ) return Unbounded_String is
      use Utils;
      ERROR   : constant Unbounded_String := To_Unbounded_String( "input_error" );
      CHANGED : constant Unbounded_String := To_Unbounded_String( "is_changed_from_base" );
      
   begin
      if( val_and_err.error /= no_error )then
         return ERROR;
      else
         case val_and_err.dtype is
            when real_type       => if( val_and_err.rval /= val_and_err.rdefault ) then return CHANGED; end if;
            when integer_type    => if( val_and_err.ival /= val_and_err.idefault ) then return CHANGED; end if;
            when enumerated_type => if( val_and_err.eval /= val_and_err.edefault ) then return CHANGED; end if;
            when boolean_type    => if( val_and_err.bval /= val_and_err.bdefault ) then return CHANGED; end if;
            when string_type     => if( val_and_err.sval /= val_and_err.sdefault ) then return CHANGED; end if;
            when date_type       => null; -- TODO cval, cdefault  : Ada.Calendar.Time;
            when decimal_type    => if( val_and_err.dval /= val_and_err.ddefault ) then return CHANGED; end if;
         end case;
      end if;
      return Null_Unbounded_String;
   end Get_Class;

   function Make_Description_Table( desc : Parameter_Rec; lang : languages ) return Unbounded_String is
      s : Unbounded_String;
   begin
      s := s & "<h4>" & desc.Description( label, lang ) & "</h4>" & LINE_BREAK; 
      s := s & "<table width='100%'>" & LINE_BREAK;
      if( desc.Description( label, lang ) /=  desc.Description( description, lang ))then
         s := s & "   <tr><td colspan='2'>" & desc.Description( description, lang ) & "</td></tr>" & LINE_BREAK; 
      end if;
      if( desc.edit_info.min > Float_Type'First )then
         s := s & "   <tr><th>" & Lookup( "Minimum Value", lang ) & ":</th><td>" & Format( desc.edit_info.min, lang ) & "</td></tr>" & LINE_BREAK; 
      end if;
      if( desc.edit_info.max < Float_Type'Last )then
         s := s & "   <tr><th>" & Lookup( "Maximum Value", lang ) & ":</th><td>" & Format( desc.edit_info.max, lang ) & "</td></tr>" & LINE_BREAK; 
      end if;
      s := s & "   <tr><th>" & Lookup( "Units", lang ) & ":</th><td>" & Units_String( desc.logical_type, desc.units, lang ) & "</td></tr>" & LINE_BREAK; 
      
      s := s & "</table>" & LINE_BREAK;
      return s;   
   end Make_Description_Table; 
   
   function Write_Help_Box( 
      key  : Unbounded_String;
      elem : Parameter_Rec;
      lang : Languages ) return Unbounded_String is
   use Templates_Parser;
      translations       : Translate_Set;
      call : Unbounded_String;  
      text : String := " ";
      censored_key : Unbounded_String := Censor_String( Censor_Id( key ));
   begin
      if( text = "" )then
         return Null_Unbounded_String;
      end if;
      Insert( translations, Assoc( "KEY", censored_key ));
      Insert( translations, Assoc( "MAIN-HELP-TEXT", Make_Description_Table( elem, lang ) ));
      Insert( translations, Assoc( "ROOT", euws.Mefisto_Root ));
      Insert( translations, Assoc( "CHANGED_PARAMETERS", "CHANGED PARAMS GO HERE" ));
      
      Insert( translations, Assoc( "IMG", HELP_IMG ));
      return Web_Utils.Parse_Template( 
         TuS( euws.template_components_path ) &
         euws.Dir_Separator & "jquery_help", translations );
   end Write_Help_Box;
   
   function Create_Single_Input( 
      key              : Unbounded_String;
      lang             : Languages;
      param_desc       : Parameter_Rec;
      val_and_err      : Value_And_Error;
      enum_type        : Enumerated_Type_Rec;
      do_complete_row  : Boolean;
      make_hidden      : Boolean := False ) return Unbounded_String is
      use Ada.Exceptions; 
      use Utils;
      
      s : Unbounded_String;
      class : Unbounded_String := Get_Class( val_and_err );
      label_str   : String := param_desc.Description( label, lang );
      short_help  : Unbounded_String := To_Unbounded_String( param_desc.Description( journalese, lang ));
      full_help   : Unbounded_String := To_Unbounded_String( param_desc.Description( description, lang ));
      input_text  : Unbounded_String;
      extras      : Unbounded_String;
   begin
      Log( "key |" & TS( key ) & "| onchange = |" & TS( param_desc.Edit_Info.onchange ) & "| logical type |" & param_desc.logical_type'Img );
      Put_Line( "label_str = " & label_str );
      if( param_desc.edit_info.onchange /= TuS( "" ))then
         extras := " onchange='" & param_desc.Edit_Info.onchange & "' ";
      end if;
      if( not param_desc.edit_info.enabled )then
         extras := extras & " disabled='disabled' ";
      end if;
      
      if( do_complete_row )then
         s := s & 
            "<tr><td width='50%'><label for='" & key & "'>" & label_str &
            "</label></td><td>";  
      end if;
      if( make_hidden )then
         s := s & "<input type='hidden' name='"& key & "' id='" & key & "' value='";
         case val_and_err.dtype is
            when enumerated_type =>s := s & val_and_err.eval;
            when real_type    => s := s & val_and_err.rval'Img;
            when integer_type => s := s & val_and_err.ival'Img;
            when decimal_type => s := s & val_and_err.dval'Img; 
            when string_type  => s := s & val_and_err.sval;
            when boolean_type => s := s & val_and_err.bval'Img;
            when others =>
               Raise_Exception( Param_Exception'Identity, 
                      "Parameter-System.Input_Buffer : Create_Single_Input for: " &
                      Data_Kind'Image( val_and_err.dtype ) &
                      " is not inplemented key:" & To_String( key ));
         end case;
         s := s & "' />";
      elsif param_desc.edit_info.display = read_only then
         s := s & "<span class='static_value'>";
         case val_and_err.dtype is
            when enumerated_type =>s := s & val_and_err.eval;
            when real_type    => s := s & Format( val_and_err.rval, lang );
            when integer_type => s := s & Format( val_and_err.ival, lang );
            when decimal_type => s := s & Format( Float_Type( val_and_err.dval ), lang ); 
            when string_type  => s := s & val_and_err.sval;
            when boolean_type => s := s & Format( val_and_err.bval, lang );
            when others =>
               Raise_Exception( Param_Exception'Identity, 
                      "Parameter-System.Input_Buffer : Create_Single_Input for: " &
                      Data_Kind'Image( val_and_err.dtype ) &
                      " is not inplemented key:" & To_String( key ));
         end case;
         s := s & "&nbsp;</span>";
         -- FIXME passwords
      else
         case val_and_err.dtype is
            when enumerated_type =>
               s := s & 
                  Make_Select( 
                     varname    => key,
                     selected   => val_and_err.eval,
                     enum       => enum_type,
                     lang       => lang,
                     help       => short_help,
                     extras     => extras,
                     class      => class );
            when real_type | integer_type | decimal_type | string_type =>
               -- hack because we've used a format that may have commas or spaces
               -- as the text in val_and_err.text
               if( val_and_err.error = no_error )then
                  if( val_and_err.dtype = real_type )then
                     input_text := Format_For_Input_Field( val_and_err.rval, param_desc.edit_info );
                  elsif( val_and_err.dtype = integer_type )then
                     input_text := Format_For_Input_Field( val_and_err.ival, param_desc.edit_info );
                  else
                     input_text := val_and_err.text;
                  end if;
               else 
                  input_text := val_and_err.text;
               end if;  
               s := s & 
                  Make_Input(
                     varname    => key,
                     value      => input_text,
                     class      => class,
                     size       => param_desc.edit_info.length,
                     help       => short_help,
                     extras     => extras,
                     error_text => val_and_err.error_message );
               
           when boolean_type =>
                s := s & 
                  Make_Checkbox(
                     varname    => key,
                     checked    => val_and_err.bval,
                     class      => class,
                     extras     => extras,
                     help       => short_help );
            when others =>        
               Raise_Exception( Param_Exception'Identity, 
                      "Parameter-System.Input_Buffer : Create_Single_Input for: " &
                      Data_Kind'Image( val_and_err.dtype ) &
                      " is not inplemented key:" & To_String( key ));
         end case;
      end if;
      if( do_complete_row )then
         s := s & "</td>";
         s := s & "<td width='15%' >" & Units_String( param_desc.logical_type, param_desc.units, lang ) & "</td>";
         s := s & "<td>";
         s := s & Write_Help_Box( 
            key, param_desc, lang );
         s := s & "</td></tr>" & LINE_BREAK; 
      end if;
      return s;  
   end Create_Single_Input;
   
  function Make_Ajax_Call_Indexed( 
      action          : Ajax_Action_Type;
      key             : Unbounded_String;
      row             : Natural;
      lang            : Languages;
      ajax_target_key : Unbounded_String ) return Unbounded_String is
      ERROR_CHECK_JS : constant String := " new Ajax.Updater( 'error_section', '" & euws.Mefisto_Root & "ajax', { method: 'post', parameters: 'action=error_check'})";
   use Templates_Parser;
      translations       : Translate_Set;
   begin
      case action is
      when insert_above =>
         Insert( translations, Assoc( "IMG", HACK_ROOT& INSERT_BEFORE_IMG ));
         Insert( translations, Assoc( "ALT", Lookup( "ajax_insert_before",lang )));
      when insert_below => 
         Insert( translations, Assoc( "IMG", HACK_ROOT& INSERT_AFTER_IMG ));
         Insert( translations, Assoc( "ALT",  Lookup( "ajax_insert_after",lang )));
      when delete => 
         Insert( translations, Assoc( "IMG", HACK_ROOT& DELETE_IMG ));
         Insert( translations, Assoc( "ALT", Lookup( "ajax_delete",lang )));
      when save => 
         Insert( translations, Assoc( "IMG", HACK_ROOT& SAVE_IMG ));
         Insert( translations, Assoc( "ALT", Lookup( "ajax_save",lang )));
      when copy =>
         Insert( translations, Assoc( "IMG", HACK_ROOT& COPY_IMG ));
         Insert( translations, Assoc( "ALT", Lookup( "ajax_copy",lang )));
      when error_check  => null;
      end case;
      Insert( translations, Assoc( "TABLE-DATA-ID", key ));
      Insert( translations, Assoc( "KEY", key ));
      Insert( translations, Assoc( "ROW", row ));
      Insert( translations, Assoc( "ACTION", Censor_String( Ajax_Action_Type'Image( action ))));
      Insert( translations, Assoc( "URL", HACK_ROOT & "array_update" ));
      Insert( translations, Assoc( "TABLE-CONTAINER-ID", Censor_String( Censor_Id( ajax_target_key ))));
      Put_Line( "path for templates |" & euws.template_components_path & "/jquery_ajax" & ".thtml |" );
      return Web_Utils.Parse_Template( TuS( euws.template_components_path ) & 
         euws.Dir_Separator &
         "jquery_ajax", translations );
   end Make_Ajax_Call_Indexed;
   
 
   function Make_Indexed_Block(
      complete_sys      : Parameter_System_Rec;
      key               : Unbounded_String;
      buff              : Buffer;
      ajax_target_key   : Unbounded_String ) return Unbounded_String is
   use Ada.Exceptions; 
   use Text_Utils;
      s : Unbounded_String;
      INDENT : constant String := "  ";
      complete_value : Complete_Param_And_Value_Rec;
   begin
      Put_Line( "Looking for key |" & TS( key ) & "| " );
      complete_value := buff.params.Element( key );
      case complete_value.reference_desc.index_type is
         when none => 
            Raise_Exception( Param_Exception'Identity, 
              "Parameter-System.Input_Buffer : inconsistent state list: but no collection type key:" & To_String( key ));
         when integer_type =>
            declare
               use Value_And_Error_Map_Package;
               parameter  : Parameter_Rec;
               num_params : constant Natural := Natural( complete_value.system_desc.parameters.Length );  
               make_hidden : Boolean;
               print_edit  : Boolean;
            begin
               s := s & "<table class='editable_table' id='table-" & key & "'> " & LINE_BREAK;
               s := s & INDENT & "<tbody>" & LINE_BREAK;
               s := s & INDENT & INDENT & "<tr>" & LINE_BREAK;
               for pno in 1 .. num_params loop
                  parameter := complete_value.system_desc.parameters.Element( pno );
                  s := s & INDENT & INDENT & INDENT & "<th>" & parameter.Description( label, buff.lang ) & 
                           "<br/>" & Units_String( parameter.logical_type, parameter.units, buff.lang ) &
                           "</th>" & LINE_BREAK;
               end loop;   
               s := s & INDENT & INDENT & "</tr>" & LINE_BREAK;
               each_index:
               for row in 1 .. complete_value.current_size loop
                  s := s & INDENT & INDENT & "<tr id='" & key & "_row_" & Trim( Natural'Image( row )) & "' >" & LINE_BREAK;
                  each_param:
                  for pno in 1 .. num_params loop
                     declare
                        index_key     : Unbounded_String;
                        val_and_err   : Value_And_Error_Access;
                        cell          : Unbounded_String;
                        enum          : Enumerated_Type_Rec;
                     begin
                        parameter := complete_value.system_desc.parameters.Element( pno );
                        if( To_String( parameter.enum_type_ref ) /= "" )then
                           enum := Enum_Search.Get_Enum( complete_sys, To_String( parameter.enum_type_ref ));
                        end if;
                        index_key := Line_Extractor.Make_Key( key, row, parameter.instance_name );
                        index_key := Censor_Id( index_key );
                        val_and_err := Get_Value_And_Error( 
                           param_and_value => complete_value,
                           index           => row,
                           postfix         => parameter.name );
                        make_hidden := ( row = complete_value.current_size ) and then
                          (( parameter.Edit_Info.topset ) or ( parameter.logical_type = tax_band ));
                        cell := Create_Single_Input( 
                           key              => index_key,
                           lang             => buff.lang,
                           param_desc       => parameter,
                           val_and_err      => val_and_err.all,
                           enum_type        => enum,
                           do_complete_row  => False,
                           make_hidden      => make_hidden );
                        if( make_hidden )then
                           cell := cell & Lookup( "All other income", buff.lang );
                        end if;
                        s := s & INDENT & INDENT & INDENT & "<td>" & cell & "</td>";
                     end;
                     s := s & LINE_BREAK;
                  end loop each_param;
                  --
                  -- action options 
                  --
                  for action in Ajax_Action_Type'First .. delete loop
                     print_edit := not 
                      ((( action = delete ) and ( complete_value.current_size = 1 )) or
                       (( action /= delete ) and ( complete_value.current_size = complete_value.reference_desc.maximum_size )));
                     if( print_edit )then
                        s := s & "<td>" & Make_Ajax_Call_Indexed( 
                           action => action,
                           key    => key,
                           row             => row,
                           lang            => buff.lang,
                           ajax_target_key => ajax_target_key ) & "</td>"; 
                     else
                        s := s & "<td></td>";
                     end if;
                  end loop;
                  s := s & INDENT & INDENT & "</tr>" & LINE_BREAK;
               end loop each_index;
               s := s & INDENT & "</tbody>" & LINE_BREAK & "</table>" & LINE_BREAK;
              --  s := s & "</div>" & LINE_BREAK;
               
            end;
         when enumerated_type =>
            Raise_Exception( Param_Exception'Identity, 
               "Parameter-System.Input_Buffer : enumerated type index is not inplemented key:" & To_String( key ));
                  -- -- find enum type ref
                  -- -- loop through elements; key is +[id]
          when string_type => --
            Raise_Exception( Param_Exception'Identity, 
               "Parameter-System.Input_Buffer : list of params using string index is not inplemented key:" & To_String( key ));
       end case;
       return s;
   end Make_Indexed_Block;

   
   procedure Create_HTML_Inputs( 
      html_cells         : in out Templates_Parser.Vector_Tag;
      buff               : Buffer;
      base_sys           : Parameter_System_Rec; 
      sys                : Parameter_System_Rec; 
      parameter_prefix   : Unbounded_String ) is
   prefix_copy : Unbounded_String := parameter_prefix;
   
   use Text_Utils.String_Maps_Package;
   use Ada.Exceptions; 
   use Templates_Parser;
   use Utils;

   begin
      Iterate_Parameters:
      declare
      use Parameter_Rec_Package;
         num_params : constant Natural := Natural( sys.parameters.Length );  
         parameter  : Parameter_Rec;
         key        : Unbounded_String;
         html       : Unbounded_String;
       begin
         for pno in 1 .. num_params loop
            parameter := sys.parameters.Element( pno );
            -- FIXME MISSING array params 
            if( parameter.edit_info.display = label_only )then
               html := 
                  TuS( "<tr><td colspan='2' align='middle'><h3>" ) & 
                  parameter.Description( label, buff.lang ) & 
                  "</h3></td></tr>"; 
               html_cells := html_cells & html;              
            else
               if( parameter.collection_type = singular ) then
                  declare
                     complete_value : Complete_Param_And_Value_Rec;
                  begin
                     key := parameter_prefix & DELIMITER & parameter.instance_name;
                     Put_Line( "looking for key |" & To_String( key ) & "|" );
                     complete_value := buff.params.Element( key );
                     html := Create_Single_Input( 
                           key              => key,
                           lang             => buff.lang,
                           param_desc       => complete_value.param_desc,
                           val_and_err      => complete_value.val.all,
                           enum_type        => complete_value.enum_type,
                           do_complete_row  => True );
                     html_cells := html_cells & html;
                     -- if( complete_value.on_display_handler /= null ) and then 
                             -- ( complete_value.val.error = no_error ) then
                        -- complete_value.on_display_handler.all( buff ); 
                     -- end if;
                  end;
               else
                  Raise_Exception( Param_Exception'Identity, 
                      "Parameter-System.Input_Buffer : non single param is not inplemented parameter" & To_String( key ));
               end if;
            end if;
         end loop;
      end Iterate_Parameters;
      
      Iterate_References:
      declare
      use Parameter_System_Reference_Rec_Package;  
      use Parameter_Rec_Package;
         num_references : constant Natural := Natural( sys.parameter_system_references.Length );  
         key            : Unbounded_String;
         ref            : Parameter_System_Reference_Rec;
         row            : Unbounded_String;
         ajax_target    : Unbounded_String;
      begin
         for rno in 1 .. num_references loop
            ref := sys.parameter_system_references.Element( rno );
            key := parameter_prefix & DELIMITER & ref.instance_name;
            ajax_target :=  Censor_String( Censor_Id( key & "_td" )); -- clean this up as jquery doesn't like dots in insert target ids
            Log( "looking for key " & TS( key ));
            row := TuS( "<tr id='" ) & key & "_tr' class='input_table_block'><td id='" & ajax_target & "' >" & LINE_BREAK &
            Make_Indexed_Block(
               complete_sys      => base_sys,
               key               => key,
               buff              => buff,
               ajax_target_key   => ajax_target ) & LINE_BREAK &
               "</td></tr>" & 
               LINE_BREAK;
            html_cells := html_cells & row;
         end loop;
      end Iterate_References;
      
      Iterate_Subsystems:
      declare
         use Parameter_System_Rec_Package;
         subsys : Parameter_System_Rec;
         num_subsystems : constant Natural := Natural( sys.parameter_systems.Length );
      begin
         for sno in 1 .. num_subsystems loop
            subsys := sys.parameter_systems.Element( sno ).all;
            if( subsys.instance_name /= Null_Unbounded_String ) then
               Create_HTML_Inputs(
                  html_cells,
                  buff,
                  base_sys,
                  subsys,
                  parameter_prefix & DELIMITER & subsys.instance_name );
               end if;
         end loop;
      
      end Iterate_Subsystems;
   
   end Create_HTML_Inputs;

   function Create_Inner_Input_Page(
      buff                : Buffer;
      base_sys            : Parameter_System_Rec; 
      sys                 : Parameter_System_Rec; 
      parameter_prefix    : Unbounded_String;
      error_count         : Natural;
      job_is_running      : Boolean;
      user                : EU.BE.Users.User_Type;
      extra_translations  : Templates_Parser.Translate_Set ) return Unbounded_String is
   use Templates_Parser;
      
      html_cells    : Templates_Parser.Vector_Tag;
      translations  : Translate_Set;
   begin
      Create_HTML_Inputs( 
         html_cells,
         buff,
         base_sys, 
         sys,
         parameter_prefix );   
      Put_Line( euws.template_components_path );
      Insert( translations, extra_translations );
      Insert( translations, Assoc( "MAIN-HELP", "MAIN HELP GOES HERE" ));
      Insert( translations, Assoc( "INFORMATION", Lookup( "Information", buff.lang ) ));
      -- Insert( translations, Assoc( "VALIDATION", "VALIDATION CODE HERE" ));
      Insert( translations, Assoc( "ROOT", euws.Mefisto_Root ));
      Insert( translations, Assoc( "LANG", Censor_String( buff.lang'Img )));
      Insert( translations, Assoc( "IS_INPUT_PAGE", True ));
      Insert( translations, Assoc( "HEADER", sys.Description( label, buff.lang )));
      Insert( translations, Assoc( "ERROR-COUNT", error_count ));
      Insert( translations, Assoc( "MAIN-HELP", "HEHEHEH" ));
      --   Write_Help_Box( 
      --      parameter_prefix, Describable( sys ), buff.lang )));
      Insert( translations, Assoc( "INPUT-CELL", html_cells ));
      return Web_Utils.Parse_Template( TuS( euws.template_components_path ) & 
         euws.Dir_Separator & "input_new", translations );      
   end Create_Inner_Input_Page;
   
   -- not used any more
   function Create_Input_Page(
      buff                : Buffer;
      model_menu          : Unbounded_String;
      base_sys            : Parameter_System_Rec; 
      sys                 : Parameter_System_Rec; 
      parameter_prefix    : Unbounded_String;
      main_error_message  : Unbounded_String;
      job_is_running      : Boolean;
      user                : EU.BE.Users.User_Type;
      extra_translations  : Templates_Parser.Translate_Set ) return Unbounded_String is
   use Templates_Parser;
      
      html_cells    : Templates_Parser.Vector_Tag;
      translations  : Translate_Set;
   begin
      Create_HTML_Inputs( 
         html_cells,
         buff,
         base_sys, 
         sys,
         parameter_prefix );   
      Put_Line( euws.template_components_path );
      
      Insert( translations, extra_translations );
      Insert( translations, Assoc( "MAIN-HELP", "MAIN HELP GOES HERE" ));
      Insert( translations, Assoc( "MODEL-MENU", model_menu ));
      Insert( translations, Assoc( "INFORMATION", Lookup( "Information", buff.lang ) ));
      Insert( translations, Assoc( "ROOT", euws.Mefisto_Root ));
      Insert( translations, Assoc( "LANG", Censor_String( buff.lang'Img )));
      Insert( translations, Assoc( "IS_INPUT_PAGE", True ));
      Insert( translations, Assoc( "HEADER", sys.Description( label, buff.lang )));
      
      Insert( translations, Assoc( "MAIN-HELP", "HEHEHEH" ));
       Insert( translations, Assoc( "INPUT-CELL", html_cells ));
      return Web_Utils.Parse_Template( TuS( euws.template_components_path ) & 
         euws.Dir_Separator & "input", translations );      
   end Create_Input_Page;


   function Create_Wrapper_Input_Page(
      model_menu          : Unbounded_String;
      user                : EU.BE.Users.User_Type;
      extra_translations  : Templates_Parser.Translate_Set ) return Unbounded_String is
   use Templates_Parser;
      
      html_cells    : Templates_Parser.Vector_Tag;
      translations  : Translate_Set;
   begin
      Insert( translations, extra_translations );
      Insert( translations, Assoc( "MAIN-HELP", "MAIN HELP GOES HERE" ));
      Insert( translations, Assoc( "MODEL-MENU", model_menu ));
      Insert( translations, Assoc( "ROOT", euws.Mefisto_Root ));
      Insert( translations, Assoc( "IS_INPUT_PAGE", True ));
      Insert( translations, Assoc( "MAIN-HELP", "HEHEHEH" ));
      return Web_Utils.Parse_Template( TuS( euws.template_components_path ) & 
         euws.Dir_Separator & "input_container", translations );      
   end Create_Wrapper_Input_Page;

   
   procedure Switch_Payment_For_UB_1( buff : in out Buffer ) is
      payment_type     : Complete_Param_And_Value_Rec := buff.params.Element( TuS( "be.unemployment_benefits.unemployment_benefit_after_regular_employment.cohabitating_non_disabled.more_than_16_months_unemployed_20_years.payment_type" ));
      lump_sum         : Complete_Param_And_Value_Rec := buff.params.Element( TuS( "be.unemployment_benefits.unemployment_benefit_after_regular_employment.cohabitating_non_disabled.more_than_16_months_unemployed_20_years.lump_sum" ));
      replacement_rate : Complete_Param_And_Value_Rec := buff.params.Element( TuS( "be.unemployment_benefits.unemployment_benefit_after_regular_employment.cohabitating_non_disabled.more_than_16_months_unemployed_20_years.replacement_rate" ));
   begin
      if( payment_type.val.eval = TuS( "lump_sum" ))then
         replacement_rate.val.rval := 0.0;
         lump_sum.param_desc.edit_info.enabled := True;
         replacement_rate.param_desc.edit_info.enabled := False;
      else
         lump_sum.val.rval := 0.0;
         lump_sum.param_desc.edit_info.enabled := False;
         replacement_rate.param_desc.edit_info.enabled := True;
      end if;
      buff.params.Replace( TuS( "be.unemployment_benefits.unemployment_benefit_after_regular_employment.cohabitating_non_disabled.more_than_16_months_unemployed_20_years.lump_sum" ), lump_sum );
      buff.params.Replace( TuS( "be.unemployment_benefits.unemployment_benefit_after_regular_employment.cohabitating_non_disabled.more_than_16_months_unemployed_20_years.replacement_rate" ), replacement_rate );
   end Switch_Payment_For_UB_1;

end Parameter_System.Input_Buffer.EU_Renderer;
