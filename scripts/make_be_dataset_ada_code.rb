#!/usr/bin/ruby

# require 'csv'

ENUMERATED_TYPES = { 
        'dcz'   => 'Citizenship_Type',
        'dcu'   => 'Consensual_Union_Type',
        'deh'   => 'Education_Highest_Status_Type', 
        'dec'   => 'Education_Current_Status_Type', 
        'drgn1' => 'Nuts_Level_1_Type', 
        'dms'   => 'Marital_Status_Type', 
        'loc'   => 'Occupation_Isco_1_Digit',
        'dgn'   => 'Gender_Type', 
        'amrtn' => 'Tenure_Type'}
        
        
AMOUNTS = [ 'dwt', 'yiy' ]        
BOOLEANS = [ 'lcs' ]
INTEGERS = [ 'amrrm' ]

## FIXME : make Write_Household_Header!! 

def censorLabel( label )  # chop off up to 1st ':', cap first word, trim blanks
        return label.
                sub( /(.*?):/,'').
                strip.
                downcase.
                capitalize
end

def censor( s )
        return s.
              strip.
              downcase.        
              gsub( /[=\:\)\('"’;:\.]/,'' ).
              gsub( /[ \-,]/, '_' ).
              gsub( /[\&\+]/,'_and_').
              gsub( /\//,'_or_' ).
              gsub( /__/,'_').
              gsub( /__/,'_').
              gsub( /\%/,'pct').
              gsub( /_$/, '' ).
              gsub( /^_/, '' )   
end

def makeKey( s )
        return s.gsub( /_/, '-' ).upcase()
end

def makeId( s )
        return s.gsub( /\-/, '_' ).downcase()
end

MISSING_INPUT_ENTRIES = {
      "byrmy" => "benefit : early retirement	Early retirement pension - months per year"
}


#      "idorighh" => "original household id",
#      "idorigperson" =>"original person id",

MISSING_OUTPUT_ENTRIES = {
      "tu_household_be_headid" => "Head of household Indentifier",
      "idhh" => "identification number of the household",
      "idperson" => "identification number of the person",
      "idpartner" => "identification number of the person’s partner (0 for no partner in the household)",
      "idmother" => "identification number of the person’s mother (0 for no mother in the household)",
      "idfather" => "identification number of the person’s father (0 for no father in the household)",
      "dwt" => "sample weight (same for all hh-members) dag: person’s age ",
      "dgn" => "0 if the person is a woman, 1 if a man",
      "dms" => "person’s marital status  (1 single, 2 married, 3 separated, 4 divorced, 5 widowed)",
      "dec" => "person’s current education status (0 not completed primary,1 primary, 2 lower secondary, 3 upper secondary, 4 post secondary, 5 tertiary)",
      "les" => "person’s employment status (0 pre-school, 1 farmer, 2 self-employed, 3 employee, 4 pensioner, 5 unemployed, 6 student, 7 inactive, 8 disabled, 9 other)",
      "yem" => "person’s monthly employment earnings",
      "yse" => "person’s monthly self-employment earnings",
      "ils_dispy" => "person’s monthly disposable income",
      "ils_origy" => "person’s monthly original income",
      "ils_ben" => "sum of all benefits received by the person",
      "ils_bensim" => "sum of all simulated benefits in ils_ben",
      "ils_tax" => "sum of all taxes paid by the person",
      "ils_taxsim" => "sum of all simulated taxes in ils_tax",
      "ils_sicee" => "employee social insurance contributions paid by the person",
      "ils_sicer" => "employer social insurance contributions paid for the person (not included in ils_dispy)",
      "ils_sicse" => "self-employed social insurance contributions paid by the person",
      "ils_dispy" => "standard disposable income",
      "il_taxabley" => "taxable income",
      "il_taxabley_bf" => "taxable income (something bf)",
      "il_taxabley_bf_mq" => "taxable income (something mq)",
      "ils_pen" => "pension",
      "ils_bennt" => "bennt",
      "ils_bensim" => "bensim",
      "ils_taxsim" => "taxsim",
      "ils_benmt" => "ils_benmt",
      'ils_tinty' => 'ils_tinty',
      'tu_family_be_headid' => 'tu_family_be_headid'
}

def addMainOutputEntriesToDict( dict )
        
        MISSING_OUTPUT_ENTRIES.merge( MISSING_INPUT_ENTRIES ).each{
                |varname,text|
                if( not dict.has_key?( varname ))then
                        eu = EuroModDataEntry.new
                        eu.sys_var_name = varname
                        eu.sys_modgen  = 1 
                        eu.is_monetary = 1
                        eu.default_variable = 1
                        eu.default_value = 0.0
                        eu.label = text
                        eu.be = ""
                        eu.it = ""
                        eu.sys_end_desc = ""
                        # puts "eu.sys_var_name=#{eu.sys_var_name}\n"
                        dict[ eu.sys_var_name ] = eu
                end 
        }
        return dict;
end

class EuroModDataEntry
        attr_writer  :sys_var_name, :sys_modgen , :is_monetary, :default_variable, :default_value, :label, :be, :it, :sys_end_desc, :pos;
        attr_reader  :sys_var_name, :sys_modgen , :is_monetary, :default_variable, :default_value, :label, :be, :it, :sys_end_desc, :pos;
        
        def level
                vg = variableGroup()
                return "Personal"
                
                return 'Household' if( vg == 'Assets') 
                if( vg == 'Expenditure') then
                        if( @sys_var_name == 'xpp' ) or ( @sys_var_name == 'xmp' )then
                                return 'Personal'
                        else
                                return 'Household'
                        end
                end
                
                return 'Personal'
        end
        
        def variableName
                v = censor( label )
                if( v.nil? or v == '' )then
                        v = sys_var_name
                end
                return v          
        end
        
        def variableGroup
                firstChar = @sys_var_name[0,1]
                puts "sysvarname #{@sys_var_name} firstChar #{firstChar}"
                case firstChar       
                        when 'a' then return 'Assets'
                        when 'b' then return 'Benefits'
                        when 'd' then return 'Demographics'
                        when 'i' then return 'Identifiers'
                        when 'k' then return 'InKind'
                        when 'l' then return 'LabourMarket'
                        when 'p' then return 'Pension'
                        when 's' then return 'System'
                        when 't' then return 'Tax'
                        when 'x' then return 'Expenditure'
                        when 'y' then return 'Income'
                end
        end
        
        def is_enum
                return ENUMERATED_TYPES.has_key?( @sys_var_name )   
        end
        
        def adaType
                vg = variableGroup()
                # puts "vg = #{vg} is_monetary = #{@is_monetary}"
                if( ENUMERATED_TYPES.has_key?( @sys_var_name )) then
                        return ENUMERATED_TYPES[ @sys_var_name ]
                elsif( BOOLEANS.include?( @sys_var_name )) then
                        return 'Boolean'
                elsif( INTEGERS.include?( @sys_var_name )) then
                        return 'Integer'
                elsif( @is_monetary == 1 ) or AMOUNTS.include?( @sys_var_name ) then
                        return 'Amount'
                elsif( ( vg == 'Demographics') or 
                       ( vg == 'LabourMarket' ) or 
                       ( vg == 'Identifiers')) then
                        return 'Integer'
                elsif( vg == 'Income' ) then
                        if( @label.index( 'months' ) != nil ) or ( @sys_var_name =~ /^(.*)y$/ )then
                                return 'Integer'
                        else
                                return 'Amount'
                        end 
                else
                        return 'Amount'
                end
        end
        
end



def loadDataDictionary( dictionaryName )
        vd = {}
        file = File.open( dictionaryName, 'rb' )
        file.each_line{
                |line|
                elements = line.split("\t")
                puts "line is "; p elements
                eu = EuroModDataEntry.new
                eu.sys_var_name = elements[ 1 ]
                eu.sys_modgen  = elements[ 2 ]
                puts( "read eu.sys_var_name as |#{eu.sys_var_name}| eu.sys_modgen as |#{eu.sys_modgen}|"  )
                eu.is_monetary = elements[ 3 ]
                eu.default_variable = elements[ 4 ]
                eu.default_value = elements[ 5 ]
                eu.label = elements[ 6 ]
                eu.be = elements[ 7 ]
                eu.it = elements[ 8 ]
                eu.sys_end_desc = elements[ 9 ]
                # puts "eu.sys_var_name=#{eu.sys_var_name}\n"
                if( not eu.sys_var_name.nil? )then
                        vd[ eu.sys_var_name ] = eu
                end
        }
        file.close;
        return vd
end



def loadVarnames( filename, delim )
        varnames = []
        file = File.open( filename, 'rb' )
        line = file.readline()
        file.close
        p = 0
        cells = line.split( "\t" )
        p cells
        cells.each{
                |cell|
                varnames << cell
        }
        return varnames
end
        
INDENT = '   '

WRITE_PERSON_HEADER = 
'   procedure Write_Person( file : File_Type; person : Person_Rec ) is
    begin
  
'

READ_PERSON_HEADER = 
'   procedure Read_Person( file : File_Type ) return Person_Rec is
    begin
'

WRITE_FOOTER = INDENT + 'end Write_#{which};';

def writeRecordDeclarations( file, variables, dict, level )
        
        variables.each{
               |sys_var_name|
               var = dict[ sys_var_name ]
               if( not var.nil? ) then
                       if( var.level() == level) then
                                label = censorLabel( var.label )
                                file.write( INDENT*2 + "#{var.variableName} : #{var.adaType}; -- (#{var.sys_var_name}) : #{label}\n")                        
                       end 
               end
        }
        
        
end

def writeTranslationsFile(  file, variables, dict )
        variables.each{
               |sys_var_name|
               var = dict[ sys_var_name ]
               if( not var.nil? ) then
                        label = censorLabel( var.label )
                        file.write( "#{var.variableName}=#{label}\n")                        
               end
        }
       
end

def writeReadStatements( file, variables, dict ) 
        p = 0
         variables.each{
               |sys_var_name|
               p += 1
               var = dict[ sys_var_name ]
               if( not var.nil? ) then
                       if( var.is_enum() )then
                               if( var.level() == 'Personal' ) then
                                       file.write( INDENT*4 + "person.#{var.variableName} := Convert( Get_String( slices, #{p} ));\n")
                               else
                                       file.write( INDENT*4 + "hhtmp_#{var.variableName}( pno ) := Convert( Get_String( slices, #{p} ));\n")
                               end
                       
                       else
                               if( var.level() == 'Personal' ) then
                                       file.write( INDENT*4 + "person.#{var.variableName} := Convert( slices, #{p} );\n")
                               else
                                       file.write( INDENT*4 + "hhtmp_#{var.variableName}( pno ) := Convert( slices, #{p} );\n")
                               end
                       end
               end
         }
       
end

def writeWriteStatements( file, variables, dict ) 
        p = 0
         variables.each{
               |sys_var_name|
               p += 1
               var = dict[ sys_var_name ]
               if( not var.nil? ) then
                        if( var.level() == 'Personal' ) then
                                target = "person.#{var.variableName}"
                        else
                                target = "hhtmp_#{var.variableName}( pno )"
                        end
                        atype = var.adaType() 
                        if( atype ==  'Integer' ) or ( atype == 'Amount' ) then
                                ;
                        elsif( atype ==  'Boolean' )
                                target = "#{var.adaType}'Image( #{target} )"
                        else
                                target = "Value( #{target} )"
                        end
                            
                        file.write( INDENT*4 + "Put( file, Format( #{target} ));\n")
       
               end
         }
       
end


def writeLocalReadDeclarations( file, variables, dict ) 
        p = 0
        variables.each{
               |sys_var_name|
               p += 1
               var = dict[ sys_var_name ]
               if( not var.nil? ) then
                       if( var.level() == 'Household' ) then
                                atype = var.adaType()
                                atype = 'Integer' if atype != 'Amount'
                                file.write( INDENT*4 + "hhtmp_#{var.variableName} : array( Person_Range ) of #{atype};\n ");
                       end
               end
         }
      
end


def writeHTMLTemplate( file, variables, dict, level, isResults )
        
        ['Assets',
        'Benefits',
        'Demographics',
        'Identifiers',
        'InKind',
        'LabourMarket',
        'Pension',
        'System',
        'Tax',
        'Expenditure',
        'Income' ].each{
                |category|
                p = 0;
                key = makeKey(category)
                id = makeId( category )+"_block"
                file.write( "<div id='#{id}' class='displayBlock'>\n" )
                file.write( INDENT + "<h2>@_#{key}_@</h2>\n" )
                file.write( INDENT + "<table class='dataTable'>\n" )
                variables.each{
                       |sys_var_name|
                       var = dict[ sys_var_name ]
                       if( not var.nil? ) then
                               
                               if( var.level() == level ) and (category == var.variableGroup() )then
                                       p += 1
                                       
                                       if (p % 2) == 0 then
                                               cssClass =  "tableRowEven" 
                                       else 
                                               cssClass= "tableRowOdd" 
                                       end
                                       key = makeKey( var.variableName() )
                                       id = makeId( var.variableName() )
                                       file.write( INDENT*2+"<tr class='#{cssClass}'  id='#{id}'>\n" )
                                       file.write( INDENT*3 + "<td class='key'>@_#{key}-STR_@</td>\n")        
                                       if( isResults )then
                                               file.write( INDENT*3 + "<td class='value'>@_#{key}-VALUE-PRE_@</td>\n")
                                               file.write( INDENT*3 + "<td class='value'>@_#{key}-VALUE-POST_@</td>\n")
                                       else
                                               file.write( INDENT*3 + "<td class='value'>@_#{key}-VALUE_@</td>\n")
                                       end
                                       file.write( INDENT*2+"</tr>\n")
                               end
                       end
                }
                file.write( INDENT+"</table>\n")
                file.write( "</div>\n\n")
                
        }
end

def writeTemplateMappings( file, variables, dict, level, isResults )
        variables.each{
                |sys_var_name|
                if( not sys_var_name.nil? ) and ( sys_var_name != "" )then
                        sys_var_name = sys_var_name.gsub( /[\n\t ]/, "" )
                        var = dict[ sys_var_name ]
                        if( not var.nil? ) then
                               if( var.level() == level ) then
                                       key = makeKey( var.variableName() )
                                       if( level == 'Personal' )then
                                                target = "pers.#{var.variableName}"
                                       else
                                                target = "hh.#{var.variableName}"
                                       end
                                       atype = var.adaType()
                                       if( atype = 'Amount') or ( atype = 'Integer' ) or ( atype = 'Boolean') then
                                                fmtStmt = "Web_Format( #{target}, lang )"
                                       else
                                                fmtStmt = "Web_Format( #{target}, lang )"
                                       end
                                       postfix = ""
                                       if( isResults )then
                                                postfix = " & postfix"
                                       end      
                                       file.write( INDENT*3 + "Insert( translations, Templates_Parser.Assoc( \"#{key}-VALUE\" #{postfix}, #{fmtStmt}  ));\n" );
                                       file.write( INDENT*3 + "Insert( translations, Templates_Parser.Assoc( \"#{key}-STR\", Lookup( \"#{var.variableName()}\", lang )));\n" );
                               end
                        else
                               puts( "WARNING NO VARIABLE |#{sys_var_name}| was found in the data dictionary HALTING ")
                               Process.exit
                        end
                end
        }
end

def writeDataDefs( be_variables, var_dict, output_dir,  isResults )
        recFile = File.new( "#{output_dir}/be_eu_declarations.inc", 'w')
        recFile.write( INDENT + "type Person_Rec is record\n")
        writeRecordDeclarations( recFile, be_variables, var_dict, 'Personal' )
        recFile.write( INDENT + "end record\n\n\n")
        
        recFile.write( INDENT + "type Household_Rec is record\n")
        writeRecordDeclarations( recFile, be_variables, var_dict, 'Household' )
        recFile.write( INDENT + "end record\n")
        recFile.close
        
        engFile = File.new( "#{output_dir}/be_english_translations.txt", 'w' )
        writeTranslationsFile( engFile, be_variables, var_dict )
        engFile.close
        
        wrFile = File.new( "#{output_dir}/read_statements.inc", 'w' )
        writeReadStatements( wrFile, be_variables, var_dict )
        wrFile.close
        
        wrFile = File.new( "#{output_dir}/write_statements.inc", 'w' )
        writeWriteStatements( wrFile, be_variables, var_dict )
        wrFile.close
        
        ldFile = File.new( "#{output_dir}/local_declarations.inc", 'w' )
        writeLocalReadDeclarations( ldFile, be_variables, var_dict )
        ldFile.close
        
        htFile = File.new( "#{output_dir}/html_template_personal.html", 'w' )
        writeHTMLTemplate( htFile, be_variables, var_dict, "Personal", isResults )
        htFile.close
        
        htFile = File.new( "#{output_dir}/html_template_household.html", 'w' )
        writeHTMLTemplate( htFile, be_variables, var_dict, "Household", isResults )
        htFile.close
        
        htFile = File.new( "#{output_dir}/template_mappings_household.inc", 'w' )
        writeTemplateMappings( htFile, be_variables, var_dict, "Household", isResults )
        htFile.close
        
        htFile = File.new( "#{output_dir}/template_mappings_personal.inc", 'w' )
        writeTemplateMappings( htFile, be_variables, var_dict, "Personal", isResults )
        htFile.close
end

def createDir( directory_name )
        if FileTest::directory?( directory_name)
                return
        end
        Dir::mkdir( directory_name )
end
        
if( ARGV.length < 4 )then
        puts "usage <data Dictionary Name> <dataset Name> <output File Name> <directory for created files>"
else
        
        dataDictionaryName = ARGV[0] #'/home/graham_s/VirtualWorlds/projects/mefisto/backend/ada/working/users/default/default/params/variables.txt'
        datasetName = ARGV[1]  #'../../../model/data/be_2006_a2.txt'
        outputFileName = ARGV[2] #'../working/users/default/default/output/be_2009_std.txt'
        outputDirectory = ARGV[3]
        createDir( outputDirectory )
        createDir( "#{outputDirectory}/data_declarations" )
        createDir( "#{outputDirectory}/household_output" )
        
        be_variables = loadVarnames( datasetName, '\t' )
        
        var_dict = loadDataDictionary( dataDictionaryName ) 
        var_dict = addMainOutputEntriesToDict( var_dict )
        be_household_output_variables = loadVarnames( outputFileName, "\t" )
        
        be_household_output_variables.each{
                |sys_var_name|
                var = var_dict[ sys_var_name ]
                if( not var.nil? ) then
                        puts "varname #{var.sys_var_name} var.adaType() = #{var.adaType()}\n"
                end
        }
        
        puts "input data"
        writeDataDefs( be_variables, var_dict, "#{outputDirectory}/data_declarations", false )
        puts "main output"
        writeDataDefs( be_household_output_variables, var_dict, "#{outputDirectory}/household_output", true )
end
