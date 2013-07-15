#!/usr/bin/ruby

require 'convert_libs.rb'


def createOneEnum( deffile, bodyfile, translationsFile, pp_deffile, pp_bodyfile, lines,  p )
        s = lines[p].strip.split
        enumName = s[0]   
        puts "entering with |#{enumName}|\n"
        enumeratedType = EnumeratedType.new( enumName )
        while ( true )
                p += 1      
                break if( lines[p].nil? ) or ( lines[p] == "\n") or (lines[p].length < 1 )
                lines[p].strip!
                if lines[p] =~ /^ *([0-9]+) +(.*) *$/ then
                   puts "on lines[#{p}] = |#{lines[p]}|"
                   puts "val=#{$1} k=|#{$2}|"
                   entry = EnumEntry.new( $1, $2 )
                   enumeratedType.enums << entry
                end
        end
        enumeratedType.writeEnum( deffile )
        enumeratedType.writeValue( deffile, bodyfile )
        enumeratedType.writeConvert( deffile, bodyfile )
        enumeratedType.writePrettyPrint( pp_deffile, pp_bodyfile )
        enumeratedType.writeTranslations( translationsFile )
        return p+1
                
end

deffile = File.new( 'created/enums.ads.tmp', 'w' );
bodyfile = File.new( 'created/enums.adb.tmp', 'w' );
pp_deffile = File.new( 'created/prettyprint.ads.tmp', 'w' );
pp_bodyfile = File.new( 'created/prettyprint.adb.tmp', 'w' );
translationsFile = File.new( 'created/enum_translations.txt', 'w' )

mf = File.new( "enumerated_types.txt" );
lines = mf.readlines
nl = lines.length;
p = 0
while( p < nl )
        p = createOneEnum( deffile, bodyfile, translationsFile, pp_deffile, pp_bodyfile, lines, p )
end
