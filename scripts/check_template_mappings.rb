#!/usr/bin/ruby

require 'find'

def to_float( s )
        if( s =~ /[0-9\.]+/)then
                return Float( s )
        else
                return -99.0
        end
end

def check_one_file( infilename )
        puts "================= ON FILE #{infilename}"
        mf = File.new( infilename );
        lines = mf.readlines
        mf.close;
        
        nl = lines.length;
        elements = Array.new
        mismatches = 0
        nl.times{
                |p|
                split_line = lines[p].split( "\t" )        
                elements[p] = split_line
        }
        has_constants = false
        nlm1 = nl-1
        
        nlm1.times{
                |p|
                line_len = elements[ p ].length
                # puts "elements[#{p}] length #{line_len} last but 2 = |#{elements[p][ line_len-2 ]}| last but 1 = |#{elements[ p ][ line_len-1 ]}| "
                # check elements[p] for a constant .. We look for the 'name' identifier, then "$XXX" at the end
                if( elements[ p ][ line_len-1 ] =~ /\# \-\- should default to (.*?) .*/ ) then
                        actual =  elements[ p ][ line_len-2 ]   
                        should_be = $1
                        varname   = elements[p-1][ line_len - 2 ]
                        # puts "varname #{varname} comparing actual=|#{actual}| with should_be |#{should_be}|\n"
                        mismatch = false;
                        if( actual =~ /(.*)#([a-z])/ )then
                                v = $1
                                v_actual = to_float( v )
                                if( should_be =~ /(.*)#([a-z])/ )then
                                        v_should_be = to_float( $1 )
                                else
                                        mismatch = true
                                end
                        else
                                v_should_be = to_float( should_be )
                                v_actual = to_float( actual )
                        end
                        puts "comparing #{v_actual} with #{v_should_be}"
                        if( v_actual != v_should_be )then
                                puts "** mismatch on varname #{varname} comparing actual=|#{actual}| with should_be |#{should_be}|\n"
                                mismatches += 1
                        end
                end
        }
        return mismatches
end

euromod_path = "/home/graham_s/VirtualWorlds/projects/mefisto/backend/ada/"
inpath = euromod_path + "working/users/test_user2/run_21/params/"
outpath = euromod_path + "tmp/translated/"
mismatches = 0
Find.find( inpath ) do
        |fname|
        if( FileTest.file?( fname ) ) then
                file_root = File.basename( fname )
                if file_root =~ /(.*)\.txt/ then
                        mismatches += check_one_file( fname );
                end
        end
end

puts "\n\n================total mismatches #{mismatches} ==============\n\n"
