#!/usr/bin/ruby

require 'find'

PRINT_ALL = true

$mappings_in_be_mefisto = {
     "UI_LowLimDep7to12M" => 38.75,
     "CB_BaseAmtEmpCh1" => 83.40, 
     "CB_BaseAmtEmpCh2" => 154.33, 
     "CB_BaseAmtEmpCh3" => 230.42, 
     "CB_SuppCh1Age6to11" => 14.53, 
     "CB_SuppCh1Age12to17a" => 22.12, 
     "CB_SuppCh1Age18to25a" => 25.50, 
     "CB_SuppCh2Age6to11" => 28.98, 
     "CB_SuppCh2Age12to17" => 44.27, 
     "CB_SuppCh2Age18to25" => 56.29, 
     "IS_AmtSACat1" => 483.86, 
     "IT_AlwCh1Supp" => 1370.00, 
     "IT_AlwCh2Supp" => 2150.00, 
     "IT_AlwCh3Supp" => 4360.00, 
     "IT_AlwCh4Supp" => 4870.00, 
     "IT_AlwDepMin65Supp" => 1370.00, 
     "IT_AlwDepPlus65Supp" => 2730.00, 
     "IT_AlwLPSupp" => 1370.00, 
     "IT_BaseAlw" => 6430.00, 
     "IT_Bracket1" => 7900.00, 
     "IT_Bracket2" => 11240.00, 
     "IT_Bracket3" => 18730.00, 
     "IT_Bracket4" => 34330.00, 
     "IT_Bracket5" => 999999999.99, 
     "IT_Bracket6" => 0, 
     "IT_Bracket7" => 0, 
     "IT_Bracket8" => 0, 
     "IT_MarrQuotRate" => 0.30, 
     "IT_MarrQuotUpLim" => 9280.00, 
     "IT_RTCBaseAmt1" => 300.00, 
     "IT_RTCIncLowLim1" => 5500.00, 
     "IT_RTCIncLowLim2" => 5500.00, 
     "IT_RTCIncUpLim1" => 22000.00, 
     "IT_RTCIncUpLim2" => 999999999.99, 
     "IT_Rate1" => 0.25, 
     "IT_Rate2" => 0.30, 
     "IT_Rate3" => 0.40, 
     "IT_Rate4" => 0.45, 
     "IT_Rate5" => 0.50, 
     "IT_Rate6" => 0, 
     "IT_Rate7" => 0, 
     "IT_Rate8" => 0, 
     "IT_TCChild" => 390.00, 
     "IT_TCDisab" => 2389.45, 
     "IT_TCReplIncLim1" => 20630.00, 
     "IT_TCReplIncLim2" => 41260.00, 
     "IT_TCUBIncLim2" => 25750.00, 
     "IT_TCUBIncLim1" => 20630.00, 
     "IT_TCUBInd" => 1861.42, 
     "IT_TCPens" => 1861.42,
     "IT_TCReplIncLim1" => 20630.0,
     "IT_TCReplIncLim2" => 4126.0,
     #"IT_ZTDisab" => 15423.94, 
     #"IT_ZTOldUn" => 15391.28, 
     #"IT_ZTOthReplInc" => 13881.55, 
     "IS_AmtSACat2" => 725.79, 
     "IS_AmtSACat3" => 967.72, 
     "UI_LowLimDep6M" => 38.75, 
     "UI_LowLimDep12M" => 38.75, 
     "UI_LowLimSingle6M" => 32.56, 
     "UI_LowLimSingle12M" => 32.56, 
     "UI_LowLimCohab55to57" => 30.77, 
     "UI_LowLimCohab55" => 27.61, 
     "UI_LowLimCohab58" => 33.82, 
     "UI_LowLimOldDep" => 40.62, 
     "UI_LowLimSingle55" => 37.35, 
     "UI_LowLimSingleY55" => 33.99, 
     "UI_PostStudyDepFam" => 37.02, 
     "UI_PostStudyNoPrivCohab18" => 14.38, 
     "UI_PostStudyNoPrivCohabY18" => 9.02, 
     "UI_PostStudyPrivCohab18" => 15.34, 
     "UI_PostStudyPrivCohabY18" => 9.54, 
     "UI_PostStudySingle21" => 27.38, 
     "UI_PostStudySingleY18" => 10.52, 
     "UI_PostStudySingleY21" => 16.53, 
     "UI_ReplRateDep6M" => 0.60, 
     "UI_ReplRateDep7to12M" => 0.60, 
     "UI_ReplRateDep12M" => 0.60, 
     "UI_ReplRateSingle6M" => 0.60, 
     "UI_ReplRateSingle7to12M" => 0.60, 
     "UI_SenSuppDep" => 0.60, 
     "UI_SuppRateCohab55to57" => 0.45, 
     "UI_SuppRateCohab58" => 0.55, 
     "UI_SuppRateCohabY55" => 0.45, 
     "UI_SuppRateSingle55" => 0.60, 
     "UI_SuppRateSingleY55" => 0.54, 
     "UI_UpLimDep6M" => 50.92, 
     "UI_UpLimDep7to12M" => 47.46, 
     "UI_UpLimDep12M" => 44.35, 
     "UI_UpLimSingle6M" => 50.92, 
     "UI_UpLimSingle12M" => 47.46, 
     "UI_UpLimCohab55to57" => 36.96, 
     "UI_UpLimCohab55" => 33.26, 
     "UI_UpLimCohab58" => 40.65, 
     "UI_UpLimOldDep" => 48.67, 
     "UI_UpLimSingle55" => 44.35, 
     "UI_UpLimSingleY55" => 41.24
}

def parse_one_file( infilename, outfilename, pos )
        
        mf = File.new( infilename );
        lines = mf.readlines
        mf.close;
        
        num_lines = lines.length;
        elements = Array.new
        
        num_lines.times{
                |p|
                split_line = lines[p].split( "\t" )        
                elements[p] = split_line
        }
        has_constants = false
        num_linesm1 = num_lines-1
        
        num_linesm1.times{
                |line_no|
                line_len = elements[ line_no ].length
                # clean up the strings a little; esp. we need the random line endiings
                # at len-1 deleted
                line_len.times{
                        |i|
                        elements[ line_no ][ i ].strip!
                }
                puts "elements[#{line_no}] length #{line_len} 2nd element #{elements[line_no][2]} target element #{elements[ line_no ][ line_len - pos ]}"
                # check elements[p] for a constant .. We look for the 'name' identifier, then "$XXX" at the end
                if( elements[ line_no ][ 2 ] =~ /const.*_name/ ) and ( elements[ line_no ][ line_len - pos ] =~ /^\$(.*)$/ ) then
                        # on next elements after p, replace value with template marker
                        key = $1
                        # if we actually possess a mapping for this ..
                        if( $mappings_in_be_mefisto.key?( key ))then
                                puts "replacing #{key}"
                                has_constants = true
                                defval = elements[ line_no + 1 ][ line_len - pos ]
                                elements[ line_no + 1 ][ line_len - pos ] = "@_#{key}_@"
                                # add #y|#m if needed at end of template
                                if defval =~ /(.*)#([a-z])/ then
                                        elements[ line_no + 1 ][ line_len - pos ] += "\##{$2}"    
                                end 
                                # append a comment with the base value for checking
                                highlight = ''
                                puts "#{infilename}"
                                diff = $mappings_in_be_mefisto[key] - defval.to_f
                                if( diff != 0 )then
                                        puts "#{infilename} diff is #{diff} for variable #{key}  #{defval} current backend mapping = #{$mappings_in_be_mefisto[key]}"
                                        highlight = '**DIFFDIFF**'
                                end
                                elements[ line_no + 1 ][ line_len - 1 ] = "# -- should default to #{defval} current backend mapping = #{$mappings_in_be_mefisto[key]} diff #{diff} #{highlight}"
                                $mappings_in_be_mefisto.delete( key )
                                # p elements[p+1]
                        end
                end
        }
        # p elements
        if( has_constants ) then
                outf = File.new( outfilename, "w" );
                puts "writing to #{outfilename}\n"
                p elements
                num_linesm1.times{
                        |line_no|
                        outline = elements[ line_no ].join( ",")
                        p elements[ line_no ]
                        outf.write( outline )
                        outf.write( "\n" )
                        break if( elements[ line_no ][2] == 'sys_end_par')
                }
                outf.close
        end
end

## to library somewhere
def createDir( directory_name )
        if FileTest::directory?( directory_name)
                return
        end
        Dir::mkdir( directory_name )
end


#euromod_path = "/home/graham_s/VirtualWorlds/projects/mefisto/backend/ada/"
# inpath = euromod_path + "tmp/em_jan_10/EuromodFiles_F2.42/Param/txt/"
#inpath = euromod_path + "tmp/em_mar_31/"
#outpath = euromod_path + "tmp/em_mar_31/translated/"


if( ARGV.length < 3 )then
        puts "usage <directory with .txt files> <output directory with .csv files> <column (measured from the end) we want to change>"
else
        inpath = ARGV[0]
        outpath = ARGV[1]
        pos = ARGV[2].to_i
        createDir( outpath )
        Find.find( inpath ) do
                |fname|
                if( FileTest.file?( fname ) ) then
                        file_root = File.basename( fname )
                        if file_root != 'variables' and 
                           file_root != 'emconfig' and
                           file_root =~ /(.*)\.txt/  then

                                puts "on file |#{fname}|"
                                parse_one_file( fname,  outpath +  "/#{$1}.csv", pos );
                        end
                end
        end
        $mappings_in_be_mefisto.sort().each{
                |k,v|
                puts( "PARAMS NOT MAPPED #{k} | #{v}\n" )

        }
end
