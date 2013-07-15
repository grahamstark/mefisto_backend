#!/usr/bin/ruby

def loadDataset( infileName, outfileName ) 
        vd = {}
        lno = 0
        rpos = 0
        file = File.open( infileName, 'rb' )
        outfile = File.open( outfileName, 'wb' )
        file.each_line{
                |line|
                lno += 1
                elements = line.split("\t")
                if( lno == 1 )then
                        puts "st line is "; p elements
                        outfile.write( line )
                        elements.each{
                                |elem|
                                if( elem == 'drgn1' )then
                                        break;
                                end
                                rpos += 1
                                
                        }
                else
                        reg = elements[ rpos ];
                        puts "rpos #{rpos} reg #{reg}"
                        if( reg == "2" )then
                                outfile.write( line )       
                        end
                end
        }
        file.close
        outfile.close
end

if( ARGV.length < 2 )then
        puts "use: <infile> <outfile"
else
        infileName = ARGV[0] 
        outfileName =  ARGV[1]          
        loadDataset( infileName, outfileName )
end 
