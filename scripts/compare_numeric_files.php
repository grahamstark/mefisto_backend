#!/usr/bin/php

<?php
 
class Comparer{

        static function nearlyEqual( $a, $b, $tol = 0.00001 ){
                if(( ! is_numeric( $a )) || ( ! is_numeric( $b ))){
                        return false;       
                }
                return abs( $a-$b) < $tol;
        }
        
        function isEUNumber( $s ){
                if( is_numeric( $s )){
                        return true;        
                }
                if( preg_match( '/[0-9]+\.*\#[a-z]/', $s )){
                        return true;               
                }
                return false;
        }

        /**
         * Compare 2 parameter type files. Split all the elements in each line using delim; 
         * if the elements look like numbers, compare them using nearlyEqual above,
         * else compare strings exactly.
         * @param delim should be a regep enclosed in "/"s,
         * @return an array of string error messages, 1 per error found.
        */
        static function compareLinesSensibly( array $o1, array $o2, $delim = '/\t|,/', $doNumericOnly = true ){
                $out = array();
                $rowCount1 = count( $o1 );
                $rowCount2 = count( $o2 );
                if( $rowCount1 != $rowCount2 ){
                          $out[] = "line counts don't match 1=$rowCount1 2=$rowCount2";      
                }
                $rowsToCheck = min( $rowCount1, $rowCount2 );
                $numericCompares = 0;
                for( $rowPos = 0; $rowPos < $rowsToCheck; $rowPos++ ){
                        $line1 = trim($o1[$rowPos]);
                        $line2 = trim($o2[$rowPos]);
                        $splitLine1 =  preg_split( $delim, $line1 );
                        $splitLine2 =  preg_split( $delim, $line2 );
                        $colCount1 = count( $splitLine1 );
                        $colCount2 = count( $splitLine2 );
                        $rowPosp1 = $rowPos+1;
                        # echo "on line $rowPos; line sizes are $colCount1  $colCount2\n";
                        if( $colCount1 != $colCount2 ){
                                  $out[] = "line sizes don't match on line $rowPosp1 1=$colCount1 2=$colCount2";      
                        }
                        $colsToCheck = min( $colCount1, $colCount2 );
                        for( $colPos = 0; $colPos < $colsToCheck; $colPos++ ){
                                $cell1 = trim($splitLine1[$colPos]);
                                $cell2 = trim($splitLine2[$colPos]);
                                $colPosp1 = $colPos + 1; // 1st col is "1" not zero in displays
                                if( self::isEUNumber( $cell1 )){
                                        if( self::isEUNumber( $cell2 )){
                                                $numericCompares++;
                                                if( ! self::nearlyEqual( doubleval( $cell2 ), doubleval($cell1 ), 0.00005 ) ){
                                                        $out[]= "line $rowPosp1 element $colPosp1 numbers not equal 1 = {$cell1} 2= {$cell2}"; 
                                                }
                                        } else {
                                                $out[] = "line $rowPosp1; element $colPosp1 1 looks like number ({$cell1}); 2 doesn't {$cell2})";
                                        }
                                } else {
                                        if(( $cell1 != $cell2 ) && ( ! $doNumericOnly )){
                                                $out[] = "line $rowPosp1; element $colPosp1; strings not same 1 = ({$cell1}); 2= {$cell2})";
                                        }
                                }
                        }                
                }
                $out[] = $numericCompares;
                return $out;
        }
}

$a1 = file( $argv[1] );
$a2 = file( $argv[2] );
echo "comparing files {$argv[1]} and  {$argv[2]}\n";

$dump = Comparer::compareLinesSensibly( $a1,  $a2 );
if( count( $dump ) > 0 ){
        print_r( $dump );       
} else {
        echo "no differences";
}       

/**
$s = 100;
echo "$s isEUNumber= ".Comparer::isEUNumber( $s )."\n";

$s = "1000#m";
echo "$s isEUNumber= ".Comparer::isEUNumber( $s )."\n";

$s = "SGSGSGSG";
echo "$s isEUNumber= ".Comparer::isEUNumber( $s )."\n";
**/
