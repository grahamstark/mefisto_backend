with Ada.Text_IO;
with Ada.Exceptions;
with AUnit.Test_Cases;
with AUnit.Assertions;
with EU.BE.I81N;
with Input_Sources.File;
with Sax.Readers;     
with DOM.Readers; 
with DOM.Core;        
with DOM.Core.Documents;    
with DOM.Core.Nodes;
with DOM.Core.Attrs;
with XML_Utils;
with XML_Utils.Conversions;
with BE_Base_Model_Types;
with Parameter_System;
with Parameter_System.XML;
with Parameter_System.Input_Buffer;
with Parameter_System.Input_Buffer.Utils;
with Text_Utils;
with Line_Extractor;
with Keyed_Text_Buffer;


package body Parameter_System.Input_Buffer.Tests is
   use AUnit.Assertions;
   use AUnit.Test_Cases.Registration;
   use Ada.Text_IO;
   use BE_Base_Model_Types;
   use Text_Utils;
   
   procedure Set_Up (T : in out Test_Case) is
   begin
      null;
--      BE_Ps.Set_Delimiter( DELIMITER );
--      NA_Ps.Set_Delimiter( DELIMITER );
--      Line_Extractor.Set_Delimiter( DELIMITER );
   end Set_Up;

   ----------
   -- Name --
   ----------

   
   procedure Register_Tests( t : in out Test_Case ) is
   begin 
        -- Register_Routine( T, Test_Basic_XML'Access, "Test Basic XML" );
        null;
   end Register_Tests;

   
end Parameter_System.Input_Buffer.Tests;
