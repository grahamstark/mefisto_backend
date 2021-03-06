with GNAT.String_Split; 
with Ada.Text_IO;
with Ada.Calendar;          
with Ada.Directories;       
with Ada.IO_Exceptions;
with GNAT.MD5;
with EU_Logger;

package body EU.BE.Users.IO is
   
   procedure Log( s : String ) is
   begin
      EU_Logger.Log( EU_Logger.users, s );
   end Log;

   
   procedure Load_Users( root : String; users : in out User_Maps.Map ) is
   use Ada.Calendar;
   use Ada.Directories;
   use Ada.Text_IO;
      -- 
      -- borrowed from Rosetta Code: http://www.rosettacode.org/wiki/Walk_Directory_Tree#Ada
      --
      procedure Walk( dir_name : String; file_pattern : String ) is
         
         procedure Add_To_User_List( directory_entry : Directory_Entry_Type ) is
            file  : File_Type;
            user  : User_Type;
            name  : constant String := Full_Name( directory_entry );
         begin
            Put_Line( "opening " & name );
            user := Read_User( name );
            if( user.utype /= deleted )then
               users.Insert( user.username, user );
            end if;
         exception
            when others => Put_Line( "failed to open " & name );
         end Add_To_User_List;
         
         procedure Walk( directory_entry : Directory_Entry_Type ) is
         begin
            if Simple_Name( directory_entry ) /= "." and 
               then Simple_Name( directory_entry ) /= ".." then
               Walk( Full_Name( directory_entry ), file_pattern );
            end if;
         exception
            when Ada.IO_Exceptions.Name_Error => null;
         end Walk;
         
      begin
         Search( dir_name, file_pattern, (others => True), Add_To_User_List'Access );
         Search( dir_name, "", (Directory => True, others => False), Walk'Access );
         exception
            when Ada.IO_Exceptions.Name_Error => null; -- don't care if there's no such directory.
      end Walk;
      
   begin
      Walk( root, USER_FILE_NAME );
   end Load_Users;
   

   function Read_User( filename : String ) return User_Type is
      use EU_Key_Value_IO;
      use Ada.Text_IO;
      file : Ada.Text_IO.File_Type; 
      u : User_Type;
   begin
      Open( file, In_File, filename );
      u.username := Read( file,"username" );
      u.password := Read( file,"password" );
      u.title := Read( file,"title" );
      u.description := Read( file,"description" );
      u.email := Read( file,"email" );
      u.work_dir := Read( file,"work_dir" );
      u.utype := User_Class'Value( Read( file,"utype" ));
      u.lang := Languages'Value( Read( file,"lang" ));
      u.last_used := Read( file, "last_used" ); -- it's used now ..
      Close( file );
      return u;
   end Read_User;
   
   procedure Write_User( filename : String; user : User_Type ) is
      use EU_Key_Value_IO;
      use Ada.Text_IO;
      file : Ada.Text_IO.File_Type; 
   begin
      Create( file, Out_File, filename );
      Write( file, "username", user.username );
      Write( file, "password", user.password );
      Write( file, "title", user.title );
      Write( file, "description", user.description );
      Write( file, "email", user.email );
      Write( file, "work_dir", user.work_dir );
      Write( file, "utype", Censor_String( User_Class'Image( user.utype )));
      Write( file, "lang", Censor_String( Languages'Image( user.lang )));
      Write( file, "last_used", Clock ); -- we're using it now.. 
      Close( file );
   end Write_User;
   
   function Create_Directories_For_Run( root : String;  sep: String; user : User_Type; run_number : String ) return Unbounded_String is
   use Ada.Directories;
      target_dir : constant String := root & sep & TS( user.username );
      run_dir    : constant String := target_dir & sep & run_number;
   begin
      Log( "Create_Directories_For_Run: creating run directory |" & run_dir & "|" );
      Create_User_Files( root, sep, user );      
      Create_Path( run_dir & sep & "params" );
      Create_Path( run_dir & sep & "log" );
      Create_Path( run_dir & sep & "output" );
      
      return TuS( run_dir );
   end Create_Directories_For_Run;
   
   -- function Get_Next_Run_Number( root : String;  sep: String; user : User_Type ) return Positive is
   -- use Ada.Directories;
   -- use Ada.Text_IO;
      -- target_dir : constant String := root & TS( user.username );
      -- highest : Natural := 0;
      -- 
      -- procedure Find_Highest( directory_entry : Directory_Entry_Type ) is
         -- file  : File_Type;
         -- name  : constant String := Base_Name( Simple_Name( directory_entry ));
         -- nstr  : constant String := name( 5 .. name'Length );
         -- n     : Positive;
      -- begin
         -- Put_Line( "name |" & name & "| nstr |" & nstr & "|" );
         -- n := Positive'Value( nstr );
         -- if( n > highest )then
            -- highest := n;
         -- end if;
      -- exception
         -- when others => Put_Line( "failed to open " & name );
      -- end Find_Highest;
      -- 
   -- begin
      -- if( not Exists( target_dir ))then
         -- return 1;
      -- end if;
      -- Search( target_dir, "run_*", ( Directory => True, others => False ), Find_Highest'Access );
      -- return highest + 1;
      -- exception
         -- when Ada.IO_Exceptions.Name_Error => null; -- don't care if there's no such directory.
   -- end Get_Next_Run_Number;
   
   procedure Create_User_Files( root : String;  sep: String; user : User_Type ) is
   use Ada.Directories;
      target_dir : constant String := root & sep & TS( user.username );
      target_file : constant String := target_dir & sep & USER_FILE_NAME;
   begin
      if( not Exists( target_dir ))then
         Create_Path( target_dir );   
      end if;
      if( Exists( target_file ))then
         Delete_File( target_file );   
      end if;
      Write_User( target_file, user );
   end Create_User_Files;
   
end EU.BE.Users.IO;
