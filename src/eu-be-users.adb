with Ada.Text_IO;
with Text_Utils;
with GNAT.MD5;

package body EU.BE.Users is

   function Compare_Users( left, right : User_Type ) return Boolean is
   begin
      return left = right;
   end Compare_Users;



   function Validate( 
      users    : User_Maps.Map;
      username : Unbounded_String; 
      password : Unbounded_String ) return User_Type is
      
      use User_Maps;
      user : User_Type := INVALID_USER;
   begin
      if( Contains( users, username )) then
         user := Element( users, username );
         if( user.password /= GNAT.MD5.Digest( TS( password ) )) then
            user := INVALID_USER;
         end if;
      end if;
      return user;
   end Validate;
   
  

-- begin
   -- User_Maps.Insert( 
       -- users,
       -- TuS("a_user"),
       -- ( username    => TuS("a_user"),
         -- utype       => registered,
         -- lang        => en,
         -- password    => TuS("xx"),
         -- title       => TuS("Mr X"),
         -- description => TuS(""),
         -- work_dir    => TuS(""),
         -- email       => TuS( "xx@xx.xx" ),
         -- preferences => ( use_svg_graphics=> True, others=> False ) ));
  
end EU.BE.Users;
