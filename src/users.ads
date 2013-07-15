with Ada.Strings.Unbounded;

package Users is

   use Ada.Strings.Unbounded;
   
   type User_Level is ( invalid, temp, standard, expert, admin );

   type User_Type is tagged private;
   
   function Name( this : User_Type ) return Unbounded_String;
   function Level( this : User_Type ) return User_Level;
   function Get( username : Unbounded_String; password : Unbounded_String ) return User_Type;
   procedure Set_Password( this : in out User_Type; password : Unbounded_String );

private
   
   subtype String32 is String( 1 .. 32 );

   type User_Type is tagged record
      name         : Unbounded_String := Null_Unbounded_String;
      level        : User_Level := invalid;
      password_md5 : String32;
   end record;
   
end Users;
