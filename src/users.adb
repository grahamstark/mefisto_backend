with GNAT.MD5;

package body Users is
 
   function Name( this : User_Type ) return Unbounded_String is
   begin
      return this.name;
   end Name;
   
   function Level( this : User_Type ) return User_Level is
   begin
      return this.level;
   end Level;
   
   function Get( username : Unbounded_String; password : Unbounded_String ) return User_Type is
      use GNAT.MD5;
      u : User_Type;
      pw  : constant String := To_String( password );
      md5 : constant Message_Digest := Digest( pw );
   begin
      u.name := username;
      u.password_md5 := md5; -- To_String( password )
      u.level := admin;
      return u;
   end Get;
   
   procedure Set_Password( this : in out User_Type; password : Unbounded_String ) is
   begin
      this.password_md5 := GNAT.MD5.Message_Digest( To_String( password ));
   end Set_Password;
   
end Users;
