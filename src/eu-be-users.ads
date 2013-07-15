with Ada.Containers.Hashed_Maps;
with Ada.Strings.Unbounded;
with Ada.Calendar;


with AWS.Response;
with AWS.Session;
with AWS.Status;

with Text_Utils;
with EU.BE.I81N;

package EU.BE.Users is

   use Ada.Strings.Unbounded;
   use Text_Utils;
   use EU.BE.I81N;
   use Ada.Calendar;
   
   type Preference_Type is ( use_svg_graphics, other_preference );
   type Preference_Array is array( Preference_Type ) of Boolean;
   type User_Class is ( anon, registered, expert, admin, invalid, deleted );

   type User_Type is record
      username     : Unbounded_String;
      password     : Unbounded_String;
      title        : Unbounded_String;
      description  : Unbounded_String;
      email        : Unbounded_String;
      work_dir     : Unbounded_String;
      utype        : User_Class       := anon;
      lang         : Languages        := Languages'First;
      preferences  : Preference_Array := ( others=>False );
      last_used    : Time;
   end record;


   function Compare_Users( left, right : User_Type ) return Boolean;
   
   package User_Maps is new Ada.Containers.Hashed_Maps(
      Key_Type         => Unbounded_String,
      Element_Type     => User_Type,
      Hash             => Text_Utils.Hash_String,
      "="              => Compare_Users,
      Equivalent_Keys  => Text_Utils.Compare_String );
   
   INVALID_USER : constant User_Type :=
      (utype       => invalid,
       lang        => Languages'First,
       username    => TuS("INVALID"),
       password    => TuS("INVALID"),
       title       => TuS("INVALID"),
       description => TuS("INVALID"),
       email       => TuS("INVALID"),
       work_dir    => TuS("INVALID"),
       last_used   => Time_Of( Year_Number'First, Month_Number'First, Day_Number'First, Day_Duration'First ),
       preferences => ( others=> false ) );

   function Validate( 
      users    : User_Maps.Map;
      username : Unbounded_String; 
      password : Unbounded_String ) return User_Type;
      
   type Cookie_Support_Level is ( unknown, no, yes );
      
   package User_Session_Data is new AWS.Session.Generic_Data(
       User_Type,
       INVALID_USER );
       
   type Login_Result is record
      user           : User_Type := INVALID_USER;
      response       : AWS.Response.Data;
      validated      : Boolean := False;
      new_session    : Boolean := False;
      cookie_support : Cookie_Support_Level := unknown;
   end record;
   
private

end EU.BE.Users;
