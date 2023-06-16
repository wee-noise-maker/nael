with Ada.Containers.Vectors;
with Ada.Containers.Doubly_Linked_Lists;
with Ada.Strings.Unbounded;

package Nael.Value_Exchange is

   package User_Control_Value_Vectors
   is new Ada.Containers.Vectors (Controller_Id, Float);

   type Label_Info is record
      Id : Controller_Id;
      Label : Ada.Strings.Unbounded.Unbounded_String;
   end record;

   package Label_Info_List
   is new Ada.Containers.Doubly_Linked_Lists (Label_Info);

   protected type Instance is
      procedure Set (Id : Controller_Id; Value : Float);
      function Get (Id : Controller_Id) return Float;

      procedure Set_Label (Id : Controller_Id; Str : String);
      procedure Pop_Label (Info : out Label_Info; Success : out Boolean);
   private
      Values         : User_Control_Value_Vectors.Vector;
      Label_Requests : Label_Info_List.List;
   end Instance;

   type Any_Access is access all Instance;

end Nael.Value_Exchange;
