with Ada.Containers.Vectors;

package Nael.Value_Exchange is

   package User_Control_Value_Vectors
   is new Ada.Containers.Vectors (Controller_Id, Float);

   protected type Instance is
      procedure Set (Id : Controller_Id; Value : Float);
      function Get (Id : Controller_Id) return Float;
   private
      Values : User_Control_Value_Vectors.Vector;
   end Instance;

   type Any_Access is access all Instance;

end Nael.Value_Exchange;
