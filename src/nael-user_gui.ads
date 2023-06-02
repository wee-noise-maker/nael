package Nael.User_GUI is

   type Setup
   is abstract tagged
   private;

   type Controller_Id is range 1 .. 1000;

private

   type Setup
   is abstract tagged
           record
              Plop : Natural := 0;
           end record;

end Nael.User_GUI;
