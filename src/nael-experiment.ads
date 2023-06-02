with Nael.Lab_GUI;
with Nael.Value_Exchange;

package Nael.Experiment is

   type Instance
   is abstract tagged
   private;

   function Setup
     (This         : in out Instance;
      User_Contols : in out Nael.Lab_GUI.User_Control_Setup'Class)
      return Boolean
   is (True);

   procedure Render (This        : in out Instance;
                     Sample_Rate :        Natural;
                     Buffer      :    out Framebuffer;
                     Values      : in out Value_Exchange.Instance)
   is abstract;

private

   type Instance
   is abstract tagged
           record
              Plop : Natural := 0;
           end record;

end Nael.Experiment;
