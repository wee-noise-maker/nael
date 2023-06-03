with Nael.Experiment;
with Nael.Lab_GUI;
with Nael.Value_Exchange;

package Filters is

   type Instance is new Nael.Experiment.Instance with record
      Kind_Drop        : Nael.Controller_Id;
      Noise_Amp_Slider : Nael.Controller_Id;
      Alpha_Slider     : Nael.Controller_Id;
   end record;

   overriding
   function Setup
     (This          : in out Instance;
      User_Controls : in out Nael.Lab_GUI.User_Control_Setup'Class)
      return Boolean;

   overriding
   procedure Render (This        : in out Instance;
                     Sample_Rate :        Natural;
                     Buffer      :    out Nael.Framebuffer;
                     Values      : in out Nael.Value_Exchange.Instance);

private

   type Filter_Interface is interface;
   type Any_Filter is access all Filter_Interface'Class;

   procedure Render (This   : in out Filter_Interface;
                     Input  :        Nael.Mono_Frame;
                     Output :    out Nael.Mono_Frame;
                     P1     :        Float)
   is abstract;

end Filters;
