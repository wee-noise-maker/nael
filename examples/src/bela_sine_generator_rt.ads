with Nael.Experiment;
with Nael.Lab_GUI;
with Nael.Value_Exchange;

package Sine_Generator_RT is

   type Instance is new Nael.Experiment.Instance with record
      Freq_Slider : Nael.Controller_Id;
      Amp_Slider  : Nael.Controller_Id;
   end record;

   overriding
   function Setup
     (This         : in out Instance;
      User_Contols : in out Nael.Lab_GUI.User_Control_Setup'Class)
      return Boolean;

   overriding
   procedure Render (This        : in out Instance;
                     Sample_Rate :        Natural;
                     Buffer      :    out Nael.Block;
                     Values      : in out Nael.Value_Exchange.Instance);

end Sine_Generator_RT;
