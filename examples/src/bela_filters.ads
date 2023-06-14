with Nael.Experiment;
with Nael.Lab_GUI;
with Nael.Value_Exchange;
with Nael.Sample_Reader;
with Nael.MIDI_Exchange;

package Bela_Filters is

   type Instance is new Nael.Experiment.Instance with record
      Input_Drop   : Nael.Controller_Id;
      Kind_Drop    : Nael.Controller_Id;
      Lvl_Slider   : Nael.Controller_Id;

      Alpha_Slider : Nael.Controller_Id;

      Freq_Slider  : Nael.Controller_Id;
      Q_Slider     : Nael.Controller_Id;

      Sample : Nael.Sample_Reader.Instance;
   end record;

   overriding
   function Setup
     (This          : in out Instance;
      User_Controls : in out Nael.Lab_GUI.User_Control_Setup'Class)
      return Boolean;

   overriding
   procedure Render (This        : in out Instance;
                     Sample_Rate :        Natural;
                     Buffer      :    out Nael.Block;
                     Values      : in out Nael.Value_Exchange.Instance;
                     MIDI_Input  : in out Nael.MIDI_Exchange.Instance);

private

   type Filter_Interface is interface;
   type Any_Filter is access all Filter_Interface'Class;

   procedure Render (This   : in out Filter_Interface;
                     Input  :        Nael.Mono_Frame;
                     Output :    out Nael.Mono_Frame;
                     P1     :        Float)
   is abstract;

end Bela_Filters;
