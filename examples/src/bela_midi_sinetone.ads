with Nael.Experiment;
with Nael.Lab_GUI;
with Nael.Value_Exchange;
with Nael.MIDI_Exchange;

package Bela_MIDI_Sinetone is

   type Instance is new Nael.Experiment.Instance with record
      Phase     : Float := 0.0;
      Frequency : Float := 440.0;
      Amplitude : Float := 0.0;

      Is_Note_On : Boolean := False;
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
                     Values      : in out Nael.Value_Exchange.Instance;
                     MIDI_Input  : in out Nael.MIDI_Exchange.Instance);

end Bela_MIDI_Sinetone;
