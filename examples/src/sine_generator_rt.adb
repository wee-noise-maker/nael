with Ada.Numerics; use Ada.Numerics;
with Ada.Numerics.Elementary_Functions; use Ada.Numerics.Elementary_Functions;

package body Sine_Generator_RT is

   --  G_Total_Sample : Natural := 0; -- How many samples have gone by?

   G_Phase : Float := 0.0;

   -----------
   -- Setup --
   -----------

   overriding
   function Setup
     (This         : in out Instance;
      User_Contols : in out Nael.Lab_GUI.User_Control_Setup'Class)
      return Boolean
   is
   begin
      This.Freq_Slider := User_Contols.Add_Slider ("Frequency (Hz)",
                                                   50.0, 10_000.0, 1.0,
                                                   Default => 440.0);

      This.Amp_Slider := User_Contols.Add_Slider ("Amplitude",
                                                  0.0, 1.0, 0.01,
                                                  Default => 0.6);
      return True;
   end Setup;

   ------------
   -- Render --
   ------------

   overriding
   procedure Render (This        : in out Instance;
                     Sample_Rate :        Natural;
                     Buffer      :    out Nael.Framebuffer;
                     Values      : in out Nael.Value_Exchange.Instance)
   is
      Frequency : Float := 220.0; -- Frequency of the sine wave in Hz
      Amplitude : Float := 0.6; -- Amplitude of the sine wave (1.0 max)

   begin
      Frequency := Values.Get (This.Freq_Slider);
      Amplitude := Values.Get (This.Amp_Slider);

      for Output of Buffer loop
         --  G_Total_Sample := G_Total_Sample + 1;
         --
         --  Output := G_Amplitude *
         --    Sin (2.0 * Pi * Float (G_Total_Sample) *
         --             G_Frequency / Float (Sample_Rate));

         G_Phase := G_Phase + 2.0 * Pi * Frequency / Float (Sample_Rate);

         if G_Phase >= 2.0 * Pi then
            G_Phase := G_Phase - 2.0 * Pi;
         end if;

         Output := Amplitude * Sin (G_Phase);
      end loop;
   end Render;

end Sine_Generator_RT;
