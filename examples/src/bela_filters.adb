with AAA.Strings;
with Ada.Numerics; use Ada.Numerics;
with Ada.Numerics.Elementary_Functions; use Ada.Numerics.Elementary_Functions;
with Ada.Numerics.Float_Random; use Ada.Numerics.Float_Random;

with Examples_Ressources; use Examples_Ressources;

package body Filters is

   -- First example ----------------------------------------------------------
   type First_Example_Filter is record
      Previous_Frame : Nael.Mono_Frame := 0.0;
   end record;

   ------------
   -- Render --
   ------------

   procedure Render (This   : in out First_Example_Filter;
                     Input  :        Nael.Mono_Frame;
                     Output :    out Nael.Mono_Frame)
   is
   begin
      Output := Input - This.Previous_Frame;
      This.Previous_Frame := Input;
   end Render;
   ---------------------------------------------------------------------------

   -- Second example ----------------------------------------------------------
   type Second_Example_Filter is record
      Previous_Output : Nael.Mono_Frame := 0.0;
   end record;

   ------------
   -- Render --
   ------------

   procedure Render (This   : in out Second_Example_Filter;
                     Input  :        Nael.Mono_Frame;
                     Output :    out Nael.Mono_Frame;
                     Alpha  :        Float)
   is
   begin
      Output := Alpha * This.Previous_Output + (1.0 - Alpha) * Input;
      This.Previous_Output := Output;
   end Render;
   ---------------------------------------------------------------------------

   -- Third example ----------------------------------------------------------
   type Third_Example_Filter is record
      Previous_Output_1 : Nael.Mono_Frame := 0.0;
      Previous_Output_2 : Nael.Mono_Frame := 0.0;
      Previous_Input_1 : Nael.Mono_Frame := 0.0;
      Previous_Input_2 : Nael.Mono_Frame := 0.0;

      B0, B1, B2, A1, A2 : Float := 0.0; -- Coefs
   end record;

   ---------------------------
   -- Calculate_Coefficient --
   ---------------------------

   procedure Calculate_Coefficient
     (This        : in out Third_Example_Filter;
      Sample_Rate :        Natural;
      Frequency   :        Float;
      Q           :        Float)
   is
      K : constant Float := Tan (Pi * Frequency / Float (Sample_Rate));
      Norm : constant Float := 1.0 / (1.0 + K / Q + K**2);
   begin
      This.B0 := K**2 * Norm;
      This.B1 := 2.0 * This.B0;
      This.B2 := This.B0;

      This.A1 := 2.0 * (K**2 - 1.0) * Norm;
      This.A2 := (1.0 - K / Q + K**2) * Norm;
   end Calculate_Coefficient;

   ------------
   -- Render --
   ------------

   procedure Render (This        : in out Third_Example_Filter;
                     Input       :        Nael.Mono_Frame;
                     Output      :    out Nael.Mono_Frame)
   is
   begin
      Output :=
        This.B0 * Input
          + This.B1 * This.Previous_Input_1
          + This.B2 * This.Previous_Input_2
          - This.A1 * This.Previous_Output_1
          - This.A2 * This.Previous_Output_2;

      This.Previous_Input_2 := This.Previous_Input_1;
      This.Previous_Input_1 := Input;

      This.Previous_Output_2 := This.Previous_Output_1;
      This.Previous_Output_1 := Output;
   end Render;
   ---------------------------------------------------------------------------

   Gen : Ada.Numerics.Float_Random.Generator;

   G_First  : First_Example_Filter;
   G_Second : Second_Example_Filter;
   G_Third  : Third_Example_Filter;

   -----------
   -- Setup --
   -----------

   overriding
   function Setup
     (This          : in out Instance;
      User_Controls : in out Nael.Lab_GUI.User_Control_Setup'Class)
      return Boolean
   is
      use AAA.Strings;
   begin
      This.Input_Drop := User_Controls.Add_Drop_Down
        ("Input",
         Empty_Vector
         .Append ("Drum loop")
         .Append ("Noise"));

      This.Lvl_Slider := User_Controls.Add_Slider ("Input level",
                                                   0.0, 1.0, 0.01,
                                                   Default => 0.8);

      This.Kind_Drop := User_Controls.Add_Drop_Down
        ("Filter Kind",
         Empty_Vector
         .Append ("1st (High pass)")
         .Append ("2nd (Low pass)")
         .Append ("3rd (Resonant low pass)"));

      This.Alpha_Slider := User_Controls.Add_Slider ("Alpha (2nd)",
                                                     0.0, 1.0, 0.01,
                                                     Default => 0.9);

      This.Freq_Slider := User_Controls.Add_Slider ("Frequency (3rd)",
                                                    50.0, 22_000.0, 1.0,
                                                    Default => 1000.0);
      This.Q_Slider := User_Controls.Add_Slider ("Q (3rd)",
                                                 0.5, 10.0, 0.01,
                                                 Default => 0.707);
      This.Sample.Open
        (Resource_Path & "hip-to-the-hop_102bpm_A_major.raw_s16le_44100");
      return True;
   end Setup;

   ------------
   -- Render --
   ------------

   overriding
   procedure Render (This        : in out Instance;
                     Sample_Rate :        Natural;
                     Buffer      :    out Nael.Block;
                     Values      : in out Nael.Value_Exchange.Instance)
   is
      Input_Kind : constant Natural := Natural (Values.Get (This.Input_Drop));
      Kind       : constant Natural := Natural (Values.Get (This.Kind_Drop));
      Level      : constant Float := Values.Get (This.Lvl_Slider);
      Alpha      : constant Float := Values.Get (This.Alpha_Slider);
      Freq       : constant Float := Values.Get (This.Freq_Slider);
      Q          : constant Float := Values.Get (This.Q_Slider);
      Input      : Nael.Mono_Frame;
   begin

      Calculate_Coefficient (G_Third, Sample_Rate, Freq, Q);

      for Output of Buffer loop
         case Input_Kind is
            when 0 =>
               This.Sample.Read (Input, In_Loop => True);
               Input := Input * Level;
            when others =>
               Input := Level * (Random (Gen) * 2.0 - 1.0);
         end case;

         case Kind is
            when 0 =>
               Render (G_First, Input, Output);
            when 1 =>
               Render (G_Second, Input, Output, Alpha);
            when 2 =>
               Render (G_Third, Input, Output);
            when others =>
               Output := Input;
         end case;
      end loop;
   end Render;

end Filters;
