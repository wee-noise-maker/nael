with AAA.Strings;
with Ada.Numerics; use Ada.Numerics;
with Ada.Numerics.Elementary_Functions; use Ada.Numerics.Elementary_Functions;
with Ada.Numerics.Float_Random; use Ada.Numerics.Float_Random;

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

   Gen : Ada.Numerics.Float_Random.Generator;

   G_First  : First_Example_Filter;
   G_Second : Second_Example_Filter;

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
      This.Kind_Drop := User_Controls.Add_Drop_Down
        ("Filter Kind",
         Empty_Vector
         .Append ("First example")
         .Append ("Second example"));

      This.Noise_Amp_Slider := User_Controls.Add_Slider ("Noise level",
                                                         0.0, 1.0, 0.01,
                                                         Default => 0.8);

      This.Alpha_Slider := User_Controls.Add_Slider ("Alpha (Second example)",
                                                     0.0, 1.0, 0.01,
                                                     Default => 0.9);

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
      Kind      : Natural;
      Noise_Amp : Float;
      Alpha     : Float;
      Input : Nael.Mono_Frame;
   begin
      Kind := Natural (Values.Get (This.Kind_Drop));
      Noise_Amp := Values.Get (This.Noise_Amp_Slider);
      Alpha := Values.Get (This.Alpha_Slider);

      for Output of Buffer loop
         Input := Noise_Amp * (Random (Gen) * 2.0 - 1.0);

         case Kind is
            when 0 =>
               Render (G_First, Input, Output);
            when 1 =>
               Render (G_Second, Input, Output, Alpha);
            when others =>
               Output := Input;
         end case;
      end loop;
   end Render;

end Filters;
