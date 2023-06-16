with MIDI;

with Ada.Numerics; use Ada.Numerics;
with Ada.Numerics.Elementary_Functions; use Ada.Numerics.Elementary_Functions;

package body Bela_MIDI_Sinetone is

   ---------
   -- Map --
   ---------

   function Map (X, In_Min, In_Max, Out_Min, Out_Max : Float) return Float is
   begin
      return (X - In_Min) * (Out_Max - Out_Min) / (In_Max - In_Min) + Out_Min;
   end Map;

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
      User_Contols.Enable_Keyboard;
      User_Contols.Enable_Pianoroll;
      return True;
   end Setup;

   -------------
   -- Note_On --
   -------------

   procedure Note_On (This        : in out Instance;
                      Note_Number :        MIDI.MIDI_Key;
                      Velocity    :        MIDI.MIDI_Data)
   is
      Decibels : Float;
   begin
      This.Frequency := 2.0**((Float (Note_Number) - 69.0) / 12.0) * 440.0;

      Decibels := Map (Float (Velocity),
                       In_Min  => Float (MIDI.MIDI_Data'First),
                       In_Max  => Float (MIDI.MIDI_Data'Last),
                       Out_Min => -40.0,
                       Out_Max => 0.0);

      This.Amplitude := 10.0**(Decibels / 20.0);

      This.Is_Note_On := True;
   end Note_On;

   --------------
   -- Note_Off --
   --------------

   procedure Note_Off (This        : in out Instance;
                       Note_Number :        MIDI.MIDI_Key)
   is
      pragma Unreferenced (Note_Number);
   begin
      This.Is_Note_On := False;
   end Note_Off;

   ------------
   -- Render --
   ------------

   overriding
   procedure Render (This        : in out Instance;
                     Sample_Rate :        Natural;
                     Buffer      :    out Nael.Block;
                     Values      : in out Nael.Value_Exchange.Instance;
                     MIDI_Input  : in out Nael.MIDI_Exchange.Instance)
   is

      Msg     : MIDI.Message;
      Success : Boolean;
   begin

      loop
         MIDI_Input.Pop (Msg, Success);
         exit when not Success;

         case Msg.Kind is
            when MIDI.Note_On =>
               Note_On (This, Msg.Key, Msg.Velocity);
            when MIDI.Note_Off =>
               Note_Off (This, Msg.Key);
            when others =>
               null;
         end case;
      end loop;

      for Output of Buffer loop
         if This.Is_Note_On then
            This.Phase := This.Phase +
              2.0 * Pi * This.Frequency / Float (Sample_Rate);

            if This.Phase >= 2.0 * Pi then
               This.Phase := This.Phase - 2.0 * Pi;
            end if;

            Output := This.Amplitude * Sin (This.Phase);
         else
            Output := 0.0;
         end if;
      end loop;
   end Render;

end Bela_MIDI_Sinetone;
