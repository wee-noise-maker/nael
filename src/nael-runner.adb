with Nael.Audio_Backend;
with Nael.Lab_GUI;
with Nael.Value_Exchange;
with Nael.Frame_Exchange;
with Nael.MIDI_Exchange;

procedure Nael.Runner (Sample_Rate : Natural := 44_100;
                       Block_Size  : Natural := 64)
is

   The_Experiment : Exp;
   Val_Exchange   : aliased Nael.Value_Exchange.Instance;
   Block_Exchange : aliased Nael.Frame_Exchange.Instance;
   MIDI_Exchange  : aliased Nael.MIDI_Exchange.Instance;
   User_Controls  : Nael.Lab_GUI.User_Control_Setup;
   Lab            : Nael.Lab_GUI.Instance;

   --------------------
   -- Audio_Callback --
   --------------------

   procedure Audio_Callback (Buffer : out Block) is
   begin
      The_Experiment.Render (Sample_Rate,
                             Buffer,
                             Val_Exchange,
                             MIDI_Exchange);

      --  Hard Clipping
      for Elt of Buffer loop
         Elt := Float'Min (Float'Max (-1.0, Elt), 1.0);
      end loop;

      Block_Exchange.Push (Buffer);
   end Audio_Callback;

begin
   if not The_Experiment.Setup (User_Controls) then
      return;
   end if;

   Lab.Start (Sample_Rate,
                 User_Controls,
                 Val_Exchange'Unrestricted_Access,
                 Block_Exchange'Unrestricted_Access,
                 MIDI_Exchange'Unrestricted_Access);

   Nael.Audio_Backend.Start (Sample_Rate,
                             Block_Size,
                             Audio_Callback'Unrestricted_Access);

   loop
      delay 0.1;
      exit when Lab.Closed;
   end loop;
end Nael.Runner;
