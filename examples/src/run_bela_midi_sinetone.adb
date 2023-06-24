with Nael.Runner;
with Bela_MIDI_Sinetone;

procedure Run_Bela_MIDI_Sinetone is
   procedure Run is new Nael.Runner (Bela_MIDI_Sinetone.Instance);
begin
   Run;
end Run_Bela_MIDI_Sinetone;
