with Bela_Sine_Generator_RT;
with Bela_Filters;
with Bela_MIDI_Sinetone;

with Nael.Runner;
procedure Examples is
   pragma Warnings (Off);
   Sine_Exp : aliased Bela_Sine_Generator_RT.Instance;
   Filters_Exp : aliased Bela_Filters.Instance;
   MIDI_Sinetone_Exp : aliased Bela_MIDI_Sinetone.Instance;
   pragma Warnings (On);
begin
   --  Nael.Runner.Run (Filters_Exp);
   --  Nael.Runner.Run (Sine_Exp);
   Nael.Runner.Run (MIDI_Sinetone_Exp);
end Examples;
