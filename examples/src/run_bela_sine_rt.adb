with Nael.Runner;
with Bela_Sine_Generator_RT;

procedure Run_Bela_Sine_RT is
   procedure Run is new Nael.Runner (Bela_Sine_Generator_RT.Instance);
begin
   Run;
end Run_Bela_Sine_RT;
