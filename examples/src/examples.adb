with Bela_Sine_Generator_RT;
with Bela_Filters;

with Nael.Runner;
procedure Examples is
   Sine_Exp : aliased Bela_Sine_Generator_RT.Instance;
   Filters_Exp : aliased Bela_Filters.Instance;
begin
   --  Nael.Runner.Run (Filters_Exp);
   Nael.Runner.Run (Sine_Exp);
end Examples;
