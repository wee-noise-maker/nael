with Sine_Generator_RT;
with Filters;

with Nael.Runner;
procedure Examples is
   Sine_Exp : aliased Sine_Generator_RT.Instance;
   Filters_Exp : aliased Filters.Instance;
begin
   Nael.Runner.Run (Filters_Exp);
--   Nael.Runner.Run (Sine_Exp);
end Examples;
