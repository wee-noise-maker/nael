with Nael.Runner;
with Bela_Filters;

procedure Run_Bela_Filters is
   procedure Run is new Nael.Runner (Bela_Filters.Instance);
begin
   Run;
end Run_Bela_Filters;
