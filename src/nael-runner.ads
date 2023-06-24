with Nael.Experiment;

generic
   type Exp is new Nael.Experiment.Instance with private;
procedure Nael.Runner (Sample_Rate : Natural := 44_100;
                       Block_Size  : Natural := 64);
