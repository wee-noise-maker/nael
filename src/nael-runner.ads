with Nael.Experiment;

package Nael.Runner is

   procedure Run (Exp         : aliased in out Nael.Experiment.Instance'Class;
                  Sample_Rate :                Natural := 44_100;
                  Block_Size  :                Natural := 64);

end Nael.Runner;
