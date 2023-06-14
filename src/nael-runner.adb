with Nael.Audio_Backend;
with Nael.Lab_GUI;
with Nael.Value_Exchange;
with Nael.Frame_Exchange;
with Nael.MIDI_Exchange;

package body Nael.Runner is

   type Any_Experiment is access all Nael.Experiment.Instance'Class;

   G_Exp            : Any_Experiment := null;
   G_Sample_Rate    : Natural := 44_100;
   G_Val_Exchange   : aliased Nael.Value_Exchange.Instance;
   G_Block_Exchange : aliased Nael.Frame_Exchange.Instance;
   G_MIDI_Exchange  : aliased Nael.MIDI_Exchange.Instance;

   --------------------
   -- Audio_Callback --
   --------------------

   procedure Audio_Callback (Buffer : out Block) is
   begin
      if G_Exp /= null then
         G_Exp.Render (G_Sample_Rate,
                       Buffer,
                       G_Val_Exchange,
                       G_MIDI_Exchange);

         --  Hard Clipping
         for Elt of Buffer loop
            Elt := Float'Min (Float'Max (-1.0, Elt), 1.0);
         end loop;

         G_Block_Exchange.Push (Buffer);
      else
         Buffer := (others => 0.0);
      end if;
   end Audio_Callback;

   ---------
   -- Run --
   ---------

   procedure Run (Exp         : aliased in out Nael.Experiment.Instance'Class;
                  Sample_Rate :                Natural := 44_100;
                  Block_Size  :                Natural := 64)
   is
      User_Controls : Nael.Lab_GUI.User_Control_Setup;

      Lab : Nael.Lab_GUI.Instance;
   begin
      if not Exp.Setup (User_Controls) then
         return;
      end if;

      G_Exp := Exp'Unchecked_Access;
      G_Sample_Rate := Sample_Rate;

      Lab.Start (Sample_Rate,
                 User_Controls,
                 G_Val_Exchange'Access,
                 G_Block_Exchange'Access,
                 G_MIDI_Exchange'Access);

      Nael.Audio_Backend.Start (Sample_Rate,
                                Block_Size,
                                Audio_Callback'Access);

      loop
         delay 0.1;
         exit when Lab.Closed;
      end loop;

      G_Exp := null;
   end Run;

end Nael.Runner;
