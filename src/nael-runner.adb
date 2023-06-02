with Nael.Experiment;
with Nael.Audio_Backend;
with Nael.Lab_GUI;
with Nael.Value_Exchange;

with Gtk.Oscilloscope; use Gtk.Oscilloscope;
with Gtk.Layered.Waveform.Ring_Data_Buffer;
with Gtk.Layered.Waveform;

with Ada.Real_Time; use Ada.Real_Time;

package body Nael.Runner is

   type Any_Experiment is access all Nael.Experiment.Instance'Class;

   G_Exp          : Any_Experiment := null;
   G_Sample_Rate  : Natural := 44_100;
   G_Values       : Nael.Value_Exchange.Any_Access := null;
   G_Oscilloscope : Gtk_Oscilloscope;
   G_Plop         : Float := 0.0;

   --------------------
   -- Audio_Callback --
   --------------------

   procedure Audio_Callback (Buffer : out Framebuffer) is
      use Nael.Value_Exchange;
   begin
      if G_Exp /= null and then G_Values /= null then
         G_Exp.Render (G_Sample_Rate, Buffer, G_Values.all);
      end if;

      declare
         use Gtk.Layered.Waveform.Ring_Data_Buffer;
         use Gtk.Layered.Waveform;

         Osc_Buffer : Gtk_Wavefrom_Ring_Data_Buffer :=
           G_Oscilloscope.Get_Buffer (1);
      begin

         for Elt of Buffer loop
            Osc_Buffer.Put (X_Axis (G_Plop), Y_Axis (Elt));
            G_Plop := G_Plop + 1.0 / Float (G_Sample_Rate);
         end loop;
      end;

   end Audio_Callback;

   ---------
   -- Run --
   ---------

   procedure Run (Exp         : aliased in out Nael.Experiment.Instance'Class;
                  Sample_Rate :                Natural := 44_100;
                  Block_Size  :                Natural := 64)
   is
      User_Controls : Nael.Lab_GUI.User_Control_Setup;
      Val_Exchange  : aliased Nael.Value_Exchange.Instance;
      Lab : Nael.Lab_GUI.Instance;
   begin
      if not Exp.Setup (User_Controls) then
         return;
      end if;

      G_Exp := Exp'Unchecked_Access;
      G_Sample_Rate := Sample_Rate;
      G_Values := Val_Exchange'Unchecked_Access;

      Lab.Start (User_Controls,
                 Val_Exchange'Unchecked_Access,
                 G_Oscilloscope);

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
