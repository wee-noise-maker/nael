with Nael.Experiment;
with Nael.Audio_Backend;
with Nael.Lab_GUI;
with Nael.Value_Exchange;
with Nael.FFT;

with Gtk.Oscilloscope; use Gtk.Oscilloscope;
with Gtk.Layered.Waveform.Ring_Data_Buffer;
with Gtk.Layered.Waveform;

with Ada.Real_Time; use Ada.Real_Time;

with Ada.Numerics.Generic_Complex_Arrays;
with Ada.Numerics;
with Ada.Numerics.Generic_Complex_Elementary_Functions;

with Ada.Numerics.Complex_Arrays;
with Ada.Complex_Text_IO;
with Ada.Text_IO; use Ada.Text_IO;

with Ada.Numerics.Complex_Elementary_Functions;
with Ada.Numerics.Elementary_Functions;

with Ada.Complex_Text_IO;          use Ada.Complex_Text_IO;

package body Nael.Runner is

   type Any_Experiment is access all Nael.Experiment.Instance'Class;

   G_Exp          : Any_Experiment := null;
   G_Sample_Rate  : Natural := 44_100;
   G_Values       : Nael.Value_Exchange.Any_Access := null;
   G_Oscilloscope : Gtk_Oscilloscope;
   G_Analyser     : Gtk_Oscilloscope;
   G_Clock        : Float := 0.0;

   FFT_Size : constant := Analyser_FFT_Size;
   G_FFT : Nael.FFT.Instance (Window_Size => FFT_Size,
                              Hop_Size    => FFT_Size / 4);

   G_Freq_Domain : Ada.Numerics.Complex_Arrays.Complex_Vector (1 .. FFT_Size);

   subtype Bin_Range is Natural range 1 .. FFT_Size / 2;
   type Bin_Array is array (Bin_Range) of Float;
   G_Enerrgy      : Bin_Array := (others => 0.0);
   G_Peak_Enerrgy : Bin_Array := (others => 0.0);

   procedure Process_FFT is
      use Gtk.Layered.Waveform.Ring_Data_Buffer;
      use Gtk.Layered.Waveform;

      Analyser_Buffer : Gtk_Wavefrom_Ring_Data_Buffer :=
        G_Analyser.Get_Buffer (1);

      Analyser_Peak_Buffer : Gtk_Wavefrom_Ring_Data_Buffer :=
        G_Analyser.Get_Buffer (2);
   begin

      --  Get fresh FFT results
      G_Freq_Domain := G_FFT.Copy_Frequency_Domain;

      --  Compute and display current energy
      for Index in Bin_Range loop
         declare
            use Ada.Numerics.Elementary_Functions;

            Rel : constant Float := G_Freq_Domain (Index).Re;
            Im  : constant Float := G_Freq_Domain (Index).Im;
            Mag : constant Float := Sqrt (Rel**2 + Im**2);
         begin
            G_Enerrgy (Index) := Mag;

            --  Show current energy
            Analyser_Buffer.Put (X_Axis (Index), Y_Axis (Mag));
         end;
      end loop;

      --  Compute and display decaying peak energy
      for Index in Bin_Range loop
         declare
            Local_Average : Float := 0.0;
            Num_Points : Natural := 0;
         begin
            --  Compute local average
            for K in Integer'Max (Bin_Range'First, Index - 5) ..
              Integer'Min (Bin_Range'Last, Index + 5)
            loop
               Local_Average := Local_Average + G_Enerrgy (K);
               Num_Points := Num_Points + 1;
            end loop;
            Local_Average := Local_Average / Float (Num_Points);

            --  Decay
            G_Peak_Enerrgy (Index) :=
              G_Peak_Enerrgy (Index) * 0.98;


            --  Check for new peak
            if Local_Average > G_Peak_Enerrgy (Index) then
               G_Peak_Enerrgy (Index) := Local_Average;
            end if;

            --  Show peak energy
            Analyser_Peak_Buffer.Put (X_Axis (Index),
                                      Y_Axis (G_Peak_Enerrgy (Index)));
         end;
      end loop;

   end Process_FFT;

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
            Osc_Buffer.Put (X_Axis (G_Clock), Y_Axis (Elt));
            G_Clock := G_Clock + 1.0 / Float (G_Sample_Rate);

            if G_FFT.Push_Frame (Elt) then
               Process_FFT;
            end if;
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
                 G_Oscilloscope,
                 G_Analyser);

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
