with Ada.Numerics.Elementary_Functions;

with Glib.Object; use Glib.Object;
with Glib; use Glib;
with Gdk.Rectangle; use Gdk.Rectangle;

with Ada.Text_IO; use Ada.Text_IO;

package body Nael.Lab_GUI.Spectrum_Analyser_Widget is

   Class : Ada_GObject_Class := Uninitialized_Class;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Widget      :    out Spectrum_Analyser;
                      Sample_Rate : in     Natural)
   is
   begin
      Widget := new Spectrum_Analyser_Record;
      Spectrum_Analyser_Widget.Initialize (Widget, Sample_Rate);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Widget      : not null access Spectrum_Analyser_Record'Class;
      Sample_Rate : in              Natural)
   is
   begin
      G_New (Widget, Spectrum_Analyser_Widget.Get_Type);
      Gtk.Drawing_Area.Initialize (Widget);

      Widget.Sample_Rate := Sample_Rate;

      --  Pre-compute bin frequencies
      for N in Bin_Range loop
         Widget.Bin_Frequency (N) :=
           (Float (N) - 1.0) *
             Float (Sample_Rate) / Float (Analyser_FFT_Size);
      end loop;

      Return_Boolean_Callback.Connect
        (Widget,
         "draw",
         Return_Boolean_Callback.To_Marshaller (Draw'Access));

   end Initialize;

   ----------------
   -- Push_Frame --
   ----------------

   procedure Push_Frame (Widget : in out Spectrum_Analyser_Record;
                         Frame  :        Mono_Frame)
   is
   begin
      if Widget.FFT.Push_Frame (Frame) then
         Widget.Process_FFT;
         Widget.Queue_Draw;
      end if;
   end Push_Frame;

   --------------
   -- Get_Type --
   --------------

   function Get_Type return Glib.GType is
   begin
      Initialize_Class_Record
        (Ancestor     => Gtk.Drawing_Area.Get_Type,
         Class_Record => Class,
         Type_Name    => "SpectrumAnalyser");
      return Class.The_Type;
   end Get_Type;

   -----------------
   -- Process_FFT --
   -----------------

   procedure Process_FFT (Widget : in out Spectrum_Analyser_Record'Class) is
      use Ada.Numerics.Elementary_Functions;
   begin
      --  Compute and current energy
      for Index in Bin_Range loop
         declare
            use Ada.Numerics.Elementary_Functions;

            Rel : constant Float := Widget.FFT.Rel (Index);
            Img : constant Float := Widget.FFT.Img (Index);
            Mag : constant Float := Sqrt (Rel**2 + Img**2);
         begin
            if Mag <= 0.0 then
               Widget.Energy (Index) := 0.0;
            else
               Widget.Energy (Index) := 20.0 * Log (Mag, 10.0);
            end if;
         end;
      end loop;

      --  Compute decaying peak energy
      for Index in Bin_Range loop
         declare
            Local_Average : Float := 0.0;
            Num_Points : Natural := 0;
         begin
            --  Compute local average
            for K in Integer'Max (Bin_Range'First, Index - 5) ..
              Integer'Min (Bin_Range'Last, Index + 5)
            loop
               Local_Average := Local_Average + Widget.Energy (K);
               Num_Points := Num_Points + 1;
            end loop;
            Local_Average := Local_Average / Float (Num_Points);

            --  Decay
            Widget.Peak_Energy (Index) :=
              Widget.Peak_Energy (Index) * 0.98;


            --  Check for new peak
            if Local_Average > Widget.Peak_Energy (Index) then
               Widget.Peak_Energy (Index) := Local_Average;
            end if;

         end;
      end loop;

   end Process_FFT;

   ----------
   -- Draw --
   ----------

   function Draw
     (Widget  : access Spectrum_Analyser_Record'Class;
      Context : Cairo.Cairo_Context)
      return Boolean
   is
      use Cairo;

      Area   : Gdk_Rectangle;
   begin
      Widget.Get_Allocation (Area);
      declare
         Margin : constant Gdouble := 10.0;
         Height : constant Gdouble := Gdouble (Area.Height) - Margin * 2.0;
         Width  : constant Gdouble := Gdouble (Area.Width) - Margin * 2.0;
         Top    : constant Gdouble := Margin;
         Bot    : constant Gdouble := Top + Height;
         Left   : constant Gdouble := Margin;
         Right  : constant Gdouble := Left + Width;

         function Freq_Offset (F : Float) return Gdouble;
         --  For a given frequency F in Hz, return the offset in the drawing
         --  area.

         -----------------
         -- Freq_Offset --
         -----------------

         function Freq_Offset (F : Float) return Gdouble is
            use Ada.Numerics.Elementary_Functions;
            Max_Freq : constant Float :=
              Widget.Bin_Frequency (Bin_Range'Last);
            Min_Freq : constant Float := 20.0;

            Range_Freq : constant Float := abs (Max_Freq - Min_Freq);
         begin
            if F <= Min_Freq then
               return 0.0;
            end if;
            return Gdouble (Log (F - Min_Freq, 2.0) / Log (Range_Freq, 2.0)) * Width;
         end Freq_Offset;

         ---------------
         -- Db_Offset --
         ---------------

         function Db_Offset (Db : Float) return Gdouble is
            Max_Db : constant Float := 50.0;
            Min_Db : constant Float := -40.0;

            Range_Db : constant Float := Max_Db - Min_Db;
         begin
            return Gdouble ((Db - Min_Db) / Range_Db) * Height;
         end Db_Offset;

         ------------------
         -- Bin_X_Offset --
         ------------------

         function Bin_X_Offset (N : Bin_Range) return Gdouble is
            Ret : constant Gdouble := Freq_Offset (Widget.Bin_Frequency (N));
         begin
            --  Ada.Text_IO.Put_Line ("Bin " & N'Img &
            --                          " Freq:" &  Widget.Bin_Frequency (N)'Img &
            --                          " Offset: " & Ret'Img);
            return Ret;
         end Bin_X_Offset;

         --------------------
         -- Draw_Freq_Line --
         --------------------

         procedure Draw_Freq_Line (F : Float) is
            use Ada.Numerics;

            X : constant Gdouble := Freq_Offset (F);
         begin
            Set_Source_Rgba (Context, 0.0, 0.0, 0.0, 0.5);
            Move_To (Context, Left + X, Bot);
            Line_To (Context, Left + X, Top);
            Stroke (Context);

            Move_To (Context, Left + X + 5.0, Top);
            Save (Context);
            Rotate (Context, Pi / 2.0);
            Show_Text (Context, Natural (F)'Img & " Hz");
            Restore (Context);
         end Draw_Freq_Line;

         ------------------
         -- Draw_Db_Line --
         ------------------

         procedure Draw_Db_Line (Db : Float) is
            use Ada.Numerics;

            Y : constant Gdouble := Db_Offset (Db);
         begin

            Set_Source_Rgba (Context, 0.0, 0.0, 0.0, 0.5);
            Move_To (Context, Left, Bot - Y);
            Line_To (Context, Right, Bot - Y);
            Stroke (Context);

            Move_To (Context, Left, Bot - Y - 5.0);
            Show_Text (Context, Integer (Db)'Img & " Db");
         end Draw_Db_Line;
      begin

         for X in 1 .. 9 loop
            if X >= 3 then
               Draw_Freq_Line (Float (X * 10));
            end if;
            Draw_Freq_Line (Float (X * 100));
            Draw_Freq_Line (Float (X * 1000));
            if X <= 2 then
               Draw_Freq_Line (Float (X * 10_000));
            end if;
         end loop;

         for X in -4 .. 5 loop
            Draw_Db_Line (Float (X * 10));
         end loop;

         Set_Source_Rgb (Context, 1.0, 0.0, 0.0);
         Move_To (Context, Left, Bot);
         for N in Bin_Range loop
            Line_To (Context,
                     Left + Bin_X_Offset (N),
                     Bot - Db_Offset (Widget.Energy (N)));
         end loop;
         Stroke (Context);

         Set_Source_Rgb (Context, 0.0, 1.0, 0.0);
         Move_To (Context, Left, Bot);
         for N in Bin_Range loop
            Line_To (Context,
                     Left + Bin_X_Offset (N),
                     Bot - Db_Offset (Widget.Peak_Energy (N)));
         end loop;
         Stroke (Context);

         Set_Source_Rgb (Context, 0.0, 0.0, 1.0);
         for N in Bin_Range loop
            Move_To (Context, Left + Bin_X_Offset (N), Bot);
            Line_To (Context,
                     Left + Bin_X_Offset (N),
                     Bot - Db_Offset (Widget.Energy (N)));
            Stroke (Context);
         end loop;
      end;

      return True;
   end Draw;

end Nael.Lab_GUI.Spectrum_Analyser_Widget;
