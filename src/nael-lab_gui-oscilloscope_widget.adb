with Glib.Object; use Glib.Object;
with Glib; use Glib;
with Gdk.Rectangle; use Gdk.Rectangle;

with Gtk.Drawing_Area; use Gtk.Drawing_Area;
with Gtk.GRange; use Gtk.GRange;
with Gtk.Scale; use Gtk.Scale;
with Gtk.Enums; use Gtk.Enums;

package body Nael.Lab_GUI.Oscilloscope_Widget is

   Class : Ada_GObject_Class := Uninitialized_Class;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Widget      :    out Oscilloscope;
                      Sample_Rate :        Natural)
   is
   begin
      Widget := new Oscilloscope_Record;
      Oscilloscope_Widget.Initialize (Widget, Sample_Rate);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Widget      : not null access Oscilloscope_Record'Class;
      Sample_Rate :                 Natural)
   is
   begin
      Widget.Sample_Rate := Sample_Rate;

      G_New (Widget, Oscilloscope_Widget.Get_Type);
      Gtk.Table.Initialize (Widget, 4, 4, Homogeneous => True);

      Widget.Set_Col_Spacings (3);
      Widget.Set_Row_Spacings (3);

      Gtk_New (Widget.DA);
      Attach (Widget, Widget.DA,
              1, 2,
              0, 1);

      --  Trigger Level Scale
      Gtk_New_With_Range (Widget.Trig_Lvl_Scale,
                          Orientation_Vertical,
                          Min =>  -1.0, Max => 1.0, Step => 0.01);
      Widget.Trig_Lvl_Scale.Set_Value (Gdouble (Widget.Trigger_Level));
      Widget.Trig_Lvl_Scale.Set_Inverted (True);
      Attach (Widget, Widget.Trig_Lvl_Scale,
              0, 1,
              0, 1,
              Xoptions => Shrink);

      --  Zoom Scale
      Gtk_New_With_Range (Widget.Zoom_Scale,
                          Orientation_Horizontal,
                          Min => 1.0, Max => 1000.0, Step => 0.1);
      Widget.Zoom_Scale.Set_Value (Gdouble (Widget.Zoom));
      Widget.Zoom_Scale.Set_Inverted (True);
      Attach (Widget, Widget.Zoom_Scale,
              1, 2,
              1, 2,
              Yoptions => Shrink);

      --  Callbacks

      DA_Return_Handlers.Connect
        (Widget.DA,
         "draw",
         Context_Marshaller.To_Marshaller (Draw'Access),
         Widget.all'Unrestricted_Access);

      Scale_Handlers.Connect
        (Widget.Trig_Lvl_Scale,
         Signal_Value_Changed,
         On_Trig_Lvl_Change'Access,
         Widget.all'Unrestricted_Access);

      Scale_Handlers.Connect
        (Widget.Zoom_Scale,
         Signal_Value_Changed,
         On_Zoom_Change'Access,
         Widget.all'Unrestricted_Access);
   end Initialize;

   ----------------
   -- Push_Frame --
   ----------------

   procedure Push_Frame (Widget : in out Oscilloscope_Record;
                         Frame  :        Mono_Frame)
   is
   begin
      case Widget.Mode is
         when Live =>
            Widget.Buffer (Widget.Ring_Ptr) := Frame;
            Widget.Ring_Ptr := Widget.Ring_Ptr + 1;

            if Widget.Ring_Ptr >= Widget.Points_To_Display then
               Widget.Ring_Ptr := 0;
               Widget.Queue_Draw;
            end if;

         when Continous | Single =>

            case Widget.State is
            when Waiting_For_Trigger =>

               if Widget.Prev_Frame < Widget.Trigger_Level
                 and then
                   Frame > Widget.Trigger_Level
               then
                  Widget.State := Triggered;

                  Widget.Hold_Off_Counter :=
                    Natural (Float (Widget.Buffer'Length) * Widget.Hold_Off);
               end if;

            when Triggered =>

               Widget.Buffer (Widget.Ring_Ptr) := Frame;
               Widget.Ring_Ptr := Widget.Ring_Ptr + 1;

               if Widget.Ring_Ptr >= Widget.Points_To_Display then
                  Widget.Ring_Ptr := 0;
                  Widget.Queue_Draw;

                  Widget.State := Hold_Off;
               end if;

            when Hold_Off =>
               if Widget.Hold_Off_Counter > 0 then
                  Widget.Hold_Off_Counter := Widget.Hold_Off_Counter - 1;
               else

                  if Widget.Mode = Single then
                     Widget.State := Stopped;
                  else
                     Widget.State := Waiting_For_Trigger;
                  end if;
               end if;

            when Stopped =>
               null;

            end case;

      end case;
   end Push_Frame;

   --------------
   -- Set_Zoom --
   --------------

   procedure Set_Zoom (Widget : in out Oscilloscope_Record;
                       Zoom   :        Float)
   is
   begin
      Widget.Zoom := Float'Max (Zoom, 1.0);

      Widget.Points_To_Display :=
        Natural (Float (Widget.Buffer'Length) / Widget.Zoom);

      Widget.Queue_Draw;
   end Set_Zoom;

   ------------------
   -- Set_Hold_Off --
   ------------------

   procedure Set_Hold_Off (Widget   : in out Oscilloscope_Record;
                           Hold_Off :        Float)
   is
   begin
      Widget.Hold_Off := Hold_Off;
   end Set_Hold_Off;

   --------------
   -- Get_Type --
   --------------

   function Get_Type return Glib.GType is
   begin
      Initialize_Class_Record
        (Ancestor     => Gtk.Table.Get_Type,
         Class_Record => Class,
         Type_Name    => "Oscilloscope");
      return Class.The_Type;
   end Get_Type;

   ----------
   -- Draw --
   ----------

   function Draw
     (Widget    : access Gtk.Drawing_Area.Gtk_Drawing_Area_Record'Class;
      Context   : Cairo.Cairo_Context;
      Osc       : Oscilloscope)
      return Boolean
   is
      use Cairo;
      Area   : Gdk_Rectangle;

      Nbr_Of_Points : constant Natural := Osc.Points_To_Display;
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

         function Lvl_To_Y (Lvl : Float) return Gdouble
         is (Bot - (1.0 + Gdouble (Lvl)) * Height / 2.0);

      begin
         Move_To (Context, Left, Top + 5.0);
         Show_Text (Context, "Mode: " & Osc.Mode'Img);
         Move_To (Context, Left, Top + 15.0);
         Show_Text (Context, "Trigger: " & Osc.State'Img);

         Set_Line_Width (Context, 0.5);

         --  Zero line
         Set_Source_Rgba (Context, 0.0, 0.0, 0.0, 0.5);
         Move_To (Context, Left, Top + Height / 2.0);
         Line_To (Context, Right, Top + Height / 2.0);
         Stroke (Context);

         Set_Source_Rgba (Context, 1.0, 0.0, 0.0, 1.0);
         Move_To (Context,
                  Left,
                  Lvl_To_Y (Osc.Buffer (Osc.Buffer'First)));

         for X in Osc.Buffer'First ..
           Osc.Buffer'First + Nbr_Of_Points - 1
         loop
            Line_To (Context,
                     Left + (Gdouble (X) / Gdouble (Nbr_Of_Points)) * Width,
                     Lvl_To_Y (Osc.Buffer (X)));
         end loop;
         Stroke (Context);

         --  Trigger level
         Set_Line_Width (Context, 1.0);
         Set_Source_Rgba (Context, 0.0, 0.0, 1.0, 0.5);
         Move_To (Context, Left, Lvl_To_Y (Osc.Trigger_Level));
         Line_To (Context, Left + 20.0, Lvl_To_Y (Osc.Trigger_Level));
         Stroke (Context);
      end;
      return True;
   end Draw;

   ------------------------
   -- On_Trig_Lvl_Change --
   ------------------------

   procedure On_Trig_Lvl_Change
     (Widget    : access Gtk.Scale.Gtk_Scale_Record'Class;
      Osc       : Oscilloscope)
   is
   begin
      Osc.Trigger_Level := Float (Widget.Get_Value);
      Osc.Queue_Draw;
   end On_Trig_Lvl_Change;

   --------------------
   -- On_Zoom_Change --
   --------------------

   procedure On_Zoom_Change
     (Widget    : access Gtk.Scale.Gtk_Scale_Record'Class;
      Osc       : Oscilloscope)
   is
   begin
      Osc.Set_Zoom (Float (Widget.Get_Value));
   end On_Zoom_Change;

end Nael.Lab_GUI.Oscilloscope_Widget;
