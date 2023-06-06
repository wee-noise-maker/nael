with Ada.Numerics.Complex_Arrays;

with Gtk.Widget; use Gtk.Widget;

with Nael.Frame_Exchange;

private with Gtk.Marshallers;
private with Gtk.Table;
private with Gtk.Drawing_Area;
private with Gtk.Scale;
private with Cairo;
private with Glib;
private with Gtk.Handlers;
private with Nael.FFT;

private package Nael.Lab_GUI.Oscilloscope_Widget is

   type Oscilloscope_Record
   is new Gtk_Widget_Record
   with private;

   type Oscilloscope is access all Oscilloscope_Record'Class;

   procedure Gtk_New (Widget      :    out Oscilloscope;
                      Sample_Rate : in     Natural);

   procedure Initialize
     (Widget      : not null access Oscilloscope_Record'Class;
      Sample_Rate : in              Natural);

   procedure Push_Frame (Widget : in out Oscilloscope_Record;
                         Frame  :        Mono_Frame);

   procedure Set_Zoom (Widget : in out Oscilloscope_Record;
                       Zoom   :        Float);

   procedure Set_Hold_Off (Widget   : in out Oscilloscope_Record;
                           Hold_Off :        Float);

private

   type Osc_Mode is (Live, Continous, Single);
   type Trigger_State is (Waiting_For_Trigger,
                          Triggered,
                          Hold_Off,
                          Stopped);

   type Oscilloscope_Record
   is new Gtk.Table.Gtk_Table_Record
   with record
      Mode  : Osc_Mode  := Continous;
      State : Trigger_State := Waiting_For_Trigger;

      Trigger_Level    : Float := 0.5;
      Prev_Frame       : Mono_Frame := 0.0;
      Hold_Off         : Float := 0.4;
      Hold_Off_Counter : Natural := 7_000;

      Sample_Rate : Natural;
      Buffer      : Block (0 .. 44_100 - 1);
      Ring_Ptr    : Natural := 0;

      Zoom : Float := 150.0;
      Points_To_Display : Natural := 44_100 / 150;

      DA : Gtk.Drawing_Area.Gtk_Drawing_Area;
      Trig_Lvl_Scale : Gtk.Scale.Gtk_Scale;
      Zoom_Scale : Gtk.Scale.Gtk_Scale;
   end record;
   function Get_Type return Glib.GType;

   package DA_Return_Handlers
   is new Gtk.Handlers.User_Return_Callback
     (Gtk.Drawing_Area.Gtk_Drawing_Area_Record,
      Boolean,
      Oscilloscope);

   --  There is no Context_Marshaller in Gtk.Handlers.User_Return_Callback, so
   --  we create one for the "draw" callback on Drawing_Area.
   package Context_Marshaller
   is new DA_Return_Handlers.Marshallers.Generic_Marshaller
     (Cairo.Cairo_Context, Cairo.Get_Context);

   function Draw
     (Widget    : access Gtk.Drawing_Area.Gtk_Drawing_Area_Record'Class;
      Context   : Cairo.Cairo_Context;
      Osc       : Oscilloscope)
      return Boolean;

   package Scale_Handlers
   is new Gtk.Handlers.User_Callback
     (Gtk.Scale.Gtk_Scale_Record,
      Oscilloscope);

   procedure On_Trig_Lvl_Change
     (Widget    : access Gtk.Scale.Gtk_Scale_Record'Class;
      Osc       : Oscilloscope);

   procedure On_Zoom_Change
     (Widget    : access Gtk.Scale.Gtk_Scale_Record'Class;
      Osc       : Oscilloscope);

end Nael.Lab_GUI.Oscilloscope_Widget;
