with Gtk.Widget; use Gtk.Widget;

with Nael.MIDI_Exchange;

private with Glib;
private with Glib.Main;
private with Glib.Object;
private with Gtk.Box;
private with Gtk.Toggle_Button;
private with Gtk.Handlers;
private with Gtk.Scale;
private with Gtk.Drawing_Area;
private with Gtk.Scrolled_Window;
private with MIDI;
private with Cairo;

private with Ada.Real_Time;

private package Nael.Lab_GUI.Pianoroll_Widget is

   type Pianoroll_Record
   is new Gtk_Widget_Record
   with private;

   type Pianoroll is access all Pianoroll_Record'Class;

   procedure Gtk_New (Widget  :      out Pianoroll;
                      MIDI_Ex : not null MIDI_Exchange.Any_Access);

   procedure Initialize
     (Widget  : not null access Pianoroll_Record'Class;
      MIDI_Ex : not null        MIDI_Exchange.Any_Access);

private

   Nbr_Steps : constant := 16;
   Nbr_Keys  : constant := 128;

   Cell_Height_Request : constant := 15;
   Cell_Width_Request : constant := 25;
   Line_Width : constant := 1.0;

   type Step_Range is mod Nbr_Steps;
   subtype Key_Range is MIDI.MIDI_Key;

   type Key_State is array (Key_Range) of Boolean;
   type State_Matrix is array (Step_Range) of Key_State;

   type Pianoroll_Record
   is new Gtk.Box.Gtk_Vbox_Record
   with record
      State : State_Matrix := (others => (others => False));
      Notes_On : Key_State := (others => False);

      MIDI_Ex : MIDI_Exchange.Any_Access := null;

      Scroll : Gtk.Scrolled_Window.Gtk_Scrolled_Window;
      Scroll_To_Center : Boolean := True;
      DA : Gtk.Drawing_Area.Gtk_Drawing_Area;
      Surface : Cairo.Cairo_Surface := Cairo.Null_Surface;
      Surface_Width, Surface_Height : Glib.Gint;

      Play : Gtk.Toggle_Button.Gtk_Toggle_Button;
      BPM_Scale : Gtk.Scale.Gtk_Scale;
      BPM : Positive := 110;
      Next_Trigger : Ada.Real_Time.Time := Ada.Real_Time.Time_Last;
      Next_Step    : Step_Range := Step_Range'First;
   end record;

   package Widget_Callback
   is new Gtk.Handlers.Callback (Pianoroll_Record);

   package Pianoroll_Source
   is new Glib.Main.Generic_Sources (Pianoroll);

   function Get_Type return Glib.GType;

   package Pianoroll_User_Data is new Glib.Object.User_Data (Pianoroll);

   procedure Full_Redraw (Widget : in out Pianoroll_Record'Class);

   procedure Draw_Step (Widget : in out Pianoroll_Record'Class;
                        Cr     :        Cairo.Cairo_Context;
                        Step   :        Step_Range;
                        Key    :        Key_Range;
                        State  :        Boolean);

end Nael.Lab_GUI.Pianoroll_Widget;
