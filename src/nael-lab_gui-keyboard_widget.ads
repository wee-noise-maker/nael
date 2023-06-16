with Gtk.Widget; use Gtk.Widget;

with Nael.MIDI_Exchange;

private with Glib;
private with Gtk.Drawing_Area;
private with MIDI;

private package Nael.Lab_GUI.Keyboard_Widget is

   type Keyboard_Record
   is new Gtk_Widget_Record
   with private;

   type Keyboard is access all Keyboard_Record'Class;

   procedure Gtk_New (Widget  :      out Keyboard;
                      MIDI_Ex : not null MIDI_Exchange.Any_Access);

   procedure Initialize
     (Widget  : not null access Keyboard_Record'Class;
      MIDI_Ex : not null        MIDI_Exchange.Any_Access);

private

   Octaves : constant := 4;
   White_Keys_Per_Octave : constant := 7;
   Black_Keys_Per_Octave : constant := 5;
   Nbr_White_Keys : constant := Octaves * White_Keys_Per_Octave;
   Nbr_Black_Keys : constant := Octaves * Black_Keys_Per_Octave;

   type Keyboard_Record
   is new Gtk.Drawing_Area.Gtk_Drawing_Area_Record
   with record
      Last_On : MIDI.MIDI_Key := 0;

      MIDI_Ex : MIDI_Exchange.Any_Access := null;

      Width : Integer := 0;
      Height : Integer := 0;
   end record;

   function Get_Type return Glib.GType;

end Nael.Lab_GUI.Keyboard_Widget;
