with Gtk.Widget; use Gtk.Widget;

with Nael.MIDI_Exchange;

private with Glib;
private with Glib.Object;
private with Gtk.Table;
private with Gtk.Button;
private with Gtk.Handlers;
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
   Nbr_White_Keys : constant := Octaves * White_Keys_Per_Octave;

   type Key_Range is range 1 .. 12 * Octaves;
   type Key_Array is array (Key_Range) of Gtk.Button.Gtk_Button;

   type Keyboard_Record
   is new Gtk.Table.Gtk_Table_Record
   with record
      Keys : Key_Array;
      MIDI_Ex : MIDI_Exchange.Any_Access := null;
   end record;

   function Key_To_MIDI (Widget : in out Keyboard_Record'Class;
                         K      :        Key_Range)
                         return MIDI.MIDI_Key;

   package Widget_Callback
   is new Gtk.Handlers.Callback (Keyboard_Record);

   function Get_Type return Glib.GType;

   package Key_Range_User_Data is new Glib.Object.User_Data (Key_Range);
   package Keyboard_User_Data is new Glib.Object.User_Data (Keyboard);

end Nael.Lab_GUI.Keyboard_Widget;
