with Glib.Object; use Glib.Object;
with Glib; use Glib;

with Gtk.Button; use Gtk.Button;
with Gtk.Enums; use Gtk.Enums;

package body Nael.Lab_GUI.Keyboard_Widget is

   Class : Ada_GObject_Class := Uninitialized_Class;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Widget  :      out Keyboard;
                      MIDI_Ex : not null MIDI_Exchange.Any_Access)
   is
   begin
      Widget := new Keyboard_Record;
      Keyboard_Widget.Initialize (Widget, MIDI_Ex);
   end Gtk_New;

   ------------------
   -- Key_On_Press --
   ------------------

   procedure Key_On_Press (Self : access Gtk_Button_Record'Class) is
      use MIDI;
      use Nael.MIDI_Exchange;

      K : constant Key_Range := Key_Range_User_Data.Get (Self, "key");
      Widget : constant Keyboard :=
        Keyboard_User_Data.Get (Self, "keyboard");
   begin
      if Widget.MIDI_Ex /= null then
         Widget.MIDI_Ex.Push ((Kind => Note_On,
                               Chan => 0,
                               Key  => Widget.Key_To_MIDI (K),
                               Velocity => 127));
      end if;
   end Key_On_Press;

   --------------------
   -- Key_On_Release --
   --------------------

   procedure Key_On_Release (Self : access Gtk_Button_Record'Class) is
      use MIDI;
      use Nael.MIDI_Exchange;

      K : constant Key_Range := Key_Range_User_Data.Get (Self, "key");
      Widget : constant Keyboard :=
        Keyboard_User_Data.Get (Self, "keyboard");
   begin
      if Widget.MIDI_Ex /= null then
         Widget.MIDI_Ex.Push ((Kind => Note_Off,
                               Chan => 0,
                               Key  => Widget.Key_To_MIDI (K),
                               Velocity => 127));
      end if;
   end Key_On_Release;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Widget  : not null access Keyboard_Record'Class;
      MIDI_Ex : not null        MIDI_Exchange.Any_Access)
   is
   begin
      G_New (Widget, Keyboard_Widget.Get_Type);
      Gtk.Table.Initialize
        (Widget,
         Rows        => 2,
         Columns     => Nbr_White_Keys * 2,
         Homogeneous => True);

      Widget.MIDI_Ex := MIDI_Ex;

      for K in Key_Range loop
         declare
            Key : Gtk_Button renames Widget.Keys (K);
         begin
            Gtk_New (Key);

            Key_Range_User_Data.Set (Key, K, "key");
            Keyboard_User_Data.Set (Key, Widget.all'Unchecked_Access,
                                    "keyboard");

            Key.On_Pressed (Key_On_Press'Access);
            Key.On_Released (Key_On_Release'Access);
         end;
      end loop;

      for Oct in 0 .. Octaves - 1 loop
         declare
            Keys : Key_Array renames Widget.Keys;
            C : constant Key_Range := Key_Range (1 + Oct * 12);

            I : constant Guint := Guint (Oct * White_Keys_Per_Octave * 2);
            --  Table column index

         begin
            Widget.Attach (Keys (C + 00), I + 00, I + 02, 1, 2); -- C
            Widget.Attach (Keys (C + 02), I + 02, I + 04, 1, 2); -- D
            Widget.Attach (Keys (C + 04), I + 04, I + 06, 1, 2); -- E
            Widget.Attach (Keys (C + 05), I + 06, I + 08, 1, 2); -- F
            Widget.Attach (Keys (C + 07), I + 08, I + 10, 1, 2); -- G
            Widget.Attach (Keys (C + 09), I + 10, I + 12, 1, 2); -- A
            Widget.Attach (Keys (C + 11), I + 12, I + 14, 1, 2); -- B

            Widget.Attach (Keys (C + 01), I + 01, I + 03, 0, 1); -- C#
            Widget.Attach (Keys (C + 03), I + 03, I + 05, 0, 1); -- D#
            Widget.Attach (Keys (C + 06), I + 07, I + 09, 0, 1); -- G#
            Widget.Attach (Keys (C + 08), I + 09, I + 11, 0, 1); -- A#
            Widget.Attach (Keys (C + 10), I + 11, I + 13, 0, 1); -- B#
         end;
      end loop;
   end Initialize;

   -----------------
   -- Key_To_MIDI --
   -----------------

   function Key_To_MIDI (Widget : in out Keyboard_Record'Class;
                         K      :        Key_Range)
                         return MIDI.MIDI_Key
   is
      pragma Unreferenced (Widget);
      use MIDI;
   begin
      return MIDI.C3 + MIDI.MIDI_Key (K);
   end Key_To_MIDI;

   --------------
   -- Get_Type --
   --------------

   function Get_Type return Glib.GType is
   begin
      Initialize_Class_Record
        (Ancestor     => Gtk.Table.Get_Type,
         Class_Record => Class,
         Type_Name    => "Keyboard");
      return Class.The_Type;
   end Get_Type;

end Nael.Lab_GUI.Keyboard_Widget;
