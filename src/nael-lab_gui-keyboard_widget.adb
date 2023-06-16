with Glib.Object; use Glib.Object;
with Glib; use Glib;
with Cairo;

with Gdk.Event;
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

   ----------
   -- Draw --
   ----------

   function Draw (Self : access Gtk_Widget_Record'Class;
                  Cr   : Cairo.Cairo_Context)
                  return Boolean
   is
      use Cairo;

      Widget : Keyboard_Record renames Keyboard_Record (Self.all);

      Width       : constant Gdouble := Gdouble (Self.Get_Allocated_Width);
      Height      : constant Gdouble := Gdouble (Self.Get_Allocated_Height);
      Key_Width   : constant Gdouble := Width / Gdouble (Nbr_White_Keys);

   begin
      Widget.Width := Integer (Width);
      Widget.Height := Integer (Height);

      --  White keys
      for K in 0 .. Nbr_White_Keys - 1 loop
         Rectangle (Cr,
                    X      =>  Gdouble (K) * Key_Width,
                    Y      =>  0.0,
                    Width  =>  Key_Width,
                    Height => Height);
         Stroke (Cr);
      end loop;

      --  Black keys
      for Oct in 0 .. Octaves - 1 loop
         declare
            Oct_Offset : constant Gdouble :=
              Key_Width * Gdouble (White_Keys_Per_Octave * Oct);
         begin
            for K in 0 .. Black_Keys_Per_Octave - 1 loop
               declare
                  Key_Offset : constant Gdouble :=
                    (case K is
                        when 0      => 0.75 * Key_Width,
                        when 1      => 1.75 * Key_Width,
                        when 2      => 3.75 * Key_Width,
                        when 3      => 4.75 * Key_Width,
                        when others => 5.75 * Key_Width);
               begin
                  Rectangle (Cr,
                             X      =>  Oct_Offset + Key_Offset,
                             Y      =>  0.0,
                             Width  => Key_Width / 2.0,
                             Height => Height / 2.0);
                  Cairo.Fill (Cr);
               end;
            end loop;
         end;
      end loop;

      return True;
   end Draw;

   ------------------------
   -- On_DA_Button_Press --
   ------------------------

   function On_DA_Button_Press
     (Self  : access Gtk_Widget_Record'Class;
      Event : Gdk.Event.Gdk_Event_Button) return Boolean
   is
      use Gdk.Event;
      use MIDI;
      use Nael.MIDI_Exchange;

      Widget : Keyboard_Record renames Keyboard_Record (Self.all);

      Octave_Width : constant Integer := Widget.Width / Octaves;

      X : constant Integer := Integer (Event.X);
      Y : constant Integer := Integer (Event.Y);
      Oct : constant Integer := X / Octave_Width;

      Oct_Offset : constant Gdouble := Gdouble (X mod Octave_Width);
      Key_Width   : constant Gdouble :=
        Gdouble (Octave_Width / White_Keys_Per_Octave);

      Key : Integer := -1;

      Velocity : constant MIDI.MIDI_Data := (case Event.Button is
                                                when 1 => 127,
                                                when 2 => 127 / 2,
                                                when others => 127 / 3);
      --  Different velocity depending on which mouse button is pressed

   begin

      if Widget.MIDI_Ex = null then
         return False;
      end if;

      --  Note off for the last key played. Doesn't matter if this is a button
      --  press or release since even for a press we have to terminate the
      --  previous note before starting a new one.
      if Widget.Last_On /= 0 then
         Widget.MIDI_Ex.Push ((Kind     => Note_Off,
                               Chan     => 0,
                               Key      => Widget.Last_On,
                               Velocity => Velocity));
         Widget.Last_On := 0;

      end if;

      case Event.The_Type is
         when Button_Press =>

            --  Check if cursor is on a black key
            if Y < Widget.Height / 2 then
               case Natural ((Oct_Offset / Key_Width) * 100.0) is
               when 075 .. 125 =>
                  Key := 1;
               when 175 .. 225 =>
                  Key := 3;
               when 375 .. 425 =>
                  Key := 6;
               when 475 .. 525 =>
                  Key := 8;
               when 575 .. 625 =>
                  Key := 10;
               when others =>
                  null;
               end case;
            end if;

            --  It's not a black key
            if Key = -1 then
               case Natural ((Oct_Offset / Key_Width) * 100.0) is
               when 000 .. 100 =>
                  Key := 0;
               when 101 .. 200 =>
                  Key := 2;
               when 201 .. 300 =>
                  Key := 4;
               when 301 .. 400 =>
                  Key := 5;
               when 401 .. 500 =>
                  Key := 7;
               when 501 .. 600 =>
                  Key := 9;
               when others =>
                  Key := 11;
               end case;
            end if;

            Widget.Last_On := MIDI.C2 + MIDI_Key (Key + Oct * 12);

            if Widget.MIDI_Ex /= null then
               Widget.MIDI_Ex.Push ((Kind     => Note_On,
                                     Chan     => 0,
                                     Key      => Widget.Last_On,
                                     Velocity => Velocity));
            end if;

         when others =>
            null;
      end case;

      return True;
   end On_DA_Button_Press;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Widget  : not null access Keyboard_Record'Class;
      MIDI_Ex : not null        MIDI_Exchange.Any_Access)
   is
      use Gdk.Event;

   begin
      G_New (Widget, Keyboard_Widget.Get_Type);
      Gtk.Drawing_Area.Initialize (Widget);

      Widget.Set_Size_Request (-1, 100);
      Widget.On_Draw (Draw'Access);

      --  Mouse click events
      Widget.Set_Events (Widget.Get_Events
                         or Gdk.Event.Button_Press_Mask
                         or Gdk.Event.Button_Release_Mask);
      Widget.On_Button_Press_Event (On_DA_Button_Press'Access);
      Widget.On_Button_Release_Event (On_DA_Button_Press'Access);

      Widget.MIDI_Ex := MIDI_Ex;

   end Initialize;

   --------------
   -- Get_Type --
   --------------

   function Get_Type return Glib.GType is
   begin
      Initialize_Class_Record
        (Ancestor     => Gtk.Drawing_Area.Get_Type,
         Class_Record => Class,
         Type_Name    => "Keyboard");
      return Class.The_Type;
   end Get_Type;

end Nael.Lab_GUI.Keyboard_Widget;
