with Ada.Unchecked_Deallocation;

with MIDI; use MIDI;

with Glib.Object; use Glib.Object;
with Glib; use Glib;
with Gdk.Window;
with Gdk.Event;

with Gtk.Box;             use Gtk.Box;
with Gtk.Frame;           use Gtk.Frame;
with Gtk.Toggle_Button;   use Gtk.Toggle_Button;
with Gtk.Enums;           use Gtk.Enums;
with Gtk.Drawing_Area;    use Gtk.Drawing_Area;
with Gtk.Scrolled_Window; use Gtk.Scrolled_Window;
with Gtk.Scale;           use Gtk.Scale;
with Cairo;             use Cairo;
with Cairo.Surface;

package body Nael.Lab_GUI.Pianoroll_Widget is

   Is_Black_Key : constant Key_State :=
     (Cs1 - 24 | Cs1 - 12 | Cs1 | Cs2 | Cs3 | Cs4 | Cs5 | Cs6 | Cs7 | Cs8 |
      Cs8 + 12 |

      Ds1 - 24 | Ds1 - 12 | Ds1 | Ds2 | Ds3 | Ds4 | Ds5 | Ds6 | Ds7 | Ds8 |
      Ds8 + 12 |

      Fs1 - 24 | Fs1 - 12 | Fs1 | Fs2 | Fs3 | Fs4 | Fs5 | Fs6 | Fs7 | Fs8 |
      Fs8 + 12 |

      Gs1 - 24 | Gs1 - 12 | Gs1 | Gs2 | Gs3 | Gs4 | Gs5 | Gs6 | Gs7 | Gs8 |

      As0 - 12 | As0 | As1 | As2 | As3 | As4 | As5 | As6 | As7 | As8
      => True,
      others => False);

   Class : Ada_GObject_Class := Uninitialized_Class;

   --------------------
   -- Sequencer_Task --
   --------------------

   task body Sequencer_Task is
      use Ada.Real_Time;

      PR : Pianoroll := null;

      Next_Trigger : Ada.Real_Time.Time := Ada.Real_Time.Time_Last;
      Next_Step    : Step_Range := Step_Range'First;
      Notes_On : Key_State := (others => False);

      Exit_Request : Boolean := False;
   begin
      accept Start (PR : not null Pianoroll) do
         Sequencer_Task.PR := PR;
      end Start;

      Next_Trigger := Clock;

      while not Exit_Request loop
         select
            delay until Next_Trigger;
         or
            accept Stop do
               Exit_Request := True;
            end Stop;
         end select;

         Next_Trigger := Next_Trigger +
           Milliseconds ((60 * 1000) / (PR.BPM * 4));

         for Key in Key_Range loop
            if Notes_On (Key) then
               PR.MIDI_Ex.Push ((Kind => Note_Off,
                                 Chan => 0,
                                 Key  => Key,
                                 Velocity => 127));
            end if;
         end loop;

         if PR.Playing then

            Notes_On := PR.State (Next_Step);

            for Key in Key_Range loop
               if Notes_On (Key) then
                  PR.MIDI_Ex.Push ((Kind => Note_On,
                                    Chan => 0,
                                    Key  => Key,
                                    Velocity => 127));
               end if;
            end loop;

            Next_Step := Next_Step + 1;
         end if;
      end loop;
   end Sequencer_Task;

   ----------------
   -- On_DA_Draw --
   ----------------

   function On_DA_Draw
     (Self : access Glib.Object.GObject_Record'Class;
      Cr   : Cairo.Cairo_Context)
      return Boolean
   is
   begin
      if Self.all in Pianoroll_Record'Class then
         Set_Source_Surface (Cr,
                             Pianoroll_Record (Self.all).Surface,
                             0.0, 0.0);
         Paint (Cr);
      end if;
      return True;
   end On_DA_Draw;

   ------------------------
   -- On_DA_Button_Press --
   ------------------------

   function On_DA_Button_Press
     (Self  : access Glib.Object.GObject_Record'Class;
      Event : Gdk.Event.Gdk_Event_Button)
      return Boolean
   is
      use Gdk.Event;

      Widget : Pianoroll_Record renames Pianoroll_Record (Self.all);

      Width       : constant Integer := Integer (Widget.Surface_Width);
      Height      : constant Integer := Integer (Widget.Surface_Height);
      Cell_Width  : constant Integer := Integer (Width / (Nbr_Steps + 1));
      Cell_Height : constant Integer := Integer (Height / (Nbr_Keys));
   begin
      if Event.The_Type = Button_Press then
         declare

            X : constant Integer := Integer (Event.X);
            Y : constant Integer := Integer (Event.Y);

            Step : constant Integer := (X / Cell_Width) - 1;
            Key : constant Integer := (Y / Cell_Height) - 1;
         begin
            if Step in Integer (Step_Range'First) .. Integer (Step_Range'Last)
              and then
                Key in Integer (Key_Range'First) .. Integer (Key_Range'Last)
            then
               declare
                  Cr : Cairo_Context;

                  S : constant Step_Range := Step_Range (Step);
                  K : constant Key_Range := Key_Range (Key);
               begin
                  Widget.State (S)(K) := not Widget.State (S)(K);

                  Cr := Create (Widget.Surface);
                  Draw_Step (Widget, Cr, S, K, Widget.State (S)(K));

                  Widget.DA.Queue_Draw;
               end;
            end if;
         end;
      end if;
      return True;
   end On_DA_Button_Press;

   ----------------------
   -- On_Size_Allocate --
   ----------------------

   procedure On_Size_Allocate
     (Self       : access Glib.Object.GObject_Record'Class;
      Allocation : Gtk_Allocation)
   is
      Widget : Pianoroll_Record renames Pianoroll_Record (Self.all);
      Surface : Cairo_Surface renames Widget.Surface;
   begin
      if Surface /= Null_Surface then
         Cairo.Surface.Destroy (Surface);
      end if;

      Widget.Surface_Width := Allocation.Width;
      Widget.Surface_Height := Allocation.Height;

      Surface := Gdk.Window.Create_Similar_Surface (Widget.Get_Window,
                                                    Cairo_Content_Color,
                                                    Allocation.Width,
                                                    Allocation.Height);

      Full_Redraw (Pianoroll_Record (Self.all));

      --  On the first rezise, scroll to the middle of the roll
      if Widget.Scroll_To_Center then
         Widget.Scroll.Get_Vadjustment.Set_Value
           (Gdouble (Allocation.Height) / 5.0);
         Widget.Scroll_To_Center := False;
      end if;
   end On_Size_Allocate;

   ------------------
   -- BPM_Callback --
   ------------------

   procedure BPM_Callback  (Self : access Glib.Object.GObject_Record'Class) is
      Widget : Pianoroll_Record renames Pianoroll_Record (Self.all);
   begin
      Widget.BPM := Positive (Widget.BPM_Scale.Get_Value);
   end BPM_Callback;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Widget  :      out Pianoroll;
                      MIDI_Ex : not null MIDI_Exchange.Any_Access)
   is
   begin
      Widget := new Pianoroll_Record;
      Pianoroll_Widget.Initialize (Widget, MIDI_Ex);
   end Gtk_New;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Self : access Gtk_Widget_Record'Class) is
      Widget : Pianoroll_Record renames Pianoroll_Record (Self.all);

      procedure Free
      is new Ada.Unchecked_Deallocation (Sequencer_Task,
                                         Sequencer_Task_Access);
   begin
      if Widget.Seq_Task /= null then
         Widget.Seq_Task.Stop;

         while not Widget.Seq_Task'Terminated loop
            delay 0.001;
         end loop;

         Free (Widget.Seq_Task);

      end if;
   end Destroy;

   ---------------------
   -- On_Play_Toggled --
   ---------------------

   procedure On_Play_Toggled (Self : access Glib.Object.GObject_Record'Class)
   is
      Widget : Pianoroll_Record renames Pianoroll_Record (Self.all);
   begin
      Widget.Playing := Widget.Play.Get_Active;
   end On_Play_Toggled;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Widget  : not null access Pianoroll_Record'Class;
      MIDI_Ex : not null        MIDI_Exchange.Any_Access)
   is
      Frame : Gtk_Frame;
      Control_Hbox : Gtk_Hbox;
   begin
      G_New (Widget, Pianoroll_Widget.Get_Type);
      Gtk.Box.Initialize_Vbox (Widget, Homogeneous => False);

      Gtk_New_Hbox (Control_Hbox, Homogeneous => False);
      Widget.Pack_Start (Control_Hbox, Expand => False);

      Gtk_New (Widget.Play, "Play");
      Widget.Play.On_Toggled (On_Play_Toggled'Access, Widget.all'Access);
      Control_Hbox.Pack_Start (Widget.Play, Expand => False);

      Gtk_New (Frame, "BPM");
      Control_Hbox.Add (Frame);
      Gtk_New_With_Range (Widget.BPM_Scale,
                          Gtk.Enums.Orientation_Horizontal,
                          50.0, 150.0, 1.0);
      Widget.BPM_Scale.Set_Value (Gdouble (Widget.BPM));
      Widget.BPM_Scale.On_Value_Changed (BPM_Callback'Access, Widget);
      Frame.Add (Widget.BPM_Scale);

      Gtk_New (Widget.Scroll);
      Widget.Scroll.Set_Policy (Hscrollbar_Policy => Gtk.Enums.Policy_Never,
                                Vscrollbar_Policy => Gtk.Enums.Policy_Always);
      Widget.Pack_Start (Widget.Scroll, Expand => True);

      Gtk_New (Widget.DA);
      Widget.DA.Set_Size_Request (Nbr_Steps * Cell_Width_Request,
                                  Nbr_Keys * Cell_Height_Request);
      Widget.DA.Set_Vexpand (True);
      Widget.DA.Set_Hexpand (True);
      Widget.Scroll.Add (Widget.DA);

      --  Redraw events
      Widget.DA.On_Draw (On_DA_Draw'Access, Widget);

      --  Mouse click events
      declare
         use Gdk.Event;
      begin
         Widget.DA.Set_Events (Widget.DA.Get_Events
                               or Gdk.Event.Button_Press_Mask
                               or Gdk.Event.Button_Motion_Mask);
         Widget.DA.On_Button_Press_Event (On_DA_Button_Press'Access, Widget);
      end;

      Widget.DA.On_Size_Allocate (On_Size_Allocate'Access, Widget);

      Widget.MIDI_Ex := MIDI_Ex;

      Widget.State (0)(MIDI.C2) := True;
      Widget.State (4)(MIDI.C2) := True;
      Widget.State (8)(MIDI.C2) := True;
      Widget.State (12)(MIDI.C2) := True;

      Widget.Seq_Task := new Sequencer_Task;
      Widget.Seq_Task.Start (Widget.all'Access);

      Widget.On_Destroy (Destroy'Access);
   end Initialize;

   --------------
   -- Get_Type --
   --------------

   function Get_Type return Glib.GType is
   begin
      Initialize_Class_Record
        (Ancestor     => Gtk.Box.Get_Vbox_Type,
         Class_Record => Class,
         Type_Name    => "Pianoroll");
      return Class.The_Type;
   end Get_Type;

   ---------------
   -- Draw_Step --
   ---------------

   procedure Draw_Step (Widget : in out Pianoroll_Record'Class;
                        Cr     :        Cairo.Cairo_Context;
                        Step   :        Step_Range;
                        Key    :        Key_Range;
                        State  :        Boolean)
   is
      Width       : constant Gdouble := Gdouble (Widget.Surface_Width);
      Height      : constant Gdouble := Gdouble (Widget.Surface_Height);

      Cell_Width  : constant Gdouble :=
        Gdouble (Width / Gdouble (Nbr_Steps + 1));

      Cell_Height : constant Gdouble := Gdouble (Height / Gdouble (Nbr_Keys));

      Margin : constant := 1.0 * Line_Width;

   begin
      if State then
         Set_Source_Rgb (Cr, 0.0, 1.0, 0.0);
      else
         if Is_Black_Key (Key) then
            Set_Source_Rgb (Cr, 0.8, 0.8, 0.8);
         else
            Set_Source_Rgb (Cr, 1.0, 1.0, 1.0);
         end if;
      end if;

      Rectangle (Cr,
                 X      => (Gdouble (Step) + 1.0) * Cell_Width + Margin,
                 Y      => (Gdouble (Key) + 1.0) * Cell_Height + Margin,
                 Width  => Cell_Width - 2.0 * Margin,
                 Height => Cell_Height - 2.0 * Margin);
      Cairo.Fill (Cr);
   end Draw_Step;

   -----------------
   -- Full_Redraw --
   -----------------

   procedure Full_Redraw (Widget : in out Pianoroll_Record'Class) is
      Cr : Cairo_Context;
      Width  : constant Gdouble := Gdouble (Widget.Surface_Width);
      Height : constant Gdouble := Gdouble (Widget.Surface_Height);

      Cell_Width  : constant Gdouble :=
        Gdouble (Width / Gdouble (Nbr_Steps + 1));
      Cell_Height : constant Gdouble := Gdouble (Height / Gdouble (Nbr_Keys));

   begin
      if Widget.Surface /= Null_Surface then

         Cr := Create (Widget.Surface);

         --  Clear all
         Set_Source_Rgb (Cr, 1.0, 1.0, 1.0);
         Paint (Cr);

         --  Draw keyboard
         for Key in Key_Range loop
            if Is_Black_Key (Key) then
               Set_Source_Rgb (Cr, 0.0, 0.0, 0.0);
            else
               Set_Source_Rgb (Cr, 1.0, 1.0, 1.0);
            end if;

            Rectangle (Cr,
                       X      => 0.0,
                       Y      => (Gdouble (Key) + 1.0) * Cell_Height,
                       Width  => Cell_Width,
                       Height => Cell_Height);
            Cairo.Fill (Cr);

            if Is_Black_Key (Key) then
               Set_Source_Rgb (Cr, 0.8, 0.8, 0.8);

               Rectangle (Cr,
                          X      => Cell_Width,
                          Y      => (Gdouble (Key) + 1.0) * Cell_Height,
                          Width  => Width,
                          Height => Cell_Height);
               Cairo.Fill (Cr);

               Set_Source_Rgb (Cr, 1.0, 1.0, 1.0);
            else
               Set_Source_Rgb (Cr, 0.0, 0.0, 0.0);
            end if;

            Move_To (Cr, 0.0, (Gdouble (Key) + 1.7) * Cell_Height);
            Show_Text (Cr, Key_Img (Key));
         end loop;

         --  Draw the grid
         Save (Cr);
         Set_Source_Rgba (Cr, 0.0, 0.0, 0.0, 0.5);
         Set_Line_Width (Cr, Line_Width);
         for Step in 0 .. Nbr_Steps loop
            Move_To (Cr, Gdouble (Step) * Cell_Width, 0.0);
            Line_To (Cr, Gdouble (Step) * Cell_Width, Height);
            Stroke (Cr);
         end loop;
         for Key in 0 .. Nbr_Keys loop
            Move_To (Cr, 0.0, Gdouble (Key) * Cell_Height);
            Line_To (Cr, Width,  Gdouble (Key) * Cell_Height);
            Stroke (Cr);
         end loop;
         Restore (Cr);

         --  Enabled steps
         for Step in Step_Range loop
            for Key in Key_Range loop
               if Widget.State (Step)(Key) then
                  Draw_Step (Widget, Cr, Step, Key, True);
               end if;
            end loop;
         end loop;

         Widget.Queue_Draw;
      end if;

   end Full_Redraw;

end Nael.Lab_GUI.Pianoroll_Widget;
