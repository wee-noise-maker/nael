with Ada.Text_IO;
with Ada.Exceptions;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with Gtk.Main;
with Gtk.Window;          use Gtk.Window;
with Gtk.Frame;           use Gtk.Frame;
with Gtk.Box;             use Gtk.Box;
with Gtk.Scale;           use Gtk.Scale;
with Gtk.Enums;           use Gtk.Enums;
with Gtk.Combo_Box_Text;  use Gtk.Combo_Box_Text;
with Gtk.Combo_Box;       use Gtk.Combo_Box;
with Gtk.GRange;          use Gtk.GRange;
with Gtk.Switch;          use Gtk.Switch;
with Gtk.Separator;       use Gtk.Separator;
with Gtk.Scrolled_Window; use Gtk.Scrolled_Window;
with Glib; use Glib;
with Glib.Main;

with System.Storage_Elements; use System.Storage_Elements;

with Nael.Lab_GUI.Spectrum_Analyser_Widget;
use Nael.Lab_GUI.Spectrum_Analyser_Widget;
with Nael.Lab_GUI.Oscilloscope_Widget; use Nael.Lab_GUI.Oscilloscope_Widget;
with Nael.Lab_GUI.Keyboard_Widget; use Nael.Lab_GUI.Keyboard_Widget;
with Nael.Lab_GUI.Pianoroll_Widget; use Nael.Lab_GUI.Pianoroll_Widget;
with Gtk.Widget; use Gtk.Widget;

package body Nael.Lab_GUI is

   package User_Control_Frame_Vectors
   is new Ada.Containers.Indefinite_Vectors (Controller_Id,
                                             Gtk_Frame);

   ----------
   -- Hash --
   ----------

   function Hash (Key : System.Storage_Elements.Integer_Address)
                  return Ada.Containers.Hash_Type
   is
      use Ada.Containers;
   begin
      return Hash_Type (Key mod Hash_Type'Modulus);
   end Hash;

   ---------------------
   -- Equivalent_Keys --
   ---------------------

   function Equivalent_Keys (A, B : System.Storage_Elements.Integer_Address)
                             return Boolean
   is (A = B);

   --------------------------
   -- Set_Experiment_Title --
   --------------------------

   procedure Set_Experiment_Title (This : in out User_Control_Setup;
                                   Title : String)
   is
   begin
      This.Title := To_Unbounded_String (Title);
   end Set_Experiment_Title;

   ---------------------
   -- Enable_Keyboard --
   ---------------------

   procedure Enable_Keyboard (This : in out User_Control_Setup) is
   begin
      This.Keyboard_Enabled := True;
   end Enable_Keyboard;

   ----------------------
   -- Enable_Pianoroll --
   ----------------------

   procedure Enable_Pianoroll (This : in out User_Control_Setup) is
   begin
      This.Pianoroll_Enabled := True;
   end Enable_Pianoroll;

   ----------------
   -- Add_Slider --
   ----------------

   function Add_Slider (This           : in out User_Control_Setup;
                        Name           :        String;
                        Min, Max, Step :        Float;
                        Default        :        Float := 0.0)
                        return Controller_Id
   is
   begin
      This.Controls.Append ((Kind        => Slider,
                             Default     => Default,
                             Name        => To_Unbounded_String (Name),
                             Slider_Min  => Min,
                             Slider_Max  => Max,
                             Slider_Step => Step));
      return This.Controls.Last_Index;
   end Add_Slider;

   -------------------
   -- Add_Drop_Down --
   -------------------

   function Add_Drop_Down (This           : in out User_Control_Setup;
                           Name           :        String;
                           Values         :        AAA.Strings.Vector)
                           return Controller_Id
   is
   begin
      This.Controls.Append ((Kind        => Drop_Down,
                             Default     => 0.0,
                             Name        => To_Unbounded_String (Name),
                             Drop_Values => Values));
      return This.Controls.Last_Index;
   end Add_Drop_Down;

   ----------------
   -- Add_Switch --
   ----------------

   function Add_Switch (This           : in out User_Control_Setup;
                        Name           :        String;
                        Default        :        Boolean)
                        return Controller_Id
   is
   begin
      This.Controls.Append ((Kind    => Switch,
                             Default => (if Default then 1.0 else 0.0),
                             Name        => To_Unbounded_String (Name)));
      return This.Controls.Last_Index;
   end Add_Switch;

   -------------------
   -- Add_Separator --
   -------------------

   procedure Add_Separator (This : in out User_Control_Setup) is
   begin
      This.Controls.Append ((Kind    => Separator,
                             Default => 0.0,
                             Name    => To_Unbounded_String ("")));
   end Add_Separator;

   ----------------------
   -- Add_User_Control --
   ----------------------

   procedure Add_User_Control
     (Addr_To_Id  : in out Address_To_Controller_Id_Map.Map;
      Frames      : in out User_Control_Frame_Vectors.Vector;
      Box         :        Gtk_Hbox;
      Id          :        Controller_Id;
      Ctrl        :        User_Control_Info;
      Exchange    : in out Value_Exchange.Instance;
      Scale_CB    :        Cb_Gtk_Range_Void;
      Combo_CB    :        Cb_Gtk_Combo_Box_Void;
      Switch_CB   :        Cb_Gtk_Switch_Boolean_Boolean)
   is
      Frame : Gtk_Frame;

      Addr : Integer_Address := 0;

   begin
      Gtk_New (Frame, To_String (Ctrl.Name));
      Frame.Set_Border_Width (3);
      Frame.Set_Vexpand (False);
      Frame.Set_Hexpand (False);

      Box.Pack_Start (Frame, Fill => False, Expand => False);

      case Ctrl.Kind is
         when Slider =>
            declare
               Scale : Gtk_Scale;
            begin
               Gtk_New_With_Range
                 (Scale,
                  Orientation => Gtk.Enums.Orientation_Horizontal,
                  Min         => Gdouble (Ctrl.Slider_Min),
                  Max         => Gdouble (Ctrl.Slider_Max),
                  Step        => Gdouble (Ctrl.Slider_Step));

               Scale.Set_Value (Gdouble (Ctrl.Default));
               Scale.Set_Vexpand (False);
               Frame.Add (Scale);

               Scale.On_Value_Changed (Scale_CB);

               Addr := To_Integer (Scale.all'Address);
            end;

         when Drop_Down =>
            declare
               Drop : Gtk_Combo_Box_Text;
            begin
               Gtk_New (Drop);
               for Elt of Ctrl.Drop_Values loop
                  Drop.Append_Text (Elt);
               end loop;

               Drop.Set_Active (0);

               Frame.Add (Drop);

               Drop.On_Changed (Combo_CB);

               Addr := To_Integer (Drop.all'Address);
            end;

         when Switch =>
            declare
               Switch : Gtk_Switch;
            begin
               Gtk_New (Switch);
               Switch.Set_Active (Ctrl.Default > 0.5);

               Switch.Set_Halign (Align_Center);
               Switch.Set_Valign (Align_Center);
               Frame.Add (Switch);

               Switch.On_State_Set (Switch_CB);

               Addr := To_Integer (Switch.all'Address);
            end;

         when Separator =>
            declare
               Sep : Gtk_Separator;
            begin
               Gtk_New_Hseparator (Sep);
               Frame.Add (Sep);
               Addr := To_Integer (Sep.all'Address);
            end;
      end case;

      Frames.Append (Frame);
      Exchange.Set (Id, Ctrl.Default);
      Addr_To_Id.Insert (Addr, Id);
   end Add_User_Control;

   --------------
   -- GUI_Task --
   --------------

   task body GUI_Task is
      Window : Gtk_Window;
      Graphs_Frame : Gtk_Frame;

      Graphs_Vbox : Gtk_Vbox;

      User_Controls_Frame : Gtk_Frame;
      Frame : Gtk_Frame;
      User_Controls_Scroll : Gtk_Scrolled_Window;
      User_Controls_Vbox : Gtk_Vbox;
      Pane   : Gtk_Hbox;

      Addr_To_Id  : Address_To_Controller_Id_Map.Map;

      Exchange : Value_Exchange.Any_Access;
      Block_Exchange : Frame_Exchange.Any_Access;

      Spectrum     : Nael.Lab_GUI.Spectrum_Analyser_Widget.Spectrum_Analyser;
      Oscilloscope : Nael.Lab_GUI.Oscilloscope_Widget.Oscilloscope;
      Keyboard     : Nael.Lab_GUI.Keyboard_Widget.Keyboard;
      Pianoroll    : Nael.Lab_GUI.Pianoroll_Widget.Pianoroll;

      Ctrl_Frames  : User_Control_Frame_Vectors.Vector;

      -------------------------------------
      -- Window_Size_Allocation_Callback --
      -------------------------------------

      procedure Window_Size_Allocation_Callback
        (Self       : access Gtk_Widget_Record'Class;
         Allocation : Gtk_Allocation)
      is
         pragma Unreferenced (Self);
      begin
         User_Controls_Vbox.Set_Size_Request (Allocation.Width / 3, -1);
      end Window_Size_Allocation_Callback;

      --------------------
      -- Scale_Callback --
      --------------------

      procedure Scale_Callback (Self : access Gtk_Range_Record'Class) is
         Int_Addr : constant Integer_Address := To_Integer (Self.all'Address);
         Id : constant Controller_Id := Addr_To_Id.Element (Int_Addr);
      begin
         Exchange.Set (Id, Float (Self.Get_Value));
      end Scale_Callback;

      --------------------
      -- Combo_Callback --
      --------------------

      procedure Combo_Callback (Self : access Gtk_Combo_Box_Record'Class) is
         Int_Addr : constant Integer_Address := To_Integer (Self.all'Address);
         Id : constant Controller_Id := Addr_To_Id.Element (Int_Addr);
      begin
         Exchange.Set (Id, Float (Self.Get_Active));
      end Combo_Callback;

      ---------------------
      -- Switch_Callback --
      ---------------------

      function Switch_Callback (Self  : access Gtk_Switch_Record'Class;
                                State : Boolean) return Boolean
      is
         Int_Addr : constant Integer_Address := To_Integer (Self.all'Address);
         Id : constant Controller_Id := Addr_To_Id.Element (Int_Addr);
      begin
         Exchange.Set (Id, (if State then 1.0 else 0.0));
         return True;
      end Switch_Callback;

      ----------------------
      -- Timeout_Callback --
      ----------------------

      function Timeout_Callback return Boolean is
         use Frame_Exchange;

         Block   : Block_Access;
         Label   : Nael.Value_Exchange.Label_Info;
         Success : Boolean;
      begin

         loop
            Exchange.Pop_Label (Label, Success);
            exit when not Success;

            Ctrl_Frames (Label.Id).Set_Label
              (Ada.Strings.Unbounded.To_String (Label.Label));
         end loop;

         loop
            Block_Exchange.Pop (Block);
            exit when Block = null;

            for Elt of Block.all loop
               Spectrum.Push_Frame (Elt);
               Oscilloscope.Push_Frame (Elt);
            end loop;

         end loop;
         return True;
      end Timeout_Callback;

   begin

      accept Start
        (Sample_Rate    :          Natural;
         User_Controls  :          User_Control_Setup'Class;
         Exchange       : not null Nael.Value_Exchange.Any_Access;
         Block_Exchange : not null Nael.Frame_Exchange.Any_Access;
         MIDI_Exchange  : not null Nael.MIDI_Exchange.Any_Access)
      do
         Gtk.Main.Init;

         Gtk.Window.Gtk_New (Window);

         Window.Set_Title (To_String (User_Controls.Title));

         --  Callback to set the size of the control vbox/frame
         Window.On_Size_Allocate
           (Window_Size_Allocation_Callback'Unrestricted_Access,
            After => False);

         Gtk_New_Hbox (Pane);
         Window.Add (Pane);

         Gtk_New (Graphs_Frame);
         Pane.Pack_Start (Graphs_Frame, True, True);
         Graphs_Frame.Set_Border_Width (3);

         Gtk_New_Vbox (Graphs_Vbox);
         Graphs_Frame.Add (Graphs_Vbox);

         --  Spectrum Analyser
         Gtk_New (Frame, "Spectrum Analyzer");
         Graphs_Vbox.Pack_Start (Frame, Expand => True);
         Gtk_New (Spectrum, Sample_Rate);
         Frame.Add (Spectrum);
         ---------------------

         --  Oscilloscope
         Gtk_New (Frame, "Oscilloscope");
         Graphs_Vbox.Pack_Start (Frame, Expand => True);
         Gtk_New (Oscilloscope, Sample_Rate);
         Frame.Add (Oscilloscope);
         ----------------

         --  Pianoroll
         if User_Controls.Pianoroll_Enabled then
            Gtk_New (Frame, "Piano Roll");
            Graphs_Vbox.Add (Frame);
            Gtk_New (Pianoroll, MIDI_Exchange);
            Frame.Add (Pianoroll);
         end if;
         -------------

         --  Keyboard
         if User_Controls.Keyboard_Enabled then
            Gtk_New (Frame, "Keyboard");
            Graphs_Vbox.Pack_Start (Frame, Expand => False);

            Gtk_New (Keyboard, MIDI_Exchange);
            Frame.Add (Keyboard);
         end if;
         ----------------

         Gtk_New (User_Controls_Frame, "Controls");
         Pane.Pack_Start (User_Controls_Frame, False, True);
         User_Controls_Frame.Set_Border_Width (3);

         Gtk_New (User_Controls_Scroll);
         User_Controls_Scroll.Set_Policy
           (Hscrollbar_Policy => Gtk.Enums.Policy_Never,
            Vscrollbar_Policy => Gtk.Enums.Policy_Automatic);
         User_Controls_Frame.Add (User_Controls_Scroll);

         Gtk_New_Vbox (User_Controls_Vbox);
         User_Controls_Scroll.Add (User_Controls_Vbox);

         for Index in
           User_Controls.Controls.First_Index ..
             User_Controls.Controls.Last_Index
         loop
            Add_User_Control
              (Addr_To_Id,
               Ctrl_Frames,
               User_Controls_Vbox,
               Index,
               User_Controls.Controls.Element (Index),
               Exchange.all,
               Scale_CB  => Scale_Callback'Unrestricted_Access,
               Combo_CB  => Combo_Callback'Unrestricted_Access,
               Switch_CB => Switch_Callback'Unrestricted_Access);
         end loop;

         GUI_Task.Exchange := Exchange;
         GUI_Task.Block_Exchange := Block_Exchange;
      end Start;

      --  Periodic callback
      declare
         Unused : Glib.Main.G_Source_Id;
      begin
         Unused := Glib.Main.Timeout_Add
           (1000 / 30, Timeout_Callback'Unrestricted_Access);
      end;

      Window.Show_All;
      Gtk.Main.Main;

   exception
      when E : others =>
         Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Message (E));
   end GUI_Task;

   -----------
   -- Start --
   -----------

   procedure Start
     (This           :   in out Instance;
      Sample_Rate    :          Natural;
      User_Controls  :          User_Control_Setup'Class;
      Exchange       : not null Nael.Value_Exchange.Any_Access;
      Block_Exchange : not null Nael.Frame_Exchange.Any_Access;
      MIDI_Exchange  : not null Nael.MIDI_Exchange.Any_Access)
   is
   begin
      This.GUI_T.Start (Sample_Rate, User_Controls,
                        Exchange, Block_Exchange, MIDI_Exchange);
   end Start;

   ------------
   -- Closed --
   ------------

   function Closed (This : Instance) return Boolean
   is (This.GUI_T'Terminated);

end Nael.Lab_GUI;
