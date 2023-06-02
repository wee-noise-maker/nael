with Ada.Text_IO;
with Ada.Exceptions;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with Gtk.Main;
with Gtk.Window;         use Gtk.Window;
with Gtk.Frame;          use Gtk.Frame;
with Gtk.Box;            use Gtk.Box;
with Gtk.Oscilloscope;   use Gtk.Oscilloscope;
with Gtk.Oscilloscope.Channels_Panel; use Gtk.Oscilloscope.Channels_Panel;
with Gtk.Oscilloscope.Sweeper_Panel; use Gtk.Oscilloscope.Sweeper_Panel;
with Gtk.Scale;          use Gtk.Scale;
with Gtk.Enums;          use Gtk.Enums;
with Gtk.Combo_Box_Text; use Gtk.Combo_Box_Text;
with Gtk.GRange;         use Gtk.GRange;
with Glib; use Glib;
with Glib.Main;

with Gtk.Layered.Waveform; use Gtk.Layered.Waveform;
with Gtk.Layered.Waveform.Ring_Data_Buffer;

with Gtk.Missed;

with System.Storage_Elements; use System.Storage_Elements;

package body Nael.Lab_GUI is

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

   ----------------------
   -- Add_User_Control --
   ----------------------

   procedure Add_User_Control
     (Addr_To_Id  : in out Address_To_Controller_Id_Map.Map;
      Box         :        Gtk_Hbox;
      Id          :        Controller_Id;
      Ctrl        :        User_Control_Info;
      Exchange    : in out Value_Exchange.Instance;
      Scale_CB    :        Cb_Gtk_Range_Void)
   is
      Frame : Gtk_Frame;

      Addr : Integer_Address;
   begin
      Gtk_New (Frame, To_String (Ctrl.Name));
      Frame.Set_Border_Width (3);
      Frame.Set_Vexpand (False);
      Frame.Set_Hexpand (False);

      Box.Add (Frame);

      case Ctrl.Kind is
         when Slider =>
            declare
               Scale : Gtk_Scale;
            begin
               Gtk_New_With_Range
                 (Scale,
                  Orientation => Gtk.Enums.Orientation_Horizontal,
                  Min         => GDouble (Ctrl.Slider_Min),
                  Max         => GDouble (Ctrl.Slider_Max),
                  Step        => GDouble (Ctrl.Slider_Step));

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

               Frame.Add (Drop);
               Addr := To_Integer (Drop.all'Address);
            end;
      end case;

      Exchange.Set (Id, Ctrl.Default);
      Addr_To_Id.Insert (Addr, Id);
   end Add_User_Control;

   --------------
   -- GUI_Task --
   --------------

   task body GUI_Task is
      Window : Gtk_Window;
      Graphs_Frame : Gtk_Frame;
      User_Controls_Frame : Gtk_Frame;
      User_Controls_Vbox : Gtk_Vbox;
      Pane   : Gtk_HBox;
      Channels  : Gtk_Oscilloscope_Channels_Panel;
      Sweeper   : Gtk_Oscilloscope_Sweeper_Panel;

      Osc : Gtk_Oscilloscope;
      Osc_Chan : Gtk.Oscilloscope.Channel_Number;
      Addr_To_Id  : Address_To_Controller_Id_Map.Map;

      Exchange : Value_Exchange.Any_Access;

      --------------------
      -- Scale_Callback --
      --------------------

      procedure Scale_Callback (Self : access Gtk_Range_Record'Class) is
         Int_Addr : constant Integer_Address := To_Integer (Self.all'Address);
         Id : Constant Controller_Id := Addr_To_Id.Element (Int_Addr);
      begin
         Exchange.Set (Id, Float (Self.Get_Value));
      end Scale_Callback;

   begin

      accept Start
        (Lab           :   in out Instance'Class;
         User_Controls :          User_Control_Setup'Class;
         Exchange      : not null Value_Exchange.Any_Access;
         Oscillo       :      out Gtk.Oscilloscope.Gtk_Oscilloscope)
      do
         Gtk.Main.Init;

         Gtk.Window.Gtk_New (Window);

         Window.Set_Title ("Nael Audio Experimentation Lab");
         Window.On_Delete_Event (Gtk.Missed.Delete_Event_Handler'Access);
         Window.On_Destroy (Gtk.Missed.Destroy_Handler'Access);

         Gtk_New_HBox (Pane);
         Pane.Set_Spacing (3);
         Window.Add (Pane);

         Gtk_New (Graphs_Frame);
         Pane.Pack_Start (Graphs_Frame, True, True);
         Graphs_Frame.Set_Border_Width (3);

         Gtk_New (Osc, Refresh_Period => 0.02);
         Graphs_Frame.Add (Osc);

         Gtk_New (User_Controls_Frame, "Controls");
         Pane.Pack_Start (User_Controls_Frame, False, False);
         User_Controls_Frame.Set_Border_Width (3);

         Gtk_New_Vbox (User_Controls_Vbox);
         User_Controls_Frame.Add (User_Controls_Vbox);

         --  Sweeper
         declare
            Frame : Gtk_Frame;
         begin
            Gtk_New (Frame, "Lower sweeper");
            Frame.Set_Border_Width (3);
            Frame.Set_Vexpand (False);
            User_Controls_Vbox.Add (Frame);
            Gtk_New (Sweeper, Osc, Lower);
            Sweeper.Set_Border_Width (3);
            Frame.Add (Sweeper);
         end;

         --  Channels
         declare
            Frame : Gtk_Frame;
         begin
            Gtk_New (Frame, "Channels");
            Frame.Set_Border_Width (3);
            Frame.Set_Vexpand (False);
            User_Controls_Vbox.Add (Frame);
            Gtk_New (Channels, Osc);
            Channels.Set_Border_Width (3);
            Frame.Add (Channels);
         end;

         for Index in
           User_Controls.Controls.First_Index ..
             User_Controls.Controls.Last_Index
         loop
            Add_User_Control (Addr_To_Id,
                              User_Controls_Vbox,
                              Index,
                              User_Controls.Controls.Element (Index),
                              Exchange.all,
                              Scale_CB => Scale_Callback'Unrestricted_Access);
         end loop;

         Osc_Chan := Osc.Add_Channel;

         GUI_Task.Exchange := Exchange;
         Oscillo := Osc;
      end Start;

      Window.Show_All;
      Gtk.Main.Main;

   exception
      when E : others =>
         Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Message (E));
   end GUI_Task;

   -----------
   -- Start --
   -----------

   procedure Start (This          :   in out Instance;
                    User_Controls :          User_Control_Setup'Class;
                    Exchange      : not null Value_Exchange.Any_Access;
                    Oscillo       :      out Gtk_Oscilloscope)
   is
   begin
      This.GUI_T.Start (This, User_Controls, Exchange, Oscillo);
   end Start;

   ------------
   -- Closed --
   ------------

   function Closed (This : Instance) return Boolean
   is (This.GUI_T'Terminated);

end Nael.Lab_GUI;
