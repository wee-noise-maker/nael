with AAA.Strings;

with Nael.Value_Exchange;

with Gtk.Oscilloscope;

private with Ada.Strings.Unbounded;
private with Ada.Containers.Indefinite_Vectors;
private with Ada.Containers.Hashed_Maps;
private with System.Storage_Elements;

package Nael.Lab_GUI is

   type User_Control_Setup
   is tagged limited private;

   function Add_Slider (This           : in out User_Control_Setup;
                        Name           :        String;
                        Min, Max, Step :        Float;
                        Default        :        Float := 0.0)
                        return Controller_Id;

   function Add_Drop_Down (This           : in out User_Control_Setup;
                           Name           :        String;
                           Values         :        AAA.Strings.Vector)
                           return Controller_Id;

   type Instance
   is tagged limited
   private;

   procedure Start
     (This          :   in out Instance;
      User_Controls :          User_Control_Setup'Class;
      Exchange      : not null Value_Exchange.Any_Access;
      Oscillo       :      out Gtk.Oscilloscope.Gtk_Oscilloscope;
      Analyser      :      out Gtk.Oscilloscope.Gtk_Oscilloscope);

   function Closed (This : Instance) return Boolean;

private

   type User_Control_Kind is (Slider, Drop_Down);

   type User_Control_Info (Kind : User_Control_Kind := Slider) is record
      Name : Ada.Strings.Unbounded.Unbounded_String;
      Default : Float;
      case Kind is
         when Slider =>
            Slider_Min, Slider_Max, Slider_Step : Float;
         when Drop_Down =>
            Drop_Values : AAA.Strings.Vector;
      end case;
   end record;

   package User_Control_Info_Vectors
   is new Ada.Containers.Indefinite_Vectors (Controller_Id,
                                             User_Control_Info);

   type User_Control_Setup
   is tagged limited record
      Controls : User_Control_Info_Vectors.Vector;
   end record;

   task type GUI_Task is
      entry Start
        (Lab           :   in out Instance'Class;
         User_Controls :          User_Control_Setup'Class;
         Exchange      : not null Value_Exchange.Any_Access;
         Oscillo       :      out Gtk.Oscilloscope.Gtk_Oscilloscope;
         Analyser      :      out Gtk.Oscilloscope.Gtk_Oscilloscope);
   end GUI_Task;

   function Hash (Key : System.Storage_Elements.Integer_Address)
                  return Ada.Containers.Hash_Type;

   function Equivalent_Keys (A, B : System.Storage_Elements.Integer_Address)
                  return Boolean;

   package Address_To_Controller_Id_Map
   is new Ada.Containers.Hashed_Maps
     (Key_Type        => System.Storage_Elements.Integer_Address,
      Element_Type    => Controller_Id,
      Hash            => Hash,
      Equivalent_Keys => Equivalent_Keys);

   package User_Control_Value_Vectors
   is new Ada.Containers.Indefinite_Vectors (Controller_Id,
                                             Float);

   type Instance
   is tagged limited
           record
              GUI_T             : GUI_Task;
              Audio_Out_Channel : Gtk.Oscilloscope.Channel_Number;
           end record;

end Nael.Lab_GUI;
