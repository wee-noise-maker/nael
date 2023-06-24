with AAA.Strings;

with Nael.Value_Exchange;
with Nael.Frame_Exchange;
with Nael.MIDI_Exchange;

private with Ada.Strings.Unbounded;
private with Ada.Containers.Indefinite_Vectors;
private with Ada.Containers.Hashed_Maps;
private with System.Storage_Elements;

package Nael.Lab_GUI is

   type User_Control_Setup
   is tagged limited private;

   procedure Set_Experiment_Title (This : in out User_Control_Setup;
                                   Title : String);

   procedure Enable_Keyboard (This : in out User_Control_Setup);
   procedure Enable_Pianoroll (This : in out User_Control_Setup);

   procedure Add_Separator (This : in out User_Control_Setup);

   function Add_Slider (This           : in out User_Control_Setup;
                        Name           :        String;
                        Min, Max, Step :        Float;
                        Default        :        Float := 0.0)
                        return Controller_Id;

   function Add_Drop_Down (This           : in out User_Control_Setup;
                           Name           :        String;
                           Values         :        AAA.Strings.Vector)
                           return Controller_Id;

   function Add_Switch (This           : in out User_Control_Setup;
                        Name           :        String;
                        Default        :        Boolean)
                        return Controller_Id;

   type Instance
   is tagged limited
   private;

   procedure Start
     (This           :   in out Instance;
      Sample_Rate    :          Natural;
      User_Controls  :          User_Control_Setup'Class;
      Exchange       : not null Nael.Value_Exchange.Any_Access;
      Block_Exchange : not null Nael.Frame_Exchange.Any_Access;
      MIDI_Exchange  : not null Nael.MIDI_Exchange.Any_Access);

   function Closed (This : Instance) return Boolean;

private

   type User_Control_Kind is (Slider, Drop_Down, Switch, Separator);

   type User_Control_Info (Kind : User_Control_Kind := Slider) is record
      Name : Ada.Strings.Unbounded.Unbounded_String;
      Default : Float;
      case Kind is
         when Slider =>
            Slider_Min, Slider_Max, Slider_Step : Float;
         when Drop_Down =>
            Drop_Values : AAA.Strings.Vector;
         when Switch | Separator =>
            null;
      end case;
   end record;

   package User_Control_Info_Vectors
   is new Ada.Containers.Indefinite_Vectors (Controller_Id,
                                             User_Control_Info);

   type User_Control_Setup
   is tagged limited record
      Title : Ada.Strings.Unbounded.Unbounded_String :=
        Ada.Strings.Unbounded.To_Unbounded_String
          ("Nael Audio Experimentation Lab");

      Controls : User_Control_Info_Vectors.Vector;
      Keyboard_Enabled : Boolean := False;
      Pianoroll_Enabled : Boolean := False;
   end record;

   task type GUI_Task is
      entry Start
        (Sample_Rate    :          Natural;
         User_Controls  :          User_Control_Setup'Class;
         Exchange       : not null Nael.Value_Exchange.Any_Access;
         Block_Exchange : not null Nael.Frame_Exchange.Any_Access;
         MIDI_Exchange  : not null Nael.MIDI_Exchange.Any_Access);
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
              GUI_T : GUI_Task;
           end record;

end Nael.Lab_GUI;
