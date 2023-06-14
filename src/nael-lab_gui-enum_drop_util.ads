generic
   type Enum_Type is (<>);
package Nael.Lab_GUI.Enum_Drop_Util is

   function Add_Drop_Down (Ctrl : in out User_Control_Setup'Class;
                           Name :        String)
                           return Controller_Id;
   --  Add a drop down control for the values of Enum_Type

   type Image_Function
   is not null access function (E : Enum_Type) return String;

   function Add_Drop_Down (Ctrl  : in out User_Control_Setup'Class;
                           Name  :        String;
                           Image :        Image_Function)
                           return Controller_Id;
   --  Add a drop down control for the values of Enum_Type, with a custom
   --  function to convert enumaration values to string representation.

   function From_Value (V : Float) return Enum_Type;
   --  Convert from a Float value to enum type. Use this function with the
   --  value retrieved from a Value_Exchange.

end Nael.Lab_GUI.Enum_Drop_Util;
