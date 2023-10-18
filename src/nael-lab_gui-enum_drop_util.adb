package body Nael.Lab_GUI.Enum_Drop_Util is

   -------------------
   -- Add_Drop_Down --
   -------------------

   function Add_Drop_Down (Ctrl    : in out User_Control_Setup'Class;
                           Name    :        String;
                           Default :        Enum_Type := Enum_Type'First)
                           return Controller_Id
   is
      Values : AAA.Strings.Vector := AAA.Strings.Empty_Vector;
   begin
      for Elt in Enum_Type loop
         Values.Append (Elt'Img);
      end loop;

      return Ctrl.Add_Drop_Down (Name, Values, Default => Default'Enum_Rep);
   end Add_Drop_Down;

   -------------------
   -- Add_Drop_Down --
   -------------------

   function Add_Drop_Down (Ctrl    : in out User_Control_Setup'Class;
                           Name    :        String;
                           Image   :        Image_Function;
                           Default :        Enum_Type := Enum_Type'First)
                           return Controller_Id
   is
      Values : AAA.Strings.Vector := AAA.Strings.Empty_Vector;
   begin
      for Elt in Enum_Type loop
         Values.Append (Image (Elt));
      end loop;

      return Ctrl.Add_Drop_Down (Name, Values, Default => Default'Enum_Rep);
   end Add_Drop_Down;

   ----------------
   -- From_Value --
   ----------------

   function From_Value (V : Float) return Enum_Type is
   begin
      return Enum_Type'Val (Enum_Type'Pos (Enum_Type'First) + Integer (V));
   end From_Value;

end Nael.Lab_GUI.Enum_Drop_Util;
