package body Nael.Value_Exchange is

   protected body Instance is

      ---------
      -- Set --
      ---------

      procedure Set (Id : Controller_Id; Value : Float) is
      begin
         if Id not in Values.First_Index .. Values.Last_Index then
            Values.Set_Length (Ada.Containers.Count_Type (Id));
         end if;

         Values.Replace_Element (Id, Value);
      end Set;

      ---------
      -- Get --
      ---------

      function Get (Id : Controller_Id) return Float is
      begin
         if Id in Values.First_Index .. Values.Last_Index then
            return Values.Element (Id);
         else
            return 0.0;
         end if;
      end Get;

      ---------------
      -- Set_Label --
      ---------------

      procedure Set_Label (Id : Controller_Id; Str : String) is
      begin
         Label_Requests.Append
           ((Id, Ada.Strings.Unbounded.To_Unbounded_String (Str)));
      end Set_Label;

      ---------------
      -- Pop_Label --
      ---------------

      procedure Pop_Label (Info : out Label_Info; Success : out Boolean) is
      begin
         if not Label_Requests.Is_Empty then
            Info := Label_Requests.First_Element;
            Label_Requests.Delete_First;
            Success := True;
         else
            Success := False;
         end if;
      end Pop_Label;

   end Instance;

end Nael.Value_Exchange;
