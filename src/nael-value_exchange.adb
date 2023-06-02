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

   end Instance;

end Nael.Value_Exchange;
