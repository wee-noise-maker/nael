
package body Nael.Frame_Exchange is

   protected body Instance is

      ----------
      -- Push --
      ----------

      procedure Push (B : Block) is
      begin
         Blocks.Append (new Block'(B));
      end Push;

      ---------
      -- Pop --
      ---------

      procedure Pop (B_Ptr : out Block_Access) is
      begin
         if Blocks.Is_Empty then
            B_Ptr := null;
         else
            B_Ptr := Blocks.First_Element;
            Blocks.Delete_First;
         end if;
      end Pop;

   end Instance;

end Nael.Frame_Exchange;
