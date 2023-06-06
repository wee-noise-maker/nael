with Ada.Containers.Doubly_Linked_Lists;

package Nael.Frame_Exchange is

   package Block_Access_List
   is new Ada.Containers.Doubly_Linked_Lists (Block_Access);

   protected type Instance is
      procedure Push (B : Block);
      procedure Pop (B_Ptr : out Block_Access);

   private
      Blocks : Block_Access_List.List;
   end Instance;

   type Any_Access is access all Instance;

end Nael.Frame_Exchange;
