with Ada.Containers.Doubly_Linked_Lists;
with MIDI;

package Nael.MIDI_Exchange is

   package Message_List
   is new Ada.Containers.Doubly_Linked_Lists (MIDI.Message,
                                              MIDI."=");

   protected type Instance is
      procedure Push (Msg : MIDI.Message);
      procedure Pop (Msg : out MIDI.Message; Success : out Boolean);

   private
      Messages : Message_List.List;
   end Instance;

   type Any_Access is access all Instance;

end Nael.MIDI_Exchange;
