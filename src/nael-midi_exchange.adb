package body Nael.MIDI_Exchange is

   protected body Instance is

      ----------
      -- Push --
      ----------

      procedure Push (Msg : MIDI.Message) is
      begin
         Messages.Append (Msg);
      end Push;

      ---------
      -- Pop --
      ---------

      procedure Pop (Msg : out MIDI.Message; Success : out Boolean) is
      begin
         if Messages.Is_Empty then
            Success := False;
         else
            Success := True;
            Msg := Messages.First_Element;
            Messages.Delete_First;
         end if;
      end Pop;

   end Instance;

end Nael.MIDI_Exchange;
