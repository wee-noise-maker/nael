with GNAT.OS_Lib; use GNAT.OS_Lib;
with Interfaces; use Interfaces;

package body Nael.Sample_Reader is

   ----------
   -- Open --
   ----------

   procedure Open (This : in out Instance; Filename : String) is
   begin
      if This.FD /= Invalid_FD then
         Close (This.FD);
      end if;

      This.FD := Open_Read (Filename, Binary);
      if This.FD = Invalid_FD then
         raise Program_Error with
           "Failed to open file '" & Filename & "':" &
           Errno_Message (Default => "Unknown Error");
      end if;
   end Open;

   ----------
   -- Read --
   ----------

   procedure Read (This    : in out Instance;
                   Buffer  :    out Block;
                   In_Loop :        Boolean := False)
   is
      type S16_Buffer is array (Natural range <>) of Integer_16;

      In_Buffer : S16_Buffer (Buffer'Range);

      Len : Integer;
      Last : Natural;
   begin
      if This.FD = Invalid_FD then
         Buffer := (others => 0.0);
      else

         Len := Read (This.FD, In_Buffer'Address, In_Buffer'Length * 2);

         Last := Buffer'First + (Len / 2) - 1;
         for Index in Buffer'First .. Last loop
            Buffer (Index) := Float (In_Buffer (Index)) / Float (Integer_16'Last);
         end loop;

         if Last /= Buffer'Last then

            if In_Loop then
               --  Set read cursor to 0
               Lseek (This.FD, 0, 0);

               --  Recursive call to fill the remaining frames
               This.Read (Buffer (Last + 1 .. Buffer'Last), In_Loop);
            else

               --  Fill remaining frames with zeroes
               for Index in Last + 1 .. Buffer'Last loop
                  Buffer (Index) := 0.0;
               end loop;
            end if;
         end if;
      end if;
   end Read;

   ----------
   -- Read --
   ----------

   procedure Read (This    : in out Instance;
                   Frame   :    out Mono_Frame;
                   In_Loop :        Boolean := False)
   is
      Buffer : Block (1 .. 1);
   begin
      This.Read (Buffer, In_Loop);
      Frame := Buffer (1);
   end Read;

end Nael.Sample_Reader;
