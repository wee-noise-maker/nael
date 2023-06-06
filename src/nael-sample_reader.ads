private with GNAT.OS_Lib;

package Nael.Sample_Reader is

   type Instance
   is tagged
   private;

   procedure Open (This : in out Instance; Filename : String);
   --  Open a raw 16bit mono sample file

   procedure Read (This    : in out Instance;
                   Buffer  :    out Block;
                   In_Loop :        Boolean := False);
   --  Return the next frames from the sample. At then end of the file, if
   --  In_Loop is true, start reading from the beginning again, otherwise
   --  fill Buffer with zeroes.

   procedure Read (This    : in out Instance;
                   Frame   :    out Mono_Frame;
                   In_Loop :        Boolean := False);
   --  Return the next frame from the sample. At then end of the file, if
   --  In_Loop is true, start reading from the beginning again, otherwise
   --  fill Buffer with zeroes.

private

   type Instance
   is tagged
           record
              FD : GNAT.OS_Lib.File_Descriptor := GNAT.OS_Lib.Invalid_FD;
           end record;

end Nael.Sample_Reader;
