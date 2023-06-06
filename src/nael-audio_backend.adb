with Ada.Text_IO;
with Ada.Exceptions;

with GNAT.OS_Lib;
with System;
with Interfaces.C;


package body Nael.Audio_Backend is

   type Stereo_Frame is record
      L, R : Interfaces.Integer_16;
   end record;

   type Stereo_Buffer is array (Natural range <>) of Stereo_Frame;

   procedure RTaudio_Callback (Buf    : System.Address;
                               Frames : Interfaces.C.unsigned);
   pragma Export (C, RTaudio_Callback, "nael_rtaudio_callback");

   User_Callback : Callback := null;

   ----------------------
   -- RTaudio_Callback --
   ----------------------

   procedure RTaudio_Callback (Buf : System.Address;
                               Frames : Interfaces.C.unsigned)
   is
      Out_Buffer : Stereo_Buffer (1 .. Natural (Frames))
        with Address => Buf;

      Float_Frames : Block (Out_Buffer'Range);

      subtype S16 is Interfaces.Integer_16;

      Sample : S16;
   begin

      if User_Callback /= null then
         User_Callback.all (Float_Frames);

         --  Clamp signal
         for Index in Float_Frames'Range loop
            if Float_Frames (Index) > 1.0 then
               Sample := S16'Last;
            elsif Float_Frames (Index) < -1.0 then
               Sample := S16'First;
            else
               Sample := S16 (Float_Frames (Index) * Float (S16'Last));
            end if;

            Out_Buffer (Index).L := Sample;
            Out_Buffer (Index).R := Sample;
         end loop;
      else
         Out_Buffer := (others => (0, 0));
      end if;

   exception
      when E : others =>
         Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Information (E));
         GNAT.OS_Lib.OS_Exit (42);
   end RTaudio_Callback;

   -----------
   -- Start --
   -----------

   procedure Start (Sample_Rate : Natural;
                    Buffer_Size : Natural;
                    CB          : not null Callback)
   is
      use Interfaces.C;

      function RTaudio_Init (Sample_Rate : Interfaces.C.unsigned;
                             Frames      : Interfaces.C.unsigned)
                             return Interfaces.C.int;
      pragma Import (C, RTaudio_Init, "nael_rtaudio_init");
   begin

      if GNAT.OS_Lib.Getenv ("OS").all = "Windows_NT" then
         --  Select driver for openal on Windows
         GNAT.OS_Lib.Setenv ("ALSOFT_DRIVERS", "dsound");
      end if;

      if RTaudio_Init (Interfaces.C.unsigned (Sample_Rate),
                       Interfaces.C.unsigned (Buffer_Size)) /= 0
      then
         Ada.Text_IO.Put_Line ("rtaudio init error");
         GNAT.OS_Lib.OS_Exit (42);
      end if;

      User_Callback := CB;
   end Start;

end Nael.Audio_Backend;
