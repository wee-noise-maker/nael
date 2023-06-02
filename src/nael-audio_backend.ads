package Nael.Audio_Backend is

   type Callback is access procedure (Buffer : out Framebuffer);

   procedure Start (Sample_Rate : Natural;
                    Buffer_Size : Natural;
                    CB          : not null Callback);

end Nael.Audio_Backend;
