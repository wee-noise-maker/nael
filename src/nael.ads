package Nael is

   subtype Mono_Frame is Float;

   type Framebuffer is array (Natural range <>) of Mono_Frame;

   type Controller_Id is range 1 .. 1000;

private

   Analyser_FFT_Size : constant := 1024;

end Nael;
