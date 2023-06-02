package Nael is

   subtype Mono_Frame is Float;

   type Framebuffer is array (Natural range <>) of Mono_Frame;

   type Controller_Id is range 1 .. 1000;

end Nael;
