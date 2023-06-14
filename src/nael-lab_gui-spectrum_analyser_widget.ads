with Gtk.Widget; use Gtk.Widget;

with Nael.Frame_Exchange;

private with Gtk.Drawing_Area;
private with Cairo;
private with Glib;
private with Gtk.Handlers;
private with Nael.FFT;

private package Nael.Lab_GUI.Spectrum_Analyser_Widget is

   type Spectrum_Analyser_Record
   is new Gtk_Widget_Record
   with private;

   type Spectrum_Analyser is access all Spectrum_Analyser_Record'Class;

   procedure Gtk_New (Widget      :    out Spectrum_Analyser;
                      Sample_Rate :        Natural);

   procedure Initialize
     (Widget      : not null access Spectrum_Analyser_Record'Class;
      Sample_Rate :                 Natural);

   procedure Push_Frame (Widget : in out Spectrum_Analyser_Record;
                         Frame  :        Mono_Frame);

private

   subtype Bin_Range is Natural range 1 .. Analyser_FFT_Size / 2;
   type Bin_Array is array (Bin_Range) of Float;

   type Spectrum_Analyser_Record
   is new Gtk.Drawing_Area.Gtk_Drawing_Area_Record
   with record
      Sample_Rate : Natural;

      Block_Exchange : Nael.Frame_Exchange.Any_Access;

      FFT : Nael.FFT.Instance (Analyser_FFT_Size,
                               Analyser_FFT_Size / 3);

      Energy        : Bin_Array := (others => 0.0);
      Peak_Energy   : Bin_Array := (others => 0.0);
      Bin_Frequency : Bin_Array := (others => 0.0);
   end record;

   procedure Process_FFT (Widget : in out Spectrum_Analyser_Record'Class);

   function Get_Type return Glib.GType;

   function Draw
     (Widget  : access Spectrum_Analyser_Record'Class;
      Context : Cairo.Cairo_Context)
      return Boolean;

   package Return_Boolean_Callback is
     new Gtk.Handlers.Return_Callback
       (Spectrum_Analyser_Record,
        Boolean);

end Nael.Lab_GUI.Spectrum_Analyser_Widget;
