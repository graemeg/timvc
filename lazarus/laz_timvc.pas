{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit laz_timvc; 

interface

uses
    mvc_base, widget_controllers, mvc_events, lcl_controllers, mvc_criteria, 
  LazarusPackageIntf;

implementation

procedure Register; 
begin
end; 

initialization
  RegisterPackage('laz_timvc', @Register); 
end.
