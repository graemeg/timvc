unit mvc_events;

interface
uses
  SysUtils
  ,Classes
  ,tiObject
  ,mvc_base
  ;

type

  {: Indicates that a simple property of an observed object changed. }
  TMVCSimplePropChangeEvent = class(TMVCEvent)

  end;

implementation

end.
