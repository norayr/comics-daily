unit x11rotation;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, ctypes, xlib, x, xrandr, unixtype;

type
  TRotationEvent = procedure(Sender: TObject; IsPortrait: Boolean) of object;

  TX11Rotation = class(TThread)
  private
    FOnRotation: TRotationEvent;
    procedure DoRotation(IsPortrait: Boolean);
  protected
    procedure Execute; override;
  public
    constructor Create;
    property OnRotation: TRotationEvent read FOnRotation write FOnRotation;
  end;

implementation

constructor TX11Rotation.Create;
begin
  inherited Create(True); // Create suspended
  FreeOnTerminate := True;
end;

procedure TX11Rotation.DoRotation(IsPortrait: Boolean);
begin
  if Assigned(FOnRotation) then
    FOnRotation(Self, IsPortrait);
end;

procedure TX11Rotation.Execute;
var
  display: PDisplay;
  eventBase, errorBase: cint;
  rootWindow: TWindow;
  event: TXEvent;
  xrrEvent: PXRRScreenChangeNotifyEvent;
  eventCount: integer;
begin
  display := XOpenDisplay(nil);
  if display = nil then Exit;

  if not XRRQueryExtension(display, @eventBase, @errorBase) then
  begin
    XCloseDisplay(display);
    Exit;
  end;

  rootWindow := XRootWindow(display, XDefaultScreen(display));
  XRRSelectInput(display, rootWindow, RRScreenChangeNotifyMask);

  eventCount := 0;

  while not Terminated do
  begin
    XNextEvent(display, @event);
    if event._type = eventBase + RRScreenChangeNotify then
    begin
      Inc(eventCount);
      if (eventCount mod 3) = 0 then
      begin
        xrrEvent := PXRRScreenChangeNotifyEvent(@event);
        Synchronize(@DoRotation, xrrEvent^.width < xrrEvent^.height);
      end;
    end;
  end;

  XCloseDisplay(display);
end;

end.

