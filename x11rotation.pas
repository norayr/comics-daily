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
    FMainWindow: TWindow;
    FDisplay: PDisplay;
    procedure DoRotation(IsPortrait: Boolean);
    procedure SetRotationAtom(IsPortrait: Boolean);
  protected
    procedure Execute; override;
  public
    constructor Create(MainWindow: TWindow);
    property OnRotation: TRotationEvent read FOnRotation write FOnRotation;
  end;

implementation

constructor TX11Rotation.Create(MainWindow: TWindow);
begin
  inherited Create(True); // Create suspended
  FreeOnTerminate := True;
  FMainWindow := MainWindow;
end;

procedure TX11Rotation.SetRotationAtom(IsPortrait: Boolean);
const
  ATOM_NAME = '_HILDON_PORTRAIT_MODE_SUPPORT';
var
  atom: TAtom;
  value: TAtom;
begin
  atom := XInternAtom(FDisplay, PChar(ATOM_NAME), False);
  if atom = None then
  begin
    WriteLn('Failed to create X atom');
    Exit;
  end;

  value := IfThen(IsPortrait, 1, 0);
  XChangeProperty(FDisplay, FMainWindow, atom, XA_ATOM, 32, PropModeReplace, @value, 1);
end;

procedure TX11Rotation.DoRotation(IsPortrait: Boolean);
begin
  SetRotationAtom(IsPortrait);
  if Assigned(FOnRotation) then
    FOnRotation(Self, IsPortrait);
end;

procedure TX11Rotation.Execute;
var
  eventBase, errorBase: cint;
  rootWindow: TWindow;
  event: TXEvent;
  xrrEvent: PXRRScreenChangeNotifyEvent;
  eventCount: integer;
begin
  FDisplay := XOpenDisplay(nil);
  if FDisplay = nil then Exit;

  if not XRRQueryExtension(FDisplay, @eventBase, @errorBase) then
  begin
    XCloseDisplay(FDisplay);
    Exit;
  end;

  rootWindow := XRootWindow(FDisplay, XDefaultScreen(FDisplay));
  XRRSelectInput(FDisplay, rootWindow, RRScreenChangeNotifyMask);

  eventCount := 0;

  while not Terminated do
  begin
    XNextEvent(FDisplay, @event);
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

  XCloseDisplay(FDisplay);
end;

end.

