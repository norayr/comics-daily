unit x11rotation;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, xlib, x, xrandr, unixtype;

type
  TRotationEvent = procedure(Sender: TObject; IsPortrait: Boolean) of object;

  TX11Rotation = class(TThread)
  private
    FOnRotation: TRotationEvent;
    FMainWindow: TWindow;
    FDisplay: PDisplay;
    FIsPortrait: Boolean;
    procedure DoRotation;
  protected
    procedure Execute; override;
  public
    constructor Create(MainWindow: TWindow);
    property OnRotation: TRotationEvent read FOnRotation write FOnRotation;
    procedure SetRotationAtom;
  end;

implementation

const
  XA_ATOM = 4; // Defining XA_ATOM if not available

type
  PXRRScreenChangeNotifyEvent = ^TXRRScreenChangeNotifyEvent;
  TXRRScreenChangeNotifyEvent = record
    _type: cint;
    serial: culong;
    send_event: TBool;
    display: PDisplay;
    window: TWindow;
    root: TWindow;
    timestamp: TTime;
    config_timestamp: TTime;
    size_index: cint;
    subpixel_order: cint;
    rotation: cuint;
    width: cint;
    height: cint;
    mwidth: cint;
    mheight: cint;
  end;

constructor TX11Rotation.Create(MainWindow: TWindow);
begin
  inherited Create(True); // Create suspended
  FreeOnTerminate := True;
  FMainWindow := MainWindow;
end;

procedure TX11Rotation.SetRotationAtom;
const
  ATOM_NAME = '_HILDON_PORTRAIT_MODE_SUPPORT';
var
  atom: TAtom;
  value: LongInt; // Use LongInt to match the expected data type
begin
  if FMainWindow = 0 then
  begin
    WriteLn('SetRotationAtom: Invalid main window');
    Exit;
  end;

  atom := XInternAtom(FDisplay, PChar(ATOM_NAME), False);
  if atom = None then
  begin
    WriteLn('Failed to create X atom');
    Exit;
  end;

  if FIsPortrait then
    value := 1
  else
    value := 0;

  WriteLn('Setting rotation atom: ', ATOM_NAME, ' to ', value);
  XChangeProperty(FDisplay, FMainWindow, atom, XA_ATOM, 32, PropModeReplace, PByte(@value), 1);
  XFlush(FDisplay); // Ensure the command is sent to the X server
end;

procedure TX11Rotation.DoRotation;
begin
  SetRotationAtom;
  if Assigned(FOnRotation) then
    FOnRotation(Self, FIsPortrait);
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
  if FDisplay = nil then
  begin
    WriteLn('Unable to open X display');
    Exit;
  end;

  XSynchronize(FDisplay, True); // Make X11 synchronous for debugging

  if not XRRQueryExtension(FDisplay, @eventBase, @errorBase) then
  begin
    WriteLn('XRandR extension not supported');
    XCloseDisplay(FDisplay);
    Exit;
  end;

  rootWindow := XRootWindow(FDisplay, XDefaultScreen(FDisplay));
  XRRSelectInput(FDisplay, rootWindow, RRScreenChangeNotifyMask);

  eventCount := 0;

  while not Terminated do
  begin
    try
      XNextEvent(FDisplay, @event);
      if event._type = eventBase + RRScreenChangeNotify then
      begin
        Inc(eventCount);
        if (eventCount mod 3) = 0 then
        begin
          xrrEvent := PXRRScreenChangeNotifyEvent(@event);
          FIsPortrait := xrrEvent^.width < xrrEvent^.height;
          WriteLn('Screen change detected: ', xrrEvent^.width, 'x', xrrEvent^.height, ' Rotation: ', xrrEvent^.rotation);
          Synchronize(@DoRotation);
        end;
      end;
    except
      on E: Exception do
      begin
        WriteLn('Exception in X11 event loop: ', E.Message);
        Terminate; // Exit the loop if there's an exception
      end;
    end;
  end;

  XCloseDisplay(FDisplay);
end;

end.

