unit x11rotation;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, xlib, x, xrandr, unixtype;

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

  TRotationEvent = procedure(Sender: TObject; IsPortrait: Boolean) of object;

  TX11Rotation = class;

  TRotationThread = class(TThread)
  private
    FOwner: TX11Rotation;
  protected
    procedure Execute; override;
  public
    constructor Create(AOwner: TX11Rotation);
  end;

  TX11Rotation = class
  private
    FDisplay: PDisplay;
    FWindow: TWindow;
    FScreen: cint;
    FRootWindow: TWindow;
    FRunning: Boolean;
    FRotationAtom: culong; // Use culong for Atom
    FOnRotation: TRotationEvent;
    FThread: TRotationThread;
    procedure HandleScreenChangeEvent(const Event: PXRRScreenChangeNotifyEvent);
  public
    constructor Create(AWindow: TWindow);
    destructor Destroy; override;
    procedure Start;
    procedure Stop;
    procedure SetRotationAtom(IsPortrait: Boolean);
    property OnRotation: TRotationEvent read FOnRotation write FOnRotation;
  end;

implementation

const
  XA_CARDINAL = 6;

constructor TX11Rotation.Create(AWindow: TWindow);
begin
  FDisplay := XOpenDisplay(nil);
  FWindow := AWindow;
  FScreen := DefaultScreen(FDisplay);
  FRootWindow := RootWindow(FDisplay, FScreen);
  FRotationAtom := XInternAtom(FDisplay, '_HILDON_PORTRAIT_MODE_SUPPORT', False);
end;

destructor TX11Rotation.Destroy;
begin
  Stop;
  inherited Destroy;
end;

procedure TX11Rotation.HandleScreenChangeEvent(const Event: PXRRScreenChangeNotifyEvent);
var
  IsPortrait: Boolean;
begin
  IsPortrait := Event^.rotation in [RR_Rotate_90, RR_Rotate_270];
  if Assigned(FOnRotation) then
    FOnRotation(Self, IsPortrait);
  SetRotationAtom(IsPortrait);
  WriteLn('Rotation detected: ', IsPortrait);
end;

procedure TX11Rotation.SetRotationAtom(IsPortrait: Boolean);
var
  Value: LongInt;
begin
  if IsPortrait then
    Value := 1
  else
    Value := 0;
  XChangeProperty(FDisplay, FRootWindow, FRotationAtom, XA_CARDINAL, 32, PropModeReplace, @Value, 1);
  WriteLn('Set rotation atom: ', Value);
end;

procedure TX11Rotation.Start;
begin
  if not FRunning then
  begin
    FRunning := True;
    FThread := TRotationThread.Create(Self);
    FThread.Start;
  end;
end;

procedure TX11Rotation.Stop;
begin
  if FRunning then
  begin
    FRunning := False;
    if Assigned(FThread) then
    begin
      FThread.WaitFor;
      FThread.Free;
      FThread := nil;
    end;
    XCloseDisplay(FDisplay);
  end;
end;

constructor TRotationThread.Create(AOwner: TX11Rotation);
begin
  inherited Create(True);
  FOwner := AOwner;
  FreeOnTerminate := False;
end;

procedure TRotationThread.Execute;
var
  Event: TXEvent;
  XRRNotifyEvent: PXRRScreenChangeNotifyEvent;
begin
  XRRSelectInput(FOwner.FDisplay, FOwner.FRootWindow, RRScreenChangeNotifyMask);
  WriteLn('Started listening for screen change events');
  while FOwner.FRunning do
  begin
    XNextEvent(FOwner.FDisplay, @Event);
    if Event._type = RRScreenChangeNotify then
    begin
      XRRNotifyEvent := PXRRScreenChangeNotifyEvent(@Event);
      FOwner.HandleScreenChangeEvent(XRRNotifyEvent);
    end;
  end;
end;

end.

