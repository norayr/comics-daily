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

  TX11Rotation = class
  private
    FDisplay: PDisplay;
    FWindow: TWindow;
    FScreen: cint;
    FRootWindow: TWindow;
    FRunning: Boolean;
    FRotationAtom: culong; // Use culong for Atom
    FOnRotation: TRotationEvent;
    procedure HandleScreenChangeEvent(const Event: PXRRScreenChangeNotifyEvent);

  public
    constructor Create(AWindow: TWindow);
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
var
  Event: TXEvent;
  XRRNotifyEvent: PXRRScreenChangeNotifyEvent;
begin
  if not FRunning then
  begin
    XRRSelectInput(FDisplay, FRootWindow, RRScreenChangeNotifyMask);
    FRunning := True;
    WriteLn('Started listening for screen change events');
    while FRunning do
    begin
      XNextEvent(FDisplay, @Event);
      if Event._type = RRScreenChangeNotify then
      begin
        XRRNotifyEvent := PXRRScreenChangeNotifyEvent(@Event);
        HandleScreenChangeEvent(XRRNotifyEvent);
      end;
    end;
  end;
end;

procedure TX11Rotation.Stop;
begin
  FRunning := False;
  XCloseDisplay(FDisplay);
end;

end.

