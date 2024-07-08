unit x11rotation;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, xlib, x, xrandr, unixtype, ctypes;

type
  TRotationEvent = procedure(Sender: TObject; IsPortrait: Boolean) of object;

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

  TX11Rotation = class(TThread)
  private
    FOnRotation: TRotationEvent;
    FMainWindow: TWindow;
    FDisplay: PDisplay;
    FIsPortrait: Boolean;
    procedure DoRotation;
    procedure LogRotationEvent(const Message: string);
  protected
    procedure Execute; override;
  public
    constructor Create(MainWindow: TWindow);
    property OnRotation: TRotationEvent read FOnRotation write FOnRotation;
    procedure SetRotationAtom(IsPortrait: Boolean);
  end;

implementation

const
  XA_CARDINAL = 6;

{ TX11Rotation }

constructor TX11Rotation.Create(MainWindow: TWindow);
begin
  inherited Create(True); // Create suspended
  FMainWindow := MainWindow;
  FDisplay := XOpenDisplay(nil);
  FIsPortrait := True;
end;

procedure TX11Rotation.Execute;
var
  Event: TXEvent;
  xrrEvent: PXRRScreenChangeNotifyEvent;
  EventBase, ErrorBase: cint;
begin
  if FDisplay = nil then
  begin
    LogRotationEvent('Unable to open X display');
    Exit;
  end;

  if not XRRQueryExtension(FDisplay, @EventBase, @ErrorBase) then
  begin
    LogRotationEvent('XRandR extension not supported');
    XCloseDisplay(FDisplay);
    Exit;
  end;

  XRRSelectInput(FDisplay, XRootWindow(FDisplay, XDefaultScreen(FDisplay)), RRScreenChangeNotifyMask);
  LogRotationEvent('Started listening for screen change events');

  while not Terminated do
  begin
    XNextEvent(FDisplay, @Event);
    if Event._type = EventBase + RRScreenChangeNotify then
    begin
      xrrEvent := PXRRScreenChangeNotifyEvent(@Event);
      FIsPortrait := (xrrEvent^.rotation = RR_Rotate_0) or (xrrEvent^.rotation = RR_Rotate_180);
      Synchronize(@DoRotation);
    end;
  end;

  XCloseDisplay(FDisplay);
end;

procedure TX11Rotation.DoRotation;
begin
  if Assigned(FOnRotation) then
  begin
    LogRotationEvent(Format('Rotation detected: %s', [BoolToStr(FIsPortrait, True)]));
    FOnRotation(Self, FIsPortrait);
  end;
end;

procedure TX11Rotation.SetRotationAtom(IsPortrait: Boolean);
var
  Atom: TAtom;
  Value: cuint;
begin
  if FDisplay = nil then
    Exit;

  Atom := XInternAtom(FDisplay, '_HILDON_PORTRAIT_MODE_SUPPORT', False);
  if IsPortrait then
    Value := 1
  else
    Value := 0;
  XChangeProperty(FDisplay, FMainWindow, Atom, XA_CARDINAL, 32, PropModeReplace, @Value, 1);
  LogRotationEvent(Format('Set rotation atom: %d', [Value]));
end;

procedure TX11Rotation.LogRotationEvent(const Message: string);
begin
  WriteLn(Message);
end;

end.

