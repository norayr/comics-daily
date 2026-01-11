unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  FPImage, FPReadJPEG, FPReadPNG, FPReadGIF, FPWriteBMP, LazFileUtils,
  IntfGraphics, Math, GoComicsAPI, x, Gtk2, Gdk2, Gdk2x, xatom,
  LCLType, LCLIntf;

const
  margin = 10;

type
  { TForm1 }

  TForm1 = class(TForm)
    zoomIn: TButton;
    lastButton: TButton;
    firstButton: TButton;
    ComboBox1: TComboBox;
    PrevButton: TButton;
    NextButton: TButton;
    ShowComicButton: TButton;
    SaveComicButton: TButton;
    Memo1: TMemo;
    Image1: TImage;
    zoomOut: TButton;
    procedure ComboBox1Change(Sender: TObject);
    procedure firstButtonClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure lastButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure ShowComicButtonClick(Sender: TObject);
    procedure SaveComicButtonClick(Sender: TObject);
    procedure PrevButtonClick(Sender: TObject);
    procedure NextButtonClick(Sender: TObject);
    procedure Image1MouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure zoomInClick(Sender: TObject);
    procedure zoomOutClick(Sender: TObject);
    procedure ZoomImage(ZoomFactor: Double);
    procedure ResetAndReloadComic;
    procedure FitAndCenter;
  private
    FPrevClientWidth, FPrevClientHeight: Integer;
    FWinPropertySet: boolean;
    FIsComicLoaded: Boolean;
    FComicStream: TMemoryStream;
    FFileName: string;
    FContentType: string;
    FCurrentComic: string;
    FGoComics: TGoComics;
    FCachedBitmap: TBitmap;
    FIsPortrait: Boolean;

    FScaleFactor: Double;
    FOffsetX, FOffsetY: Integer;
    FLastMouseX, FLastMouseY: Integer;
    FIsPanning: Boolean;

    FComic_Section: Real;
    procedure LoadImageFromStream(Stream: TMemoryStream; const ContentType: string);
    procedure ResizeImage;
    function GetComicsDailyDir: string;
    function GetFileExtension(const ContentType: string): string;
    procedure LoadCurrentComic;
    procedure UpdateButtonStates;
    procedure UpdateLayout;
    procedure HandleRotation(Sender: TObject; IsPortrait: Boolean);
    procedure CacheImage(Bitmap: TBitmap);
    procedure LoadCachedImage;

    procedure StartPan(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure PerformPan(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure EndPan(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
  public
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  ComboBox1.Style := csDropDownList;
  ComboBox1.ReadOnly:= True;

  ComboBox1.Items.Add('academiawaltz');
  ComboBox1.Items.Add('agnes');
  ComboBox1.Items.Add('andycapp');
  ComboBox1.Items.Add('arloandjanis');
  ComboBox1.Items.Add('artbymoga');
  ComboBox1.Items.Add('aunty-acid');
  ComboBox1.Items.Add('babyblues');
  ComboBox1.Items.Add('bignate');
  ComboBox1.Items.Add('bird-and-moon');
  ComboBox1.Items.Add('bleeker');
  ComboBox1.Items.Add('breaking-cat-news');
  ComboBox1.Items.Add('broomhilda');
  ComboBox1.Items.Add('calvinandhobbes');
  ComboBox1.Items.Add('cats-cafe');
  ComboBox1.Items.Add('citizendog');
  ComboBox1.Items.Add('culdesac');
  ComboBox1.Items.Add('crabgrass');
  ComboBox1.Items.Add('dinosaur-comics');
  ComboBox1.Items.Add('doonesbury');
  ComboBox1.Items.Add('emmy-lou');
  ComboBox1.Items.Add('everyday-people-cartoons');
  ComboBox1.Items.Add('eyebeam');
  ComboBox1.Items.Add('fminus');
  ComboBox1.Items.Add('fowl-language');
  ComboBox1.Items.Add('foxtrot');
  ComboBox1.Items.Add('fredbasset');
  ComboBox1.Items.Add('garfield');
  ComboBox1.Items.Add('grand-avenue');
  ComboBox1.Items.Add('gray-matters');
  ComboBox1.Items.Add('herman');
  ComboBox1.Items.Add('homefree');
  ComboBox1.Items.Add('liz-climo-cartoons');
  ComboBox1.Items.Add('littledoglost');
  ComboBox1.Items.Add('lockhorns');
  ComboBox1.Items.Add('lola');
  ComboBox1.Items.Add('lostsheep');
  ComboBox1.Items.Add('overboard');
  ComboBox1.Items.Add('peanuts');
  ComboBox1.Items.Add('pearlsbeforeswine');
  ComboBox1.Items.Add('perry-bible-fellowship');
  ComboBox1.Items.Add('phoebe-and-her-unicorn');
  ComboBox1.Items.Add('poochcafe');
  ComboBox1.Items.Add('poorly-drawn-lines');
  ComboBox1.Items.Add('sarahs-scribbles');
  ComboBox1.Items.Add('savage-chickens');
  ComboBox1.Items.Add('stonesoup');
  ComboBox1.Items.Add('sunshine-state');
  ComboBox1.Items.Add('super-fun-pak-comix');
  ComboBox1.Items.Add('ten-cats');
  ComboBox1.Items.Add('the-adventures-of-business-cat');
  ComboBox1.Items.Add('the-awkward-yeti');
  ComboBox1.Items.Add('theothercoast');
  ComboBox1.Items.Add('thinlines');
  ComboBox1.Items.Add('understanding-chaos');
  ComboBox1.Items.Add('wallace-the-brave');
  ComboBox1.Items.Add('wizardofid');
  ComboBox1.Items.Add('worry-lines');
  ComboBox1.Items.Add('wrong-hands');
  ComboBox1.Items.Add('wtduck');
  ComboBox1.Items.Add('1-and-done');
  ComboBox1.Items.Add('9to5');

  ComboBox1.ItemIndex := 22; // Select fminus by default
  ComboBox1.ReadOnly := True;

  Memo1.Enabled := False;
  Memo1.Visible := False;

  Form1.Caption := 'comics daily (RSS)';

  prevButton.Enabled := False;
  nextButton.Enabled := False;
  firstButton.Enabled := False;
  lastButton.Enabled := False;
  zoomIn.Enabled := False;
  zoomOut.Enabled := False;
  SaveComicButton.Enabled := False;
  UpdateLayout;

  Self.OnShow := @FormShow;
  Self.OnClose := @FormClose;
  FPrevClientWidth := 0;
  FPrevClientHeight := 0;

  FCachedBitmap := nil;

  FScaleFactor := 1.0;
  FOffsetX := 0;
  FOffsetY := 0;

  Image1.OnMouseDown := @StartPan;
  Image1.OnMouseMove := @PerformPan;
  Image1.OnMouseUp := @EndPan;
  Image1.OnMouseWheel := @Image1MouseWheel;

  zoomIn.OnClick := @zoomInClick;
  zoomOut.OnClick := @zoomOutClick;

  firstButton.Visible := False;
  lastButton.Visible := False;
end;

procedure TForm1.FormShow(Sender: TObject);
begin
  // Rotation is handled by Hildon atom set in FormActivate
end;

procedure TForm1.FitAndCenter;
begin
  // Force recompute best-fit scale on next ResizeImage
  FScaleFactor := 1.0;

  // Force centering logic to run
  FOffsetX := 0;
  FOffsetY := 0;

  ResizeImage;
end;

procedure TForm1.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  FreeAndNil(FCachedBitmap);
  FreeAndNil(FGoComics);
  FreeAndNil(FComicStream);
end;

procedure TForm1.HandleRotation(Sender: TObject; IsPortrait: Boolean);
begin
  if IsPortrait then
    WriteLn('HandleRotation: Portrait mode detected')
  else
    WriteLn('HandleRotation: Landscape mode detected');

  FIsPortrait := IsPortrait;
  UpdateLayout;

  if Assigned(FCachedBitmap) then
    //LoadCachedImage;
    FitAndCenter
end;

procedure TForm1.ResetAndReloadComic;
begin
  FScaleFactor := 1.0;
  FOffsetX := 0;
  FOffsetY := 0;

  if Assigned(FCachedBitmap) and (FComicStream <> nil) then
  begin
    FComicStream.Position := 0;
    LoadImageFromStream(FComicStream, FContentType);
  end;
end;

procedure TForm1.CacheImage(Bitmap: TBitmap);
begin
  if not Assigned(FCachedBitmap) then
    FCachedBitmap := TBitmap.Create;

  FCachedBitmap.Assign(Bitmap);
end;

procedure TForm1.LoadImageFromStream(Stream: TMemoryStream; const ContentType: string);
var
  Picture: TPicture;
begin
  if Stream.Size = 0 then
  begin
    WriteLn('ERROR: Stream is empty');
    raise Exception.Create('Image stream is empty');
  end;

  WriteLn('LoadImageFromStream - Size: ', Stream.Size, ', ContentType: ', ContentType);
  Stream.Position := 0;

  try
    WriteLn('Creating TPicture...');
    Picture := TPicture.Create;
    try
      WriteLn('Loading from stream...');
      Picture.LoadFromStream(Stream);
      WriteLn('Picture loaded: ', Picture.Width, 'x', Picture.Height);

      // Create or update cached bitmap
      if not Assigned(FCachedBitmap) then
      begin
        WriteLn('Creating new FCachedBitmap');
        FCachedBitmap := TBitmap.Create;
      end;

      WriteLn('Setting bitmap size: ', Picture.Width, 'x', Picture.Height);
      FCachedBitmap.Width := Picture.Width;
      FCachedBitmap.Height := Picture.Height;

      WriteLn('Drawing picture to bitmap...');
      FCachedBitmap.Canvas.Draw(0, 0, Picture.Graphic);
      WriteLn('Bitmap created successfully');

      //ResizeImage;
      FitAndCenter;

      zoomIn.Enabled := True;
      zoomOut.Enabled := True;
      SaveComicButton.Enabled := True;

      WriteLn('Image display complete');
    finally
      Picture.Free;
    end;
  except
    on E: Exception do
    begin
      WriteLn('ERROR in LoadImageFromStream: ', E.ClassName, ': ', E.Message);
      raise;
    end;
  end;
end;

procedure TForm1.ResizeImage;
var
  OriginalWidth, OriginalHeight: Integer;
  ScaledWidth, ScaledHeight: Integer;
  TargetWidth, TargetHeight: Integer;
  TempBitmap: TBitmap;
  ScaleX, ScaleY, BestScale: Double;
begin
  if not Assigned(FCachedBitmap) then
    Exit;

  OriginalWidth := FCachedBitmap.Width;
  OriginalHeight := FCachedBitmap.Height;

  TargetWidth := Image1.Width;
  TargetHeight := Image1.Height;

  if (OriginalWidth <= 0) or (OriginalHeight <= 0) or (TargetWidth <= 0) or (TargetHeight <= 0) then
    Exit;

  // Always compute best-fit scale
  ScaleX := TargetWidth / OriginalWidth;
  ScaleY := TargetHeight / OriginalHeight;

  //if ScaleX < ScaleY then
  //  BestScale := ScaleX
  //else
  //  BestScale := ScaleY;

  if not FIsPortrait then
    BestScale := ScaleX             // landscape: fit width
  else
    BestScale := Min(ScaleX, ScaleY); // portrait: fit whole page


  // Do not upscale beyond original size
  if BestScale > 2.0 then
    BestScale := 2.0;
  // or maybe try
  // if (BestScale > 1.0) and FIsPortrait then BestScale := 1.0;


  // If scale factor is 1.0 (initial load / "auto-fit requested"), apply best fit
  if FScaleFactor = 1.0 then
  begin
    FScaleFactor := BestScale;
    WriteLn('Auto-fit scale: ', FScaleFactor:0:3,
            ' (original: ', OriginalWidth, 'x', OriginalHeight,
            ', target: ', TargetWidth, 'x', TargetHeight, ')');
  end;

  ScaledWidth := Round(OriginalWidth * FScaleFactor);
  ScaledHeight := Round(OriginalHeight * FScaleFactor);

  // If we are in auto-fit mode (scale == best fit), always re-center
  if Abs(FScaleFactor - BestScale) < 0.0001 then
  begin
    FOffsetX := (TargetWidth - ScaledWidth) div 2;
    FOffsetY := (TargetHeight - ScaledHeight) div 2;
  end;
  WriteLn('Target=', TargetWidth, 'x', TargetHeight,
        ' Original=', OriginalWidth, 'x', OriginalHeight,
        ' ScaleX=', ScaleX:0:3, ' ScaleY=', ScaleY:0:3,
        ' Best=', BestScale:0:3);

  TempBitmap := TBitmap.Create;
  try
    TempBitmap.SetSize(TargetWidth, TargetHeight);
    TempBitmap.Canvas.Brush.Color := clBlack;
    TempBitmap.Canvas.FillRect(0, 0, TargetWidth, TargetHeight);

    TempBitmap.Canvas.StretchDraw(
      Rect(FOffsetX, FOffsetY, FOffsetX + ScaledWidth, FOffsetY + ScaledHeight),
      FCachedBitmap
    );

    Image1.Picture.Bitmap.Assign(TempBitmap);
  finally
    TempBitmap.Free;
  end;
end;

procedure TForm1.LoadCachedImage;
var
  Scale: Double;
  NewWidth, NewHeight: Integer;
begin
  if Assigned(FCachedBitmap) then
  begin
    Scale := Min(Image1.Width / FCachedBitmap.Width, Image1.Height / FCachedBitmap.Height);
    NewWidth := Round(FCachedBitmap.Width * Scale);
    NewHeight := Round(FCachedBitmap.Height * Scale);

    Image1.Picture.Bitmap.SetSize(Image1.Width, Image1.Height);
    Image1.Picture.Bitmap.Canvas.Brush.Color := clBlack;
    Image1.Picture.Bitmap.Canvas.FillRect(0, 0, Image1.Width, Image1.Height);

    Image1.Picture.Bitmap.SetSize(NewWidth, NewHeight);
    Image1.Picture.Bitmap.Canvas.StretchDraw(Rect(0, 0, NewWidth, NewHeight), FCachedBitmap);

    Image1.Left := 0;
    Image1.Top := 0;
  end;
end;

function TForm1.GetFileExtension(const ContentType: string): string;
begin
  if Pos('image/gif', ContentType) > 0 then
    Result := '.gif'
  else if Pos('image/jpeg', ContentType) > 0 then
    Result := '.jpg'
  else if Pos('image/png', ContentType) > 0 then
    Result := '.png'
  else
    Result := '';
end;

function TForm1.GetComicsDailyDir: string;
var
  HomeDir: string;
begin
  HomeDir := GetEnvironmentVariable('HOME');
  if HomeDir = '' then
    raise Exception.Create('Could not find home directory.');

  Result := IncludeTrailingPathDelimiter(HomeDir) + 'comics_daily';

  if not DirectoryExists(Result) then
    if not CreateDir(Result) then
      raise Exception.Create('Could not create comics_daily directory.');
end;

procedure TForm1.UpdateButtonStates;
begin
  if Assigned(FGoComics) then
  begin
    PrevButton.Enabled := FGoComics.HasPrevious;
    NextButton.Enabled := FGoComics.HasNext;

    WriteLn('Button states - Prev: ', BoolToStr(PrevButton.Enabled, True),
            ', Next: ', BoolToStr(NextButton.Enabled, True));
  end
  else
  begin
    PrevButton.Enabled := False;
    NextButton.Enabled := False;
  end;
end;

procedure TForm1.UpdateLayout;
var
  FormWidth, FormHeight: Integer;
begin
  WriteLn(' -x-x-x-x update layout -x-x-x-x');

  FormWidth := ClientWidth;
  FormHeight := ClientHeight;

  ShowComicButton.Left := FormWidth - lastButton.Width - ShowComicButton.Width - Margin - Margin;
  ShowComicButton.Top := FormHeight - ShowComicButton.Height - SaveComicButton.Height - PrevButton.Height - Margin * 3;

  PrevButton.Left := ShowComicButton.Left;
  PrevButton.Top := ShowComicButton.Top + ShowComicButton.Height + Margin;
  NextButton.Left := ShowComicButton.Left + ShowComicButton.Width - NextButton.Width;
  NextButton.Top := PrevButton.Top;

  lastButton.Left := FormWidth - LastButton.Width - Margin;
  lastButton.Top := PrevButton.Top;
  firstButton.Left := PrevButton.Left - firstButton.Width - Margin;
  firstButton.Top := PrevButton.Top;

  SaveComicButton.Left := ShowComicButton.Left;
  SaveComicButton.Top := PrevButton.Top + PrevButton.Height + Margin;

  ComboBox1.Left := Margin;
  ComboBox1.Top := firstButton.Top;

  zoomIn.Left := firstButton.Left;
  zoomIn.Top := ShowComicButton.Top;
  zoomOut.Left := lastButton.Left;
  zoomOut.Top := ShowComicButton.Top;

  if FormWidth < FormHeight then
  begin
    ComboBox1.Top := ShowComicButton.Top - ComboBox1.Height - Margin;
    ComboBox1.Left := ShowComicButton.Left;
  end;

  FComic_Section := (ShowComicButton.Top - Margin) / ClientHeight;
  Image1.SetBounds(0, 0, FormWidth, Round(FormHeight * FComic_Section));
  if Assigned(FCachedBitmap) then
  begin
    FScaleFactor := 1.0;
    FOffsetX := 0;
    FOffsetY := 0;
    ResizeImage;
  end;
  WriteLn(' exiting update layout');
end;

procedure TForm1.Image1MouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
var
  OldScaleFactor: Double;
  MouseOffsetX, MouseOffsetY: Integer;
begin
  OldScaleFactor := FScaleFactor;

  if WheelDelta > 0 then
    FScaleFactor := FScaleFactor * 1.1
  else
    FScaleFactor := FScaleFactor / 1.1;

  if FScaleFactor < 0.5 then
    FScaleFactor := 0.5;
  if FScaleFactor > 3.0 then
    FScaleFactor := 3.0;

  MouseOffsetX := MousePos.X - FOffsetX;
  MouseOffsetY := MousePos.Y - FOffsetY;

  FOffsetX := Round(MouseOffsetX * (FScaleFactor / OldScaleFactor - 1)) + FOffsetX;
  FOffsetY := Round(MouseOffsetY * (FScaleFactor / OldScaleFactor - 1)) + FOffsetY;

  ResizeImage;
  Handled := True;
end;

procedure TForm1.ZoomImage(ZoomFactor: Double);
var
  OldScaleFactor: Double;
  MousePos: TPoint;
  MouseOffsetX, MouseOffsetY: Integer;
begin
  OldScaleFactor := FScaleFactor;
  FScaleFactor := FScaleFactor * ZoomFactor;

  if FScaleFactor < 0.5 then
    FScaleFactor := 0.5;
  if FScaleFactor > 3.0 then
    FScaleFactor := 3.0;

  MousePos := Point(Image1.Width div 2, Image1.Height div 2);
  MouseOffsetX := MousePos.X - FOffsetX;
  MouseOffsetY := MousePos.Y - FOffsetY;

  FOffsetX := Round(MouseOffsetX * (FScaleFactor / OldScaleFactor - 1)) + FOffsetX;
  FOffsetY := Round(MouseOffsetY * (FScaleFactor / OldScaleFactor - 1)) + FOffsetY;

  ResizeImage;
end;

procedure TForm1.zoomInClick(Sender: TObject);
begin
  ZoomImage(1.1);
end;

procedure TForm1.zoomOutClick(Sender: TObject);
begin
  ZoomImage(0.9);
end;

procedure TForm1.StartPan(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  FIsPanning := True;
  FLastMouseX := X;
  FLastMouseY := Y;
end;

procedure TForm1.PerformPan(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
  if FIsPanning then
  begin
    FOffsetX := FOffsetX + (X - FLastMouseX);
    FOffsetY := FOffsetY + (Y - FLastMouseY);
    ResizeImage;
  end;
  FLastMouseX := X;
  FLastMouseY := Y;
end;

procedure TForm1.EndPan(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  FIsPanning := False;
end;

procedure TForm1.ComboBox1Change(Sender: TObject);
begin
  PrevButton.Enabled := False;
  NextButton.Enabled := False;
  FreeAndNil(FCachedBitmap);
  FreeAndNil(FGoComics);
end;

procedure TForm1.ShowComicButtonClick(Sender: TObject);
begin
  Self.Enabled := False;
  try
    if FIsComicLoaded and (FCurrentComic = ComboBox1.Text) then
    begin
      // Just reload current comic
      if Assigned(FCachedBitmap) then
        ResetAndReloadComic;
    end
    else
    begin
      // Load new comic feed
      FCurrentComic := ComboBox1.Text;

      FreeAndNil(FGoComics);
      FGoComics := TGoComics.Create(FCurrentComic);

      WriteLn('Loading RSS feed for: ', FCurrentComic);
      if FGoComics.LoadFeed then
      begin
        WriteLn('Feed loaded successfully');
        LoadCurrentComic;
        FIsComicLoaded := True;
      end
      else
      begin
        ShowMessage('Failed to load comic feed. Check console for details.');
        FIsComicLoaded := False;
      end;

      UpdateButtonStates;
    end;
  finally
    Self.Enabled := True;
  end;
end;

procedure TForm1.firstButtonClick(Sender: TObject);
begin
  // Not used in RSS version
end;

procedure TForm1.FormActivate(Sender: TObject);
var
  widget: PGtkWidget;
  atom: TGdkAtom;
  value: cardinal;
begin
  widget := PGtkWidget(Handle);
  if (widget <> nil) and (widget^.window <> nil) and (not FWinPropertySet) then
  begin
    FWinPropertySet := True;
    atom := gdk_atom_intern('_HILDON_PORTRAIT_MODE_SUPPORT', True);
    value := 1;
    if atom <> 0 then
    begin
      gdk_property_change(widget^.window, atom,
        gdk_x11_xatom_to_atom(XA_CARDINAL), 32,
        GDK_PROP_MODE_REPLACE, @value, 1);
    end;
  end;
end;

procedure TForm1.lastButtonClick(Sender: TObject);
begin
  // Not used in RSS version
end;

procedure TForm1.PrevButtonClick(Sender: TObject);
begin
  Self.Enabled := False;
  try
    if Assigned(FGoComics) and FGoComics.MoveToPrevious then
    begin
      LoadCurrentComic;
      UpdateButtonStates;
    end;
  finally
    Self.Enabled := True;
  end;
end;

procedure TForm1.NextButtonClick(Sender: TObject);
begin
  Self.Enabled := False;
  try
    if Assigned(FGoComics) and FGoComics.MoveToNext then
    begin
      LoadCurrentComic;
      UpdateButtonStates;
    end;
  finally
    Self.Enabled := True;
  end;
end;

procedure TForm1.FormResize(Sender: TObject);
begin
  WriteLn('form resize');
  UpdateLayout;
  if (FPrevClientWidth = 0) or (FPrevClientWidth <> ClientWidth) then
  begin
    ResizeImage;
  end;
  FPrevClientWidth := ClientWidth;
  FPrevClientHeight := ClientHeight;
end;

procedure TForm1.LoadCurrentComic;
begin
  if not Assigned(FGoComics) then
  begin
    WriteLn('GoComics object not initialized');
    Exit;
  end;

  WriteLn('Loading current comic from feed...');
  WriteLn('Current image URL: ', FGoComics.GetCurrentImageUrl);

  FreeAndNil(FComicStream);
  FComicStream := FGoComics.GetCurrentComic(FFileName, FContentType);

  if Assigned(FComicStream) and (FComicStream.Size > 0) then
  begin
    WriteLn('Comic downloaded, loading image...');
    try
      FScaleFactor := 1.0;
      FOffsetX := 0;
      FOffsetY := 0;

      LoadImageFromStream(FComicStream, FContentType);
      WriteLn('Comic loaded successfully');
    except
      on E: Exception do
      begin
        WriteLn('Error loading image: ', E.Message);
        ShowMessage('Error loading comic image: ' + E.Message);
      end;
    end;
  end
  else
  begin
    WriteLn('Failed to download comic');
    ShowMessage('Failed to download comic. Check console for details.');
  end;
end;

procedure TForm1.SaveComicButtonClick(Sender: TObject);
var
  SaveDir, SavePath: string;
  Ext: string;
  BaseFileName: string;
begin
  if not Assigned(FComicStream) or (FComicStream.Size = 0) then
  begin
    ShowMessage('No comic loaded to save.');
    Exit;
  end;

  try
    SaveDir := GetComicsDailyDir;
    Ext := GetFileExtension(FContentType);
    if Ext = '' then
      Ext := '.jpg';  // Default to .jpg

    // Create filename with proper extension
    if FFileName <> '' then
      BaseFileName := FFileName
    else
      BaseFileName := FormatDateTime('yyyy-mm-dd', Now);

    // Remove any existing extension from filename
    if Pos('.', BaseFileName) > 0 then
      BaseFileName := Copy(BaseFileName, 1, Pos('.', BaseFileName) - 1);

    SavePath := SaveDir + PathDelim + FCurrentComic + '_' + BaseFileName + Ext;

    WriteLn('Saving comic to: ', SavePath);
    FComicStream.Position := 0;
    FComicStream.SaveToFile(SavePath);

    ShowMessage('Comic saved to: ' + SavePath);
  except
    on E: Exception do
      ShowMessage('Error saving comic: ' + E.Message);
  end;
end;

end.
