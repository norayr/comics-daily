unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  FPImage, FPReadJPEG, FPReadPNG, FPReadGIF, FPWriteBMP, LazFileUtils,
  IntfGraphics, Math, {x11rotation,} GoComicsAPI, x, Gtk2, Gdk2, Gdk2x, xatom;

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
  private
    FPrevClientWidth, FPrevClientHeight: Integer;
    FWinPropertySet: boolean; //for hildon
    FIsComicLoaded: Boolean; // to not fetch when loaded
    FComicStream: TMemoryStream;
    FFileName: string;
    FContentType: string;
    FCurrentDate: TDateTime;
    FCurrentComic: string;
    FGoComics: TGoComics;
    FPrevComicUrl: string;
    FNextComicUrl: string;
    FFirstComicUrl: string;
    FLastComicUrl: string;
    //FRotationHandler: TX11Rotation;
    FCachedBitmap: TBitmap;   // Cached bitmap
    FIsPortrait: Boolean;       // Current orientation state

    FScaleFactor: Double;       // Zoom factor
    FOffsetX, FOffsetY: Integer; // Pan offsets
    // Variables to keep track of pan state
    FLastMouseX, FLastMouseY: Integer;
    FIsPanning: Boolean;

    FComic_Section: Real;
    procedure LoadImageFromStream(Stream: TMemoryStream; const ContentType: string);
    procedure ResizeImage;
    function GetComicsDailyDir: string;
    function GetFileExtension(const ContentType: string): string;
    procedure LoadComic(const Comic: string; const ComicDate: TDateTime);
    procedure LoadLatestComic(const Comic: string);
    procedure UpdateButtonStates;
    procedure UpdateLayout;
    procedure UpdateNavigationUrls;
    procedure HandleRotation(Sender: TObject; IsPortrait: Boolean);
    procedure CacheImage(Bitmap: TBitmap);
    procedure LoadCachedImage;

    // Methods for zooming and panning
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

procedure DisableControls;
begin


end;

procedure EnableControls;
begin

end;

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

  ComboBox1.ItemIndex := 22; // Select some item by defauln
  ComboBox1.ReadOnly := True;

  Memo1.Enabled := False;
  Memo1.Visible := False;

  Form1.Caption := 'comics daily';

  prevButton.Enabled := False;
  nextButton.Enabled := False;
  firstButton.Enabled := False;
  lastButton.Enabled := False;
  zoomIn.Enabled := False;
  zoomOut.Enabled := False;
  SaveComicButton.Enabled := False;
  UpdateLayout; // Adjust the layout based on the initial form size

  // Ensure FormShow event is connected
  Self.OnShow := @FormShow;
  Self.OnClose := @FormClose;
  FPrevClientWidth := 0;
  FPrevClientHeight := 0;

  FCachedBitmap := nil;

  FScaleFactor := 1.0;
  FOffsetX := 0;
  FOffsetY := 0;

  // Set up mouse event handlers for zooming and panning
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
  // Create and start the rotation handler
  //if not Assigned(FRotationHandler) then
  //begin
  //  FRotationHandler := TX11Rotation.Create(TWindow(Handle));
  //  FRotationHandler.OnRotation := @HandleRotation;
  //  FRotationHandler.Start;
  //  WriteLn('FormShow: Started rotation handler');
  //end;
end;

procedure TForm1.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  // Stop the rotation handler
  //if Assigned(FRotationHandler) then
  //begin
  //  FRotationHandler.Stop;
  //  FreeAndNil(FRotationHandler);
  //  WriteLn('FormClose: Stopped rotation handler');
  //end;

  FreeAndNil(FCachedBitmap);
end;

procedure TForm1.HandleRotation(Sender: TObject; IsPortrait: Boolean);
begin
  if IsPortrait then
    WriteLn('HandleRotation: Portrait mode detected')
  else
    WriteLn('HandleRotation: Landscape mode detected');

  FIsPortrait := IsPortrait;
  ResetAndReloadComic; // Recalculate and reload the image size to fit the new layout
end;



procedure TForm1.UpdateNavigationUrls;
begin
  FPrevComicUrl := FGoComics.PrevComicUrl;
  FNextComicUrl := FGoComics.NextComicUrl;
  FFirstComicUrl := FGoComics.FirstComicUrl;
  FLastComicUrl := FGoComics.LastComicUrl;

  // Show the navigation URLs for debugging purposes
  //ShowMessage('Prev: ' + FPrevComicUrl + ' Next: ' + FNextComicUrl + ' First: ' + FFirstComicUrl + ' Last: ' + FLastComicUrl);
end;

procedure TForm1.ComboBox1Change(Sender: TObject);
begin
  PrevButton.Enabled := False;
  NextButton.Enabled := False;
  firstButton.Enabled := False;
  lastButton.Enabled := False;
  FPrevComicUrl := '';
  FNextComicUrl := '';
  FFirstComicUrl := '';
  FLastComicUrl := '';

  // Clear cached image as the comic selection has changed
  FreeAndNil(FCachedBitmap);
end;

procedure TForm1.ShowComicButtonClick(Sender: TObject);
begin
  Self.Enabled := False;
  if FIsComicLoaded and (FCurrentComic = ComboBox1.Text) then
  begin
    // Reset and reload the comic from the cached stream
    if Assigned(FCachedBitmap) then
    begin
      ResetAndReloadComic;
    end;
  end
  else
  begin
    // Fetch and load the comic from the internet
    FCurrentComic := ComboBox1.Text;

    FreeAndNil(FGoComics); // Free previous instance if it exists
    FGoComics := TGoComics.Create(FCurrentComic); // Initialize GoComics object

    // Restore saved URLs
    FGoComics.PrevComicUrl := FPrevComicUrl;
    FGoComics.NextComicUrl := FNextComicUrl;
    FGoComics.FirstComicUrl := FFirstComicUrl;
    FGoComics.LastComicUrl := FLastComicUrl;

    FComicStream := TMemoryStream.Create; // Initialize FComicStream

    LoadLatestComic(FCurrentComic);
    UpdateButtonStates;
    FIsComicLoaded := True;
  end;
  Self.Enabled := True;
end;


procedure TForm1.firstButtonClick(Sender: TObject);
begin
  Self.Enabled := False;
  if FGoComics.FirstComicUrl <> '' then
  begin
    FCurrentDate := FGoComics.FirstComicDate;
    LoadComic(FCurrentComic, FCurrentDate);
    UpdateNavigationUrls;
    UpdateButtonStates;
  end;
  Self.Enabled := True;
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
  Self.Enabled := False;
  LoadLatestComic(FCurrentComic);
  UpdateButtonStates;
  Self.Enabled := True;
end;

procedure TForm1.PrevButtonClick(Sender: TObject);
begin
  Self.Enabled := False;
  if FGoComics.PrevComicUrl <> '' then
  begin
    FCurrentDate := FGoComics.PrevComicDate;
    LoadComic(FCurrentComic, FCurrentDate);
    UpdateNavigationUrls;
    UpdateButtonStates;
  end;
  Self.Enabled := True;
end;

procedure TForm1.NextButtonClick(Sender: TObject);
begin
  Self.Enabled := False;
  if (FGoComics.NextComicUrl <> '') and (FCurrentDate < Now) then
  begin
    FCurrentDate := FGoComics.NextComicDate;
    LoadComic(FCurrentComic, FCurrentDate);
    UpdateNavigationUrls;
    UpdateButtonStates;
  end;
  Self.Enabled := True;
end;

procedure TForm1.FormResize(Sender: TObject);
begin
  WriteLn('form resize');
  UpdateLayout; // Adjust the layout when the form is resized
  if (FPrevClientWidth = 0) or (FPrevClientWidth <> ClientWidth) then
  begin
    ResizeImage; // Resize the image when the form is resized
  end;
  FPrevClientWidth := ClientWidth;
  FPrevClientHeight := ClientHeight;
  //Refresh;
end;

procedure TForm1.LoadLatestComic(const Comic: string);
var
  LatestComicUrl, DateStr: string;
  ComicDate: TDateTime;
begin
  if Comic = '' then
  begin
    Memo1.Lines.Add('No comic selected.');
    Exit;
  end;

  Memo1.Lines.Add('Fetching the latest comic for ' + Comic + '...');
  try
    LatestComicUrl := FGoComics.GetLatestComicUrl;
    DateStr := Copy(LatestComicUrl, Length(LatestComicUrl) - 9, 10);
    ComicDate := EncodeDate(StrToInt(Copy(DateStr, 1, 4)),
      StrToInt(Copy(DateStr, 6, 2)),
      StrToInt(Copy(DateStr, 9, 2)));
    FCurrentDate := ComicDate;
    LoadComic(Comic, ComicDate);
    UpdateNavigationUrls;
    UpdateButtonStates;
  finally
    // GoComics object is managed globally now, no need to free here
  end;
end;

procedure TForm1.LoadComic(const Comic: string; const ComicDate: TDateTime);
var
  RetryDate: TDateTime;
begin
  if Comic = '' then
  begin
    Memo1.Lines.Add('No comic selected.');
    Exit;
  end;
  Form1.Caption := Comic + ' ' + DateToStr(ComicDate);
  Memo1.Lines.Add('Fetching comic for ' + Comic + ' on ' + DateToStr(ComicDate) + '...');
  try
    FreeAndNil(FComicStream); // Free previous instance if it exists
    FComicStream := TMemoryStream.Create;
    try
      try
        FComicStream := FGoComics.GetImageUrl(ComicDate, FFileName, FContentType);
      except
        on E: Exception do
        begin
          Memo1.Lines.Add('Error fetching comic for ' + DateToStr(ComicDate) + ': ' + E.Message);
          RetryDate := ComicDate - 1; // Try the previous day
          Memo1.Lines.Add('Retrying with previous day''s comic...');
          try
            FComicStream := FGoComics.GetImageUrl(RetryDate, FFileName, FContentType);
          except
            on E: Exception do
            begin
              Memo1.Lines.Add('Error fetching comic for ' + DateToStr(RetryDate) + ': ' + E.Message);
              raise;
            end;
          end;
          FCurrentDate := RetryDate;
        end;
      end;

      Memo1.Lines.Add('Content Type: ' + FContentType); // Log content type for debugging
      if Pos('text/html', FContentType) > 0 then
        raise Exception.Create('Received HTML instead of image. Please check the image URL.');

      LoadImageFromStream(FComicStream, FContentType);
      FCurrentDate := ComicDate;
      Memo1.Lines.Add('Comic displayed successfully for ' + DateToStr(FCurrentDate));
      SaveComicButton.Enabled := True;
    finally
      // Do not free FComicStream here as it is still needed for the image display
    end;
  except
    on E: Exception do
    begin
      Memo1.Lines.Add('Error: ' + E.Message);
    end;
  end;

  // Save the current navigation URLs
  UpdateNavigationUrls;
end;

procedure TForm1.SaveComicButtonClick(Sender: TObject);
var
  FileStream: TFileStream;
  FullFilePath: string;
  Extension: string;
begin
  if Assigned(FComicStream) and (FComicStream.Size > 0) then
  begin
    Extension := GetFileExtension(FContentType);
    FullFilePath := IncludeTrailingPathDelimiter(GetComicsDailyDir) + ChangeFileExt(FFileName, Extension);
    FileStream := TFileStream.Create(FullFilePath, fmCreate);
    try
      FComicStream.Position := 0;
      FileStream.CopyFrom(FComicStream, FComicStream.Size);
      Memo1.Lines.Add('Comic saved successfully as ' + FullFilePath);
    finally
      FileStream.Free;
    end;
  end
  else
    Memo1.Lines.Add('No comic to save.');
end;

procedure TForm1.LoadImageFromStream(Stream: TMemoryStream; const ContentType: string);
var
  Img: TFPMemoryImage;
  Reader: TFPCustomImageReader;
  Bitmap: TBitmap;
  x, y: Integer;
  Color2: TFPColor;
  Scale: Double;
  NewWidth, NewHeight: Integer;
  InsectPositionX: Integer;
  MovingInsect: TImage;
  InsectPaths: array[0..2] of string;
  InsectImages: array[0..2] of TPicture;
  i, InsectFrameIndex: Integer;
  FrameChangeInterval: Integer;
  FrameChangeCounter: Integer;
begin
  Img := TFPMemoryImage.Create(0, 0);
  try
    if Pos('jpeg', ContentType) > 0 then
      Reader := TFPReaderJPEG.Create
    else if Pos('png', ContentType) > 0 then
      Reader := TFPReaderPNG.Create
    else if Pos('gif', ContentType) > 0 then
      Reader := TFPReaderGIF.Create
    else
      raise Exception.Create('Unsupported image format: ' + ContentType);

    try
      Stream.Position := 0;
      Img.LoadFromStream(Stream, Reader);

      // Initialize the insect image paths
      InsectPaths[0] := '/usr/share/pixmaps/comics-daily/comics-daily-insect-lf.png';
      InsectPaths[1] := '/usr/share/pixmaps/comics-daily/comics-daily-insect.png';
      InsectPaths[2] := '/usr/share/pixmaps/comics-daily/comics-daily-insect-rf.png';

      // Load the insect images
      for i := 0 to 2 do
      begin
        if not FileExists(InsectPaths[i]) then
          InsectPaths[i] := ExtractFilePath(ParamStr(0)) + ExtractFileName(InsectPaths[i]); // Look in current directory

        if FileExists(InsectPaths[i]) then
        begin
          InsectImages[i] := TPicture.Create;
          InsectImages[i].LoadFromFile(InsectPaths[i]);
        end
        else
          raise Exception.Create('Insect image not found: ' + InsectPaths[i]);
      end;

      // Create and configure the moving insect image
      MovingInsect := TImage.Create(Self);
      MovingInsect.Parent := Self;
      MovingInsect.Transparent := True;
      MovingInsect.SetBounds(0, 0, 48, 48); // Initial position and size
      MovingInsect.Top := Round(ClientHeight * FComic_Section + margin);
      MovingInsect.Visible := True; // Show the moving insect

      // Set initial frame
      InsectFrameIndex := 0;
      MovingInsect.Picture.Assign(InsectImages[InsectFrameIndex]);

      // Initialize frame change variables
      FrameChangeInterval := 5; // Change frame every 5 pixels
      FrameChangeCounter := 0;

      // Convert TFPMemoryImage to TBitmap
      Bitmap := TBitmap.Create;
      try
        Bitmap.SetSize(Img.Width, Img.Height);
        Bitmap.PixelFormat := pf24bit;

        for y := 0 to Img.Height - 1 do
        begin
          for x := 0 to Img.Width - 1 do
          begin
            Color2 := Img.Colors[x, y];
            Bitmap.Canvas.Pixels[x, y] := RGBToColor(Color2.red shr 8, Color2.green shr 8, Color2.blue shr 8);
          end;

          // Move the insect image
          InsectPositionX := (y * ClientWidth) div Img.Height;
          MovingInsect.Left := InsectPositionX;

          // Update the frame counter
          Inc(FrameChangeCounter);
          if FrameChangeCounter >= FrameChangeInterval then
          begin
            // Move to the next frame in the sequence
            InsectFrameIndex := (InsectFrameIndex + 1) mod 4; // There are 4 frames in the sequence
            case InsectFrameIndex of
              0: MovingInsect.Picture.Assign(InsectImages[0]); // comics-daily-insect-lf.png
              1: MovingInsect.Picture.Assign(InsectImages[1]); // comics-daily-insect.png
              2: MovingInsect.Picture.Assign(InsectImages[2]); // comics-daily-insect-rf.png
              3: MovingInsect.Picture.Assign(InsectImages[1]); // comics-daily-insect.png
            end;
            FrameChangeCounter := 0;
          end;

          // Force a repaint to update the position of the insect image
          Application.ProcessMessages;
          // Optional delay
          // Sleep(10); // Adjust as needed

          // Uncomment the following line for debugging
          // writeln('Insect x=', MovingInsect.Left, ' y=', MovingInsect.Top);
        end;

        // Hide the moving insect
        MovingInsect.Visible := False;
        MovingInsect.Free; // Free the insect image

        // Free the insect images
        for i := 0 to 2 do
          InsectImages[i].Free;

        // Cache the bitmap
        CacheImage(Bitmap);

        // Calculate scaling to fit within Image1 while preserving aspect ratio
        Scale := Min(Image1.Width / Bitmap.Width, Image1.Height / Bitmap.Height);
        NewWidth := Round(Bitmap.Width * Scale);
        NewHeight := Round(Bitmap.Height * Scale);

        // Clear the image before drawing and set the background to black
        Image1.Picture.Bitmap.SetSize(Image1.Width, Image1.Height);
        Image1.Picture.Bitmap.Canvas.Brush.Color := clBlack; // Set background to black
        Image1.Picture.Bitmap.Canvas.FillRect(0, 0, Image1.Width, Image1.Height);

        // Resize Image1 to fit the scaled image
        Image1.Picture.Bitmap.SetSize(NewWidth, NewHeight);
        Image1.Picture.Bitmap.Canvas.StretchDraw(Rect(0, 0, NewWidth, NewHeight), Bitmap);

        // Center the image in the middle of Image1
        Image1.Left := 0;
        Image1.Top := 0;
      finally
        Bitmap.Free;
      end;
    finally
      Reader.Free;
    end;
  finally
    Img.Free;
  end;
end;


procedure TForm1.ResetAndReloadComic;
begin
  // Reset the scale factor and offsets
  FScaleFactor := 1.0;
  FOffsetX := 0;
  FOffsetY := 0;

  // Reset the layout
  UpdateLayout;

  // Load the cached image
  LoadCachedImage;
end;

procedure TForm1.ResizeImage;
var
  NewWidth, NewHeight: Integer;
  SrcRect, DestRect: TRect;
begin
  if Assigned(FComicStream) and (FComicStream.Size > 0) then
  begin
    // Check if a cached image exists
    if Assigned(FCachedBitmap) then
    begin
      // Calculate new dimensions based on the scale factor
      NewWidth := Round(FCachedBitmap.Width * FScaleFactor);
      NewHeight := Round(FCachedBitmap.Height * FScaleFactor);

      // Create source and destination rectangles for stretching/cropping
      SrcRect := Rect(0, 0, FCachedBitmap.Width, FCachedBitmap.Height);

      // Ensure the new dimensions fit within the Image1 boundaries
      DestRect.Left := FOffsetX;
      DestRect.Top := FOffsetY;
      DestRect.Right := DestRect.Left + NewWidth;
      DestRect.Bottom := DestRect.Top + NewHeight;

      // Clear the image before drawing
      Image1.Picture.Bitmap.SetSize(Image1.Width, Image1.Height);
      Image1.Picture.Bitmap.Canvas.Brush.Color := clBlack;
      Image1.Picture.Bitmap.Canvas.FillRect(0, 0, Image1.Width, Image1.Height);

      // Draw the cached bitmap onto the Image1 canvas
      Image1.Picture.Bitmap.Canvas.StretchDraw(DestRect, FCachedBitmap);

      // Ensure the image remains within the boundaries of Image1
      if DestRect.Left > 0 then
        FOffsetX := 0
      else if DestRect.Right < Image1.Width then
        FOffsetX := Image1.Width - NewWidth;

      if DestRect.Top > 0 then
        FOffsetY := 0
      else if DestRect.Bottom < Image1.Height then
        FOffsetY := Image1.Height - NewHeight;

      // Prevent offsets from going out of bounds
      if NewWidth <= Image1.Width then
        FOffsetX := 0;
      if NewHeight <= Image1.Height then
        FOffsetY := 0;
    end
    else
    begin
      // If no cached image is available, load from the stream
      FComicStream.Position := 0;
      LoadImageFromStream(FComicStream, FContentType);
    end;
  end;
end;

procedure TForm1.CacheImage(Bitmap: TBitmap);
begin
  if not Assigned(FCachedBitmap) then
    FCachedBitmap := TBitmap.Create;
  FCachedBitmap.Assign(Bitmap);
end;

procedure TForm1.LoadCachedImage;
var
  Scale: Double;
  NewWidth, NewHeight: Integer;
begin
  if Assigned(FCachedBitmap) then
  begin
    // Calculate scaling to fit within Image1 while preserving aspect ratio
    Scale := Min(Image1.Width / FCachedBitmap.Width, Image1.Height / FCachedBitmap.Height);
    NewWidth := Round(FCachedBitmap.Width * Scale);
    NewHeight := Round(FCachedBitmap.Height * Scale);

    // Clear the image before drawing and set the background to black
    Image1.Picture.Bitmap.SetSize(Image1.Width, Image1.Height);
    Image1.Picture.Bitmap.Canvas.Brush.Color := clBlack;
    Image1.Picture.Bitmap.Canvas.FillRect(0, 0, Image1.Width, Image1.Height);

    // Resize Image1 to fit the scaled image
    Image1.Picture.Bitmap.SetSize(NewWidth, NewHeight);
    Image1.Picture.Bitmap.Canvas.StretchDraw(Rect(0, 0, NewWidth, NewHeight), FCachedBitmap);

    // Center the image in the middle of Image1
    Image1.Left := 0; //(ClientWidth - Image1.Width) div 2;
    Image1.Top := 0; //(ClientHeight - Image1.Height) div 2;
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
    Result := ''; // Default or raise an error for unsupported types
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
var
  HasValidNext: Boolean;
begin
  PrevButton.Enabled := (FGoComics.PrevComicUrl <> '') and (FGoComics.PrevComicDate > 0);
  NextButton.Enabled := (FGoComics.NextComicUrl <> '') and (FGoComics.NextComicDate > 0) and (FGoComics.NextComicDate <= Now);
  firstButton.Enabled := PrevButton.Enabled;
  lastButton.Enabled  := NextButton.Enabled;
//  zoomIn.Enabled  := PrevButton.Enabled or NextButton.Enabled;
//  zoomOut.Enabled := PrevButton.Enabled or NextButton.Enabled;
  //firstButton.Enabled := FCurrentDate <> FGoComics.FirstComicDate;
  firstButton.Enabled := PrevButton.Enabled;
  lastButton.Enabled := NextButton.Enabled;


  zoomIn.Enabled := PrevButton.Enabled or NextButton.Enabled;
  zoomOut.Enabled := PrevButton.Enabled or NextButton.Enabled;
  {if NextButton.Enabled = False then
  begin
    lastButton.Enabled := False
  end
 else
  begin
  lastButton.Enabled := FCurrentDate <> FGoComics.LastComicDate
  end;
  }
end;

procedure TForm1.UpdateLayout;
var
  FormWidth, FormHeight: Integer;
  ComboBoxRight, SaveButtonLeft, SaveButtonRight: Integer;
begin
  WriteLn(' -x-x-x-x update layout -x-x-x-x');
  WriteLn('scr width: ', Screen.Width);
  WriteLn('scr height: ', Screen.Height);
  WriteLn('client width: ', ClientWidth);
  WriteLn('client height: ', ClientHeight);
  WriteLn('form width: ', FormWidth);
  WriteLn('form height: ', FormHeight);
  // Use Screen dimensions for layout calculation
  FormWidth := ClientWidth;
  FormHeight := ClientHeight;
  WriteLn('-x-x-x-x-x-x-x-x-x-');
  WriteLn('client width: ', ClientWidth);
  WriteLn('client height: ', ClientHeight);
  WriteLn('form width: ', FormWidth);
  WriteLn('form height: ', FormHeight);
  WriteLn('-x-x-x-x-x-x-x-x-x-');
  WriteLn;

  // Position ShowComicButton
  ShowComicButton.Left := FormWidth - lastButton.Width - ShowComicButton.Width - Margin - Margin;
  ShowComicButton.Top := FormHeight - ShowComicButton.Height - SaveComicButton.Height - PrevButton.Height - Margin * 3;
  WriteLn('show button ', ShowComicButton.Left, ' ', ShowComicButton.Top);

  // Position PrevButton and NextButton
  PrevButton.Left := ShowComicButton.Left;
  PrevButton.Top := ShowComicButton.Top + ShowComicButton.Height + Margin;
  NextButton.Left := ShowComicButton.Left + ShowComicButton.Width - NextButton.Width;
  NextButton.Top := PrevButton.Top;

  lastButton.Left := FormWidth - LastButton.Width - Margin;
  lastButton.Top := PrevButton.Top;
  firstButton.Left := PrevButton.Left - firstButton.Width - Margin;
  firstButton.Top := PrevButton.Top;

  // Position SaveComicButton
  SaveComicButton.Left := ShowComicButton.Left;
  SaveComicButton.Top := PrevButton.Top + PrevButton.Height + Margin;

  // Position ComboBox1
  ComboBox1.Left := Margin;
  ComboBox1.Top := firstButton.Top;

  // Calculate right boundaries for overlap detection
  ComboBoxRight := ComboBox1.Left + ComboBox1.Width;
  SaveButtonLeft := SaveComicButton.Left;
  SaveButtonRight := SaveComicButton.Left + SaveComicButton.Width;
  zoomIn.Left := firstButton.Left;
  zoomIn.Top := ShowComicButton.Top;
  zoomOut.Left := lastButton.Left;
  zoomOut.Top := ShowComicButton.Top;

  if FormWidth < FormHeight then
  begin
    ComboBox1.Top := ShowComicButton.Top - ComboBox1.Height - Margin;
    ComboBox1.Left := ShowComicButton.Left;
  end;

  // Resize and position Image1
  FComic_Section := (ShowComicButton.Top - Margin) / ClientHeight;
  Image1.SetBounds(0, 0, FormWidth, Round(FormHeight * FComic_Section));
  WriteLn(' exiting update layout');
end;


// Methods for zooming and panning
procedure TForm1.Image1MouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
var
  OldScaleFactor: Double;
  MouseOffsetX, MouseOffsetY: Integer;
begin
  // Save the old scale factor
  OldScaleFactor := FScaleFactor;

  // Adjust the scale factor based on the mouse wheel direction
  if WheelDelta > 0 then
    FScaleFactor := FScaleFactor * 1.1
  else
    FScaleFactor := FScaleFactor / 1.1;

  // Clamp the scale factor to reasonable limits
  if FScaleFactor < 0.5 then
    FScaleFactor := 0.5;
  if FScaleFactor > 3.0 then
    FScaleFactor := 3.0;

  // Calculate the mouse offsets based on the old scale factor
  MouseOffsetX := MousePos.X - FOffsetX;
  MouseOffsetY := MousePos.Y - FOffsetY;

  // Adjust the offsets to zoom by the mouse point
  FOffsetX := Round(MouseOffsetX * (FScaleFactor / OldScaleFactor - 1)) + FOffsetX;
  FOffsetY := Round(MouseOffsetY * (FScaleFactor / OldScaleFactor - 1)) + FOffsetY;

  // Redraw the image with the new scale factor
  ResizeImage;

  Handled := True;
end;

procedure TForm1.ZoomImage(ZoomFactor: Double);
var
  OldScaleFactor: Double;
  MousePos: TPoint;
  MouseOffsetX, MouseOffsetY: Integer;
begin
  // Save the old scale factor
  OldScaleFactor := FScaleFactor;

  // Adjust the scale factor based on the zoom factor
  FScaleFactor := FScaleFactor * ZoomFactor;

  // Clamp the scale factor to reasonable limits
  if FScaleFactor < 0.5 then
    FScaleFactor := 0.5;
  if FScaleFactor > 3.0 then
    FScaleFactor := 3.0;

  // Get the center point of the Image1
  MousePos := Point(Image1.Width div 2, Image1.Height div 2);

  // Calculate the mouse offsets based on the old scale factor
  MouseOffsetX := MousePos.X - FOffsetX;
  MouseOffsetY := MousePos.Y - FOffsetY;

  // Adjust the offsets to zoom by the center point
  FOffsetX := Round(MouseOffsetX * (FScaleFactor / OldScaleFactor - 1)) + FOffsetX;
  FOffsetY := Round(MouseOffsetY * (FScaleFactor / OldScaleFactor - 1)) + FOffsetY;

  // Redraw the image with the new scale factor
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
  // Initialize pan state
  FIsPanning := True;
  FLastMouseX := X;
  FLastMouseY := Y;
end;

procedure TForm1.PerformPan(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
  if FIsPanning then
  begin
    // Calculate the offset based on the mouse movement
    FOffsetX := FOffsetX + (X - FLastMouseX);
    FOffsetY := FOffsetY + (Y - FLastMouseY);

    // Redraw the image with the new offset
    ResizeImage;
  end;
  FLastMouseX := X;
  FLastMouseY := Y;
end;

procedure TForm1.EndPan(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  FIsPanning := False;
end;
end.

