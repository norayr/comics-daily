unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  FPImage, FPReadJPEG, FPReadPNG, FPReadGIF, FPWriteBMP,
  LazFileUtils, IntfGraphics, Math,
  GoComicsAPI;

const
  comic_section = 0.8;

type

  { TForm1 }

  TForm1 = class(TForm)
    lastButton: TButton;
    firstButton: TButton;
    ComboBox1: TComboBox;
    PrevButton: TButton;
    NextButton: TButton;
    ShowComicButton: TButton;
    SaveComicButton: TButton;
    Memo1: TMemo;
    Image1: TImage;
    procedure ComboBox1Change(Sender: TObject);
    procedure firstButtonClick(Sender: TObject);
    procedure lastButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure ShowComicButtonClick(Sender: TObject);
    procedure SaveComicButtonClick(Sender: TObject);
    procedure PrevButtonClick(Sender: TObject);
    procedure NextButtonClick(Sender: TObject);
  private
    FComicStream: TMemoryStream;
    FFileName: string;
    FContentType: string;
    FCurrentDate: TDateTime;
    FCurrentComic: string;
    FGoComics: TGoComics;
    procedure LoadImageFromStream(Stream: TMemoryStream; const ContentType: string);
    procedure ResizeImage;
    function GetComicsDailyDir: string;
    function GetFileExtension(const ContentType: string): string;
    procedure LoadComic(const Comic: string; const ComicDate: TDateTime);
    procedure LoadLatestComic(const Comic: string);
    procedure UpdateButtonStates;
    procedure UpdateLayout;
  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
  ComboBox1.Items.Add('andycapp');
  ComboBox1.Items.Add('calvinandhobbes');
  ComboBox1.Items.Add('cats-cafe');
  ComboBox1.Items.Add('citizendog');
  ComboBox1.Items.Add('dinosaur-comics');
  ComboBox1.Items.Add('fminus');
  ComboBox1.Items.Add('fredbasset');
  ComboBox1.Items.Add('garfield');
  ComboBox1.Items.Add('liz-climo-cartoons');
  ComboBox1.Items.Add('lola');
  ComboBox1.Items.Add('peanuts');
  ComboBox1.Items.Add('pearlsbeforeswine');
  ComboBox1.Items.Add('perry-bible-fellowship');
  ComboBox1.Items.Add('poorly-drawn-lines');
  ComboBox1.Items.Add('sarahs-scribbles');
  ComboBox1.Items.Add('savage-chickens');
  ComboBox1.Items.Add('wizardofid');

  ComboBox1.ItemIndex := 0; // Select the first item by default

  Memo1.Enabled := False;
  Memo1.Visible := False;

  Form1.Caption:= 'comics daily';

//  FCurrentComic := ComboBox1.Text; // Initialize the current comic
//  FGoComics := TGoComics.Create(FCurrentComic); // Initialize GoComics object
//  FComicStream := TMemoryStream.Create; // Initialize FComicStream
//  LoadLatestComic(FCurrentComic); // Load the latest comic on startup
//  UpdateButtonStates;
  prevButton.Enabled:= False; nextButton.Enabled:= False; firstButton.Enabled:= False; lastButton.Enabled := False;
  SaveComicButton.Enabled:= False;
  UpdateLayout; // Adjust the layout based on the initial form size
end;

procedure TForm1.firstButtonClick(Sender: TObject);
begin
  if FGoComics.FirstComicUrl <> '' then
  begin
    FCurrentDate := FGoComics.FirstComicDate;
    LoadComic(FCurrentComic, FCurrentDate);
    FGoComics.PrevComicUrl := '';
    UpdateButtonStates;
    //firstButton.Enabled := False;
    //prevButton.Enabled := False;
    //nextButton.Enabled := True;
    //lastButton.Enabled := True;
  end;
end;

procedure TForm1.ComboBox1Change(Sender: TObject);
begin
  PrevButton.Enabled := False;
  NextButton.Enabled := False;
  firstButton.Enabled := False;
  lastButton.Enabled := False;
end;


procedure TForm1.lastButtonClick(Sender: TObject);
begin
  LoadLatestComic(FCurrentComic);
  FGoComics.NextComicUrl := '';
  UpdateButtonStates;
  //firstButton.Enabled := True;
  //prevButton.Enabled := True;
  //nextButton.Enabled := False;
  //lastButton.Enabled := False;
end;

procedure TForm1.FormResize(Sender: TObject);
begin
  UpdateLayout; // Adjust the layout when the form is resized
  ResizeImage; // Resize the image when the form is resized
end;

procedure TForm1.ShowComicButtonClick(Sender: TObject);
begin
  FCurrentComic := ComboBox1.Text;
  FreeAndNil(FGoComics); // Free previous instance if it exists
  FGoComics := TGoComics.Create(FCurrentComic); // Initialize GoComics object

  // i guess only this has to be added?
  FComicStream := TMemoryStream.Create; // Initialize FComicStream


  LoadLatestComic(FCurrentComic);
  UpdateButtonStates;
end;

procedure TForm1.PrevButtonClick(Sender: TObject);
begin
  if FGoComics.PrevComicUrl <> '' then
  begin
    FCurrentDate := FGoComics.PrevComicDate;
    LoadComic(FCurrentComic, FCurrentDate);
    UpdateButtonStates;
  end;
end;

procedure TForm1.NextButtonClick(Sender: TObject);
begin
  if (FGoComics.NextComicUrl <> '') and (FCurrentDate < Now) then
  begin
    FCurrentDate := FGoComics.NextComicDate;
    LoadComic(FCurrentComic, FCurrentDate);
    UpdateButtonStates;
  end;
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
      raise Exception.Create('Unsupported image format: ' + ContentType); // Improved error message

    try
      Stream.Position := 0;
      Img.LoadFromStream(Stream, Reader);

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
        end;

        // Calculate scaling to fit within Image1 while preserving aspect ratio
        Scale := Min(Image1.Width / Bitmap.Width, Image1.Height / Bitmap.Height);
        NewWidth := Round(Bitmap.Width * Scale);
        NewHeight := Round(Bitmap.Height * Scale);

        // Resize Image1 to fit the scaled image
        Image1.Picture.Bitmap.SetSize(NewWidth, NewHeight);
        Image1.Picture.Bitmap.Canvas.StretchDraw(Rect(0, 0, NewWidth, NewHeight), Bitmap);

        // Center the image in the middle of Image1
        Image1.Left := 0; //(ClientWidth - Image1.Width) div 2;
        Image1.Top := 0; //(ClientHeight - Image1.Height) div 2;
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

procedure TForm1.ResizeImage;
begin
  if Assigned(FComicStream) and (FComicStream.Size > 0) then
  begin
    FComicStream.Position := 0;
    LoadImageFromStream(FComicStream, FContentType);
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
begin
  PrevButton.Enabled := FGoComics.PrevComicUrl <> '';
  NextButton.Enabled := (FGoComics.NextComicUrl <> '') and (FCurrentDate < Now);
  lastButton.Enabled := NextButton.Enabled;
  firstButton.Enabled := PrevButton.Enabled;
end;

procedure TForm1.UpdateLayout;
const
  Margin = 10;
var
  FormWidth, FormHeight: Integer;
begin
  FormWidth := ClientWidth;
  FormHeight := ClientHeight;

  // Resize and position Image1
  Image1.SetBounds(0, 0, FormWidth, Round(FormHeight * comic_section));


  // Position ShowComicButton
  ShowComicButton.Left := FormWidth - lastButton.Width - ShowComicButton.Width - Margin - Margin;
  //ShowComicButton.Top := Min(Image1.Height, Round(FormHeight * comic_section)) + Margin * 2;
  //  ShowComicButton.Top := Image1.Picture.Bitmap.Height + ShowcomicButton.Height + Margin;
  ShowComicButton.Top := FormHeight - ShowComicButton.Height - SaveComicButton.Height - PrevButton.Height - Margin * 3;

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
  ComboBox1.Top := SaveComicButton.Top;
end;


end.

