unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  FPImage, FPReadJPEG, FPReadPNG, FPReadGIF, FPWriteBMP,
  LazFileUtils, IntfGraphics, Math,
  GoComicsAPI;

type

  { TForm1 }

  TForm1 = class(TForm)
    ComboBox1: TComboBox;
    PrevButton: TButton;
    NextButton: TButton;
    ShowComicButton: TButton;
    SaveComicButton: TButton;
    Memo1: TMemo;
    Image1: TImage;
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
    procedure LoadImageFromStream(Stream: TMemoryStream; const ContentType: string);
    procedure ResizeImage;
    function GetComicsDailyDir: string;
    function GetFileExtension(const ContentType: string): string;
    procedure LoadComic(const Comic: string; const ComicDate: TDateTime);
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

  FCurrentComic := ComboBox1.Text; // Initialize the current comic
  FCurrentDate := Now; // Set the current date to today
  UpdateButtonStates;
  UpdateLayout; // Adjust the layout based on the initial form size
end;

procedure TForm1.FormResize(Sender: TObject);
begin
  UpdateLayout; // Adjust the layout when the form is resized
  ResizeImage; // Resize the image when the form is resized
end;

procedure TForm1.ShowComicButtonClick(Sender: TObject);
begin
  FCurrentComic := ComboBox1.Text;
  FCurrentDate := Now; // Reset to today when a new comic is selected
  LoadComic(FCurrentComic, FCurrentDate);
  UpdateButtonStates;
end;

procedure TForm1.PrevButtonClick(Sender: TObject);
begin
  FCurrentDate := FCurrentDate - 1;
  LoadComic(FCurrentComic, FCurrentDate);
  UpdateButtonStates;
end;

procedure TForm1.NextButtonClick(Sender: TObject);
begin
  if FCurrentDate < Now then
  begin
    FCurrentDate := FCurrentDate + 1;
    LoadComic(FCurrentComic, FCurrentDate);
    UpdateButtonStates;
  end;
end;

procedure TForm1.LoadComic(const Comic: string; const ComicDate: TDateTime);
var
  GoComics: TGoComics;
  RetryDate: TDateTime;
begin
  if Comic = '' then
  begin
    Memo1.Lines.Add('No comic selected.');
    Exit;
  end;

  Memo1.Lines.Add('Fetching comic for ' + Comic + ' on ' + DateToStr(ComicDate) + '...');
  try
    GoComics := TGoComics.Create(Comic);
  except
    on E: Exception do
    begin
      Memo1.Lines.Add('Error creating comic endpoint: ' + E.Message);
      Exit;
    end;
  end;

  FComicStream := TMemoryStream.Create;
  try
    try
      FComicStream := GoComics.GetImageUrl(ComicDate, FFileName, FContentType);
    except
      on E: Exception do
      begin
        Memo1.Lines.Add('Error fetching comic for ' + DateToStr(ComicDate) + ': ' + E.Message);
        RetryDate := ComicDate - 1; // Try the previous day
        Memo1.Lines.Add('Retrying with previous day''s comic...');
        try
          FComicStream := GoComics.GetImageUrl(RetryDate, FFileName, FContentType);
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
    GoComics.Free;
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
        Image1.Left := (ClientWidth - Image1.Width) div 2;
        Image1.Top := (ClientHeight - Image1.Height) div 2;
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
  PrevButton.Enabled := True;
  NextButton.Enabled := FCurrentDate < Date;
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
  Image1.SetBounds(0, 0, FormWidth, Round(FormHeight * 0.8));

  // Position ShowComicButton
  ShowComicButton.Left := FormWidth - ShowComicButton.Width - Margin;
  ShowComicButton.Top := Image1.Height + Margin;

  // Position PrevButton and NextButton
  PrevButton.Left := ShowComicButton.Left;
  PrevButton.Top := ShowComicButton.Top + ShowComicButton.Height + Margin;
  NextButton.Left := ShowComicButton.Left + ShowComicButton.Width - NextButton.Width;
  NextButton.Top := PrevButton.Top;

  // Position SaveComicButton
  SaveComicButton.Left := ShowComicButton.Left;
  SaveComicButton.Top := PrevButton.Top + PrevButton.Height + Margin;

  ComboBox1.Top := ShowComicButton.Top;
end;

end.

