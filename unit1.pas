unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  FPImage, FPReadJPEG, FPReadPNG, FPReadGIF,
  LazFileUtils, IntfGraphics,
  GoComicsAPI;

type

  { TForm1 }

  TForm1 = class(TForm)
    ShowComicButton: TButton;
    SaveComicButton: TButton;
    Memo1: TMemo;
    Image1: TImage;
    procedure ShowComicButtonClick(Sender: TObject);
    procedure SaveComicButtonClick(Sender: TObject);
  private
    FComicStream: TMemoryStream;
    FFileName: string;
    FContentType: string;
    procedure LoadImageFromStream(Stream: TMemoryStream; const ContentType: string);
    function GetComicsDailyDir: string;
    function GetFileExtension(const ContentType: string): string;
  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

procedure TForm1.ShowComicButtonClick(Sender: TObject);
var
  GoComics: TGoComics;
  ComicDate: TDateTime;
begin
  Memo1.Lines.Add('Fetching comic...');
  GoComics := TGoComics.Create('pearlsbeforeswine');
  FComicStream := TMemoryStream.Create;
  try
    ComicDate := Now;
    FComicStream := GoComics.GetImageUrl(ComicDate, FFileName, FContentType);
    Memo1.Lines.Add('Content Type: ' + FContentType); // Log content type for debugging
    if Pos('text/html', FContentType) > 0 then
      raise Exception.Create('Received HTML instead of image. Please check the image URL.');

    LoadImageFromStream(FComicStream, FContentType);
    Memo1.Lines.Add('Comic displayed successfully!');
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
  Img: TFPCustomImage;
  Reader: TFPCustomImageReader;
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
      Image1.Picture.Bitmap.Assign(Img);
    finally
      Reader.Free;
    end;
  finally
    Img.Free;
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

end.

