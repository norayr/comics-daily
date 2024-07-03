unit GoComicsAPI;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, fphttpclient, opensslsockets, LazFileUtils, strutils,
  FPImage, FPReadJPEG, FPReadPNG, FPReadGIF;

const
  BASE_URL = 'https://www.gocomics.com';

type
  EInvalidDateError = class(Exception);
  EInvalidEndpointError = class(Exception);

  { TGoComics }

  TGoComics = class
  private
    FEndpoint: string;
    FTitle: string;
    FStartDate: TDateTime;
    HTTPClient: TFPHTTPClient;
    function GetStartDate: TDateTime;
    function GetTitle: string;
    function FormatDate(const ADate: TDateTime): string;
    function ExtractImageUrlFromHtml(const Html: string): string;
    function ExtractLatestComicUrlFromHtml(const Html: string): string;
  public
    constructor Create(const AEndpoint: string);
    destructor Destroy; override;
    function GetImageUrl(const ADate: TDateTime; out FileName: string; out ContentType: string): TMemoryStream;
    function GetLatestComicUrl: string;
    property StartDate: TDateTime read GetStartDate;
    property Title: string read GetTitle;
  end;

implementation

{ TGoComics }

function TGoComics.ExtractImageUrlFromHtml(const Html: string): string;
var
  ImgPos, SrcPos: Integer;
  ImgTag, SrcUrl: string;
begin
  Result := '';
  ImgPos := Pos('<img', Html);
  while ImgPos > 0 do
  begin
    ImgTag := Copy(Html, ImgPos, PosEx('>', Html, ImgPos) - ImgPos + 1);
    SrcPos := Pos('src="', ImgTag);
    if SrcPos > 0 then
    begin
      SrcPos := SrcPos + Length('src="');
      SrcUrl := Copy(ImgTag, SrcPos, PosEx('"', ImgTag, SrcPos) - SrcPos);
      if Pos('assets.amuniversal.com', SrcUrl) > 0 then
      begin
        Result := SrcUrl;
        Break;
      end;
    end;
    ImgPos := PosEx('<img', Html, ImgPos + Length('<img'));
  end;
end;

function TGoComics.ExtractLatestComicUrlFromHtml(const Html: string): string;
var
  Pos1, Pos2: Integer;
begin
  Pos1 := Pos('<div class="gc-deck gc-deck--cta-0">', Html);
  if Pos1 > 0 then
  begin
    Pos1 := PosEx('<a class="gc-blended-link gc-blended-link--primary"', Html, Pos1);
    if Pos1 > 0 then
    begin
      Pos1 := PosEx('href="', Html, Pos1) + Length('href="');
      Pos2 := PosEx('"', Html, Pos1);
      Result := BASE_URL + Copy(Html, Pos1, Pos2 - Pos1);
    end;
  end;
  if Result = '' then
    raise Exception.Create('Latest comic URL not found.');
end;

function TGoComics.GetStartDate: TDateTime;
begin
  Result := FStartDate;
end;

function TGoComics.GetTitle: string;
begin
  Result := FTitle;
end;

function TGoComics.FormatDate(const ADate: TDateTime): string;
begin
  Result := FormatDateTime('yyyy"/"mm"/"dd', ADate);
end;

constructor TGoComics.Create(const AEndpoint: string);
begin
  FEndpoint := AEndpoint;
  HTTPClient := TFPHTTPClient.Create(nil);

  // Simulated start date for the comic; this should be replaced with actual data if available
  FStartDate := EncodeDate(2000, 1, 1);
end;

destructor TGoComics.Destroy;
begin
  HTTPClient.Free;
  inherited Destroy;
end;

function TGoComics.GetImageUrl(const ADate: TDateTime; out FileName: string; out ContentType: string): TMemoryStream;
var
  URL, formattedDate, ComicImg: string;
  Response: TStringStream;
begin
  formattedDate := FormatDate(ADate);
  URL := Format('%s/%s/%s', [BASE_URL, FEndpoint, formattedDate]);
  Response := TStringStream.Create('');
  Result := TMemoryStream.Create;
  try
    HTTPClient.AllowRedirect := True;
    HTTPClient.Get(URL, Response); // Get the HTML page
    ComicImg := ExtractImageUrlFromHtml(Response.DataString); // Extract the image URL from HTML

    if ComicImg = '' then
      raise Exception.Create('Comic image URL not found in the HTML response.');

    // Download the comic image
    HTTPClient.Get(ComicImg, Result); // Get the image directly into the stream
    Result.Position := 0;
    ContentType := HTTPClient.ResponseHeaders.Values['Content-Type']; // Extract content type
    FileName := ExtractFileName(ComicImg); // Use the extracted file name
  except
    Result.Free;
    raise;
  end;
  Response.Free;
end;

function TGoComics.GetLatestComicUrl: string;
var
  URL, ResponseStr: string;
  Response: TStringStream;
begin
  URL := Format('%s/%s', [BASE_URL, FEndpoint]);
  Response := TStringStream.Create('');
  try
    HTTPClient.Get(URL, Response);
    ResponseStr := Response.DataString;
    Result := ExtractLatestComicUrlFromHtml(ResponseStr);
  finally
    Response.Free;
  end;
end;

initialization
  ImageHandlers.RegisterImageReader('JPEG Image', 'jpg', TFPReaderJPEG);
  ImageHandlers.RegisterImageReader('PNG Image', 'png', TFPReaderPNG);
  ImageHandlers.RegisterImageReader('GIF Image', 'gif', TFPReaderGIF);

end.

