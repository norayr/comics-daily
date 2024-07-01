unit GoComicsAPI;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, fpjson, jsonparser, IdHTTP, IdSSLOpenSSL, IdCompressorZLib;

const
  BASE_URL = 'https://www.gocomics.com';
  BASE_RANDOM_URL = 'https://www.gocomics.com/random';

type
  EInvalidDateError = class(Exception);
  EInvalidEndpointError = class(Exception);

  { TGoComics }

  TGoComics = class
  private
    FEndpoint: string;
    FTitle: string;
    FStartDate: TDateTime;
    FHTTP: TIdHTTP;
    FSSLHandler: TIdSSLIOHandlerSocketOpenSSL;
    function GetStartDate: TDateTime;
    function GetTitle: string;
    function GetImageUrl(const ADate: TDateTime): string;
    function FormatDate(const ADate: TDateTime): string;
    function GetJSONData(const URL: string): TJSONObject;
    function ParseDate(const ADateStr: string): TDateTime;
  public
    constructor Create(const AEndpoint: string);
    destructor Destroy; override;
    procedure DownloadComic(const ADate: TDateTime; const APath: string);
    procedure ShowComic(const ADate: TDateTime);
    function RandomDate: TDateTime;
    property StartDate: TDateTime read GetStartDate;
    property Title: string read GetTitle;
  end;

implementation

{ TGoComics }

function TGoComics.GetStartDate: TDateTime;
begin
  Result := FStartDate;
end;

function TGoComics.GetTitle: string;
begin
  Result := FTitle;
end;

function TGoComics.GetImageUrl(const ADate: TDateTime): string;
var
  URL: string;
  Response: TStringStream;
  ComicHTML, ComicImg: string;
begin
  URL := Format('%s/%s/%s', [BASE_URL, FEndpoint, FormatDate(ADate)]);
  Response := TStringStream.Create;
  try
    FHTTP.Get(URL, Response);
    ComicHTML := Response.DataString;

    // Parse HTML to get the image URL (simplified)
    // You need to implement proper HTML parsing here
    ComicImg := ''; // Extract image URL from ComicHTML
    Result := ComicImg;
  finally
    Response.Free;
  end;
end;

function TGoComics.FormatDate(const ADate: TDateTime): string;
begin
  Result := FormatDateTime('yyyy/mm/dd', ADate);
end;

function TGoComics.GetJSONData(const URL: string): TJSONObject;
var
  Response: TStringStream;
begin
  Response := TStringStream.Create;
  try
    FHTTP.Get(URL, Response);
    Result := TJSONObject(GetJSON(Response.DataString));
  finally
    Response.Free;
  end;
end;

function TGoComics.ParseDate(const ADateStr: string): TDateTime;
begin
  Result := EncodeDate(StrToInt(Copy(ADateStr, 1, 4)),
    StrToInt(Copy(ADateStr, 6, 2)), StrToInt(Copy(ADateStr, 9, 2)));
end;

constructor TGoComics.Create(const AEndpoint: string);
var
  JSONData, EndpointData: TJSONObject;
  JSONString: string;
begin
  FEndpoint := AEndpoint;
  FHTTP := TIdHTTP.Create(nil);
  FSSLHandler := TIdSSLIOHandlerSocketOpenSSL.Create(nil);
  FHTTP.IOHandler := FSSLHandler;
  FHTTP.HandleRedirects := True;

  // Load endpoint data from JSON
  JSONData := GetJSONData('endpoints.json'); // Adjust path as necessary
  try
    if JSONData.Find(FEndpoint) <> nil then
    begin
      EndpointData := JSONData.Objects[FEndpoint];
      FTitle := EndpointData.Get('title', '');
      FStartDate := ParseDate(EndpointData.Get('start_date', ''));
    end
    else
      raise EInvalidEndpointError.Create('Invalid endpoint');
  finally
    JSONData.Free;
  end;
end;

destructor TGoComics.Destroy;
begin
  FHTTP.Free;
  FSSLHandler.Free;
  inherited Destroy;
end;

procedure TGoComics.DownloadComic(const ADate: TDateTime; const APath: string);
var
  ImageURL, FilePath: string;
  FileStream: TFileStream;
begin
  if ADate < FStartDate then
    raise EInvalidDateError.CreateFmt('Search for dates after %s. Your input: %s',
      [DateToStr(FStartDate), DateToStr(ADate)]);

  ImageURL := GetImageUrl(ADate);
  FilePath := IncludeTrailingPathDelimiter(APath) + FEndpoint + '.png';
  FileStream := TFileStream.Create(FilePath, fmCreate);
  try
    FHTTP.Get(ImageURL, FileStream);
  finally
    FileStream.Free;
  end;
end;

procedure TGoComics.ShowComic(const ADate: TDateTime);
begin
  // Implement display logic
end;

function TGoComics.RandomDate: TDateTime;
var
  Response: TStringStream;
  JSONData: TJSONObject;
begin
  Response := TStringStream.Create;
  try
    FHTTP.Get(BASE_RANDOM_URL + '/' + FEndpoint, Response);
    JSONData := TJSONObject(GetJSON(Response.DataString));
    try
      Result := ParseDate(JSONData.Get('date', ''));
    finally
      JSONData.Free;
    end;
  finally
    Response.Free;
  end;
end;

end.

