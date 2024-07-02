unit GoComicsAPI;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, fpjson, jsonparser, IdHTTP, IdSSLOpenSSL, LazFileUtils, strutils;

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
    function GetImageUrl(const ADate: TDateTime; out FileName: string; out ContentType: string): TMemoryStream;
    function FormatDate(const ADate: TDateTime): string;
    function GetJSONData(const URL: string): TJSONObject;
    function ParseDate(const ADateStr: string): TDateTime;
   // function ExtractImageUrlFromHTML(const HTML: string): string;
  public
    constructor Create(const AEndpoint: string);
    destructor Destroy; override;
    procedure DownloadComic(const ADate: TDateTime; const APath: string; out FullFilePath: string);
    procedure ShowComic(const ADate: TDateTime);
    function RandomDate: TDateTime;
    property StartDate: TDateTime read GetStartDate;
    property Title: string read GetTitle;
  end;

implementation

{ TGoComics }
function ReadStreamToString(AStream: TStream): string;
var
  StringStream: TStringStream;
begin
  StringStream := TStringStream.Create('');
  try
    StringStream.CopyFrom(AStream, AStream.Size);
    Result := StringStream.DataString;
  finally
    StringStream.Free;
  end;
end;

function ReadFileToString(const FileName: string): string;
var
  FileStream: TFileStream;
  StringStream: TStringStream;
begin
  FileStream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  try
    StringStream := TStringStream.Create('');
    try
      StringStream.CopyFrom(FileStream, FileStream.Size);
      Result := StringStream.DataString;
    finally
      StringStream.Free;
    end;
  finally
    FileStream.Free;
  end;
end;

function TGoComics.GetStartDate: TDateTime;
begin
  Result := FStartDate;
end;

function TGoComics.GetTitle: string;
begin
  Result := FTitle;
end;

function PosEx(const SubStr, S: string; Offset: Integer = 1): Integer;
var
  I, Len, LenSubStr: Integer;
begin
  Len := Length(S);
  LenSubStr := Length(SubStr);
  Result := 0;
  if (Offset > Len) or (LenSubStr = 0) then
    Exit;
  for I := Offset to Len - LenSubStr + 1 do
  begin
    if Copy(S, I, LenSubStr) = SubStr then
    begin
      Result := I;
      Exit;
    end;
  end;
end;

function ExtractImageUrlFromHTML(const HTML: string): string;
var
  MetaTagStart, ContentStart, ContentEnd: Integer;
  MetaTag, ImageUrl: string;
begin
  // Find the <meta property="og:image" ...> tag
  MetaTagStart := Pos('<meta property="og:image"', HTML);
  if MetaTagStart = 0 then
    Exit('');

  // Find the content attribute within the <meta property="og:image" ...> tag
  ContentStart := PosEx('content="', HTML, MetaTagStart);
  if ContentStart = 0 then
    Exit('');
  ContentStart := ContentStart + Length('content="');

  // Find the end of the content attribute
  ContentEnd := PosEx('"', HTML, ContentStart);
  if ContentEnd = 0 then
    Exit('');

  // Extract the image URL
  ImageUrl := Copy(HTML, ContentStart, ContentEnd - ContentStart);

  Result := ImageUrl;
end;


function TGoComics.GetImageUrl(const ADate: TDateTime; out FileName: string; out ContentType: string): TMemoryStream;
var
  URL, formattedDate, ComicImg, ResponseStr: string;
  Response: TStringStream;
  ResponseFile: TextFile;
begin
  formattedDate := FormatDate(ADate);
  URL := Format('%s/%s/%s', [BASE_URL, FEndpoint, formattedDate]);
  Response := TStringStream.Create('');
  try
    try
      FHTTP.Get(URL, Response);
      ResponseStr := Response.DataString;  // Convert the stream to a string for parsing

      // Save ResponseStr to a file
      AssignFile(ResponseFile, 'ResponseStr.html');
      Rewrite(ResponseFile);
      try
        Write(ResponseFile, ResponseStr);
      finally
        CloseFile(ResponseFile);
      end;

      ComicImg := ExtractImageUrlFromHTML(ResponseStr);

      if ComicImg = '' then
        raise Exception.Create('Comic image URL not found.');

      FileName := ExtractFileName(ComicImg);
      ContentType := '';
      Result := TMemoryStream.Create;
      try
        FHTTP.Get(ComicImg, Result);
        ContentType := FHTTP.Response.ContentType;
        Result.Position := 0;
      except
        Result.Free;
        raise;
      end;
    except
      on E: EIdHTTPProtocolException do
      begin
        // Handle specific HTTP errors
        if E.ErrorCode = 404 then
          raise Exception.Create('Comic for this date not found.');
        raise;
      end;
      on E: Exception do
      begin
        // Handle all other exceptions
        raise;
      end;
    end;
  finally
    Response.Free;
  end;
end;







function TGoComics.FormatDate(const ADate: TDateTime): string;
begin
  // Ensure to use 'yyyy/mm/dd' format
  Result := FormatDateTime('yyyy"/"mm"/"dd', ADate);
end;

function TGoComics.GetJSONData(const URL: string): TJSONObject;
var
  Response: TStringStream;
begin
  if Pos('http', URL) = 1 then
  begin
    // URL is a web resource, fetch it using HTTP
    Response := TStringStream.Create('');
    try
      FHTTP.Get(URL, Response);
      Result := TJSONObject(GetJSON(Response.DataString));
    finally
      Response.Free;
    end;
  end
  else
  begin
    // URL is a local file
    Result := TJSONObject(GetJSON(ReadFileToString(URL)));
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
begin
  FEndpoint := AEndpoint;
  FHTTP := TIdHTTP.Create(nil);
  FSSLHandler := TIdSSLIOHandlerSocketOpenSSL.Create(nil);
  FHTTP.IOHandler := FSSLHandler;
  FHTTP.HandleRedirects := True;

  // Set SSL options
  FSSLHandler.SSLOptions.Method := sslvTLSv1_1;
  FSSLHandler.SSLOptions.Mode := sslmClient;

  // Load endpoint data from JSON
  JSONData := GetJSONData('/tmp/endpoints.json'); // Adjust path as necessary
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

procedure TGoComics.DownloadComic(const ADate: TDateTime; const APath: string; out FullFilePath: string);
var
  ImageStream: TMemoryStream;
  FilePath, FileName, ContentType, Extension: string;
  FileStream: TFileStream;
begin
  if ADate < FStartDate then
    raise EInvalidDateError.CreateFmt('Search for dates after %s. Your input: %s',
      [DateToStr(FStartDate), DateToStr(ADate)]);

  ImageStream := GetImageUrl(ADate, FileName, ContentType);

  if ContentType = 'image/gif' then
    Extension := '.gif'
  else if ContentType = 'image/jpeg' then
    Extension := '.jpg'
  else if ContentType = 'image/png' then
    Extension := '.png'
  else
    Extension := ''; // Default or raise an error for unsupported types

  FullFilePath := IncludeTrailingPathDelimiter(APath) + ChangeFileExt(FileName, Extension);
  FileStream := TFileStream.Create(FullFilePath, fmCreate);
  try
    ImageStream.Position := 0;
    FileStream.CopyFrom(ImageStream, ImageStream.Size);
  finally
    FileStream.Free;
    ImageStream.Free;
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

initialization

end.

