unit GoComicsAPI;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, fpjson, jsonparser, fphttpclient, opensslsockets, LazFileUtils, strutils, fgl;

const
  BASE_URL = 'https://www.gocomics.com';
  BASE_RANDOM_URL = 'https://www.gocomics.com/random';

type
  EInvalidDateError = class(Exception);
  EInvalidEndpointError = class(Exception);

  TStringListHelper = class helper for TStringList
    function FindURL(const Domain: string): string;
  end;

  { TGoComics }

  TGoComics = class
  private
    FEndpoint: string;
    FTitle: string;
    FStartDate: TDateTime;
    HTTPClient: TFPHTTPClient;
    function GetStartDate: TDateTime;
    function GetTitle: string;
    function GetImageUrl(const ADate: TDateTime; out FileName: string; out ContentType: string): TMemoryStream;
    function FormatDate(const ADate: TDateTime): string;
    function GetJSONData(const URL: string): TJSONObject;
    function ParseDate(const ADateStr: string): TDateTime;
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

function TStringListHelper.FindURL(const Domain: string): string;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    if Pos(Domain, Strings[i]) > 0 then
      Exit(Strings[i]);
  Result := '';
end;

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

function GetHtmlFromUrl(const Url: string): string;
var
  HttpClient: TFPHTTPClient;
begin
  HttpClient := TFPHTTPClient.Create(nil);
  try
    Result := HttpClient.SimpleGet(Url);
  finally
    HttpClient.Free;
  end;
end;

function ExtractImageUrlFromHtml(const Html: string): string;
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
      HTTPClient.AllowRedirect := True;
      HTTPClient.Get(URL, Response);
      ResponseStr := Response.DataString;  // Convert the stream to a string for parsing

      // Save ResponseStr to a file
      AssignFile(ResponseFile, 'ResponseStr.html');
      Rewrite(ResponseFile);
      try
        Write(ResponseFile, ResponseStr);
      finally
        CloseFile(ResponseFile);
      end;

      ComicImg := ExtractImageUrlFromHtml(ResponseStr);

      if ComicImg = '' then
        raise Exception.Create('Comic image URL not found.');

      FileName := ExtractFileName(ComicImg);
      ContentType := '';
      Result := TMemoryStream.Create;
      try
        HTTPClient.Get(ComicImg, Result);
        ContentType := HTTPClient.ResponseHeaders.Values['Content-Type'];
        Result.Position := 0;
      except
        Result.Free;
        raise;
      end;
    except
      on E: EHTTPClient do
      begin
        if Pos('404', E.Message) > 0 then
          raise Exception.Create('Comic for this date not found.');
        raise;
      end;
      on E: Exception do
      begin
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
      HTTPClient.Get(URL, Response);
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
  HTTPClient := TFPHTTPClient.Create(nil);

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
  HTTPClient.Free;
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
    HTTPClient.Get(BASE_RANDOM_URL + '/' + FEndpoint, Response);
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

