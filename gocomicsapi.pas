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
    FPrevComicUrl: string;
    FNextComicUrl: string;
    FFirstComicUrl: string;
    FLastComicUrl: string;
    FPrevComicDate: TDateTime;
    FNextComicDate: TDateTime;
    FFirstComicDate: TDateTime;
    FLastComicDate: TDateTime;
    HTTPClient: TFPHTTPClient;
    function GetStartDate: TDateTime;
    function GetTitle: string;
    function FormatDate(const ADate: TDateTime): string;
    function ExtractImageUrlFromHtml(const Html: string): string;
    function ExtractLatestComicUrlFromHtml(const Html: string): string;
    procedure ExtractNavigationUrlsFromHtml(const Html: string);
  public
    constructor Create(const AEndpoint: string);
    destructor Destroy; override;
    function GetImageUrl(const ADate: TDateTime; out FileName: string; out ContentType: string): TMemoryStream;
    function GetLatestComicUrl: string;
    property StartDate: TDateTime read GetStartDate;
    property Title: string read GetTitle;
    property PrevComicUrl: string read FPrevComicUrl write FPrevComicUrl;
    property NextComicUrl: string read FNextComicUrl write FNextComicUrl;
    property FirstComicUrl: string read FFirstComicUrl write FFirstComicUrl;
    property LastComicUrl: string read FLastComicUrl write FLastComicUrl;
    property PrevComicDate: TDateTime read FPrevComicDate;
    property NextComicDate: TDateTime read FNextComicDate;
    property FirstComicDate: TDateTime read FFirstComicDate;
    property LastComicDate: TDateTime read FLastComicDate;
  end;

implementation
    uses fpjson, jsonparser, dateutils;
{ TGoComics }

function TGoComics.ExtractImageUrlFromHtml(const Html: string): string;
var
  ImgPos, SrcPos: Integer;
  ImgTag, SrcUrl: string;
begin
  Result := '';
  // Look for the main comic image container
  ImgPos := Pos('<div class="Comic_comic__container__AHdOD"', Html);
  if ImgPos > 0 then
  begin
    // Find the first <img> tag within this container
    ImgPos := PosEx('<img', Html, ImgPos);
    if ImgPos > 0 then
    begin
      ImgTag := Copy(Html, ImgPos, PosEx('>', Html, ImgPos) - ImgPos + 1);
      SrcPos := Pos('src="', ImgTag);
      if SrcPos > 0 then
      begin
        SrcPos := SrcPos + Length('src="');
        SrcUrl := Copy(ImgTag, SrcPos, PosEx('"', ImgTag, SrcPos) - SrcPos);
        if Pos('featureassets.gocomics.com', SrcUrl) > 0 then
          Result := SrcUrl;
      end;
    end;
  end;
end;

function TGoComics.ExtractLatestComicUrlFromHtml(const Html: string): string;
var
  PrevUrlStart, PrevUrlEnd: Integer;
  PrevUrl, DatePart: string;
  Year, Month, Day: Integer;
  ComicDate: TDateTime;
  JsonStart, JsonEnd, JsonDepth: Integer;
  JsonStr: string;
  JsonData: TJSONData;
  DateStr: string;
begin
  Result := '';

  // Method 1: Enhanced JSON parsing with multiple path checks
  JsonStart := 1;
  repeat
    JsonStart := PosEx('<script type="application/ld+json"', Html, JsonStart);
    if JsonStart > 0 then
    begin
      JsonStart := PosEx('{', Html, JsonStart);
      JsonEnd := JsonStart;
      JsonDepth := 1;

      // Accurate JSON boundary detection
      while (JsonEnd <= Length(Html)) and (JsonDepth > 0) do
      begin
        Inc(JsonEnd);
        case Html[JsonEnd] of
          '{': Inc(JsonDepth);
          '}': Dec(JsonDepth);
        end;
      end;

      if JsonDepth = 0 then
      begin
        JsonStr := Copy(Html, JsonStart, JsonEnd - JsonStart + 1);
        try
          JsonData := GetJSON(JsonStr);
          try
            if (JsonData.JSONType = jtObject) then
            begin
              // Try multiple possible date locations
              DateStr := TJSONObject(JsonData).Get('datePublished', '');

              // First fallback: mainEntity.datePublished
              if (DateStr = '') and (JsonData.FindPath('mainEntity') <> nil) then
                DateStr := TJSONObject(JsonData.FindPath('mainEntity')).Get('datePublished', '');

              // Second fallback: isPartOf.datePublished
              if (DateStr = '') and (JsonData.FindPath('isPartOf') <> nil) then
                DateStr := TJSONObject(JsonData.FindPath('isPartOf')).Get('datePublished', '');

              if DateStr <> '' then
              begin
                // Handle ISO format (2025-04-02)
                if TryStrToDate(DateStr, ComicDate, 'yyyy-mm-dd') then
                begin
                  Result := BASE_URL + '/' + FEndpoint + '/' +
                            FormatDateTime('yyyy/mm/dd', ComicDate);
                  Exit;
                end;

                // Handle textual format (April 3, 2025)
                try
                  ComicDate := ScanDateTime('mmmm d, yyyy', DateStr);
                  Result := BASE_URL + '/' + FEndpoint + '/' +
                            FormatDateTime('yyyy/mm/dd', ComicDate);
                  Exit;
                except
                  on E: EConvertError do ; // Continue trying
                end;
              end;
            end;
          finally
            JsonData.Free;
          end;
        except
          // Invalid JSON - continue searching
        end;
      end;
      JsonStart := JsonEnd + 1;
    end;
  until JsonStart <= 0;

  // Fallback: Check JSON-LD script for explicit date
  PrevUrlStart := Pos('"datePublished":"', Html);
  if PrevUrlStart > 0 then
  begin
    PrevUrlStart := PrevUrlStart + 16;
    PrevUrlEnd := PosEx('"', Html, PrevUrlStart);
    DatePart := Copy(Html, PrevUrlStart, PrevUrlEnd - PrevUrlStart);

    // Parse date like "April 3, 2025"
    ComicDate := ScanDateTime('mmmm d, yyyy', DatePart);
    Result := BASE_URL + '/' + FEndpoint + '/' + FormatDateTime('yyyy/mm/dd', ComicDate);
    Exit;
  end;

  if Result = '' then
    raise Exception.Create('Latest comic URL not found.');
end;

procedure TGoComics.ExtractNavigationUrlsFromHtml(const Html: string);
var
  NavPos, LinkPos: Integer;
  Url: string;
  Year, Month, Day: Integer;
begin
  // Previous button
  FPrevComicUrl := '';
  NavPos := Pos('Controls_controls__button_previous__P4LhX', Html);
  if NavPos > 0 then
  begin
    LinkPos := PosEx('href="', Html, NavPos);
    if LinkPos > 0 then
    begin
      LinkPos := LinkPos + 6;
      FPrevComicUrl := BASE_URL + Copy(Html, LinkPos, PosEx('"', Html, LinkPos) - LinkPos);
    end;
  end;

  // Next button
  FNextComicUrl := '';
  NavPos := Pos('Controls_controls__button_next__6zPfv', Html);
  if NavPos > 0 then
  begin
    LinkPos := PosEx('href="', Html, NavPos);
    if LinkPos > 0 then
    begin
      LinkPos := LinkPos + 6;
      FNextComicUrl := BASE_URL + Copy(Html, LinkPos, PosEx('"', Html, LinkPos) - LinkPos);
    end;
  end;

  // Extract dates from URLs
  if FPrevComicUrl <> '' then
  begin
    if TryStrToInt(Copy(FPrevComicUrl, Length(FPrevComicUrl) - 9, 4), Year) and
       TryStrToInt(Copy(FPrevComicUrl, Length(FPrevComicUrl) - 4, 2), Month) and
       TryStrToInt(Copy(FPrevComicUrl, Length(FPrevComicUrl) - 1, 2), Day) then
      FPrevComicDate := EncodeDate(Year, Month, Day);
  end;

  if FNextComicUrl <> '' then
  begin
    if TryStrToInt(Copy(FNextComicUrl, Length(FNextComicUrl) - 9, 4), Year) and
       TryStrToInt(Copy(FNextComicUrl, Length(FNextComicUrl) - 4, 2), Month) and
       TryStrToInt(Copy(FNextComicUrl, Length(FNextComicUrl) - 1, 2), Day) then
      FNextComicDate := EncodeDate(Year, Month, Day);
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

    ExtractNavigationUrlsFromHtml(Response.DataString); // Extract navigation URLs

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

