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

procedure TGoComics.ExtractNavigationUrlsFromHtml(const Html: string);
var
  NavPos, LinkPos, ClassPos: Integer;
  Url, ClassStr: string;
  YearStr, MonthStr, DayStr: string;
  Year, Month, Day: Integer;
begin
  NavPos := Pos('<nav class="gc-calendar-nav" role="group" aria-label="Date Navigation Controls">', Html);
  if NavPos > 0 then
  begin
    // Reset URLs
    FFirstComicUrl := '';
    FPrevComicUrl := '';
    FNextComicUrl := '';
    FLastComicUrl := '';

    LinkPos := PosEx('<a role=''button'' href=''', Html, NavPos);
    while LinkPos > 0 do
    begin
      LinkPos := LinkPos + Length('<a role=''button'' href=''''');
      Url := Copy(Html, LinkPos, PosEx('''', Html, LinkPos) - LinkPos);

      ClassPos := PosEx('class=''fa ', Html, LinkPos);
      if ClassPos > 0 then
      begin
        ClassStr := Copy(Html, ClassPos + Length('class='''), PosEx('''', Html, ClassPos + Length('class=''')) - (ClassPos + Length('class=''')));

        if Pos('fa-backward', ClassStr) > 0 then
          if Url = ' class=' then begin FFirstComicUrl := '' end else begin FFirstComicUrl := BASE_URL + Url end
        else if Pos('fa-caret-left', ClassStr) > 0 then
          if Url = ' class=' then begin FPrevComicUrl := '' end else begin FPrevComicUrl := BASE_URL + Url end
          //FPrevComicUrl := BASE_URL + Url
        else if Pos('fa-caret-right', ClassStr) > 0 then
          if Url = ' class=' then begin FNextComicUrl := '' end else begin FNextComicUrl := BASE_URL + Url end
          //FNextComicUrl := BASE_URL + Url
        else if Pos('fa-forward', ClassStr) > 0 then
          if Url = ' class=' then begin FLastComicUrl := '' end else begin FLastComicUrl := BASE_URL + Url; end
          //FLastComicUrl := BASE_URL + Url;
      end;

      LinkPos := PosEx('<a role=''button'' href=''', Html, LinkPos);
    end;

    // Extract dates for URLs
    if FFirstComicUrl <> '' then
    begin
      YearStr := Copy(FFirstComicUrl, Length(FFirstComicUrl) - 9, 4);
      MonthStr := Copy(FFirstComicUrl, Length(FFirstComicUrl) - 4, 2);
      DayStr := Copy(FFirstComicUrl, Length(FFirstComicUrl) - 1, 2);
      if TryStrToInt(YearStr, Year) and TryStrToInt(MonthStr, Month) and TryStrToInt(DayStr, Day) then
        FFirstComicDate := EncodeDate(Year, Month, Day);
    end;

    if FPrevComicUrl <> '' then
    begin
      YearStr := Copy(FPrevComicUrl, Length(FPrevComicUrl) - 9, 4);
      MonthStr := Copy(FPrevComicUrl, Length(FPrevComicUrl) - 4, 2);
      DayStr := Copy(FPrevComicUrl, Length(FPrevComicUrl) - 1, 2);
      if TryStrToInt(YearStr, Year) and TryStrToInt(MonthStr, Month) and TryStrToInt(DayStr, Day) then
        FPrevComicDate := EncodeDate(Year, Month, Day);
    end;

    if FNextComicUrl <> '' then
    begin
      YearStr := Copy(FNextComicUrl, Length(FNextComicUrl) - 9, 4);
      MonthStr := Copy(FNextComicUrl, Length(FNextComicUrl) - 4, 2);
      DayStr := Copy(FNextComicUrl, Length(FNextComicUrl) - 1, 2);
      if TryStrToInt(YearStr, Year) and TryStrToInt(MonthStr, Month) and TryStrToInt(DayStr, Day) then
        FNextComicDate := EncodeDate(Year, Month, Day);
    end;

    if FLastComicUrl <> '' then
    begin
      YearStr := Copy(FLastComicUrl, Length(FLastComicUrl) - 9, 4);
      MonthStr := Copy(FLastComicUrl, Length(FLastComicUrl) - 4, 2);
      DayStr := Copy(FLastComicUrl, Length(FLastComicUrl) - 1, 2);
      if TryStrToInt(YearStr, Year) and TryStrToInt(MonthStr, Month) and TryStrToInt(DayStr, Day) then
        FLastComicDate := EncodeDate(Year, Month, Day);
    end;
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

