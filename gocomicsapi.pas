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
      FPrevComicDate: TDateTime;
      FNextComicDate: TDateTime;
      FFirstComicDate: TDateTime;
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
      property FirstComicUrl: string read FFirstComicUrl;
      property PrevComicDate: TDateTime read FPrevComicDate;
      property NextComicDate: TDateTime read FNextComicDate;
      property FirstComicDate: TDateTime read FFirstComicDate;
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
  NavPos, FirstPos, PrevPos, NextPos, FirstEndPos, PrevEndPos, NextEndPos: Integer;
  FirstUrl, PrevUrl, NextUrl: string;
  YearStr, MonthStr, DayStr: string;
  Year, Month, Day: Integer;
begin
  NavPos := Pos('<nav class="gc-calendar-nav" role="group" aria-label="Date Navigation Controls">', Html);
  if NavPos > 0 then
  begin
    FirstPos := PosEx('<a role=''button'' href=''/', Html, NavPos);
    if FirstPos > 0 then
    begin
      FirstPos := FirstPos + Length('<a role=''button'' href=''');
      FirstEndPos := PosEx('''', Html, FirstPos);
      FirstUrl := Copy(Html, FirstPos, FirstEndPos - FirstPos);
      FFirstComicUrl := BASE_URL + FirstUrl;

      // Extract date components
      YearStr := Copy(FirstUrl, Length(FirstUrl) - 9, 4);
      MonthStr := Copy(FirstUrl, Length(FirstUrl) - 4, 2);
      DayStr := Copy(FirstUrl, Length(FirstUrl) - 1, 2);

      // Ensure that the date components are correctly extracted
      if TryStrToInt(YearStr, Year) and
         TryStrToInt(MonthStr, Month) and
         TryStrToInt(DayStr, Day) then
      begin
        FFirstComicDate := EncodeDate(Year, Month, Day);
      end
      else
      begin
        raise Exception.Create('Failed to extract first comic date from URL: ' + FirstUrl);
      end;
    end;

    PrevPos := PosEx('fa-caret-left', Html, FirstEndPos);
    if PrevPos > 0 then
    begin
      PrevPos := PosEx('<a role=''button'' href=''/', Html, FirstEndPos);
      if PrevPos > 0 then
      begin
        PrevPos := PrevPos + Length('<a role=''button'' href=''');
        PrevEndPos := PosEx('''', Html, PrevPos);
        PrevUrl := Copy(Html, PrevPos, PrevEndPos - PrevPos);
        FPrevComicUrl := BASE_URL + PrevUrl;

        // Extract date components
        YearStr := Copy(PrevUrl, Length(PrevUrl) - 9, 4);
        MonthStr := Copy(PrevUrl, Length(PrevUrl) - 4, 2);
        DayStr := Copy(PrevUrl, Length(PrevUrl) - 1, 2);

        // Ensure that the date components are correctly extracted
        if TryStrToInt(YearStr, Year) and
           TryStrToInt(MonthStr, Month) and
           TryStrToInt(DayStr, Day) then
        begin
          FPrevComicDate := EncodeDate(Year, Month, Day);
        end
        else
        begin
          raise Exception.Create('Failed to extract previous comic date from URL: ' + PrevUrl);
        end;
      end;
    end;

    NextPos := PosEx('fa-caret-right', Html, PrevEndPos);
    if NextPos > 0 then
    begin
      NextPos := PosEx('<a role=''button'' href=''/', Html, PrevEndPos);
      if NextPos > 0 then
      begin
        NextPos := NextPos + Length('<a role=''button'' href=''');
        NextEndPos := PosEx('''', Html, NextPos);
        NextUrl := Copy(Html, NextPos, NextEndPos - NextPos);
        FNextComicUrl := BASE_URL + NextUrl;

        // Extract date components
        YearStr := Copy(NextUrl, Length(NextUrl) - 9, 4);
        MonthStr := Copy(NextUrl, Length(NextUrl) - 4, 2);
        DayStr := Copy(NextUrl, Length(NextUrl) - 1, 2);

        // Ensure that the date components are correctly extracted
        if TryStrToInt(YearStr, Year) and
           TryStrToInt(MonthStr, Month) and
           TryStrToInt(DayStr, Day) then
        begin
          FNextComicDate := EncodeDate(Year, Month, Day);
        end
        else
        begin
          raise Exception.Create('Failed to extract next comic date from URL: ' + NextUrl);
        end;
      end;
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

