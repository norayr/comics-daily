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
    function IsSubscriptionBlockedPage(const Html: string): Boolean;
    function ExtractUrlDate(const Url: string): string;
    function TryExtractDateFromUrl(const UrlDatePart: string; out ComicDate: TDateTime): Boolean;
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

function TGoComics.IsSubscriptionBlockedPage(const Html: string): Boolean;
begin
  // Check for common subscription blocker text patterns
  Result := (Pos('subscribe to access this comic''s archive', LowerCase(Html)) > 0) or
            (Pos('trying to slow you down', LowerCase(Html)) > 0) {or
            (Pos('$4.99/month', Html) > 0 and Pos('archive', LowerCase(Html)) > 0) or
            (Pos('subscriber content', LowerCase(Html)) > 0);}
end;

function TGoComics.ExtractUrlDate(const Url: string): string;
var
  EndpointPos, DateStart: Integer;
begin
  Result := '';
  // Find the position after the endpoint in the URL
  EndpointPos := Pos('/' + FEndpoint + '/', Url);
  if EndpointPos > 0 then
  begin
    DateStart := EndpointPos + Length(FEndpoint) + 2;
    // Extract the date part (assuming format: /yyyy/mm/dd)
    if DateStart <= Length(Url) then
      Result := Copy(Url, DateStart);
  end;
end;

function TGoComics.TryExtractDateFromUrl(const UrlDatePart: string; out ComicDate: TDateTime): Boolean;
var
  Year, Month, Day: Word;
  TmpYear, TmpMonth, TmpDay: Integer;
  Parts: TStringArray;
begin
  Result := False;
  if UrlDatePart = '' then Exit;

  // Split by '/'
  Parts := UrlDatePart.Split('/');

  // Make sure we have exactly 3 parts (year, month, day)
  if Length(Parts) >= 3 then
  begin
    if TryStrToInt(Parts[0], TmpYear) and
       TryStrToInt(Parts[1], TmpMonth) and
       TryStrToInt(Parts[2], TmpDay) then
    begin
      if (TmpYear > 1900) and (TmpYear < 2100) and
         (TmpMonth >= 1) and (TmpMonth <= 12) and
         (TmpDay >= 1) and (TmpDay <= 31) then
      begin
        try
          Year := Word(TmpYear);
          Month := Word(TmpMonth);
          Day := Word(TmpDay);
          ComicDate := EncodeDate(Year, Month, Day);
          Result := True;
        except
          // Invalid date - do nothing
        end;
      end;
    end;
  end;
end;

function TGoComics.ExtractImageUrlFromHtml(const Html: string): string;
var
  ImgPos, SrcPos, SrcEnd: Integer;
  ImgTag, SrcAttribute: string;
begin
  Result := '';

  // Look for the comic image container
  ImgPos := Pos('class="Comic_comic__container__', Html);
  if ImgPos <= 0 then
    ImgPos := Pos('class="comic__container', Html); // Try alternate class

  if ImgPos > 0 then
  begin
    // Find the img tag within this container
    ImgPos := PosEx('<img', Html, ImgPos);
    if ImgPos > 0 then
    begin
      // Find the end of this img tag
      SrcPos := PosEx('src="', Html, ImgPos);
      if SrcPos > 0 then
      begin
        SrcPos := SrcPos + 5; // Length of 'src="'
        SrcEnd := PosEx('"', Html, SrcPos);
        if SrcEnd > 0 then
        begin
          SrcAttribute := Copy(Html, SrcPos, SrcEnd - SrcPos);
          // Check if this is a comic image (should contain gocomics.com)
          if Pos('gocomics.com', SrcAttribute) > 0 then
            Result := SrcAttribute;
        end;
      end;

      // Try alternate attribute (srcset)
      if Result = '' then
      begin
        SrcPos := PosEx('srcset="', Html, ImgPos);
        if SrcPos > 0 then
        begin
          SrcPos := SrcPos + 8; // Length of 'srcset="'
          SrcEnd := PosEx('"', Html, SrcPos);
          if SrcEnd > 0 then
          begin
            SrcAttribute := Copy(Html, SrcPos, SrcEnd - SrcPos);
            // Extract the first URL from srcset (before space)
            SrcEnd := Pos(' ', SrcAttribute);
            if SrcEnd > 0 then
              SrcAttribute := Copy(SrcAttribute, 1, SrcEnd - 1);

            if Pos('gocomics.com', SrcAttribute) > 0 then
              Result := SrcAttribute;
          end;
        end;
      end;
    end;
  end;

  // If still not found, try regex-like approach with imageSrcSet attribute
  if Result = '' then
  begin
    ImgPos := Pos('imageSrcSet="', Html);
    if ImgPos > 0 then
    begin
      ImgPos := ImgPos + 13; // Length of 'imageSrcSet="'
      SrcEnd := PosEx('"', Html, ImgPos);
      if SrcEnd > 0 then
      begin
        SrcAttribute := Copy(Html, ImgPos, SrcEnd - ImgPos);
        // The image URL typically starts with https://
        if Pos('https://', SrcAttribute) > 0 then
        begin
          SrcPos := Pos('https://', SrcAttribute);
          SrcEnd := PosEx(' ', SrcAttribute, SrcPos);
          if SrcEnd > 0 then
            Result := Copy(SrcAttribute, SrcPos, SrcEnd - SrcPos)
          else
            Result := Copy(SrcAttribute, SrcPos);
        end;
      end;
    end;
  end;
end;

function TGoComics.ExtractLatestComicUrlFromHtml(const Html: string): string;
var




  JsonStart, JsonEnd, JsonDepth: Integer;
  JsonStr: string;
  JsonData: TJSONData;
  DateStr: string;
  ComicDate: TDateTime;
   PrevUrlStart, PrevUrlEnd: Integer;
  PrevUrl, DatePart: string;
  Year, Month, Day: Integer;

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

  // Method 2: HTML pattern search with validation
  PrevUrlStart := 1;
  repeat
    PrevUrlStart := PosEx('"datePublished":"', Html, PrevUrlStart);
    if PrevUrlStart > 0 then
    begin
      PrevUrlStart := PrevUrlStart + 16;
      PrevUrlEnd := PosEx('"', Html, PrevUrlStart);

      // Validate positions before extraction
      if (PrevUrlEnd > PrevUrlStart) and (PrevUrlEnd <= Length(Html)) then
      begin
        DatePart := Copy(Html, PrevUrlStart, PrevUrlEnd - PrevUrlStart);

        // Try ISO format first
        if TryStrToDate(DatePart, ComicDate, 'yyyy-mm-dd') then
        begin
          Result := BASE_URL + '/' + FEndpoint + '/' +
                    FormatDateTime('yyyy/mm/dd', ComicDate);
          Exit;
        end;

        // Try textual format with error handling
        try
          ComicDate := ScanDateTime('mmmm d, yyyy', DatePart);
          Result := BASE_URL + '/' + FEndpoint + '/' +
                    FormatDateTime('yyyy/mm/dd', ComicDate);
          Exit;
        except
          on E: EConvertError do
            PrevUrlStart := PrevUrlEnd; // Continue searching
        end;
      end
      else
      begin
        PrevUrlStart := 0; // Prevent infinite loop
      end;
    end;
  until PrevUrlStart <= 0;

  // Fallback: Check JSON-LD script for explicit date
  PrevUrlStart := Pos('"datePublished":"', Html);
  if PrevUrlStart > 0 then
  begin
    PrevUrlStart := PrevUrlStart + 16;
    PrevUrlEnd := PosEx('"', Html, PrevUrlStart);
    DatePart := Copy(Html, PrevUrlStart, PrevUrlEnd - PrevUrlStart);

    // Parse date like "April 3, 2025"
    ComicDate := DateUtils.ScanDateTime('mmmm d, yyyy', DatePart);
    Result := BASE_URL + '/' + FEndpoint + '/' + FormatDateTime('yyyy/mm/dd', ComicDate);
    Exit;
  end;

  if Result = '' then
    raise Exception.Create('Latest comic URL not found.');
end;

procedure TGoComics.ExtractNavigationUrlsFromHtml(const Html: string);
var
  PrevStart, NextStart, LinkStart, LinkEnd, DisabledCheck: Integer;
  NavUrl, UrlSection: string;
begin
  // Initialize URLs to empty
  FPrevComicUrl := '';
  FNextComicUrl := '';
  FPrevComicDate := 0;
  FNextComicDate := 0;

  try
    // Check if page is subscription-blocked and exit early if so
    if IsSubscriptionBlockedPage(Html) then
      Exit;

    // PREVIOUS button extraction
    PrevStart := Pos('Controls_controls__button_previous__', Html);
    if PrevStart > 0 then
    begin
      DisabledCheck := PosEx('aria-disabled="true"', Html, PrevStart);
      if (DisabledCheck = 0) or (DisabledCheck > PosEx('>', Html, PrevStart)) then
      begin
        LinkStart := PosEx('href="', Html, PrevStart);
        if LinkStart > 0 then
        begin
          LinkStart := LinkStart + 6;
          LinkEnd := PosEx('"', Html, LinkStart);
          if LinkEnd > 0 then
          begin
            NavUrl := Copy(Html, LinkStart, LinkEnd - LinkStart);
            if NavUrl.StartsWith('/') then
              FPrevComicUrl := BASE_URL + NavUrl
            else
              FPrevComicUrl := NavUrl;

            UrlSection := ExtractUrlDate(FPrevComicUrl);
            if TryExtractDateFromUrl(UrlSection, FPrevComicDate) then
              WriteLn('Previous comic date extracted: ' + DateToStr(FPrevComicDate));
          end;
        end;
      end;
    end;

    // NEXT button extraction
    NextStart := Pos('Controls_controls__button_next__', Html);
    if NextStart > 0 then
    begin
      DisabledCheck := PosEx('aria-disabled="true"', Html, NextStart);
      if (DisabledCheck = 0) or (DisabledCheck > PosEx('>', Html, NextStart)) then
      begin
        LinkStart := PosEx('href="', Html, NextStart);
        if LinkStart > 0 then
        begin
          LinkStart := LinkStart + 6;
          LinkEnd := PosEx('"', Html, LinkStart);
          if LinkEnd > 0 then
          begin
            NavUrl := Copy(Html, LinkStart, LinkEnd - LinkStart);
            if NavUrl.StartsWith('/') then
              FNextComicUrl := BASE_URL + NavUrl
            else
              FNextComicUrl := NavUrl;

            UrlSection := ExtractUrlDate(FNextComicUrl);
            if TryExtractDateFromUrl(UrlSection, FNextComicDate) then
              WriteLn('Next comic date extracted: ' + DateToStr(FNextComicDate));
          end;
        end;
      end;
    end;

    // Validate URLs
    if (FPrevComicUrl <> '') and (not FPrevComicUrl.StartsWith('http')) then
      FPrevComicUrl := '';
    if (FNextComicUrl <> '') and (not FNextComicUrl.StartsWith('http')) then
      FNextComicUrl := '';

    // Debug logging
    if FPrevComicUrl <> '' then
      WriteLn('Extracted previous URL: ' + FPrevComicUrl);
    if FNextComicUrl <> '' then
      WriteLn('Extracted next URL: ' + FNextComicUrl);

  except
    on E: Exception do
    begin
      WriteLn('Error extracting navigation URLs: ' + E.Message);
      FPrevComicUrl := '';
      FNextComicUrl := '';
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
    try
      // Make sure we have a valid client
      if not Assigned(HTTPClient) then
        HTTPClient := TFPHTTPClient.Create(nil);

      HTTPClient.AllowRedirect := True;

      // Add logging or debugging information before the request
      WriteLn('Requesting URL: ' + URL);

      HTTPClient.Get(URL, Response); // Get the HTML page

      // Add validation for the response
      if Response.Size = 0 then
        raise Exception.Create('Empty response received');

      ComicImg := ExtractImageUrlFromHtml(Response.DataString); // Extract the image URL from HTML

      if ComicImg = '' then
        raise Exception.Create('Comic image URL not found in the HTML response.');

      ExtractNavigationUrlsFromHtml(Response.DataString); // Extract navigation URLs

      // Add validation before downloading the image
      if ComicImg = '' then
        raise Exception.Create('Empty comic image URL');

      // Download the comic image
      WriteLn('Downloading image from: ' + ComicImg);
      HTTPClient.Get(ComicImg, Result); // Get the image directly into the stream
      Result.Position := 0;
      ContentType := HTTPClient.ResponseHeaders.Values['Content-Type']; // Extract content type
      FileName := ExtractFileName(ComicImg); // Use the extracted file name
    except
      on E: Exception do
      begin
        WriteLn('Error in GetImageUrl: ' + E.Message);
        // Make sure we free the result stream in case of error
        FreeAndNil(Result);
        raise; // Re-raise the exception after cleanup
      end;
    end;
  finally
    Response.Free;
  end;
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

