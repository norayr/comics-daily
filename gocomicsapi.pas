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
    property NextComicDate: TDateTime read FNextComicDate write FNextComicDate;
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
  // Initialize URLs
  FPrevComicUrl := '';
  FNextComicUrl := '';
  FPrevComicDate := 0;
  FNextComicDate := 0;

  try
    // PREVIOUS button extraction
    PrevStart := Pos('Controls_controls__button_previous__', Html);
    if PrevStart <= 0 then
      PrevStart := Pos('class="btn gc-jumble-controls--btn--prev"', Html); // Try alternate class

    if PrevStart > 0 then
    begin
      // Check if disabled
      DisabledCheck := PosEx('aria-disabled="true"', Html, PrevStart);
      if (DisabledCheck = 0) or (DisabledCheck > PosEx('>', Html, PrevStart)) then
      begin
        // Find href link
        LinkStart := PosEx('href="', Html, PrevStart);
        if LinkStart > 0 then
        begin
          LinkStart := LinkStart + 6; // Length of 'href="'
          LinkEnd := PosEx('"', Html, LinkStart);
          if LinkEnd > 0 then
          begin
            NavUrl := Copy(Html, LinkStart, LinkEnd - LinkStart);
            // Add base URL if it's a relative path
            if NavUrl.StartsWith('/') then
              FPrevComicUrl := BASE_URL + NavUrl
            else
              FPrevComicUrl := NavUrl;

            WriteLn('Found previous URL: ' + FPrevComicUrl);

            // Extract date from URL
            UrlSection := ExtractUrlDate(FPrevComicUrl);
            if TryExtractDateFromUrl(UrlSection, FPrevComicDate) then
              WriteLn('Previous comic date extracted: ' + DateToStr(FPrevComicDate));
          end;
        end;
      end;
    end;

    // NEXT button extraction - try multiple possible class names
    NextStart := Pos('Controls_controls__button_next__', Html);
    if NextStart <= 0 then
      NextStart := Pos('class="btn gc-jumble-controls--btn--next"', Html); // Try alternate class

    if NextStart > 0 then
    begin
      // Check if disabled
      DisabledCheck := PosEx('aria-disabled="true"', Html, NextStart);
      if (DisabledCheck = 0) or (DisabledCheck > PosEx('>', Html, NextStart)) then
      begin
        // Find href link
        LinkStart := PosEx('href="', Html, NextStart);
        if LinkStart > 0 then
        begin
          LinkStart := LinkStart + 6; // Length of 'href="'
          LinkEnd := PosEx('"', Html, LinkStart);
          if LinkEnd > 0 then
          begin
            NavUrl := Copy(Html, LinkStart, LinkEnd - LinkStart);
            // Add base URL if it's a relative path
            if NavUrl.StartsWith('/') then
              FNextComicUrl := BASE_URL + NavUrl
            else
              FNextComicUrl := NavUrl;

            WriteLn('Found next URL: ' + FNextComicUrl);

            // Extract date from URL
            UrlSection := ExtractUrlDate(FNextComicUrl);
            if TryExtractDateFromUrl(UrlSection, FNextComicDate) then
            begin
              WriteLn('Next comic date extracted: ' + DateToStr(FNextComicDate));

              // Only validate future dates - don't clear valid next URLs
              if FNextComicDate > (Date() + 1) then // Allow for tomorrow in case of timezone differences
              begin
                WriteLn('Warning: Next comic date is too far in the future, ignoring');
                FNextComicUrl := '';
                FNextComicDate := 0;
              end;
            end
            else
            begin
              // Invalid date in URL - clear it
              WriteLn('Could not extract date from next URL, clearing');
              FNextComicUrl := '';
              FNextComicDate := 0;
            end;
          end;
        end;
      end
      else
      begin
        WriteLn('Next button is disabled in HTML');
      end;
    end
    else
    begin
      WriteLn('Could not find next button element in HTML');
    end;

    // Ensure we're only validating properly, not over-restricting
    if FNextComicDate > 0 then
      WriteLn('Final next comic date: ' + DateToStr(FNextComicDate));

    if FPrevComicDate > 0 then
      WriteLn('Final previous comic date: ' + DateToStr(FPrevComicDate));
  except
    on E: Exception do
    begin
      WriteLn('Error extracting navigation URLs: ' + E.Message);
      // Don't clear URLs on error - this can break navigation
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
  TempClient: TFPHTTPClient;
  RetryCount: Integer;
  Success: Boolean;
  IsLatestComic: Boolean;
begin
    if ADate > Date() then
    raise EInvalidDateError.Create('Cannot load comics for future dates');
  formattedDate := FormatDate(ADate);
  URL := Format('%s/%s/%s', [BASE_URL, FEndpoint, formattedDate]);
  Response := TStringStream.Create('');
  Result := TMemoryStream.Create;
  TempClient := nil;
  IsLatestComic := False;

  try
    try
      // Check if this might be the latest comic
      if CompareDate(ADate, Date()) >= 0 then
      begin
        WriteLn('Potential latest comic date detected');
        IsLatestComic := True;
      end;

      // Create a fresh HTTP client for this request
      TempClient := TFPHTTPClient.Create(nil);
      TempClient.AllowRedirect := True;
      TempClient.AddHeader('User-Agent', 'Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36');

      WriteLn('Requesting URL: ' + URL);

      // Implement retry logic
      RetryCount := 0;
      Success := False;

      while (not Success) and (RetryCount < 3) do
      begin
        try
          TempClient.Get(URL, Response);

          if TempClient.ResponseStatusCode >= 400 then
          begin
            // If we're trying to access a date that doesn't exist yet
            if IsLatestComic and (TempClient.ResponseStatusCode = 404) then
            begin
              WriteLn('Comic not available for this date (404). Likely beyond latest comic.');
              FNextComicUrl := ''; // Clear next URL as we're at the latest
              FNextComicDate := 0;
              raise Exception.Create('No comic available for this date');
            end
            else
              raise Exception.CreateFmt('HTTP error: %d', [TempClient.ResponseStatusCode]);
          end
          else
            Success := True;

        except
          on E: Exception do
          begin
            Inc(RetryCount);
            WriteLn(Format('Request failed (attempt %d/3): %s', [RetryCount, E.Message]));

            if RetryCount >= 3 then
              raise;

            Sleep(1000);
          end;
        end;
      end;

      // Process the HTML
      ComicImg := ExtractImageUrlFromHtml(Response.DataString);
      if ComicImg = '' then
        raise Exception.Create('Comic image URL not found');

      // Extract navigation URLs
      ExtractNavigationUrlsFromHtml(Response.DataString);

      // Special handling for latest comic - ensure next is disabled
      if IsLatestComic then
      begin
        if FNextComicUrl <> '' then
          WriteLn('Warning: Next URL found for potential latest comic: ' + FNextComicUrl);

        // Double check if this really is the latest
        if CompareDate(ADate, Date()) >= 0 then
        begin
          WriteLn('Confirmed latest comic, clearing next URL');
          FNextComicUrl := '';
          FNextComicDate := 0;
        end;
      end;

      // Download the image
      Result.Clear();
      Result.Position := 0;

      RetryCount := 0;
      Success := False;

      while (not Success) and (RetryCount < 3) do
      begin
        try
          WriteLn('Downloading image from: ' + ComicImg);
          TempClient.Get(ComicImg, Result);
          Success := True;
        except
          on E: Exception do
          begin
            Inc(RetryCount);
            WriteLn(Format('Image download failed (attempt %d/3): %s', [RetryCount, E.Message]));

            if RetryCount >= 3 then
              raise;

            Result.Clear();
            Result.Position := 0;
            Sleep(1000);
          end;
        end;
      end;

      Result.Position := 0;
      ContentType := TempClient.ResponseHeaders.Values['Content-Type'];
      FileName := ExtractFileName(ComicImg);

    except
      on E: Exception do
      begin
        WriteLn('Error in GetImageUrl: ' + E.Message);
        FreeAndNil(Result);
        raise;
      end;
    end;
  finally
    Response.Free;
    if Assigned(TempClient) then
      TempClient.Free;
  end;
end;

function TGoComics.GetLatestComicUrl: string;
var
  URL, ResponseStr: string;
  Response: TStringStream;
  TempClient: TFPHTTPClient;
begin
  Result := '';
  URL := Format('%s/%s', [BASE_URL, FEndpoint]);
  Response := TStringStream.Create('');
  TempClient := TFPHTTPClient.Create(nil);

  try
    try
      TempClient.AllowRedirect := True;
      TempClient.AddHeader('User-Agent', 'Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36');

      WriteLn('Getting latest comic from: ' + URL);
      TempClient.Get(URL, Response);

      if TempClient.ResponseStatusCode >= 400 then
        raise Exception.CreateFmt('HTTP error: %d', [TempClient.ResponseStatusCode]);

      ResponseStr := Response.DataString;

      // Try to extract the latest comic URL
      Result := ExtractLatestComicUrlFromHtml(ResponseStr);

      // If we couldn't extract it, fall back to today's date
      if Result = '' then
      begin
        WriteLn('Could not extract latest comic URL, falling back to today''s date');
        Result := Format('%s/%s/%s', [BASE_URL, FEndpoint, FormatDate(Date())]);
      end;

      WriteLn('Latest comic URL: ' + Result);
    except
      on E: Exception do
      begin
        WriteLn('Error getting latest comic: ' + E.Message);
        // Fall back to today's date
        Result := Format('%s/%s/%s', [BASE_URL, FEndpoint, FormatDate(Date())]);
      end;
    end;
  finally
    Response.Free;
    TempClient.Free;
  end;
end;

initialization
  ImageHandlers.RegisterImageReader('JPEG Image', 'jpg', TFPReaderJPEG);
  ImageHandlers.RegisterImageReader('PNG Image', 'png', TFPReaderPNG);
  ImageHandlers.RegisterImageReader('GIF Image', 'gif', TFPReaderGIF);

end.

