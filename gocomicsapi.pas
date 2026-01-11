unit GoComicsAPI;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, fphttpclient, opensslsockets, LazFileUtils, strutils,
  FPImage, FPReadJPEG, FPReadPNG, FPReadGIF, XMLRead, DOM;

const
  RSS_BASE_URL = 'https://comiccaster.xyz/rss/';

type
  EInvalidDateError = class(Exception);
  EInvalidEndpointError = class(Exception);

  TComicItem = record
    Title: string;
    Link: string;
    PubDate: TDateTime;
    ImageUrl: string;
    Description: string;
  end;

  { TGoComics }

  TGoComics = class
  private
    FEndpoint: string;
    FTitle: string;
    FComicItems: array of TComicItem;
    FCurrentIndex: Integer;
    HTTPClient: TFPHTTPClient;
    function GetRSSUrl: string;
    function FetchRSSFeed: string;
    function ParseRSSFeed(const RSSContent: string): Boolean;
    function ExtractImageFromDescription(const Description: string): string;
    function ExtractImageFromEnclosure(ItemNode: TDOMNode): string;
  public
    constructor Create(const AEndpoint: string);
    destructor Destroy; override;
    function LoadFeed: Boolean;
    function GetCurrentComic(out FileName: string; out ContentType: string): TMemoryStream;
    function MoveToPrevious: Boolean;
    function MoveToNext: Boolean;
    function GetCurrentImageUrl: string;
    function GetCurrentDate: TDateTime;
    function HasPrevious: Boolean;
    function HasNext: Boolean;
    property Title: string read FTitle;
    property Endpoint: string read FEndpoint;
  end;

implementation

{ TGoComics }

constructor TGoComics.Create(const AEndpoint: string);
begin
  FEndpoint := AEndpoint;
  FTitle := AEndpoint;
  HTTPClient := TFPHTTPClient.Create(nil);
  SetLength(FComicItems, 0);
  FCurrentIndex := -1;
end;

destructor TGoComics.Destroy;
begin
  HTTPClient.Free;
  SetLength(FComicItems, 0);
  inherited Destroy;
end;

function TGoComics.GetRSSUrl: string;
begin
  Result := RSS_BASE_URL + FEndpoint;
end;

function TGoComics.FetchRSSFeed: string;
var
  Response: TStringStream;
  RSSUrl: string;
begin
  Result := '';
  RSSUrl := GetRSSUrl;
  Response := TStringStream.Create('');
  try
    try
      HTTPClient.AddHeader('User-Agent', 'Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36');
      WriteLn('Fetching RSS feed from: ', RSSUrl);
      HTTPClient.Get(RSSUrl, Response);
      
      if HTTPClient.ResponseStatusCode = 200 then
      begin
        Result := Response.DataString;
        WriteLn('RSS feed fetched successfully, size: ', Length(Result), ' bytes');
      end
      else
        WriteLn('HTTP error: ', HTTPClient.ResponseStatusCode);
    except
      on E: Exception do
      begin
        WriteLn('Error fetching RSS feed: ', E.Message);
        Result := '';
      end;
    end;
  finally
    Response.Free;
  end;
end;

function TGoComics.ExtractImageFromDescription(const Description: string): string;
var
  ImgPos, SrcPos, SrcEnd: Integer;
  DescStr: AnsiString;
begin
  Result := '';
  DescStr := AnsiString(Description);
  
  // Look for <img src="..." in description
  ImgPos := Pos('<img', DescStr);
  if ImgPos > 0 then
  begin
    SrcPos := PosEx('src="', DescStr, ImgPos);
    if SrcPos > 0 then
    begin
      SrcPos := SrcPos + 5; // Skip 'src="'
      SrcEnd := PosEx('"', DescStr, SrcPos);
      if SrcEnd > 0 then
      begin
        Result := Copy(DescStr, SrcPos, SrcEnd - SrcPos);
        WriteLn('Extracted image from description: ', Copy(Result, 1, 80), '...');
        Exit;
      end;
    end;
  end;
  
  // Alternative: Look for direct URL in description
  if Result = '' then
  begin
    // Look for https://featureassets.gocomics.com or similar
    SrcPos := Pos('https://featureassets.gocomics.com', DescStr);
    if SrcPos <= 0 then
      SrcPos := Pos('https://assets.amuniversal.com', DescStr);
    
    if SrcPos > 0 then
    begin
      SrcEnd := SrcPos;
      // Find end of URL (space, <, or quote)
      while (SrcEnd <= Length(DescStr)) and 
            (DescStr[SrcEnd] <> ' ') and 
            (DescStr[SrcEnd] <> '<') and 
            (DescStr[SrcEnd] <> '"') and
            (DescStr[SrcEnd] <> '''') do
        Inc(SrcEnd);
      
      Result := Copy(DescStr, SrcPos, SrcEnd - SrcPos);
      WriteLn('Extracted direct URL from description: ', Copy(Result, 1, 80), '...');
    end;
  end;
end;

function TGoComics.ExtractImageFromEnclosure(ItemNode: TDOMNode): string;
var
  EnclosureNode: TDOMNode;
  UrlAttr: TDOMNode;
begin
  Result := '';
  
  // Look for <enclosure url="..." /> tag
  EnclosureNode := ItemNode.FindNode('enclosure');
  if Assigned(EnclosureNode) then
  begin
    UrlAttr := EnclosureNode.Attributes.GetNamedItem('url');
    if Assigned(UrlAttr) then
    begin
      Result := UrlAttr.NodeValue;
      WriteLn('Extracted image from enclosure: ', Copy(Result, 1, 80), '...');
    end;
  end;
end;

function TGoComics.ParseRSSFeed(const RSSContent: string): Boolean;
var
  Doc: TXMLDocument;
  RootNode, ChannelNode, ItemNode, ChildNode: TDOMNode;
  ItemsList: TDOMNodeList;
  i: Integer;
  Item: TComicItem;
  TitleNode, LinkNode, PubDateNode, DescNode: TDOMNode;
  DateStr: string;
  StringStream: TStringStream;
begin
  Result := False;
  SetLength(FComicItems, 0);
  
  if RSSContent = '' then
  begin
    WriteLn('RSS content is empty');
    Exit;
  end;
  
  StringStream := TStringStream.Create(RSSContent);
  try
    try
      WriteLn('Parsing RSS XML...');
      ReadXMLFile(Doc, StringStream);
      
      try
        RootNode := Doc.DocumentElement;
        if not Assigned(RootNode) then
        begin
          WriteLn('No root node in XML');
          Exit;
        end;
        
        WriteLn('Root node: ', RootNode.NodeName);
        
        // Find channel node
        ChannelNode := RootNode.FindNode('channel');
        if not Assigned(ChannelNode) then
        begin
          WriteLn('No channel node found');
          Exit;
        end;
        
        WriteLn('Found channel node');
        
        // Get channel title
        TitleNode := ChannelNode.FindNode('title');
        if Assigned(TitleNode) and Assigned(TitleNode.FirstChild) then
        begin
          FTitle := TitleNode.FirstChild.NodeValue;
          WriteLn('Feed title: ', FTitle);
        end;
        
        // Get all item nodes
        ItemsList := ChannelNode.GetChildNodes;
        WriteLn('Total child nodes in channel: ', ItemsList.Count);
        
        for i := 0 to ItemsList.Count - 1 do
        begin
          ItemNode := ItemsList.Item[i];
          if ItemNode.NodeName = 'item' then
          begin
            WriteLn('Processing item ', i);
            
            // Initialize item
            Item.Title := '';
            Item.Link := '';
            Item.PubDate := 0;
            Item.ImageUrl := '';
            Item.Description := '';
            
            // Extract title
            TitleNode := ItemNode.FindNode('title');
            if Assigned(TitleNode) and Assigned(TitleNode.FirstChild) then
              Item.Title := TitleNode.FirstChild.NodeValue;
            
            // Extract link
            LinkNode := ItemNode.FindNode('link');
            if Assigned(LinkNode) and Assigned(LinkNode.FirstChild) then
              Item.Link := LinkNode.FirstChild.NodeValue;
            
            // Extract pubDate
            PubDateNode := ItemNode.FindNode('pubDate');
            if Assigned(PubDateNode) and Assigned(PubDateNode.FirstChild) then
            begin
              DateStr := PubDateNode.FirstChild.NodeValue;
              // Try to parse RFC822 date format
              // Example: "Wed, 08 Jan 2026 05:00:00 +0000"
              try
                // Simple date extraction - get the date parts
                // Format: Day, DD Mon YYYY HH:MM:SS +ZZZZ
                Item.PubDate := Now; // Fallback to now
                // TODO: Implement proper RFC822 date parsing if needed
                WriteLn('PubDate: ', DateStr);
              except
                Item.PubDate := Now;
              end;
            end;
            
            // Extract description
            DescNode := ItemNode.FindNode('description');
            if Assigned(DescNode) and Assigned(DescNode.FirstChild) then
            begin
              Item.Description := DescNode.FirstChild.NodeValue;
              // Try to extract image from description
              Item.ImageUrl := ExtractImageFromDescription(Item.Description);
            end;
            
            // Try enclosure if no image in description
            if Item.ImageUrl = '' then
              Item.ImageUrl := ExtractImageFromEnclosure(ItemNode);
            
            WriteLn('Item: ', Item.Title, ' | Image: ', Copy(Item.ImageUrl, 1, 50));
            
            // Add to array if we have an image URL
            if Item.ImageUrl <> '' then
            begin
              SetLength(FComicItems, Length(FComicItems) + 1);
              FComicItems[High(FComicItems)] := Item;
            end;
          end;
        end;
        
        WriteLn('Total comics loaded: ', Length(FComicItems));
        
        if Length(FComicItems) > 0 then
        begin
          FCurrentIndex := 0; // Start with first (most recent) comic
          Result := True;
        end;
        
      finally
        Doc.Free;
      end;
      
    except
      on E: Exception do
      begin
        WriteLn('Error parsing RSS: ', E.Message);
        Result := False;
      end;
    end;
  finally
    StringStream.Free;
  end;
end;

function TGoComics.LoadFeed: Boolean;
var
  RSSContent: string;
begin
  RSSContent := FetchRSSFeed;
  Result := ParseRSSFeed(RSSContent);
end;

function TGoComics.GetCurrentComic(out FileName: string; out ContentType: string): TMemoryStream;
var
  ImageUrl: string;
  TempClient: TFPHTTPClient;
begin
  Result := nil;
  
  if (FCurrentIndex < 0) or (FCurrentIndex >= Length(FComicItems)) then
  begin
    WriteLn('Invalid current index: ', FCurrentIndex);
    Exit;
  end;
  
  ImageUrl := FComicItems[FCurrentIndex].ImageUrl;
  if ImageUrl = '' then
  begin
    WriteLn('No image URL for current comic');
    Exit;
  end;
  
  Result := TMemoryStream.Create;
  TempClient := TFPHTTPClient.Create(nil);
  
  try
    try
      TempClient.AddHeader('User-Agent', 'Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36');
      WriteLn('Downloading image from: ', ImageUrl);
      TempClient.Get(ImageUrl, Result);
      
      if TempClient.ResponseStatusCode = 200 then
      begin
        Result.Position := 0;
        ContentType := TempClient.ResponseHeaders.Values['Content-Type'];
        
        // If no Content-Type, guess from URL/data
        if ContentType = '' then
        begin
          WriteLn('WARNING: No Content-Type header, guessing from data');
          // Check magic numbers in the file
          Result.Position := 0;
          if Result.Size >= 4 then
          begin
            if (Result.ReadByte = $FF) and (Result.ReadByte = $D8) then
              ContentType := 'image/jpeg'
            else
            begin
              Result.Position := 0;
              if (Result.ReadByte = $89) and (Result.ReadByte = $50) and 
                 (Result.ReadByte = $4E) and (Result.ReadByte = $47) then
                ContentType := 'image/png'
              else
                ContentType := 'image/jpeg'; // Default fallback
            end;
          end
          else
            ContentType := 'image/jpeg'; // Default fallback
          Result.Position := 0;
        end;
        
        FileName := ExtractFileName(ImageUrl);
        WriteLn('Image downloaded successfully, size: ', Result.Size, ' bytes');
        WriteLn('Content-Type: ', ContentType);
        WriteLn('FileName: ', FileName);
      end
      else
      begin
        WriteLn('HTTP error downloading image: ', TempClient.ResponseStatusCode);
        FreeAndNil(Result);
      end;
      
    except
      on E: Exception do
      begin
        WriteLn('Error downloading image: ', E.Message);
        FreeAndNil(Result);
      end;
    end;
  finally
    TempClient.Free;
  end;
end;

function TGoComics.MoveToPrevious: Boolean;
begin
  Result := False;
  if HasPrevious then
  begin
    Inc(FCurrentIndex); // RSS items are in reverse chronological order
    Result := True;
    WriteLn('Moved to previous comic, index: ', FCurrentIndex);
  end;
end;

function TGoComics.MoveToNext: Boolean;
begin
  Result := False;
  if HasNext then
  begin
    Dec(FCurrentIndex); // RSS items are in reverse chronological order
    Result := True;
    WriteLn('Moved to next comic, index: ', FCurrentIndex);
  end;
end;

function TGoComics.GetCurrentImageUrl: string;
begin
  Result := '';
  if (FCurrentIndex >= 0) and (FCurrentIndex < Length(FComicItems)) then
    Result := FComicItems[FCurrentIndex].ImageUrl;
end;

function TGoComics.GetCurrentDate: TDateTime;
begin
  Result := 0;
  if (FCurrentIndex >= 0) and (FCurrentIndex < Length(FComicItems)) then
    Result := FComicItems[FCurrentIndex].PubDate;
end;

function TGoComics.HasPrevious: Boolean;
begin
  // Previous = older comic = higher index
  Result := (FCurrentIndex >= 0) and (FCurrentIndex < Length(FComicItems) - 1);
end;

function TGoComics.HasNext: Boolean;
begin
  // Next = newer comic = lower index
  Result := (FCurrentIndex > 0);
end;

initialization
  ImageHandlers.RegisterImageReader('JPEG Image', 'jpg', TFPReaderJPEG);
  ImageHandlers.RegisterImageReader('PNG Image', 'png', TFPReaderPNG);
  ImageHandlers.RegisterImageReader('GIF Image', 'gif', TFPReaderGIF);

end.
