//
// Copyright 2012 Shaun Simpson
// shauns2029@gmail.com
//
// This code is a proof of concept only
// it is not for distribution or duplication.
// Copying, compiling, etc of this code is strictly forbidden.
//
// Permission to use this file in any way is withheld.
//
unit MetOffice;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ExtCtrls, HTTPSend, SynaCode, XMLRead, DOM, FileUtil,
  LCLProc, Graphics;

type
  TWeatherReport = record
    Title: string;
    Day: string;
    Report: string;
    TempDay: integer;
    TempNight: integer;
    WindSpeedDay: integer;
  end;

  { TMetOffice }

  TMetOffice = class(TObject)
  private
    FReports: array [0..4] of TWeatherReport;
    FURL: string;
    FXMLResponseTime: TDateTime;
    FTitle: string;

    function FindAttribute(Node: TDOMNode; Name: string): TDOMNode;
    function FindNodeByAttribute(Node: TDOMNode; Index: integer; Name, Value: string): TDOMNode;
    function FindNodeByName(Node: TDOMNode; Name: string): TDOMNode;
    function Get5DayForecast(Node: TDOMNode; Day: integer; var Report: TWeatherReport): boolean;
    function GetDayTemp(Report: TStringList; var Temp: integer): boolean;
    function GetDayWindSpeed(Report: TStringList; var Temp: integer): boolean;
    function GetForecastImage(Node: TDOMNode; Day: integer; var ImageURL: string): boolean;
    function GetNightTemp(Report: TStringList; var Temp: integer): boolean;
    procedure Log(Message: String);
  public
    ErrorMessage: string;

    function GetForecast(BaseURL, PageURL: string; Day: integer;
      out Forecast: TWeatherReport; Images: array of TImage; var ImageURLs: array of string): boolean;

    function GetLocation(BaseURL, PageURL: string; out Location: string): boolean;

    constructor Create;
    destructor Destroy;
  published

  end;

implementation

constructor TMetOffice.Create;
var
  i: integer;
begin
  inherited Create;

  FXMLResponseTime := 0;
  ErrorMessage := '';
  FURL := '';
  FTitle := '';
end;

destructor TMetOffice.Destroy;
var
  i: Integer;
begin
  inherited Destroy;
end;

function TMetOffice.GetLocation(BaseURL, PageURL: string; out Location: string): boolean;
var
  Connection: THTTPSend;
  XMLResponse: TXMLDocument;
  RequestURL: string;
  Node: TDOMNode;
  Nodes: TDOMNodeList;
  Found: boolean;
  URL, ImageURL: string;
  ImStr: string;
  Name, Value: string;
  TextContent: string;
  Temp: string;
  i: integer;
begin
  Result := False;

  URL := BaseURL + PageURL;

  Log('Attempting to get weather from MetOffice');
  Connection := THTTPSend.Create();
  XMLResponse := TXMLDocument.Create();

  try
    Connection.Timeout := 10000;

    RequestURL := URL;

    Connection.Document.Clear;
    Connection.Headers.Clear;

    if (Connection.HTTPMethod('GET', RequestURL)) then
    begin
      try
        Log('Got response for forecast');

        // Remove stuff that breaks forecast
        SetString(TextContent, Connection.Document.Memory, Connection.Document.Size);

        TextContent := StringReplace(TextContent, '&nbsp;', '',[rfReplaceAll]);
        TextContent := StringReplace(TextContent, 'UV:', '',[rfReplaceAll]);
        TextContent := StringReplace(TextContent, #194#176, '°',[rfReplaceAll]);

        Connection.Document.Clear;
        Connection.Document.WriteBuffer(Pointer(TextContent)^, Length(TextContent));
        Connection.Document.Position := 0;

        ReadXMLFile(XMLResponse, Connection.Document);

        FXMLResponseTime := Now;
      except
        on E: Exception do
        begin
          ErrorMessage := E.Message;
          Log('Exception getting weather from MetOffice');
          Log(E.Message);
          Result := False;
          Exit;
        end;
      end;
    end;

    Location := 'Unknown Location';

    if Pos('<title>', TextContent) > 1 then
    begin
      Location := Copy(TextContent, Pos('<title>', TextContent) + 7, 128);

      if Pos('forecast', Location) > 1 then
      begin
        Location := Copy(Location, 1, Pos('forecast', Location) - 1);
        Result := True;
      end;
    end;

  finally
    XMLResponse.Free();
    Connection.Free();
  end;
end;

function TMetOffice.GetForecast(BaseURL, PageURL: string;
  Day: integer; out Forecast: TWeatherReport;
  Images: array of TImage; var ImageURLs: array of string): boolean;
var
  Connection: THTTPSend;
  XMLResponse: TXMLDocument;
  RequestURL: string;
  Node: TDOMNode;
  Nodes: TDOMNodeList;
  Found: boolean;
  URL, ImageURL: string;
  ImStr: string;
  Name, Value: string;
  TextContent: string;
  Temp: string;
  i: integer;
begin
  Result := True;

  URL := BaseURL + PageURL;

  if (FURL <> URL) or (Now > FXMLResponseTime + EncodeTime(0, 15, 0, 0)) then
  begin
    FXMLResponseTime := Now;
    FURL := URL;

    Log('Attempting to get weather from MetOffice');
    Connection := THTTPSend.Create();
    XMLResponse := TXMLDocument.Create();

    ImageURL := '';

    try
      Connection.Timeout := 15000;

      RequestURL := URL;

      Connection.Document.Clear;
      Connection.Headers.Clear;

      if (Connection.HTTPMethod('GET', RequestURL)) then
      begin
        try
          Log('Got response for forecast');

          // Remove stuff that breaks forecast
          SetString(TextContent, Connection.Document.Memory, Connection.Document.Size);

          TextContent := StringReplace(TextContent, '&nbsp;', '',[rfReplaceAll]);
          TextContent := StringReplace(TextContent, 'UV:', '',[rfReplaceAll]);
          TextContent := StringReplace(TextContent, #194#176, '°',[rfReplaceAll]);
          TextContent := StringReplace(TextContent, #194+'?', '°',[rfReplaceAll]);

          Connection.Document.Clear;
          Connection.Document.WriteBuffer(Pointer(TextContent)^, Length(TextContent));
          Connection.Document.Position := 0;

          ReadXMLFile(XMLResponse, Connection.Document);
        except
          on E: Exception do
          begin
            ErrorMessage := E.Message;
            Log('Exception getting weather from MetOffice');
            Log(E.Message);
            Result := False;
            Exit;
          end;
        end;
      end;

      Forecast.Title := 'Unknown Location';

      if Pos('<title>', TextContent) > 1 then
      begin
        Forecast.Title := Copy(TextContent, Pos('<title>', TextContent) + 7, 128);

        if Pos('forecast', Forecast.Title) > 1 then
          Forecast.Title := Copy(Forecast.Title, 1, Pos('forecast', Forecast.Title) - 1);
      end;
      TextContent := '';

      FTitle := Forecast.Title;

      Node := XMLResponse.DocumentElement.FindNode('body');

      for i := 0 to High(FReports) do
      begin
        Result := Get5DayForecast(Node, i, FReports[i]);
        if not Result then Break;
      end;

      if Result then
      begin
        for i := 0 to High(Images) do
        begin
          if GetForecastImage(Node, i, ImageURL) then
          begin
            RequestURL := BaseURL + ImageURL;

            // If there is an element avaliable fill it with the image URL
            if i <= High(ImageURLs) then
              ImageURLs[i] := RequestURL;

            RequestURL := EncodeURL(RequestURL);

            Connection.Document.Clear;
            Connection.Headers.Clear;

            if Connection.HTTPMethod('GET', RequestURL) then
            begin
              Log('Got response for forecast image');

              Images[i].Picture.LoadFromStream(Connection.Document);
            end;
          end;
        end;
      end;
    finally
      XMLResponse.Free();
      Connection.Free();
    end;
  end;

  if Result then
  begin
    // Get cached report
    if Day <= High(FReports) then
      ForeCast := FReports[Day]
    else Result := False
  end;
end;


function TMetOffice.GetDayTemp(Report: TStringList; var Temp: integer): boolean;
var
  TempStr, TempStr2: string;
  StartTemp, EndTemp: integer;
  i, j: Integer;
  Values: string;
begin
  Result := False;

  try
    for i := 0 to Report.Count - 1 do
    begin
      if Pos('Temperature:', Report.Strings[i]) > 0 then
      begin
        TempStr := Report.Strings[i];
        TempStr := StringReplace(TempStr, 'Temperature:', '', [rfReplaceAll, rfIgnoreCase]);
        TempStr := StringReplace(TempStr, '°C', '', [rfReplaceAll]);
        TempStr := Trim(TempStr);
        TempStr2 := TempStr;
        TempStr := Trim(Copy(TempStr, 1, 2));
        Temp := StrToInt(TempStr);
        Result := True;
        Break;
      end;
    end;
  except
    on E: Exception do
    begin
      Log('Exception getting day temperature');
      Log(E.Message);

      for i := 1 to Length(TempStr2) do
      begin
        Values := Values + ', "' + TempStr2[i] + '" ' + IntToStr(Integer(TempStr2[i]));
      end;
      Log('Values: ' + Values);

      ErrorMessage := E.Message;
    end;
  end;
end;

function TMetOffice.GetNightTemp(Report: TStringList; var Temp: integer): boolean;
var
  TempStr, TempStr2, Values: string;
  StartTemp, EndTemp: integer;
  i: Integer;
begin
  Result := False;

  try
    for i := 0 to Report.Count - 1 do
    begin
      if Pos('Temperature:', Report.Strings[i]) > 0 then
      begin
        TempStr := Report.Strings[i];
        TempStr := StringReplace(TempStr, 'Temperature:', '', [rfReplaceAll, rfIgnoreCase]);
        TempStr := StringReplace(TempStr, '°C', '', [rfReplaceAll]);
        TempStr := Trim(TempStr);
        TempStr2 := TempStr;
        TempStr := Copy(TempStr, Pos('(', TempStr), Length(TempStr));
        TempStr := StringReplace(TempStr, '(', '', [rfReplaceAll]);
        TempStr := StringReplace(TempStr, ')', '', [rfReplaceAll]);
        TempStr := Trim(Copy(TempStr, 1, 2));
        Temp := StrToInt(TempStr);
        Result := True;
        Break;
      end;
    end;
  except
    on E: Exception do
    begin
      Log('Exception getting night temperature');
      Log(E.Message);

      for i := 1 to Length(TempStr2) do
      begin
        Values := Values + ', "' + TempStr2[i] + '" ' + IntToStr(Integer(TempStr2[i]));
      end;
      Log('Values: ' + Values);

      ErrorMessage := E.Message;
    end;
  end;
end;

function TMetOffice.GetDayWindSpeed(Report: TStringList; var Temp: integer): boolean;
var
  TempStr: string;
  StartTemp, EndTemp: integer;
  i: Integer;
begin
  Result := False;

  try
    for i := 0 to Report.Count - 1 do
    begin
      if Pos('Wind Sp:', Report.Strings[i]) > 0 then
      begin
        TempStr := Report.Strings[i];
        TempStr := StringReplace(TempStr, 'Wind Sp:', '', [rfReplaceAll, rfIgnoreCase]);
        TempStr := StringReplace(TempStr, 'mph', '', [rfReplaceAll, rfIgnoreCase]);
        TempStr := Trim(TempStr);
        TempStr := Trim(Copy(TempStr, 1, 2));
        Temp := StrToInt(TempStr);
        Result := True;
        Break;
      end;
    end;
  except
    on E: Exception do
    begin
      Log('Exception getting day wind speed');
      Log(E.Message);
      ErrorMessage := E.Message;
    end;
  end;
end;

function TMetOffice.Get5DayForecast(Node: TDOMNode; Day: integer; var Report: TWeatherReport): boolean;
var
  Connection: THTTPSend;
  RequestURL: string;
  XMLResponse: TXMLDocument;
  Found: boolean;
  ImageURL: string;
  ImStr: string;
  Name, Value: string;
  TextContent: string;
  i: integer;
  ReportData: TStringList;
  WeatherSymbols: TDOMNode;
begin
  Result := False;
  ReportData := TStringList.Create;

  // Get contents
  if Assigned(Node) then
  begin
    Node := FindNodeByAttribute(Node, 0, 'class', 'content');
  end;

  // Get report
  if Assigned(Node) then
  begin
    WeatherSymbols := FindNodeByAttribute(Node, Day, 'class', 'weathersymbols');

    if Assigned(WeatherSymbols)
     and Assigned(WeatherSymbols.PreviousSibling)
     and (WeatherSymbols.PreviousSibling.HasChildNodes) then
    begin
      Report.Day := Copy(WeatherSymbols.PreviousSibling.FirstChild.NodeValue, 1, 3);
    end;

    Node := FindNodeByAttribute(Node, Day, 'class', 'weathervalues');

    if Assigned(Node) then
    begin
      ReportData.Clear;

      Node := Node.FirstChild;

      repeat
        if Assigned(Node) then
        begin
          Value := Trim(Node.NodeValue);

          if Assigned(Node.FirstChild) and (Value = '') and (ReportData.Count > 0) then
          begin
            Value := ReportData.Strings[ReportData.Count - 1];
            ReportData.Strings[ReportData.Count - 1] := Value + ' ' + Node.FirstChild.NodeValue;
          end
          else if (Value <> '') then
          begin
            ReportData.Add(Value);
          end;

          Node := Node.NextSibling;
        end;
      until not Assigned(Node);

      ReportData.Text := StringReplace(ReportData.Text,
        'Feels Like Temperature:', 'Feels Like:', [rfReplaceAll, rfIgnoreCase]);
      ReportData.Text := StringReplace(ReportData.Text, '?C', '', [rfReplaceAll]);

      Result := True;
    end;
  end;

  Report.Title := FTitle;
  Report.Report := ReportData.Text;

  if not GetDayTemp(ReportData, Report.TempDay) then
    Report.TempDay := 99;

  if not GetNightTemp(ReportData, Report.TempNight) then
    Report.TempNight := 99;

  if not GetDayWindSpeed(ReportData, Report.WindSpeedDay) then
    Report.WindSpeedDay := -1;

  ReportData.Free;
end;

function TMetOffice.GetForecastImage(Node: TDOMNode; Day: integer;
  var ImageURL: string): boolean;
var
  Connection: THTTPSend;
  RequestURL: string;
  XMLResponse: TXMLDocument;
  Nodes: TDOMNodeList;
  Found: boolean;
  ImStr: string;
  Name, Value: string;
  TextContent: string;
  i: integer;
begin
  Result := False;

  // Get contents
  if Assigned(Node) then
  begin
    Node := FindNodeByAttribute(Node, 0, 'class', 'content');
  end;

  // Get report
  if Assigned(Node) then
  begin
    Node := FindNodeByAttribute(Node, Day, 'class', 'weathersymbols');

    if Assigned(Node) then
    begin
      Node := FindNodeByName(Node, 'img');

      if Assigned(Node) then
      begin
        Node := FindAttribute(Node, 'src');

        if Assigned(Node) then
        begin
          ImageURL := Node.NodeValue;
          Result := True;
        end;
      end;
    end;
  end;
end;

procedure TMetOffice.Log(Message: string);
begin
  DebugLn(Self.ClassName + #9#9 + Message);
end;

function TMetOffice.FindAttribute(Node: TDOMNode; Name: string): TDOMNode;
var
  i: integer;
begin
  Result := nil;

  i := 0;
  while Assigned(Node.Attributes.Item[i]) do
  begin
    if (Name = Node.Attributes.Item[i].NodeName) then
    begin
      Result := Node.Attributes.Item[i];
      Break;
    end
    else Inc(i);
  end;
end;

function TMetOffice.FindNodeByName(Node: TDOMNode; Name: string): TDOMNode;
var
  First: TDOMNode;

  procedure EnumerateNode(Node: TDOMNode);
  begin
    while Assigned(Node) and not Assigned(First) do
    begin
      if Node.NodeName = Name then
      begin
        First := Node;
        Exit;
      end
      else EnumerateNode(Node.FirstChild);

      Node := Node.NextSibling;
    end;
  end;
begin
  First := nil;
  EnumerateNode(Node);
  Result := First;
end;

function TMetOffice.FindNodeByAttribute(Node: TDOMNode; Index: integer; Name, Value: string): TDOMNode;
var
  Nodes: TDOMNodeList;
  Attribute: TDOMNode;
  i, j: integer;
begin
  Result := nil;

  if Assigned(Node) and (Node.HasChildNodes) then
  begin
    j := 0;
    Nodes := Node.ChildNodes;

    for i := 0 to Nodes.Count - 1 do
    begin
      Attribute := FindAttribute(Nodes.Item[i], Name);

      if Assigned(Attribute)
       and (Name = Attribute.NodeName) and (Value = Attribute.NodeValue) then
      begin
        if Index = j then
        begin
          Result := Nodes.Item[i];
          Break;
        end
        else Inc(j);
      end;
    end;
  end;
end;

end.

