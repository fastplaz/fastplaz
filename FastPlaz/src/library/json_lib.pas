unit json_lib;

{$mode objfpc}{$H+}
{-- $include ../../define_fastplaz.inc}
(*
  USAGE
    json := TJSONUtil.Create;

    // load from string
    json.LoadFromJsonString('{}');

    // get value from json
    message := json['response/message'];

    // write to json
    json['code'] := Int16(404);
    json['response/count'] = jArray.Count;
    json['response/message'] = 'success';


*)

interface

uses
  fpjson, variants, jsonparser, contnrs, Classes, SysUtils;

type

  { TJSONUtilItem }

  // A lightweight view on one node of a TJSONUtil document. Items are owned
  // by the TJSONUtil / parent item that handed them out; do not free them.
  TJSONUtilItem = class
  private
    FJSONData: TJSONData;
    FChildren: TFPObjectList;
    function GetAsJSON: TJSONStringType;
    function GetAsString: TJSONStringType;
    function GetIsObject: boolean;
    function GetItem(PathString: UnicodeString): TJSONUtilItem;
    {$PUSH}{$HINTS OFF}
    procedure SetItem(PathString: UnicodeString; AValue: TJSONUtilItem);
    {$POP}

    function FindObject(const PathString: UnicodeString; AllowCreate: boolean;
      var ElementName: UnicodeString): TJSONObject;
    function FindElement(const PathString: UnicodeString; CreateParent: boolean;
      var ParentObject: TJSONObject; var ElementName: UnicodeString): TJSONData;

  public
    constructor Create(JSONData: TJSONData);
    destructor Destroy; override;
    // nil when the path does not exist
    property Item[PathString: UnicodeString]: TJSONUtilItem read GetItem write SetItem; default;

    property Data: TJSONData read FJSONData;
    property IsObject: boolean read GetIsObject;
    property AsString: TJSONStringType read GetAsString;
    property AsJSON: TJSONStringType read GetAsJSON;
  end;

  { TJSONUtil }

  TJSONUtil = class
  private
    FJsonObject: TJSONObject;
    FItems: TFPObjectList;
    FModified: boolean;
    function GetAsArray: TJSONArray;
    function GetAsJSON: TJSONStringType;
    function GetAsJSONFormated: TJSONStringType;
    function GetIsArray: boolean;
    function getIsEmpty: boolean;
    function GetIsObject: boolean;
    function GetItem(PathString: UnicodeString): TJSONUtilItem;
    function GetValue(PathString: UnicodeString): variant;
    function GetValueArray(PathString: UnicodeString): TJSONArray;
    {$PUSH}{$HINTS OFF}
    procedure SetItem(PathString: UnicodeString; AValue: TJSONUtilItem);
    {$POP}
    procedure SetValue(PathString: UnicodeString; AValue: variant);
    procedure SetValueArray(PathString: UnicodeString; AValue: TJSONArray);

    function FindObject(const PathString: UnicodeString;
      AllowCreate: boolean): TJSONObject;
    function FindObject(const PathString: UnicodeString; AllowCreate: boolean;
      var ElementName: UnicodeString): TJSONObject;
    function FindElement(const PathString: UnicodeString;
      CreateParent: boolean): TJSONData;
    function FindElement(const PathString: UnicodeString; CreateParent: boolean;
      var ParentObject: TJSONObject; var ElementName: UnicodeString): TJSONData;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure DeletePath(const PathString: UnicodeString);
    property Modified: boolean read FModified;
    // The root of the document. It is normally a TJSONObject; after loading
    // a JSON array it is a TJSONArray instance behind this type (check
    // IsArray, or use AsArray) and the path based accessors are no-ops.
    property Data: TJSONObject read FJsonObject;
    property IsObject: boolean read GetIsObject;
    property IsArray: boolean read GetIsArray;
    // the root as an array; nil when the root is an object
    property AsArray: TJSONArray read GetAsArray;
    property AsJSON: TJSONStringType read GetAsJSON;
    property AsJSONFormated: TJSONStringType read GetAsJSONFormated;
    property Value[PathString: UnicodeString]: variant read GetValue write SetValue; default;
    property ValueArray[PathString: UnicodeString]: TJSONArray
      read GetValueArray write SetValueArray;
    property ArrayValue[PathString: UnicodeString]: TJSONArray
      read GetValueArray write SetValueArray;

    // nil when the path does not exist; the item is owned by this object
    property Item[PathString: UnicodeString]: TJSONUtilItem read GetItem write SetItem;
    property IsEmpty: boolean read getIsEmpty;

    // Returns False (and leaves an empty object) when JsonString is not a
    // JSON object or array.
    function LoadFromJsonString(const JsonString: string; Const UseUTF8 : Boolean = False): boolean;
    function SaveToFile(AFileName: string): boolean;
  end;


implementation

uses
  commonbase_lib;

{ TJSONUtilItem }

function TJSONUtilItem.GetItem(PathString: UnicodeString): TJSONUtilItem;
var
  o: TJSONObject;
  El: TJSONData;
  ElName: UnicodeString;
  i: integer;
begin
  Result := nil;
  if not Assigned(FJSONData) then
    Exit;
  ElName := '';
  o := nil;
  El := FindElement(StripSlash('/' + PathString), False, o, ElName);
  if not Assigned(El) then
  begin
    // either the path does not exist or it points to an object
    if not Assigned(o) then
      Exit;
    i := o.IndexOfName(string(ElName));
    if i = -1 then
      Exit;
    El := o.Items[i];
  end;
  Result := TJSONUtilItem.Create(El);
  FChildren.Add(Result);
end;

function TJSONUtilItem.GetIsObject: boolean;
begin
  Result := Assigned(FJSONData) and (FJSONData.JSONType = jtObject);
end;

function TJSONUtilItem.GetAsString: TJSONStringType;
begin
  Result := '';
  if not Assigned(FJSONData) then
    Exit;
  try
    Result := FJSONData.AsString;
  except
    on E: Exception do
    begin
      Result := E.Message;
    end;
  end;
end;

function TJSONUtilItem.GetAsJSON: TJSONStringType;
begin
  Result := '';
  if Assigned(FJSONData) then
    Result := FJSONData.AsJSON;
end;

{$PUSH}{$HINTS OFF}
procedure TJSONUtilItem.SetItem(PathString: UnicodeString; AValue: TJSONUtilItem);
begin
end;
{$POP}

function TJSONUtilItem.FindObject(const PathString: UnicodeString;
  AllowCreate: boolean; var ElementName: UnicodeString): TJSONObject;
var
  S, El: UnicodeString;
  P, I: integer;
  T: TJSonObject;
begin
  //  Writeln('Looking for : ', APath);
  S := PathString;
  // relative and absolute paths both start at this item; a non-object
  // item has no children
  if (FJSONData is TJSONObject) then
    Result := TJSONObject(FJSONData)
  else
    Result := nil;
  if Result = nil then
  begin
    ElementName := S;
    Exit;
  end;
  repeat
    P := Pos('/', S);
    if (P <> 0) then
    begin
      // Only real paths, ignore double slash
      if (P <> 1) then
      begin
        El := Copy(S, 1, P - 1);
        if (Result.Count = 0) then
          I := -1
        else
          I := Result.IndexOfName(string(El));
        if (I = -1) then
          // No element with this name.
        begin
          if AllowCreate then
          begin
            // Create new node.
            T := Result;
            Result := TJSonObject.Create;
            T.Add(string(El), Result);
          end
          else
            Result := nil;
        end
        else
          // Node found, check if it is an object
        begin
          if (Result.Items[i].JSONtype = jtObject) then
            Result := Result.Objects[string(el)]
          else
          begin
            //            Writeln(el,' type wrong');
            if AllowCreate then
            begin
              //              Writeln('Creating ',el);
              Result.Delete(I);
              T := Result;
              Result := TJSonObject.Create;
              T.Add(string(El), Result);
            end
            else
              Result := nil;
          end;
        end;
      end;
      Delete(S, 1, P);
    end;
  until (P = 0) or (Result = nil);
  ElementName := S;
end;

function TJSONUtilItem.FindElement(const PathString: UnicodeString;
  CreateParent: boolean; var ParentObject: TJSONObject;
  var ElementName: UnicodeString): TJSONData;
var
  i: integer;
begin
  Result := nil;
  ParentObject := FindObject(PathString, CreateParent, ElementName);
  if Assigned(ParentObject) then
  begin
    //    Writeln('Found parent, looking for element:',ElementName);
    i := ParentObject.IndexOfName(string(ElementName));
    //    Writeln('Element index is',i);
    if (i <> -1) and (ParentObject.items[i].JSONType <> jtObject) then
      Result := ParentObject.Items[i];
  end;
end;

constructor TJSONUtilItem.Create(JSONData: TJSONData);
begin
  inherited Create;
  FJSONData := JSONData;
  FChildren := TFPObjectList.Create(True);
end;

destructor TJSONUtilItem.Destroy;
begin
  FreeAndNil(FChildren);
  inherited Destroy;
end;

{ TJSONUtil }

function TJSONUtil.GetValue(PathString: UnicodeString): variant;
var
  El: TJSONData;
  ElName: UnicodeString;
begin
  {$PUSH}{$NOTES OFF}
  Result := '';
  ElName := UnicodeString(ReplaceAll(string(PathString), ['/'], ['.']));
  {
  ElName := '';
  if Pos('/', PathString) <> 1 then
    PathString := '/' + PathString;
  try
    El := FindElement(StripSlash(PathString), False, o, ElName);
  except
  end;
  }
  try
    El := FJsonObject.GetPath(UTF8String(ElName));
  except
    Exit;
  end;
  if not Assigned(El) then
    Exit;
  try
    case El.JSONType of
      jtString: Result := El.AsString;
      jtBoolean: Result := El.AsBoolean;
      jtNumber:
        case TJSONNumber(El).NumberType of
          ntInteger: Result := El.AsInteger;
          ntInt64: Result := El.AsInt64;
          ntQWord: Result := El.AsQWord;
          ntFloat: Result := El.AsFloat;
        end;
    end;
  except
  end;
  {$POP}
end;

function TJSONUtil.GetValueArray(PathString: UnicodeString): TJSONArray;
var
  o: TJSONObject;
  El: TJSONData;
  ElName: UnicodeString;
begin
  Result := nil;
  ElName := '';
  o := nil;
  if Pos('/', PathString) <> 1 then
    PathString := '/' + PathString;
  El := FindElement(StripSlash(PathString), False, o, ElName);
  if not Assigned(El) then
    Exit;

  if El.JSONType = jtArray then
    Result := (El as TJSONArray);
end;

{$PUSH}{$HINTS OFF}
procedure TJSONUtil.SetItem(PathString: UnicodeString; AValue: TJSONUtilItem);
begin
end;
{$POP}

function TJSONUtil.GetAsJSON: TJSONStringType;
begin
  Result := FJsonObject.AsJSON;
end;

function TJSONUtil.GetIsObject: boolean;
begin
  Result := Assigned(FJsonObject) and (FJsonObject.JSONType = jtObject);
end;

function TJSONUtil.GetIsArray: boolean;
begin
  Result := Assigned(FJsonObject) and (FJsonObject.JSONType = jtArray);
end;

function TJSONUtil.GetAsArray: TJSONArray;
begin
  Result := nil;
  if GetIsArray then
    Result := TJSONArray(TJSONData(FJsonObject));
end;

function TJSONUtil.GetAsJSONFormated: TJSONStringType;
begin
  Result := JsonFormatter(AsJSON, False);
end;

function TJSONUtil.getIsEmpty: boolean;
begin
  Result := True;
  if not Assigned(FJsonObject) then Exit;
  if FJsonObject.Count = 0 then Exit;
  Result := False;
end;

function TJSONUtil.GetItem(PathString: UnicodeString): TJSONUtilItem;
var
  o: TJSONObject;
  El: TJSONData;
  ElName: UnicodeString;
  i: integer;
begin
  Result := nil;
  ElName := '';
  o := nil;
  if Pos('/', PathString) <> 1 then
    PathString := '/' + PathString;
  El := FindElement(StripSlash(PathString), False, o, ElName);
  if not Assigned(El) then
  begin
    // either the path does not exist or it points to an object
    if not Assigned(o) then
      Exit;
    i := o.IndexOfName(string(ElName));
    if i = -1 then
      Exit;
    El := o.Items[i];
  end;
  Result := TJSONUtilItem.Create(El);
  FItems.Add(Result);
end;

// Takes ownership of AValue and replaces whatever is at PathString.
procedure TJSONUtil.SetValueArray(PathString: UnicodeString; AValue: TJSONArray);
var
  o: TJSONObject;
  ElName: UnicodeString;
  i: integer;
begin
  if not Assigned(AValue) then
    Exit;
  o := nil;
  ElName := '';
  if Pos('/', PathString) <> 1 then
    PathString := '/' + PathString;
  FindElement(StripSlash(PathString), True, o, ElName);
  if (not Assigned(o)) or (ElName = '') then
  begin
    AValue.Free;
    Exit;
  end;

  // FindElement returns nil for objects, so look the name up directly
  i := o.IndexOfName(string(ElName));
  if (i <> -1) then
    o.Delete(i);

  o.Add(string(ElName), AValue);
  FModified := True;
end;

procedure TJSONUtil.SetValue(PathString: UnicodeString; AValue: variant);
var
  o: TJSONObject;
  El: TJSONData;
  valueAsString, ElName: UnicodeString;
  i: integer;
begin
  {$PUSH}{$NOTES OFF}
  o := nil;
  ElName := '';
{
  // SIMPLE WAY
  El := FindElement(StripSlash(PathString), True, o, ElName);
  if Assigned(El) then
  begin
    I := O.IndexOfName(elName);
    o.Delete(i);
  end;

  case VarType(AValue) of
    varstring:
    begin
      El := TJSONString.Create(AValue);
      o.Add(ElName, El);
    end;//-- varstring

    varshortint,
    varsmallint,
    varinteger,
    vardouble:
    begin
      El:=TJSONIntegerNumber.Create(AValue);
      o.Add(ElName,El);
    end; //-- integer
  end;

  FModified := True;
  Exit;
}

  if Pos('/', PathString) <> 1 then
    PathString := '/' + PathString;
  FindElement(StripSlash(PathString), True, o, ElName);
  if (not Assigned(o)) or (ElName = '') then
    Exit;

  case VarType(AValue) of
    varstring, varustring, varolestr:
    begin
      valueAsString := AValue;
      El := TJSONString.Create(UTF8Encode(valueAsString));
    end;

    varshortint, varsmallint, varinteger,
    varbyte, varword, varlongword:
      El := TJSONIntegerNumber.Create(AValue);

    varint64:
      El := TJSONInt64Number.Create(AValue);

    varqword:
      El := TJSONQWordNumber.Create(AValue);

    varsingle, vardouble, varcurrency, vardate:
      El := TJSONFloatNumber.Create(AValue);

    varboolean:
      El := TJSONBoolean.Create(AValue);

    varnull, varempty:
      El := TJSONNull.Create;

    else
      Exit; // unsupported variant type: leave the document untouched
  end;

  // replace whatever is at the path, whatever its type
  i := o.IndexOfName(string(ElName));
  if i <> -1 then
    o.Delete(i);
  o.Add(string(ElName), El);

  FModified := True;
  {$POP}
end;

function TJSONUtil.FindObject(const PathString: UnicodeString;
  AllowCreate: boolean): TJSONObject;
var
  s: UnicodeString;
begin
  s := '';
  Result := FindObject(PathString, AllowCreate, s);
end;

function TJSONUtil.FindObject(const PathString: UnicodeString;
  AllowCreate: boolean; var ElementName: UnicodeString): TJSONObject;
var
  S, El: UnicodeString;
  P, I: integer;
  T: TJSonObject;
begin
  //  Writeln('Looking for : ', APath);
  S := PathString;
  // relative and absolute paths both start at the root; an array root has
  // no named children, so every path misses
  if not GetIsObject then
  begin
    Result := nil;
    ElementName := S;
    Exit;
  end;
  Result := FJsonObject;
  repeat
    P := Pos('/', S);
    if (P <> 0) then
    begin
      // Only real paths, ignore double slash
      if (P <> 1) then
      begin
        El := Copy(S, 1, P - 1);
        if (Result.Count = 0) then
          I := -1
        else
          I := Result.IndexOfName(string(El));
        if (I = -1) then
          // No element with this name.
        begin
          if AllowCreate then
          begin
            // Create new node.
            T := Result;
            Result := TJSonObject.Create;
            T.Add(string(El), Result);
          end
          else
            Result := nil;
        end
        else
          // Node found, check if it is an object
        begin
          if (Result.Items[i].JSONtype = jtObject) then
            Result := Result.Objects[string(el)]
          else
          begin
            //            Writeln(el,' type wrong');
            if AllowCreate then
            begin
              //              Writeln('Creating ',el);
              Result.Delete(I);
              T := Result;
              Result := TJSonObject.Create;
              T.Add(string(El), Result);
            end
            else
              Result := nil;
          end;
        end;
      end;
      Delete(S, 1, P);
    end;
  until (P = 0) or (Result = nil);
  ElementName := S;
end;

function TJSONUtil.FindElement(const PathString: UnicodeString;
  CreateParent: boolean): TJSONData;
var
  o: TJSONObject;
  elementName: UnicodeString;
begin
  elementName := '';
  o := nil;
  Result := FindElement(PathString, CreateParent, o, elementName);
end;

function TJSONUtil.FindElement(const PathString: UnicodeString;
  CreateParent: boolean; var ParentObject: TJSONObject;
  var ElementName: UnicodeString): TJSONData;
var
  i: integer;
begin
  Result := nil;
  ParentObject := FindObject(PathString, CreateParent, ElementName);
  if Assigned(ParentObject) then
  begin
    //    Writeln('Found parent, looking for element:',ElementName);
    i := ParentObject.IndexOfName(string(ElementName));
    //    Writeln('Element index is',i);
    if (i <> -1) and (ParentObject.items[i].JSONType <> jtObject) then
      Result := ParentObject.Items[i];
  end;
end;

constructor TJSONUtil.Create;
begin
  inherited Create;

  FJsonObject := TJSONObject.Create;
  FItems := TFPObjectList.Create(True);
  FModified := False;
end;

destructor TJSONUtil.Destroy;
begin
  FreeAndNil(FItems);
  FreeAndNil(FJsonObject);
  inherited Destroy;
end;

// Empties the document; the root becomes an (empty) object again.
procedure TJSONUtil.Clear;
begin
  FItems.Clear;
  if GetIsObject then
    FJsonObject.Clear
  else
  begin
    FreeAndNil(FJsonObject);
    FJsonObject := TJSONObject.Create;
  end;
  FModified := False;
end;

procedure TJSONUtil.DeletePath(const PathString: UnicodeString);
var
  P: UnicodeString;
  L: integer;
  Node: TJSONObject;
  ElName: UnicodeString;
begin
  ElName := '';
  P := StripSlash(PathString);
  L := Length(P);
  if (L > 0) then
  begin
    Node := FindObject(P, False, ElName);
    if Assigned(Node) then
    begin
      L := Node.IndexOfName(string(ElName));
      if (L <> -1) then
        Node.Delete(L);
    end;
  end;
end;

function TJSONUtil.LoadFromJsonString(const JsonString: string;
  const UseUTF8: Boolean): boolean;
var
  parsed: TJSONData;
begin
  Result := False;
  // items handed out so far point into the old document
  FItems.Clear;
  FreeAndNil(FJsonObject);
  FModified := False;
  try
    parsed := GetJSON(JsonString, UseUTF8);
  except
    parsed := nil;
  end;
  if (parsed is TJSONObject) or (parsed is TJSONArray) then
  begin
    // an array root is kept behind the TJSONObject typed field for the
    // callers that cast Data to TJSONArray; see the Data property
    FJsonObject := TJSONObject(parsed);
    Result := True;
  end
  else
  begin
    // empty input or a scalar: not a document we can address
    parsed.Free;
    FJsonObject := TJSONObject.Create;
  end;
end;

function TJSONUtil.SaveToFile(AFileName: string): boolean;
var
  lst: TStringList;
begin
  Result := False;
  if AFileName.IsEmpty then
    Exit;

  lst := TStringList.Create;
  try
    try
      lst.Text := String( FJsonObject.AsJSON).Trim;
      lst.SaveToFile(AFileName);
      Result := True;
    except
      on e:Exception do
      begin
      end;
    end;
  finally
    lst.Free;
  end;
end;

end.
