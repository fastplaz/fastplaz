unit json_lib;

{$mode objfpc}{$H+}
{$include ../../define_fastplaz.inc}

{

  inspiration from jsonConfig

}

interface

uses
  fpjson, variants, jsonparser, Classes, SysUtils;

type

  { TJSONUtilItem }

  TJSONUtilItem = class
  private
    FKey, FJSONData: TJSONObject;
    function GetAsJSON: TJSONStringType;
    function GetAsString: TJSONStringType;
    function GetIsObject: boolean;
    function GetItem(PathString: UnicodeString): TJSONUtilItem;
    procedure SetItem(PathString: UnicodeString; AValue: TJSONUtilItem);

    function FindObject(const PathString: UnicodeString; AllowCreate: boolean;
      var ElementName: UnicodeString): TJSONObject;
    function FindElement(const PathString: UnicodeString; CreateParent: boolean;
      var ParentObject: TJSONObject; var ElementName: UnicodeString): TJSONData;

  public
    constructor Create(JSONData: TJSONObject);
    destructor Destroy; override;
    property Item[PathString: UnicodeString]: TJSONUtilItem read GetItem write SetItem; default;

    property IsObject: boolean read GetIsObject;
    property AsString: TJSONStringType read GetAsString;
    property AsJSON: TJSONStringType read GetAsJSON;
  end;

  { TJSONUtil }

  TJSONUtil = class
  private
    FJsonObject: TJSONObject;
    FKey: TJSONObject;
    FModified: boolean;
    function GetAsJSON: TJSONStringType;
    function GetAsJSONFormated: TJSONStringType;
    function GetItem(PathString: UnicodeString): TJSONUtilItem;
    function GetValue(PathString: UnicodeString): variant;
    function GetValueArray(PathString: UnicodeString): TJSONArray;
    procedure SetItem(PathString: UnicodeString; AValue: TJSONUtilItem);
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
    procedure DeletePath(const PathString: UnicodeString);
    property Modified: boolean read FModified;
    property Data: TJSONObject read FJsonObject;
    property AsJSON: TJSONStringType read GetAsJSON;
    property AsJSONFormated: TJSONStringType read GetAsJSONFormated;
    property Value[PathString: UnicodeString]: variant read GetValue write SetValue; default;
    property ValueArray[PathString: UnicodeString]: TJSONArray
      read GetValueArray write SetValueArray;

    property Item[PathString: UnicodeString]: TJSONUtilItem read GetItem write SetItem;

    function LoadFromJsonString(const JsonString: string): boolean;
  end;


implementation

uses
  common;

{ TJSONUtilItem }

function TJSONUtilItem.GetItem(PathString: UnicodeString): TJSONUtilItem;
var
  o: TJSONObject;
  El: TJSONData;
  ElName: UnicodeString;
  i: integer;
begin
  ElName := '';
  El := FindElement(StripSlash('/' + PathString), False, o, ElName);
  if not Assigned(El) then
  begin
    i := o.IndexOfName(string(ElName));
    Result := TJSONUtilItem.Create(TJSONObject(o.Items[i]));
    //Result := TJSONUtilItem.Create(TJSONObject(TJSONBoolean.Create(False)));
    Exit;
  end;
  Result := TJSONUtilItem.Create(TJSONObject(El));
end;

function TJSONUtilItem.GetIsObject: boolean;
begin
  Result := (FJSONData.JSONType = jtObject);
end;

function TJSONUtilItem.GetAsString: TJSONStringType;
begin
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
  Result := FJSONData.AsJSON;
end;

procedure TJSONUtilItem.SetItem(PathString: UnicodeString; AValue: TJSONUtilItem);
begin

end;

function TJSONUtilItem.FindObject(const PathString: UnicodeString;
  AllowCreate: boolean; var ElementName: UnicodeString): TJSONObject;
var
  S, El: UnicodeString;
  P, I: integer;
  T: TJSonObject;
begin
  //  Writeln('Looking for : ', APath);
  S := PathString;
  if Pos('/', S) = 1 then
    Result := FJSONData
  else
    Result := FKey;
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

constructor TJSONUtilItem.Create(JSONData: TJSONObject);
begin
  FJSONData := JSONData;
  FKey := TJSONObject.Create;
end;

destructor TJSONUtilItem.Destroy;
begin
  FreeAndNil(FKey);
end;

{ TJSONUtil }

function TJSONUtil.GetValue(PathString: UnicodeString): variant;
var
  o: TJSONObject;
  El: TJSONData;
  ElName: UnicodeString;
begin
  Result := '';
  ElName := '';
  if Pos('/', PathString) <> 1 then
    PathString := '/' + PathString;
  El := FindElement(StripSlash(PathString), False, o, ElName);
  if not Assigned(El) then
    Exit;
  if El.JSONType = jtString then
    Result := El.AsString;
  if El.JSONType = jtBoolean then
    Result := El.AsBoolean;
  if (El is TJSONIntegerNumber) then
    Result := El.AsInteger;
  if (El is TJSONFloatNumber) then
    Result := El.AsFloat;
end;

function TJSONUtil.GetValueArray(PathString: UnicodeString): TJSONArray;
var
  o: TJSONObject;
  El: TJSONData;
  ElName: UnicodeString;
begin
  ElName := '';
  if Pos('/', PathString) <> 1 then
    PathString := '/' + PathString;
  El := FindElement(StripSlash(PathString), False, o, ElName);
  if not Assigned(El) then
    Exit;

  if El.JSONType = jtArray then
    Result := (El as TJSONArray);
end;

procedure TJSONUtil.SetItem(PathString: UnicodeString; AValue: TJSONUtilItem);
begin

end;

function TJSONUtil.GetAsJSON: TJSONStringType;
begin
  Result := FJsonObject.AsJSON;
end;

function TJSONUtil.GetAsJSONFormated: TJSONStringType;
begin
  Result := JsonFormatter(AsJSON);
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
  El := FindElement(StripSlash(PathString), False, o, ElName);
  if not Assigned(El) then
  begin
    i := o.IndexOfName(string(ElName));
    Result := TJSONUtilItem.Create(TJSONObject(o.Items[i]));
    Exit;
  end;
  Result := TJSONUtilItem.Create(TJSONObject(El));

end;

procedure TJSONUtil.SetValueArray(PathString: UnicodeString; AValue: TJSONArray);
var
  o: TJSONObject;
  El: TJSONData;
  ElName: UnicodeString;
  i: integer;
begin
  if Pos('/', PathString) <> 1 then
    PathString := '/' + PathString;
  El := FindElement(StripSlash(PathString), True, o, ElName);

  if Assigned(El) then
  begin
    i := o.IndexOfName(string(elName));
    if (I <> -1) then
      o.Delete(i);
  end;

  o.Add(string(ElName), AValue);
end;

procedure TJSONUtil.SetValue(PathString: UnicodeString; AValue: variant);
var
  o: TJSONObject;
  El: TJSONData;
  ElName: UnicodeString;
  i: integer;
begin
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
  El := FindElement(StripSlash(PathString), True, o, ElName);
  case VarType(AValue) of

    varstring:
    begin
      if Assigned(El) and (El.JSONType <> jtString) then
      begin
        I := O.IndexOfName(string(elName));
        o.Delete(i);
        El := nil;
      end;
      if not Assigned(El) then
      begin
        El := TJSONString.Create(AValue);
        o.Add(string(ElName), El);
      end
      else
        El.AsString := AVAlue;
    end;//-- varstring

    varshortint,
    varsmallint,
    varint64,
    varinteger:
    begin
      if Assigned(El) and (not (El is TJSONIntegerNumber)) then
      begin
        I := o.IndexOfName(string(elName));
        if (I <> -1) then // Normally not needed...
          o.Delete(i);
        El := nil;
      end;
      if not Assigned(El) then
      begin
        El := TJSONIntegerNumber.Create(AValue);
        o.Add(string(ElName), El);
      end
      else
        El.AsInteger := AValue;
    end; //-- integer

    vardouble:
    begin
      if Assigned(El) and (not (El is TJSONFloatNumber)) then
      begin
        I := o.IndexOfName(string(elName));
        o.Delete(i);
        El := nil;
      end;
      if not Assigned(El) then
      begin
        El := TJSONFloatNumber.Create(AValue);
        O.Add(string(ElName), El);
      end
      else
        El.AsFloat := AValue;
    end; //-- double

    varboolean:
    begin
      if Assigned(El) and (el.JSONType <> jtBoolean) then
      begin
        I := O.IndexOfName(string(elName));
        o.Delete(i);
        El := nil;
      end;
      if not Assigned(El) then
      begin
        El := TJSONBoolean.Create(AValue);
        O.Add(string(ElName), El);
      end
      else
        El.AsBoolean := AValue;
    end;
  end;


  FModified := True;
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
  if Pos('/', S) = 1 then
    Result := FJsonObject
  else
    Result := FKey;
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
  FKey := FJsonObject;
  FModified := False;
end;

destructor TJSONUtil.Destroy;
begin
  if Assigned(FJsonObject) then
    FreeAndNil(FJsonObject);
  inherited Destroy;
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

function TJSONUtil.LoadFromJsonString(const JsonString: string):boolean;
begin
  FJsonObject.Clear;
  try
    FJsonObject := TJSONObject(GetJSON(JsonString));
    Result := true;
  except
    Result := false;
  end;
end;

end.
