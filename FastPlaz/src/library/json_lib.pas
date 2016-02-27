unit json_lib;

{$mode objfpc}{$H+}
{$include ../../define_fastplaz.inc}

{

  inspiration from jsonConfig

}

interface

uses
  fpjson, variants,
  strutils, Classes, SysUtils;

type

  { TJSONUtilItem }

  TJSONUtilItem = class
  private
    FKey, FJSONData: TJSONObject;
    function GetAsJSON: TJSONStringType;
    function GetAsString: TJSONStringType;
    function GetIsObject: boolean;
    function GetItem(PathString: string): TJSONUtilItem;
    procedure SetItem(PathString: string; AValue: TJSONUtilItem);

    function FindObject(const PathString: UnicodeString; AllowCreate: boolean;
      var ElementName: UnicodeString): TJSONObject;
    function FindElement(const PathString: UnicodeString; CreateParent: boolean;
      var ParentObject: TJSONObject; var ElementName: UnicodeString): TJSONData;

  public
    constructor Create(JSONData: TJSONObject);
    destructor Destroy;
    property Item[PathString: string]: TJSONUtilItem read GetItem write SetItem; default;

    property IsObject: boolean read GetIsObject;
    property AsString: TJSONStringType read GetAsString;
    property AsJSON: TJSONStringType read GetAsJSON;
  end;

  { TJSONUtil }

  TJSONUtil = class
  private
    FJsonItem: TJSONUtilItem;
    FJsonObject: TJSONObject;
    FKey: TJSONObject;
    FModified: boolean;
    function GetAsJSON: TJSONStringType;
    function GetAsJSONFormated: TJSONStringType;
    function GetItem(PathString: string): TJSONUtilItem;
    function GetValue(PathString: string): variant;
    function GetValueArray(PathString: string): TJSONArray;
    procedure SetItem(PathString: string; AValue: TJSONUtilItem);
    procedure SetValue(PathString: string; AValue: variant);

    function FindObject(const PathString: UnicodeString;
      AllowCreate: boolean): TJSONObject;
    function FindObject(const PathString: UnicodeString; AllowCreate: boolean;
      var ElementName: UnicodeString): TJSONObject;
    function FindElement(const PathString: UnicodeString;
      CreateParent: boolean): TJSONData;
    function FindElement(const PathString: UnicodeString; CreateParent: boolean;
      var ParentObject: TJSONObject; var ElementName: UnicodeString): TJSONData;
    procedure SetValueArray(PathString: string; AValue: TJSONArray);
  public
    constructor Create;
    destructor Destroy;
    procedure DeletePath(const PathString: UnicodeString);
    property Modified: boolean read FModified;
    property Data: TJSONObject read FJsonObject;
    property AsJSON: TJSONStringType read GetAsJSON;
    property AsJSONFormated: TJSONStringType read GetAsJSONFormated;
    property Value[PathString: string]: variant read GetValue write SetValue; default;
    property ValueArray[PathString: string]: TJSONArray
      read GetValueArray write SetValueArray;

    property Item[PathString: string]: TJSONUtilItem read GetItem write SetItem;
  end;


implementation

uses
  common;

{ TJSONUtilItem }

function TJSONUtilItem.GetItem(PathString: string): TJSONUtilItem;
var
  o: TJSONObject;
  El: TJSONData;
  ElName: UnicodeString;
  i : integer;
begin
  El := FindElement(StripSlash('/' + PathString), False, o, ElName);
  if not Assigned(El) then
  begin
    i := o.IndexOfName(ElName);
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

procedure TJSONUtilItem.SetItem(PathString: string; AValue: TJSONUtilItem);
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
          I := Result.IndexOfName(El);
        if (I = -1) then
          // No element with this name.
        begin
          if AllowCreate then
          begin
            // Create new node.
            T := Result;
            Result := TJSonObject.Create;
            T.Add(El, Result);
          end
          else
            Result := nil;
        end
        else
          // Node found, check if it is an object
        begin
          if (Result.Items[i].JSONtype = jtObject) then
            Result := Result.Objects[el]
          else
          begin
            //            Writeln(el,' type wrong');
            if AllowCreate then
            begin
              //              Writeln('Creating ',el);
              Result.Delete(I);
              T := Result;
              Result := TJSonObject.Create;
              T.Add(El, Result);
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
    i := ParentObject.IndexOfName(ElementName);
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

function TJSONUtil.GetValue(PathString: string): variant;
var
  o: TJSONObject;
  El: TJSONData;
  ElName: UnicodeString;
begin
  Result := False;
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

function TJSONUtil.GetValueArray(PathString: string): TJSONArray;
begin

end;

procedure TJSONUtil.SetItem(PathString: string; AValue: TJSONUtilItem);
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

function TJSONUtil.GetItem(PathString: string): TJSONUtilItem;
var
  o: TJSONObject;
  El: TJSONData;
  ElName: UnicodeString;
  i: integer;
begin
  Result := nil;
  El := FindElement(StripSlash(PathString), False, o, ElName);
  if not Assigned(El) then
  begin
    i := o.IndexOfName(ElName);
    Result := TJSONUtilItem.Create(TJSONObject(o.Items[i]));
    Exit;
  end;
  Result := TJSONUtilItem.Create(TJSONObject(El));

end;

procedure TJSONUtil.SetValueArray(PathString: string; AValue: TJSONArray);
var
  o: TJSONObject;
  El: TJSONData;
  ElName: UnicodeString;
  i: integer;
begin
  El := FindElement(StripSlash(PathString), True, o, ElName);
  if Assigned(El) and (not (El is TJSONArray)) then
  begin
    I := O.IndexOfName(elName);
    o.Delete(i);
    El := nil;
  end;
  if not Assigned(El) then
  begin
    o.Add(ElName, AValue);
  end
  else
  begin
    //--- todo: fill data
  end;
end;

procedure TJSONUtil.SetValue(PathString: string; AValue: variant);
var
  o: TJSONObject;
  El: TJSONData;
  ElName: UnicodeString;
  i: integer;
begin
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

  El := FindElement(StripSlash(PathString), True, o, ElName);
  case VarType(AValue) of

    varstring:
    begin
      if Assigned(El) and (El.JSONType <> jtString) then
      begin
        I := O.IndexOfName(elName);
        o.Delete(i);
        El := nil;
      end;
      if not Assigned(El) then
      begin
        El := TJSONString.Create(AValue);
        o.Add(ElName, El);
      end
      else
        El.AsString := AVAlue;
    end;//-- varstring

    varshortint,
    varsmallint,
    varinteger:
    begin
      if Assigned(El) and (not (El is TJSONIntegerNumber)) then
      begin
        I := o.IndexOfName(elName);
        if (I <> -1) then // Normally not needed...
          o.Delete(i);
        El := nil;
      end;
      if not Assigned(El) then
      begin
        El := TJSONIntegerNumber.Create(AValue);
        o.Add(ElName, El);
      end
      else
        El.AsInteger := AValue;
    end; //-- integer

    vardouble:
    begin
      if Assigned(El) and (not (El is TJSONFloatNumber)) then
      begin
        I := o.IndexOfName(elName);
        o.Delete(i);
        El := nil;
      end;
      if not Assigned(El) then
      begin
        El := TJSONFloatNumber.Create(AValue);
        O.Add(ElName, El);
      end
      else
        El.AsFloat := AValue;
    end; //-- double

    varboolean:
    begin
      if Assigned(El) and (el.JSONType <> jtBoolean) then
      begin
        I := O.IndexOfName(elName);
        o.Delete(i);
        El := nil;
      end;
      if not Assigned(El) then
      begin
        El := TJSONBoolean.Create(AValue);
        O.Add(ElName, El);
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
          I := Result.IndexOfName(El);
        if (I = -1) then
          // No element with this name.
        begin
          if AllowCreate then
          begin
            // Create new node.
            T := Result;
            Result := TJSonObject.Create;
            T.Add(El, Result);
          end
          else
            Result := nil;
        end
        else
          // Node found, check if it is an object
        begin
          if (Result.Items[i].JSONtype = jtObject) then
            Result := Result.Objects[el]
          else
          begin
            //            Writeln(el,' type wrong');
            if AllowCreate then
            begin
              //              Writeln('Creating ',el);
              Result.Delete(I);
              T := Result;
              Result := TJSonObject.Create;
              T.Add(El, Result);
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
    i := ParentObject.IndexOfName(ElementName);
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
  P: string;
  L: integer;
  Node: TJSONObject;
  ElName: UnicodeString;
begin
  P := StripSlash(PathString);
  L := Length(P);
  if (L > 0) then
  begin
    Node := FindObject(P, False, ElName);
    if Assigned(Node) then
    begin
      L := Node.IndexOfName(ElName);
      if (L <> -1) then
        Node.Delete(L);
    end;
  end;
end;

end.
