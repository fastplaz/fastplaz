unit html_lib;

{$mode objfpc}{$H+}

interface

uses
  common, sqldb,
  strutils, fpjson,
  Classes, SysUtils;

const
  __HTMLLIB_FORMID_LENGTH = 5;
  __HTMLLIB_FORMCSRFTOKEN_LENGTH = 12;
  __HTML_CSRF_TOKEN_KEY = '_csrf_token_key';
  __HTML_CSRF_TOKEN_KEY_FAILEDCOUNT = '_csrf_token_key_failedcount';
  __HTML_ALLBLOCK =
    '(table|thead|tfoot|caption|col|colgroup|tbody|tr|td|th|div|dl|dd|dt|ul|ol|li|pre|form|map|area|blockquote|address|math|style|p|h[1-6]|hr|fieldset|legend|section|article|aside|hgroup|header|footer|nav|figure|details|menu|summary)';

type

  { THTMLUtil }

  THTMLUtil = class
  private
    FFormID: string;
    function GetFormID: string;
    function setTag(const Tag: string; const Content: string;
      const Options: array of string; EndTag: boolean = True): string;
    function setTag(const Tag: string; const Content: string;
      EndTag: boolean = True): string;
    function setCsrfTokenHtml(const FormIDDefault: string = ''): string;
    function NewCsrfToken(const AFormID: string): string;
  public
    constructor Create;
    destructor Destroy; override;
    property FormID: string read GetFormID;
    // Forgets every CSRF token of the session.
    procedure ResetCSRF;
    function CSRF(const FormIDDefault: string = ''; AsHTML : boolean = true): string;
    // True when the posted csrftoken belongs to the posted __formid (or to
    // the last form rendered). With Force the tokens are reset afterwards.
    function CheckCSRF(Force: boolean = True): boolean;
    // Compares a posted token with the session, no side effects.
    function IsValidCsrfToken(const AToken: string; const AFormID: string = ''): boolean;
    function H1(const Content: string; Options: array of string): string;
    function H1(const Content: string): string;
    function Block(const Content: string; Options: array of string): string;
    function Block(const Content: string): string;
    function img(const ImageUrl: string; Options: array of string): string;
    function img(const ImageUrl: string): string;
    function AddForm(Options: array of string): string;
    function EndForm(): string;
    function AddInput(Options: array of string; Mandatory: boolean = False): string;

    function AddInputLTE(InputID, InputType: string; LabelName: string = '';
      Value: string = ''; Placeholder: string = ''; Required: boolean = True;
      ButtonLabel: string = ''): string;
    function AddSelectLTE(InputID: string; LabelName: string = '';
      Data: TSQLQuery = nil; IndexFieldName: string = 'id';
      ValueFieldName: string = 'name'): string;

    function AddField(TagName: string; Options: array of string): string;
    function AddButton(TagName: string; Options: array of string): string;
    function Link(const URL: string; Options: array of string): string;
    function Link(const Text, URL: string; Options: array of string): string;

    function ReCaptcha(const PublicKey: string; const Version: string = 'v1'): string;
    function Permalink(Title: string): string;

    function AddMenu(Title, Icon, URL: string; RgihtLabel: string = '';
      IsAjax: boolean = False; AjaxTarget: string = ''): TJSONObject;
    function AddNotif(NotifType: integer; Title, Icon, URL: string;
      IsAjax: boolean = False; AjaxTarget: string = ''): TJSONObject;
  end;

function H1(Content: string; StyleClass: string = ''): string;
function H2(Content: string; StyleClass: string = ''): string;
function H3(Content: string; StyleClass: string = ''): string;
function li(Content: string; StyleClass: string = ''): string;
function Span(Content: string; StyleClass: string = ''): string;
function Block(Content: string; StyleClass: string = ''; BlockID: string = ''): string;

// Escape &, <, >, " and ' so a value can be placed in HTML text or in a
// double or single quoted attribute.
function HTMLEncode(const S: string): string;
function StripTags(const Content: string): string;
function StripTagsCustom(const Content: string; const TagStart: string;
  const TagEnd: string): string;
function MoreLess(const Content: string; CharacterCount: integer = 100;
  Suffix: string = '...'): string;
// Constant time string comparison for secrets such as CSRF tokens.
function SecureEquals(const A, B: string): boolean;
// Session key that stores the CSRF token of one form.
function CsrfSessionKey(const AFormID: string): string;

var
  HTMLUtil: THTMLUtil;

implementation

uses
  fastplaz_handler;

function H1(Content: string; StyleClass: string): string;
begin
  if StyleClass = '' then
    Result := '<H1>' + Content + '</H1>'
  else
    Result := '<H1 class="' + StyleClass + '">' + Content + '</H1>';
end;

function H2(Content: string; StyleClass: string): string;
begin
  if StyleClass = '' then
    Result := '<H2>' + Content + '</H2>'
  else
    Result := '<H2 class="' + StyleClass + '">' + Content + '</H2>';
end;

function H3(Content: string; StyleClass: string): string;
begin
  if StyleClass = '' then
    Result := '<H3>' + Content + '</H3>'
  else
    Result := '<H3 class="' + StyleClass + '">' + Content + '</H3>';
end;

function li(Content: string; StyleClass: string): string;
begin
  if StyleClass = '' then
    Result := '<li>' + Content + '</li>'
  else
    Result := '<li class="' + StyleClass + '">' + Content + '</li>';
end;

function Span(Content: string; StyleClass: string): string;
begin
  if StyleClass = '' then
    Result := '<span>' + Content + '</span>'
  else
    Result := '<span class="' + StyleClass + '">' + Content + '</span>';
end;

function Block(Content: string; StyleClass: string; BlockID: string): string;
var
  s: string;
begin
  if BlockID <> '' then
    s := ' id="' + BlockID + '" ';
  if StyleClass = '' then
    Result := '<div' + s + '>' + Content + '</div>'
  else
    Result := '<div ' + s + ' class="' + StyleClass + '">' + Content + '</div>';
end;

function HTMLEncode(const S: string): string;
var
  i: integer;
begin
  Result := '';
  for i := 1 to Length(S) do
    case S[i] of
      '&': Result := Result + '&amp;';
      '<': Result := Result + '&lt;';
      '>': Result := Result + '&gt;';
      '"': Result := Result + '&quot;';
      '''': Result := Result + '&#39;';
      else
        Result := Result + S[i];
    end;
end;

// Remove every TagStart ... TagEnd span, scanning left to right. A TagStart
// without a matching TagEnd after it is kept as text; a TagEnd without a
// TagStart before it is kept as well. The old implementation looked for the
// first TagEnd anywhere in the string and looped forever on "a > b <i>x</i>".
function StripTagsCustom(const Content: string; const TagStart: string;
  const TagEnd: string): string;
var
  p, q, start: integer;
begin
  Result := '';
  if (TagStart = '') or (TagEnd = '') then
    Exit(Content);
  start := 1;
  repeat
    p := PosEx(TagStart, Content, start);
    if p = 0 then
      Break;
    q := PosEx(TagEnd, Content, p + Length(TagStart));
    if q = 0 then
      Break;
    Result := Result + Copy(Content, start, p - start);
    start := q + Length(TagEnd);
  until False;
  Result := Result + Copy(Content, start, MaxInt);
end;

// HTML variant: a '<' only opens a tag when a tag name, '/', '!' or '?'
// follows, so "1 < 2" survives.
function StripTags(const Content: string): string;
var
  p, q, start: integer;
begin
  Result := '';
  start := 1;
  p := 1;
  while p <= Length(Content) do
  begin
    if (Content[p] = '<') and (p < Length(Content)) and
      (Content[p + 1] in ['a'..'z', 'A'..'Z', '/', '!', '?']) then
    begin
      q := PosEx('>', Content, p + 1);
      if q = 0 then
        Break;
      Result := Result + Copy(Content, start, p - start);
      start := q + 1;
      p := start;
    end
    else
      Inc(p);
  end;
  Result := Result + Copy(Content, start, MaxInt);
end;

// Shorten Content to about CharacterCount characters, cutting at a word
// boundary, and append Suffix when something was cut off.
function MoreLess(const Content: string; CharacterCount: integer;
  Suffix: string): string;
var
  p: integer;
begin
  if CharacterCount <= 0 then
    CharacterCount := 100;
  Result := Trim(StripTags(Content));
  if Length(Result) <= CharacterCount then
    Exit;
  // cut at the last word boundary unless the cut already lands on one
  if Result[CharacterCount + 1] <> ' ' then
  begin
    p := RPos(' ', Copy(Result, 1, CharacterCount));
    if p > 1 then
      CharacterCount := p - 1;
  end;
  Result := TrimRight(Copy(Result, 1, CharacterCount));
  if Suffix <> '' then
    Result := Result + ' ' + Suffix;
end;

{ THTMLUtil }

function THTMLUtil.GetFormID: string;
begin
  if FFormID = '' then
  begin
    FFormID := RandomString(__HTMLLIB_FORMID_LENGTH);
  end;
  Result := FFormID;
end;

function THTMLUtil.setTag(const Tag: string; const Content: string;
  const Options: array of string; EndTag: boolean): string;
var
  i: integer;
  s: string;
begin
  s := '<' + Tag + ' ';
  for  i := Low(Options) to High(Options) do
  begin
    s := s + ' ' + Options[i];
  end;
  s := s + '>' + Content;
  if EndTag then
    s := s + '</' + Tag + '>';

  Result := s;
end;

function THTMLUtil.setTag(const Tag: string; const Content: string;
  EndTag: boolean): string;
begin
  Result := '<' + Tag + '>' + Content;
  if EndTag then
    Result := Result + '</' + Tag + '>';
end;


function SecureEquals(const A, B: string): boolean;
var
  i, diff: integer;
begin
  // length mismatch is reported without comparing, the loop itself runs
  // over the whole string regardless of where the first difference is
  if (Length(A) <> Length(B)) or (A = '') then
    Exit(False);
  diff := 0;
  for i := 1 to Length(A) do
    diff := diff or (Ord(A[i]) xor Ord(B[i]));
  Result := diff = 0;
end;

function CsrfSessionKey(const AFormID: string): string;
begin
  Result := __HTML_CSRF_TOKEN_KEY;
  if AFormID <> '' then
    Result := Result + ':' + AFormID;
end;

// Issues a fresh token for AFormID. Each form keeps its own token in the
// session (a page with two forms used to invalidate the first one), and
// __HTML_CSRF_TOKEN_KEY still holds the last token for older callers.
function THTMLUtil.NewCsrfToken(const AFormID: string): string;
begin
  Result := RandomString(__HTMLLIB_FORMCSRFTOKEN_LENGTH, AFormID);
  _SESSION[__HTML_CSRF_TOKEN_KEY] := Result;
  if AFormID <> '' then
    _SESSION[CsrfSessionKey(AFormID)] := Result;
  SessionController.ForceUpdate;
end;

function THTMLUtil.setCsrfTokenHtml(const FormIDDefault: string): string;
var
  s, key: string;
begin
  s := FormIDDefault;
  if s = '' then
    s := FormID;
  key := NewCsrfToken(s);
  Result := #13'<input type="hidden" name="csrftoken" value="' + HTMLEncode(key) +
    '" id="FormCsrfToken_' + HTMLEncode(s) + '" />';
end;

function THTMLUtil.CSRF(const FormIDDefault: string; AsHTML: boolean): string;
begin
  if AsHTML then
    Result := setCsrfTokenHtml(FormIDDefault)
  else
    Result := NewCsrfToken(FormIDDefault);
end;

function THTMLUtil.IsValidCsrfToken(const AToken: string; const AFormID: string): boolean;
begin
  Result := False;
  if AToken = '' then
    Exit;
  if AFormID <> '' then
    Result := SecureEquals(AToken, _SESSION[CsrfSessionKey(AFormID)]);
  if not Result then
    Result := SecureEquals(AToken, _SESSION[__HTML_CSRF_TOKEN_KEY]);
end;

function THTMLUtil.CheckCSRF(Force: boolean): boolean;
begin
  Result := False;
  if _POST['csrftoken'] = '' then
  begin
    if not Force then
      Result := True;
    Exit;
  end;

  Result := IsValidCsrfToken(_POST['csrftoken'], _POST['__formid']);

  if Force then
    ResetCSRF;
end;

procedure THTMLUtil.ResetCSRF;
var
  postedFormId: string;
begin
  _SESSION[__HTML_CSRF_TOKEN_KEY] := '';
  postedFormId := _POST['__formid'];
  if postedFormId <> '' then
    SessionController.DeleteKey(CsrfSessionKey(postedFormId));
  if FFormID <> '' then
    SessionController.DeleteKey(CsrfSessionKey(FFormID));
  SessionController.DeleteKey(__HTML_CSRF_TOKEN_KEY_FAILEDCOUNT);
end;

constructor THTMLUtil.Create;
begin
  FFormID := '';
end;

destructor THTMLUtil.Destroy;
begin
  inherited Destroy;
end;

function THTMLUtil.H1(const Content: string; Options: array of string): string;
begin
  Result := setTag('H1', Content, Options);
end;

function THTMLUtil.H1(const Content: string): string;
begin
  Result := setTag('H1', Content);
end;

function THTMLUtil.Block(const Content: string; Options: array of string): string;
begin
  Result := setTag('div', Content, Options);
end;

function THTMLUtil.Block(const Content: string): string;
begin
  Result := setTag('div', Content);
end;

function THTMLUtil.img(const ImageUrl: string; Options: array of string): string;
begin
  Result := '';
end;

function THTMLUtil.img(const ImageUrl: string): string;
begin
  Result := setTag('img', '', ['src="' + HTMLEncode(ImageUrl) + '"'], False);
end;

function THTMLUtil.AddForm(Options: array of string): string;
begin
  Result := setTag('form', '', Options, False);
  Result := Result + '<input type="hidden" name="__formid" id="form__id" value="' +
    GetFormID + '" />';
  Result := Result + setCsrfTokenHtml;
  Result := Result + '<div>';
  //todo: set session token
end;

function THTMLUtil.EndForm: string;
begin
  //todo: clear session-token, after post
  FFormID := '';
  Result := '</div></form>';
end;

function THTMLUtil.AddInput(Options: array of string; Mandatory: boolean): string;
begin
  Result := setTag('input', '', Options, False);
  if Mandatory then
    Result := Result + '<span class="form-mandatory-flag">*</span>';
end;

// Every caller supplied value is HTML-escaped: Value is typically what the
// user just submitted, so without escaping it could close the attribute and
// inject markup (reflected XSS).
function THTMLUtil.AddInputLTE(InputID, InputType: string; LabelName: string;
  Value: string; Placeholder: string; Required: boolean; ButtonLabel: string): string;
var
  btnName: string;
begin
  InputID := HTMLEncode(InputID);
  InputType := HTMLEncode(InputType);
  LabelName := HTMLEncode(LabelName);
  Value := HTMLEncode(Value);
  Placeholder := HTMLEncode(Placeholder);
  Result := '<div class="form-group">';
  if InputType = 'hidden' then
  begin
    Result := '<input type="hidden" id="'+InputID+'" name="'+InputID+'" value="'+Value+'" >';
    Exit;
  end;
  if InputType = 'checkbox' then
  begin
    Result := Result + '<div class="col-sm-offset-2 col-sm-9">';
    Result := Result + '<div class="checkbox">';
    Result := Result + '<label><input id="' + InputID + '" name="' + InputID +
      '" type="checkbox"> ' + LabelName + '</label>';
    Result := Result + '</div>';
    Result := Result + '</div>';
  end
  else
  begin
    if Value <> '' then
      Value := ' value="' + Value + '" ';
    Result := Result + '<label for="' + InputID + '" class="col-sm-2 control-label">' +
      LabelName + '</label>';
    if InputType = 'password' then
      Result := Result + '<div class="col-sm-5 input-group input-group-sm">'
    else
      Result := Result + '<div class="col-sm-9 input-group input-group-sm">';
    if InputType = 'email' then
      Result := Result + '<span class="input-group-addon"><i class="fa fa-envelope"></i></span>';
    Result := Result + '<input id="' + InputID + '" name="' + InputID +
      '" type="' + InputType + '" class="form-control EntTab" ' + Value + ' placeholder="' + Placeholder + '" ';
    if Required then
      Result := Result + ' required>'
    else
      Result := Result + '>';

    //-- button
    if ButtonLabel <> '' then
    begin
      btnName := 'btn-' + StringReplace(ButtonLabel, ' ', '', [rfReplaceAll]);
      btnName := HTMLEncode(btnName);
      Result := Result + '<span class="input-group-btn"><button id="' +
        btnName + '" name="' + btnName + '" type="button" class="btn btn-info btn-flat">' +
        HTMLEncode(ButtonLabel) + '</button></span>';
    end;

    Result := Result + '</div>';
  end;
  Result := Result + '</div>';
end;

function THTMLUtil.AddSelectLTE(InputID: string; LabelName: string;
  Data: TSQLQuery; IndexFieldName: string; ValueFieldName: string): string;
var
  html: TStringList;
  index, Value: string;
begin
  Result := '';
  if Data = nil then
    Exit;

  InputID := HTMLEncode(InputID);
  html := TStringList.Create;
  try
    Data.First;
    html.Add('<DIV class="form-group">');
    html.Add('<LABEL FOR="' + InputID + '" class="col-sm-2 control-label">' +
      HTMLEncode(LabelName) + '</LABEL>');
    html.Add('<DIV CLASS="col-sm-9 ">');
    html.Add('<SELECT id="' + InputID + '" name="' + InputID + '" class="form-control">');
    html.Add('<OPTION value="0">- NONE -</OPTION>');
    // "repeat ... until EOF" ran the body once on an empty dataset
    while not Data.EOF do
    begin
      index := Data.FieldByName(IndexFieldName).AsString;
      Value := Data.FieldByName(ValueFieldName).AsString;
      html.Add('<OPTION value="' + HTMLEncode(index) + '">' + HTMLEncode(Value) + '</OPTION>');
      Data.Next;
    end;
    html.Add('</SELECT>');
    html.Add('</DIV>');
    html.Add('</DIV>');
    Result := html.Text;
  finally
    html.Free;
  end;
end;

function THTMLUtil.AddField(TagName: string; Options: array of string): string;
begin
  Result := ''; // next development
end;

function THTMLUtil.AddButton(TagName: string; Options: array of string): string;
begin
  Result := ''; // next development
end;

function THTMLUtil.Link(const URL: string; Options: array of string): string;
begin
  Result := Link(URL, URL, Options);
end;

function THTMLUtil.Link(const Text, URL: string; Options: array of string): string;
var
  s: string;
  i: integer;
begin
  s := '';
  for  i := Low(Options) to High(Options) do
  begin
    s := s + ' ' + Options[i];
  end;
  Result := '<a href="' + HTMLEncode(URL) + '"' + s + '>' + HTMLEncode(Text) + '</a>';
end;

// The reCAPTCHA v2 widget. Version is kept for compatibility: the v1 API
// was shut down in 2018, so every version renders the v2 markup.
function THTMLUtil.ReCaptcha(const PublicKey: string; const Version: string): string;
begin
  Result := #13'<div class="g-recaptcha" data-sitekey="' + HTMLEncode(PublicKey) +
    '"></div>' + #13'<script src="https://www.google.com/recaptcha/api.js?hl=en"></script>';
end;

function THTMLUtil.Permalink(Title: string): string;
begin
  Result := SafeText(Title.ToLower).Replace('--', '-').Replace('--', '-');
end;

function THTMLUtil.AddMenu(Title, Icon, URL: string; RgihtLabel: string;
  IsAjax: boolean; AjaxTarget: string): TJSONObject;
var
  o: TJSONObject;
begin
  o := TJSONObject.Create;
  o.Add('title', Title);
  o.Add('icon', Icon);
  o.Add('url', URL);
  if RgihtLabel <> '' then
    o.Add('right-label', RgihtLabel);
  if IsAjax then
    o.Add('ajax', '1');
  if AjaxTarget <> '' then
    o.Add('rel', AjaxTarget);

  Result := o;
end;

function THTMLUtil.AddNotif(NotifType: integer; Title, Icon, URL: string;
  IsAjax: boolean; AjaxTarget: string): TJSONObject;
var
  o: TJSONObject;
begin
  o := TJSONObject.Create;
  o.Add('title', Title);
  o.Add('icon', Icon);
  o.Add('url', URL);
  if IsAjax then
    o.Add('ajax', '1');
  if AjaxTarget <> '' then
    o.Add('rel', AjaxTarget);
  Result := o;
end;

initialization

finalization

end.
