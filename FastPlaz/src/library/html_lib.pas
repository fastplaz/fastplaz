unit html_lib;

{$mode objfpc}{$H+}

interface

uses
  strutils,
  Classes, SysUtils;

function H1(Content: string; StyleClass: string = ''): string;
function H2(Content: string; StyleClass: string = ''): string;
function H3(Content: string; StyleClass: string = ''): string;
function li(Content: string; StyleClass: string = ''): string;
function Span(Content: string; StyleClass: string = ''): string;
function Block(Content: string; StyleClass: string = ''; BlockID:string=''): string;

function StripTags( const Content:string):string;
function StripTagsCustom( const Content:string; const TagStart:string; const TagEnd:string):string;
function MoreLess( const Content:string; CharacterCount:integer=100):string;

implementation

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
  s : string;
begin
  if BlockID <> '' then
    s := ' id="'+BlockID+'" ';
  if StyleClass = '' then
    Result := '<div'+s+'>' + Content + '</div>'
  else
    Result := '<div '+s+' class="' + StyleClass + '">' + Content + '</div>';
end;

function StripTags(const Content: string): string;
var
  s : string;
begin
  s := Content;
  while ((Pos('<',s) > 0) or (Pos('>',s) > 0)) do
    s := StringReplace(s,
      copy(s,pos('<',s),pos('>',s)-pos('<',s)+1),'',[rfIgnoreCase,rfReplaceAll]);
  Result := s;
end;

function StripTagsCustom(const Content: string; const TagStart: string;
  const TagEnd: string): string;
var
  s : string;
begin
  s := Content;
  while ((Pos(TagStart,s) > 0) or (Pos(TagEnd,s) > 0)) do
    s := StringReplace(s,
      copy(s,pos(TagStart,s),pos(TagEnd,s)-pos(TagStart,s)+1),'',[rfIgnoreCase,rfReplaceAll]);
  Result := s;
end;

function MoreLess(const Content: string; CharacterCount: integer): string;
begin
  Result:=Copy(Content,1,CharacterCount);
  Result:=Copy(Result,1,RPos(' ', Result)-1);
end;

end.

