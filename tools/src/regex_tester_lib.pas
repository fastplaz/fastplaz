unit regex_tester_lib;

{$mode objfpc}{$H+}

interface

uses
  regexpr_lib, Classes, SysUtils;

const
  REGEX_EMAIL = '([\w\d\-\.]+@[\w\d\-]+(\.[\w\d\-]+)+)';
  REGEX_EMAIL2 = '[^\w\d\-\.]([\w\d\-\.]+@[\w\d\-]+(\.[\w\d\-]+)+)[^\w\d\-\.]';
  REGEX_URL = '((http)|(https)|(ftp)):\/\/([\-\w]+\.)+\w{2,}(\/[%\-\w]+(\.\w{2,})?)*';
  REGEX_IP =
    '^(25[0-5]|2[0-4][0-9]|[0-1]{1}[0-9]{2}|[1-9]{1}[0-9]{1}|[1-9])\.(25[0-5]' +
    '|2[0-4][0-9]|[0-1]{1}[0-9]{2}|[1-9]{1}[0-9]{1}|[1-9]|0)\.(25[0-5]|2[0-4]' +
    '[0-9]|[0-1]{1}[0-9]{2}|[1-9]{1}[0-9]{1}|[1-9]|0)\.(25[0-5]|2[0-4][0-9]|[' +
    '0-1]{1}[0-9]{2}|[1-9]{1}[0-9]{1}|[0-9])$';
  REGEX_TELP = '\(([0-9]{2}|0{1}((x|[0-9]){2}[0-9]{2}))\)\s*[0-9]{3,4}[- ]*[0-9]{4}';
  REGEX_TELP2 = '^[01]?[- .]?\(?[2-9]\d{2}\)?[- .]?\d{3}[- .]?\d{4}$';

  REGEX_NONAME_GROUP = '[noname]';

// Runs AExpression against AText.
// AMatches receives every match, AGroups receives "index name = value"
// for each group of the first match. Returns the number of matches.
// Raises an exception if AExpression is invalid.
function RegexExecute(const AExpression, AText: string;
  AMatches, AGroups: TStrings): integer;

implementation

function RegexExecute(const AExpression, AText: string;
  AMatches, AGroups: TStrings): integer;
var
  i: integer;
  s: string;
  re: TRegExpr;
begin
  Result := 0;
  AMatches.Clear;
  AGroups.Clear;
  if (AExpression = '') or (AText = '') then
    Exit;

  re := TRegExpr.Create;
  try
    re.Expression := RegExprString(AExpression);
    if not re.Exec(RegExprString(AText)) then
      Exit;

    // groups are taken from the first match
    for i := 1 to re.GroupCount do
    begin
      s := string(re.GroupName[i]);
      if s.IsEmpty then
        s := REGEX_NONAME_GROUP;
      AGroups.Add(Format('%d %s = %s', [i, s, string(re.Match[i])]));
    end;
    repeat
      AMatches.Add(string(re.Match[0]));
    until not re.ExecNext();
    Result := AMatches.Count;
  finally
    re.Free;
  end;
end;

end.
