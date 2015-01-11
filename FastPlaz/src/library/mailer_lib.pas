unit mailer_lib;

{$mode objfpc}{$H+}
{$include ../../define.inc}

interface

uses
  {$IFDEF SYNAPSE}
    {$IFDEF XMAILER}
  xmailer,
    {$ENDIF}
  {$ENDIF}
  Classes, SysUtils;

type

  { TMailer }
  TMailer = class
  private
    FErrorMessage: string;
    FMailServer: string;
    FMailPassword: string;
    FMailUserName: string;
    FPort: string;
    FTo, FCc, FBcc: TStringList;
    FEmailFormat: string;
    FLogs: string;
    FSSL, FTLS: boolean;
    function getEmailFormat: string;
    function getMailServer: string;
    function getSmtpPort: string;
    function getSSL: boolean;
    function getTLS: boolean;
    procedure setEmailFormat(AValue: string);
    procedure setMailPassword(AValue: string);
    procedure setMailServer(AValue: string);
    procedure setMailUserName(AValue: string);
    procedure setSmptPort(AValue: string);
    procedure setSSL(AValue: boolean);
    procedure setTLS(AValue: boolean);

    {$IFDEF XMAILER}
    procedure xmailer_OnProgress(const AProgress, AMax: integer; const AStatus: string);
    {$ENDIF XMAILER}
  public
    Subject, Sender: string;
    Message: TStringList;
    constructor Create;
    destructor Destroy; override;
    property emailFormat: string read getEmailFormat write setEmailFormat;
    property MailServer: string read getMailServer write setMailServer;
    property Port: string read getSmtpPort write setSmptPort;
    property UserName: string read FMailUserName write setMailUserName;
    property Password: string read FMailPassword write setMailPassword;
    property SSL: boolean read getSSL write setSSL;
    property TLS: boolean read getTLS write setTLS;
    property ErrorMessage: string read FErrorMessage;
    property Logs: string read FLogs;
    procedure AddTo(Email: string; Name: string = '');
    procedure AddCc(Email: string; Name: string = '');
    procedure AddBcc(Email: string; Name: string = '');
    procedure Clear;
    function Send: boolean;
  end;


implementation

uses common;

{ TMailer }

procedure TMailer.AddTo(Email: string; Name: string);
begin
  if Name = '' then
    FTo.Add(Email)
  else
    FTo.Add(Name + '<' + Email + '>');
end;

procedure TMailer.AddCc(Email: string; Name: string);
begin
  if Name = '' then
    FCc.Add(Email)
  else
    FCc.Add(Name + '<' + Email + '>');
end;

procedure TMailer.AddBcc(Email: string; Name: string);
begin
  if Name = '' then
    FBcc.Add(Email)
  else
    FBcc.Add(Name + '<' + Email + '>');
end;

procedure TMailer.Clear;
begin
  FTo.Clear;
  FBcc.Clear;
  FCc.Clear;
  FLogs := '';
  Subject := '';
  Message.Clear;
end;

function TMailer.Send: boolean;
{$IFDEF XMAILER}
var
  Mail: TSendMail;
{$ENDIF XMAILER}
begin
  Result := False;

  {$IFDEF XMAILER}
  try
    try
      Mail := TSendMail.Create;
      Mail.OnProgress := @xmailer_OnProgress;
      Mail.Smtp.Host := FMailServer;
      Mail.Smtp.Port := FPort;
      Mail.Smtp.UserName := FMailUserName;
      Mail.Smtp.Password := FMailPassword;
      Mail.Smtp.SSL := FSSL;
      Mail.Smtp.TLS := FTLS;

      Mail.Sender := Sender;
      Mail.Receivers.Text := FTo.Text;
      Mail.Subject := Subject;
      Mail.Message.Text := Message.Text;
      if FEmailFormat = 'html' then
        Mail.ContentType := ctTextHTML
      else
        Mail.ContentType := ctTextPlain;
      Mail.Send;
      FLogs := FLogs + 'OK';
      Result := True;
    except
      on e: Exception do
      begin
        FErrorMessage := e.Message;
        FLogs := e.Message + #13;
      end;
    end;
  finally
    Mail.Free;
  end;
  {$ENDIF XMAILER}

end;

function TMailer.getMailServer: string;
begin
  Result := FMailServer;
end;

function TMailer.getEmailFormat: string;
begin
  Result := FEmailFormat;
end;

function TMailer.getSmtpPort: string;
begin
  Result := FPort;
end;

function TMailer.getSSL: boolean;
begin
  Result := FSSL;
end;

function TMailer.getTLS: boolean;
begin
  Result := FTLS;
end;

procedure TMailer.setEmailFormat(AValue: string);
begin
  FEmailFormat := AValue;
end;

procedure TMailer.setMailPassword(AValue: string);
begin
  if FMailPassword = AValue then
    Exit;
  FMailPassword := AValue;
end;

procedure TMailer.setMailServer(AValue: string);
begin
  if FMailServer = AValue then
    Exit;
  FMailServer := AValue;
end;

procedure TMailer.setMailUserName(AValue: string);
begin
  if FMailUserName = AValue then
    Exit;
  FMailUserName := AValue;
end;

procedure TMailer.setSmptPort(AValue: string);
begin
  if FPort = AValue then
    Exit;
  FPort := AValue;
end;

procedure TMailer.setSSL(AValue: boolean);
begin
  FSSL := AValue;
end;

procedure TMailer.setTLS(AValue: boolean);
begin
  FTLS := AValue;
end;

{$IFDEF XMAILER}
procedure TMailer.xmailer_OnProgress(const AProgress, AMax: integer; const AStatus: string);
begin
  FLogs := FLogs + FormatDateTime('YYYY-mm-dd hh:nn:ss', now) + ' | ' + AStatus + #13;
end;
{$ENDIF XMAILER}

constructor TMailer.Create;
begin
  FTo := TStringList.Create;
  FCc := TStringList.Create;
  FBcc := TStringList.Create;
  message := TStringList.Create;
  FSSL := False;
  FTLS := False;
  FEmailFormat := 'html';
  FLogs := '';
end;

destructor TMailer.Destroy;
begin
  inherited Destroy;
  FreeAndNil(message);
  FreeAndNil(FTo);
  FreeAndNil(FCc);
  FreeAndNil(FBcc);
end;

end.
