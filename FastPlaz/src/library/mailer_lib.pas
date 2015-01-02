unit mailer_lib;

{$mode objfpc}{$H+}
{$include ../../define.inc}

interface

uses
  Classes, SysUtils;

type

{
CakePHP
$email->to($toEmailAddress);
$email->from(array('no-reply@rumah123.com' => 'Rumah123.com'));
$email->subject('Listing Inquiry '.$d['ads_id'].' Rumah123.com');
$email->emailFormat('html');
$email->viewVars($dataAgents);
$email->template('v4-enquiry', 'v4-default');

$email->send();

}
  { TMailer }
  TMailer = class
  private
    FTo, FCc, FBcc: TStringList;
    FEmailFormat, FMailServer: string;
    FPort: integer;
    function getEmailFormat: string;
    function getMailServer: string;
    function getSmtpPort: integer;
    procedure setEmailFormat(AValue: string);
    procedure setMailServer(AValue: string);
    procedure setSmptPort(AValue: integer);
  public
    subject, fromName, fromEmail: string;
    message: TStringList;
    constructor Create;
    destructor Destroy; override;
    property emailFormat: string read getEmailFormat write setEmailFormat;
    property MailServer: string read getMailServer write setMailServer;
    property SmtpPort: integer read getSmtpPort write setSmptPort;
    procedure AddTo(Email: string; Name: string = '');
    procedure AddCc(Email: string; Name: string = '');
    procedure AddBcc(Email: string; Name: string = '');
    procedure Clear;
    function Send: boolean;
  end;


implementation

{ TMailer }

procedure TMailer.AddTo(Email: string; Name: string);
begin

end;

procedure TMailer.AddCc(Email: string; Name: string);
begin

end;

procedure TMailer.AddBcc(Email: string; Name: string);
begin

end;

procedure TMailer.Clear;
begin
  FTo.Clear;
  FBcc.Clear;
  FCc.Clear;
  subject := '';
  fromName := '';
  fromEmail := '';
end;

function TMailer.Send: boolean;
begin
  Result := True;
end;

function TMailer.getMailServer: string;
begin
  Result := '';
end;

function TMailer.getEmailFormat: string;
begin
  Result := FEmailFormat;
end;

function TMailer.getSmtpPort: integer;
begin

end;

procedure TMailer.setEmailFormat(AValue: string);
begin
  FEmailFormat := AValue;

end;

procedure TMailer.setMailServer(AValue: string);
begin

end;

procedure TMailer.setSmptPort(AValue: integer);
begin

end;

constructor TMailer.Create;
begin
  FTo := TStringList.Create;
  FCc := TStringList.Create;
  FBcc := TStringList.Create;
  message := TStringList.Create;
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
