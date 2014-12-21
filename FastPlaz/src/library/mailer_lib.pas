unit mailer_lib;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type

  { TMailer }

  TMailer = class
  private
    FTo, FCc, FBcc: TStringList;
    FMailServer: string;
    FPort: integer;
    function getMailServer: string;
    function getSmtpPort: integer;
    procedure setMailServer(AValue: string);
    procedure setSmptPort(AValue: integer);
  public
    constructor Create;
    destructor Destroy; override;
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

end;

function TMailer.Send: boolean;
begin

end;

function TMailer.getMailServer: string;
begin

end;

function TMailer.getSmtpPort: integer;
begin

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
end;

destructor TMailer.Destroy;
begin
  inherited Destroy;
  FreeAndNil(FTo);
  FreeAndNil(FCc);
  FreeAndNil(FBcc);
end;

end.
