unit user_controller;

{$mode objfpc}{$H+}

interface

uses
  fpjson,
  Classes, SysUtils, fpcgi, HTTPDefs, fastplaz_handler, html_lib, user_util, user_model,
  database_lib, security_util, language_lib, mailer_lib;

//const
//  {$include '../../../define_cms.inc'}

type

  { TUserModule }

  TUserModule = class(TMyCustomWebModule)
    procedure RequestHandler(Sender: TObject; ARequest: TRequest;
      AResponse: TResponse; var Handled: boolean);
  private
    function Tag_MainContent_Handler(const TagName: string;
      Params: TStringList): string;
    function Tag_UserMenu(const TagName: string; Params: TStringList): string;
    function Tag_UserInfo(const TagName: string; Params: TStringList): string;
    procedure BeforeRequestHandler(Sender: TObject; ARequest: TRequest);
  public
    User: TUserUtil;
    Mail: TMailer;
    constructor CreateNew(AOwner: TComponent; CreateMode: integer); override;
    destructor Destroy; override;

    function Login: string;
    function DoLogin: string;
    function Logout: string;
    function Dashboard: string;
    function ChangePassword: string;
    function LostPassword: string;
    function Register: string;
    function DoRegister: string;

    procedure onLoginAttemps(Sender: TObject);
  end;

implementation

uses theme_controller, common, logutil_lib;

constructor TUserModule.CreateNew(AOwner: TComponent; CreateMode: integer);
begin
  inherited CreateNew(AOwner, CreateMode);

  DataBaseInit;
  LanguageInit;
  QueryExec('SET CHARACTER SET utf8;');

  User := TUserUtil.Create();
  User.LoginAttempsMax := 5;
  User.OnLoginAttemps := @onLoginAttemps;
  OnRequest := @RequestHandler;
  BeforeRequest := @BeforeRequestHandler;
  Tags['userinfo'] := @Tag_UserInfo;
end;

destructor TUserModule.Destroy;
begin
  inherited Destroy;
  if Assigned(User) then
    User.Free;
end;

function TUserModule.Login: string;
var
  s: string;
  i: integer;
begin
  if Session.IsExpired then
    Session.Clear;
  if User.FailedLoginCount >= User.LoginAttempsMax then
  begin
    Result := ThemeUtil.RenderFromContent(@TagController, '',
      'modules/users/view/login-attempts.html');
    Exit;
  end;

  if User.isLoggedIn then
    Redirect(BaseURL + USER_URL_DASHBOARD);

  if isPost then
    doLogin
  else
  begin
    Result := ThemeUtil.RenderFromContent(@TagController, '',
      'modules/users/view/login.html');
  end;
end;

function TUserModule.DoLogin: string;
begin
  if not isPost then
    Exit;

  if not isValidCSRF then
  begin
    ThemeUtil.FlashMessages := MSG_TOKEN_INVALID;
    Redirect(BaseURL);
  end;

  if not User.Login(_POST['data[username]'], _POST['data[password]']) then
  begin
    ThemeUtil.FlashMessages := Format( MSG_LOGIN_INVALID, [User.FailedLoginCount]);
    Redirect(BaseURL + USER_URL_LOGIN);
  end;

  if _GET['url'] = '' then
    Redirect(BaseURL + USER_URL_DASHBOARD)
  else
    Redirect(BaseURL + _GET['url']);
end;

function TUserModule.Logout: string;
begin
  User.Logout;
  Redirect(BaseURL + USER_URL_LOGIN);
end;

function TUserModule.Dashboard: string;
begin
  if not User.isLoggedIn or Session.IsExpired then
    Redirect(BaseURL + USER_URL_LOGIN);


  Result := ThemeUtil.RenderFromContent(@TagController, '',
    'modules/users/view/dashboard.html');
end;

function TUserModule.ChangePassword: string;
var
  userId: integer;
  hashedPassword: string;
begin
  Result := '';
  if not User.isLoggedIn then
    Redirect(BaseURL + USER_URL_LOGIN);
  if isPost then
  begin
    if (_POST['data[oldpassword]'] = '') or (_POST['data[password]'] = '') or
      (_POST['data[password2]'] = '') then
    begin
      Redirect(BaseURL + USER_URL_CHANGEPASSWORD, MSG_FIELD_INCORRECT);
    end;

    if _POST['data[password]'] <> _POST['data[password2]'] then
    begin
      Redirect(BaseURL + USER_URL_CHANGEPASSWORD, MSG_PASSWORDNEW_INVALID);
    end;

    userId := User.UserIdLoggedIn;

    if User.FindFirst([USER_FIELDNAME_ID + '=' + i2s(userId)]) then
    begin
      hashedPassword := User[USER_FIELDNAME_PASSWORD];
      with TSecurityUtil.Create do
      begin
        if not CheckSaltedHash(_POST['data[oldpassword]'], hashedPassword) then
        begin
          Redirect(BaseURL + USER_URL_CHANGEPASSWORD, MSG_PASSWORD_INVALID);
        end;

        if User.ChangePassword(userId, _POST['data[password]']) then
        begin
          Redirect(BaseURL + USER_URL_DASHBOARD, MSG_PASSWORD_UPDATED);
        end
        else
        begin
          Redirect(BaseURL + USER_URL_CHANGEPASSWORD, MSG_PASSWORD_FAILED);
        end;

        Free;
      end;
    end;

  end;

  Result := ThemeUtil.RenderFromContent(@TagController, Result,
    'modules/users/view/changepassword.html');
end;

function TUserModule.LostPassword: string; // reset password
begin
  Result := '';
  if isPost then
  begin
    if not isValidCSRF then
    begin
      Redirect(BaseURL + USER_URL_LOSTPASSWORD);
    end;
    if not User.isEmailExists(_POST['data[email]']) then

    begin
      ThemeUtil.FlashMessages := MSG_USERNAMEEMAIL_NOT_EXISTS;
    end
    else
    begin

      // --- send lost password
      Result := '... password send ...';

    end;
  end;
  Result := ThemeUtil.RenderFromContent(@TagController, Result,
    'modules/users/view/lostpassword.html');
end;

function TUserModule.Register: string;
begin
  Result := '';
  if isPost and isAjax then
  begin
    Result := DoRegister;
    die(Result);
  end;
  User.Logout;
  Result := ThemeUtil.RenderFromContent(@TagController, Result,
    'modules/users/view/register.html');
end;

function TUserModule.DoRegister: string;
var
  code: integer;
  token, email, pass1, pass2: string;

  function setOutput(code: integer; message: string; url: string = ''): string;
  var
    o, oData: TJSONObject;
  begin
    o := TJSONObject.Create();
    o.Add('code', code);
    o.Add('msg', message);
    if url <> '' then
      o.Add('url', url);
    o.Add('token', HTMLUtil.CSRF('registerform'));
    Result := JsonFormatter(o.AsJSON);
    FreeAndNil(o);
  end;

begin
  token := _SESSION[__HTML_CSRF_TOKEN_KEY];
  if not isValidCSRF then
  begin
    //HTMLUtil.ResetCSRF;
    //Result := setOutput(9, 'Security: Invalid CSRF Token: ' + _POST['csrftoken'] + '--' + token);
    //Exit;
  end;

  //HTMLUtil.ResetCSRF;
  code := -1;
  email := trim(_POST['data[email]']);
  pass1 := trim(_POST['data[password]']);
  pass2 := trim(_POST['data[password2]']);

  if (email = '') or (pass1 = '') or (pass2 = '') then
  begin
    Result := setOutput(1, MSG_VALUE_INVALID);
    Exit;
  end;

  if User.isEmailExists(email) then
  begin
    Result := setOutput(2, MSG_USER_EXISTS);
    Exit;
  end;

  if pass1 <> pass2 then
  begin
    Result := setOutput(3, MSG_PASSWORD_INVALID);
    Exit;
  end;

  code := User.Add(email, pass1, nil);
  if code = 0 then
    Result := setOutput(3, MSG_USER_ADD_FAILED)
  else
  begin
    Result := setOutput(0, 'OK', BaseURL + USER_URL_REGISTER_THANKYOU);

    //-- TODO: send welcome email
    Mail := TMailer.Create();
    Mail.AddTo(email);
    Mail.Subject := 'Welcome Email - ' + AppData.sitename;


    // .....

    FreeAndNil(Mail);

  end;
end;

procedure TUserModule.onLoginAttemps(Sender: TObject);
begin
  // notification too many login attempts

  Redirect(BaseURL + USER_URL_LOGIN);
end;


procedure TUserModule.RequestHandler(Sender: TObject; ARequest: TRequest;
  AResponse: TResponse; var Handled: boolean);
begin
  if isAjax then
    Response.ContentType := 'application/json';

  ThemeUtil.Layout := 'user';
  //if ARequest.PathInfo = '/user-dashbpard' then
  //  ThemeUtil.Layout := 'user-dashboard';

  Tags['maincontent'] := @Tag_MainContent_Handler; //<<-- tag maincontent handler
  Tags['usermenu'] := @Tag_UserMenu;
  Response.Content := ThemeUtil.Render();
  Handled := True;
end;

function TUserModule.Tag_MainContent_Handler(const TagName: string;
  Params: TStringList): string;
var
  action: string;
begin
  action := _GET['$2'];
  if TagName = 'maincontent' then
  begin
    case action of
      'login':
      begin
        Result := Login;
      end;
      'logout':
      begin
        Result := Logout;
      end;
      'dashboard':
      begin
        Result := Dashboard;
      end;
      'changepassword':
      begin
        Result := ChangePassword;
      end;
      'lostpassword':
      begin
        Result := LostPassword;
      end;
      'register':
      begin
        Result := Register;
      end;

    end;

  end;//-- if TagName = 'maincontent' then
end;

function TUserModule.Tag_UserMenu(const TagName: string; Params: TStringList): string;

begin
  Result := '<b>this is user-menu</b>';
  if User.isHaveAdmin then
    Result := Result + 'have admin';
end;

function TUserModule.Tag_UserInfo(const TagName: string; Params: TStringList): string;
begin
  Result := User.UserInfo[Params.Values['type']];
end;

procedure TUserModule.BeforeRequestHandler(Sender: TObject; ARequest: TRequest);
begin
  LogUtil.Add('before request', 'user');
end;


initialization
{
  Add this to route.pas
  //Route.Add('^(user|member)-(login|logout|dashboard|lostpassword|register|list)/?$', TUserModule);
}
end.
