unit about_controller;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, httpdefs, fpHTTP, fpWeb;

type

  { TAboutModule }

  TAboutModule = class(TFPWebModule)
    procedure DataModuleRequest(Sender: TObject; ARequest: TRequest;
      AResponse: TResponse; var Handled: boolean);
    procedure viewRequest(Sender: TObject; ARequest: TRequest;
      AResponse: TResponse; var Handled: boolean);
  private
  public
    procedure TagController(Sender: TObject; const TagString: string;
      TagParams: TStringList; Out ReplaceText: string);
  end;

var
  AboutModule: TAboutModule;

implementation

uses common, fastplaz_handler, theme_controller, versioninfo_lib;

{$R *.lfm}

{ TAboutModule }

procedure TAboutModule.DataModuleRequest(Sender: TObject; ARequest: TRequest;
  AResponse: TResponse; var Handled: boolean);
begin
end;

procedure TAboutModule.viewRequest(Sender: TObject; ARequest: TRequest;
  AResponse: TResponse; var Handled: boolean);
begin
  AResponse.Content := ThemeUtil.Render(@TagController);
  Handled := True;
end;

procedure TAboutModule.TagController(Sender: TObject; const TagString: string;
  TagParams: TStringList; out ReplaceText: string);
var
  tags: TStrings;
  ContributorInfo: TStringList;

  procedure a(const msg: string);
  begin
    ContributorInfo.Add(msg);
  end;

begin
  // fcl-web only: use direct call to ThemeUtil.TagController
  ThemeUtil.TagController(Sender, TagString, TagParams, ReplaceText);

  tags := ExplodeTags(TagString);
  case tags[0] of
    'maincontent':
    begin
      ContributorInfo := TStringList.Create;
      a('<h1>About <a href="' + _APP_URL + '">' + _APP + '</a></h1>');
      a('<p>example vesion: [$version]</p>');
      a('<p>');
      a('<a href="http://www.fastplaz.com">Fastplaz</a> adalah satu satu web framework dengan menggunakan bahasa <a href="http://www.freepascal.org/" target="_blank">free pascal</a>.');
      a('<br>Dibuat sedemikian rupa sehingga requirement server yang diperlukan pun sederhana dan minimal,');
      a('<br>dan bahkan bisa di<i>compile</i> langsung dari console/terminal tanpa memerlukan ide/editor <a href="http://www.lazarus.freepascal.org/" target="_blank">Lazarus</a>.');
      a('<br>Cukup dengan menggunakan <a href="http://httpd.apache.org/" target="_blank">apache</a> biasa seperti di shared hosting, <a href="http://www.fastplaz.com">fastplaz</a> sudah bisa langsung digunakan.');
      a('</p>');
      a('<p>');
      a('Bagi <b>Developer</b>,');
      a('<br>developer bisa langung melakukan kompilasi melalui console/terminal, tanpa perlu melakukan instalasi x/gui maupun lazarus sebagai editornya.');
      a('<br>Cukup install fpc/freepascal, kompilasi sudah bisa dilakukan.');
      a('<br>Desain struktur direktori dan theme/template dibuat sedemikian rupa agar memudahkan dalam melakukan pengembangan aplikasi.');
      a('<br>Untuk kebutuhan dan penggunaan khusus, memang Lazarus diperlukan, tetapi cukup dipasang di pc/desktop/laptop saja.');
      a('</p>');
      a('<p>');
      a('Bagi <b>End User</b>,');
      a('<br>yang menggunakan <i>binary</i> <i>excutable</i> aplikasi/sytem/cms yang berbasis fastplaz,');
      a('<br>diharapkan akan mudah melakukan instalasi tanpa perlu konfigurasi yang menyulitkan.');
      a('<br>Cukup menggunakan <i>shared hosting</i> biasa, aplikasi sudah bisa dipasang.');
      a('</p>');
      a('<p>');
      a('Bagi <b>Consumer</b>, penikmat web/app berbasis fastplaz,');
      a('<br>diharapkan bisa mendapatkan pengalaman menggunakan web-app yang lebih cepat dan ringan');
      a('</p>');
      a('<p>');
      a('<i>** syarat dan ketentuan berlaku</i>');
      a('</p>');
      a('<h3>Initiator</h3>');
      a('<ul>');
      a('<li>Luri Darmawan</li>');
      a('<li>Agung Wicaksana</li>');
      a('</ul>');
      a('<h3>Special Thanks</h3>');
      a('<ul>');
      a('<li>Cahyo Sasongko</li>');
      a('<li>Mario Ray Mahardhika (qtemplate idea)</li>');
      a('<li>Oka Prinarjaya</li>');
      a('<li>Takeda Matsuki</li>');
      a('<li>Tigor Mangatur Manurung</li>');
      a('<li>and All Gemblung Team</li>');
      a('</ul>');

      a('<h3>Contributor</h3>');
      a('<ul>');
      a('<li>You</li>');
      a('</ul>');
      a('<h3>Special Group</h3>');
      a('<ul>');
      a('<li><a href="http://www.facebook.com/groups/Pascal.ID/">Pascal Indonesia</a></li>');
      a('<li><a href="http://www.facebook.com/groups/35688476100/">PHP Indonesia</a></li>');
      a('</ul>');
      a('<div class="bs-callout bs-callout-warning">');
      a('Halaman ini adalah contoh sederhana pemanggilan module "<a href="' +
        _APP_URL + '/about/' + '">about</a>"');
      a('<br>Lihat file "controller file: src/app/about/about_controller.pas"');
      a('</div>');

      a('<div class="highlight pull-right"><pre>');
      a('<code class="bash">controller file: src/app/about/about_controller.pas');
      a('html template  : [ code inline ]');
      a('layout theme   : themes/{$theme}/templates/master.html');
      a('</code></pre></div>');

      ReplaceText := ThemeUtil.RenderFromContent(@TagController, ContributorInfo.Text);
      FreeAndNil(ContributorInfo);
    end; //-- maincontent - end
  end;
end;

initialization
  Route.Add('about', TMyCustomWebModuleClass( TAboutModule), ALL, False);

end.
