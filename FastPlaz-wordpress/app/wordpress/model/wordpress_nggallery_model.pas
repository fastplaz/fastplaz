unit wordpress_nggallery_model;

{$mode objfpc}{$H+}

interface

uses
  database_lib, fpTemplate,
  Classes, SysUtils;

type

  { TWPNGGallery }

  TWPNGGallery = class(TSimpleModel)
  private
    FBaseURL: string;
    FPath: string;
    procedure ngTagController(Sender: TObject; const TagString:String; TagParams: TStringList; Out ReplaceText: String);
  public
    constructor Create(const DefaultTableName: string = '');
    destructor Destroy; override;
    property Path: string read FPath write FPath;
    property BaseURL: string read FBaseURL write FBaseURL;
    function Render(const Content:string):string;
  end;

var
  WPNGGallery: TWPNGGallery;

implementation

uses common, fastplaz_handler, logutil_lib, theme_controller;

{ TWPNGGallery }

procedure TWPNGGallery.ngTagController(Sender: TObject;
  const TagString: String; TagParams: TStringList; out ReplaceText: String);
var
  tags : TStrings;
  id, src: string;
begin
  tags := Explode(TagString, ' ');
  if tags.Count = 0 then
  begin
    FreeAndNil(tags);
    Exit;
  end;

  case tags[0] of
    'singlepic':
    begin
      id := tags.Values['id'];
      AddJoin(AppData.table_prefix+'_ngg_gallery', 'gid', 'galleryid', ['path']);
      FindFirst([AppData.table_prefix + '_ngg_pictures.pid=' + id],
        '', 'filename,alttext');
      if Data.RecordCount > 0 then
      begin
        src := BaseURL + FPath + '/' + Value['path'].AsString + '/' +
          Value['filename'].AsString;
        ReplaceText := '<div><img src="' + src + '"';
        if tags.Values['w'] <> '' then
          ReplaceText := ReplaceText + ' width="' + tags.Values['w'];
        if tags.Values['h'] <> '' then
          ReplaceText := ReplaceText + ' height="' + tags.Values['h'];
        if tags.Values['float'] <> '' then
          ReplaceText := ReplaceText + ' class="ngg-singlepic float_' + tags.Values['float'] + '"';
        ReplaceText := ReplaceText + ' alt="' + Value['alttext'].AsString + '"';
        ReplaceText := ReplaceText + ' title="' + Value['alttext'].AsString + '"';
        ReplaceText := ReplaceText + '></div>';
      end;
    end;
  end;

end;

constructor TWPNGGallery.Create(const DefaultTableName: string);
begin
  inherited Create(AppData.table_prefix + '_ngg_pictures');
  Path := Config.GetValue('wordpress/path', '');
  BaseURL := Config.GetValue('wordpress/base_url', '');
  if BaseURL = '' then
  begin
    BaseURL := 'http://' + GetEnvironmentVariable('SERVER_NAME');
  end;
end;

destructor TWPNGGallery.Destroy;
begin
  inherited Destroy;
end;

function TWPNGGallery.Render(const Content: string): string;
var
  template: TFPTemplate;
begin
  Result := Content;
  template := TFPTemplate.Create;
  template.Template := Content;
  template.AllowTagParams := True;
  template.StartDelimiter := '[';
  template.EndDelimiter := ']';
  template.ParamValueSeparator := '=';
  template.OnReplaceTag := @ngTagController;
  Result := template.GetContent;
  FreeAndNil(template);
end;

initialization
  //WPNGGallery:= TWPNGGallery.Create;

finalization
  //FreeAndNil( WPNGGallery);

end.
