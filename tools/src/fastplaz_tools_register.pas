unit fastplaz_tools_register;

{$mode objfpc}{$H+}

interface

uses
  Dialogs, LazarusPackageIntf, ProjectIntf, NewItemIntf, IDEMsgIntf, IDEExternToolIntf,
  FileUtil, PackageIntf, Classes, SysUtils;

const
  FastPlaz = 'FastPlaz';
  _APP_SLOGAN = 'Fast Web Development Framework for Pascal';
  {$ifdef windows}
  _APP_EXTENSION = '.exe';
  {$else}
  _APP_EXTENSION = '.bin';

  {$endif}


procedure Register;
procedure log(const Msg: string; AFileName: string = '';
  ATheUrgency: TMessageLineUrgency = mluNote);
function ucwords(const str: string): string;
function ScanDirAndCopy(SourceDirectory, TargetDirectory: string; AProjectName: string = 'fastplaz'): boolean;

var
  bCreateProject: boolean = False;
  bExpert: boolean = False;
  FastPlazRuntimeDirectory: string;
  ModulTypeName, Permalink: string;

implementation

uses modsimple_lib, modsimplejson_lib, model_lib, project_lib, projectapi_lib,
  packageapp_lib, menu_experts;

function ucwords(const str: string): string;
var
  i: integer;
  s: string;
begin
  s := ' ' + lowerCase(str);
  for i := 1 to Length(s) do
  begin
    if s[i] = ' ' then
      s[i + 1] := upcase(s[i + 1]);
  end;
  Result := trim(s);
end;

function ScanDirAndCopy(SourceDirectory, TargetDirectory: string;
  AProjectName: string): boolean;
var
  Rec: TSearchRec;
  R: integer;
  lsFileSource, lsFileTarget, lsSubDir: string;
  targetFileName: string;
begin
  Result := True;
  try
    R := FindFirst(SourceDirectory, faAnyFile, Rec);
    while R = 0 do
    begin
      if ((Rec.Attr and faDirectory) <> 0) and (Rec.Name <> '.') and
        (Rec.Name <> '..') then
      begin
        lsSubDir := extractfilepath(SourceDirectory) + Rec.Name;
        ForceDirectories(TargetDirectory + DirectorySeparator + Rec.Name);
        ScanDirAndCopy(lsSubDir + DirectorySeparator + '*', TargetDirectory +
          DirectorySeparator + Rec.Name);
      end
      else
      begin
        if ((Rec.Name <> '.') and (Rec.Name <> '..')) then
        begin
          lsFileSource := extractfilepath(SourceDirectory) + Rec.Name;
          targetFileName := Rec.Name;
          targetFileName := StringReplace(targetFileName, '%projectname%', AProjectName, [rfReplaceAll]);
          lsFileTarget := TargetDirectory + DirectorySeparator + targetFileName;
          CopyFile(lsFileSource, lsFileTarget);
        end;
      end;
      R := FindNext(Rec);
    end;
  finally
    FindClose(Rec);
  end;
end;

procedure log(const Msg: string; AFileName: string; ATheUrgency: TMessageLineUrgency);
begin
  if IDEMessagesWindow <> nil then
    IDEMessagesWindow.AddCustomMessage(ATheUrgency, Msg, AFileName, 0, 0, FastPlaz);
end;

procedure Register;
begin
  CreateIDEMenus;

  //RegisterUnit('fastplaz_tools_register', @fastplaz_tools_register.Register);
  RegisterNewItemCategory(TNewIDEItemCategory.Create(FastPlaz));
  RegisterProjectDescriptor(TProjectFastPlazDescriptor.Create, FastPlaz);
  RegisterProjectDescriptor(TProjectAPIFastPlazDescriptor.Create, FastPlaz);
  //RegisterProjectDescriptor(TPackageAppDescriptor.Create, FastPlaz);

  RegisterProjectFileDescriptor(TFileDescDefaultModule.Create, FastPlaz);
  RegisterProjectFileDescriptor(TFileDescJSONModule.Create, FastPlaz);
  RegisterProjectFileDescriptor(TFileDescModel.Create, FastPlaz);
  //RegisterProjectFileDescriptor(TWebStructure.Create, FastPlaz);

end;


initialization
  //RegisterPackage('FastPlaz', @Register);


end.
