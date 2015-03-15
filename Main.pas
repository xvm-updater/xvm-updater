{
    XVM Updater - World of Tanks's XVM one click installer/updater
    Copyright (C) 2013 - 2014  Edgar 'LaCourgette' Fournival

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.
}

unit Main;

interface

uses
  Windows, SysUtils, Classes, IOUtils, Forms, FileCtrl, StrUtils, Controls,
  WoT_Utils, Masks, Languages, ImgList, ButtonGroup, DLThread, AbUnZper,
  AbArcTyp, ComCtrls, StdCtrls;
// Note: I'm using a customized version of ButtonGroup.pas, allowing me to not
//   display the ugly focus rectangle of the TButtonGroup component. However,
//   I can't share the modified source code according to Embarcadero's license.
//   Simply put { ... } around FFocusIndex handler in Paint event or use my
//   compiled version (ButtonGroup.dcu).

// No declaration of this function in Windows unit, strange.
function GetLongPathName(ShortPathName: PChar; LongPathName: PChar;
  cchBuffer: Integer): Integer; stdcall; external kernel32 name 'GetLongPathNameW';

type
  TfWindow = class(TForm)
    lWarning: TLabel;
    gbOptions: TGroupBox;
    cbKeepConfig: TCheckBox;
    eDirectory: TEdit;
    bChangeDirectory: TButton;
    bProcess: TButton;
    cbShowWinChances: TCheckBox;
    lXVMversion: TLabel;
    cmbXVMVersion: TComboBox;
    lConfig: TLabel;
    cmbConfig: TComboBox;
    cbEnableStatsDisplay: TCheckBox;
    bgLanguage: TButtonGroup;
    ilLanguages: TImageList;
    gbProgress: TGroupBox;
    lCurrentAction: TLabel;
    pbCurrentAction: TProgressBar;
    procedure FormCreate(Sender: TObject);
    procedure bChangeDirectoryClick(Sender: TObject);
    procedure bProcessClick(Sender: TObject);
    procedure cbKeepConfigClick(Sender: TObject);
    procedure bgLanguageButtonClicked(Sender: TObject; Index: Integer);
    procedure cmbXVMVersionChange(Sender: TObject);
  private
    DLThread: TDLThread;
    VersionsFiles, ConfigsFiles: TStringList;
    WOTDir: String;
    Version: String;
    CustomScript: String;
    LastNightlyRev: String;
    function Replace(Data: PString; Version: String):Boolean;
    procedure UnzipProgress(Sender: TObject; Progress: Byte; var Abort: Boolean);
    function Parse(Data: String; Execute: Boolean):Integer;
    procedure SetVersion;
    procedure UpdateVersions(Data: TMemoryStream);
    procedure UpdateConfigs(Data: TMemoryStream);
    procedure UpdateNightlyRev(Data: TMemoryStream);
    procedure ChangeLanguage(NewLng: TLanguage);
  public
    currentLanguage: TLanguage;
  end;

var
  fWindow: TfWindow;

implementation

{$R *.dfm}


procedure TfWindow.bChangeDirectoryClick(Sender: TObject);
var
  chosenDirectory: String;
  okDir: Boolean;
begin
  okDir := false;
  repeat
    if SelectDirectory(sSelectDirectory[currentLanguage], '', chosenDirectory) then
      begin
        if FileExists(chosenDirectory+'\worldoftanks.exe') then
          begin
            WOTDir := chosenDirectory;
            okDir := true;
            SetVersion;
          end
        else
          if MessageBox(0, PChar(sFailDirectory[currentLanguage]), 'XVM Updater', +mb_YESNO +mb_ICONWARNING) = 7 then
            okDir := true;
      end
    else okDir := true;
  until okDir;
end;


procedure TfWindow.SetVersion;
begin
  eDirectory.Text := WOTDir;
  Version := GetVersion(WOTDir+'\worldoftanks.exe');
  TDLThread.Create(
    'http://edgar-fournival.fr/obj/wotxvm/xvm-versions?version='+Version,
    UpdateVersions);
  LastNightlyRev := '';
end;


procedure TfWindow.bProcessClick(Sender: TObject);
var
  Script: AnsiString;
  DVersion, ScriptURL: String;
begin
  Script := '';

  if WOTDir = '' then
    begin
      MessageBox(0,
        PChar(sSpecifyDirectory[currentLanguage]),
        'XVM Updater', +mb_OK +mb_ICONWARNING);
      Exit;
    end;

  // Check if client is closed
  if ProcessExists('worldoftanks.exe') then
    begin
      MessageBox(0,
        PChar(sClientRunning[currentLanguage]),
        'XVM Updater', +mb_OK +mb_ICONINFORMATION);
      Exit;
    end;

  pbCurrentAction.State := pbsNormal;

  // Disable controls on main form
  bChangeDirectory.Enabled := false;
  cbKeepConfig.Enabled := false;
  cbShowWinChances.Enabled := false;
  cbEnableStatsDisplay.Enabled := false;
  bProcess.Enabled := false;
  cmbConfig.Enabled := false;
  cmbXVMversion.Enabled := false;

  // We want to download the XVM version corresponding to user's choice
  DVersion := Version;
  if cmbXVMversion.ItemIndex > 0 then
    DVersion := VersionsFiles[cmbXVMversion.ItemIndex];
  ScriptURL := 'http://edgar-fournival.fr/obj/wotxvm/script?version='+DVersion;

  // Set up script source if forced from command line
  if CustomScript <> '' then
    begin
      if FileExists(CustomScript) then
        Script := TFile.ReadAllText(CustomScript)
      else
        ScriptURL := CustomScript;
    end;

  Parse('PERCENT = 00', true);

  // If local file as script source isn't forced
  if (Script = '') then
    begin
      Parse('STATUS_'+
        LanguageMin[currentLanguage]+' = '+
        sScriptDownload[currentLanguage], true);

      DLThread := TDLThread.Create(ScriptURL, True);
      try
        DLThread.Start;
        repeat
          Sleep(10);
          Application.ProcessMessages;
        until DLThread.Finished or Application.Terminated;
        if Application.Terminated then Exit;
        SetString(Script, PAnsiChar(DLThread.Data.Memory), DLThread.Data.Size);
      finally
        DLThread.Data.Free;
        DLThread.Free;
      end;
    end;

  // Process
  if Length(Script) > 0 then
    begin
      Parse('PERCENT = 05', true);
      Parse('STATUS_'+
        LanguageMin[currentLanguage]+' = '+
        sInformationsCollecting[currentLanguage], true);

      if Replace(@Script, Version) then
        begin
          Parse('PERCENT = 10', true);
          Parse(Script, true);
          pbCurrentAction.Max := 100;
          pbCurrentAction.Position := 100;
        end;
    end;

  // Re-enable main form controls
  bChangeDirectory.Enabled := true;
  cbKeepConfig.Enabled := true;
  cbShowWinChances.Enabled := true;
  cbEnableStatsDisplay.Enabled := true;
  bProcess.Enabled := true;

  if (cmbConfig.Items.Count > 1) and (not cbKeepConfig.Checked) then
    cmbConfig.Enabled := true;
  if cmbXVMversion.Items.Count > 1 then
    cmbXVMversion.Enabled := true;

  bProcess.Caption := siInstallUpdate[currentLanguage];
end;


procedure TfWindow.cbKeepConfigClick(Sender: TObject);
begin
  if cbKeepConfig.Checked then
    cmbConfig.Enabled := False
  else
    cmbConfig.Enabled := True;
end;


function TfWindow.Replace(Data: PString; Version: String):Boolean;

  function ExtractLongPathName(const ShortName: string): string;
  begin
    SetLength(Result, GetLongPathName(PChar(ShortName), nil, 0));
    SetLength(Result, GetLongPathName(PChar(ShortName), PChar(Result), Length(Result)));
  end;

var
  TempFolder, WinFolder: String;
  Versions: TStringList;
  lng: DWORD;
begin
  // %TEMP%
  SetLength(TempFolder, MAX_PATH);
  lng := GetTempPath(MAX_PATH, PChar(TempFolder));
  SetLength(TempFolder, lng-1);
  TempFolder := ExtractLongPathName(TempFolder);  // Fix for failing Windows functions
  Data^ := StringReplace(Data^, '%TEMP%', TempFolder, [rfReplaceAll]);

  // %WINDOWS%
  SetLength(WinFolder, MAX_PATH);
  lng := GetWindowsDirectory(PChar(WinFolder), MAX_PATH);
  SetLength(WinFolder, lng);
  Data^ := StringReplace(Data^, '%WINDOWS%', WinFolder, [rfReplaceAll]);

  // %CUSTOMCONFIG%
  if (cmbConfig.ItemIndex > 0) then
    Data^ := StringReplace(Data^, '%CUSTOMCONFIG%',
      ConfigsFiles[cmbConfig.ItemIndex-1], [rfReplaceAll]);

  // OPTIONS:
  Data^ := StringReplace(Data^, 'OPTIONsaveConfig',
    IntToStr(Integer(cbKeepConfig.Checked)), [rfReplaceAll]);
  Data^ := StringReplace(Data^, 'OPTIONshowWinChances',
    IntToStr(Integer(cbShowWinChances.Checked)), [rfReplaceAll]);
  Data^ := StringReplace(Data^, 'OPTIONenableStats',
    IntToStr(Integer(cbEnableStatsDisplay.Checked)), [rfReplaceAll]);
  Data^ := StringReplace(Data^, 'OPTIONcustomConfig',
    IntToStr(Integer((cmbConfig.ItemIndex > 0) and (not cbKeepConfig.Checked))), [rfReplaceAll]);

  // %WOTOLDVERSION%
  try
    Versions := TStringList.Create;
    GetSubDirectories(eDirectory.Text+'\res_mods\', Versions);
    Versions.Sort;
    if Versions.Count > 1 then
      Data^ := StringReplace(Data^, '%WOTOLDVERSION%',
                             Versions[Versions.Count - 2], [rfReplaceAll]);
  except
    MessageBox(0,
      PChar(sFailModsDir[currentLanguage]),
      'XVM Updater', +mb_OK +mb_ICONWARNING);
    Result := false;
    Exit;
  end;

  Data^ := StringReplace(Data^, '%WOT%', WOTDir, [rfReplaceAll]);
  Data^ := StringReplace(Data^, '%WOTVERSION%', Version, [rfReplaceAll]);

  Result := true;
end;


procedure TfWindow.UnzipProgress(Sender: TObject; Progress: Byte; var Abort: Boolean);
begin
  pbCurrentAction.Position := Progress;
  Application.ProcessMessages;
end;


function TfWindow.Parse(Data: String; Execute: Boolean):Integer;

  function ReadLine(Data: String; Position: PInteger):String;
    begin
      Result := '';

      while ((Data[Position^] <> #10) and
             (Data[Position^] <> #13) and
             (Position^ <= Length(Data))) do
        begin
          Result := Result + Data[Position^];
          Inc(Position^);
        end;
    end;

var
  Buffer, LineBuffer: String;
  Position: Integer;
  Unzipper: TAbUnzipper;
begin
  Position := 1;
  Buffer := '';
  LineBuffer := '';

  while Position < Length(Data) do
    begin
      Application.ProcessMessages;

      if ((Data[Position] = #13) or (Data[Position] = #10)) then

        begin
          if (AnsiLeftStr(Buffer, 5) = 'ENDIF') then
            begin
              Result := Position;
              Exit;
            end;

          Buffer := '';
          Position := Position + 1;
        end

      else if (Data[Position] <> ' ') then

        begin
          Buffer := Buffer + Data[Position];
          Inc(Position);
        end

      else
        begin
          // STATUS: write the current performed action in 'Current action:' line.
          // EX:
          //   STATUS = Downloading XVM...
          if (AnsiLeftStr(Buffer, 6) = 'STATUS') then

            begin
              LineBuffer := ReadLine(Data, @Position);
              if Execute then
                begin
                  if AnsiRightStr(Buffer, 2) = LanguageMin[currentLanguage] then
                    lCurrentAction.Caption := siCurrentAction[currentLanguage] +
                      AnsiRightStr(LineBuffer, Length(LineBuffer)-3)
                  // Default is english
                  else if AnsiRightStr(Buffer, 2) = LanguageMin[lngEN] then
                    lCurrentAction.Caption := siCurrentAction[currentLanguage] +
                      AnsiRightStr(LineBuffer, Length(LineBuffer)-3);
                end;
            end

          // GETFILE: Download a file from the Internet and store it on the disk.
          // EX:
          //   GETFILE "http://aaa.com/bbb.txt" "C:\ccc.txt"
          else if (AnsiLeftStr(Buffer, 7) = 'GETFILE') then

            begin
              LineBuffer := ReadLine(Data, @Position);
              if Execute then
                begin
                  LineBuffer := AnsiRightStr(LineBuffer, Length(LineBuffer)-1);
                  DLThread := TDLThread.Create(StrSplit(LineBuffer)[0], True);
                  DLThread.Start;

                  repeat
                    Sleep(10);
                    Application.ProcessMessages;
                  until DLThread.Finished or Application.Terminated;

                  if (DLThread.Data.Size < 1) or
                     DLThread.Failed or
                     Application.Terminated then
                    begin
                      Result := Length(Data);
                      Exit;
                    end;

                  DLThread.Data.SaveToFile(StrSplit(LineBuffer)[1]);
                  DLThread.Data.Free;
                  DLThread.Free;
                end;
            end

          // PERCENT: define the process progression (progress bar position).
          // EX:
          //   PERCENT = 50
          else if (AnsiLeftStr(Buffer, 7) = 'PERCENT') then

            begin
              LineBuffer := ReadLine(Data, @Position);
              if Execute then
                bProcess.Caption := Format('%s (%d%%)',
                  [siInstallUpdate[currentLanguage], StrToInt(AnsiRightStr(LineBuffer, Length(LineBuffer)-3))]);
            end

          // UNPACK: unzip a specified zipfile in a specified directory.
          // EX:
          //   UNPACK "C:\aaa.zip" "C:\Folder"
          else if (AnsiLeftStr(Buffer, 6) = 'UNPACK') then

            begin
              LineBuffer := ReadLine(Data, @Position);
              if Execute then
                begin
                  pbCurrentAction.Position := 0;
                  LineBuffer := AnsiRightStr(LineBuffer, Length(LineBuffer)-1);

                  Unzipper := TAbUnzipper.Create(nil);
                  with Unzipper do begin
                    FileName := StrSplit(LineBuffer)[0];
                    BaseDirectory := StrSplit(LineBuffer)[1]+'\';
                    ExtractOptions := [eoCreateDirs, eoRestorePath];
                    OnArchiveProgress := UnzipProgress;
                    ExtractFiles('*.*');
                  end;
                  Unzipper.Destroy;
                end;
            end

          // COPY: copy a file.
          // EX:
          //   COPY "C:\aaa.txt" "C:\bbb.txt"
          else if (AnsiLeftStr(Buffer, 4) = 'COPY') then

            begin
              LineBuffer := ReadLine(Data, @Position);
              if Execute then
                begin
                  LineBuffer := AnsiRightStr(LineBuffer, Length(LineBuffer)-1);
                  FCopy(StrSplit(LineBuffer)[0], StrSplit(LineBuffer)[1]);
                end;
            end

          // RUN: execute a specified file and wait process termination.
          // Second parameter: specified parameters for the process (can be null).
          // EX:
          //   RUN "C:\aaa.exe" ""
          else if (AnsiLeftStr(Buffer, 3) = 'RUN') then

            begin
              LineBuffer := ReadLine(Data, @Position);
              if Execute then
                begin
                  pbCurrentAction.Style := pbstMarquee;
                  LineBuffer := AnsiRightStr(LineBuffer, Length(LineBuffer)-1);
                  ExecuteAndWait(StrSplit(LineBuffer)[0], StrSplit(LineBuffer)[1], Application);

                  if Application.Terminated then
                    begin
                      Result := Length(Data);
                      Exit;
                    end;

                  pbCurrentAction.Style := pbstNormal;
                end;
            end

          // CONFIG_SWC: enables win chances display in XVM configuration.
          // EX:
          //   CONFIG_SWC C:\XVM.xvmconf
          else if (AnsiLeftStr(Buffer, 10) = 'CONFIG_SWC') then

            begin
              LineBuffer := ReadLine(Data, @Position);
              if Execute then
                begin
                  LineBuffer := AnsiRightStr(LineBuffer, Length(LineBuffer)-1);
                  if FileExists(LineBuffer) then
                    begin
                      FileReplaceString(LineBuffer, '"showChances": false',
                        '"showChances": true');
                      FileReplaceString(LineBuffer, '"showChances":false',
                        '"showChances": true');
                    end;
                end;
            end

          // CONFIG_ESD: enables player stats display in XVM configuration.
          // EX:
          //   CONFIG_ESD C:\XVM.xvmconf
          else if (AnsiLeftStr(Buffer, 10) = 'CONFIG_ESD') then

            begin
              LineBuffer := ReadLine(Data, @Position);
              if Execute then
                begin
                  LineBuffer := AnsiRightStr(LineBuffer, Length(LineBuffer)-1);
                  if FileExists(LineBuffer) then
                    begin
                      FileReplaceString(LineBuffer, '"showPlayersStatistics": false',
                        '"showPlayersStatistics": true');
                      FileReplaceString(LineBuffer, '"showPlayersStatistics":false',
                      '"showPlayersStatistics": true');
                    end;
                end;
            end

          // CONFIG_RTC: removes trailing configs/ path in the boot config file.
          // EX:
          //   CONFIG_RTC C:\XVM.xvmconf
          else if (AnsiLeftStr(Buffer, 10) = 'CONFIG_RTC') then

            begin
              LineBuffer := ReadLine(Data, @Position);
              if Execute then
                begin
                  LineBuffer := AnsiRightStr(LineBuffer, Length(LineBuffer)-1);
                  if FileExists(LineBuffer) then
                    FileReplaceString(LineBuffer, '${"configs/', '${"');
                end;
            end

          // DELETEDIR: delete a specified directory.
          // EX:
          //   DELETEDIR C:\Folder
          else if (AnsiLeftStr(Buffer, 9) = 'DELETEDIR') then

            begin
              LineBuffer := ReadLine(Data, @Position);
              if Execute then
                if DirectoryExists(AnsiRightStr(LineBuffer, Length(LineBuffer)-1)+'\') then
                  DeleteDirectory(AnsiRightStr(LineBuffer, Length(LineBuffer)-1)+'\');
            end

          else if (AnsiLeftStr(Buffer, 6) = 'DELETE') then

            begin
              LineBuffer := ReadLine(Data, @Position);
              if Execute then
                SysUtils.DeleteFile(AnsiRightStr(LineBuffer, Length(LineBuffer)-1));
            end

          // IF_NEXISTS: execute the next commands block if the specified file doesn't exist.
          // EX:
          //   IF_NEXISTS C:\aaa.txt
          else if (AnsiLeftStr(Buffer, 10) = 'IF_NEXISTS') then

            begin
              LineBuffer := ReadLine(Data, @Position);
              if Execute then
                begin
                  try
                    if FileExists(AnsiRightStr(LineBuffer, Length(LineBuffer)-1)) then
                      Position := Position +
                        Parse(AnsiRightStr(Data, Length(Data)-Position), false)
                    else
                      Position := Position +
                        Parse(AnsiRightStr(Data, Length(Data)-Position), true);
                  except
                    Position := Position +
                      Parse(AnsiRightStr(Data, Length(Data)-Position), true);
                  end;
                end;
            end

          // IF_EXISTS: execute the next commands block if the specified file exists.
          // EX:
          //   IF_EXISTS C:\aaa.txt
          else if (AnsiLeftStr(Buffer, 9) = 'IF_EXISTS') then

            begin
              LineBuffer := ReadLine(Data, @Position);
              if Execute then
                begin
                  try
                    if FileExists(AnsiRightStr(LineBuffer, Length(LineBuffer)-1)) then
                      Position := Position +
                        Parse(AnsiRightStr(Data, Length(Data)-Position), true)
                    else
                      Position := Position +
                        parse(AnsiRightStr(Data, Length(Data)-Position), false);
                  except
                    Position := Position +
                      Parse(AnsiRightStr(Data, Length(Data)-Position), false);
                  end;
                end;
            end

          // IF: execute the next commands block if the IF statement is followed by '1'.
          // Used with 'Replace' procedure that defines checked options in the script.
          // EX:
          //   IF 1
          else if (AnsiLeftStr(Buffer, 2) = 'IF') then

            begin
              LineBuffer := ReadLine(Data, @Position);
              if Execute then
                begin
                  if AnsiRightStr(LineBuffer, Length(LineBuffer)-1) = '1' then
                    Position := Position +
                      Parse(AnsiRightStr(Data, Length(Data)-Position), true)
                  else
                    Position := Position +
                      Parse(AnsiRightStr(Data, Length(Data)-Position), false);
                end;
            end

          else

            begin
              Buffer := '';
              ReadLine(Data, @Position);
            end;

        end;
    end;

  Result := Length(Data);
end;


procedure TfWindow.UpdateVersions(Data: TMemoryStream);
var
  Position: Integer;
  Line, Versions: String;
begin
  SetString(Versions, PAnsiChar(Data.Memory), Data.Size);
  Position := 1;

  cmbXVMVersion.Items.Clear;
  VersionsFiles.Clear;

  while Position < Length(Versions) do
    begin
      Line := '';
      while ((Versions[Position] <> #10) and
             (Versions[Position] <> #13) and
             (Position <= Length(Versions))) do
        begin
          Line := Line + Versions[Position];
          Inc(Position);
        end;
      Inc(Position);
      if Trim(Line) <> '' then
        begin
          cmbXVMversion.Items.Add(StrSplit(Line)[0]);
          VersionsFiles.Add(StrSplit(Line)[1]);
        end;
    end;

  // Avoid garbage in the menu if HTML code is returned
  if cmbXVMversion.Items.Count > 10 then cmbXVMVersion.Items.Clear;

  cmbXVMversion.ItemIndex := 0;
  if cmbXVMversion.Items.Count < 1 then
    begin
      cmbXVMversion.ItemIndex := cmbXVMversion.Items.Add(sStable[currentLanguage]);
      versionsFiles.Add('');
    end;

  if cmbXVMversion.Items.Count > 1 then
    cmbXVMversion.Enabled := true
  else
    cmbXVMversion.Enabled := false;
end;


procedure TfWindow.UpdateConfigs(Data: TMemoryStream);
var
  Position: Integer;
  Line, Versions: String;
begin
  SetString(Versions, PAnsiChar(Data.Memory), Data.Size);
  Position := 1;

  while Position < Length(Versions) do
    begin
      Line := '';
      while ((Versions[Position] <> #10) and
             (Versions[Position] <> #13) and
             (Position <= Length(Versions))) do
        begin
          Line := Line + Versions[Position];
          Inc(Position);
        end;
      Inc(Position);
      if Trim(Line) <> '' then
        begin
          cmbConfig.Items.Add(StrSplit(Line)[0]);
          ConfigsFiles.Add(StrSplit(Line)[1]);
        end;
    end;

  // Avoid garbage in the menu if HTML code is returned
  if cmbConfig.Items.Count > 10 then
    begin
      cmbConfig.Items.Clear;
      cmbConfig.ItemIndex := cmbConfig.Items.Add(sDefault[currentLanguage]);
    end;

  if cmbConfig.Items.Count > 1 then cmbConfig.Enabled := true;
end;


procedure TfWindow.UpdateNightlyRev(Data: TMemoryStream);
var
  I: Integer;
  OldItemIndex: Integer;
begin
  // Avoid garbage
  if (Data.Size < 10) and (Data.Size > 0) then
    begin
      SetString(LastNightlyRev, PAnsiChar(Data.Memory), Data.Size);

      for I := 0 to cmbXVMversion.Items.Count - 1 do
        if MatchesMask(cmbXVMversion.Items[I], '*nightly*') then
          begin
            OldItemIndex := cmbXVMversion.ItemIndex;
            cmbXVMversion.Items[I] := cmbXVMversion.Items[I]+' ('+LastNightlyRev+')';
            cmbXVMversion.ItemIndex := OldItemIndex;
          end;
    end;
end;


procedure TfWindow.bgLanguageButtonClicked(Sender: TObject; Index: Integer);
begin
  ChangeLanguage(TLanguage(Index));
end;


procedure TfWindow.ChangeLanguage(NewLng: TLanguage);
var
  OldLng: TLanguage;
begin
  OldLng := currentLanguage;
  currentLanguage := NewLng;

  lWarning.Caption := siWarning[currentLanguage];
  gbOptions.Caption := siOptions[currentLanguage];
  cbKeepConfig.Caption := siKeepConfig[currentLanguage];
  bChangeDirectory.Caption := siModify[currentLanguage];
  gbProgress.Caption := siProgress[currentLanguage];
  lCurrentAction.Caption := siCurrentAction[currentLanguage];
  bProcess.Caption := siInstallUpdate[currentLanguage];
  fWindow.Caption := siForm[currentLanguage];
  cbShowWinChances.Caption := siShowWinChances[currentLanguage];
  lXVMversion.Caption := siXVMversion[currentLanguage];
  lConfig.Caption := siConfig[currentLanguage];
  cbEnableStatsDisplay.Caption := siEnableStats[currentLanguage];

  cmbXVMversion.Left := 114 + (lXVMversion.Width - 96);
  lConfig.Left := 265 - (lConfig.Width - 88);

  if cmbConfig.Items[0] = sDefault[OldLng] then
    begin
      cmbConfig.Items[0] := sDefault[currentLanguage];
      if not(cmbConfig.ItemIndex > 0) then cmbConfig.ItemIndex := 0;
    end;

  if cmbXVMversion.Items[0] = sStable[OldLng] then
    begin
      cmbXVMversion.Items[0] := sStable[currentLanguage];
      if not(cmbConfig.ItemIndex > 0) then cmbXVMversion.ItemIndex := 0;
    end;
end;


procedure TfWindow.cmbXVMVersionChange(Sender: TObject);
begin
  if MatchesMask(cmbXVMversion.Items[cmbXVMversion.ItemIndex], '*nightly*') and
     (Length(LastNightlyRev) = 0) then
    TDLThread.Create(
      'http://edgar-fournival.fr/obj/wotxvm/get_last_nightly',
      UpdateNightlyRev);
end;

procedure TfWindow.FormCreate(Sender: TObject);
const
  WoT_Dir: array[1..9] of String = (
      'World_of_Tanks',
      'World of Tanks',
      'Games\World_of_Tanks',
      'Games\World of Tanks',
      'Program Files (x86)\World_of_Tanks',
      'Program Files (x86)\World of Tanks',
      'Program Files\World_of_Tanks',
      'Program Files\World of Tanks',
      'World_of_Tanks_closed_Beta'
    );
var
  vDrivesSize: Cardinal;
  vDrives: array[0..128] of Char;
  vDrive: PChar;
  I: Integer;
  searchResult: TSearchRec;
  currentSC: String;
  systemDrive: String;
  Directory: String;
begin
  // DYNAMIC VERSIONS & CONFIGS LOADING SUPPORT
  VersionsFiles := TStringList.Create;
  ConfigsFiles := TStringList.Create;

  cmbXVMversion.ItemIndex := cmbXVMversion.Items.Add(sStable[currentLanguage]);
  cmbConfig.ItemIndex := cmbConfig.Items.Add(sDefault[currentLanguage]);
  TDLThread.Create('http://edgar-fournival.fr/obj/wotxvm/xvm-configs.php',
    UpdateConfigs);

  {MessageBox(0, 'XVM Updater '+_VERSION_+' - TEST RELEASE'+#13#10+
                'DO NOT SHARE'+#13#10+
                'MAY BE UNSTABLE', 'XVM Updater', +mb_OK +mb_ICONWARNING);}

  // AUTO LANGUAGE SELECTION
  // http://msdn.microsoft.com/en-us/library/cc233965.aspx
  // http://msdn.microsoft.com/en-us/library/dd318135.aspx
       if (GetUserDefaultLCID and $00FF) = $0C then ChangeLanguage(lngFR)
  else if (GetUserDefaultLCID and $00FF) = $07 then ChangeLanguage(lngDE)
  else if (GetUserDefaultLCID and $00FF) = $15 then ChangeLanguage(lngPL)
  else if (GetUserDefaultLCID and $00FF) = $19 then ChangeLanguage(lngRU)
  else if (GetUserDefaultLCID and $00FF) = $22 then ChangeLanguage(lngUA)
  else if (GetUserDefaultLCID and $00FF) = $0E then ChangeLanguage(lngHU)
  else if (GetUserDefaultLCID and $00FF) = $0B then ChangeLanguage(lngFI)
  else if (GetUserDefaultLCID and $00FF) = $13 then ChangeLanguage(lngNL)
  else ChangeLanguage(lngEN);

  bgLanguage.ItemIndex := Integer(currentLanguage);

  // FORCING SCRIPT SOURCE FROM COMMAND LINE SUPPORT
  CustomScript := '';
  if ParamCount > 0 then
    begin
      CustomScript := ParamStr(1);
      MessageBox(0,
        PChar(sForcedSource[currentLanguage]),
        'XVM Updater', +mb_OK +mb_ICONINFORMATION);
    end;

  // WOT INSTALLATION FOLDER AUTO-DETECTION

  // Checking global Start menu shortcut (fast)
  try
    GetDir(0, systemDrive);
    if FileExists(systemDrive[1]+':\ProgramData\Microsoft\Windows\Start Menu\Programs\World of Tanks\World of tanks.lnk') then
      begin
        Directory := ExtractFileDir(
          GetShortcutTarget(
            systemDrive[1]+':\ProgramData\Microsoft\Windows\Start Menu\Programs\World of Tanks\World of tanks.lnk'));
        if FileExists(Directory + '\worldoftanks.exe') then
          begin
            WOTDir := Directory;
            SetVersion;
            Exit;
          end;
      end;
  except
    // Fail silently.
  end;

  // Checking desktop shortcut linking to "worldoftanks.exe" or "xvm-stat.exe" (slow)
  try
    if FindFirst(GetDesktop+'\'+'*.lnk', faAnyFile, searchResult) = 0 then
      begin
        repeat
          CurrentSC := GetShortcutTarget(GetDesktop+'\'+searchResult.Name);
          if MatchesMask(CurrentSC, '*WorldOfTanks.exe') or
             MatchesMask(CurrentSC, '*xvm-stat.exe') or
             MatchesMask(CurrentSC, '*WOTLauncher.exe') then
            begin
              Directory := ExtractFileDir(CurrentSC);
              if FileExists(Directory + '\worldoftanks.exe') then
                begin
                  WOTDir := Directory;
                  SetVersion;
                  Exit;
                end;
            end;
        until FindNext(searchResult) <> 0;
        SysUtils.FindClose(searchResult);
      end;
  except
    // Fail silently.
  end;

  // Checking known directories on each disk (slower)
  try
    vDrivesSize := GetLogicalDriveStrings(SizeOf(vDrives), vDrives);
    if vDrivesSize = 0 then Exit;

    vDrive := vDrives;
    while vDrive^ <> #0 do
      begin
        if IsDriveReady(vDrive) then
          begin
            for I := 1 to Length(WoT_Dir) do
              begin
                if DirectoryExists(StrPas(vDrive)+WoT_Dir[I]+'\') then
                  begin
                    Directory := StrPas(vDrive)+WoT_Dir[I];
                    if FileExists(Directory + '\worldoftanks.exe') then
                      begin
                        WOTDir := Directory;
                        SetVersion;
                        Exit;
                      end;
                  end;
              end;
          end;
        Inc(vDrive, SizeOf(vDrive));
      end;
  except
    // Fail silently.
  end;
end;

end.
