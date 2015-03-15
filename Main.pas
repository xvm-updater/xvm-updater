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
  WoT_Utils, Languages, ImgList, DLThread, AbUnZper, AbArcTyp, Math,
  ComCtrls, StdCtrls, ProgressStatus, Menus, Masks, DetectionThread, pngimage,
  ExtCtrls, ButtonGroup;
// Note: I'm using a customized version of StdCtrls.pas, allowing me to not
//   display the ugly focus rectangle of the TComboBox component. However,
//   I can't share the modified source code according to Embarcadero's license.
//   Simply comment this line in TCustomComboBox.CNDrawItem:
//      if odFocused in State then DrawFocusRect(hDC, rcItem);
//   and change the previous if by:
//      if (Integer(itemID) >= 0) and (odSelected in State) and not(odComboBoxEdit in State) then
//   or use my compiled version (StdCtrls.dcu).

// No declaration of this function in the Windows unit, strange.
function GetLongPathName(ShortPathName: PChar; LongPathName: PChar;
  cchBuffer: Integer): Integer; stdcall; external kernel32 name 'GetLongPathNameW';

type
  TProcessMode = (pmInstallUpdate, pmApplyOptions);

  TfWindow = class(TForm)
    bgLanguage: TButtonGroup;
    gbOptions: TGroupBox;
    cbKeepConfig: TCheckBox;
    bProcess: TButton;
    lXVMversion: TLabel;
    cmbXVMVersion: TComboBox;
    cbEnableStatsDisplay: TCheckBox;
    ilLanguages: TImageList;
    pmProcess: TPopupMenu;
    miInstallUpdate: TMenuItem;
    miApplyOptions: TMenuItem;
    cbInstallations: TComboBox;
    iDonate: TImage;
    lDonate: TLabel;
    bMoreOptions: TButton;
    GroupBox1: TGroupBox;
    lCourgette: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure bProcessClick(Sender: TObject);
    procedure cmbXVMVersionChange(Sender: TObject);
    procedure miInstallUpdateClick(Sender: TObject);
    procedure miApplyOptionsClick(Sender: TObject);
    procedure cbInstallationsChange(Sender: TObject);
    procedure bgLanguageButtonClicked(Sender: TObject; Index: Integer);
  private
    DLThread: TDLThread;
    DetectionThread: TDetectionThread;
    VersionsFiles, ConfigsFiles: TStringList;
    WOTDir: String;
    Version: String;
    CustomScript: String;
    LastNightlyRev: String;
    ProcessMode: TProcessMode;
    PreviousItem: Integer;
    function Replace(Data: PString; Version: String):Boolean;
    procedure UnzipProgress(Sender: TObject; Progress: Byte; var Abort: Boolean);
    function Parse(Data: String; Execute: Boolean):Integer;
    procedure UpdateVersions(Data: TMemoryStream);
    procedure UpdateNightlyRev(Data: TMemoryStream);
    procedure ChangeLanguage(NewLng: TLanguage);
  public
    currentLanguage: TLanguage;
    Status: TProgressStatus;
    procedure SetVersion(Path: String);
  end;

var
  fWindow: TfWindow;

implementation

{$R *.dfm}


procedure TfWindow.SetVersion(Path: String);
begin
  WOTDir := Path;
  cbInstallations.ItemIndex := cbInstallations.Items.IndexOf(Path);
  PreviousItem := cbInstallations.ItemIndex;
  Version := GetVersion(WOTDir + '\worldoftanks.exe');
  //TDLThread.Create(
  //  'http://edgar-fournival.fr/obj/wotxvm/xvm-versions?version='+Version,
  //  UpdateVersions);
end;


procedure TfWindow.miApplyOptionsClick(Sender: TObject);
begin
  ProcessMode := pmApplyOptions;
  bProcess.Caption := miApplyOptions.Caption;
end;


procedure TfWindow.miInstallUpdateClick(Sender: TObject);
begin
  ProcessMode := pmInstallUpdate;
  bProcess.Caption := miInstallUpdate.Caption;
end;


procedure TfWindow.bgLanguageButtonClicked(Sender: TObject; Index: Integer);
begin
  ChangeLanguage(TLanguage(Index));
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

  Status.SetupControls(Self);

  // Disable controls on main form
  cbInstallations.Enabled := false;
  cbKeepConfig.Enabled := false;
  cbEnableStatsDisplay.Enabled := false;
  bProcess.Enabled := false;
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

        if not(DLThread.Failed) then
          SetString(Script, PAnsiChar(DLThread.Data.Memory), DLThread.Data.Size);
      finally
        DLThread.Data.Free;
        DLThread.Free;
      end;
    end;

  // Process
  if Length(Script) > 0 then
    begin
      Parse('STATUS_'+
        LanguageMin[currentLanguage]+' = '+
        sInformationsCollecting[currentLanguage], true);

      if Replace(@Script, Version) then Parse(Script, true);
    end;

  Status.SetProgress(100);

  // Re-enable main form controls
  cbInstallations.Enabled := true;
  cbKeepConfig.Enabled := true;
  cbEnableStatsDisplay.Enabled := true;
  bProcess.Enabled := true;

  if cmbXVMversion.Items.Count > 1 then cmbXVMversion.Enabled := true;
end;


procedure TfWindow.cbInstallationsChange(Sender: TObject);
var
  chosenDirectory: String;
  okDir: Boolean;
begin
  if cbInstallations.ItemIndex = cbInstallations.Items.Count-1 then
    begin
      cbInstallations.ItemIndex := PreviousItem;
      okDir := false;
      repeat
        if SelectDirectory(sSelectDirectory[currentLanguage], '', chosenDirectory) then
          begin
            if FileExists(chosenDirectory + '\worldoftanks.exe') then
              begin
                okDir := true;
                cbInstallations.Items.Insert(cbInstallations.Items.Count-1,
                                             chosenDirectory);
                SetVersion(chosenDirectory);
              end
            else
              if MessageBox(0, PChar(sFailDirectory[currentLanguage]), 'XVM Updater', +mb_YESNO +mb_ICONWARNING) = 7 then
                okDir := true;
          end
        else okDir := true;
      until okDir;
    end
  else SetVersion(cbInstallations.Text);
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
  //if (cmbConfig.ItemIndex > 0) then
  //  Data^ := StringReplace(Data^, '%CUSTOMCONFIG%',
  //    ConfigsFiles[cmbConfig.ItemIndex-1], [rfReplaceAll]);

  // OPTIONS:
  Data^ := StringReplace(Data^, 'OPTIONsaveConfig',
    IntToStr(Integer(cbKeepConfig.Checked)), [rfReplaceAll]);
  Data^ := StringReplace(Data^, 'OPTIONenableStats',
    IntToStr(Integer(cbEnableStatsDisplay.Checked)), [rfReplaceAll]);
  //Data^ := StringReplace(Data^, 'OPTIONcustomConfig',
  //  IntToStr(Integer((cmbConfig.ItemIndex > 0) and (not cbKeepConfig.Checked))), [rfReplaceAll]);

  // %WOTOLDVERSION%
  try
    Versions := TStringList.Create;
    GetSubDirectories(WOTDir + '\res_mods\', Versions);
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
  Status.SetProgress(Progress);
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
                    Status.SetCurrentOperation(AnsiRightStr(LineBuffer, Length(LineBuffer)-3))
                  // Default is english
                  else if AnsiRightStr(Buffer, 2) = LanguageMin[lngEN] then
                    Status.SetCurrentOperation(AnsiRightStr(LineBuffer, Length(LineBuffer)-3));
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

          // UNPACK: unzip a specified zipfile in a specified directory.
          // EX:
          //   UNPACK "C:\aaa.zip" "C:\Folder"
          else if (AnsiLeftStr(Buffer, 6) = 'UNPACK') then

            begin
              LineBuffer := ReadLine(Data, @Position);
              if Execute then
                begin
                  Status.SetNormal;
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
                  Status.SetWaiting;
                  LineBuffer := AnsiRightStr(LineBuffer, Length(LineBuffer)-1);
                  ExecuteAndWait(StrSplit(LineBuffer)[0], StrSplit(LineBuffer)[1], Application);

                  if Application.Terminated then
                    begin
                      Result := Length(Data);
                      Exit;
                    end;

                  Status.SetNormal;
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
              // Ignore
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


procedure TfWindow.ChangeLanguage(NewLng: TLanguage);
var
  OldLng: TLanguage;
begin
  OldLng := currentLanguage;
  currentLanguage := NewLng;

  gbOptions.Caption := siOptions[currentLanguage];
  cbKeepConfig.Caption := siKeepConfig[currentLanguage];
  //bChangeDirectory.Caption := siModify[currentLanguage];
  bProcess.Caption := IfThen(ProcessMode = pmInstallUpdate,
                         siInstallUpdate[currentLanguage],
                         siApplyOptions[currentLanguage]);
  fWindow.Caption := siForm[currentLanguage];
  lXVMversion.Caption := siXVMversion[currentLanguage];
  cbEnableStatsDisplay.Caption := siEnableStats[currentLanguage];
  miInstallUpdate.Caption := siInstallUpdate[currentLanguage];
  miApplyOptions.Caption := siApplyOptions[currentLanguage];

  lDonate.Caption := siDonate[currentLanguage];
  lDonate.Font.Height := IfThen(currentLanguage = lngRU, -10, -14);
  if currentLanguage = lngUA then lDonate.Font.Height := -13;
  lDonate.Top := IfThen(currentLanguage = lngRU, iDonate.Top + 5, iDonate.Top + 3);

  cbInstallations.Items.Delete(cbInstallations.Items.Count-1);
  cbInstallations.Items.Add(siBrowse[currentLanguage]);

  cmbXVMversion.Left := 114 + (lXVMversion.Width - 96);

  if cmbXVMversion.Items[0] = sStable[OldLng] then
    begin
      cmbXVMversion.Items[0] := sStable[currentLanguage];
      if not(cmbXVMversion.ItemIndex > 0) then cmbXVMversion.ItemIndex := 0;
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
var
  LCode: Cardinal;
begin
  LastNightlyRev := '';
  ProcessMode := pmInstallUpdate;
  Status := TProgressStatus.Create;

  lDonate.Font.Color := RGB(0, 51, 102); // Paypal original color

  // DYNAMIC VERSIONS & CONFIGS LOADING SUPPORT
  VersionsFiles := TStringList.Create;
  cmbXVMversion.ItemIndex := cmbXVMversion.Items.Add(sStable[currentLanguage]);

  // WOT INSTALLATION FOLDER AUTO-DETECTION
  DetectionThread := TDetectionThread.Create;

  {MessageBox(0, 'XVM Updater '+_VERSION_+' - TEST RELEASE'+#13#10+
                'DO NOT SHARE'+#13#10+
                'MAY BE UNSTABLE', 'XVM Updater', +mb_OK +mb_ICONWARNING);}

  // AUTO LANGUAGE SELECTION
  // http://msdn.microsoft.com/en-us/library/cc233965.aspx
  // http://msdn.microsoft.com/en-us/library/dd318135.aspx
  LCode := GetUserDefaultLCID and $00FF;
       if LCode = $0C then ChangeLanguage(lngFR)
  else if LCode = $07 then ChangeLanguage(lngDE)
  else if LCode = $15 then ChangeLanguage(lngPL)
  else if LCode = $19 then ChangeLanguage(lngRU)
  else if LCode = $22 then ChangeLanguage(lngUA)
  else if LCode = $0E then ChangeLanguage(lngHU)
  else if LCode = $0B then ChangeLanguage(lngFI)
  else if LCode = $13 then ChangeLanguage(lngNL)
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

  // Wait before starting up, we need to detect at least one WoT installation
  while not(DetectionThread.Finished or DetectionThread.InstallationFound) do Sleep(10);
end;

end.
