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

unit WoT_Utils;

// FUNCTIONS DESCRIBED BELOW ARE EITHER WRITTEN BY MYSELF OR TAKEN FROM PUBLIC DOMAIN.

interface

uses
  SysUtils, Windows, Classes, Masks, ShellAPI, ShlObj, ActiveX, Forms, Tlhelp32,
  ComOBJ;

type
  TStringArray = array of String;
  PString = ^String;

function GetVersion(FileName: String):String;
procedure GetSubDirectories(const directory: String; List: TStringList);
function GetDesktop:String;
function GetStartMenu:String;
function GetShortcutTarget(FileName: String):String;

procedure FCopy(source: String; destination: String);
function DeleteDirectory(Dir: String):Boolean;
procedure ExecuteAndWait(Filename, Parameters: String; Application: TApplication);
function ProcessExists(exeFileName: String):Boolean;
function IsDriveReady(Root: String):Boolean;

function StrSplit(Source: String):TStringArray;
procedure FileReplaceString(const FileName, SearchString, ReplaceString: String);


implementation


function GetVersion(FileName: String):String;
var
  VerInfoSize: DWORD;
  VerInfo: Pointer;
  VerValueSize: DWORD;
  VerValue: PVSFixedFileInfo;
  Dummy: DWORD;
begin
  VerInfoSize := GetFileVersionInfoSize(PChar(FileName), Dummy);
  if VerInfoSize > 0 then
    begin
      GetMem(VerInfo, VerInfoSize);
      GetFileVersionInfo(PChar(FileName), 0, VerInfoSize, VerInfo);
      VerQueryValue(VerInfo, '\', Pointer(VerValue), VerValueSize);

      with VerValue^ do
        begin
          Result := IntToStr(dwFileVersionMS shr 16);
          Result := Result + '.' + IntToStr(dwFileVersionMS and $FFFF);
          Result := Result + '.' + IntToStr(dwFileVersionLS shr 16);
        end;
      FreeMem(VerInfo, VerInfoSize);
    end
end;


procedure GetSubDirectories(const directory: String; List: TStringList);
var
  sr: TSearchRec;
begin
  try
    if FindFirst(IncludeTrailingPathDelimiter(directory)+'*.*', faDirectory, sr) < 0 then
      Exit
    else
      repeat
        if (sr.Attr and faDirectory <> 0) AND (sr.Name <> '.') AND
           (sr.Name <> '..') AND MatchesMask(sr.Name, '*.*.*') then
          List.Add(sr.Name);
      until FindNext(sr) <> 0;
  finally
    SysUtils.FindClose(sr);
  end;
end;


// By LaCourgette: split one string into two, separated by a space and optionnaly
// delimited by quotes to allow strings with spaces to be parsed.
function StrSplit(Source: String):TStringArray;
var
  Buffer: String;
  Position, ArrayInc: Integer;
begin
  Position := 1;
  ArrayInc := 0;
  Buffer := '';
  SetLength(Result, 2);

  try
    while Position < Length(Source) do
      begin
        if (Source[Position] = '"') then
          begin
            Inc(Position);
            repeat
              begin
                Buffer := Buffer + Source[Position];
                Inc(Position);
              end
            until ((Source[Position] = '"')  or (Position >= Length(Source)));
            Result[ArrayInc] := Trim(Buffer);
            Inc(ArrayInc);
            Inc(Position);
            Buffer := '';
          end
        else
          begin
            if Source[Position] = ' ' then Inc(Position) else
              begin
                repeat
                  begin
                    Buffer := Buffer + Source[Position];
                    Inc(Position);
                  end
                until ((Source[Position] = ' ') or (Position > Length(Source)));
                Result[ArrayInc] := Trim(Buffer);
                Inc(ArrayInc);
                Buffer := '';
              end;
          end;
      end;
  except
    // Avoid access violation error if incorrect syntax
  end;
end;


procedure FCopy(source: String; destination: String);
begin
  if FileExists(source) then
    CopyFile(PChar(source), PChar(destination), false);
  // Show errors.
end;


function GetDesktop:String;
begin
  SetLength(Result, MAX_PATH);
  SHGetFolderPath(0, CSIDL_DESKTOPDIRECTORY, 0, SHGFP_TYPE_CURRENT, PChar(Result));
  SetLength(Result, StrLen(PChar(Result)));
  // Can't fail.
end;


function GetShortcutTarget(FileName: String):String;
var
  ShellLink: IShellLink;
  ShortcutWC: array[0..MAX_PATH] of Char;
  pfd: TWin32FindData;
begin
  if (UpperCase(ExtractFileExt(FileName)) <> '.LNK') then
    FileName := FileName + '.lnk';
  ShellLink := CreateComObject(CLSID_ShellLink) as IShellLink;
  (ShellLink as IPersistFile).Load(StringToOleStr(FileName), STGM_READ);
  ShellLink.GetPath(ShortcutWC, Max_Path, pfd, SLGP_UNCPRIORITY);
  Result := ShortcutWC;
end;


function DeleteDirectory(Dir: String):Boolean;
var
  fos: TSHFileOpStruct;
begin
  ZeroMemory(@fos, SizeOf(fos));
  with fos do begin
    wFunc := FO_DELETE;
    fFlags := FOF_SILENT or FOF_NOCONFIRMATION;
    pFrom := PChar(Dir + #0);
  end;
  Result := (0 = ShFileOperation(fos));  // Fail silently anyway.
end;


procedure ExecuteAndWait(Filename, Parameters: String; Application: TApplication);
var
  SEInfo: TShellExecuteInfo;
  ExitCode: DWORD;
begin
  FillChar(SEInfo, SizeOf(SEInfo), 0);
  SEInfo.cbSize := SizeOf(TShellExecuteInfo);

  with SEInfo do begin
    fMask := SEE_MASK_NOCLOSEPROCESS;
    Wnd := Application.Handle;
    lpFile := PChar(Filename);
    lpParameters := PChar(Parameters);
    if (Win32MajorVersion > 5) then lpVerb := PChar('runas');
    nShow := SW_SHOWNORMAL;
  end;

  if ShellExecuteEx(@SEInfo) then
    begin
      repeat
        Sleep(10);
        Application.ProcessMessages;
        GetExitCodeProcess(SEInfo.hProcess, ExitCode);
      until (ExitCode <> STILL_ACTIVE) or Application.Terminated;
    end
end;


function ProcessExists(exeFileName: String):Boolean;
var
  ContinueLoop: BOOL;
  FSnapshotHandle: THandle;
  FProcessEntry32: TProcessEntry32;
begin
  try
    FSnapshotHandle := CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS, 0);
    FProcessEntry32.dwSize := SizeOf(FProcessEntry32);
    ContinueLoop := Process32First(FSnapshotHandle, FProcessEntry32);
    Result := False;
    while Integer(ContinueLoop) <> 0 do
    begin
      if ((UpperCase(ExtractFileName(FProcessEntry32.szExeFile)) =
        UpperCase(ExeFileName)) or (UpperCase(FProcessEntry32.szExeFile) =
        UpperCase(ExeFileName))) then
      begin
        Result := True;
      end;
      ContinueLoop := Process32Next(FSnapshotHandle, FProcessEntry32);
    end;
    CloseHandle(FSnapshotHandle);
  except
    Result := False; // Fail silently & allow processing.
  end;
end;


procedure FileReplaceString(const FileName, SearchString, ReplaceString: String);
var
  fs: TFileStream;
  S: UTF8String;
begin
  try
    fs := TFileStream.Create(FileName, fmOpenread);
    try
      SetLength(S, fs.Size);
      fs.ReadBuffer(S[1], fs.Size);
    finally
      fs.Free;
    end;
    S := UTF8Encode(StringReplace(UTF8Decode(S),
                    SearchString,
                    ReplaceString,
                    [rfReplaceAll, rfIgnoreCase]));
    fs := TFileStream.Create(FileName, fmCreate);
    try
      fs.WriteBuffer(S[1], Length(S));
    finally
      fs.Free;
    end;
  except
    // Fail & continue silently.
  end;
end;


function GetStartMenu:String;
  procedure FreePidl(pidl: PItemIDList);
  var
    allocator: IMalloc;
  begin
    if Succeeded(SHGetMalloc(allocator)) then allocator.Free(pidl);
  end;
var
  pidl: PItemIDList;
  buf: array[0..MAX_PATH] of Char;
begin
  if Succeeded(SHGetSpecialFolderLocation(0, CSIDL_STARTMENU, pidl)) then
  SHGetPathFromIDList(pidl, buf);
  Result := StrPas(buf);
  FreePIDL(pidl);
end;


function IsDriveReady(Root: String):Boolean;
var
  Oem: Cardinal;
  Dw1, Dw2: DWORD;
begin
  Oem := SetErrorMode(SEM_FAILCRITICALERRORS);
  if Char(Root[1]) in ['a'..'z'] then Dec(Char(Root[1]), $20);
  try
    if Length(Root) = 1 then Root := Root + ':\';
    Result := GetVolumeInformation(PChar(Root), nil, 0, nil, Dw1, Dw2, nil, 0);
    if DiskSize(Ord(Char(Root[1])) - $40) = -1 then Result := false;
  finally
    SetErrorMode(Oem);
  end;
end;

end.
