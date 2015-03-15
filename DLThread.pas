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

unit DLThread;

interface

uses
  SysUtils, Classes, Windows, IdHTTP, IdComponent, ComCtrls;

type
  TDownloadCallback = procedure(Data: TMemoryStream) of Object;

  TDLThread = class(TThread)
    constructor Create(cURL: String; cShowErrors: Boolean); overload;
    constructor Create(cURL: String; cCallback: TDownloadCallback); overload;
  public
    Data: TMemoryStream;
    Failed: Boolean;
  private
    Callback: TDownloadCallback;
    ShowErrors: Boolean;
  protected
    URL: String;
    procedure Execute; override;
    procedure DWork(ASender: TObject; AWorkMode: TWorkMode; AWorkCount: Int64);
    procedure DWorkBegin(ASender: TObject; AWorkMode: TWorkMode; AWorkCountMax: Int64);
    procedure Redirect(Sender: TObject; var dest: String; var NumRedirect: Integer; var Handled: Boolean; var VMethod: string);
    procedure HandleException(Error: String);
  end;

implementation

uses Languages, Main;


constructor TDLThread.Create(cURL: String; cShowErrors: Boolean);
begin
  FreeOnTerminate := False;

  URL := cURL;
  ShowErrors := cShowErrors;
  Failed := False;
  Data := TMemoryStream.Create;

  inherited Create(true);
end;


constructor TDLThread.Create(cURL: String; cCallback: TDownloadCallback);
begin
  FreeOnTerminate := True; // Free after callback.

  URL := cURL;
  Callback := cCallback;
  ShowErrors := False;
  Failed := False;
  Data := TMemoryStream.Create;

  inherited Create(false);
end;


procedure TDLThread.DWork(ASender: TObject; AWorkMode: TWorkMode; AWorkCount: Int64);
begin
  if Terminated then Abort;

  Synchronize(procedure
    begin
      fWindow.pbCurrentAction.Position := AWorkCount;
    end);
end;


procedure TDLThread.DWorkBegin(ASender: TObject; AWorkMode: TWorkMode; AWorkCountMax: Int64);
begin
  Synchronize(procedure
    begin
      fWindow.pbCurrentAction.Max := AWorkCountMax;
      fWindow.pbCurrentAction.Position := 0;
    end);
end;


procedure TDLThread.Redirect(Sender: TObject; var dest: String; var NumRedirect: Integer; var Handled: Boolean; var VMethod: string);
begin
   Handled := True;
end;


procedure TDLThread.HandleException(Error: String);
begin
  Failed := True;
  if not(ShowErrors) or Terminated then Exit; // Fail silently.

  Synchronize(procedure
    begin
      MessageBox(0, PChar(sFailDownload[fWindow.CurrentLanguage]+#13#10+Error), 'XVM Updater', +mb_OK +mb_ICONWARNING);
      with fWindow do
        begin
          pbCurrentAction.Max := 100;
          pbCurrentAction.Position := 100;
          pbCurrentAction.State := pbsError;

          bChangeDirectory.Enabled := true;
          cbKeepConfig.Enabled := true;
          cbShowWinChances.Enabled := true;
          cbEnableStatsDisplay.Enabled := true;
          if (cmbConfig.Items.Count > 1) and (not cbKeepConfig.Checked) then cmbConfig.Enabled := true;
          if cmbXVMversion.Items.Count > 1 then cmbXVMversion.Enabled := true;
        end;
    end);
end;


procedure TDLThread.Execute;
begin
  with TIdHTTP.Create(nil) do
    begin
      try
        try
          if not Assigned(Callback) then
            begin
              OnWork := Self.DWork;
              OnWorkBegin := Self.DWorkBegin;
            end;
          OnRedirect := Self.Redirect;
          HandleRedirects := True;
          Get(Self.URL, Data);
        finally
          Free;
        end;
      except
        on E : Exception do HandleException(E.Message);
      end;
    end;
  if Assigned(Callback) then
    begin
      Callback(Data);
      Data.Free;
    end;
  Terminate;
end;

end.
