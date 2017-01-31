unit AsyncDownloader;

interface

uses
  Classes, SysUtils, Windows, UrlMon, ActiveX, DateUtils, WoT_Utils, IOUtils,
  Forms, ComCtrls;

type
  TAsyncDownloadCallback = procedure(Data: String) of Object;

  TAsyncDownloader = class(TThread, IBindStatusCallback)
    protected
      FRefCount: Integer;
      FURL: WideString;
      FDestination: WideString;
      FCallback: TAsyncDownloadCallback;
      // IUnknown
      function QueryInterface(const IID: TGUID; out Obj): Integer; stdcall;
      function _AddRef: Integer; stdcall;
      function _Release: Integer; stdcall;
    public
      constructor Create(URL: WideString; StartImmediately: Boolean); overload;
      constructor Create(URL: WideString); overload;
      constructor Create(URL: WideString; Callback: TAsyncDownloadCallback); overload;
      procedure Download(Destination: WideString); overload;
      function GetString():String;
      procedure Execute; override;
      // IBindStatusCallback
      function OnStartBinding(dwReserved: DWORD; pib: IBinding): HResult; stdcall;
      function GetPriority(out nPriority): HResult; stdcall;
      function OnLowResource(reserved: DWORD): HResult; stdcall;
      function OnProgress(ulProgress, ulProgressMax, ulStatusCode: ULONG;
        szStatusText: LPCWSTR): HResult; stdcall;
      function OnStopBinding(hresult: HResult; szError: LPCWSTR): HResult; stdcall;
      function GetBindInfo(out grfBINDF: DWORD; var bindinfo: TBindInfo): HResult; stdcall;
      function OnDataAvailable(grfBSCF: DWORD; dwSize: DWORD; formatetc: PFormatEtc;
        stgmed: PStgMedium): HResult; stdcall;
      function OnObjectAvailable(const iid: TGUID; punk: IUnknown): HResult; stdcall;
    end;

implementation

uses
  Languages, Main;

constructor TAsyncDownloader.Create(URL: WideString; StartImmediately: Boolean);
begin
  FreeOnTerminate := StartImmediately;
  FURL := URL;
  FDestination := GetTempFolder+'XVM_Updater_'+IntToStr(MillisecondOfTheDay(Now))+'.tmp';
  FCallback := nil;

  inherited Create(not StartImmediately);
end;

constructor TAsyncDownloader.Create(URL: WideString);
begin
  Create(URL, False);
end;

constructor TAsyncDownloader.Create(URL: WideString; Callback: TAsyncDownloadCallback);
begin
  Create(URL, True);
  FCallback := Callback;
end;

procedure TAsyncDownloader.Download(Destination: WideString);
begin
  FDestination := Destination;
  Start;
end;

function TAsyncDownloader.GetString:String;
begin
  if FileExists(FDestination) then
    Result := IOUtils.TFile.ReadAllText(FDestination)
  else
    Result := '';
end;

procedure TAsyncDownloader.Execute;
var
  Result: HResult;
begin
  Result := URLDownloadToFile(nil, PWideChar(FURL), PWideChar(FDestination), 0, Self);

  if Assigned(FCallback) then
    FCallback(GetString)
  else if (Result <> S_OK) and (Result <> E_ABORT) then
    begin
      Synchronize(procedure begin
        with fWindow do
          begin
            pbCurrentAction.Max := 100;
            pbCurrentAction.Position := 100;
            pbCurrentAction.State := pbsError;

            bChangeDirectory.Enabled := true;
            cbKeepConfig.Enabled := true;
            if (cmbConfig.Items.Count > 1) and (not cbKeepConfig.Checked) then
              cmbConfig.Enabled := true;
            if cmbXVMversion.Items.Count > 1 then
              cmbXVMversion.Enabled := true;
          end;
        MessageBox(0, PChar(sFailDownload[fWindow.CurrentLanguage]+#13#10+'URL Moniker 0x'+IntToHex(Result, 8)), 'XVM Updater', +mb_OK +mb_ICONWARNING);
      end);
    end;

  Terminate;
end;

function TAsyncDownloader.QueryInterface(const IID: TGUID;
  out Obj): Integer;
begin
  if GetInterface(IID, Obj) then Result := S_OK
                            else Result := E_NOINTERFACE;
end;

function TAsyncDownloader._AddRef: Integer;
begin
  Inc(FRefCount);
  Result := FRefCount;
end;

function TAsyncDownloader._Release: Integer;
begin
  Dec(FRefCount);
  Result := FRefCount;
end;

function TAsyncDownloader.GetBindInfo(out grfBINDF: DWORD;
  var bindinfo: TBindInfo): HResult;
begin
  Result := E_NOTIMPL;
end;

function TAsyncDownloader.GetPriority(out nPriority): HResult;
begin
  Result := E_NOTIMPL;
end;

function TAsyncDownloader.OnDataAvailable(grfBSCF, dwSize: DWORD;
  formatetc: PFormatEtc; stgmed: PStgMedium): HResult;
begin
  Result := E_NOTIMPL;
end;

function TAsyncDownloader.OnLowResource(reserved: DWORD): HResult;
begin
  Result := E_NOTIMPL;
end;

function TAsyncDownloader.OnObjectAvailable(const iid: TGUID;
  punk: IUnknown): HResult;
begin
  Result := E_NOTIMPL;
end;

function TAsyncDownloader.OnStartBinding(dwReserved: DWORD;
  pib: IBinding): HResult;
begin
  Result := E_NOTIMPL;
end;

function TAsyncDownloader.OnStopBinding(hresult: HResult;
  szError: LPCWSTR): HResult;
begin
  Result := E_NOTIMPL;
end;

function TAsyncDownloader.OnProgress(ulProgress, ulProgressMax,
  ulStatusCode: ULONG; szStatusText: LPCWSTR): HResult;
begin
  if not Assigned(FCallback) then
    Synchronize(procedure begin
      fWindow.pbCurrentAction.Max := ulProgressMax;
      fWindow.pbCurrentAction.Position := ulProgress;
    end);

  if Application.Terminated then
    Result := E_ABORT
  else
    Result := S_OK;
end;

end.
