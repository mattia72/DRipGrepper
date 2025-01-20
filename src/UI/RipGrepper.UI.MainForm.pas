unit RipGrepper.UI.MainForm;

interface

uses

	System.Variants,
	System.Classes,
	Vcl.Graphics,
	Vcl.Controls,
	Vcl.Forms,
	Vcl.StdCtrls,
	Vcl.ComCtrls,
	Vcl.ExtCtrls,
	Vcl.ImgList,
	Vcl.ActnList,
	Vcl.ToolWin,
	Vcl.Menus,
	RipGrepper.Tools.ProcessUtils,
	RipGrepper.Settings.RipGrepperSettings,
	RipGrepper.Data.Matches,
	RipGrepper.Common.Constants,
	RipGrepper.Common.Interfaces,
	Winapi.Windows,
	System.ImageList,
	System.Actions,
	System.Threading,
	Vcl.WinXCtrls,
	System.Diagnostics,
	RipGrepper.Common.Sorter,
	RipGrepper.Data.HistoryItemObject,
	u_dzDpiScaleUtils,
	RipGrepper.OpenWith.Constants,
	System.IniFiles,
	RipGrepper.UI.TopFrame,
	RipGrepper.UI.DpiScaler,
	RipGrepper.UI.ParentFrame,
	Winapi.Messages,
	RipGrepper.Helper.UI.DarkMode;

type
	TRipGrepperForm = class(TForm)
		var
			ParentFrame1 : TParentFrame;

			procedure ActionCancelExecute(Sender : TObject);
			procedure FormCreate(Sender : TObject);
			procedure FormClose(Sender : TObject; var Action : TCloseAction);
			procedure FormShow(Sender : TObject);

		private const
			RIPGREPPER_FORM = 'RipGrepperForm';

		var
			FSettings : TRipGrepperSettings;
			FThemeHandler : TThemeHandler;
			procedure WMSettingChange(var Message : TWMSettingChange); message WM_SETTINGCHANGE;

		protected
			procedure CreateParams(var Params : TCreateParams); override;

		public
			constructor Create(_settings : TRipGrepperSettings); reintroduce; overload;
			constructor Create(AOwner : TComponent); overload; override;
			destructor Destroy; override;
			class function CreateAndShow(const _settings : TRipGrepperSettings) : TRipGrepperForm;

			procedure Init;
			procedure Loaded; override;
	end;

var
	RipGrepperForm : TRipGrepperForm;

implementation

uses
	System.Math,
	System.UITypes,
	System.IOUtils,
	System.SysUtils,
	System.Types,
	RipGrepper.Tools.DebugUtils,
	RipGrepper.Helper.UI,
	RipGrepper.Tools.FileUtils,
	RipGrepper.Helper.Types,
	System.Generics.Defaults,
	Vcl.Dialogs,
	Vcl.Clipbrd,
	Winapi.ShellAPI,
	Winapi.CommCtrl,
	System.StrUtils,
	RipGrepper.UI.SearchForm,
	RipGrepper.Data.Parsers,
	RipGrepper.Parsers.VimGrepMatchLine,
	RipGrepper.Common.ParsedObject,
	RipGrepper.OpenWith,
	RipGrepper.OpenWith.ConfigForm,
	System.TypInfo;

{$R *.dfm}

constructor TRipGrepperForm.Create(_settings : TRipGrepperSettings);
begin
	TDebugUtils.DebugMessage('TRipGrepperForm.Create _settings');
	inherited Create(nil);

	if Assigned(_settings) then begin
		FSettings := _settings;
	end;

	TDebugUtils.DebugMessage('TRipGrepperForm.Create: ' + TPath.GetFileName(FSettings.IniFile.FileName));
end;

constructor TRipGrepperForm.Create(AOwner : TComponent);
begin
	TDebugUtils.DebugMessage('TRipGrepperForm.Create AOwner');
	inherited Create(AOwner);
	FThemeHandler := TThemeHandler.Create(self);
	{$IFDEF STANDALONE}
	FThemeHandler.HandleThemes(GSettings.AppSettings.ColorTheme);
	TDebugUtils.DebugMessage('TRipGrepperForm.Create AOwner STANDALONE');
	Init;
	{$ENDIF}
end;

procedure TRipGrepperForm.FormCreate(Sender : TObject);
begin
	TDebugUtils.DebugMessage('TRipGrepperForm.FormCreate');
	// it work only in ctor :/ FThemeHandler.HandleThemes(GSettings.AppSettings.ColorTheme);
end;

destructor TRipGrepperForm.Destroy;
begin
	TDebugUtils.DebugMessage('TRipGrepperForm.Destroy');
	FThemeHandler.Free;
	inherited;
end;

procedure TRipGrepperForm.ActionCancelExecute(Sender : TObject);
begin
	ModalResult := mrCancel;
	Close();
end;

class function TRipGrepperForm.CreateAndShow(const _settings : TRipGrepperSettings) : TRipGrepperForm;
begin
	TDebugUtils.DebugMessage('TRipGrepperForm.CreateAndShow: ' + TPath.GetFileName(_settings.IniFile.FileName));
	Result := TRipGrepperForm.Create(_settings);

	try
		Result.Show;
	finally
		TDebugUtils.DebugMessage('TRipGrepperForm.CreateAndShow: finally');
	end;
end;

procedure TRipGrepperForm.FormClose(Sender : TObject; var Action : TCloseAction);
begin
	TDebugUtils.DebugMessage('TRipGrepperForm.FormClose - begin action: ' + Integer(Action).ToString);
	// Settings.StoreToDict; not enough?
end;

procedure TRipGrepperForm.FormShow(Sender : TObject);
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TRipGrepperForm.FormShow');
	inherited;
end;

procedure TRipGrepperForm.CreateParams(var Params : TCreateParams);
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TRipGrepperForm.CreateParams');
	inherited CreateParams(Params);
	{$IFDEF STANDALONE}
	Params.ExStyle := Params.ExStyle or WS_EX_APPWINDOW; // force show in taskbar
	Params.WndParent := GetDesktopwindow;
	{$ENDIF}
end;

procedure TRipGrepperForm.Init;
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TRipGrepperForm.Init');
	ParentFrame.Init;
end;

procedure TRipGrepperForm.Loaded;
// var
// // PropInfo : PPropInfo;
// // i : Integer;
// // cmp : TComponent;
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TRipGrepperForm.Loaded');
	inherited Loaded;
	// PropInfo := GetPropInfo(Self, 'StyleElements');    why did i need this?
	// if Assigned(PropInfo) then
	// SetOrdProp(Self, PropInfo, 0);
	// for I := 0 to ComponentCount - 1 do begin
	// cmp := Components[I];
	// PropInfo := GetPropInfo(cmp, 'StyleElements');
	// if Assigned(PropInfo) then
	// SetOrdProp(cmp, PropInfo, 0);
	// //? SetOrdProp(cmp, PropInfo, Ord([seFont, seClient, seBorder]));
	// end;
end;

procedure TRipGrepperForm.WMSettingChange(var message : TWMSettingChange);
begin
	if SameText('ImmersiveColorSet', string(message.Section)) then begin
		FThemeHandler.HandleThemes(GSettings.AppSettings.ColorTheme);
	end;
end;

end.
