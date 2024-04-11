unit RipGrepper.UI.MainForm;

interface

uses

	// Winapi.Messages,
	System.Variants,
	System.Classes,
	Vcl.Graphics,
	Vcl.Controls,
	Vcl.Forms,
	Vcl.StdCtrls,
	Vcl.ComCtrls,
	Vcl.ExtCtrls,
	// System.ImageList,
	Vcl.ImgList,
	// System.Actions,
	Vcl.ActnList,
	Vcl.ToolWin,
	Vcl.Menus,
	RipGrepper.Tools.ProcessUtils,
	RipGrepper.Common.Settings,
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
	RipGrepper.OpenWith.SimpleTypes,
	System.IniFiles,
	ToolsAPI,
	RipGrepper.UI.BottomFrame,
	RipGrepper.UI.TopFrame,
	RipGrepper.UI.DpiScaler,
	RipGrepper.UI.ParentFrame;

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
			function GetSettings : TRipGrepperSettings;
			property Settings : TRipGrepperSettings read GetSettings write FSettings;

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
	RipGrepper.Tools.DebugTools,
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
	RipGrepper.Helper.ListBox,
	u_dzVclUtils,
	RipGrepper.Parsers.VimGrepMatchLine,
	RipGrepper.Common.ParsedObject,
	RipGrepper.OpenWith,
	RipGrepper.OpenWith.ConfigForm,
	GX_OtaUtils,
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
	if IsStandAlone then begin
		TDebugUtils.DebugMessage('TRipGrepperForm.Create AOwner STANDALONE');
		Init;
	end;
end;

procedure TRipGrepperForm.FormCreate(Sender : TObject);
begin
	TDebugUtils.DebugMessage('TRipGrepperForm.FormCreate');
end;

destructor TRipGrepperForm.Destroy;
begin
	TDebugUtils.DebugMessage('TRipGrepperForm.Destroy');

	// FImageScaler.Free;
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
	TDebugUtils.DebugMessage('TRipGrepperForm.FormClose - begin action:' + Integer(Action).ToString);
	Settings.Store;
end;

procedure TRipGrepperForm.FormShow(Sender : TObject);
begin
	TDebugUtils.DebugMessage('TRipGrepperForm.FormShow - begin');
	inherited;
	// AllFrames.OnShow(Sender);
	TDebugUtils.DebugMessage('TRipGrepperForm.FormShow - end');
end;

procedure TRipGrepperForm.CreateParams(var Params : TCreateParams);
begin
	TDebugUtils.DebugMessage('TRipGrepperForm.CreateParams');
	inherited CreateParams(Params);

	if IsStandAlone then begin
		Params.ExStyle := Params.ExStyle or WS_EX_APPWINDOW;
		Params.WndParent := GetDesktopwindow;
	end;
end;

function TRipGrepperForm.GetSettings : TRipGrepperSettings;
begin
	if not Assigned(FSettings) then begin
		FSettings := GSettings;
	end;
	Result := FSettings;
end;

procedure TRipGrepperForm.Init;
begin
	TDebugUtils.DebugMessage('TRipGrepperForm.Init');
	// if not IsStandalone then begin
	TDebugUtils.DebugMessage('TRipGrepperDockableForm.FrameCreated FindImageListForDpiScaler');
	// end;

	ParentFrame.Init;
end;

procedure TRipGrepperForm.Loaded;
var
	PropInfo : PPropInfo;
	i : Integer;
	cmp : TComponent;
begin
	TDebugUtils.DebugMessage('TRipGrepperForm.Loaded');
	inherited Loaded;
	PropInfo := GetPropInfo(Self, 'StyleElements');
	if Assigned(PropInfo) then
		SetOrdProp(Self, PropInfo, 0);
	for I := 0 to ComponentCount - 1 do begin
		cmp := Components[I];
		PropInfo := GetPropInfo(cmp, 'StyleElements');
		if Assigned(PropInfo) then
			SetOrdProp(cmp, PropInfo, 0);
	end;
end;

end.
