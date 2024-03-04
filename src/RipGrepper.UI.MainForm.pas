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
	RipGrepper.Common.Types,
	RipGrepper.Common.Interfaces,
	Winapi.Windows,
	System.ImageList,
	System.Actions,
	System.Threading,
	Vcl.WinXCtrls,
	System.Diagnostics,
	RipGrepper.Common.Sorter,
	RipGrepper.Data.HistoryItemObject,
	RipGrepper.UI.DockableForm,
	u_dzDpiScaleUtils,
	RipGrepper.OpenWith.SimpleTypes,
	System.IniFiles,
	ToolsAPI,
	GX_BaseForm,
	RipGrepper.UI.MainFrame,
	RipGrepper.UI.BottomFrame,
	RipGrepper.UI.TopFrame;

type
	TRipGrepperForm = class(TfmBaseForm { TfmIdeDockForm } )
		var
			BottomFrame : TRipGrepperBottomFrame;
			TopFrame : TRipGrepperTopFrame;
			MainFrame : TRipGrepperMainFrame;
			procedure ActionCancelExecute(Sender : TObject);
			procedure FormCreate(Sender : TObject);
			procedure FormClose(Sender : TObject; var Action : TCloseAction);
			procedure FormShow(Sender : TObject);
			/// <summary>
			/// Returns the class of the frame that you want embedded in the dockable form
			/// </summary>
			function GetFrameClass : TCustomFrameClass; // override;

		private const
			RIPGREPPER_FORM = 'RipGrepperForm';

		var
			FSettings : TRipGrepperSettings;
			FImageScaler : TImageListScaler;
			procedure LoadSettings;
			function GetSettings : TRipGrepperSettings;
			procedure InitForm;
			property Settings : TRipGrepperSettings read GetSettings write FSettings;

		protected
			procedure ApplyDpi(_NewDpi : Integer; _NewBounds : PRect); override;
			procedure ArrangeControls; override;
			procedure CreateParams(var Params : TCreateParams); override;

		public
			constructor Create(_settings : TRipGrepperSettings); reintroduce; overload;
			constructor Create(AOwner : TComponent); overload; override;
			destructor Destroy; override;
			class function CreateAndShow(const _settings : TRipGrepperSettings) : TRipGrepperForm;
			procedure InitStatusBar;
			procedure Loaded; override;
			procedure SetStatusBarMessage(const _bWithElapsedTime : Boolean = False);
			procedure SetStatusBarStatistic(const _s : string);
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
	inherited Create(nil);
	InitForm;

	if Assigned(_settings) then begin
		FSettings := _settings;
	end;
	TDebugUtils.DebugMessage('TRipGrepperForm.Create: ' + FSettings.IniFile.FileName);
end;

constructor TRipGrepperForm.Create(AOwner : TComponent);
begin
	inherited Create(AOwner);

	InitForm;
	LoadSettings;
end;

procedure TRipGrepperForm.FormCreate(Sender : TObject);
begin
    //
end;

destructor TRipGrepperForm.Destroy;
begin
	TDebugUtils.DebugMessage('TRipGrepperForm.Destroy');
	// if not IsStandAlone then begin
	// TDebugUtils.DebugMessage('UnRegisterDockableForm - ' + RIPGREPPER_FORM);
	// IdeDockManager.UnRegisterDockableForm(self, RIPGREPPER_FORM);
	// end;

	// TRipGrepperSettingsInstance.FreeInstance;
	// Settings.Free;
	FImageScaler.Free;
	inherited;
end;

procedure TRipGrepperForm.ActionCancelExecute(Sender : TObject);
begin
	ModalResult := mrCancel;
	Close();
end;

procedure TRipGrepperForm.ApplyDpi(_NewDpi : Integer; _NewBounds : PRect);
var
	li : TImageList;
begin
	inherited;
	if not Assigned(FImageScaler) then
		FImageScaler := TImageListScaler.Create(Self, TopFrame.ImageListButtons);
	li := FImageScaler.GetScaledList(_NewDpi);
	TopFrame.Toolbar1.Images := li;
end;

procedure TRipGrepperForm.ArrangeControls;
begin
	inherited;
	// SetColumnWidths;
end;

class function TRipGrepperForm.CreateAndShow(const _settings : TRipGrepperSettings) : TRipGrepperForm;
begin
	TDebugUtils.DebugMessage('TRipGrepperForm.CreateAndShow: ' + _settings.IniFile.FileName);
	Result := TRipGrepperForm.Create(_settings);

	try
		Result.Show;
		// IdeDockManager.ShowForm(Result);
		// if (mrOk = form.ShowModal()) then begin
		// Result := form.ListViewResult.Items[form.ListViewResult.ItemIndex].SubItems[0];
		// end;
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
	SetStatusBarMessage();
	TDebugUtils.DebugMessage('TRipGrepperForm.FormShow - end');
end;

procedure TRipGrepperForm.InitStatusBar;
begin
	SetStatusBarMessage();
	SetStatusBarStatistic('Ready.');
end;

procedure TRipGrepperForm.LoadSettings;
begin
	Settings.Load;
	// LoadBeforeSearchSettings();
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

function TRipGrepperForm.GetFrameClass : TCustomFrameClass;
begin
	Result := nil; // TRipGrepperForm;
end;

function TRipGrepperForm.GetSettings : TRipGrepperSettings;
begin
	if not Assigned(FSettings) then begin
		FSettings := GSettings;
	end;
	Result := FSettings;
end;

procedure TRipGrepperForm.InitForm;
begin
	MainFrame.Init();

	InitDpiScaler;

	// if not IsStandAlone then begin
	// TDebugUtils.DebugMessage('RegisterDockableForm - ' + RIPGREPPER_FORM);
	// IdeDockManager.RegisterDockableForm(TRipGrepperForm, self, RIPGREPPER_FORM);
	// end;

	TDebugUtils.DebugMessage('TRipGrepperForm.InitForm Ended');
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

procedure TRipGrepperForm.SetStatusBarMessage(const _bWithElapsedTime : Boolean = False);
var
	msg : string;
begin
	if _bWithElapsedTime then begin
		msg := Format('Search took %s seconds with ' + EXE_AND_VERSION_FORMAT, [MainFrame.HistObject.ElapsedTimeText, MainFrame.ExeVersion]);
		BottomFrame.FStatusBarStatus := IfThen(MainFrame.HistObject.RipGrepResult = RIPGREP_ERROR, 'ERROR', 'SUCCES');
	end else begin
		msg := Format(EXE_AND_VERSION_FORMAT, [MainFrame.ExeVersion]);
		BottomFrame.FStatusBarStatus := 'READY';
	end;
	BottomFrame.FStatusBarMessage := msg;
end;

procedure TRipGrepperForm.SetStatusBarStatistic(const _s : string);
begin
	BottomFrame.FStatusBarStatistic := _s;
end;

end.
