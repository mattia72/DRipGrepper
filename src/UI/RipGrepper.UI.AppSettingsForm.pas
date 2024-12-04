unit RipGrepper.UI.AppSettingsForm;

interface

uses
	Winapi.Windows,
	Winapi.Messages,
	System.SysUtils,
	System.Variants,
	System.Classes,
	Vcl.Graphics,
	Vcl.Controls,
	Vcl.Forms,
	Vcl.Dialogs,
	Vcl.ExtCtrls,
	Vcl.StdCtrls,
	RipGrepper.Settings.Persistable,
	RipGrepper.Settings.AppSettings,
	RipGrepper.UI.SettingsFormBase,
	RipGrepper.UI.ColorSelectorFrame,
	RipGrepper.Settings.FontColors,
	RipGrepper.Settings.RipGrepperSettings,
	RTTI;

type

	TAppSettingsForm = class(TSettingsBaseForm)
		Panel1 : TPanel;
		chDebugTrace : TCheckBox;
		chExpertMode : TCheckBox;
		grpFontColors : TGroupBox;
		grpDeveloper : TGroupBox;
		procedure FormShow(Sender : TObject);

		private
			FAppSettings : TAppSettings;
			FFontColorSettings : TColorSettings;
			function AddSelectionFrames : integer;
			procedure WriteColorSettings;

		protected
			procedure OnCancel; override;
			procedure OnOk; override;
			procedure ReadSettings; override;
			procedure WriteSettings; override;

		public
			constructor Create(_Owner : TComponent; _settings : TRipGrepperSettings);
	end;

var
	AppSettingsForm : TAppSettingsForm;

implementation

uses
	RipGrepper.Tools.DebugUtils,
	System.TypInfo,
	System.RegularExpressions,
	System.Math;

const
	COMPONENT_NAME_COLORSELECTOR = '_ColorSelector';

	{$R *.dfm}

constructor TAppSettingsForm.Create(_Owner : TComponent; _settings : TRipGrepperSettings);
begin
	inherited Create(_Owner, _settings);
	Caption := 'General';
	FFontColorSettings := (FSettings as TRipGrepperSettings).FontColorSettings;
	FAppSettings := (FSettings as TRipGrepperSettings).AppSettings;
end;

procedure TAppSettingsForm.FormShow(Sender : TObject);
begin
	ReadSettings;
	var
	allHeight := AddSelectionFrames;
	grpFontColors.Height := allHeight + grpFontColors.Margins.Bottom;
	self.Height := grpFontColors.Height + 4* grpFontColors.Margins.Bottom + grpDeveloper.Height;
end;

procedure TAppSettingsForm.OnCancel;
begin
	inherited;
end;

procedure TAppSettingsForm.OnOk;
begin
	inherited;
end;

procedure TAppSettingsForm.ReadSettings;
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TAppSettingsForm.ReadSettings');
	FAppSettings.LoadFromDict;
	FFontColorSettings.LoadFromDict;
	chDebugTrace.Checked := FAppSettings.DebugTrace;
	chExpertMode.Checked := FAppSettings.ExpertMode;
end;

procedure TAppSettingsForm.WriteSettings;
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TAppSettingsForm.WriteSettings');
	FAppSettings.DebugTrace := chDebugTrace.Checked;
	FAppSettings.ExpertMode := chExpertMode.Checked;
	WriteColorSettings;
	inherited WriteSettings;
end;

function TAppSettingsForm.AddSelectionFrames : integer;
var
	Context : TRttiContext;
	TypeFontColors : TRttiType;
	prop : TRttiField;
	NewFrame : TColorSelectorFrame;
	fa : TFontAttributes;
	colors : ^TFontColors;
	allHeight : integer;
begin
	allHeight := 0;
	Context := TRttiContext.Create;
	try
		TypeFontColors := Context.GetType(TypeInfo(TFontColors));
		for prop in TypeFontColors.GetFields do begin
			if prop.Visibility = mvPublic then begin
				colors := @FFontColorSettings.FontColors;
				fa := prop.GetValue(colors).AsType<TFontAttributes>;
				NewFrame := TColorSelectorFrame.Create(Self);
				NewFrame.Name := prop.Name + COMPONENT_NAME_COLORSELECTOR;
				InsertComponent(NewFrame); // !!!
				NewFrame.Parent := grpFontColors;
				NewFrame.Align := alTop;
				NewFrame.LabelText.Caption := TRegex.Replace(prop.Name, '[A-Z]', ' $0') + ':';
				NewFrame.AssignFontAttributes(fa);
				Inc(allHeight, NewFrame.Height +
					{ } IfThen(NewFrame.AlignWithMargins,
					{ } NewFrame.Margins.Top + NewFrame.Margins.Bottom, 2));
			end;
		end;
		Result := allHeight;
	finally
		Context.Free;
	end;
end;

procedure TAppSettingsForm.WriteColorSettings;
var
	compName : string;
	comp : TComponent;
	csf : TColorSelectorFrame;
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TAppSettingsForm.WriteColorSettings');
	var
	fc := FFontColorSettings.FontColors;
	for var i := 0 to ComponentCount - 1 do begin
		comp := Components[i];
		if not Assigned(comp) then begin
			dbgMsg.ErrorMsgFmt('%i Component not exists?', [comp]);
			continue;
		end;
		compName := comp.Name;
		if compName.EndsWith(COMPONENT_NAME_COLORSELECTOR) then begin
			csf := comp as TColorSelectorFrame;
			if Assigned(csf) then begin
				fc.SetByName(compName.Replace(COMPONENT_NAME_COLORSELECTOR, ''),
					{ } csf.SelectedFontAttributes);
			end else begin
				dbgMsg.ErrorMsgFmt('%s settings not saved.', [compName]);
			end;
		end;
	end;
	FFontColorSettings.FontColors := fc;
end;

end.
