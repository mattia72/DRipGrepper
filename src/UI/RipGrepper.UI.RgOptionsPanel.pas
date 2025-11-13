unit RipGrepper.UI.RgOptionsPanel;

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
	Vcl.StdCtrls,
	Vcl.ExtCtrls,
	RipGrepper.UI.CustomCheckOptions,
	RipGrepper.Settings.RipGrepperSettings,
	RipGrepper.Common.Constants;

const
	PNL_MAIN_PADDING_BOTTOM = 4;
	PNL_MAIN_PADDING_LEFT = 2;
	OUTPUT_OPTIONS_COL_NUM = 3;
	FILTER_OPTIONS_COL_NUM = 3;

type
	// Event type for option change
	TRgOptionChangeEvent = procedure(Sender : TObject; Item : TCustomCheckItem) of object;

	TOptionPanel = class(TCustomPanel)
		strict private
			FOnOptionChange : TRgOptionChangeEvent;
			FSettings : TRipGrepperSettings;
			procedure SetSettings(const Value : TRipGrepperSettings);

		protected
			FCheckOptionsGroup : TCustomCheckOptions;
			procedure onCheckOptionSelect(_sender : TObject; _item : TCustomCheckItem); virtual;

		public
			pnlMain : TPanel;
			FEventsEnabled : Boolean;
			constructor Create(_owner : TComponent); override;
			procedure AddItems(); virtual;
			procedure AdjustHeight();
			procedure UpdateExpertMode(const _bExpert : Boolean);
			property CheckOptionsGroup : TCustomCheckOptions read FCheckOptionsGroup;
			property EventsEnabled : Boolean read FEventsEnabled write FEventsEnabled;
			property Settings : TRipGrepperSettings read FSettings write SetSettings;
			property OnOptionChange : TRgOptionChangeEvent read FOnOptionChange write FOnOptionChange;
	end;

	TRgFilterOptionsPanel = class(TOptionPanel)
		protected
			procedure onCheckOptionSelect(_sender : TObject; _item : TCustomCheckItem); override;

		public
			constructor Create(_owner : TComponent); override;
			procedure AddItems(); override;
	end;

	TAppOptionsPanel = class(TOptionPanel)
		public
			constructor Create(_owner : TComponent); override;
			procedure AddItems(); override;
	end;

	TRgOutputOptionsPanel = class(TOptionPanel)
		strict private
			FIsVsCodeRipGrep : Boolean;
			FVsCodeChecked : Boolean;
			function getIsVsCodeRipGrep() : Boolean;
			procedure updatePrettyItem();

		protected
			procedure onCheckOptionSelect(_sender : TObject; _item : TCustomCheckItem); override;

		public
			constructor Create(_owner : TComponent); override;
			procedure AddItems(); override;
			property IsVsCodeRipGrep : Boolean read getIsVsCodeRipGrep;
	end;

implementation

uses
	Spring,
	RipGrepper.Tools.DebugUtils,
	System.StrUtils,
	System.Math,
	ArrayEx,
	RipGrepper.Settings.SettingVariant,
	RipGrepper.Tools.FileUtils;

constructor TOptionPanel.Create(_owner : TComponent);
begin
	inherited Create(_owner);
	BevelOuter := bvNone;
	Align := alClient;
	FEventsEnabled := True;

	// Create pnlMain programmatically
	pnlMain := TPanel.Create(Self);
	pnlMain.Name := 'TOptionPanelMain';
	pnlMain.Parent := Self;
	pnlMain.Align := alClient;
	pnlMain.BevelOuter := bvNone;
	pnlMain.Caption := '';
	pnlMain.Padding.Left := PNL_MAIN_PADDING_LEFT;
	pnlMain.Padding.Right := PNL_MAIN_PADDING_LEFT;
	pnlMain.Padding.Bottom := PNL_MAIN_PADDING_BOTTOM;

	FCheckOptionsGroup := TCustomCheckOptions.Create(Self);
	FCheckOptionsGroup.Parent := pnlMain;
	FCheckOptionsGroup.Align := alClient;
	FCheckOptionsGroup.OnItemChange := onCheckOptionSelect;
end;

constructor TRgFilterOptionsPanel.Create(_owner : TComponent);
begin
	inherited Create(_owner);

	FCheckOptionsGroup.Columns := FILTER_OPTIONS_COL_NUM;
end;

procedure TRgFilterOptionsPanel.AddItems();
var
	encodingItems : TArrayEx<string>;
begin
	// Add encoding options
	encodingItems.Add('ascii');
	encodingItems.Add('big5');
	encodingItems.Add('euc-jp');
	encodingItems.Add('euc-kr');
	encodingItems.Add('gb18030');
	encodingItems.Add('gbk');
	encodingItems.Add('iso-8859-1');
	encodingItems.Add('iso-8859-2');
	encodingItems.Add('iso-8859-3');
	encodingItems.Add('iso-8859-4');
	encodingItems.Add('iso-8859-5');
	encodingItems.Add('iso-8859-6');
	encodingItems.Add('iso-8859-7');
	encodingItems.Add('iso-8859-8');
	encodingItems.Add('iso-8859-10');
	encodingItems.Add('iso-8859-13');
	encodingItems.Add('iso-8859-14');
	encodingItems.Add('iso-8859-15');
	encodingItems.Add('iso-8859-16');
	encodingItems.Add('koi8-r');
	encodingItems.Add('koi8-u');
	encodingItems.Add('shift_jis');
	encodingItems.Add('utf-8');
	encodingItems.Add('utf-16');
	encodingItems.Add('utf-16be');
	encodingItems.Add('utf-16le');
	encodingItems.Add('utf-32');
	encodingItems.Add('utf-32be');
	encodingItems.Add('utf-32le');
	encodingItems.Add('windows-1250');
	encodingItems.Add('windows-1251');
	encodingItems.Add('windows-1252');
	encodingItems.Add('windows-1253');
	encodingItems.Add('windows-1254');
	encodingItems.Add('windows-1255');
	encodingItems.Add('windows-1256');
	encodingItems.Add('windows-1257');
	encodingItems.Add('windows-1258');
	encodingItems.Add('windows-874');

	var
	sfs := Settings.SearchFormSettings;
	// Add checkbox options
	FCheckOptionsGroup.AddCheckboxItem(RG_FILTER_OPTION_HIDDEN_CAPTION, 'Search hidden files and directories',
		{ } sfs.Hidden);
	FCheckOptionsGroup.AddCheckboxItem(RG_FILTER_OPTION_NO_IGNORE_CAPTION, 'Don''t use ignore files',
		{ } sfs.NoIgnore);
	FCheckOptionsGroup.AddCheckboxComboItem(RG_FILTER_OPTION_ENCODING_CAPTION, 'Specify text encoding',
		{ } encodingItems,
		{ } sfs.Encoding);

	inherited;
end;

procedure TRgFilterOptionsPanel.onCheckOptionSelect(_sender : TObject; _item : TCustomCheckItem);
begin
	inherited;
end;

constructor TRgOutputOptionsPanel.Create(_owner : TComponent);
begin
	inherited Create(_owner);
	FVsCodeChecked := False;

	FCheckOptionsGroup.Columns := OUTPUT_OPTIONS_COL_NUM;
end;

procedure TRgOutputOptionsPanel.AddItems();
begin
	// Add checkbox options
	var
	sfs := Settings.SearchFormSettings;
	FCheckOptionsGroup.AddLabelComboItem(RG_OUTPUT_OPTION_OUTPUT_FORMAT_CAPTION,
		{ } 'Output format of rg.exe (json is recommended)',
		{ } OUTPUT_FORMATS,
		{ } sfs.OutputFormat,
		{ } False { start in new line } ,
		{ } True { show in expert mode only } );
	FCheckOptionsGroup.AddCheckboxItem(RG_OUTPUT_OPTION_PRETTY_CAPTION,
		{ } 'Parse pretty output',
		{ } sfs.Pretty,
		{ } False { start in new line } ,
		{ } True { show in expert mode only } );
	FCheckOptionsGroup.AddCheckboxSpinItem(RG_OUTPUT_OPTION_CONTEXT_CAPTION,
		{ } 'Number of context lines',
		{ } 0, 20, 0,
		{ } sfs.Context,
		{ } False { start in new line } );

	// Check if VSCode RipGrep and disable pretty option
	if IsVsCodeRipGrep then begin
		var
		prettyItem := CheckOptionsGroup.GetItemByCaption(RG_OUTPUT_OPTION_PRETTY_CAPTION);
		if Assigned(prettyItem) then begin
			prettyItem.DisabledHint := 'rg.exe in VSCode doesn''t support --pretty';
			prettyItem.Enabled := False;
		end;
	end;
	inherited;
end;

function TRgOutputOptionsPanel.getIsVsCodeRipGrep() : Boolean;
begin
	if not FVsCodeChecked then begin
		FIsVsCodeRipGrep := TFileUtils.IsVsCodeRipGrep(Settings.RipGrepParameters.RipGrepPath);
		FVsCodeChecked := True;
	end;
	Result := FIsVsCodeRipGrep;
end;

procedure TRgOutputOptionsPanel.onCheckOptionSelect(_sender : TObject; _item : TCustomCheckItem);
begin
	inherited;

	var
	dbgMsg := TDebugMsgBeginEnd.New('TRgOutputOptionsPanel.onCheckOptionSelect');
	if not FEventsEnabled then begin
		dbgMsg.MsgFmt('Events for %s disabled, exiting', [_item.Setting.Name]);
		Exit;
	end;

	// Handle the functionality that was previously in the individual event handlers
	if (_item.Setting.Name = 'OutputFormat') and (not IsVsCodeRipGrep) then begin
		updatePrettyItem;
	end;

	if _item.Setting.Name = 'Context' then begin
		(_item.Setting as IIntegerSetting).Value := IfThen(_item.SpinEdit.Enabled, _item.SpinEdit.Value, 0);
	end;

	// Fire change event
	if Assigned(OnOptionChange) then begin
		OnOptionChange(Self, _item);
	end;
end;

procedure TRgOutputOptionsPanel.updatePrettyItem();
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TRgOutputOptionsPanel.updatePrettyItem');

	var
	prettyItem := CheckOptionsGroup.GetItemByCaption(RG_OUTPUT_OPTION_PRETTY_CAPTION);
	if (OUTPUT_FORMAT_JSON = Settings.SearchFormSettings.OutputFormat.Value) then begin
		// If json is selected, disable pretty option (not compatible)
		if Assigned(prettyItem) and Assigned(prettyItem.CheckBox) then begin
			prettyItem.CheckBox.Checked := False;
			Settings.SearchFormSettings.Pretty.Value := False;
			prettyItem.DisabledHint := 'Pretty output is not compatible with JSON format';
			prettyItem.Enabled := False;
			dbgMsg.Msg('Pretty option disabled due to json output format selection');
		end;
	end else begin
		// Enable pretty option if not json
		if Assigned(prettyItem) then begin
			prettyItem.Enabled := True;
			dbgMsg.Msg('Pretty option enabled');
		end;
	end;
end;

procedure TOptionPanel.AddItems();
begin
	FCheckOptionsGroup.AlignControlItems;
end;

procedure TOptionPanel.AdjustHeight();
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TOptionPanel.AdjustHeight');

	// Ensure width fits within parent with margins
	if Assigned(Parent) then begin
		Width := Parent.ClientWidth - 16; // Leave margin for proper appearance
	end;
	// Make sure the check options group uses full available width
	FCheckOptionsGroup.Width := Width - 8;
	// Height should match the check options group height plus pnlMain padding
	Height := FCheckOptionsGroup.Height +
	{ } FCheckOptionsGroup.Margins.Top + FCheckOptionsGroup.Margins.Bottom +
	{ } pnlMain.Padding.Top + pnlMain.Padding.Bottom;
	dbgMsg.MsgFmt('Panel %s height: %d (CheckOptionsGroup: %d + padding: %d)', [name, Height, FCheckOptionsGroup.Height,
		pnlMain.Padding.Top + pnlMain.Padding.Bottom]);
end;

procedure TOptionPanel.onCheckOptionSelect(_sender : TObject; _item : TCustomCheckItem);
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TOptionPanel.onCheckOptionSelect');

	if not FEventsEnabled then begin
		dbgMsg.MsgFmt('Events for %s disabled, exiting', [_item.Setting.Name]);
		Exit;
	end;

	if Assigned(_item.CheckBox) then begin
		if Assigned(_item.ComboBox) then begin
			_item.SetControlEnabled(siSecond, _item.Checked);
		end;
		if Assigned(_item.SpinEdit) then begin
			_item.SetControlEnabled(siSecond, _item.Checked);
		end;
	end;

	// Handle the functionality that was previously in the individual event handlers
	case _item.Setting.SettingType of
		stNotSet :
		{ } raise Exception.Create('Setting invalid.');
		stString : begin
			var
			setting := (_item.Setting as IStringSetting);
			setting.Value := _item.ComboText;
			dbgMsg.MsgFmt('%s option changed to: %s', [setting.Name, setting.Value]);
		end;
		stInteger : begin
			var
			setting := (_item.Setting as IIntegerSetting);
			setting.Value := _item.SpinValue;
			dbgMsg.MsgFmt('%s option changed to: %s', [setting.Name, setting.Value.ToString]);
		end;
		stBool : begin
			var
			setting := (_item.Setting as IBoolSetting);
			setting.Value := _item.Checked;
			dbgMsg.MsgFmt('%s option changed to: %s', [setting.Name, BoolToStr(setting.Value, True)]);
		end;
		stStrArray :
		{ } raise Exception.Create('Array type not implemented');
	end;

	// Fire change event
	if Assigned(OnOptionChange) then begin
		OnOptionChange(Self, _item);
	end;
end;

procedure TOptionPanel.SetSettings(const Value : TRipGrepperSettings);
begin
	FSettings := Value;
	if Assigned(FCheckOptionsGroup) then begin
		FCheckOptionsGroup.Settings := FSettings;
	end;
end;

procedure TOptionPanel.UpdateExpertMode(const _bExpert : Boolean);
begin
	CheckOptionsGroup.ShowExpertItems(_bExpert);
	CheckOptionsGroup.AlignControlItems();
	if _bExpert then begin
		CheckOptionsGroup.SetDefaultValues();
	end;
end;

constructor TAppOptionsPanel.Create(_owner : TComponent);
begin
	inherited Create(_owner);
	FCheckOptionsGroup.UseFlowLayout := True;
end;

procedure TAppOptionsPanel.AddItems();
begin
	// Add checkbox options
	var
	sa := Settings.AppSettings;
	FCheckOptionsGroup.AddCheckboxItem('Expert Options*',
		{ } 'Show Expert Options',
		{ } sa.ExpertMode, True);

	inherited;
end;

end.
