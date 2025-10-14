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
	RipGrepper.Settings.RipGrepperSettings;

const
	// Constants for OrderIndex values
	RG_FILTER_OPTION_HIDDEN_INDEX = 0;
	RG_FILTER_OPTION_NO_IGNORE_INDEX = 1;
	RG_FILTER_OPTION_ENCODING_INDEX = 2;

	RG_OUTPUT_OPTION_PRETTY_INDEX = 0;
	RG_OUTPUT_OPTION_CONTEXT_INDEX = 1;
	RG_OUTPUT_OPTION_OUTPUT_FORMAT_INDEX = 2;

type
	// Event type for option change
	TRgOptionChangeEvent = procedure(Sender : TObject; Item : TCustomCheckItem) of object;

	TOptionPanel = class(TCustomPanel)
		strict private
			FSettings : TRipGrepperSettings;
			procedure SetSettings(const Value : TRipGrepperSettings);

		private
			FCheckOptionsGroup : TCustomCheckOptions;

		protected

		public
			FEventsEnabled : Boolean;
			procedure AdjustHeight();
			property CheckOptionsGroup : TCustomCheckOptions read FCheckOptionsGroup;
			property EventsEnabled : Boolean read FEventsEnabled write FEventsEnabled;
			property Settings : TRipGrepperSettings read FSettings write SetSettings;
	end;

	TRgFilterOptionsPanel = class(TOptionPanel)
		pnlMain : TPanel;

		strict private
			FOnOptionChange : TRgOptionChangeEvent;

		private
			procedure onCheckOptionSelect(_sender : TObject; _item : TCustomCheckItem);

		public
			constructor Create(_owner : TComponent); override;
			procedure AddItems();
			property OnOptionChange : TRgOptionChangeEvent read FOnOptionChange write FOnOptionChange;
	end;

	TRgOutputOptionsPanel = class(TOptionPanel)
		pnlMain : TPanel;

		strict private
			FOnOptionChange : TRgOptionChangeEvent;

		private
			procedure onCheckOptionSelect(_sender : TObject; _item : TCustomCheckItem);

		public
			constructor Create(_owner : TComponent); override;
			procedure AddItems();
			// procedure LoadFromSettings();
			property OnOptionChange : TRgOptionChangeEvent read FOnOptionChange write FOnOptionChange;
	end;

implementation

uses
	Spring,
	RipGrepper.Tools.DebugUtils,
	RipGrepper.Common.Constants,
	System.StrUtils,
	System.Math,
	ArrayEx;

constructor TRgFilterOptionsPanel.Create(_owner : TComponent);
begin
	inherited Create(_owner);
	BevelOuter := bvNone;
	Align := alClient;
	FEventsEnabled := True; // Default to enabled

	// Create pnlMain programmatically
	pnlMain := TPanel.Create(Self);
	pnlMain.Parent := Self;
	pnlMain.Align := alClient;
	pnlMain.BevelOuter := bvNone;
	pnlMain.Caption := '';

	FCheckOptionsGroup := TCustomCheckOptions.Create(Self);
	FCheckOptionsGroup.Parent := pnlMain;
	FCheckOptionsGroup.Align := alClient;
	FCheckOptionsGroup.Columns := 3;
	FCheckOptionsGroup.OnItemSelect := onCheckOptionSelect;
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

	// Add checkbox options
	FCheckOptionsGroup.AddCheckboxItem('--hidden', 'Include hidden files in search', RG_FILTER_OPTION_HIDDEN_INDEX);
	FCheckOptionsGroup.AddCheckboxItem('--no-ignore', 'Don''t respect ignore files', RG_FILTER_OPTION_NO_IGNORE_INDEX);
	FCheckOptionsGroup.AddCheckboxComboItem('--encoding=', 'Specify text encoding', RG_FILTER_OPTION_ENCODING_INDEX, encodingItems);

end;

procedure TRgFilterOptionsPanel.onCheckOptionSelect(_sender : TObject; _item : TCustomCheckItem);
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TRgFilterOptionsPanel.onCheckOptionSelect');

	if not FEventsEnabled then begin
		dbgMsg.MsgFmt('Events for idx:%d disabled, exiting', [_item.OrderIndex]);
		Exit;
	end;

	// Handle the functionality that was previously in the individual event handlers
	case _item.OrderIndex of
		RG_FILTER_OPTION_HIDDEN_INDEX : begin
			Settings.SearchFormSettings.Hidden := _item.Checked;
			dbgMsg.Msg('Hidden option changed to: ' + BoolToStr(Settings.SearchFormSettings.Hidden));
		end;
		RG_FILTER_OPTION_NO_IGNORE_INDEX : begin
			Settings.SearchFormSettings.NoIgnore := _item.Checked;
			dbgMsg.Msg('NoIgnore option changed to: ' + BoolToStr(Settings.SearchFormSettings.NoIgnore));
		end;
		RG_FILTER_OPTION_ENCODING_INDEX : begin
			if Assigned(_item.ComboBox) then begin
				_item.ComboBox.Enabled := _item.Checked;
				Settings.SearchFormSettings.Encoding := IfThen(_item.ComboBox.Enabled, _item.ComboBox.Text);
				dbgMsg.Msg('Encoding option changed to: ' + Settings.SearchFormSettings.Encoding);
			end;
		end;
	end;

	// Fire change event
	if Assigned(FOnOptionChange) then begin
		FOnOptionChange(Self, _item);
	end;
end;

constructor TRgOutputOptionsPanel.Create(_owner : TComponent);
begin
	inherited Create(_owner);
	BevelOuter := bvNone;
	Align := alClient;
	FEventsEnabled := True; // Default to enabled

	// Create pnlMain programmatically
	pnlMain := TPanel.Create(Self);
	pnlMain.Parent := Self;
	pnlMain.Align := alClient;
	pnlMain.BevelOuter := bvNone;
	pnlMain.Caption := '';

	FCheckOptionsGroup := TCustomCheckOptions.Create(Self);
	FCheckOptionsGroup.Parent := pnlMain;
	FCheckOptionsGroup.Align := alClient;
	FCheckOptionsGroup.Columns := 3;
	FCheckOptionsGroup.OnItemSelect := onCheckOptionSelect;
end;

procedure TRgOutputOptionsPanel.AddItems();
begin
	// Add checkbox options
	FCheckOptionsGroup.AddCheckboxItem('--pretty', 'Parse pretty output', RG_OUTPUT_OPTION_PRETTY_INDEX);
	FCheckOptionsGroup.AddCheckboxSpinItem('--context=', 'Context line number', RG_OUTPUT_OPTION_CONTEXT_INDEX, 0, 20, 0);
	FCheckOptionsGroup.AddLabelComboItem('Output Format:', 'Output format', RG_OUTPUT_OPTION_OUTPUT_FORMAT_INDEX, ['json', 'vimgrep']);
end;

procedure TRgOutputOptionsPanel.onCheckOptionSelect(_sender : TObject; _item : TCustomCheckItem);
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TRgOutputOptionsPanel.onCheckOptionSelect');

	if not FEventsEnabled then begin
		dbgMsg.MsgFmt('Events for idx:%d disabled, exiting', [_item.OrderIndex]);
		Exit;
	end;

	// Handle the functionality that was previously in the individual event handlers
	case _item.OrderIndex of
		RG_OUTPUT_OPTION_PRETTY_INDEX : begin
			Settings.SearchFormSettings.Pretty := _item.Checked;
			dbgMsg.Msg('Pretty option changed to: ' + BoolToStr(Settings.SearchFormSettings.Pretty));
		end;
		RG_OUTPUT_OPTION_OUTPUT_FORMAT_INDEX : begin
			if Assigned(_item.ComboBox) then begin
				Settings.SearchFormSettings.OutputFormat := _item.ComboBox.Text;
				dbgMsg.Msg('Output format option changed to: ' + Settings.SearchFormSettings.OutputFormat);
			end;
		end;
		RG_OUTPUT_OPTION_CONTEXT_INDEX : begin
			if Assigned(_item.SpinEdit) then begin
				_item.SpinEdit.Enabled := _item.Checked;
				Settings.SearchFormSettings.Context := IfThen(_item.SpinEdit.Enabled, _item.SpinEdit.Value, 0);
				dbgMsg.Msg('Context option changed to: ' + Settings.SearchFormSettings.Context.ToString);
			end;
		end;
	end;

	// Fire change event
	if Assigned(FOnOptionChange) then begin
		FOnOptionChange(Self, _item);
	end;
end;


procedure TOptionPanel.AdjustHeight();
begin
	// Ensure width fits within parent with margins
	if Assigned(Parent) then begin
		Width := Parent.ClientWidth - 16; // Leave margin for proper appearance
	end;
	// Make sure the check options group uses full available width
	FCheckOptionsGroup.Width := Width - 8;
	// Height should match exactly the check options group height
	Height := FCheckOptionsGroup.Height;
end;

procedure TOptionPanel.SetSettings(const Value : TRipGrepperSettings);
begin
	FSettings := Value;
end;

end.
