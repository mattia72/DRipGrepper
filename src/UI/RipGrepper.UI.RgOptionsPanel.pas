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
	RG_OPTION_HIDDEN_INDEX = 0;
	RG_OPTION_NO_IGNORE_INDEX = 1;
	RG_OPTION_ENCODING_INDEX = 2;

type
	// Event type for option change
	TRgOptionChangeEvent = procedure(Sender : TObject; Item : TCustomCheckItem) of object;

	TRgOptionsPanel = class(TCustomPanel)
		pnlMain : TPanel;

		strict private
			FOnOptionChange : TRgOptionChangeEvent;
			FSettings : TRipGrepperSettings;
			procedure SetSettings(const Value : TRipGrepperSettings);

		private
			FCheckOptionsGroup : TCustomCheckOptions;
			procedure onCheckOptionSelect(_sender : TObject; _item : TCustomCheckItem);
			function getSelectedItems() : TArray<TCustomCheckItem>;

		public
			constructor Create(_owner : TComponent); override;
			procedure AddItems();
			procedure AdjustHeight();
			function GetHiddenOption : Boolean;
			function GetNoIgnoreOption : Boolean;
			function GetEncodingOption : Boolean;
			function GetEncodingValue : string;
			procedure SetHiddenOption(const _value : Boolean);
			procedure SetNoIgnoreOption(const _value : Boolean);
			procedure SetEncodingOption(const _value : Boolean);
			procedure SetEncodingValue(const _value : string);
			property CheckOptionsGroup : TCustomCheckOptions read FCheckOptionsGroup;
			property SelectedItems : TArray<TCustomCheckItem> read getSelectedItems;
			property Settings : TRipGrepperSettings read FSettings write SetSettings;
			property OnOptionChange : TRgOptionChangeEvent read FOnOptionChange write FOnOptionChange;
	end;

implementation

uses
	Spring,
	RipGrepper.Tools.DebugUtils,
	RipGrepper.Common.Constants,
	System.StrUtils;

constructor TRgOptionsPanel.Create(_owner : TComponent);
begin
	inherited Create(_owner);
	BevelOuter := bvNone;
	Align := alClient;

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

procedure TRgOptionsPanel.AddItems();
var
	encodingItems : IShared<TStringList>;
begin
	encodingItems := Shared.Make<TStringList>();

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
	FCheckOptionsGroup.AddItem('--hidden', 'Include hidden files in search', RG_OPTION_HIDDEN_INDEX);
	FCheckOptionsGroup.AddItem('--no-ignore', 'Don''t respect ignore files', RG_OPTION_NO_IGNORE_INDEX);
	FCheckOptionsGroup.AddItem('--encoding=', 'Specify text encoding', RG_OPTION_ENCODING_INDEX, encodingItems);

end;

procedure TRgOptionsPanel.AdjustHeight();
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

procedure TRgOptionsPanel.onCheckOptionSelect(_sender : TObject; _item : TCustomCheckItem);
begin

	// Handle the functionality that was previously in the individual event handlers
	case _item.OrderIndex of
		RG_OPTION_HIDDEN_INDEX : begin
			FSettings.SearchFormSettings.Hidden := _item.Checked;
		end;
		RG_OPTION_NO_IGNORE_INDEX : begin
			FSettings.SearchFormSettings.NoIgnore := _item.Checked;
		end;
		RG_OPTION_ENCODING_INDEX : begin
			if Assigned(_item.ComboBox) then begin
				_item.ComboBox.Enabled := _item.Checked;
				FSettings.SearchFormSettings.Encoding := IfThen(_item.ComboBox.Enabled, _item.ComboBox.Text);
			end;
		end;
	end;

	// Fire change event
	if Assigned(FOnOptionChange) then begin
		FOnOptionChange(Self, _item);
	end;
end;

function TRgOptionsPanel.getSelectedItems() : TArray<TCustomCheckItem>;
begin
	Result := FCheckOptionsGroup.SelectedItems;
end;

function TRgOptionsPanel.GetHiddenOption : Boolean;
begin
	Result := False;
	if FCheckOptionsGroup.Items.Count > 0 then begin
		Result := FCheckOptionsGroup.Items[0].Checked;
	end;
end;

function TRgOptionsPanel.GetNoIgnoreOption : Boolean;
begin
	Result := False;
	if FCheckOptionsGroup.Items.Count > RG_OPTION_NO_IGNORE_INDEX then begin
		Result := FCheckOptionsGroup.Items[RG_OPTION_NO_IGNORE_INDEX].Checked;
	end;
end;

function TRgOptionsPanel.GetEncodingOption : Boolean;
begin
	Result := False;
	if FCheckOptionsGroup.Items.Count > RG_OPTION_ENCODING_INDEX then begin
		Result := FCheckOptionsGroup.Items[RG_OPTION_ENCODING_INDEX].Checked;
	end;
end;

function TRgOptionsPanel.GetEncodingValue : string;
begin
	Result := '';
	if (FCheckOptionsGroup.Items.Count > RG_OPTION_ENCODING_INDEX) and Assigned(FCheckOptionsGroup.Items[RG_OPTION_ENCODING_INDEX].ComboBox)
	then begin
		Result := FCheckOptionsGroup.Items[RG_OPTION_ENCODING_INDEX].ComboBox.Text;
	end;
end;

procedure TRgOptionsPanel.SetHiddenOption(const _value : Boolean);
begin
	if FCheckOptionsGroup.Items.Count > RG_OPTION_HIDDEN_INDEX then begin
		FCheckOptionsGroup.Items[RG_OPTION_HIDDEN_INDEX].Checked := _value;
	end;
end;

procedure TRgOptionsPanel.SetNoIgnoreOption(const _value : Boolean);
begin
	if FCheckOptionsGroup.Items.Count > RG_OPTION_NO_IGNORE_INDEX then begin
		FCheckOptionsGroup.Items[RG_OPTION_NO_IGNORE_INDEX].Checked := _value;
	end;
end;

procedure TRgOptionsPanel.SetEncodingOption(const _value : Boolean);
begin
	if FCheckOptionsGroup.Items.Count > RG_OPTION_ENCODING_INDEX then begin
		FCheckOptionsGroup.Items[RG_OPTION_ENCODING_INDEX].Checked := _value;
	end;
end;

procedure TRgOptionsPanel.SetEncodingValue(const _value : string);
begin
	if (FCheckOptionsGroup.Items.Count > RG_OPTION_ENCODING_INDEX) and Assigned(FCheckOptionsGroup.Items[RG_OPTION_ENCODING_INDEX].ComboBox)
	then begin
		FCheckOptionsGroup.Items[RG_OPTION_ENCODING_INDEX].ComboBox.Text := _value;
	end;
end;

procedure TRgOptionsPanel.SetSettings(const Value : TRipGrepperSettings);
begin
	FSettings := Value;
end;

end.
