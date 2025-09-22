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
	RipGrepper.UI.CustomCheckOptions;

type
	// Event type for option change
	TRgOptionChangeEvent = procedure(Sender : TObject; Item : TCustomCheckItem) of object;

	TRgOptionsPanel = class(TCustomPanel)
		pnlMain : TPanel;

		strict private
			FOnOptionChange : TRgOptionChangeEvent;

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

		published
			property SelectedItems : TArray<TCustomCheckItem> read getSelectedItems;
			property OnOptionChange : TRgOptionChangeEvent read FOnOptionChange write FOnOptionChange;
	end;

implementation

uses
	RipGrepper.Tools.DebugUtils,
	RipGrepper.Common.Constants;

constructor TRgOptionsPanel.Create(_owner : TComponent);
begin
	inherited Create(_owner);
	
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
	encodingItems : TStringList;
begin
	encodingItems := TStringList.Create;
	try
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
		FCheckOptionsGroup.AddItem('--hidden', 'Include hidden files in search', 0);
		FCheckOptionsGroup.AddItem('--no-ignore', 'Don''t respect ignore files', 1);
		FCheckOptionsGroup.AddItem('--encoding=', 'Specify text encoding', 2, encodingItems);
	finally
		encodingItems.Free;
	end;
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
	if FCheckOptionsGroup.Items.Count > 1 then begin
		Result := FCheckOptionsGroup.Items[1].Checked;
	end;
end;

function TRgOptionsPanel.GetEncodingOption : Boolean;
begin
	Result := False;
	if FCheckOptionsGroup.Items.Count > 2 then begin
		Result := FCheckOptionsGroup.Items[2].Checked;
	end;
end;

function TRgOptionsPanel.GetEncodingValue : string;
begin
	Result := '';
	if (FCheckOptionsGroup.Items.Count > 2) and Assigned(FCheckOptionsGroup.Items[2].ComboBox) then begin
		Result := FCheckOptionsGroup.Items[2].ComboBox.Text;
	end;
end;

procedure TRgOptionsPanel.SetHiddenOption(const _value : Boolean);
begin
	if FCheckOptionsGroup.Items.Count > 0 then begin
		FCheckOptionsGroup.Items[0].Checked := _value;
	end;
end;

procedure TRgOptionsPanel.SetNoIgnoreOption(const _value : Boolean);
begin
	if FCheckOptionsGroup.Items.Count > 1 then begin
		FCheckOptionsGroup.Items[1].Checked := _value;
	end;
end;

procedure TRgOptionsPanel.SetEncodingOption(const _value : Boolean);
begin
	if FCheckOptionsGroup.Items.Count > 2 then begin
		FCheckOptionsGroup.Items[2].Checked := _value;
	end;
end;

procedure TRgOptionsPanel.SetEncodingValue(const _value : string);
begin
	if (FCheckOptionsGroup.Items.Count > 2) and Assigned(FCheckOptionsGroup.Items[2].ComboBox) then begin
		FCheckOptionsGroup.Items[2].ComboBox.Text := _value;
	end;
end;

end.