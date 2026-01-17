unit RipGrepper.UI.Components.OptionControls;

interface

uses
	System.Classes,
	System.SysUtils,
	System.Variants,
	Vcl.Controls,
	Vcl.Graphics,
	Vcl.StdCtrls,
	Vcl.ExtCtrls,
	Vcl.Samples.Spin,
	RipGrepper.UI.Components.NotifyingControls;

type
	// Interface for value provider - can be implemented by any class that provides a value
	IOptionValueProvider = interface
		['{A2F3C8D1-4E7B-4A9F-8C3D-6E5F9A1B2C4D}']
		function GetValue() : Variant;
		procedure SetValue(const _value : Variant);
		function GetTypeName() : string;
	end;

	// Base class for all option controls
	TOptionControlBase = class(TPanel)
		strict private
			FCaption : string;
			FValueProvider : IOptionValueProvider;
			FEnabled : Boolean;
			FDisabledHint : string;
			FShowInExpertMode : Boolean;
			FStartNewRow : Boolean;
			FOnValueChanged : TNotifyEvent;
			FControlPanel : TPanel;
			FHintHelper : TLabel;
			procedure setCaption(const _value : string);
			procedure setEnabled(const _value : Boolean); reintroduce;
			procedure showHintHelper();
			procedure hideHintHelper();
			function GetHintHelper() : TLabel;
			function GetControlPanel() : TPanel;

		protected
			procedure DoValueChanged; virtual;
			function GetValue() : Variant; virtual; abstract;
			procedure SetValue(const _value : Variant); virtual; abstract;
			procedure UpdateFromProvider; virtual;
			procedure UpdateToProvider; virtual;
			procedure UpdateCaption(); virtual;

		public
			constructor Create(_owner : TComponent); override;
			destructor Destroy; override;

			property ValueProvider : IOptionValueProvider read FValueProvider write FValueProvider;
			property Value : Variant read GetValue write SetValue;
			property ControlPanel : TPanel read GetControlPanel;
			property HintHelper : TLabel read GetHintHelper;

		published
			property Caption : string read FCaption write setCaption;
			property Enabled : Boolean read FEnabled write setEnabled;
			property DisabledHint : string read FDisabledHint write FDisabledHint;
			property ShowInExpertMode : Boolean read FShowInExpertMode write FShowInExpertMode default False;
			property StartNewRow : Boolean read FStartNewRow write FStartNewRow default False;
			property OnValueChanged : TNotifyEvent read FOnValueChanged write FOnValueChanged;
	end;

	// Simple checkbox option
	TOptionCheckBox = class(TOptionControlBase, INotifyingControlOwner)
		strict private
			FCheckBox : TNotifyingCheckBox;
			procedure onCheckBoxClick(_sender : TObject);

		protected
			function GetValue() : Variant; override;
			procedure SetValue(const _value : Variant); override;
			procedure UpdateCaption(); override;

		public
			constructor Create(_owner : TComponent); override;
			destructor Destroy; override;

			// INotifyingControlOwner implementation
			procedure OnSubItemEnabledChanged(_sender : TObject);

			property CheckBox : TNotifyingCheckBox read FCheckBox;

		published
			// Inherited published properties are automatically available
	end;

	// Checkbox with ComboBox
	TOptionCheckBoxCombo = class(TOptionControlBase, INotifyingControlOwner)
		strict private
			FCheckBox : TNotifyingCheckBox;
			FComboBox : TNotifyingComboBox;
			FComboItems : TStringList;
			procedure onCheckBoxClick(_sender : TObject);
			procedure onComboBoxChange(_sender : TObject);
			procedure setComboItems(const _value : TStringList);

		protected
			function GetValue() : Variant; override;
			procedure SetValue(const _value : Variant); override;

		public
			constructor Create(_owner : TComponent); override;
			destructor Destroy; override;

			// INotifyingControlOwner implementation
			procedure OnSubItemEnabledChanged(_sender : TObject);

			property CheckBox : TNotifyingCheckBox read FCheckBox;
			property ComboBox : TNotifyingComboBox read FComboBox;

		published
			property ComboItems : TStringList read FComboItems write setComboItems;
	end;

	// Checkbox with SpinEdit
	TOptionCheckBoxSpin = class(TOptionControlBase, INotifyingControlOwner)
		strict private
			FCheckBox : TNotifyingCheckBox;
			FSpinEdit : TNotifyingSpinEdit;
			FMinValue : Integer;
			FMaxValue : Integer;
			procedure onCheckBoxClick(_sender : TObject);
			procedure onSpinEditChange(_sender : TObject);
			procedure setMinValue(const _value : Integer);
			procedure setMaxValue(const _value : Integer);

		protected
			function GetValue() : Variant; override;
			procedure SetValue(const _value : Variant); override;

		public
			constructor Create(_owner : TComponent); override;
			destructor Destroy; override;

			// INotifyingControlOwner implementation
			procedure OnSubItemEnabledChanged(_sender : TObject);

			property CheckBox : TNotifyingCheckBox read FCheckBox;
			property SpinEdit : TNotifyingSpinEdit read FSpinEdit;

		published
			property MinValue : Integer read FMinValue write setMinValue default 0;
			property MaxValue : Integer read FMaxValue write setMaxValue default 100;
	end;

	// Label with ComboBox
	TOptionLabelCombo = class(TOptionControlBase, INotifyingControlOwner)
		strict private
			FLabel : TLabel;
			FComboBox : TNotifyingComboBox;
			FComboItems : TStringList;
			procedure onComboBoxChange(_sender : TObject);
			procedure setComboItems(const _value : TStringList);

		protected
			function GetValue() : Variant; override;
			procedure SetValue(const _value : Variant); override;

		public
			constructor Create(_owner : TComponent); override;
			destructor Destroy; override;

			// INotifyingControlOwner implementation
			procedure OnSubItemEnabledChanged(_sender : TObject);

			property LabelControl : TLabel read FLabel;
			property ComboBox : TNotifyingComboBox read FComboBox;

		published
			property ComboItems : TStringList read FComboItems write setComboItems;
	end;

procedure Register;

implementation

uses
	RipGrepper.UI.Components.Constants,
	RipGrepper.UI.Components.LabeledComboBox;

{ TOptionControlBase }

constructor TOptionControlBase.Create(_owner : TComponent);
begin
	inherited Create(_owner);
	BevelOuter := bvNone;
	FEnabled := True;
	FShowInExpertMode := False;
	FStartNewRow := False;
	// Visible := True;

	// Create control panel
	FControlPanel := TPanel.Create(Self);
	FControlPanel.Parent := Self;
	FControlPanel.BevelOuter := bvNone;
	FControlPanel.Left := 0;
	FControlPanel.Top := 0;
	FControlPanel.Width := Self.Width;
	FControlPanel.Height := Self.Height;
	FControlPanel.Align := alClient;
	// FControlPanel.Visible := Visible;

	// HintHelper will be created on demand
	FHintHelper := nil;
end;

destructor TOptionControlBase.Destroy;
begin
	if Assigned(FHintHelper) then begin
		FHintHelper.Free;
		FHintHelper := nil;
	end;
	FControlPanel.Free;
	FValueProvider := nil;
	inherited;
end;

function TOptionControlBase.GetControlPanel() : TPanel;
begin
	Result := FControlPanel;
end;

function TOptionControlBase.GetHintHelper() : TLabel;
begin
	if not Assigned(FHintHelper) then begin
		FHintHelper := TLabel.Create(Self);
		FHintHelper.Parent := Self;
		FHintHelper.AutoSize := False;
		FHintHelper.Alignment := taCenter;
		FHintHelper.Layout := tlCenter;
		FHintHelper.Visible := False;
		FHintHelper.Font.Color := clRed;
		FHintHelper.Font.Style := [fsBold];
		FHintHelper.Cursor := crHelp;
		FHintHelper.ShowHint := True;
		FHintHelper.Align := alClient;
		FHintHelper.BringToFront;
	end;
	Result := FHintHelper;
end;

procedure TOptionControlBase.showHintHelper();
begin
	if FDisabledHint <> '' then begin
		GetHintHelper.Caption := '⚠';
		FHintHelper.Hint := FDisabledHint;
		FHintHelper.Visible := True;
		FControlPanel.Enabled := False;
	end;
end;

procedure TOptionControlBase.hideHintHelper();
begin
	if Assigned(FHintHelper) then begin
		FHintHelper.Visible := False;
		FControlPanel.Enabled := True;
	end;
end;

procedure TOptionControlBase.DoValueChanged;
begin
	if Assigned(FOnValueChanged) then begin
		FOnValueChanged(Self);
	end;
	UpdateToProvider;
end;

procedure TOptionControlBase.setCaption(const _value : string);
begin
	if FCaption <> _value then begin
		FCaption := _value;
		UpdateCaption();
		// Invalidate();
	end;
end;

procedure TOptionControlBase.UpdateCaption();
begin
	// Override in descendants to update internal controls
end;

procedure TOptionControlBase.setEnabled(const _value : Boolean);
begin
	if FEnabled <> _value then begin
		FEnabled := _value;
		if _value then begin
			hideHintHelper();
		end else begin
			showHintHelper();
		end;
	end;
end;

procedure TOptionControlBase.UpdateFromProvider;
begin
	if Assigned(FValueProvider) then begin
		SetValue(FValueProvider.GetValue());
	end;
end;

procedure TOptionControlBase.UpdateToProvider;
begin
	if Assigned(FValueProvider) then begin
		FValueProvider.SetValue(GetValue());
	end;
end;

{ TOptionCheckBox }

constructor TOptionCheckBox.Create(_owner : TComponent);
begin
	inherited Create(_owner);

	FCheckBox := TNotifyingCheckBox.Create(Self);
	FCheckBox.Parent := ControlPanel;
	FCheckBox.OwnerItem := Self;
	FCheckBox.OnClick := onCheckBoxClick;
	FCheckBox.Left := 0;
	FCheckBox.Top := 0;
	FCheckBox.Width := 200;
	FCheckBox.Height := 21;
	// FCheckBox.AutoSize := True;
	// FCheckBox.Visible := True;

	UpdateCaption();
end;

destructor TOptionCheckBox.Destroy;
begin
	FCheckBox.Free;
	inherited;
end;

function TOptionCheckBox.GetValue() : Variant;
begin
	Result := FCheckBox.Checked;
end;

procedure TOptionCheckBox.SetValue(const _value : Variant);
begin
	FCheckBox.Checked := _value;
end;

procedure TOptionCheckBox.onCheckBoxClick(_sender : TObject);
begin
	DoValueChanged;
end;

procedure TOptionCheckBox.OnSubItemEnabledChanged(_sender : TObject);
begin
	// Handle enabled state changes if needed
end;

procedure TOptionCheckBox.UpdateCaption();
begin
	inherited;
	if Assigned(FCheckBox) then begin
		FCheckBox.Caption := Caption;
	end;
end;
{ TOptionCheckBoxCombo }

constructor TOptionCheckBoxCombo.Create(_owner : TComponent);
begin
	inherited Create(_owner);

	FCheckBox := TNotifyingCheckBox.Create(Self);
	FCheckBox.Parent := ControlPanel;
	FCheckBox.OwnerItem := Self;
	FCheckBox.OnClick := onCheckBoxClick;
	FCheckBox.Left := 0;
	FCheckBox.Top := 0;
	FCheckBox.Width := 150;
	FCheckBox.Height := 21;

	FComboBox := TNotifyingComboBox.Create(Self);
	FComboBox.Parent := ControlPanel;
	FComboBox.OwnerItem := Self;
	FComboBox.OnChange := onComboBoxChange;
	FComboBox.Left := 160;
	FComboBox.Top := 0;
	FComboBox.Width := 100;
	FComboBox.Height := 21;

	FComboItems := TStringList.Create;
end;

destructor TOptionCheckBoxCombo.Destroy;
begin
	FComboItems.Free;
	FComboBox.Free;
	FCheckBox.Free;
	inherited;
end;

function TOptionCheckBoxCombo.GetValue() : Variant;
begin
	if FCheckBox.Checked then begin
		Result := FComboBox.Text;
	end else begin
		Result := '';
	end;
end;

procedure TOptionCheckBoxCombo.SetValue(const _value : Variant);
var
	parts : TArray<string>;
begin
	if VarIsNull(_value) or VarIsEmpty(_value) then begin
		FCheckBox.Checked := False;
		FComboBox.Text := '';
	end else begin
		parts := string(_value).Split(['|']);
		FCheckBox.Checked := (Length(parts) > 0) and (parts[0] = 'True');
		if Length(parts) > 1 then begin
			FComboBox.Text := parts[1];
		end;
	end;
end;

procedure TOptionCheckBoxCombo.onCheckBoxClick(_sender : TObject);
begin
	FComboBox.Enabled := FCheckBox.Checked;
	DoValueChanged;
end;

procedure TOptionCheckBoxCombo.onComboBoxChange(_sender : TObject);
begin
	DoValueChanged;
end;

procedure TOptionCheckBoxCombo.setComboItems(const _value : TStringList);
begin
	FComboItems.Assign(_value);
	FComboBox.Items.Assign(_value);
end;

procedure TOptionCheckBoxCombo.OnSubItemEnabledChanged(_sender : TObject);
begin
	// Handle enabled state changes if needed
end;

{ TOptionCheckBoxSpin }

constructor TOptionCheckBoxSpin.Create(_owner : TComponent);
begin
	inherited Create(_owner);

	FCheckBox := TNotifyingCheckBox.Create(Self);
	FCheckBox.Parent := ControlPanel;
	FCheckBox.OwnerItem := Self;
	FCheckBox.OnClick := onCheckBoxClick;
	FCheckBox.Left := 0;
	FCheckBox.Top := 0;
	FCheckBox.Width := 150;
	FCheckBox.Height := 21;

	FSpinEdit := TNotifyingSpinEdit.Create(Self);
	FSpinEdit.Parent := ControlPanel;
	FSpinEdit.OwnerItem := Self;
	FSpinEdit.OnChange := onSpinEditChange;
	FSpinEdit.Left := 160;
	FSpinEdit.Top := 0;
	FSpinEdit.Width := 60;
	FSpinEdit.Height := 21;

	FMinValue := 0;
	FMaxValue := 100;
	FSpinEdit.MinValue := FMinValue;
	FSpinEdit.MaxValue := FMaxValue;
end;

destructor TOptionCheckBoxSpin.Destroy;
begin
	FSpinEdit.Free;
	FCheckBox.Free;
	inherited;
end;

function TOptionCheckBoxSpin.GetValue() : Variant;
begin
	if FCheckBox.Checked then begin
		Result := FSpinEdit.Value;
	end else begin
		Result := 0;
	end;
end;

procedure TOptionCheckBoxSpin.SetValue(const _value : Variant);
var
	parts : TArray<string>;
begin
	if VarIsNull(_value) or VarIsEmpty(_value) then begin
		FCheckBox.Checked := False;
		FSpinEdit.Value := FMinValue;
	end else begin
		parts := string(_value).Split(['|']);
		FCheckBox.Checked := (Length(parts) > 0) and (parts[0] = 'True');
		if Length(parts) > 1 then begin
			FSpinEdit.Value := StrToIntDef(parts[1], FMinValue);
		end;
	end;
end;

procedure TOptionCheckBoxSpin.onCheckBoxClick(_sender : TObject);
begin
	FSpinEdit.Enabled := FCheckBox.Checked;
	DoValueChanged;
end;

procedure TOptionCheckBoxSpin.onSpinEditChange(_sender : TObject);
begin
	DoValueChanged;
end;

procedure TOptionCheckBoxSpin.setMinValue(const _value : Integer);
begin
	FMinValue := _value;
	FSpinEdit.MinValue := _value;
end;

procedure TOptionCheckBoxSpin.setMaxValue(const _value : Integer);
begin
	FMaxValue := _value;
	FSpinEdit.MaxValue := _value;
end;

procedure TOptionCheckBoxSpin.OnSubItemEnabledChanged(_sender : TObject);
begin
	// Handle enabled state changes if needed
end;

{ TOptionLabelCombo }

constructor TOptionLabelCombo.Create(_owner : TComponent);
begin
	inherited Create(_owner);

	FLabel := TLabel.Create(Self);
	FLabel.Parent := ControlPanel;
	FLabel.Left := 0;
	FLabel.Top := 3;
	FLabel.Width := 150;
	FLabel.Height := 21;

	FComboBox := TNotifyingComboBox.Create(Self);
	FComboBox.Parent := ControlPanel;
	FComboBox.OwnerItem := Self;
	FComboBox.OnChange := onComboBoxChange;
	FComboBox.Left := 160;
	FComboBox.Top := 0;
	FComboBox.Width := 100;
	FComboBox.Height := 21;

	FComboItems := TStringList.Create;
end;

destructor TOptionLabelCombo.Destroy;
begin
	FComboItems.Free;
	FComboBox.Free;
	FLabel.Free;
	inherited;
end;

function TOptionLabelCombo.GetValue() : Variant;
begin
	Result := FComboBox.Text;
end;

procedure TOptionLabelCombo.SetValue(const _value : Variant);
begin
	if not VarIsNull(_value) and not VarIsEmpty(_value) then begin
		FComboBox.Text := _value;
	end else begin
		FComboBox.Text := '';
	end;
end;

procedure TOptionLabelCombo.onComboBoxChange(_sender : TObject);
begin
	DoValueChanged;
end;

procedure TOptionLabelCombo.setComboItems(const _value : TStringList);
begin
	FComboItems.Assign(_value);
	FComboBox.Items.Assign(_value);
end;

procedure TOptionLabelCombo.OnSubItemEnabledChanged(_sender : TObject);
begin
	// Handle enabled state changes if needed
end;

procedure Register;
begin
	RegisterComponents(SECTION_NAME, [TLabeledComboBox,
  {} TOptionCheckBox,
  {} TOptionCheckBoxCombo,
  {} TOptionCheckBoxSpin,
  {} TOptionLabelCombo]);

end;

end.
