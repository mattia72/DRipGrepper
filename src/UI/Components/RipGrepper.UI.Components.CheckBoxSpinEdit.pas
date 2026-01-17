unit RipGrepper.UI.Components.CheckBoxSpinEdit;

interface

uses
	System.Classes,
	System.SysUtils,
	System.Types,
	Vcl.Controls,
	Vcl.ExtCtrls,
	Vcl.Graphics,
	Vcl.Samples.Spin,
	Vcl.StdCtrls,
	Winapi.Messages,
  RipGrepper.UI.Components.BoundCheckBox;

type

	{ TCustomCheckBoxSpinEdit }

	TCustomCheckBoxSpinEdit = class(TSpinEdit)
		strict private
			FCheckBox : TBoundCheckBox;
			FCheckBoxPosition : TCheckBoxPosition;
			FCheckBoxSpacing : Integer;
			procedure setCheckBoxPosition(const _value : TCheckBoxPosition);
			procedure setCheckBoxSpacing(const _value : Integer);
			procedure CMVisiblechanged(var _message : TMessage); message CM_VISIBLECHANGED;
			procedure CMEnabledchanged(var _message : TMessage); message CM_ENABLEDCHANGED;
			procedure CMBidimodechanged(var _message : TMessage); message CM_BIDIMODECHANGED;
			procedure CMAllChildrenFlipped(var _message : TMessage); message CM_ALLCHILDRENFLIPPED;

		protected
			class function withCheckBox() : Boolean; virtual;
			class function getCheckBoxClass() : TBoundCheckBoxClass; virtual;
			procedure SetParent(_parent : TWinControl); override;
			procedure Notification(_component : TComponent; _operation : TOperation); override;
			procedure SetName(const _value : TComponentName); override;

		public
			constructor Create(_owner : TComponent); override;
			procedure SetBounds(_left : Integer; _top : Integer; _width : Integer; _height : Integer); override;
			procedure setupInternalCheckBox();
			procedure updateCheckBoxPosition();
			property CheckBox : TBoundCheckBox read FCheckBox;
			property CheckBoxPosition : TCheckBoxPosition read FCheckBoxPosition write setCheckBoxPosition default cpAbove;
			property CheckBoxSpacing : Integer read FCheckBoxSpacing write setCheckBoxSpacing default 3;
	end;

	{ TCheckBoxSpinEdit }

	TCheckBoxSpinEdit = class(TCustomCheckBoxSpinEdit)
		published
			property Align;
			property Anchors;
			property AutoSelect;
			property AutoSize;
			property BevelEdges;
			property BevelInner;
			property BevelKind;
			property BevelOuter;
			property BevelWidth;
			property BiDiMode;
			property BorderStyle;
			property CheckBox;
			property CheckBoxPosition;
			property CheckBoxSpacing;
			property Color;
			property Constraints;
			property Ctl3D;
			property DoubleBuffered;
			property DragCursor;
			property DragKind;
			property DragMode;
			property EditorEnabled;
			property Enabled;
			property Font;
			property Increment;
			property MaxLength;
			property MaxValue;
			property MinValue;
			property ParentBiDiMode;
			property ParentColor;
			property ParentCtl3D;
			property ParentDoubleBuffered;
			property ParentFont;
			property ParentShowHint;
			property PopupMenu;
			property readonly;
			property ShowHint;
			property TabOrder;
			property TabStop;
			property Touch;
			property Value;
			property Visible;
			property OnChange;
			property OnClick;
			property OnContextPopup;
			property OnDblClick;
			property OnDragDrop;
			property OnDragOver;
			property OnEndDock;
			property OnEndDrag;
			property OnEnter;
			property OnExit;
			property OnGesture;
			property OnKeyDown;
			property OnKeyPress;
			property OnKeyUp;
			property OnMouseActivate;
			property OnMouseDown;
			property OnMouseEnter;
			property OnMouseLeave;
			property OnMouseMove;
			property OnMouseUp;
			property OnMouseWheelDown;
			property OnMouseWheelUp;
			property OnStartDock;
			property OnStartDrag;
	end;

procedure Register;

implementation

uses
	RipGrepper.UI.Components.Constants;

procedure Register;
begin
	RegisterComponents(SECTION_NAME, [TCheckBoxSpinEdit]);
end;

{ TCustomCheckBoxSpinEdit }

constructor TCustomCheckBoxSpinEdit.Create(_owner : TComponent);
begin
	inherited Create(_owner);
	FCheckBoxPosition := cpAbove;
	FCheckBoxSpacing := 3;
	setupInternalCheckBox();
end;

class function TCustomCheckBoxSpinEdit.withCheckBox() : Boolean;
begin
	Result := True;
end;

class function TCustomCheckBoxSpinEdit.getCheckBoxClass() : TBoundCheckBoxClass;
begin
	Result := TBoundCheckBox;
end;

procedure TCustomCheckBoxSpinEdit.CMBidimodechanged(var _message : TMessage);
begin
	inherited;
	if FCheckBox <> nil then begin
		FCheckBox.BiDiMode := BiDiMode;
	end;
end;

procedure TCustomCheckBoxSpinEdit.CMEnabledchanged(var _message : TMessage);
begin
	inherited;
	if FCheckBox <> nil then begin
		FCheckBox.Enabled := Enabled;
	end;
end;

procedure TCustomCheckBoxSpinEdit.CMVisiblechanged(var _message : TMessage);
begin
	inherited;
	if FCheckBox <> nil then begin
		FCheckBox.Visible := Visible;
	end;
end;

procedure TCustomCheckBoxSpinEdit.CMAllChildrenFlipped(var _message : TMessage);
begin
	inherited;
	if CheckBoxPosition = cpLeft then begin
		CheckBoxPosition := cpRight;
	end else if CheckBoxPosition = cpRight then begin
		CheckBoxPosition := cpLeft;
	end else begin
		updateCheckBoxPosition();
	end;
end;

procedure TCustomCheckBoxSpinEdit.Notification(_component : TComponent; _operation : TOperation);
begin
	inherited Notification(_component, _operation);
	if (_component = FCheckBox) and (_operation = opRemove) then begin
		FCheckBox := nil;
	end;
end;

procedure TCustomCheckBoxSpinEdit.SetBounds(_left, _top, _width, _height : Integer);
begin
	if Parent <> nil then begin
		Parent.DisableAlign;
	end;
	try
		inherited SetBounds(_left, _top, _width, _height);
		updateCheckBoxPosition();
	finally
		if Parent <> nil then begin
			Parent.EnableAlign;
		end;
	end;
end;

procedure TCustomCheckBoxSpinEdit.updateCheckBoxPosition();
var
	p : TPoint;
	s : TSize;
	cbSpacing : Integer;
begin
	if FCheckBox = nil then begin
		Exit;
	end;
	s := TSize.Create(FCheckBox.Width, FCheckBox.Height);
	cbSpacing := FCheckBoxSpacing;
	case CheckBoxPosition of
		cpAbove : begin
			p := Point(Left, Top - s.Height - cbSpacing);
		end;
		cpBelow : begin
			p := Point(Left, Top + Height + cbSpacing);
		end;
		cpLeft : begin
			p := Point(Left - s.Width - cbSpacing, Top + ((Height - s.Height) div 2));
		end;
		cpRight : begin
			p := Point(Left + Width + cbSpacing, Top + ((Height - s.Height) div 2));
		end;
	end;
	FCheckBox.SetBounds(p.x, p.y, s.Width, s.Height);
	if (Parent <> nil) and Parent.HandleAllocated then begin
		Parent.Invalidate;
	end;
end;

procedure TCustomCheckBoxSpinEdit.setCheckBoxPosition(const _value : TCheckBoxPosition);
begin
	if FCheckBoxPosition <> _value then begin
		FCheckBoxPosition := _value;
		updateCheckBoxPosition();
	end;
end;

procedure TCustomCheckBoxSpinEdit.setCheckBoxSpacing(const _value : Integer);
begin
	if FCheckBoxSpacing <> _value then begin
		FCheckBoxSpacing := _value;
		updateCheckBoxPosition();
	end;
end;

procedure TCustomCheckBoxSpinEdit.SetName(const _value : TComponentName);
begin
	if (csDesigning in ComponentState) and (FCheckBox <> nil) and SameText(FCheckBox.Caption, name) then begin
		FCheckBox.Caption := _value;
		FCheckBox.IsCheckBoxModified := False;
	end;
	inherited SetName(_value);
end;

procedure TCustomCheckBoxSpinEdit.SetParent(_parent : TWinControl);
begin
	inherited SetParent(_parent);
	if FCheckBox = nil then begin
		Exit;
	end;
	FCheckBox.Parent := _parent;
	if not(csDestroying in ComponentState) then begin
		updateCheckBoxPosition();
		FCheckBox.Visible := Visible;
	end;
end;

procedure TCustomCheckBoxSpinEdit.setupInternalCheckBox();
begin
	if (FCheckBox <> nil) or not withCheckBox() then begin
		Exit;
	end;
	FCheckBox := getCheckBoxClass().Create(Self);
	FCheckBox.FreeNotification(Self);
end;

end.
