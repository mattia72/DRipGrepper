unit RipGrepper.UI.Components.LabeledComboBox;

interface

uses
	System.Classes,
	System.SysUtils,
	System.Types,
	Vcl.Controls,
	Vcl.ExtCtrls,
	Vcl.Graphics,
	Vcl.StdCtrls, System.Messaging;

type
	TLabelPosition = (lpAbove, lpBelow, lpLeft, lpRight);

	{ TCustomLabeledComboBox }

	TCustomLabeledComboBox = class(TComboBox)
		strict private
			FEditLabel : TLabel;
			FLabelPosition : TLabelPosition;
			FLabelSpacing : Integer;
			procedure setLabelPosition(const _value : TLabelPosition);
			procedure setLabelSpacing(const _value : Integer);
			procedure updateLabelPosition();
			procedure CMVisiblechanged(var _message : TMessage); message CM_VISIBLECHANGED;
			procedure CMEnabledchanged(var _message : TMessage); message CM_ENABLEDCHANGED;
			procedure CMBidimodechanged(var _message : TMessage); message CM_BIDIMODECHANGED;

		protected
			procedure SetParent(_parent : TWinControl); override;
			procedure Notification(_component : TComponent; _operation : TOperation); override;
			procedure SetName(const _value : TComponentName); override;

		public
			constructor Create(_owner : TComponent); override;
			procedure SetBounds(_left : Integer; _top : Integer; _width : Integer; _height : Integer); override;
			procedure setupInternalLabel();
			property EditLabel : TLabel read FEditLabel;
			property LabelPosition : TLabelPosition read FLabelPosition write setLabelPosition default lpAbove;
			property LabelSpacing : Integer read FLabelSpacing write setLabelSpacing default 3;
	end;

	{ TLabeledComboBox }

	TLabeledComboBox = class(TCustomLabeledComboBox)
		published
			property Align;
			property Anchors;
			property AutoComplete;
			property AutoDropDown;
			property BevelEdges;
			property BevelInner;
			property BevelKind;
			property BevelOuter;
			property BiDiMode;
			property CharCase;
			property Color;
			property Constraints;
			property Ctl3D;
			property DoubleBuffered;
			property DragCursor;
			property DragKind;
			property DragMode;
			property DropDownCount;
			property EditLabel;
			property Enabled;
			property Font;
			property ImeMode;
			property ImeName;
			property ItemHeight;
			property ItemIndex;
			property Items;
			property LabelPosition;
			property LabelSpacing;
			property MaxLength;
			property ParentBiDiMode;
			property ParentColor;
			property ParentCtl3D;
			property ParentDoubleBuffered;
			property ParentFont;
			property ParentShowHint;
			property PopupMenu;
			property ShowHint;
			property Sorted;
			property Style;
			property TabOrder;
			property TabStop;
			property Text;
			property TextHint;
			property Touch;
			property Visible;
			property OnChange;
			property OnClick;
			property OnCloseUp;
			property OnContextPopup;
			property OnDblClick;
			property OnDragDrop;
			property OnDragOver;
			property OnDrawItem;
			property OnDropDown;
			property OnEndDock;
			property OnEndDrag;
			property OnEnter;
			property OnExit;
			property OnGesture;
			property OnKeyDown;
			property OnKeyPress;
			property OnKeyUp;
			property OnMeasureItem;
			property OnMouseActivate;
			property OnMouseDown;
			property OnMouseEnter;
			property OnMouseLeave;
			property OnMouseMove;
			property OnMouseUp;
			property OnSelect;
			property OnStartDock;
			property OnStartDrag;
	end;

implementation

{ TCustomLabeledComboBox }

constructor TCustomLabeledComboBox.Create(_owner : TComponent);
begin
	inherited Create(_owner);
	FLabelPosition := lpAbove;
	FLabelSpacing := 3;
	setupInternalLabel();
end;

procedure TCustomLabeledComboBox.CMBidimodechanged(var _message : TMessage);
begin
	inherited;
	if FEditLabel <> nil then begin
		FEditLabel.BiDiMode := BiDiMode;
	end;
end;

procedure TCustomLabeledComboBox.CMEnabledchanged(var _message : TMessage);
begin
	inherited;
	if FEditLabel <> nil then begin
		FEditLabel.Enabled := Enabled;
	end;
end;

procedure TCustomLabeledComboBox.CMVisiblechanged(var _message : TMessage);
begin
	inherited;
	if FEditLabel <> nil then begin
		FEditLabel.Visible := Visible;
	end;
end;

procedure TCustomLabeledComboBox.Notification(_component : TComponent; _operation : TOperation);
begin
	inherited Notification(_component, _operation);
	if (_component = FEditLabel) and (_operation = opRemove) then begin
		FEditLabel := nil;
	end;
end;

procedure TCustomLabeledComboBox.SetBounds(_left, _top, _width, _height : Integer);
begin
	if Parent <> nil then begin
		Parent.DisableAlign;
	end;
	try
		inherited SetBounds(_left, _top, _width, _height);
		updateLabelPosition();
	finally
		if Parent <> nil then begin
			Parent.EnableAlign;
		end;
	end;
end;

procedure TCustomLabeledComboBox.updateLabelPosition();
var
	p : TPoint;
	s : TSize;
	lSpacing : Integer;
begin
	if FEditLabel = nil then begin
		Exit;
	end;
	s := TSize.Create(FEditLabel.Width, FEditLabel.Height);
	lSpacing := FLabelSpacing;
	case LabelPosition of
		lpAbove : begin
			p := Point(Left, Top - s.Height - lSpacing);
		end;
		lpBelow : begin
			p := Point(Left, Top + Height + lSpacing);
		end;
		lpLeft : begin
			if not FEditLabel.WordWrap then begin
				s.Height := Height;
			end;
			p := Point(Left - s.Width - lSpacing, Top + ((Height - s.Height) div 2));
		end;
		lpRight : begin
			if not FEditLabel.WordWrap then begin
				s.Height := Height;
			end;
			p := Point(Left + Width + lSpacing, Top + ((Height - s.Height) div 2));
		end;
	end;
	FEditLabel.SetBounds(p.x, p.y, s.Width, s.Height);
	if (Parent <> nil) and Parent.HandleAllocated then begin
		Parent.Invalidate;
	end;
end;

procedure TCustomLabeledComboBox.setLabelPosition(const _value : TLabelPosition);
begin
	if FLabelPosition <> _value then begin
		FLabelPosition := _value;
		updateLabelPosition();
	end;
end;

procedure TCustomLabeledComboBox.setLabelSpacing(const _value : Integer);
begin
	if FLabelSpacing <> _value then begin
		FLabelSpacing := _value;
		updateLabelPosition();
	end;
end;

procedure TCustomLabeledComboBox.SetName(const _value : TComponentName);
begin
	if (csDesigning in ComponentState) and (FEditLabel <> nil) and SameText(FEditLabel.Caption, Name) then begin
		FEditLabel.Caption := _value;
	end;
	inherited SetName(_value);
end;

procedure TCustomLabeledComboBox.SetParent(_parent : TWinControl);
begin
	inherited SetParent(_parent);
	if FEditLabel = nil then begin
		Exit;
	end;
	FEditLabel.Parent := _parent;
	if not (csDestroying in ComponentState) then begin
		FEditLabel.Visible := Visible;
		updateLabelPosition();
	end;
end;

procedure TCustomLabeledComboBox.setupInternalLabel();
begin
	if FEditLabel <> nil then begin
		Exit;
	end;
	FEditLabel := TLabel.Create(Self);
	FEditLabel.FreeNotification(Self);
	FEditLabel.FocusControl := Self;
end;

end.
