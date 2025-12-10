unit RipGrepper.UI.Components.NotifyingControls;

interface

uses
	Winapi.Messages,
	System.Classes,
	Vcl.Controls,
	Vcl.StdCtrls,
	Vcl.Samples.Spin;

type
	// Interface for items that can receive notifications from controls
	INotifyingControlOwner = interface
		['{8F3A5B2C-1D4E-4F7A-9B6C-2E8D5F4A1C3B}']
		procedure OnSubItemEnabledChanged(_sender : TObject);
	end;

	// Custom SpinEdit that notifies parent about enabled state changes
	TNotifyingSpinEdit = class(TSpinEdit)
		private
			FOwnerItem : INotifyingControlOwner;

		protected
			procedure CMEnabledChanged(var Message : TMessage); message CM_ENABLEDCHANGED;

		public
			property OwnerItem : INotifyingControlOwner read FOwnerItem write FOwnerItem;
	end;

	// Custom ComboBox that notifies parent about enabled state changes
	TNotifyingComboBox = class(TComboBox)
		private
			FOwnerItem : INotifyingControlOwner;

		protected
			procedure CMEnabledChanged(var Message : TMessage); message CM_ENABLEDCHANGED;

		public
			property OwnerItem : INotifyingControlOwner read FOwnerItem write FOwnerItem;
	end;

	// Custom CheckBox that notifies parent about enabled state changes
	TNotifyingCheckBox = class(TCheckBox)
		private
			FOwnerItem : INotifyingControlOwner;

		protected
			procedure CMEnabledChanged(var Message : TMessage); message CM_ENABLEDCHANGED;

		public
			property OwnerItem : INotifyingControlOwner read FOwnerItem write FOwnerItem;

		published
			property AutoSize;
	end;

procedure Register;

implementation

uses
// please don't use any uses, that pulls dependencies
	RipGrepper.UI.Components.Constants;

{ TNotifyingSpinEdit }

procedure TNotifyingSpinEdit.CMEnabledChanged(var Message : TMessage);
begin
	inherited;
	if Assigned(FOwnerItem) then begin
		FOwnerItem.OnSubItemEnabledChanged(Self);
	end;
end;

{ TNotifyingComboBox }

procedure TNotifyingComboBox.CMEnabledChanged(var Message : TMessage);
begin
	inherited;
	if Assigned(FOwnerItem) then begin
		FOwnerItem.OnSubItemEnabledChanged(Self);
	end;
end;

{ TNotifyingCheckBox }

procedure TNotifyingCheckBox.CMEnabledChanged(var Message : TMessage);
begin
	inherited;
	if Assigned(FOwnerItem) then begin
		FOwnerItem.OnSubItemEnabledChanged(Self);
	end;
end;

procedure Register;
begin
	RegisterComponents(SECTION_NAME, [TNotifyingSpinEdit, TNotifyingComboBox, TNotifyingCheckBox]);
end;

end.
