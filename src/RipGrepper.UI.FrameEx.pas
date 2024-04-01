unit RipGrepper.UI.FrameEx;

interface

uses
	Dialogs,
	Classes,
	Controls,
	StdCtrls,
	Windows,
	Messages,
	Vcl.Forms;

type

	TFrameEx = class(TFrame)
		private
			FOnShow : TNotifyEvent;
			FOnHide : TNotifyEvent;

			procedure setOnShow(ponshow : TNotifyEvent);
			procedure setOnHide(ponhide : TNotifyEvent);

			procedure FrameOnShowHide(var M : TMessage); message CM_SHOWINGCHANGED;

		published
			property OnShow : TNotifyEvent write setOnShow;
			property OnHide : TNotifyEvent write setOnHide;
	end;

implementation

procedure TFrameEx.setOnShow(ponshow : TNotifyEvent);
begin
	FOnShow := ponshow;
end;

procedure TFrameEx.setOnHide(ponhide : TNotifyEvent);
begin
	FOnHide := ponhide;
end;

procedure TFrameEx.FrameOnShowHide(var M : TMessage);
begin
	inherited;

	if Showing then // onShow
		if Assigned(FOnShow) then
			FOnShow(Self)
		else // onHide
			if Assigned(FOnHide) then
				FOnHide(Self);
end;
end.
