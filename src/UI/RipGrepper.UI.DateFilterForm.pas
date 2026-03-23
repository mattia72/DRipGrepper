unit RipGrepper.UI.DateFilterForm;

interface

uses
	Vcl.Forms,
	Vcl.StdCtrls,
	Vcl.ComCtrls,
	Vcl.ExtCtrls,
	Vcl.Controls,
	System.Classes,
	RipGrepper.Settings.NodeLook.FilterSettings,
	RipGrepper.Common.SimpleTypes,
	RipGrepper.UI.BaseForm;

type
	TDateFilterForm = class(TBaseForm)
		rgDateTimeType : TRadioGroup;
		chkDateFrom : TCheckBox;
		dtpDateFrom : TDateTimePicker;
		dtpTimeFrom : TDateTimePicker;
		chkDateTo : TCheckBox;
		dtpDateTo : TDateTimePicker;
		dtpTimeTo : TDateTimePicker;
		lblHint : TLabel;
		Panel1: TPanel;
		procedure chkDateFromClick(Sender : TObject);
		procedure chkDateToClick(Sender : TObject);
		procedure btnClearClick(Sender : TObject);
		procedure FormCloseQuery(Sender : TObject; var CanClose : Boolean);
		procedure rgDateTimeTypeClick(Sender : TObject);

		private
			FFilterSettings : TFilterSettings;
			FClearRequested : Boolean;
			procedure LoadFromSettings;
			procedure SaveToSettings;

		public
			constructor Create(_filterSettings : TFilterSettings); reintroduce;
			// True if the user clicked "Clear" to reset the date filter
			property ClearRequested : Boolean read FClearRequested;
	end;

implementation

{$R *.dfm}

uses
	System.SysUtils,
	System.DateUtils;

constructor TDateFilterForm.Create(_filterSettings : TFilterSettings);
begin
	inherited Create(nil);
	FFilterSettings := _filterSettings;
	FClearRequested := False;
	LoadFromSettings;
end;

procedure TDateFilterForm.LoadFromSettings;
var
	dtFrom, dtTo : TDateTime;
	dtt : EDateTimeType;
begin
	dtt := FFilterSettings.DateTimeType;
	rgDateTimeType.ItemIndex := Ord(dtt);

	dtFrom := FFilterSettings.DateFrom;
	dtTo := FFilterSettings.DateTo;

	if dtFrom > 0 then begin
		chkDateFrom.Checked := True;
		dtpDateFrom.Date := DateOf(dtFrom);
		dtpTimeFrom.Time := TimeOf(dtFrom);
		dtpDateFrom.Enabled := True;
		dtpTimeFrom.Enabled := True;
	end else begin
		chkDateFrom.Checked := False;
		dtpDateFrom.Date := Date();
		dtpTimeFrom.Time := 0;
		dtpDateFrom.Enabled := False;
		dtpTimeFrom.Enabled := False;
	end;

	if dtTo > 0 then begin
		chkDateTo.Checked := True;
		dtpDateTo.Date := DateOf(dtTo);
		dtpTimeTo.Time := TimeOf(dtTo);
		dtpDateTo.Enabled := True;
		dtpTimeTo.Enabled := True;
	end else begin
		chkDateTo.Checked := False;
		dtpDateTo.Date := Date();
		dtpTimeTo.Time := 0;
		dtpDateTo.Enabled := False;
		dtpTimeTo.Enabled := False;
	end;

	// Show hint for Last Access time
	if dtt = EDateTimeType.dttLastAccess then begin
		lblHint.Caption := 'Last Access Time may be disabled on some systems';
		lblHint.Visible := True;
	end;
end;

procedure TDateFilterForm.SaveToSettings;
begin
	FFilterSettings.DateTimeType := EDateTimeType(rgDateTimeType.ItemIndex);

	if chkDateFrom.Checked then begin
		FFilterSettings.DateFrom := DateOf(dtpDateFrom.Date) + TimeOf(dtpTimeFrom.Time);
	end else begin
		FFilterSettings.DateFrom := 0;
	end;

	if chkDateTo.Checked then begin
		FFilterSettings.DateTo := DateOf(dtpDateTo.Date) + TimeOf(dtpTimeTo.Time);
	end else begin
		FFilterSettings.DateTo := 0;
	end;
end;

procedure TDateFilterForm.chkDateFromClick(Sender : TObject);
begin
	dtpDateFrom.Enabled := chkDateFrom.Checked;
	dtpTimeFrom.Enabled := chkDateFrom.Checked;
end;

procedure TDateFilterForm.chkDateToClick(Sender : TObject);
begin
	dtpDateTo.Enabled := chkDateTo.Checked;
	dtpTimeTo.Enabled := chkDateTo.Checked;
end;

procedure TDateFilterForm.btnClearClick(Sender : TObject);
begin
	FFilterSettings.DateFrom := 0;
	FFilterSettings.DateTo := 0;
	FFilterSettings.DateTimeType := EDateTimeType.dttLastWrite;
	FClearRequested := True;
	ModalResult := mrOk;
end;

procedure TDateFilterForm.FormCloseQuery(Sender : TObject; var CanClose : Boolean);
begin
	if ModalResult = mrOk then begin
		if not FClearRequested then begin
			SaveToSettings;
		end;
	end;
	CanClose := True;
end;

procedure TDateFilterForm.rgDateTimeTypeClick(Sender : TObject);
begin
	if rgDateTimeType.ItemIndex = Ord(EDateTimeType.dttLastAccess) then begin
		lblHint.Caption := 'Last Access Time may be disabled on some systems';
		lblHint.Visible := True;
	end else begin
		lblHint.Visible := False;
	end;
end;

end.
