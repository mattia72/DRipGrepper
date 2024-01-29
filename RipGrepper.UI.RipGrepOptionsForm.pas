unit RipGrepper.UI.RipGrepOptionsForm;

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
	Vcl.ExtCtrls,
	Vcl.StdCtrls,
	RipGrepper.Common.Settings,
	System.Actions,
	Vcl.ActnList;

type
	TRipGrepOptionsForm = class(TForm)
		ListBox1 : TListBox;
		PanelMain : TPanel;
		btnOk : TButton;
		btnCancel : TButton;
		ActionList1 : TActionList;
		ActionOk : TAction;
		ActionCancel : TAction;
		ActionAdd : TAction;
		PanelBottom : TPanel;
		PanelTop : TPanel;
		procedure ActionCancelExecute(Sender : TObject);
		procedure ActionOkExecute(Sender : TObject);
		procedure FormShow(Sender : TObject);
		procedure ListBox1Click(Sender : TObject);
		procedure LoadRipGrepHelp;

		private
			FOptionList : TStringList;
			FRipGrepParameters : TRipGrepParameterSettings;

			{ Private-Deklarationen }
		public
			constructor Create(AOwner : TComponent; const _settings : TRipGrepParameterSettings); reintroduce; virtual;
			destructor Destroy; override;
			{ Public-Deklarationen }
	end;

var
	RipGrepOptionsForm : TRipGrepOptionsForm;

implementation

uses
	RipGrepper.Tools.ProcessUtils,
	System.RegularExpressions, RipGrepper.Common.Types;

{$R *.dfm}

constructor TRipGrepOptionsForm.Create(AOwner : TComponent; const _settings : TRipGrepParameterSettings);
begin
	inherited Create(AOwner);
	FRipGrepParameters := _settings;
	FOptionList := TStringList.Create(TDuplicates.dupIgnore, False, False);
	FOptionList.Delimiter := ' ';
	FOptionList.AddStrings(FRipGrepParameters.Options.Split([' ']));
end;

destructor TRipGrepOptionsForm.Destroy;
begin
	FOptionList.Free;
	inherited;
end;

procedure TRipGrepOptionsForm.ActionCancelExecute(Sender : TObject);
begin
	ModalResult := mrCancel;
end;

procedure TRipGrepOptionsForm.ActionOkExecute(Sender : TObject);
begin
	FRipGrepParameters.Options := FOptionList.DelimitedText;
	ModalResult := mrOk;
end;

procedure TRipGrepOptionsForm.FormShow(Sender : TObject);
begin
	LoadRipGrepHelp;
end;

procedure TRipGrepOptionsForm.ListBox1Click(Sender : TObject);
var
	selected : string;
	regex : TRegex;
	m : TMatch;
begin
	regex := TRegex.Create(RG_HELP_LINE_REGEX);
	selected := ListBox1.Items[ListBox1.ItemIndex];
	m := regex.Match(selected);
	if m.Success then begin
		if (m.Groups['value'].Value <> '') then begin
			FOptionList.Add(m.Groups['long'].Value+ m.Groups['value'].Value);
		end else if (m.Groups['value'].Value <> '') then begin
			FOptionList.Add(m.Groups['long'].Value);
        end;
	end;
end;

procedure TRipGrepOptionsForm.LoadRipGrepHelp;
var
	sl : TStrings;
begin
	sl := TStringList.Create();
	sl.Add('-help');
	try
		TSimpleProcessOutputStringReader.RunProcess(FRipGrepParameters.RipGrepPath, sl, '.', sl);
		ListBox1.Items.Assign(sl);
	finally
		sl.Free;
	end;
end;

end.
