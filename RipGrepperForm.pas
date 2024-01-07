unit RipGrepperForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ComCtrls,
  Vcl.ExtCtrls,
  System.ImageList, Vcl.ImgList, System.Actions, Vcl.ActnList, ProcessTools;

type
  TRipGrepperForm = class(TForm)
    panelMain: TPanel;
    Label1: TLabel;
    LabelParams: TLabel;
    Label3: TLabel;
    btnConfig: TButton;
    lvResult: TListView;
    pnl_Bottom: TPanel;
    btn_Save: TButton;
    btn_Cancel: TButton;
    ImageListButtons: TImageList;
    alActions: TActionList;
    ActionSearch: TAction;
    ActionCancel: TAction;
    ActionConfig: TAction;
    cmbSearchDir: TComboBox;
    cmbSearchText: TComboBox;
    cmbParameters: TComboBox;
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

procedure OnNewLine(_handler: INewLineEventHandler; const _sLine: AnsiString);

var
  Form1: TRipGrepperForm;

implementation

uses
  DebugTools;

{$R *.dfm}

procedure OnNewLine(_handler: INewLineEventHandler; const _sLine: AnsiString);
begin
  _handler.OnNewResultLine(_sLine);
  TDebugUtils.DebugMessage(string(_sLine));
end;

end.
