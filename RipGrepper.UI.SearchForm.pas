unit RipGrepper.UI.SearchForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls,
  System.ImageList, Vcl.ImgList, System.Actions, Vcl.ActnList;

type
  TSearchDialogForm = class(TForm)
    pnlSearch: TPanel;
    gbSearch: TGroupBox;
    lblParams: TLabel;
    lblPaths: TLabel;
    lblText: TLabel;
    cmbParameters: TComboBox;
    cmbSearchDir: TComboBox;
    cmbSearchText: TComboBox;
    btnConfig: TButton;
    btnSearch: TButton;
    btnCancel: TButton;
    ActionList1: TActionList;
    ImageList1: TImageList;
    ActionList: TActionList;
    ActionSearch: TAction;
    ActionCancel: TAction;
    ActionConfig: TAction;
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

var
  SearchDialogForm: TSearchDialogForm;

implementation

{$R *.dfm}

end.
