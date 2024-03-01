unit RipGrepper.UI.MainFrame;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.Menus, System.Actions, Vcl.ActnList, System.ImageList,
  Vcl.ImgList, Vcl.WinXCtrls, Vcl.ComCtrls, Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.ToolWin;

type
  TRipGrepperMainFrame = class(TFrame)
    ImageListButtons: TImageList;
    ActionList: TActionList;
    ActionSearch: TAction;
    ActionCancel: TAction;
    ActionConfig: TAction;
    ActionSwitchView: TAction;
    ActionSortByFile: TAction;
    ActionShowRelativePath: TAction;
    ActionCmdLineCopy: TAction;
    ActionSortByRow: TAction;
    ActionCopyFileName: TAction;
    ActionCopyPathToClipboard: TAction;
    ActionShowSearchForm: TAction;
    ActionShowFileIcons: TAction;
    ActionAlternateRowColors: TAction;
    ActionAbortSearch: TAction;
    ActionRefreshSearch: TAction;
    ActionIndentLine: TAction;
    ActionStatusBar: TAction;
    ActionOpenWith: TAction;
    PopupMenu1: TPopupMenu;
    Openwith1: TMenuItem;
    N1: TMenuItem;
    CopyFileNameToClipboard: TMenuItem;
    CopyPathToClipboard: TMenuItem;
    ImageListListView: TImageList;
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

implementation

{$R *.dfm}

end.
