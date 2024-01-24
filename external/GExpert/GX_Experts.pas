unit GX_Experts;

interface

uses
  SysUtils,
  Classes,
  ActnList,
  Menus,
  Forms,
  GX_ConfigurationInfo;

type
  TGX_Expert = class
  private
    FActive: Boolean;
  protected
    procedure SetActive(New: Boolean); virtual;
    procedure InternalLoadSettings(_Settings: IExpertSettings); virtual;
    procedure InternalSaveSettings(_Settings: IExpertSettings); virtual;
    procedure SetShortCut(Value: TShortCut); virtual;
    procedure UpdateAction(Action: TCustomAction); virtual;
    function HasSubmenuItems: Boolean; virtual;
    function HasCallCount: Boolean; virtual;
    procedure CreateSubMenuItems(MenuItem: TMenuItem); virtual;
    procedure SetFormIcon(Form: TForm);
  public
    constructor Create; virtual;
    function GetDefaultShortCut: TShortCut; virtual;
    function GetActionCaption: string; virtual;
    function CanHaveShortCut: Boolean; virtual;
    class function ConfigurationKey: string; virtual;
    class function GetName: string; virtual;
    function GetHelpString: string; virtual;
    function HasMenuItem: Boolean; virtual;
    procedure Execute(Sender: TObject); virtual;
    procedure Configure; virtual;
    function HasConfigOptions: Boolean; virtual;
    procedure IncCallCount;
    procedure LoadSettings;
    procedure SaveSettings;
    property Active: Boolean read FActive write FActive;
  end;

type
  TGX_ExpertClass = class of TGX_Expert;

procedure RegisterGX_Expert(AClass: TGX_ExpertClass);

implementation

uses
  Graphics;

procedure RegisterGX_Expert(AClass: TGX_ExpertClass);
begin
  // do nothing
end;

{ TGX_Expert }

function TGX_Expert.CanHaveShortCut: Boolean;
begin
  Result := False;
end;

class function TGX_Expert.ConfigurationKey: string;
begin
  Result := '';
end;

procedure TGX_Expert.Configure;
begin
  // do nothing
end;

constructor TGX_Expert.Create;
begin
  inherited;
end;

procedure TGX_Expert.CreateSubMenuItems(MenuItem: TMenuItem);
begin
  // do nothing
end;

procedure TGX_Expert.Execute(Sender: TObject);
begin
  // do nothing
end;

function TGX_Expert.GetActionCaption: string;
begin
  Result := '';
end;

function TGX_Expert.GetDefaultShortCut: TShortCut;
begin
  Result := 0;
end;

function TGX_Expert.GetHelpString: string;
begin
  Result := '';
end;

class function TGX_Expert.GetName: string;
begin
  Result := '';
end;

function TGX_Expert.HasCallCount: Boolean;
begin
  Result := False;
end;

function TGX_Expert.HasConfigOptions: Boolean;
begin
  Result := False;
end;

function TGX_Expert.HasMenuItem: Boolean;
begin
  Result := False;
end;

function TGX_Expert.HasSubmenuItems: Boolean;
begin
  Result := False;
end;

procedure TGX_Expert.IncCallCount;
begin
  // do nothing
end;

procedure TGX_Expert.InternalLoadSettings(_Settings: IExpertSettings);
begin
  // do nothing
end;

procedure TGX_Expert.InternalSaveSettings(_Settings: IExpertSettings);
begin
  // do nothing
end;

procedure TGX_Expert.LoadSettings;
begin
  InternalLoadSettings(ConfigInfo.GetExpertSettings(ConfigurationKey, ''));
end;

procedure TGX_Expert.SaveSettings;
begin
  InternalSaveSettings(ConfigInfo.GetExpertSettings(ConfigurationKey, ''));
end;

procedure TGX_Expert.SetActive(New: Boolean);
begin
  // do nothing
end;

procedure TGX_Expert.SetFormIcon(Form: TForm);
begin
  // do nothing
end;

procedure TGX_Expert.SetShortCut(Value: TShortCut);
begin
  // do nothing
end;

procedure TGX_Expert.UpdateAction(Action: TCustomAction);
begin
  // do nothing
end;

end.
