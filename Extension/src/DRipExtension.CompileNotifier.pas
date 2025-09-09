unit DRipExtension.CompileNotifier;

interface

uses
	System.Classes,
	System.SysUtils,
	Windows,
	ToolsAPI;

type
	TCompileNotifier = class(TNotifierObject, IOTAIDENotifier)
		private
			FNotifierIndex : Integer;
			class var FInstance : TCompileNotifier;

		public
			constructor Create();
			destructor Destroy(); override;

			procedure FileNotification(NotifyCode : TOTAFileNotification; const FileName : string; var Cancel : Boolean);
			procedure BeforeCompile(const Project : IOTAProject; var Cancel : Boolean);
			procedure AfterCompile(Succeeded : Boolean);

			class procedure RegisterNotifier();
			class procedure UnregisterNotifier();
	end;

implementation

uses
	DRipExtension.VSCodeBridge,
	RipGrepper.Tools.DebugUtils;

constructor TCompileNotifier.Create();
begin
	inherited Create();
	FNotifierIndex := -1;
end;

destructor TCompileNotifier.Destroy();
begin
	if FNotifierIndex <> -1 then begin
		(BorlandIDEServices as IOTAServices).RemoveNotifier(FNotifierIndex);
	end;
	inherited Destroy();
end;

procedure TCompileNotifier.FileNotification(NotifyCode : TOTAFileNotification; const FileName : string; var Cancel : Boolean);
begin
	// Not used for compile notifications
end;

procedure TCompileNotifier.BeforeCompile(const Project : IOTAProject; var Cancel : Boolean);
var
	projectName : string;
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TCompileNotifier.BeforeCompile');

	if Assigned(Project) then begin
		projectName := Project.FileName;
		dbgMsg.MsgFmt('Starting compilation of project: %s', [projectName]);
		TVsCodeBridge.SendCompileNotification('beforeCompile', projectName, False, 'Compilation started');
	end;
end;

procedure TCompileNotifier.AfterCompile(Succeeded : Boolean);
var
	projectName : string;
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TCompileNotifier.AfterCompile');

	// Get active project name
	projectName := 'Unknown Project';
	if Assigned(BorlandIDEServices) then begin
		var
		projectGroup := (BorlandIDEServices as IOTAModuleServices).GetActiveProject();
		if Assigned(projectGroup) then begin
			projectName := projectGroup.FileName;
		end;
	end;

	if Succeeded then begin
		dbgMsg.MsgFmt('Compilation succeeded for project: %s', [projectName]);
		TVsCodeBridge.SendCompileNotification('afterCompile', projectName, True);
	end else begin
		dbgMsg.MsgFmt('Compilation failed for project: %s', [projectName]);
		TVsCodeBridge.SendCompileNotification('afterCompile', projectName, False);
	end;
end;

class procedure TCompileNotifier.RegisterNotifier();
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TCompileNotifier.RegisterNotifier');

	if not Assigned(FInstance) then begin
		FInstance := TCompileNotifier.Create();
		FInstance.FNotifierIndex := (BorlandIDEServices as IOTAServices).AddNotifier(FInstance);
		dbgMsg.MsgFmt('Compile notifier registered with index: %d', [FInstance.FNotifierIndex]);
	end else begin
		dbgMsg.Msg('Compile notifier already registered');
	end;
end;

class procedure TCompileNotifier.UnregisterNotifier();
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TCompileNotifier.UnregisterNotifier');

	if Assigned(FInstance) then begin
		dbgMsg.Msg('Unregistering compile notifier');
		FreeAndNil(FInstance);
	end else begin
		dbgMsg.Msg('No compile notifier to unregister');
	end;
end;

end.
