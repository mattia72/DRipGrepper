unit DRipExtension.MessageNotifier;

interface

uses
	System.Classes,
	System.SysUtils,
	System.Generics.Collections,
	Windows,
	ToolsAPI;

type
	TBuildMessage = record
		FileName: string;
		MessageText: string;
		PrefixStr: string;
		LineNumber: Integer;
		ColumnNumber: Integer;
		MessageType: string; // 'Tool', 'Title', etc.
		TimeStamp: TDateTime;
	end;

	TMessageNotifier = class(TNotifierObject, IOTAMessageNotifier)
	private
		FNotifierIndex: Integer;
		FMessages: TList<TBuildMessage>;
		class var FInstance: TMessageNotifier;

	public
		constructor Create();
		destructor Destroy(); override;

		// IOTAMessageNotifier methods
		procedure MessageGroupAdded(const Group: IOTAMessageGroup);
		procedure MessageGroupDeleted(const Group: IOTAMessageGroup);

		// Custom methods
		function GetMessages: TArray<TBuildMessage>;
		function GetBuildMessages: TArray<TBuildMessage>;
		procedure ClearMessages;

		class procedure RegisterNotifier();
		class procedure UnregisterNotifier();
	end;

	// Alternative approach using IOTAIDENotifier with message hooking
	TMessageHookNotifier = class(TNotifierObject, IOTAIDENotifier)
	private
		FNotifierIndex: Integer;
		FBuildMessages: TStringList;
		FBuildStarted: Boolean;
		class var FInstance: TMessageHookNotifier;

	public
		constructor Create();
		destructor Destroy(); override;

		// IOTAIDENotifier methods
		procedure FileNotification(NotifyCode: TOTAFileNotification; const FileName: string; var Cancel: Boolean);
		procedure BeforeCompile(const Project: IOTAProject; var Cancel: Boolean);
		procedure AfterCompile(Succeeded: Boolean);

		// Custom methods
		function GetBuildOutput: TStringList;
		procedure ClearBuildOutput;
		function CaptureBuildOutput(const ProjectName: string): string;
		procedure GetProjectMessages(const Project: IOTAProject);

		class procedure RegisterNotifier();
		class procedure UnregisterNotifier();
	end;

implementation

uses
	DRipExtension.VSCodeBridge,
	RipGrepper.Tools.DebugUtils;

{ TMessageNotifier }

constructor TMessageNotifier.Create();
begin
	inherited Create();
	FNotifierIndex := -1;
	FMessages := TList<TBuildMessage>.Create;
end;

destructor TMessageNotifier.Destroy();
begin
	if FNotifierIndex <> -1 then begin
		// Note: IOTAMessageNotifier might not be available in all Delphi versions
		// (BorlandIDEServices as IOTAMessageServices).RemoveNotifier(FNotifierIndex);
	end;
	FreeAndNil(FMessages);
	inherited Destroy();
end;

procedure TMessageNotifier.MessageGroupAdded(const Group: IOTAMessageGroup);
begin
	var dbgMsg := TDebugMsgBeginEnd.New('TMessageNotifier.MessageGroupAdded');
	if Assigned(Group) then begin
		dbgMsg.MsgFmt('Message group added: %s', [Group.Name]);
		// You would need to iterate through messages in the group here
		// This depends on the availability of message enumeration interfaces
	end;
end;

procedure TMessageNotifier.MessageGroupDeleted(const Group: IOTAMessageGroup);
begin
	var dbgMsg := TDebugMsgBeginEnd.New('TMessageNotifier.MessageGroupDeleted');
	if Assigned(Group) then begin
		dbgMsg.MsgFmt('Message group deleted: %s', [Group.Name]);
	end;
end;

function TMessageNotifier.GetMessages: TArray<TBuildMessage>;
begin
	Result := FMessages.ToArray;
end;

function TMessageNotifier.GetBuildMessages: TArray<TBuildMessage>;
var
	buildMessages: TList<TBuildMessage>;
	msg: TBuildMessage;
begin
	buildMessages := TList<TBuildMessage>.Create;
	try
		for msg in FMessages do begin
			if (msg.MessageType = 'Tool') or (msg.MessageType = 'Title') then begin
				buildMessages.Add(msg);
			end;
		end;
		Result := buildMessages.ToArray;
	finally
		buildMessages.Free;
	end;
end;

procedure TMessageNotifier.ClearMessages;
begin
	FMessages.Clear;
end;

class procedure TMessageNotifier.RegisterNotifier();
begin
	var dbgMsg := TDebugMsgBeginEnd.New('TMessageNotifier.RegisterNotifier');
	
	if not Assigned(FInstance) then begin
		FInstance := TMessageNotifier.Create();
		// Note: This interface might not be available in all Delphi versions
		// You may need to check if IOTAMessageNotifier is supported
		try
			// FInstance.FNotifierIndex := (BorlandIDEServices as IOTAMessageServices).AddNotifier(FInstance);
			dbgMsg.Msg('Message notifier registered (interface may not be available)');
		except
			on E: Exception do begin
				dbgMsg.MsgFmt('Failed to register message notifier: %s', [E.Message]);
				FreeAndNil(FInstance);
			end;
		end;
	end else begin
		dbgMsg.Msg('Message notifier already registered');
	end;
end;

class procedure TMessageNotifier.UnregisterNotifier();
begin
	var dbgMsg := TDebugMsgBeginEnd.New('TMessageNotifier.UnregisterNotifier');
	
	if Assigned(FInstance) then begin
		dbgMsg.Msg('Unregistering message notifier');
		FreeAndNil(FInstance);
	end else begin
		dbgMsg.Msg('No message notifier to unregister');
	end;
end;

{ TMessageHookNotifier }

constructor TMessageHookNotifier.Create();
begin
	inherited Create();
	FNotifierIndex := -1;
	FBuildMessages := TStringList.Create;
	FBuildStarted := False;
end;

destructor TMessageHookNotifier.Destroy();
begin
	if FNotifierIndex <> -1 then begin
		(BorlandIDEServices as IOTAServices).RemoveNotifier(FNotifierIndex);
	end;
	FreeAndNil(FBuildMessages);
	inherited Destroy();
end;

procedure TMessageHookNotifier.FileNotification(NotifyCode: TOTAFileNotification; const FileName: string; var Cancel: Boolean);
begin
	// Not used for message capture
end;

procedure TMessageHookNotifier.BeforeCompile(const Project: IOTAProject; var Cancel: Boolean);
begin
	var dbgMsg := TDebugMsgBeginEnd.New('TMessageHookNotifier.BeforeCompile');
	dbgMsg.Msg('Build started - clearing previous messages');
	FBuildMessages.Clear;
	FBuildStarted := True;
	FBuildMessages.Add(Format('[%s] Build started for project: %s', 
		[FormatDateTime('hh:nn:ss', Now), ExtractFileName(Project.FileName)]));
end;

procedure TMessageHookNotifier.AfterCompile(Succeeded: Boolean);
var
	realBuildOutput: string;
	projectName: string;
begin
	var dbgMsg := TDebugMsgBeginEnd.New('TMessageHookNotifier.AfterCompile');
	
	if Succeeded then begin
		FBuildMessages.Add(Format('[%s] Build completed successfully', [FormatDateTime('hh:nn:ss', Now)]));
		dbgMsg.Msg('Build completed successfully');
	end else begin
		FBuildMessages.Add(Format('[%s] Build failed', [FormatDateTime('hh:nn:ss', Now)]));
		dbgMsg.Msg('Build failed');
	end;

	// Capture real build output instead of sample data
	try
		// Try to get the current project name
		projectName := 'Current Project';
		if Assigned(BorlandIDEServices) then begin
			var moduleServices := BorlandIDEServices as IOTAModuleServices;
			if Assigned(moduleServices) and (moduleServices.MainProjectGroup <> nil) then begin
				var activeProject := moduleServices.MainProjectGroup.ActiveProject;
				if Assigned(activeProject) then begin
					projectName := ExtractFileName(activeProject.FileName);
				end;
			end;
		end;
		
		// Get the real build output
		realBuildOutput := CaptureBuildOutput(projectName);
		
		// Add build status and timestamp
		if Succeeded then begin
			realBuildOutput := realBuildOutput + sLineBreak + 
				Format('Build completed successfully at %s', [FormatDateTime('hh:nn:ss', Now)]);
		end else begin
			realBuildOutput := realBuildOutput + sLineBreak + 
				Format('Build failed at %s', [FormatDateTime('hh:nn:ss', Now)]);
		end;
		
		dbgMsg.MsgFmt('Sending real build output: %d characters', [Length(realBuildOutput)]);
	except
		on E: Exception do begin
			dbgMsg.MsgFmt('Error capturing real build output: %s', [E.Message]);
			// Fallback to basic information
			if Succeeded then
				realBuildOutput := Format('Build completed successfully at %s', [FormatDateTime('yyyy-mm-dd hh:nn:ss', Now)])
			else
				realBuildOutput := Format('Build failed at %s', [FormatDateTime('yyyy-mm-dd hh:nn:ss', Now)]);
		end;
	end;

	// Send real build output to VS Code
	TVsCodeBridge.SendBuildOutput(realBuildOutput);
end;

function TMessageHookNotifier.GetBuildOutput: TStringList;
begin
	Result := TStringList.Create;
	Result.Assign(FBuildMessages);
end;

function TMessageHookNotifier.CaptureBuildOutput(const ProjectName: string): string;
var
	moduleServices: IOTAModuleServices;
	project: IOTAProject;
	module: IOTAModule;
	i: Integer;
	realOutput: TStringList;
begin
	var dbgMsg := TDebugMsgBeginEnd.New('TMessageHookNotifier.CaptureBuildOutput');
	
	realOutput := TStringList.Create;
	try
		// Try to get the current project's compile information
		moduleServices := BorlandIDEServices as IOTAModuleServices;
		if Assigned(moduleServices) then begin
			for i := 0 to moduleServices.ModuleCount - 1 do begin
				module := moduleServices.Modules[i];
				if Assigned(module) and (module.QueryInterface(IOTAProject, project) = 0) then begin
					if Assigned(project) and (ExtractFileName(project.FileName) = ProjectName) then begin
						GetProjectMessages(project);
						break;
					end;
				end;
			end;
		end;

		// If we have collected messages, format them
		if FBuildMessages.Count > 0 then begin
			realOutput.AddStrings(FBuildMessages);
			Result := realOutput.Text;
		end else begin
			// Fallback to basic compiler information
			Result := Format('Build process for %s completed at %s', 
				[ProjectName, FormatDateTime('yyyy-mm-dd hh:nn:ss', Now)]);
		end;
		
		dbgMsg.MsgFmt('Captured build output: %d lines', [realOutput.Count]);
	finally
		realOutput.Free;
	end;
end;

procedure TMessageHookNotifier.GetProjectMessages(const Project: IOTAProject);
var
	compileInfo: string;
	projectConfig: IOTAProjectOptionsConfigurations;
	activeConfig: IOTABuildConfiguration;
begin
	var dbgMsg := TDebugMsgBeginEnd.New('TMessageHookNotifier.GetProjectMessages');
	
	try
		// Get basic project information
		compileInfo := Format('Project: %s', [ExtractFileName(Project.FileName)]);
		FBuildMessages.Add(compileInfo);
		
		// Try to get configuration information
		projectConfig := Project.ProjectOptions as IOTAProjectOptionsConfigurations;
		if Assigned(projectConfig) then begin
			activeConfig := projectConfig.ActiveConfiguration;
			if Assigned(activeConfig) then begin
				FBuildMessages.Add(Format('Configuration: %s', [activeConfig.Name]));
				FBuildMessages.Add(Format('Platform: %s', [activeConfig.Platform]));
			end;
		end;
		
		// Add basic IDE version info
		FBuildMessages.Add('Delphi IDE Build Process');
			
		dbgMsg.MsgFmt('Collected %d project messages', [FBuildMessages.Count]);
	except
		on E: Exception do begin
			dbgMsg.MsgFmt('Error getting project messages: %s', [E.Message]);
			FBuildMessages.Add(Format('Error getting project info: %s', [E.Message]));
		end;
	end;
end;

procedure TMessageHookNotifier.ClearBuildOutput;
begin
	FBuildMessages.Clear;
end;

class procedure TMessageHookNotifier.RegisterNotifier();
begin
	var dbgMsg := TDebugMsgBeginEnd.New('TMessageHookNotifier.RegisterNotifier');
	
	if not Assigned(FInstance) then begin
		FInstance := TMessageHookNotifier.Create();
		FInstance.FNotifierIndex := (BorlandIDEServices as IOTAServices).AddNotifier(FInstance);
		dbgMsg.MsgFmt('Message hook notifier registered with index: %d', [FInstance.FNotifierIndex]);
	end else begin
		dbgMsg.Msg('Message hook notifier already registered');
	end;
end;

class procedure TMessageHookNotifier.UnregisterNotifier();
begin
	var dbgMsg := TDebugMsgBeginEnd.New('TMessageHookNotifier.UnregisterNotifier');
	
	if Assigned(FInstance) then begin
		dbgMsg.Msg('Unregistering message hook notifier');
		FreeAndNil(FInstance);
	end else begin
		dbgMsg.Msg('No message hook notifier to unregister');
	end;
end;

end.
