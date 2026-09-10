unit DRipExtension.IDEContextNotifier;

interface

uses
	System.Classes,
	System.SysUtils,
	ToolsAPI;

type
	{ Keeps the cached Delphi IDE context of the settings in sync with the IDE. If the user switches
	  the active project, opens or closes a project group or (un)installs a package, the cached
	  project directory and library path become outdated. They are invalidated here and the result
	  view is repainted, so the "file is outside of project scope" indicators are recalculated. }
	TIDEContextNotifier = class(TNotifierObject, IOTAIDENotifier)
		private
			procedure invalidateIDEContext(const _notifyCode : TOTAFileNotification; const _fileName : string);

		public
			procedure FileNotification(NotifyCode : TOTAFileNotification; const FileName : string; var Cancel : Boolean);
			procedure BeforeCompile(const Project : IOTAProject; var Cancel : Boolean);
			procedure AfterCompile(Succeeded : Boolean);

			class procedure RegisterNotifier();
			class procedure UnregisterNotifier();
	end;

implementation

uses
	RipGrepper.Settings.RipGrepperSettings,
	RipGrepper.Tools.DebugUtils,
	RipGrepper.UI.ParentFrame,
	Spring.DesignPatterns,
	System.TypInfo;

var
	{ The notifier is held as an interface reference, so it is destroyed by the reference counting
	  after RemoveNotifier. Freeing the instance directly would re-enter the destructor. }
	GNotifier : IOTAIDENotifier = nil;
	GNotifierIndex : Integer = -1;

procedure TIDEContextNotifier.FileNotification(NotifyCode : TOTAFileNotification; const FileName : string; var Cancel : Boolean);
begin
	{ No begin/end debug message here: this is called for every file operation of the IDE. Only the
	  notifications which may change the project directory or the library path are logged. }
	case NotifyCode of
		ofnActiveProjectChanged, ofnEndProjectGroupOpen, ofnEndProjectGroupClose, ofnPackageInstalled,
		{ } ofnPackageUninstalled : begin
			invalidateIDEContext(NotifyCode, FileName);
		end;
	end;
end;

procedure TIDEContextNotifier.BeforeCompile(const Project : IOTAProject; var Cancel : Boolean);
begin
	// The IDE context doesn't change by compiling
end;

procedure TIDEContextNotifier.AfterCompile(Succeeded : Boolean);
begin
	// The IDE context doesn't change by compiling
end;

procedure TIDEContextNotifier.invalidateIDEContext(const _notifyCode : TOTAFileNotification; const _fileName : string);
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TIDEContextNotifier.invalidateIDEContext');
	dbgMsg.MsgFmt('NotifyCode = %s, FileName = %s',
	{ } [GetEnumName(TypeInfo(TOTAFileNotification), Integer(_notifyCode)), _fileName]);

	try
		var
		settings := TSingleton.GetInstance<TRipGrepperSettings>();
		settings.SearchFormSettings.ExtensionSettings.InvalidateIDEContext();

		if Assigned(ParentFrame) and ParentFrame.IsInitialized then begin
			ParentFrame.MainFrame.RefreshFileNodeIndicators();
		end else begin
			dbgMsg.Msg('ParentFrame isn''t initialized, nothing to repaint');
		end;
	except
		// an exception must not escape into the notification chain of the IDE
		on E : Exception do begin
			dbgMsg.ErrorMsg(E.ClassName + ': ' + E.Message);
		end;
	end;
end;

class procedure TIDEContextNotifier.RegisterNotifier();
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TIDEContextNotifier.RegisterNotifier');

	if Assigned(GNotifier) then begin
		dbgMsg.Msg('IDE context notifier already registered');
		Exit;
	end;

	GNotifier := TIDEContextNotifier.Create();
	GNotifierIndex := (BorlandIDEServices as IOTAServices).AddNotifier(GNotifier);
	dbgMsg.MsgFmt('IDE context notifier registered with index: %d', [GNotifierIndex]);
end;

class procedure TIDEContextNotifier.UnregisterNotifier();
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TIDEContextNotifier.UnregisterNotifier');

	if not Assigned(GNotifier) then begin
		dbgMsg.Msg('No IDE context notifier to unregister');
		Exit;
	end;

	dbgMsg.MsgFmt('Unregistering IDE context notifier with index: %d', [GNotifierIndex]);
	if GNotifierIndex <> -1 then begin
		(BorlandIDEServices as IOTAServices).RemoveNotifier(GNotifierIndex);
		GNotifierIndex := -1;
	end;
	GNotifier := nil;
end;

end.
