unit RipGrepper.Common.IOTAFileUtils;

interface

uses
	RipGrepper.Helper.UI,
	ArrayEx,
	RipGrepper.Common.SimpleTypes;

type
	IOTAFileUtils = class

		private
			// Build dialog data (buttons, question, caption, details) for the save modified files question.
			// details is intended for the expandable info section.
			class procedure prepareSaveModifiedFilesDialog(const _relatedFiles, _allModifiedFiles : TArrayEx<string>;
				out _buttons : TMessageDialogButtons; out _question, _caption, _details : string);
			class function showSaveModifiedFilesQuestion(const _relatedFiles : TArrayEx<string>; const _allModifiedFiles : TArrayEx<string>)
				: ESaveModifiedFilesResults;

		public
			class function AskSaveModifiedFiles(const _filePath : string = '') : ESaveModifiedFilesResults;
	end;

implementation

uses
	RipGrepper.Tools.DebugUtils,
	System.SysUtils,
	RipGrepper.Common.IOTAUtils,
	System.UITypes,
	RipGrepper.Common.Constants,
	System.StrUtils;

class function IOTAFileUtils.AskSaveModifiedFiles(const _filePath : string = '') : ESaveModifiedFilesResults;
var
	baseFileName : string;
	dfmFile : string;
	modifiedRelatedFiles : TArrayEx<string>;
	modifiedBuffers : TArrayEx<string>;
	pasFile : string;
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('IOTAFileUtils.AskSaveModifiedFiles');

	modifiedBuffers := IOTAUtils.GetOpenedEditorFiles(True);
	if not _filePath.IsEmpty then begin
		baseFileName := ChangeFileExt(_filePath, '');
		dfmFile := baseFileName + '.dfm';
		pasFile := baseFileName + '.pas';

		modifiedRelatedFiles.Add(_filePath);
		dbgMsg.Msg('add modified file: ' + _filePath);

		if modifiedBuffers.Contains(dfmFile) then begin
			var idx := modifiedRelatedFiles.AddIfNotContains(dfmFile);
			dbgMsg.MsgIf(idx <> -1, 'add modified dfm: ' + dfmFile);
		end;

		if modifiedBuffers.Contains(pasFile) then begin
			var idx := modifiedRelatedFiles.AddIfNotContains(pasFile);
			dbgMsg.MsgIf(idx <> -1, 'add modified pas: ' + pasFile);
		end;
	end;

	Result := showSaveModifiedFilesQuestion(modifiedRelatedFiles, modifiedBuffers);
end;

class procedure IOTAFileUtils.prepareSaveModifiedFilesDialog(const _relatedFiles, _allModifiedFiles : TArrayEx<string>;
	out _buttons : TMessageDialogButtons; out _question, _caption, _details : string);
begin
	_buttons := [TMsgDlgBtn.mbYes, TMsgDlgBtn.mbYesToAll, TMsgDlgBtn.mbNo, TMsgDlgBtn.mbCancel];
	if _allModifiedFiles.Count = 1 then begin
		_buttons := [TMsgDlgBtn.mbYes, TMsgDlgBtn.mbNo, TMsgDlgBtn.mbCancel];
		_caption := 'Modified file found';
		if _relatedFiles.Count > 0 then begin
			_question := Format('The following file has unsaved changes:' + CRLF +
				{ } '%s' + CRLF2 + 'Do you want to save it now?', [_relatedFiles[0]]);
		end else begin
			_question := 'A file has unsaved changes.' + CRLF + 'Do you want to save it now?';
		end;
		_details := 'Modified file:' + CRLF + _allModifiedFiles[0];
	end else begin
		_caption := 'Multiple modified files found';
		if _relatedFiles.Count > 0 then begin
			_question := Format('The following related file has unsaved changes:' + CRLF +
				{ } '%s' + CRLF2 + 'Additionally, %d other file(s) have unsaved changes.' + CRLF2 +
				{ } 'Do you want to save them?', [_relatedFiles[0], _allModifiedFiles.Count - 1]);
		end else begin
			_question := Format('%d files have unsaved changes.' + CRLF2 +
				{ } 'Do you want to save them?', [_allModifiedFiles.Count]);
		end;
		_details := 'Modified files:' + CRLF + string.Join(CRLF, _allModifiedFiles.Items);
		if _relatedFiles.Count < _allModifiedFiles.Count then begin
			_details := _details + CRLF2 + 
			{ } 'Yes: save only related file(s): ' + string.Join(', ', _relatedFiles.Items) + CRLF2 +
			{ } 'Yes to All: save every modified file listed above';
		end else begin
			_details := _details + CRLF2 + 'Yes / Yes to All: save all listed files';
		end;
	end;
end;

class function IOTAFileUtils.showSaveModifiedFilesQuestion(const _relatedFiles : TArrayEx<string>;
	const _allModifiedFiles : TArrayEx<string>) : ESaveModifiedFilesResults;
var
	btnArr : TMessageDialogButtons;
	files : TArrayEx<string>;
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('IOTAFileUtils.showSaveModifiedFilesQuestion');
	Result := smfrNotSet;
	if (_relatedFiles.Count > 0) and (_allModifiedFiles.Count > 0) then begin
		var
			mainQuestion, caption, details : string;
		prepareSaveModifiedFilesDialog(_relatedFiles, _allModifiedFiles, btnArr, mainQuestion, caption, details);
		dbgMsg.Msg('File(s) modified (related): ' + string.Join(', ', _relatedFiles.Items));
		var
		res := TMsgBox.ShowQuestion(mainQuestion, caption, btnArr, 'Details', details);
		case res of
			mrYes : begin
				if _relatedFiles.IsEmpty then begin
					files := _allModifiedFiles;
				end else begin
					files := _relatedFiles;
				end;
				for var filePath in files do begin
					dbgMsg.Msg('Saving file: ' + filePath);
					if not IOTAUtils.SaveFile(filePath) then begin
						TMsgBox.ShowError(filePath + CRLF + 'couldn''t be saved.');
						Result := smfrError;
						Exit;
					end;
					Result := smfrActSaved;
				end;
			end;
			mrYesToAll : begin
				for var filePath in _allModifiedFiles do begin
					dbgMsg.Msg('Saving file: ' + filePath);
					if not IOTAUtils.SaveFile(filePath) then begin
						TMsgBox.ShowError(filePath + CRLF + 'couldn''t be saved.');
						Result := smfrError;
						Exit;
					end;
				end;
				Result := smfrAllSaved;
			end;
			mrNo : begin
				Result := smfrActNotSaved;
			end;
			mrCancel : begin
				Result := smfrCancel;
			end;
		end;
	end;
end;

end.
