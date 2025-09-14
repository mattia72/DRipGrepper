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

		if modifiedBuffers.Contains(dfmFile) then begin
			modifiedRelatedFiles.AddIfNotContains(dfmFile);
			dbgMsg.Msg('add modified dfm: ' + dfmFile);
		end;

		if modifiedBuffers.Contains(pasFile) then begin
			modifiedRelatedFiles.AddIfNotContains(pasFile);
			dbgMsg.Msg('add modified pas: ' + pasFile);
		end;
	end;

	Result := showSaveModifiedFilesQuestion(modifiedRelatedFiles, modifiedBuffers);
end;

class procedure IOTAFileUtils.prepareSaveModifiedFilesDialog(const _relatedFiles, _allModifiedFiles : TArrayEx<string>;
	out _buttons : TMessageDialogButtons; out _question, _caption, _details : string);
var
	pluralAll : Boolean;
begin
	_buttons := [TMsgDlgBtn.mbYes, TMsgDlgBtn.mbYesToAll, TMsgDlgBtn.mbNo, TMsgDlgBtn.mbCancel];
	pluralAll := _allModifiedFiles.Count <> 1;
	if _allModifiedFiles.Count = 1 then begin
		_buttons := [TMsgDlgBtn.mbYes, TMsgDlgBtn.mbNo, TMsgDlgBtn.mbCancel];
		_caption := 'Modified file found';
		if _relatedFiles.Count > 0 then begin
			_question := Format('Related file has unsaved changes:' + CRLF +
				{ } '%s' + CRLF + 'Save now?', [_relatedFiles[0]]);
		end else begin
			_question := 'Related file has unsaved changes.' + CRLF + 'Save now?';
		end;
		_details := 'Modified file:' + CRLF + _allModifiedFiles[0];
	end else begin
		_caption := 'Modified files found';
		_question := Format('There are %d modified %s. Save now?', [_allModifiedFiles.Count, IfThen(pluralAll, 'files', 'file')]);
		_details := 'Modified files:' + CRLF + string.Join(CRLF, _allModifiedFiles.Items);
		if _relatedFiles.Count < _allModifiedFiles.Count then begin
			_details := _details + CRLF + CRLF +
			{ } 'Yes: save only related file(s): ' + string.Join(', ', _relatedFiles.Items) + CRLF +
			{ } CRLF +
			{ } 'Yes to All: save every modified file listed above';
		end else begin
			_details := _details + CRLF + CRLF + 'Yes / Yes to All: save all listed files';
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
	Result := smfrNothingSaved;
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
						TMsgBox.ShowError(filePath + CRLF + 'couldn''t be saved, opening aborted.');
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
						TMsgBox.ShowError(filePath + CRLF + 'couldn''t be saved, opening aborted.');
						Result := smfrError;
						Exit;
					end;
				end;
				Result := smfrAllSaved;
			end;
			mrNo : begin
				// nothing to do...
			end;
			mrCancel : begin
				Result := smfrCancel;
			end;
		end;
	end;
end;

end.
