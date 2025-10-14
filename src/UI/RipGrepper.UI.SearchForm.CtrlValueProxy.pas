unit RipGrepper.UI.SearchForm.CtrlValueProxy;

interface

uses
	ArrayEx,
	Spring.Collections,
	RipGrepper.Common.Interfaces,
	RipGrepper.Common.SimpleTypes,
	RipGrepper.Common.IDEContextValues;

type
	TSearchFormCtrlValueProxy = record
		private
			FSearchText : string;
			function GetSearchText() : string;
			procedure SetSearchText(const Value : string);

		public
			SearchTextHist : TArrayEx<string>;
			SearchOptions : TSearchOptionSet;
			IsReplaceMode : Boolean;
			ReplaceText : string;
			ReplaceTextHist : TArrayEx<string>;
			ExtensionContext : EDelphiIDESearchContext;
			SearchPath : string;
			SearchPathHist : TArrayEx<string>;
			FileMasks : string;
			FileMasksHist : TArrayEx<string>;
			IsHiddenChecked : Boolean;
			IsNoIgnoreChecked : Boolean;
			Encoding : string;
			EncodingItems : TArrayEx<string>;
			IsPrettyChecked : Boolean;
			LineContext : Integer;
			OutputFormat : string;
			OutputFormatItems : TArrayEx<string>;
			AdditionalExpertOptions : string;
			AdditionalExpertOptionsHist : TArrayEx<string>;

			function IsEmpty() : Boolean;
			function ToString() : string;
			property SearchText : string read GetSearchText write SetSearchText;
	end;

implementation

uses
	System.SysUtils;

function TSearchFormCtrlValueProxy.IsEmpty() : Boolean;
begin
	Result := (SearchText = '') and (ReplaceText = '') and (SearchPath = '') and (FileMasks = '') and (Encoding = '') and (OutputFormat = '') and (LineContext = 0);
end;

function TSearchFormCtrlValueProxy.ToString() : string;
begin
	Result := Format('SearchText: %s, ReplaceText: %s, SearchPath: %s, FileMasks: %s', [SearchText, ReplaceText, SearchPath, FileMasks]);
end;

function TSearchFormCtrlValueProxy.GetSearchText() : string;
begin
	Result := FSearchText;
end;

procedure TSearchFormCtrlValueProxy.SetSearchText(const Value : string);
begin
	FSearchText := Value;
end;

end.
