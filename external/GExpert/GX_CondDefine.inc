{ GX_CondDefine.inc }

// Third-Party Product Defines
// --------------------------------------------------------------------------

// Define SYNEDIT below if you want to compile with the unicode SynEdit
// syntax highlighting text editor component.  It is required to use the
// Source Export tool and can optionally be used in several other places
// to show syntax highlighted code.  Note that an appropriately modified
// version of this component is included with the GExperts source.
// Delphi 2007 and earlier unconditionally require parts of this library to
// support TUnicodeStringList.  Home Page: http://mh-nexus.de/en/unisynedit.php

// Define GX_DEBUGLOG below to enable exception and SendDebug logging
// to <ModuleName>.debuglog.  This requires JCL 1.102 or later from
// http://sourceforge.net/projects/jcl and a detailed map file
// (see the linker options in the IDE).  Use sparingly, since this
// option will slow down the IDE.

{$IFNDEF GX_AutomatedBuild}
  // Turn things on we want compiled into GExperts
  // note: SynEdit is now always required
  {$DEFINE SYNEDIT}
  {.$DEFINE GX_DEBUGLOG}
{$ENDIF GX_AutomatedBuild}

// The use of Andy Hausladen's VCLFixPack is not yet tested well enough and
// might have undesirable side effects when used in an IDE plugin. Therefore
// it is disabled by default and must be enabled explicitly by setting the
// following conditional define:
{.$DEFINE GX_USE_VCL_FIX_PACK}

// Miscellaneous GExperts Defines (do not change the items below!)
// ---------------------------------------------------------------

{$IFDEF CONDITIONALEXPRESSIONS}
  {$IFDEF BCB}
    {$DEFINE GX_BCB}
  {$ELSE}
    {$DEFINE GX_Delphi}
  {$ENDIF}

  {$IF CompilerVersion >= 14}
    {$DEFINE GX_VER140_up} // Delphi 6
    {$DEFINE GX_DELPHI6_UP}
    {$IF CompilerVersion >= 15}
      {$DEFINE GX_VER150_up}  // Delphi 7
      {$DEFINE GX_DELPHI7_UP}
      {$IF CompilerVersion >= 16}
        {$DEFINE GX_VER160_up}  // Delphi 8 (BDS 1)
        {$DEFINE GX_DELPHI8_UP}
        {$IF CompilerVersion >= 17}
          {$DEFINE GX_VER170_up}  // Delphi 9/2005 (BDS 2)
          {$DEFINE GX_DELPHI2005_UP}
          {$IF CompilerVersion >= 18}
            {$DEFINE GX_VER180_up}  // BDS 2006 (10; BDS 3)
            {$DEFINE GX_DELPHI2006_UP}
            {$IFDEF VER185}
              {$DEFINE GX_VER185_up}  // Delphi 2007 (11; BDS 4)
              {$DEFINE GX_DELPHI2007_UP}
            {$ENDIF}
            {$IF CompilerVersion >= 19}
              {$DEFINE GX_VER185_up}  // Delphi 2007 (11; BDS 4)
              {$DEFINE GX_DELPHI2007_UP}
              {$DEFINE GX_VER190_up}  // Delphi .NET 2007 (11; BDS 5)
              {$IF CompilerVersion >= 20}
                {$DEFINE GX_VER200_up}  // RAD Studio 2009 (12; BDS 6)
                {$DEFINE GX_DELPHI2009_UP}
                {$IF CompilerVersion >= 21}
                  {$DEFINE GX_VER210_up}  // RAD Studio 2010 (14; BDS 7)
                  {$DEFINE GX_DELPHI2010_UP}
                  {$IF CompilerVersion >= 22}
                    {$DEFINE GX_VER220_up}  // RAD Studio XE 1 (15; BDS 8)
                    {$DEFINE GX_DELPHIXE_UP}
                    {$IF CompilerVersion >= 23}
                      {$DEFINE GX_VER230_up}  // RAD Studio XE 2 (16; BDS 9)
                      {$DEFINE GX_DELPHIXE2_UP}
                      {$IF CompilerVersion >= 24}
                        {$DEFINE GX_VER240_up}  // RAD Studio XE 3 (17; BDS 10)
                        {$DEFINE GX_DELPHIXE3_UP}
                        {$IF CompilerVersion >= 25}
                          {$DEFINE GX_VER250_up}  // RAD Studio XE 4 (18; BDS 11)
                          {$DEFINE GX_DELPHIXE4_UP}
                          {$IF CompilerVersion >= 26}
                            {$DEFINE GX_VER260_up}  // RAD Studio XE 5 (19; BDS 12)
                            {$DEFINE GX_DELPHIXE5_UP}
                            {$IF CompilerVersion >= 27}
                              {$DEFINE GX_VER270_up}  // RAD Studio XE 6 (20; BDS 14)
                              {$DEFINE GX_DELPHIXE6_UP}
                              {$IF CompilerVersion >= 28}
                                {$DEFINE GX_VER280_up}  // RAD Studio XE 7 (21; BDS 15)
                                {$DEFINE GX_DELPHIXE7_UP}
                                {$IF CompilerVersion >= 29}
                                  {$DEFINE GX_VER290_up}  // RAD Studio XE 8 (22; BDS 16)
                                  {$DEFINE GX_DELPHIXE8_UP}
                                  {$IF CompilerVersion >= 30}
                                    {$DEFINE GX_VER300_up}  // RAD Studio 10 Seattle (23; BDS 17)
                                    {$DEFINE GX_DELPHI_SEATTLE_UP}
                                    {$DEFINE GX_DELPHI10_UP}
                                    {$IF CompilerVersion >= 31}
                                      {$DEFINE GX_VER310_up}  // RAD Studio 10.1 Berlin (24; BDS 18)
                                      {$DEFINE GX_DELPHI_BERLIN_UP}
                                      {$DEFINE GX_DELPHI10_1_UP}
                                      {$IF CompilerVersion >= 32}
                                        {$DEFINE GX_VER320_up}  // RAD Studio 10.2 Tokyo (25; BDS 19)
                                        {$DEFINE GX_DELPHI_TOKYO_UP}
                                          {$DEFINE GX_DELPHI10_2_UP}
                                        {$IF CompilerVersion >= 33}
                                          {$DEFINE GX_VER330_up}  // RAD Studio 10.3 Rio (26; BDS 20)
                                          {$DEFINE GX_DELPHI_RIO_UP}
                                          {$DEFINE GX_DELPHI10_3_UP}
                                          {$IF CompilerVersion >= 34}
                                            {$DEFINE GX_VER340_up}  // RAD Studio 10.4 Sydney (27; BDS 21)
                                            {$DEFINE GX_DELPHI_SYDNEY_UP}
                                            {$DEFINE GX_DELPHI10_4_UP}
                                            {$IF CompilerVersion >= 35}
                                              {$DEFINE GX_VER350_up}  // RAD Studio 11 Alexandria (28; BDS 22)
                                              {$DEFINE GX_DELPHI11_UP}
                                              {$IF CompilerVersion >= 36}
                                                {$DEFINE GX_VER360_up}  // RAD Studio 12 Athens (29; BDS 23)
                                                {$DEFINE GX_DELPHI12_UP}
                                                {$IF CompilerVersion >= 37}
    unsupported Delphi version
    If you get here, you are probably trying to compile GExperts with a new Delphi release.
    In addition to define a new GX_VERxxx_UP symbol, you should also have a look at
    * GX_VerDepConst.pas
    * GX_GetIdeVersion.pas
    * GX_StandAloneLoadDLL.pas
    * GX_eIfDef.pas
    and also the include files for the 3rd party libraries used:
    * AbDefine.Inc (usually not necessary)
    * SynEdit.inc
    * SynEditJedi.inc (That is a copy of jedi.inc from Project JEDI)
    * dzlibjedi.Inc (That is a copy of jedi.inc from Project JEDI)
    * regexpr_compilers.inc
    todo: List the other units that might need to be adapted.
                                                {$IFEND}
                                              {$IFEND}
                                            {$IFEND}
                                          {$IFEND}
                                        {$IFEND}
                                      {$IFEND}
                                    {$IFEND}
                                  {$IFEND}
                                {$IFEND}
                              {$IFEND}
                            {$IFEND}
                          {$IFEND}
                        {$IFEND}
                      {$IFEND}
                    {$IFEND}
                  {$IFEND}
                {$IFEND}
              {$IFEND}
            {$IFEND}
          {$IFEND}
        {$IFEND}
      {$IFEND}
    {$IFEND}
  {$IFEND}
{$ELSE not CONDITIONALEXPRESSIONS}
  Sorry, but this version of GExperts does not support the IDE
  you are using. Please visit the GExperts web site at
  http://www.gexperts.org/ to obtain more information about
  support for your IDE.
{$ENDIF}

{$IFDEF LINUX}
Kylix is no longer supported.  You can try compiling with the GExperts source from early 2009.
{$ENDIF}

// Turn on IDE docking support
{$DEFINE GX_EnableIdeDockingSupport}

{$WARN SYMBOL_PLATFORM OFF}
{$WARN UNIT_PLATFORM OFF}

// All IDEs support this feature to some degree (though Delphi 8+ do not support multiline editor tabs)
{$DEFINE GX_EditorEnhancements}

{$IFDEF GX_DELPHI2005_UP}
  {$DEFINE GX_SUPPORTS_FOLDING}
  {. inlining in Delphi 2005 wasn't stable enough to use $DEFINE GX_SupportsInline}
{$ENDIF GX_DELPHI2005_UP}

{$IFDEF GX_DELPHI2006_UP}
  {$DEFINE GX_SupportsInline}
  {$DEFINE GX_HAS_ALIGN_WITH_MARGINS}
  {$DEFINE GX_HAS_STRICT_DELIMITER}
{$ENDIF GX_DELPHI2005_UP}

{$IFDEF GX_DELPHI10_UP}
  {$IFNDEF GX_DELPHI10_1_UP}
    {$DEFINE GX_TWMDPI_HAS_SCALLEDRECT_TYPO}
    {$DEFINE GX_HIDE_NAVBAR_NECESSARY}
  {$ENDIF GX_DELPHI10_1_UP}
{$ENDIF GX_DELPHI10_UP}


{$IFDEF GX_DELPHI10_2_UP}
  {$IFNDEF GX_DELPHI10_3_UP}
    // only for Delphi 10.2
    {$DEFINE GX_STARTUP_LAYOUT_FIX_ENABLED}
  {$ENDIF GX_DELPHI10_3_UP}
{$ENDIF GX_DELPHI10_2_UP}

{$IFDEF GX_DELPHI10_2_UP}
  // That problem still exists in Delphi 10.3 Rio, but let's hope it won't in later versions
  {$IFNDEF GX_DELPHI10_4_UP}
    {$DEFINE GX_SEARCH_PATH_REDRAW_FIX_ENABLED}
  {$ENDIF GX_DELPHI10_4_UP}
{$ENDIF GX_DELPHI10_2_UP}

{$IFDEF GX_DELPHI10_3_UP}
  // That problem was introduced in Delphi 10.3 Rio, and still exists in 10.4 Sydney.
  // Let's hope it will be fixed in later versions
  {$IFNDEF GX_DELPHI11_UP}
    {$DEFINE GX_KEYBOARD_SHORTCUT_BROKER_FIX_ENABLED}
  {$ENDIF GX_DELPHI10_4_UP}
  // Starting with Delphi 10.3 Rio, the popup menu for the editor window gets recreated
  // every time in its OnPopup event.
  {$DEFINE GX_EDITOR_POPUP_MENU_GETS_RECREATED}
{$ENDIF GX_DELPHI10_3_UP}

{$IFDEF GX_DELPHI10_2_UP}
  {$IFNDEF GX_DELPHI10_4_UP
    // Assigninig icons has redraw problems in the early themed IDEs (Delphi 10.2 and 10.3)
    // This was fixed in Delphi 10.4.
    {$DEFINE GX_ICONS_IN_POPUP_MENUS_ARE_BROKEN}
    // possibly earlier
    {$DEFINE GX_HAS_UNIT_REGULAREXPRESSIONS}
  {$ENDIF}
{$ENDIF GX_DELPHI10_2_UP}

{$IFDEF GX_DELPHI10_3_UP}
  // Fix for https://quality.embarcadero.com/browse/RSP-28064
  {$IFNDEF GX_DELPHI10_4_UP} // Fixed in 10.4
    {$DEFINE GX_LISTBOX_OWNERDRAW_FIX_ENABLED}
  {$ENDIF GX_DELPHI10_4_UP}
{$ENDIF GX_DELPHI10_3_UP}

{$IFDEF GX_DELPHI2010_UP}
  {$DEFINE GX_CUSTOMINIFILE_HAS_READSUBSECTIONS}
{$ENDIF}

{$IFDEF GX_DELPHI11_UP}
  {$DEFINE GX_IDE_IS_HIDPI_AWARE}
  {$IFNDEF GX_DELPHI_12_UP}
    // we hope this gets fixed in Delphi 12 and later ...
    {$DEFINE GX_WRONG_BUTTON_PARENTS_IN_PATH_EDIT_FIX_ENABLED}
    {$DEFINE GX_AV_IN_CODE_FORMATTER_FIX_ENABLED}
    {$DEFINE GX_ACTION_BROKER_IMAGELIST_FIX_ENABLED}
  {$ENDIF}
{$ENDIF}
{.$DEFINE GX_IDE_IS_HIDPI_AWARE}

{$IFDEF GX_DELPHI12_UP}
// Fix for bug #393 GExperts key mappings is removed when I unload/install another "ToolsApi" package
// todo: Try to figure out which Delphi versions had this problem. It wasn't just Delphi 12
{$DEFINE GX_KEYBOARD_BINDING_FIX}
{$ENDIF}

// Define UniSynEdit if SynEdit is available
{$IFDEF SYNEDIT}
  {$DEFINE UniSynEdit}
  {$DEFINE GX_ENHANCED_EDITOR}
{$ELSE}
  GExperts now requires SynEdit.
{$ENDIF SYNEDIT}

{$IFOPT B+}
  GExperts will not work well with "Complete Boolean Evaluation" on.
  Please turn it off in the Project Options dialog.
{$ENDIF}

{$IFOPT C-}
  GExperts does not work well with assertions turned off.
  Please turn them on in the Project Options dialog.
{$ENDIF}

