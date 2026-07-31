# Implementierungsplan: Matching Line Hints mit Kontext

## Ziel

Es wird ein neues Setting `FShowLineHint` eingefuehrt (analog zu `FShowFileHint`),
aber als Integer.

- `<= 0`: Kein Line-Hint anzeigen.
- `> 0`: Hint fuer die Match-Zeile anzeigen, inklusive Kontextzeilen um den Treffer.

Die Integer-Zahl steuert die Anzahl der Kontextzeilen vor und nach der Match-Zeile.

## Ausgangslage (kurz)

Aktuell existiert bereits `ShowFileHint` als Boolean in den App-Settings.
In der Result-Ansicht werden Hints fuer File- und Match-Nodes angezeigt,
aber es gibt keine einstellbare Kontextdarstellung fuer Match-Zeilen.

## Scope

- [src/Settings/RipGrepper.Settings.AppSettings.pas](src/Settings/RipGrepper.Settings.AppSettings.pas)
- [src/UI/RipGrepper.UI.MiddleFrame.pas](src/UI/RipGrepper.UI.MiddleFrame.pas)
- [src/UI/RipGrepper.UI.HintBuilder.pas](src/UI/RipGrepper.UI.HintBuilder.pas)
- Optional UI fuer Konfiguration:
  - [src/UI/RipGrepper.UI.Settings.ColorSettingsForm.pas](src/UI/RipGrepper.UI.Settings.ColorSettingsForm.pas)
  - [src/UI/RipGrepper.UI.Settings.ColorSettingsForm.dfm](src/UI/RipGrepper.UI.Settings.ColorSettingsForm.dfm)

## Nicht-Ziele

- Keine Aenderung am Suchalgorithmus oder rg-Parametern.
- Kein Umbau der bestehenden File-Hint-Logik.
- Keine Abhaengigkeit von --context Ausgabe aus rg (Kontext wird fuer Hint dynamisch aus Datei gelesen).

## Implementierungsschritte

1. Neues App-Setting `ShowLineHint` als Integer einfuehren
- In `TAppSettings`:
  - `KEY_SHOW_LINE_HINT = 'ShowLineHint'`
  - Feld `FShowLineHint : IIntegerSetting`
  - Getter/Setter `GetShowLineHint`, `SetShowLineHint`
  - Property `ShowLineHint : Integer`
- In `Init` default setzen (Vorschlag: `0` fuer deaktiviert, alternativ `2` fuer direkt sichtbaren Kontext).
- In `CreateSetting(...)` aufnehmen.

2. Hint-Builder um Match-Kontext erweitern
- In `TFileHintBuilder` neue Methode ergaenzen, z. B.:
  - `BuildMatchNodeHintWithContext(const _nodeData : PVSFileNodeData; const _contextLines : Integer) : string`
- Verhalten:
  - Bei `_contextLines <= 0`: leer oder Fallback auf bestehendes Match-Hint ohne Kontext.
  - Bei gueltiger Datei + gueltiger Zeilennummer:
    - Dateiinhalt lesen.
    - Bereich `row - contextLines` bis `row + contextLines` bestimmen.
    - Match-Zeile klar markieren (z. B. Prefix `>`).
    - Zeilennummern in Hint aufnehmen.
  - Bei Fehlern (Datei fehlt, Zeile ausserhalb): robust auf bestehendes Match-Hint zurueckfallen.

3. Verwendung in Result-Hints integrieren
- In `VstResultGetHint` (MiddleFrame):
  - Match-Node + relevante Spalten (`COL_FILE`, `COL_ROW_NUM`, `COL_COL_NUM`, optional `COL_MATCH_TEXT`) umstellen auf neues Verhalten.
  - `Settings.AppSettings.ShowLineHint` auslesen:
    - `<= 0` => kein Line-Hint.
    - `> 0` => Kontext-Hint ueber Builder anzeigen.
- Vorhandenes `ShowFileHint` Verhalten nicht regressiv aendern.

4. Settings-UI erweitern (optional, aber empfohlen)
- In der Settings-Form statt/zusatzlich Checkbox einen Integer-Wert konfigurierbar machen:
  - z. B. `TSpinEdit` fuer Kontextzeilen (`0..N`).
- Beim Lesen/Schreiben der Settings mit `FAppSettings.ShowLineHint` synchronisieren.
- Tooltip erklaeren:
  - `0 = aus`, `1 = nur direkte Nachbarzeilen`, `2+ = mehr Kontext`.

5. Defensive Regeln und Performance
- Nur bei Hover laden (kein Preload fuer alle Nodes).
- Optional kleiner Cache pro Datei (spaeter, wenn noetig).
- Bei sehr grossen Dateien max. Kontextbereich strikt begrenzen.

## Akzeptanzkriterien

1. Bei `ShowLineHint <= 0` wird kein Match-Line-Kontext-Hint angezeigt.
2. Bei `ShowLineHint = 1` zeigt der Hint die Match-Zeile plus je eine Zeile davor/danach (falls vorhanden).
3. Bei `ShowLineHint = N` zeigt der Hint maximal `2N + 1` Zeilen mit markierter Match-Zeile.
4. Fehlende Datei oder ungueltige Zeilennummer fuehrt nicht zu Exception, sondern zu sauberem Fallback.
5. Bestehende `ShowFileHint` Funktion bleibt unveraendert nutzbar.

## Manueller Testplan

1. Setting aus
- `ShowLineHint = 0` setzen.
- Auf Match-Nodes hovern.
- Erwartung: kein Kontext-Hint fuer Match-Zeile.

2. Kleiner Kontext
- `ShowLineHint = 1` setzen.
- Auf Match-Nodes in der Mitte einer Datei hovern.
- Erwartung: 3 Zeilen sichtbar (vorher, match, nachher), Match-Zeile markiert.

3. Groesserer Kontext
- `ShowLineHint = 3` setzen.
- Erwartung: bis zu 7 Zeilen sichtbar, inklusive Zeilennummern.

4. Randfaelle
- Treffer in erster/letzter Datei-Zeile testen.
- Datei verschieben/loeschen und erneut hovern.
- Erwartung: kein Crash, vernuenftiger Fallback-Text.

5. Regression
- `ShowFileHint` an/aus testen.
- File-Node-Hints und Match-Node-Hints weiterhin korrekt.

## Risiken und Gegenmassnahmen

1. Risiko: Hover wird traege bei grossen Dateien
- Gegenmassnahme: Kontextzeilen klein halten, spaeter optional Datei-Cache.

2. Risiko: Encoding-Probleme beim Dateilesen
- Gegenmassnahme: bestehende robuste Reader/Helfer wiederverwenden, bei Fehler fallback.

3. Risiko: Verwechslung `ShowFileHint` vs. `ShowLineHint`
- Gegenmassnahme: klare Benennung, klare Tooltips in Settings-UI.

## Deliverables

1. Neues Integer-Setting `ShowLineHint` in App-Settings.
2. Match-Hint mit konfigurierbarem Zeilenkontext.
3. (Optional) UI-Element zur Konfiguration in den Settings.
4. Kurztest nach obigem Testplan dokumentiert.
