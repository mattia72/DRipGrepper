# Implementierungsplan: cmbSearchDir History pro IDE-Kontext

## Ziel

Das Problem mit ungueltigen Search-Path-Eintraegen wie
"85 paths (e.g. ...)" wird nachhaltig behoben.

Dazu wird die Search-Path-History pro IDE-Kontext getrennt gehalten.
Nur die History fuer Custom Location wird persistent in Settings gespeichert.
IDE-Kontext-Historien bleiben nur in der Session.

## Ausgangsproblem (kurz)

1. Gekuerzter Anzeige-Text kann versehentlich als Search-Pfad gespeichert werden.
2. Eine gemeinsame SearchPathHist vermischt IDE-Kontexte und Custom Location.
3. Beim Kontextwechsel erscheinen dadurch irrelevante oder ungueltige Items im Dropdown.

## Scope

- Betroffene Hauptdatei:
  - src/UI/RipGrepper.UI.SearchForm.pas
- Optional betroffen:
  - src/UI/RipGrepper.UI.SearchForm.CtrlValueProxy.pas (nur falls Signaturen angepasst werden)

## Nicht-Ziele

- Kein Persistieren von IDE-Kontextpfaden in INI/Settings.
- Keine Aenderung an allgemeiner Search-Text-, Replace- oder FileMask-History.

## Implementierungsschritte

1. Neue Form-Felder und Hilfsmethoden einfuehren
- Neues Feld auf der Form:
  - FSearchPathHistByContext: Dictionary<EDelphiIDESearchContext, TArrayEx<string>>
- Methoden ergaenzen:
  - GetSearchPathHistForContext(_context)
  - SetSearchPathHistForContext(_context, _items)
  - StoreCurrentPathToContextHistory()
- Ziel:
  - Pro Kontext getrennte in-memory History verwalten.

2. Initiales Seeding der Historie (Fertig)
- In LoadInitialSearchSettings (oder direkt danach) die Dictionary-History vorbereiten.
- Custom-Context mit persistierter SearchPathsHistory aus Settings initial fuellen.
- IDE-Kontexte leer starten (werden durch Context-Values zur Laufzeit befuellt).

3. Speichern der History korrigieren (Kernfix) (Fertig)
- StoreCmbHistorieItems anpassen:
  - Immer in Context-Dictionary speichern (StoreCurrentPathToContextHistory).
  - Persistente SearchPathHist nur fuer dicCustomLocation aktualisieren.
  - Bei nicht-Custom-Kontexten niemals den gekuerzten Display-Text in persistente History schreiben.
- Validierungsregel:
  - Wenn cmbSearchDir disabled ist, nur den echten Pfad (FContextSearchPath) verarbeiten.

4. Laden/Anzeigen der Items kontextabhaengig machen (Fertig) 
- WriteCtrlProxyToCtrls anpassen:
  - cmbSearchDir.Items nicht mehr pauschal aus SearchPathHist fuellen.
  - Stattdessen: context-spezifische Items aus Dictionary.
  - Für dicCustomLocation: aus Custom-History.
  - Für IDE-Kontexte: aus jeweiliger Context-History.

5. Kontextwechsel robust machen (Fertig)
- UpdateCmbsOnIDEContextChange anpassen:
  - Vor Wechsel: aktuellen Pfad in bisherigem Kontext speichern.
  - Nach Wechsel: Text + Items aus neuem Kontext laden.
  - Bei dicCustomLocation: nur Custom-History anzeigen.
- Sicherstellen, dass FContextSearchPath beim Kontextwechsel korrekt gesetzt/geleert wird.

6. Persistenz beim Uebertragen in Settings bereinigen (Fertig)
- CopyProxyToSettings anpassen:
  - SearchPathsHistory in Settings nur aus dicCustomLocation schreiben.
  - Keine IDE-Kontextpfade in persistente SearchPathsHistory uebernehmen.

7. Absicherung gegen Anzeige-Label als Pfad
- SetCmbSearchPathText pruefen:
  - FContextSearchPath muss immer den vollen Rohpfad enthalten.
  - Gekuerzter Text bleibt rein visuell (nur Combo.Text/Hint).
- In den Store-Pfaden eine Guard einbauen:
  - Wenn Text dem Muster "<n> paths (e.g. ...)" entspricht, nie als realen Pfad persistieren.

## Akzeptanzkriterien

1. Nach Nutzung von Project library paths entstehen keine Eintraege wie
   "85 paths (e.g. ...)" in der persistenten Custom-History.
2. Beim Wechsel auf Custom Location werden nur userrelevante Custom-Items angezeigt.
3. IDE-Kontextwechsel zeigt nur kontextpassende Pfade und keine vermischten History-Eintraege.
4. Nach App-Neustart ist nur Custom-History persistent vorhanden.
5. SearchPath in den Suchparametern bleibt technisch korrekt (volle Pfade, keine Anzeige-Labels).

## Testplan (manuell)

1. Basisfall Custom
- Auf dicCustomLocation wechseln.
- 2-3 echte Pfade suchen.
- Dialog schliessen/oeffnen.
- Erwartung: diese Pfade sind in Custom-History sichtbar.

2. IDE-Kontext mit vielen Pfaden
- Auf dicProjectLibraryPath wechseln (lange, viele Pfade).
- Anzeige sollte verkuerzt sein (z. B. "85 paths (e.g. ...)").
- Suche ausfuehren.
- Erwartung: kein solcher Anzeige-String landet in persistenter Custom-History.

3. Kontextwechsel hin und zurueck
- Zwischen dicProjectFiles, dicActiveFile, dicCustomLocation mehrfach wechseln.
- Erwartung: pro Kontext sinnvolle, getrennte Dropdown-Items.

4. Neustart
- Anwendung/Extension neu starten.
- Erwartung: nur Custom-History persistent, IDE-Historien neu aus IDE-Kontext aufgebaut.

5. Regression
- Normale Suche, Replace-Suche, Expert-Mode einmal durchklicken.
- Erwartung: kein Nebeneffekt auf andere History-Felder.

## Risiken und Gegenmassnahmen

1. Risiko: Falscher Kontext beim Speichern
- Gegenmassnahme: Vor jedem Kontextwechsel explizit alten Kontext sichern.

2. Risiko: Leere FContextSearchPath-Werte
- Gegenmassnahme: Guard-Logik beim Store, Musterpruefung fuer Anzeige-Label.

3. Risiko: Unbeabsichtigte Persistenz von IDE-Pfaden
- Gegenmassnahme: Persistenz hart auf dicCustomLocation begrenzen.

## Rollout

1. Implementierung in kleiner, reviewbarer Aenderung in SearchForm.
2. Manueller Testplan komplett ausfuehren.
3. Falls vorhanden: relevante Unit-/GUI-Tests erweitern.
4. Danach normaler Build- und Unittest-Lauf gemaess Projektprozess.

## Deliverables

1. Code-Aenderung in SearchForm mit per-context SearchPath-History.
2. Aktualisierte, stabile Persistenzlogik fuer Custom-History.
3. Diese Dokumentation als Nachvollziehbarkeit fuer Review und Ticket-Kommentar.
