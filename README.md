
---

# OCaml B-Tree Functor

Diese Implementierung bietet einen funktionalen, selbstbalancierenden B-Baum mit einstellbarem Grad $k$.

## Installation & Ausführung

1. **Voraussetzung:** OCaml und Dune müssen installiert sein.
2. **Dateien anlegen:** Speichere den Code als `main.ml`. Erstelle im selben Ordner eine Datei namens `dune`:
   ```lisp
   (executable (name main))
   ```
3. **Starten:**
   ```bash
   dune exec ./main.exe
   ```

## Details zur Implementierung

### Struktur
- **Funktor-basiert:** `MakeBTree` erwartet ein Modul mit Typ `t` und einer `compare`-Funktion.
- **Parametrisierbar:** Der Grad $k$ wird bei `init_tree k` festgelegt. Ein Knoten fasst maximal $2k$ Elemente.
- **Unveränderlich:** Der Baum ist rein funktional; jede Operation gibt einen neuen Baum zurück.

### Algorithmus (Einfügen)
Die Kernlogik liegt in der rekursiven Funktion `insert_aux`:
- **`insert_res` Typ:** Steuert das Backtracking. Ein Knoten gibt entweder `Stay` (keine Änderung der Struktur oberhalb) oder `Split` (Median und zwei neue Teilbäume müssen nach oben gereicht werden) zurück.
- **Balancierung:** Überschreitet eine Liste die Länge $2k$, wird sie mittels `split_idx` am Median geteilt.
- **Wurzel-Split:** Falls die Wurzel geteilt wird, wird in der `insert`-Funktion automatisch eine neue Ebene (neue Wurzel) erstellt, wodurch der Baum wächst.

### Wichtige Funktionen
- `lookup / lookup_value`: Logarithmische Suche nach Schlüsseln.
- `insert`: Fügt Elemente sortiert ein; verhindert Duplikate per `failwith`.
- `dune clean`: Bereinigt Build-Artefakte vor dem Teilen des Projekts.