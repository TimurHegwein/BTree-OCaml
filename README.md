
---

# OCaml B-Tree Implementation

Dieses Repositorium enthält eine rein funktionale, selbstbalancierende Implementierung eines **B-Baums** in OCaml. Die gesamte Logik ist generisch über Funktoren realisiert.

## 📁 Projektstruktur

Obwohl das Projekt die Standard-Dune-Struktur nutzt, ist die Implementierung aktuell kompakt gehalten:

- **`bin/main.ml`**: Enthält die komplette Implementierung (Funktor `MakeBTree`, Typdefinitionen und Logik) sowie die Demo-Anwendung.
- **`bin/dune`**: Konfigurationsdatei für das Kompilat mit dem öffentlichen Namen `btree`.
- **`lib/` & `test/`**: Aktuell als Platzhalter für zukünftige Refactorings und Unit-Tests vorgesehen.

## 🚀 Installation & Ausführung

Stelle sicher, dass [OCaml](https://ocaml.org/) und das Build-System [Dune](https://dune.build/) installiert sind (am einfachsten via `opam`).

1. **Repository klonen:**
   ```bash
   git clone https://github.com/TimurHegwein/BTree-OCaml.git
   cd BTree-OCaml
   ```

2. **Kompilieren und ausführen:**
   Das Programm kann plattformübergreifend (macOS, Linux, Windows) über seinen öffentlichen Namen gestartet werden:
   ```bash
   dune exec btree
   ```

3. **Aufräumen:**
   Um die Build-Artefakte (den `_build`-Ordner) zu entfernen:
   ```bash
   dune clean
   ```

## 🛠 Details zur Implementierung

### Funktoren & Generik
Die Implementierung nutzt OCaml-Funktoren, um den Baum für beliebige Datentypen nutzbar zu machen. Voraussetzung ist lediglich ein Modul, das die `OrderedType`-Signatur (Typ `t` und eine `compare`-Funktion) erfüllt.

### Algorithmus (Einfügen & Balancierung)
- **Status-basiertes Backtracking:** Die Funktion `insert_aux` nutzt den Typ `insert_res` (`Stay` oder `Split`), um während der Rekursion zu signalisieren, ob ein Knoten geteilt werden muss.
- **Selbstbalancierend:** Erreicht ein Knoten mehr als $2k$ Elemente, wird er am Median gesplittet. Der Baum wächst bei Bedarf an der Wurzel nach oben.
- **Immutability:** Die Datenstruktur ist persistent. Jede Operation gibt einen neuen Baum zurück; der ursprüngliche Zustand bleibt erhalten.

### Wichtige Funktionen
- `lookup` / `lookup_value`: Suche mit $O(\log n)$.
- `insert`: Sortiertes Einfügen (wirft `failwith` bei Duplikaten).

## 📝 Demo
Die integrierte Demo erstellt einen Baum vom Grad $k=4$, befüllt ihn mit 50 zufälligen Studenten-Datensätzen und führt beispielhafte Suchanfragen durch.

---
*Hinweis: Da die Implementierung derzeit monolithisch in `main.ml` vorliegt, ist sie besonders einfach zu lesen und nachzuvollziehen.*
---
