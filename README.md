Hier ist die überarbeitete `README.md`, optimiert für ein GitHub-Repository. Sie deckt den Prozess vom Klonen bis zur Ausführung ab und erklärt die technischen Konzepte.

---

# OCaml B-Tree Functor Implementation

Diese Repositorium enthält eine rein funktionale, selbstbalancierende Implementierung eines **B-Baums** in OCaml. Mithilfe von Funktoren kann die Datenstruktur für beliebige Datentypen verwendet werden.

## 🚀 Installation & Ausführung

Stelle sicher, dass [OCaml](https://ocaml.org/) und das Build-System [Dune](https://dune.build/) installiert sind (am einfachsten via `opam`).

1. **Repository klonen:**
   ```bash
   git clone https://github.com/TimurHegwein/BTree-OCaml 
   cd btree
   ```

2. **Kompilieren und ausführen:**
   Das Projekt enthält eine Demo-Anwendung in der `main.ml`, die automatisch über Dune gestartet werden kann:
   ```bash
   dune exec ./main.exe
   ```

3. **Aufräumen:**
   Um die Build-Artefakte zu entfernen:
   ```bash
   dune clean
   ```

## 🛠 Details zur Implementierung

### Struktur & Funktoren
- **Funktor-basiert:** Über `module MakeBTree (Ord : OrderedType)` wird der Baum instanziiert. Er benötigt lediglich einen Typ `t` und eine `compare`-Funktion.
- **Parametrisierbar:** Der Grad $k$ des Baums wird bei der Initialisierung (`init_tree k`) festgelegt. Ein Knoten fasst nach der B-Baum-Definition maximal $2k$ Elemente.
- **Immutability:** Die Implementierung ist rein funktional. Jede Operation lässt den bestehenden Baum unverändert und gibt eine neue, aktualisierte Struktur zurück.

### Algorithmus (Einfügen & Balancierung)
Die Kernlogik des Einfügens ist in `insert_aux` implementiert:
- **`insert_res` Typ:** Steuert das Backtracking während der Rekursion. Ein Knoten liefert entweder:
    - `Stay`: Das Element wurde eingefügt, die Invarianten sind gewahrt.
    - `Split`: Der Knoten ist übergelaufen. Der Median sowie die zwei neuen Teilbäume werden an die nächsthöhere Ebene gereicht.
- **Wurzel-Wachstum:** Falls die Wurzel geteilt werden muss (`Split`), wird in der Hauptfunktion `insert` eine neue Ebene oberhalb der alten Wurzel erstellt. Dies ist der einzige Mechanismus, durch den der B-Baum an Höhe gewinnt.

### Wichtige Funktionen
- `lookup` / `lookup_value`: Logarithmische Suche ($O(\log n)$).
- `insert`: Fügt Elemente sortiert ein. **Hinweis:** Duplikate führen zum Abbruch des Programms (`failwith`).
- `split_idx`: Hilfsfunktion zur exakten Teilung von Listen am Median während eines Knoten-Splits.

## 📝 Beispiel
Das mitgelieferte Beispiel in `run_main` zeigt die Verwendung mit einem Key-Value-Paar (Studenten-Matrikelnummern und Noten):

```ocaml
module IntKV = struct
  type t = { key : int; value : int }
  let compare a b = Int.compare a.key b.key
end

module IntKVBtree = MakeBTree(IntKV)
let tree = IntKVBtree.init_tree 4 (* Baum mit Grad k=4 *)
```
