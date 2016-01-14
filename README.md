Projekt zaliczeniowy z MRJP, 01.2016

Artur Kozak [320770], `artur.kozak@students.mimuw.edu.pl`

# latte-compiler

Kompilator języka [Latte](http://www.mimuw.edu.pl/~ben/Zajecia/Mrj2015/latte.html) do asemblera x86. Parsery i przetwarzanie drzewa składni zostało zrealizowane przy pomocy [ANTLR](www.antlr.org) w wersji 3.1. Całość logiki została napisana w Pythonie 2.7 (niestety, bilbioteki antlr3 nie obsługują pythona3, zaś w momencie rozpoczynania projektu antlr4 nie był jeszcze w pełni dostępny).

### Parametry wywołania
```sh
latc_x86 [-h] [-o BIN_FILE] [-s ASM_FILE] [-d] [-r RUNTIME_FILE] [-C]
                [-O OPTIMIZER_PASSES] [--optimizer-summary]
                input_file
```
Argument obowiązkowy: plik z kodem źródłowym. Podanie `'-'` powoduje wczytywanie kodu z wejścia standardowego oraz domyślnie ustawia `--asm-output -`.

Argumenty opcjonalne:
 * `-h, --help`: pokazanie pomocy
 * `-o, --output BIN_FILE`: zmiana generowanego pliku wykonywalnego (domyślnie jak plik źródłowy bez rozszerzenia `.lat`).
 * `-s, --asm-output ASM_FILE`: zmiana generowanego pliku z kodem asemblera (domyślnie jak plik źródłowy ze zmienionym `.lat` na `.s`). Podanie `'-'` powoduje wypisanie asemblera na wyjście standardowe i nie tworzy pliku wykonywalnego.
 * `-d, --debug`: włącza tryb debugowania -- wypisuje komunikaty diagnostyczne i pomija wymaganie żeby `'OK'`/`'ERROR'` było w pierwszym wierszu.
 * `-r, --runtime RUNTIME_FILE`: zmiana pliku z biblioteczką standardową (domyślnie `runtime.o`).
 * `-C, --no-color`: wymusza wyłączenie kolorowania wyjścia (domyślnie kolory są włączone, jeśli wyjście jest terminalem).
 * `-O, --optimizer-passes N`: zmienia maksymalną liczbę przebiegów pętli optymalizatora (domyślnie `2`).
 * `--optimizer-summary`: po zakończeniu optymalizacji wypisuje statystyki ile razy użyto którego rodzaju ulepszeń.

### Użyte biblioteki
W normalnym przypadku należałoby zainstalować ANTLR w systemie, a biblioteki pythonowe przy użyciu `pip` lub systemowego managera pakietów, jednak celem łatwej kompilacji na `students` załączyłem wszystko w podkatalogu `lib/`. Dodatkowo, w podkatalogu `utils/` załączam definicje kolorowania składni gramatyk ANTLR dla `vim`a.

 * `ANTLR 3.1.3` -- zestaw narzędzi do generacji parserów, wraz z zależnościami:
   * pakiet `stringtemplate` dla Javy (w  której jest napisany sam ANTLR)
   * pakiet `antlr` dla Javy -- są to biblioteki starego ANTLR 1/2, od których zależny jest w/w `stringtemplate`;
   * moduł `antlr3` dla Pythona -- runtime library dla pythonowych parserów wygenerowanych przez ANTLR, musi być zainstalowany w tej samej wersji co sam ANTLR;
   * moduł `stringtemplate3` dla Pythona -- analogicznie jak w Javie, wymagany przez bilbioteki `antlr3`;
   * moduł `antlr` dla Pythona -- analogicznie jak w Javie, moduł `stringtemplate3` jest zależny od bibliotek starych wersji ANTLR;
 * biblioteki dostępne standardowo w Pythonie, np. [`argparse`](https://docs.python.org/2.7/library/argparse.html) do przetwarzania parametrów wiersza poleceń.

### Opis modułów
Każdy plik źródłowy jest osobnym modułem programu, krótki opis każdego mniej więcej w kolejności działania:

 1. `FuturePrint.py`: funkcje pomocnicze do wypisywania komunikatów diagnostycznych, wiadomości, ostrzeżeń i błędów. Dodatkowo umożliwia wypisywanie sekwencji kolorów ANSI celem zwiększenia czytelności wyjścia programu.
 2. `LatteErrors.py`: definicje wyjątków używanych przez program oraz klasa `Status`, służąca do buforowania i wypisywania wiadomości wyjściowych.
 3. `Utils.py`: zawiera przydatne dodatki: klasę `Flags` służącą do przetwarzania argumentów wywołania i globalnego dostępu do nich oraz instrukcję `switch`, imitującą zachowanie takiej samej instrukcji z C -- a niezmiernie przydatną przy rozważaniu wielu przypadków.
 4. `LatteMain.py`: główna funkcja kompilatora, uruchamiająca kolejno wszystkie etapy budowania programu, od otwarcia pliku wejściowego do linkowania z biblioteczką standardową.
 5. `Latte.g`, z którego ANTLR generuje `LatteLexer.py` oraz `LatteParser.py`: definicja gramatyki Latte, równoważna z zadaną w pliku `Latte.cf`, ale sprowadzona do postaci `LL*` (gdyż takie parsery generuje ANTLR). Definiuje też kilka metod pomocniczych parsera -- w tym funkcję główną, dzięki której można uruchomić sam parser, uzyskując na wyjściu opis abstrakcyjnego drzewa składni w formacie [DOT](https://en.wikipedia.org/wiki/DOT_%28graph_description_language%29), z której można wygenerować graficzną reprezentację grafu np. poleceniem poleceniem `dot -Tps -Oobrazek.ps < wejscie` z pakietu `graphviz`.
 6. `LatteTreeBuilder.g`, z którego ANTLR generuje `LatteTreeBuilder.py`: jest to druga gramatyka, zwana przez ANTLR _drzewową_, która służy do przekształcenia prostego AST wygenerowanego przez parser na drzewo "poważniejszych" obiektów, w tym przypadku będących podklasami `LatteNode`. Reguły tej gramatyki sprowadzają się do przejścia drzewa i uruchomienia w odpowiednich miejscach konstruktorów odpowiednich klas węzłów, podłączeniu odpowiednich dzieci, oraz czasami, co ważne, skorygowaniu informacji o pozycji danego węzła w oryginalnym pliku źródłowym (celem rzetelnego wypisywania informacji o błędach).
 7. `LatteNodes.py`: definiuje klasy węzłów drzewa kodu. Węzły te tworzone są w miejsce surowych węzłów abstrakcyjnego drzewa składni wygenerowanego przez parser. Liczne metody tych klas realizują też możliwe sprawdzenia statyczne: typy zmiennych, funkcji, wyrażeń (metody `check_types`); wartości zwracane (metody `check_return`), itp.
 8. `LatteUtils.py`: definiuje klasy pomocnicze dla klas węzłów kodu: symbole oraz argumenty funkcji i deklaracji.
 9. `LatteProgram.py`: zawiera kolejne klasy węzłów, będące opakowujące węzły `LatteNode` (choć tylko dla możliwości wydzielenia ich funkcji do osobnego modułu). Ich zadaniem jest utworzenie listy instrukcji kodu pośredniego -- najpierw każdy węzeł dla siebie, a potem jedną połączoną listę. Wyrażenia logiczne są od razu tworzone w postaci kodu skaczącego. Wyliczane i przydzielane jest także miejsce na stosie dla zmiennych lokalnych oraz rejestry.
 10. `LatteOptimizer.py`: definiuje klasę optymalizatora, działającą na liście instrukcji kodu pośredniego. Obok pętli głównej optymalizatora jej metody realizują pojedyncze rodzaje optymalizacji, polegających głównie na wyszukiwaniu w kodzie pośrednim odpowiednio powiązanych instrukcji, aby je usunąć lub uprościć. Zawiera też szereg metod pomocniczych, ułatwiających znajdowanie wszelakich ciągów instrukcji, dzięki czemu można łatwo dodać nowe optymalizacje.
 11. `LatteCodes.py`: definiuje klasę `Codes`, generującą instrukcje kodu pośredniego, oraz sporo operacji na tychże kodach -- w szczególności transformację ostatecznego kodu pośredniego na instrukcje asemblera.
 * `latte-runtime.c`: kilka funkcji w C składających się na biblioteczkę standardową języka Latte: `printInt`, `printString`, `readInt`, `readString`, `error`, oraz dodatkowej funkcji pomocniczej `concatString`. Operacje wejścia-wyjścia przeprowadzane są przy użyciu biblioteki `stdio.h`, liczby przechowywane są jako `int`, zaś napisy jako `char[]` (zwracane jako `char*`). Zakładamy też, że wszelka alokacja pamięci na stercie odbywa się po stronie tejże biblioteczki -- w przeciwnym razie trzeba by wywoływać odpowiedni `syscall` o powiększenie bloku pamięci, a następnie samemu nim zarządzać, co już raczej nie jest celem tego zadania :)


### Optymalizacje
Optymalizator w jednym przebiegu uruchamia kolejno opisane niżej metody. Liczba tych przebiegów ustalana jest parametrem wywołania kompilatora `-O` i domyślnie wynosi `2` (trzecia pętla rzadko coś wnosi).  W pojedynczym przebiegu głównym każda z metod może mieć kilka przebiegów z rzędu (co już jest kontrolowane parametrem w kodzie). Metody optymalizacyjne zwracają liczbę wykonanych poprawek, więc gdy dany przebieg (zarówno mały, jak i główny) nic nie wnosi, iteracja zostaje przerwana.

  1. `del_jumps_to_next`: usuwa instrukcje skoku (bezwarunkowe lub warunkowe wraz z porównaniem), które między sobą a docelową etykietą nie mają żadnej "właściwej" instrukcji (tzn. mogą mieć tam instrukcje puste, usunięte (wyoptymalizowane) lub inne etykiety). Uruchamiane "do skutku" (technicznie max. 100 przebiegów, ale zwykle wystarczają 2-3 w pierwszej pętli głównej i 0-1 w kolejnych).
  2. `del_unused_labels`: usuwa etykiety, do których żadna instrukcja nie skacze. Wystarczy, że jest uruchomione raz.
  3. `reduce_push_pop`: upraszcza sekwencje sąsiadujących `push` i `pop` -- prawdopodobnie najważniejsza optymalizacja, jako że kod pośredni wylicza wyrażenia na stosie. Jeśli obie instrukcje korzystają z tego samego adresu (np. `push %eax; pop %eax`), możemy je obie usunąć, w przeciwnym wypadku (np. `push X; pop R`) zamienić na instrukcję przeniesienia (np. `mov X R`). Dodatkowo, możemy tę samą optymalizację przeprowadzić jeśli między `push X` a `pop Y` znajdują się inne instrukcje, pod warunkiem że nie korzystają one ze stosu oraz nie wpływają na lokacje `X` i `Y`.
  4.  `propagate_constants`: propagacja stałych. Propagacja zaczyna się po znalezieniu instrukcji wstawienia stałej do rejestru lub na stos -- instrukcja ta jest usuwana, a stała jest wstawiana bezpośrednio w miejsach użycia (wystąpieniach adresu docelowego). Oczywiście wstawienia te kontynuujemy tylko do instrukcji, która zmienia wartość miejsca, gdzie stała ta miała być wpisana -- lub innej, która unieważnia wartość rejestrów, np. wywołanie innej funkcji. Należy tu pamiętać o szeregu sytuacji "wyjątkowych", kiedy trzeba zmienną podstawić na adres docelowy mimo wszystko: począwszy od instrukcji dzielenia, która wymaga swoich argumentów w rejestrach, przez prawy argument operatora lub porównania (który nie może być stałą bo jest również miejscem na wynik) po instrukcje etykiety i skoku (gdyż gdy wskoczymy z innego miejsca, zmienna może mieć inną wartość). Niemniej jednak wykonujemy propagację, a w razie potrzeby wstawiamy stałą do miejsca docelowego tuż przed instrukcją, która tego wymaga. Dodatkowo, części wstawień przed skokiem/etykietą można uniknąć po rozważeniu grafu skoków i żywotnośći zmiennych.
  5.  oznaczenie zmiennych żywych -- w planach
  6.  zliczanie referencji do napisów i zwalnianie pamięci -- w planach
  7.  `scan_labels`: to nie dokońca optymalizacja, a procedura pomocnicza dla innych optymalizacji -- niejako buduje graf przepływu przez zindeksowanie pozycji etykiet oraz skoków w kodzie pośrednim.
  8.  `clear_deleted_codes`: ostatnia "optymalizacja" uprzątająca kod pośredni -- usuwa instrukcje oznaczone jako wyoptymalizowane. Nie jest efektywne robić to na bieżąco, gdyż usuwanie ze środka listy ma złożoność liniową.

### Użyta dokumentacja
 * [dokumentacja i samouczki ANTLR3](https://theantlrguy.atlassian.net/wiki/display/ANTLR3/ANTLR+v3+documentation);
 * [dokumentacja API pythonowego dla ANTLR3](http://www.antlr3.org/api/index.html);
 * [dokumentacja bibliotek standardowych Pythona](https://docs.python.org/2.7/contents.html);
 * [wiki Pythonowa](https://wiki.python.org/moin/);
 * liczne odpowiedzi na [StackOverflow](http://stackoverflow.com/), jednak nie na tyle istotne żeby je szczegółowo cytować.
