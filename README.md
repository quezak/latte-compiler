Projekt zaliczeniowy z MRJP, 01.2016

Artur Kozak [320770], `artur.kozak@students.mimuw.edu.pl`

# latte-compiler

Kompilator języka [Latte](http://www.mimuw.edu.pl/~ben/Zajecia/Mrj2015/latte.html) do asemblera x86. Parsery i przetwarzanie drzewa składni zostało zrealizowane przy pomocy [ANTLR](www.antlr.org) w wersji 3.1. Całość logiki została napisana w Pythonie 2.7 (niestety, bilbioteki antlr3 nie obsługują pythona3, zaś w momencie rozpoczynania projektu antlr4 nie był jeszcze w pełni dostępny).

### Użyte biblioteki
W normalnym przypadku należałoby zainstalować ANTLR w systemie, a biblioteki pythonowe przy użyciu `pip` lub systemowego managera pakietów, jednak celem łatwej kompilacji na `students` załączyłem wszystko w podkatalogu `lib/`.

 * `ANTLR 3.1.3` -- zestaw narzędzi do generacji parserów, wraz z zależnościami:
   * pakiet `stringtemplate` dla Javy (w  której jest napisany sam ANTLR)
   * pakiet `antlr` dla Javy -- są to biblioteki starego ANTLR 1/2, od których zależny jest w/w `stringtemplate`;
   * moduł `antlr3` dla Pythona -- runtime library dla pythonowych parserów wygenerowanych przez ANTLR, musi być zainstalowany w tej samej wersji co sam ANTLR;
   * moduł `stringtemplate3` dla Pythona -- analogicznie jak w Javie, wymagany przez bilbioteki `antlr3`;
   * moduł `antlr` dla Pythona -- analogicznie jak w Javie, moduł `stringtemplate3` jest zależny od bibliotek starych wersji ANTLR;
 * biblioteki dostępne standardowo w Pythonie, np. [`argparse`](https://docs.python.org/2.7/library/argparse.html) do przetwarzania parametrów wiersza poleceń.

### Opis modułów
Każdy plik źródłowy jest osobnym modułem programu, krótki opis każdego mniej więcej w kolejności działania:

 * `FuturePrint.py`: funkcje pomocnicze do wypisywania komunikatów diagnostycznych, wiadomości, ostrzeżeń i błędów. Dodatkowo umożliwia wypisywanie sekwencji kolorów ANSI celem zwiększenia czytelności wyjścia programu.
 * `LatteErrors.py`: definicje wyjątków używanych przez program oraz klasa `Status`, służąca do buforowania i wypisywania wiadomości wyjściowych.
 * `Utils.py`: zawiera przydatne dodatki: klasę `Flags` służącą do przetwarzania argumentów wywołania i globalnego dostępu do nich oraz instrukcję `switch`, imitującą zachowanie takiej samej instrukcji z C -- a niezmiernie przydatną przy rozważaniu wielu przypadków.
 * `LatteMain.py`: główna funkcja kompilatora, uruchamiająca kolejno wszystkie etapy budowania programu, od otwarcia pliku wejściowego do linkowania z biblioteczką standardową.
 * `Latte.g`, z którego ANTLR generuje `LatteLexer.py` oraz `LatteParser.py`: definicja gramatyki Latte, równoważna z zadaną w pliku Latte.cf, ale sprowadzona do postaci LL* (gdyż takie parsery generuje ANTLR). Definiuje też kilka metod pomocniczych parsera -- w tym funkcję główną, dzięki której można uruchomić sam parser, uzyskując na wyjściu opis abstrakcyjnego drzewa składni w formacie [DOT](https://en.wikipedia.org/wiki/DOT_%28graph_description_language%29), z której można wygenerować graficzną reprezentację grafu np. poleceniem poleceniem `dot -Tps -Oobrazek.ps < wejscie` z pakietu `graphviz`.
 * `LatteTreeBuilder.g`, z którego ANTLR generuje `LatteTreeBuilder.py`: jest to druga gramatyka, zwana przez ANTLR _drzewową_, która służy do przekształcenia prostego AST wygenerowanego przez parser na drzewo "poważniejszych" obiektów, w tym przypadku będących podklasami `LatteNode`. Reguły tej gramatyki sprowadzają się do przejścia drzewa i uruchomienia w odpowiednich miejscach konstruktorów odpowiednich klas węzłów, podłączeniu odpowiednich dzieci, oraz czasami, co ważne, skorygowaniu informacji o pozycji danego węzła w oryginalnym pliku źródłowym (celem rzetelnego wypisywania informacji o błędach).
 * `LatteNodes.py`: definiuje klasy węzłów drzewa kodu. Węzły te tworzone są w miejsce surowych węzłów abstrakcyjnego drzewa składni wygenerowanego przez parser. Liczne metody tych klas realizują też możliwe sprawdzenia statyczne: typy zmiennych, funkcji, wyrażeń (metody `check_types`); wartości zwracane (metody `check_return`), itp.
 * `LatteUtils.py`: definiuje klasy pomocnicze dla klas węzłów kodu: symbole oraz argumenty funkcji i deklaracji.
 * `LatteProgram.py`: zawiera kolejne klasy węzłów, będące opakowujące węzły `LatteNode` (choć tylko dla możliwości wydzielenia ich funkcji do osobnego modułu). Ich zadaniem jest utworzenie listy instrukcji kodu pośredniego -- najpierw każdy węzeł dla siebie, a potem jedną połączoną listę. Wyrażenia logiczne są od razu tworzone w postaci kodu skaczącego. Wyliczane i przydzielane jest także miejsce na stosie dla zmiennych lokalnych oraz rejestry.
 * `LatteOptimizer.py`: definiuje klasę optymalizatora, działającą na liście instrukcji kodu pośredniego. Obok pętli głównej optymalizatora jej metody realizują pojedyncze rodzaje optymalizacji, polegających głównie na wyszukiwaniu w kodzie pośrednim odpowiednio powiązanych instrukcji, aby je usunąć lub uprościć. Zawiera też szereg metod pomocniczych, ułatwiających znajdowanie wszelakich ciągów instrukcji, dzięki czemu można łatwo dodać nowe optymalizacje.
 * `LatteCodes.py`: definiuje klasę `Codes`, generującą instrukcje kodu pośredniego, oraz sporo operacji na tychże kodach -- w szczególności transformację ostatecznego kodu pośredniego na instrukcje asemblera.
 * `latte-runtime.c`: kilka funkcji w C składających się na biblioteczkę standardową języka Latte: `printInt`, `printString`, `readInt`, `readString`, `error`, oraz dodatkowej funkcji pomocniczej `concatString`. Operacje wejścia-wyjścia przeprowadzane są przy użyciu biblioteki `stdio.h`, liczby przechowywane są jako `int`, zaś napisy jako `char[]` (zwracane jako `char*`). Zakładamy też, że wszelka alokacja pamięci na stercie odbywa się po stronie tejże biblioteczki -- w przeciwnym razie trzeba by wywoływać odpowiedni `syscall` o powiększenie bloku pamięci, a następnie samemu nim zarządzać, co już raczej nie jest celem tego zadania :)


### Użyta dokumentacja
 * [dokumentacja i samouczki ANTLR3](https://theantlrguy.atlassian.net/wiki/display/ANTLR3/ANTLR+v3+documentation);
 * [dokumentacja API pythonowego dla ANTLR3](http://www.antlr3.org/api/index.html);
 * [dokumentacja bibliotek standardowych Pythona](https://docs.python.org/2.7/contents.html);
 * [wiki Pythonowa](https://wiki.python.org/moin/);
 * liczne odpowiedzi na [StackOverflow](http://stackoverflow.com/), jednak nie na tyle istotne żeby je szczegółowo cytować.
