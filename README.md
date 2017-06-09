# About this repository

This is a compiler for a Java-like programming language, done with Python and ANTLR3. It outputs x86 assembly and links it with gcc. It was a final assignment on a course about compilers.

The language has all the basic datatypes and flow control instructions, strings, structs, and classes with simple (non-virtual) inheritance. Below is the original, detailed report in Polish, describing how it should be used, how all the modules work, what language features and output code optimizations are implemented and how.

# latte-compiler

Kompilator języka [Latte](http://www.mimuw.edu.pl/~ben/Zajecia/Mrj2015/latte.html) do asemblera x86. Parsery i przetwarzanie drzewa składni zostało zrealizowane przy pomocy [ANTLR](www.antlr.org) w wersji 3.1. Całość logiki została napisana w Pythonie 2.7 (niestety, bilbioteki antlr3 nie obsługują pythona3, zaś w momencie rozpoczynania projektu antlr4 nie był jeszcze w pełni dostępny).

## Parametry wywołania
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

## Użyte biblioteki
W normalnym przypadku należałoby zainstalować ANTLR w systemie, a biblioteki pythonowe przy użyciu `pip` lub systemowego managera pakietów, jednak celem łatwej kompilacji na `students` załączyłem wszystko w podkatalogu `lib/`. Dodatkowo, w podkatalogu `utils/` załączam definicje kolorowania składni gramatyk ANTLR dla `vim`a.

 * `ANTLR 3.1.3` -- zestaw narzędzi do generacji parserów, wraz z zależnościami:
   * pakiet `stringtemplate` dla Javy (w  której jest napisany sam ANTLR)
   * pakiet `antlr` dla Javy -- są to biblioteki starego ANTLR 1/2, od których zależny jest w/w `stringtemplate`;
   * moduł `antlr3` dla Pythona -- runtime library dla pythonowych parserów wygenerowanych przez ANTLR, musi być zainstalowany w tej samej wersji co sam ANTLR;
   * moduł `stringtemplate3` dla Pythona -- analogicznie jak w Javie, wymagany przez bilbioteki `antlr3`;
   * moduł `antlr` dla Pythona -- analogicznie jak w Javie, moduł `stringtemplate3` jest zależny od bibliotek starych wersji ANTLR;
 * biblioteki dostępne standardowo w Pythonie, np. [`argparse`](https://docs.python.org/2.7/library/argparse.html) do przetwarzania parametrów wiersza poleceń.

## Opis modułów
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
 10. `LatteOptimizer.py`: definiuje klasę optymalizatora, działającą na liście instrukcji kodu pośredniego. Obok pętli głównej optymalizatora jej metody realizują pojedyncze rodzaje optymalizacji, polegających głównie na wyszukiwaniu w kodzie pośrednim odpowiednio powiązanych instrukcji, aby je usunąć lub uprościć.
 11. `CodeMatcher.py`: zawiera szereg metod pomocniczych dla optymalizatora, ułatwiających znajdowanie wszelakich ciągów instrukcji, dzięki czemu można łatwo dodać nowe optymalizacje.
 12. `LatteCodes.py`: definiuje klasę `Codes`, generującą instrukcje kodu pośredniego, oraz sporo operacji na tychże kodach -- w szczególności transformację ostatecznego kodu pośredniego na instrukcje asemblera.
 * `latte-runtime.c`: kilka funkcji w C składających się na biblioteczkę standardową języka Latte: `printInt`, `printString`, `readInt`, `readString`, `error`, oraz dodatkowej funkcji pomocniczej `concatString`. Operacje wejścia-wyjścia przeprowadzane są przy użyciu biblioteki `stdio.h`, liczby przechowywane są jako `int`, zaś napisy jako `char[]` (zwracane jako `char*`). Zakładamy też, że wszelka alokacja pamięci na stercie odbywa się po stronie tejże biblioteczki -- w przeciwnym razie trzeba by wywoływać odpowiedni `syscall` o powiększenie bloku pamięci, a następnie samemu nim zarządzać, co już raczej nie jest celem tego zadania :)


## Optymalizacje
Optymalizator w jednym przebiegu uruchamia kolejno opisane niżej metody. Liczba tych przebiegów ustalana jest parametrem wywołania kompilatora `-O` i domyślnie wynosi `2` (trzecia pętla rzadko coś wnosi).  W pojedynczym przebiegu głównym każda z metod może mieć kilka przebiegów z rzędu (co już jest kontrolowane parametrem w kodzie). Metody optymalizacyjne zwracają liczbę wykonanych poprawek, więc gdy dany przebieg (zarówno mały, jak i główny) nic nie wnosi, iteracja zostaje przerwana.

  1. `del_jumps_to_next`: usuwa instrukcje skoku (bezwarunkowe lub warunkowe wraz z porównaniem), które między sobą a docelową etykietą nie mają żadnej "właściwej" instrukcji (tzn. mogą mieć tam instrukcje puste, usunięte (wyoptymalizowane) lub inne etykiety). Uruchamiane "do skutku" (technicznie max. 100 przebiegów, ale zwykle wystarczają 2-3 w pierwszej pętli głównej i 0-1 w kolejnych).
  2. `del_unused_labels`: usuwa etykiety, do których żadna instrukcja nie skacze. Wystarczy, że jest uruchomione raz.
  3. `reduce_push_pop`: upraszcza sekwencje sąsiadujących `push` i `pop` -- prawdopodobnie najważniejsza optymalizacja, jako że kod pośredni wylicza wyrażenia na stosie. Jeśli obie instrukcje korzystają z tego samego adresu (np. `push %eax; pop %eax`), możemy je obie usunąć, w przeciwnym wypadku (np. `push X; pop R`) zamienić na instrukcję przeniesienia (np. `mov X R`). Dodatkowo, możemy tę samą optymalizację przeprowadzić jeśli między `push X` a `pop Y` znajdują się inne instrukcje, pod warunkiem że nie korzystają one ze stosu oraz nie wpływają na lokacje `X` i `Y`.
  4.  `propagate_constants`: propagacja stałych. Opiera się na przeglądaniu listy instrukcji pamiętając po drodze, czy i jakie stałe znajdują się w poszczególnych rejestrach. Ze względu na dużą liczbę podprzypadków podzielona jest na mniejsze funkcje:
    1.  Zastąpienie odczytu rejestru stałą, która wiemy że się w nim znajduje -- ułatwione przez konsekwentne nazewnictwo parametrów kodu pośredniego (tak że argumenty odczytywane nazywają się tak samo dla różnych typów instrukcji).
    2.  Unieważnienie w pamięci podręcznej stałych, które były w rejestrach, które są przez daną operację zapisywane (analogicznie, pomaga spójne nazewnictwo parametrów).
    3.  W sytuacjach wyjątkowych zapamiętanie, że przed daną instrukcją niektóre stałe i tak muszą być wpisane do rejestrów, bo instrukcja wymaga żeby tam były (np. dzielenie, LEA). Rejestry takie są oznaczane do zdjęcia na poziomie generacji kodu, żeby nie rozważać tutaj przypadków.
    4.  Opróżnienie pamięci podręcznej przy instrukcjach kontroli przepływu: na wywołaniu funkcji można po prostu unieaktualnić wszystkie pamiętane stałe; na wyjściu z funkcji także, tylko uprzednio wpisać wartość zwracaną do EAX, jeśli jest stałą; wreszcie dla instrukcji skoku/etykiety wpisać wszystkie pamiętane stałe do ich rejestrów, bo rejestry mogą mieć inne wartości przy innym wariancie skoków. Ostatni z tych podprzypadków mógłby być jeszcze trochę poprawiony, gdyby rozpatrzyć możliwe ścieżki między etykietami oraz żywotność zmiennych.
    5.  Zastąpienie operatora jego wynikiem, jeśli wszystkie jego argumenty są stałymi (zapamiętanymi jako wartości rejestrów lub faktyczne stałe w kodzie).
    6.  W miejscach gdzie zapamiętano że trzeba mimo wcześniejszej propagacji wstawić stałą do rejestru, wstawić dodatkowe instrukcje kodu które to realizują -- jest to osobna pętla, żeby we wcześniejszych przypadkach nie przejmować się edycją listy w czasie iteracji.
    7.  Wreszcie przypadek od którego cała optymalizacja się zaczyna -- znajdujemy instrukcje wstawienia stałej do rejestru, oznaczamy je do usunięcia, a podstawianą stałą zapamiętujemy w pamięci podręcznej optymalizatora.
  6.  zliczanie referencji do i zwalnianie pamięci -- w planach
  7.  `scan_labels`: to nie dokońca optymalizacja, a procedura pomocnicza dla innych optymalizacji -- buduje graf przepływu przez zindeksowanie pozycji etykiet oraz skoków w kodzie pośrednim.
  8.  `clear_deleted_codes`: ostatnia "optymalizacja" uprzątająca kod pośredni -- usuwa instrukcje oznaczone jako wyoptymalizowane lub puste (dodające komentarzy w kodzie wynikowym dla ułatwienia rozwoju aplikacji). Nie jest efektywne robić to na bieżąco, gdyż usuwanie ze środka listy ma złożoność liniową i wymusza ręczne wznawianie iteracji.
  9.  Usuwanie martwego kodu: funkcje, które nie są wywoływane nie są w ogóle umieszczane w kodzie pośrednim. Klasy, które nigdy nie są alokowane również są usuwane (z poszanowaniem dziedziczenia), zaś wewnątrz używanych klas analogicznie umieszczane są tylko metody przynajmniej raz wywołane.

## Rozszerzenia

#### Tablice
Tablice `N`-elementowe alokowane są na stercie jako blok pamięci o rozmiarze `N+1` zmiennych, przy pomocy funkcji biblioteczki standardowej korzystajcej z tradycyjnego `malloc`. W pierwszej komórce pamięci wpisany jest rozmiar tablicy, zwracany w czasie stałym jako atrybut `.length`; zaś w dalszych komórkach kolejne wartości -- zmienne typów prostych lub referencje do typów złożonych. Możliwe jest oczywiście tworzenie tablic obiektów, tablic wielowymiarowych oraz tablic o niestałej długości. Operator `[]` i atrybut `.length` mogą być stosowane również bezpośrednio do wyrażeń zwracających typ tablicowy, w tym wyników obiektów tymczasowych jak wynik funkcji czy operatora `new`. Przy alokacji nowej tablicy jej elementy są ustawione na wartość domyślną danego typu -- `0` dla liczb, `false` dla wartości logicznych, `""` dla napisów oraz `null` dla referencji obiektów i innych tablic. Dostępna jest też pętla `for (TYPE x : array_expr) {...}`, pozwalająca wygodnie iterować po tablicy nie zmieniając jej. Technicznie ta pętla jest tylko lukrem syntaktycznym, po sprawdzeniu typów zostaje zamieniona na ekwiwalent
```c++
{ int _i = 0; TYPE[] _t = array_expr;
  while (_i < _t.length) {
    TYPE x = _t[_i]; _i++;
    {...}}}
```

#### Struktury
Struktury o `N` polach alokowane są jako blok pamięci o `N` zmiennych -- rozmiar struktury jest stały, więc nie ma potrzeby przechowywania jego rozmiaru w pamięci jak przy tablicach. Przy wyliczaniu wyrażenia które jest polem struktury, obliczane jest przesunięcie danego pola względem adresu jej początku. Pola oczywiście mogą być dowolnych typów, w tym tablic i innych struktur. Struktury, tak jak i tablice, można porównywać z innymi obiektami jak i wartością `null` operatorami `==` i `!=` -- jest to porównanie adresów struktur, o ile obia wyrażenia sa tego samego typu (w szczególności, zgodnie z treścią i testami zadania, `(TYPE) null` jest jedynym dozwolonym rodzajem rzutowania typów).

#### Inicjacja pól obiektów
Mimo że specyfikacja zadania tego nie wymaga, z braku dostępności konstruktorów postanowiłem dla ułatwienia wprowadzić inicjalizatory domyślnych wartości dla pól obiektów. Wzorując się na `C++11`, przy deklaracji pola można dopisać podstawienie dowolnym wyrażeniem które korzysta z już dostępnych wartości -- w tym funkcji globalnych nie korzystających z danej klasy oraz zainicjowanych wcześniej pól (w tym pól podklasy). Jeśli wyrażenie nie jest podane, podstawiana jest domyślna wartość dla danego typu, jak w tablicach. Pola inicjowane są w kolejności deklaracji.

#### Obiekty
Rozszerzenie struktur o metody. Dla ułatwienia sprawdzenia typów pola struktur są zwykłymi zmiennymi, a metody zwykłymi funkcjami w zasięgu ich klasy. Przed generacją kodu pośredniego metody zamieniane są na funkcje globalne o odpowiednio udekorowanej nazwie (np. `Counter::setValue()` zmieni się w `__Counter__setValue`); a na początku listy argumentów dodany jest parametr `self`, przekazujący referencję na właściwą instancję obiektu -- jest on niejawny i automatycznie przekazywany, jednak można też go użyć bezpośrednio. Następnie wszystkie odwołania do pól i innych metod zamieniane są na atrybut referencji `self`.

#### Proste dziedziczenie
Każda klasa może w tym rozszerzeniu dziedziczyć po jednej innej klasie, uzyskując dostęp do jej pól i metod. Klasa jest alokowana jako blok pamięci o rozmiarze sumy liczby nowych pól deklarowanych przez tę klasę oraz całkowitej liczby pól deklarowanych przez podklasy. W tymże bloku pamięci najpierw zapisane są kolejno pola podklas, poczynając od najwyższej (tej która niczego nie dziedziczy), a na samym końcu pola własne. W ten sposób znacząco ułatwiony jest dostęp do pól obiektów -- do metody podklasy jako parametr `self` można przekazać po prostu referencję do nadklasy, gdyż początkowy fragment jej bloku pamięci jest poprawnym obiektem podklasy (czyli przesunięcie pola podklasy względem adresu początku obiektu jest takie samo jakby używać tego pola bezpośrednio w podklasie).

#### Wyjście programu
Postarałem się, żeby wyjście kompilatora było maksymalnie wygodne w odbiorze, przez zaznaczanie ważnych fragmentów kolorami, podawanie gdzie możliwe powiązanej pozycji w kodzie programu i innych dodatkowych informacji oraz dodatkowe parametry wywołania. Wzorowałem się często na `gcc`, np. błędowi ponownej deklaracji zmiennej (z podanym miejscem i typem) towarzyszy notatka wskazująca miejsce i typ poprzedniej deklaracji, analogicznie przy wszelkich innych błędach typów. Dostępne są różne inne pomocnicze błędy i ostrzeżenia, np. dokładne sprawdzanie zwracania wartości z funkcji (ze wskazaniem fragmentów które nigdy nie zostaną osiągnięte), wskazywanie cyklicznego dziedziczenia, sprawdzanie rozmiaru stałych liczbowych czy nieużytych wyników wyrażeń. Dodatkowo, przy włączeniu trybu gadatliwego (`-d`), widać cały proces budowania kodu i optymalizacji, a w kodzie wynikowym `.s` są pozostawione znaczniki i komentarze pomagające w czytelności.


### Użyta dokumentacja
 * [dokumentacja i samouczki ANTLR3](https://theantlrguy.atlassian.net/wiki/display/ANTLR3/ANTLR+v3+documentation);
 * [dokumentacja API pythonowego dla ANTLR3](http://www.antlr3.org/api/index.html);
 * [dokumentacja bibliotek standardowych Pythona](https://docs.python.org/2.7/contents.html);
 * [wiki Pythonowa](https://wiki.python.org/moin/);
 * liczne odpowiedzi na [StackOverflow](http://stackoverflow.com/), jednak nie na tyle istotne żeby je szczegółowo cytować.
