# Dopravní informační systém



## Anotace

Program hledá nejrychlejší spojení v **dopravní síti** tvořené linkami a zastávkami. K danému uživatelskému vstupu vypíše **čas odjezdu** z výchozí stanice, **čas příjezdu** do cílové stanice a informace o **přestupech**. Pro nalezení cesty do přestupních stanic je uplatněn Dijkstrův algoritmus kombinovaný s ořezáváním neperspektivních částí grafu. Následně je vybráno optimální dokončení trasy jízdou přímým dopravním spojem z některé přestupní stanice.

Hlavní program se nalézá v souboru `DopravniInformacniSystem.pas`.

## Specifikace úlohy

Program načte z vstupních souborů data o linkách tvořících souvislou dopravní síť. V souboru `Prestupni.txt` se nalézá neprázdný seznam čísel přestupních stanic, tedy stanic, v nichž je povolen přestup. Soubor `Linky.txt` potom obsahuje počet linek a pro každou linku výčet stanic, jimiž projíždí. Dále v souboru `Odjezdy.txt` jsou uvedeny časy odjezdů z konečných. Linky jezdí obousměrně a jejich trasy neobsahují cykly. Nakonec, soubor `Zastavky.txt` udává, které číslo zastávky odpovídá jakému názvu stanice.

Maximální počet zastávek v dopravní síti, nejvyšší možný počet linek a přestupních stanic po řadě určují konstanty `MaxN` (může se pohybovat v tisících), `MaxL` (typicky několik desítek nebo stovek) a `MaxnP=100`. Doba uplynulá při cestě mezi libovolnými dvěma sousedními zastávkami je pokaždé stejná a rovná konstantě `IMZ` (interval mezi zastávkami v minutách). Výchozí hodnota  `IMZ` je 5. Uživatel zadá na standardní vstup, odkud kam si přeje se přepravit a v kolik hodin. Program mu vypíše nejrychlejší spojení z výchozí do cílové stanice a informace o přestupech (časy odjezdů a použité linky).

Pro více informací a podrobnější dokumentaci viz soubor `Doprovodny_dokument_dopravni_info_system.doc`.  
