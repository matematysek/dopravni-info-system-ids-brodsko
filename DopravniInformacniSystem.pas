program DopravniInformacniSystem;
{ Dopravn� informa�n� syst�m }
{ Ji�� Zeman, 1. ro�n�k OM }
{ letn� semestr 2015 }
{ Programov�n� 2 NMIN102 }
uses modulDatum;

const SOUBOR_ODJEZDY = 'Odjezdy.txt'; {soubor obsahuje �asy odjezd� z kone�n�ch stanic}
      SOUBOR_LINKY = 'Linky.txt'; {soubor obsahuje po�et linek a zast�vky, jimi� jednotliv� linky proj��d�j�}
      SOUBOR_ZASTAVKY = 'Zastavky.txt'; {soubor obsahuje n�zvy zast�vek a jejich ��seln� ozna�en�}
      SOUBOR_PRESTUPNI = 'Prestupni.txt'; {v souboru nalezneme seznam p�estupn�ch stanic}
      IMZ = 5; {kolik minut �in� interval mezi zast�vkami}
      MaxN = MaxInt - 1; {maxim�ln� po�et zast�vek v dopravn� s�ti}
      MaxL = 200; {maxim�ln� po�et linek v dopravn� s�ti}
      MaxnP = 100; {maxim�ln� po�et p�estupn�ch stanic}
type tSmer = (A, B); {sm�r linky}
     tCisloZ = 1..MaxN; {��slo zast�vky}
     tCisloL = 1..MaxL+1; {��slo linky}
     uHrana = ^tHrana; {uHrana je prvek datov� struktury DS, kter� reprezentuje graf dopravn� s�t�}
     tHrana = record
                linka: Integer;
                vok: Integer; {vzd�lenost od kone�n� stanice}
                smer: tSmer;
                doZast: tCisloZ; {do kter� zast�vky hrana vede}
                zavora: Boolean; {optimalizace I. f�ze prohled�v�n�, m� smysl hledat p�estupn� stanice jen za hranami bez z�vory}
                dalsi: uHrana; {ukazatel na dal� hranu vedouc� z dan�ho vrcholu}
               end;
     uMinuta = ^tMinuta; {uMinuta je prvek datov� struktury Odjezdy, kter� obsahuje �asy odjezd� z kone�n�ch}
     tMinuta = record
                m: 0..59;
                dalsi: uMinuta;
               end;
     uPrestup = ^tPrestup; {uPrestup je prvek datov� struktury PPS, pomoc� n�� lze dohledat p�estupy na nalezen� nejkrat� cest�}
     tPrestup = record
                    misto: tCisloZ;
                    prij, odj: Datum;
                    linka1, linka2: 1..MaxL + 1;
                    nasl: uPrestup; {n�sleduj�c� p�estup, POU��VAT A� p�i vypisov�n� nejkrat� cesty!}
                end;
var Start, Cil: tCisloZ; {��sla v�choz� a c�lov� stanice}
    StartString, CilString: String;
    Cas, c: Datum; {Cas...�as zadan� u�ivatelem}
    CasString: String[5];
    F: Text;
    lin, delkaLinky, iz: Integer; {iz...index zast�vky v r�mci linky}
    lZav, pZav: Byte; {index lev� a prav� z�vorky v na��tan�m �et�zci}
    i, j: Integer;
    zast, predZast: Integer; {zast�vka, p�edchoz� p�e�ten� zast�vka}
    udaj: String;
    DS: array[1..MaxN] of uHrana; {dopravn� s��}
    Odjezdy: array[1..MaxL, tSmer, 0..23] of uMinuta;
    L, N, nP: Integer; {po�et linek, zast�vek a p�est. stanic v dopravn� s�ti}
    seznamPrestupnich: array[1..MaxnP] of tCisloZ; {seznam p�estupn�ch stanic}
    sCilemSpojene: array[1..MaxnP] of tCisloZ; {seznam t�ch p�estupn�ch stanic, kter� maj� s c�lem n�jakou spole�nou linku}
    smeryPrijezdu: array[1..MaxnP] of tSmer; {sm�ry p��jezd� do s c�lem spojen�ch p�estupn�ch stanic}
    nazvyStanic: array[1..MaxnP] of String; {n�zvy p�estupn�ch stanic}
    nSS: Integer; {po�et s c�lem spojen�ch p�estupn�ch stanic}
    z: Char;
    errCode: Integer;
    P: uHrana;
    uM: uMinuta;
    ctenySmer: tSmer;
    nalezenStrednik: Boolean;
    priznak: ShortInt; {p��znak p�ekro�en� p�lnoci p�i v�po�tu rozd�l� �as�}
    Doba: array[1..MaxN] of Datum; {hodnoty vrchol� v Dijkstrov� algoritmu}
    Doc: array[1..MaxN] of Boolean; {ud�v�, zda je Doba pro dan� vrchol je�t� do�asn�}
    ncdc, ncdps: Boolean; {nalezena cesta do c�le nebo do p�estupn�ch stanic}
    nejblizsi: tCisloZ; {��slo vrcholu, jeho� hodnota se bude prohla�ovat za trvalou}
    w, casProhledavani: Datum; {v�ha hrany a �as, kdy prohled�v�n� dorazilo do vrcholu zpracov�van�ho v kroku Dijkstrova algoritmu}
    PPS: array[1..MaxN] of uPrestup; {jak� p�estup stanici p�edch�zel}
    dalsiPrestup: uPrestup;
    hranice: Integer; {pomocn� prom�nn� pro um�s�ov�n� z�vor (optimalizace I. f�ze prohled�v�n�)}
    hrNalezena: Boolean; {hranice nalezena, indik�tor p�i �ten� souboru Linky.txt}
    poslPrestup: tPrestup;
    minOdjezd: Datum; {odjezd linky na nejkrat� cest� z vrcholu, kde se kon� II. f�ze prohled�v�n�}
    minLinka: tCisloL; {��slo takov� linky}

procedure pridejHranu(odkud, kam: tCisloZ; l: Integer; s: tSmer; vzdOdKon: Integer);
{do datov� struktury DS p�id� hranu odkud kam linky l sm�ru s se vzd�lenost� od kone�n� vzdOdKon}
var P, Q: uHrana;
begin
 new(P);
 P^.linka := l;
 P^.smer := s;
 P^.doZast := kam;
 P^.vok := vzdOdKon;
 P^.zavora := FALSE;
 P^.dalsi := nil;
 if DS[odkud] = nil then DS[odkud] := P {p�idat prvn� hranu do pr�zdn�ho seznamu}
 else begin {jinak P^ zapojit na konec seznamu hran}
         Q := DS[odkud];
         while Q^.dalsi <> nil do
           Q := Q^.dalsi;
         Q^.dalsi := P;
  end;
end; {procedure pridejHranu()}

procedure pridejZavoru(kam: tCisloZ; l: Integer; s: tSmer);
{p�id� z�voru do zast�vky kam lince l ve sm�ru s}
var P: uHrana;
begin
  P := DS[kam];
  while (P <> nil) and ((P^.linka <> l) or (P^.smer <> s)) do
    P := P^.dalsi;
  if P <> nil then P^.zavora := TRUE;
end; {procedure pridejZavoru()}

procedure pridejOdjezd(l: Integer; s: tSmer; c: Datum);
{do datov� struktury Odjezdy p�id� odjezd linky l z kone�n� ve sm�ru s v �ase c}
var P, Q: uMinuta;
    h: Byte;
begin
  new(P);
  P^.m := minuty(c);
  h := hodiny(c);
  P^.dalsi := nil;
  if Odjezdy[l][s][h] = nil then Odjezdy[l][s][h] := P
  else begin
        Q := Odjezdy[l][s][h];
        while Q^.dalsi <> nil do
            Q := Q^.dalsi;
        Q^.dalsi := P;
     end;
end; {procedure pridejOdjezd()}

function dalsiSpoj(l: Integer; s: tSmer; c: Datum): Datum;
{vrac� prvn� �as odjezdu z kone�n� stanice linky l ve sm�ru s v nebo po �ase c}
var hy, my: ShortInt; {hodiny, minuty �asu c}
    uM: uMinuta;
    zitra: Boolean; {zda spoj odj��d� a� den pot�}
begin
  hy := hodiny(c);
  my := minuty(c);
  uM := Odjezdy[l][s][hy];
  zitra := FALSE;
  if uM <> nil then begin {nejprve hledat spoj v t� sam� hodin�}
     while (uM <> nil) and (uM^.m < my) do
        uM := uM^.dalsi;
     if uM <> nil then begin
         dalsiSpoj := hy*60 + uM^.m;
         exit;
        end;
    end;
  while TRUE do begin {jinak hledat spoj v n�sleduj�c�ch hodin�ch}
     hy := hy + 1;
     if hy >= 24 then begin
            hy := hy mod 24;
            zitra := TRUE;
        end;
     uM := Odjezdy[l][s][hy];
     if uM <> nil then begin
         if not zitra then dalsiSpoj := hy*60 + uM^.m
         else dalsiSpoj := 1440 + hy*60 + uM^.m;
         exit;
        end;
    end;
end; {function dalsiSpoj()}

procedure pridejPrestup(proStanici, mistoPrestupu: tCisloZ; pr, odj: Datum; l1, l2: tCisloL);
{v datov� struktu�e PPS nastav�, �e posledn� p�estup p�ed pr�jezdem zast�vkou proStanici nastal
v zast�vce mistoPrestupu s p��jezdem pr, odjezdem odj a z linky l1 na linku l2}
var prestup: uPrestup;
    i: Integer;
    posledni: Boolean; {indikuje, zda stanice proStanici je posledn�, kter� ukazuje na nahrazovan� p�estup}
begin
  prestup := PPS[proStanici];
  posledni := TRUE;
  if prestup <> nil then {pokud stanice ji� m� p�edchoz� ur�en� p�estup, nahradit ho} begin
       for i := 1 to N do
            if PPS[i] = prestup then posledni := FALSE;
       if posledni then dispose(prestup); {p�estup se m��e odstranit, jen kdy� je zaru�eno, �e na n�j nic neukazuje}
    end;
  new(prestup);
  prestup^.misto := mistoPrestupu;
  prestup^.prij := pr;
  prestup^.odj := odj;
  prestup^.linka1 := l1;
  prestup^.linka2 := l2;
  PPS[proStanici] := prestup;
end; {procedure pridejPrestup()}

function jePrestupni(a: tCisloZ): Boolean;
{P�len�m interval� hled� zast�vku a v seznamu p�estupn�ch stanic. Vrac�
TRUE pr�v� tehdy, kdy� zast�vka a je p�estupn�}
var i, j, k: Integer;
begin
  i := 1;
  j := nP;
  jePrestupni := FALSE;
  repeat
      k := (i + j) div 2;
      if a > seznamPrestupnich[k] then i := k + 1
      else if a < seznamPrestupnich[k] then j := k - 1
      else if seznamPrestupnich[k] = a then begin
         jePrestupni := TRUE;
         break;
        end;
  until i > j;
end; {function jePrestupni()}

function indexSCilemSpojene(zast: tCisloZ): Integer;
{vrac� index zast�vky zast v poli sCilemSpojene[], nebo nulu, jestli stanice nen� p�estupn� a s c�lem spojen�}
var i, j, k: Integer;
begin
  indexSCilemSpojene := 0;
  i := 1;
  j := nP;
  repeat
      k := (i + j) div 2;
      if zast > sCilemSpojene[k] then i := k + 1
      else if zast < sCilemSpojene[k] then j := k - 1
      else if sCilemSpojene[k] = zast then begin
         indexSCilemSpojene := k;
         break;
        end;
  until i > j;
end; {function indexSCilemSpojene()}

function jeSpolecnaLinka(zast1, zast2: tCisloZ ): Boolean;
{vrac� TRUE, existuje-li ve dvou LSS hran DS[zast1], DS[zast2] neklesaj�c�ch vzhledem k prom�nn� ^.linka
n�jak� spole�n� linka}
var P, Q: uHrana;
begin
    jeSpolecnaLinka := FALSE;
    P := DS[zast1];
    Q := DS[zast2];
    while (P <> nil) and (Q <> nil) do begin
        if P^.linka > Q^.linka then Q := Q^.dalsi
        else if Q^.linka > P^.linka then P := P^.dalsi
        else if P^.linka = Q^.linka then begin
            jeSpolecnaLinka := TRUE;
            break;
          end;
      end;
end; {function jeSpolecnaLinka()}

function najdiHranu(zast: tCisloZ; l: tCisloZ; s: tSmer): uHrana;
{vrac� ukazatel na hranu v line�rn�m spojov�m seznamu hran DS[zast], kter� vede ze zast�vky zast linkou l ve sm�ru s}
var R: uHrana;
begin
  R := DS[zast];
  while (R <> nil) and ((R^.linka <> l) or (R^.smer <> s)) do
    R := R^.dalsi;
  najdiHranu := R;
end; {function najdiHranu()}

function primeSpojeni(odkud, kam: tCisloZ; kdy: Datum; var doba: Datum): tPrestup;
{Najde nejrychlej� p��m� spojen� odkud kam po �ase kdy. Dobu, jak dlouho to potrv� od kdy, ulo�� do prom�nn� doba. Funkce vrac� �as odjezdu a pou�itou linku v prom�nn� typu tPrestup}
var P, Q, R, S: uHrana; {slou�� k proj��d�n� seznam� DS[odkud] a DS[kam]}
   minDoba, minOdjezd: Datum; {doba trv�n� dosud nejrychlej�ho spojen� a �as jeho odjezdu}
   minLinka: tCisloL; {linka pou�it� p�i nejrychlej�m spojen�}
   doba1, doba2: Datum; {pr�bاn� doby prohled�v�n� ob�ma sm�ry}
   odjezd1, odjezd2: Datum; {�asy odjezd� v obou sm�rech}
   priznak: ShortInt;
begin
  if odkud = kam then begin {odkud a kam spl�vaj� v jednu stanici}
      doba := 0;
      exit;
    end;
  P := DS[odkud]; {P, Q budou proj��d�t seznamy hran vedouc�ch z vrchol� odkud a kam}
  Q := DS[kam];
  minDoba := NEKONECNO;
  minLinka := MaxL + 1;
  minOdjezd := NEKONECNO;
  doba := NEKONECNO;
  if (P = nil) or (Q = nil) then
      {odkud nebo kam nevede ��dn� linka} exit;
  while (P <> nil) and (Q <> nil) do begin {dokud ani jeden seznam hran nedojel do konce}
      if P^.linka > Q^.linka  then Q := Q^.dalsi
      else if Q^.linka > P^.linka then P := P^.dalsi
      else {P^.linka = Q^.linka} begin
          {nalezena spole�n� linka stanic odkud a kam, spustit PROHLED�V�N�}
          if (P^.dalsi <> nil) and (P^.dalsi^.linka = P^.linka) then begin {prohled�vat
            ob�ma sm�ry}
             R := P;
             S := P^.dalsi;
             iz := indexSCilemSpojene(odkud); {zjistit zda je odkud spojen� s c�lem}
             if (iz > 0) and (PPS[odkud]^.linka2 = P^.linka) and (smeryPrijezdu[iz] = P^.smer) then begin {umo�nit pokra�ovat linkou, kterou prohled�v�n� do dan�ho vrcholu dorazilo v I. f�zi}
                odjezd1 := dalsiSpoj(P^.linka, P^.smer, rozdilCasu(kdy, P^.vok*IMZ+1, priznak)) + P^.vok*IMZ;
                if (priznak < 0) and (odjezd1 > 1440) then odjezd1 := odjezd1 - 1440; {p�lnoc byla p�ekro�ena jen kv�li p�enesen� vyhled�v�n� spoje na kone�nou}
              end
             else begin
                odjezd1 := dalsiSpoj(P^.linka, P^.smer, rozdilCasu(kdy, P^.vok*IMZ, priznak)) + P^.vok*IMZ;
                if (priznak < 0) and (odjezd1 > 1440) then odjezd1 := odjezd1 - 1440;
              end;
             if (iz > 0) and (PPS[odkud]^.linka2 = P^.linka) and (smeryPrijezdu[iz] = P^.dalsi^.smer) then begin
                odjezd2 := dalsiSpoj(P^.linka, P^.dalsi^.smer, rozdilCasu(kdy, P^.dalsi^.vok*IMZ+1, priznak)) + P^.dalsi^.vok*IMZ;
                if (priznak < 0) and (odjezd2 > 1440) then odjezd2 := odjezd2 - 1440;
              end
             else begin
                odjezd2 := dalsiSpoj(P^.linka, P^.dalsi^.smer, rozdilCasu(kdy, P^.dalsi^.vok*IMZ, priznak)) + P^.dalsi^.vok*IMZ;
                if (priznak < 0) and (odjezd2 > 1440) then odjezd2 := odjezd2 - 1440;
               end;
             {nastavit dobu1 a dobu2 zat�m na dobu �ek�n� p�ed p��jezdem spoje}
             doba1 := rozdilCasu(odjezd1, kdy, priznak);
             doba2 := rozdilCasu(odjezd2, kdy, priznak);
             if (P <> nil) and (P^.doZast = kam) then begin
                 minDoba := doba1 + IMZ;
                 minOdjezd := odjezd1;
                 minLinka := P^.linka;
               end
             else if (S <> nil) and (S^.doZast = kam) then  begin
                 minDoba := doba2 + IMZ;
                 minOdjezd := odjezd2;
                 minLinka := P^.linka;
               end;
             {prohled�v�n� - v�dycky se pohnu v tom sm�ru, kter� m� men� dobu}
             while (R <> nil) and (S <> nil) do begin
                 if doba1 < doba2 then {pohnout se v 1. sm�ru} begin
                     R := najdiHranu(R^.doZast, P^.linka, P^.smer);
                     doba1 := doba1 + IMZ;
                     if (R <> nil) and (R^.doZast = kam) then break;
                   end
                 else {pohnout se v 2. sm�ru} begin
                     S := najdiHranu(S^.doZast, P^.linka, P^.dalsi^.smer);
                     doba2 := doba2 + IMZ;
                     if (S <> nil) and (S^.doZast = kam) then break;
                    end;
               end; {of while}
             if (R <> nil) and (R^.doZast <> kam) then {1. sm�rem jsme nedojeli do konce}
                while (R <> nil) and (R^.doZast <> kam) do begin {proj��d�t trasu linky ve sm�ru P^.smer}
                R := najdiHranu(R^.doZast, P^.linka, P^.smer);
                doba1 := doba1 + IMZ;
                end
             else if (S <> nil) and (S^.doZast <> kam) then {2. sm�rem jsme nedojeli do konce}
                while (S <> nil) and (S^.doZast <> kam) do begin {proj��d�t trasu linky ve sm�ru P^.smer}
                S := najdiHranu(S^.doZast, P^.linka, P^.dalsi^.smer);
                doba2 := doba2 + IMZ;
                end;
             if (R <> nil) and (R^.doZast = kam) then begin {poda�ilo se dojet odkud kam}
                  doba1 := doba1 + IMZ;
                  if doba1 < minDoba then begin {nalezeno nov� nejrychlej� spojen�}
                      minDoba := doba1;
                      minLinka := P^.linka;
                      minOdjezd := odjezd1;
                    end;
                end
             else if (S <> nil) and (S^.doZast = kam) then begin {poda�ilo se dojet odkud kam}
                  doba2 := doba2 + IMZ;
                  if doba2 < minDoba then begin {nalezeno nov� nejrychlej� spojen�}
                      minDoba := doba2;
                      minLinka := P^.linka;
                      minOdjezd := odjezd2;
                    end;
                end;
             P := P^.dalsi; {oba sm�ry prohled�ny, m��u se posunout je�t� jednou}
            end
          else {prohled�vat jen jedn�m sm�rem, proto�e linka jinudy nejede} begin
               R := P;
               odjezd1 := dalsiSpoj(P^.linka, P^.smer, rozdilCasu(kdy, P^.vok*IMZ, priznak)) + P^.vok*IMZ;
               if (priznak < 0) and (odjezd1 > 1440) then odjezd1 := odjezd1 - 1440;
               doba1 := rozdilCasu(odjezd1, kdy, priznak); {inicializovat prom. doba1 na dobu �ek�n� p�ed p��jezdem linky}
               while (R <> nil) and (R^.doZast <> kam) do begin {proj��d�t trasu linky ve sm�ru P^.smer}
                R := najdiHranu(R^.doZast, P^.linka, P^.smer);
                doba1 := doba1 + IMZ;
                end;
               if (R <> nil) and (R^.doZast = kam) then begin {poda�ilo se dojet odkud kam}
                  doba1 := doba1 + IMZ;
                  if doba1 < minDoba then begin {nalezeno nov� nejrychlej� spojen�}
                      minDoba := doba1;
                      minLinka := P^.linka;
                      minOdjezd := odjezd1;
                    end;
                end;
            end;
          P := P^.dalsi;
         end;
    end;
    doba := minDoba;
    primeSpojeni.linka2 := minLinka;
    primeSpojeni.odj := minOdjezd;
end; {function primeSpojeni()}

procedure sestavPrestupy(var dalsiPrestup: uPrestup);
{P�estupy propoj� do line�rn�ho spojov�ho seznamu, aby je bylo mo�no vypsat.
Po ukon�en� procedury je v prom�nn� dalsiPrestup ukazatel na prvn� p�estup p�i nalezen� nejkrat� cest�}
var i: Integer;
begin
   dalsiPrestup := PPS[Cil];
   i := PPS[Cil]^.misto;
   PPS[Cil]^.nasl := nil;
   repeat {sledovat cestu v obr�cen�m po�ad�}
         PPS[i]^.nasl := dalsiPrestup;
         dalsiPrestup := PPS[i];
         i := PPS[i]^.misto;
   until dalsiPrestup^.misto = Start;

   if PPS[Cil]^.misto = Start then dalsiPrestup := dalsiPrestup^.nasl;
end; {procedure sestavPrestupy()}

function nazev(zast: tCisloZ): String;
{vrac� n�zev p�estupn� stanice s ��slem zast}
var i, j, k: Integer;
begin
  i := 1;
  j := nP;
  nazev := '?NAZEV';
  repeat
    k := (i + j) div 2;
    if seznamPrestupnich[k] < zast then i := k + 1
    else if seznamPrestupnich[k] > zast then j := k - 1
    else if seznamPrestupnich[k] = zast then begin
        nazev := nazvyStanic[k];
        break;
      end;
  until i > j;
end; {function nazev()}

procedure vypisVysledek(prestup: uPrestup);
{vyp��e informace o nalezen�m spojen� a p�estupech}
begin
  writeln('Odjezd linkou ', prestup^.linka2, ' v ', vytvorString(prestup^.odj));
   while prestup^.nasl <> nil do begin
       prestup := prestup^.nasl;
       write('Ve stanici ', nazev(prestup^.misto),' z linky ', prestup^.linka1);
       write(' v ', vytvorString(prestup^.prij), ' na linku ', prestup^.linka2);
       write(' s odj. ', vytvorString(prestup^.odj), #13#10);
    end;
   writeln('P��jezd ', vytvorString(Doba[Cil]+Cas), ' linkou ', prestup^.linka2);
end; {procedure vypisVysledek()}

begin
{{-------------T�LO PROGRAMU----------------------------------------}}
{--------------VSTUP U�IVATELE---------------------------------------}
write('Odkud: ');
readLn(StartString);
write('Kam: ');
readLn(CilString);
write('�as: ');
readLn(CasString);
Cas := datumZeStringu(CasString);
writeLn('--------------------'); {vodorovn� odd�lova�}
if Cas = NEPLATNE_DATUM then begin writeLn('Neplatn� datum.'); halt; end;
if StartString = CilString then begin writeLn('V�choz� a c�lov� stanice jsou toto�n�.'); halt; end;

{--------------NA�TEN� GRAFU DOPRAVN� S�T�---------------------------}
assign(F, SOUBOR_PRESTUPNI);
reset(F);
udaj := '';
lin := 0;
nP := 0;
while not eof(F) do begin
    read(F, z);
    if (z <> ' ') and (z <> #13) then udaj := udaj + z
    else begin
        if z = #13 then readLn(F);
        Val(udaj, lin, errCode);
        nP := nP + 1;
        seznamPrestupnich[nP] := lin;
        udaj := '';
      end;
  end;
close(F);

assign(F, SOUBOR_ZASTAVKY);
reset(F);
z := '/';
zast := 0;
udaj := '';
i := 0;
while not eof(F) do begin
    udaj := '';
    zast := 0;
    while TRUE do begin {do prvn� mezery na��t�m ��slo linky}
        read(F, z);
        if z = ' ' then break;
        udaj := udaj + z;
      end;
    Val(udaj, zast, errCode);
    udaj := '';
    while TRUE do begin {na��st jm�no stanice}
         read(F, z);
         if (z = #13) or eof(F) then break;
         udaj := udaj + z;
       end;
    if jePrestupni(zast) then begin
        i := i + 1;
        nazvyStanic[i] := udaj;
      end;
    if udaj = CilString then Cil := zast;
    if udaj = StartString then Start := zast;
    readln(F);
  end;
close(F);

assign(F, SOUBOR_LINKY);
reset(F);
udaj := '';
zast := 0;
predZast := 0;
L := 0;
lin := 0;
hranice := 0;
hrNalezena := FALSE;
{na��st po�et zast�vek v dopravn� s�ti}
z := '/';
while (z <> ' ') and (z <> #13) do begin
     read(F, z);
     udaj := udaj + z;
    end;
if z = #13 then readLn(F);
Delete(udaj, 1, 1); {smazat z�vorky okolo ��sla}
Delete(udaj, Length(udaj)-1, 2);
Val(udaj, N, errCode);
udaj := '';
for i := 1 to N do DS[i] := nil;
while not eof(F) do begin
      read(F, z);
      if (z <> ' ') and (z <> #13) then udaj := udaj + z {prodlu�ovat �ten� slovo}
      else begin
            udaj := UpCase(udaj);   {vyhodnotit slovo}
            if z = #13 then readLn(F);
            if Copy(udaj, 1, 1) = 'L' then begin
                    {nejprve um�stit p�ede�l� lince 2. z�voru}
                    if (lin > 0) and (hranice > 0) then
                        pridejZavoru(hranice, lin, A);
                    Delete(udaj, 1, 1); {smazat 'L'}
                    Delete(udaj, Length(udaj), 1); {smazat i syntax� vy�adovanou dvojte�ku}
                    lZav := Pos('(', udaj);
                    pZav := Pos(')', udaj);
                    Val(Copy(udaj, lZav + 1, pZav - lZav - 1), delkaLinky, errCode); {zji�t�na d�lka linky v zast�vk�ch}
                    Delete(udaj, lZav, Length(udaj)-lZav+1);
                    Val(udaj, lin, errCode);
                    L := L + 1;
                    predZast := 0;
                    zast := 0;
                    iz := -1;
                    hranice := 0;
                    hrNalezena := FALSE;
                end
            else begin
                    iz := iz + 1;
                    predZast := zast;
                    Val(udaj, zast, errCode);
                    if predZast > 0 then begin
                          pridejHranu(predZast, zast, lin, A, iz - 1);
                          pridejHranu(zast, predZast, lin, B, delkaLinky - iz - 1);
                        end;
                    {p�id�v�n� z�vory}
                    if not hrNalezena then {hled�m, kam um�stit 1. z�voru} begin
                        if jePrestupni(zast) or (zast = Cil) then begin {pokud je p�estupn�, um�stit z�voru ve sm�ru B}
                            hrNalezena := TRUE;
                            pridejZavoru(zast, lin, B);
                            hranice := zast;
                          end;
                      end
                    else begin
                            if jePrestupni(zast) or (zast = Cil) then
                                hranice := zast;
                           end;
                end;
            udaj := '';
        end;
      {kdy� slovo za��n� elkem, kon��m se �ten�m minul� linky (prom. lin) a �tu novou linku
      kdy� slovo neza��n� elkem, je to ��slo zast�vky (prom. zast), tak�e p�id�m n�sledn�ka
      t�to i minul� zast�vky a do DS p�id�m linku}
  end;
{p�idat 2. z�voru i posledn� lince}
if hrNalezena and (hranice > 0) then begin
   pridejZavoru(hranice, lin, A);
  end;
close(F);

assign(F, SOUBOR_ODJEZDY);
reset(F);
lin := 0;
udaj := '';
nalezenStrednik := FALSE;
while not eof(F) do begin
      read(F, z);
      if z = ';' then nalezenStrednik := TRUE
      else if (z <> ' ') and (z <> #13) then udaj := udaj + z
      else begin
            udaj := UpCase(udaj);
            if z = #13 then readLn(F);
            if Copy(udaj, 1, 1) = 'L' then begin
                    Delete(udaj, 1, 1); {smazat 'L'}
                    Delete(udaj, Length(udaj), 1); {smazat i syntax� vy�adovanou dvojte�ku}
                    Val(udaj, lin, errCode);
                    ctenySmer := A;
                    nalezenStrednik := FALSE;
               end
            else begin
                c := datumZeStringu(udaj);
                pridejOdjezd(lin, ctenySmer, c);
                if nalezenStrednik then ctenySmer := B;
              end;
            udaj := '';
           end;
  end;
close(F);

{vytvo�it seznam stanic spojen�ch s c�lem}
nSS := 0;
for i := 1 to nP do begin
  if jeSpolecnaLinka(seznamPrestupnich[i], Cil) then begin
      nSS := nSS + 1;
      sCilemSpojene[nSS] := seznamPrestupnich[i];
   end;
end;

{--------------HLED�N� NEJKRAT�� CESTY-------------------------------}
{inicializace}
ncdc := FALSE;
ncdps := FALSE;
for i := 1 to N do begin
     Doc[i] := TRUE;
     Doba[i] := NEKONECNO;
    end;
Doba[Start] := 0;
for i := 1 to N do
    PPS[i] := nil;
new(PPS[Start]);
PPS[Start]^.misto := Start;
PPS[Start]^.prij := Cas;
PPS[Start]^.linka1 := MaxL + 1;
PPS[Start]^.linka2 := MaxL + 1; {do startu jsme ��dnou linkou nep�ijeli}
{pr�chod grafem}
while (not ncdc) and (not ncdps) do begin
    {naj�t vrchol s nejmen� do�asnou hodnotou}
    nejblizsi := Cil;
    for i := 1 to N do
        if Doc[i] and (Doba[i] < Doba[nejblizsi]) then nejblizsi := i;
    {zde by se o�et�ila p��padn� nedostupnost c�le}
    Doc[nejblizsi] := FALSE; {prohl�sit hodnotu Doba nejbli��ho vrcholu za trvalou}
    {ov��it, zda byla nalezena nejkrat� cesta do s c�lem spojen�ch p�estupn�ch stanic}
    ncdps := TRUE;
    for i := 1 to nSS do
        if Doc[sCilemSpojene[i]] then begin
            ncdps := FALSE;
            break;
          end;
    if (nejblizsi = Cil) then
        ncdc := TRUE;
    {proj�t v�echny sousedy vrcholu nejblizsi}
    P := DS[nejblizsi];
    casProhledavani := Cas + Doba[nejblizsi];
    while P <> nil do begin
        if not P^.zavora then begin
            c := dalsiSpoj(P^.linka, P^.smer, rozdilCasu(casProhledavani, P^.vok*IMZ, priznak));
            if (priznak < 0) and (c > 1440) then c := c - 1440;
            c := c + P^.vok*IMZ; {c bude �as, kdy p�ijede dal� spoj do zast. nejblizsi}
            w := rozdilCasu(c, casProhledavani, priznak); {w nyn� p�edstavuje dobu �ek�n�}
            w := w + IMZ;
            if (w + Doba[nejblizsi]) < Doba[P^.doZast] then begin
               Doba[P^.doZast] := w + Doba[nejblizsi];
               {p�idat p�estup, pokud se linka, kterou cestujeme, m�nila}
               if (PPS[nejblizsi] <> nil) then begin
                if (PPS[nejblizsi]^.linka2 <> P^.linka) then{m�nila se linka}
                    pridejPrestup(P^.doZast, nejblizsi, casProhledavani, casProhledavani + w - IMZ, PPS[nejblizsi]^.linka2, P^.linka)
                else PPS[P^.doZast] := PPS[nejblizsi]; {jinak jedeme stejnou linkou}
                 end;
               {pokud prohled�v�n� dorazilo do n�kter� s c�lem spojen� p�estupn� stanice, zapamatovat si sm�r}
               {pro index stanice v poli sCilemSpojene[] si vyp�j��m prom�nnou iz}
               iz := indexSCilemSpojene(P^.doZast);
               if iz > 0 then {stanice je s c�lem spojen�} begin
                  smeryPrijezdu[iz] := P^.smer;
                end;
             end;
          end;
        P := P^.dalsi;
      end;
    end;

{--------------V�PIS V�SLEDKU----------------------------------------}
if ncdc then begin
   sestavPrestupy(dalsiPrestup);
   vypisVysledek(dalsiPrestup);
 end
else if ncdps then begin
    (**writeLn('�asy p��jezd� do p�estupn�ch stanic:');
    for i := 1 to nP do
      if not Doc[seznamPrestupnich[i]] then begin
      write(seznamPrestupnich[i]);
      write(' ', vytvorString(Doba[seznamPrestupnich[i]] + Cas), #13#10);
     end;**)
     {cykl�m p�es v�echny s c�lem spojen� stanice a hled�m nejkrat� p��m� spojen� }
     Doba[Cil] := NEKONECNO;
     for i := 1 to nSS do begin
         poslPrestup := primeSpojeni(sCilemSpojene[i], Cil, Cas + Doba[sCilemSpojene[i]], c);
         {c je nyn� d�lka trv�n� p��m�ho spojen� z dan� s c�lem spojen� stanice do c�le}
         if (Doba[sCilemSpojene[i]] + c) < Doba[Cil] then begin
             Doba[Cil] := Doba[sCilemSpojene[i]] + c;
             minLinka := poslPrestup.linka2;
             minOdjezd := poslPrestup.odj;
             nejblizsi := sCilemSpojene[i]; {nejblizsi nyn� poslou�� k uchov�n� stanice, v n�� se realizuje nejv�hodn�j� posledn� p�estup}
           end;
       end;
     {ov��it, zda neexistuje rychlej� p��m� spojen� do c�le}
     poslPrestup := primeSpojeni(Start, Cil, Cas, c);
     if c <= Doba[Cil] then begin
          Doba[Cil] := c;
          minLinka := poslPrestup.linka2;
          minOdjezd := poslPrestup.odj;
          nejblizsi := Start;
        end;
     if (Doba[Cil] = 0) or (PPS[nejblizsi]^.linka2 = minLinka) then
        PPS[Cil] := PPS[nejblizsi] {linka se pr�jezdem stanic� nejblizsi nem�nila}
     else {m�nila se linka}
        pridejPrestup(Cil, nejblizsi, Cas + Doba[nejblizsi], minOdjezd, PPS[nejblizsi]^.linka2, minLinka);
     sestavPrestupy(dalsiPrestup);
     vypisVysledek(dalsiPrestup);
    end;
end.
