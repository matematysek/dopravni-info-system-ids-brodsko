program DopravniInformacniSystem;
{ Dopravn¡ informaŸn¡ syst‚m }
{ Jiý¡ Zeman, 1. roŸn¡k OM }
{ letn¡ semestr 2015 }
{ Programov n¡ 2 NMIN102 }
uses modulDatum;

const SOUBOR_ODJEZDY = 'Odjezdy.txt'; {soubor obsahuje Ÿasy odjezd… z koneŸnìch stanic}
      SOUBOR_LINKY = 'Linky.txt'; {soubor obsahuje poŸet linek a zast vky, jimi§ jednotliv‚ linky proj¡§dØj¡}
      SOUBOR_ZASTAVKY = 'Zastavky.txt'; {soubor obsahuje n zvy zast vek a jejich Ÿ¡seln  oznaŸen¡}
      SOUBOR_PRESTUPNI = 'Prestupni.txt'; {v souboru nalezneme seznam pýestupn¡ch stanic}
      IMZ = 5; {kolik minut Ÿin¡ interval mezi zast vkami}
      MaxN = MaxInt - 1; {maxim ln¡ poŸet zast vek v dopravn¡ s¡ti}
      MaxL = 200; {maxim ln¡ poŸet linek v dopravn¡ s¡ti}
      MaxnP = 100; {maxim ln¡ poŸet pýestupn¡ch stanic}
type tSmer = (A, B); {smØr linky}
     tCisloZ = 1..MaxN; {Ÿ¡slo zast vky}
     tCisloL = 1..MaxL+1; {Ÿ¡slo linky}
     uHrana = ^tHrana; {uHrana je prvek datov‚ struktury DS, kter  reprezentuje graf dopravn¡ s¡tØ}
     tHrana = record
                linka: Integer;
                vok: Integer; {vzd lenost od koneŸn‚ stanice}
                smer: tSmer;
                doZast: tCisloZ; {do kter‚ zast vky hrana vede}
                zavora: Boolean; {optimalizace I. f ze prohled v n¡, m  smysl hledat pýestupn¡ stanice jen za hranami bez z vory}
                dalsi: uHrana; {ukazatel na dalç¡ hranu vedouc¡ z dan‚ho vrcholu}
               end;
     uMinuta = ^tMinuta; {uMinuta je prvek datov‚ struktury Odjezdy, kter  obsahuje Ÿasy odjezd… z koneŸnìch}
     tMinuta = record
                m: 0..59;
                dalsi: uMinuta;
               end;
     uPrestup = ^tPrestup; {uPrestup je prvek datov‚ struktury PPS, pomoc¡ n¡§ lze dohledat pýestupy na nalezen‚ nejkratç¡ cestØ}
     tPrestup = record
                    misto: tCisloZ;
                    prij, odj: Datum;
                    linka1, linka2: 1..MaxL + 1;
                    nasl: uPrestup; {n sleduj¡c¡ pýestup, POU¦ÖVAT A¦ pýi vypisov n¡ nejkratç¡ cesty!}
                end;
var Start, Cil: tCisloZ; {Ÿ¡sla vìchoz¡ a c¡lov‚ stanice}
    StartString, CilString: String;
    Cas, c: Datum; {Cas...Ÿas zadanì u§ivatelem}
    CasString: String[5];
    F: Text;
    lin, delkaLinky, iz: Integer; {iz...index zast vky v r mci linky}
    lZav, pZav: Byte; {index lev‚ a prav‚ z vorky v naŸ¡tan‚m ýetØzci}
    i, j: Integer;
    zast, predZast: Integer; {zast vka, pýedchoz¡ pýeŸten  zast vka}
    udaj: String;
    DS: array[1..MaxN] of uHrana; {dopravn¡ s¡œ}
    Odjezdy: array[1..MaxL, tSmer, 0..23] of uMinuta;
    L, N, nP: Integer; {poŸet linek, zast vek a pýest. stanic v dopravn¡ s¡ti}
    seznamPrestupnich: array[1..MaxnP] of tCisloZ; {seznam pýestupn¡ch stanic}
    sCilemSpojene: array[1..MaxnP] of tCisloZ; {seznam tØch pýestupn¡ch stanic, kter‚ maj¡ s c¡lem nØjakou spoleŸnou linku}
    smeryPrijezdu: array[1..MaxnP] of tSmer; {smØry pý¡jezd… do s c¡lem spojenìch pýestupn¡ch stanic}
    nazvyStanic: array[1..MaxnP] of String; {n zvy pýestupn¡ch stanic}
    nSS: Integer; {poŸet s c¡lem spojenìch pýestupn¡ch stanic}
    z: Char;
    errCode: Integer;
    P: uHrana;
    uM: uMinuta;
    ctenySmer: tSmer;
    nalezenStrednik: Boolean;
    priznak: ShortInt; {pý¡znak pýekroŸen¡ p…lnoci pýi vìpoŸtu rozd¡l… Ÿas…}
    Doba: array[1..MaxN] of Datum; {hodnoty vrchol… v DijkstrovØ algoritmu}
    Doc: array[1..MaxN] of Boolean; {ud v , zda je Doba pro danì vrchol jeçtØ doŸasn }
    ncdc, ncdps: Boolean; {nalezena cesta do c¡le nebo do pýestupn¡ch stanic}
    nejblizsi: tCisloZ; {Ÿ¡slo vrcholu, jeho§ hodnota se bude prohlaçovat za trvalou}
    w, casProhledavani: Datum; {v ha hrany a Ÿas, kdy prohled v n¡ dorazilo do vrcholu zpracov van‚ho v kroku Dijkstrova algoritmu}
    PPS: array[1..MaxN] of uPrestup; {jakì pýestup stanici pýedch zel}
    dalsiPrestup: uPrestup;
    hranice: Integer; {pomocn  promØnn  pro um¡sœov n¡ z vor (optimalizace I. f ze prohled v n¡)}
    hrNalezena: Boolean; {hranice nalezena, indik tor pýi Ÿten¡ souboru Linky.txt}
    poslPrestup: tPrestup;
    minOdjezd: Datum; {odjezd linky na nejkratç¡ cestØ z vrcholu, kde se kon  II. f ze prohled v n¡}
    minLinka: tCisloL; {Ÿ¡slo takov‚ linky}

procedure pridejHranu(odkud, kam: tCisloZ; l: Integer; s: tSmer; vzdOdKon: Integer);
{do datov‚ struktury DS pýid  hranu odkud kam linky l smØru s se vzd lenost¡ od koneŸn‚ vzdOdKon}
var P, Q: uHrana;
begin
 new(P);
 P^.linka := l;
 P^.smer := s;
 P^.doZast := kam;
 P^.vok := vzdOdKon;
 P^.zavora := FALSE;
 P^.dalsi := nil;
 if DS[odkud] = nil then DS[odkud] := P {pýidat prvn¡ hranu do pr zdn‚ho seznamu}
 else begin {jinak P^ zapojit na konec seznamu hran}
         Q := DS[odkud];
         while Q^.dalsi <> nil do
           Q := Q^.dalsi;
         Q^.dalsi := P;
  end;
end; {procedure pridejHranu()}

procedure pridejZavoru(kam: tCisloZ; l: Integer; s: tSmer);
{pýid  z voru do zast vky kam lince l ve smØru s}
var P: uHrana;
begin
  P := DS[kam];
  while (P <> nil) and ((P^.linka <> l) or (P^.smer <> s)) do
    P := P^.dalsi;
  if P <> nil then P^.zavora := TRUE;
end; {procedure pridejZavoru()}

procedure pridejOdjezd(l: Integer; s: tSmer; c: Datum);
{do datov‚ struktury Odjezdy pýid  odjezd linky l z koneŸn‚ ve smØru s v Ÿase c}
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
{vrac¡ prvn¡ Ÿas odjezdu z koneŸn‚ stanice linky l ve smØru s v nebo po Ÿase c}
var hy, my: ShortInt; {hodiny, minuty Ÿasu c}
    uM: uMinuta;
    zitra: Boolean; {zda spoj odj¡§d¡ a§ den pot‚}
begin
  hy := hodiny(c);
  my := minuty(c);
  uM := Odjezdy[l][s][hy];
  zitra := FALSE;
  if uM <> nil then begin {nejprve hledat spoj v t‚ sam‚ hodinØ}
     while (uM <> nil) and (uM^.m < my) do
        uM := uM^.dalsi;
     if uM <> nil then begin
         dalsiSpoj := hy*60 + uM^.m;
         exit;
        end;
    end;
  while TRUE do begin {jinak hledat spoj v n sleduj¡c¡ch hodin ch}
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
{v datov‚ struktuýe PPS nastav¡, §e posledn¡ pýestup pýed pr…jezdem zast vkou proStanici nastal
v zast vce mistoPrestupu s pý¡jezdem pr, odjezdem odj a z linky l1 na linku l2}
var prestup: uPrestup;
    i: Integer;
    posledni: Boolean; {indikuje, zda stanice proStanici je posledn¡, kter  ukazuje na nahrazovanì pýestup}
begin
  prestup := PPS[proStanici];
  posledni := TRUE;
  if prestup <> nil then {pokud stanice ji§ m  pýedchoz¡ urŸenì pýestup, nahradit ho} begin
       for i := 1 to N do
            if PPS[i] = prestup then posledni := FALSE;
       if posledni then dispose(prestup); {pýestup se m…§e odstranit, jen kdy§ je zaruŸeno, §e na nØj nic neukazuje}
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
{P…len¡m interval… hled  zast vku a v seznamu pýestupn¡ch stanic. Vrac¡
TRUE pr vØ tehdy, kdy§ zast vka a je pýestupn¡}
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
{vrac¡ index zast vky zast v poli sCilemSpojene[], nebo nulu, jestli stanice nen¡ pýestupn¡ a s c¡lem spojen }
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
{vrac¡ TRUE, existuje-li ve dvou LSS hran DS[zast1], DS[zast2] neklesaj¡c¡ch vzhledem k promØnn‚ ^.linka
nØjak  spoleŸn  linka}
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
{vrac¡ ukazatel na hranu v line rn¡m spojov‚m seznamu hran DS[zast], kter  vede ze zast vky zast linkou l ve smØru s}
var R: uHrana;
begin
  R := DS[zast];
  while (R <> nil) and ((R^.linka <> l) or (R^.smer <> s)) do
    R := R^.dalsi;
  najdiHranu := R;
end; {function najdiHranu()}

function primeSpojeni(odkud, kam: tCisloZ; kdy: Datum; var doba: Datum): tPrestup;
{Najde nejrychlejç¡ pý¡m‚ spojen¡ odkud kam po Ÿase kdy. Dobu, jak dlouho to potrv  od kdy, ulo§¡ do promØnn‚ doba. Funkce vrac¡ Ÿas odjezdu a pou§itou linku v promØnn‚ typu tPrestup}
var P, Q, R, S: uHrana; {slou§¡ k proj¡§dØn¡ seznam… DS[odkud] a DS[kam]}
   minDoba, minOdjezd: Datum; {doba trv n¡ dosud nejrychlejç¡ho spojen¡ a Ÿas jeho odjezdu}
   minLinka: tCisloL; {linka pou§it  pýi nejrychlejç¡m spojen¡}
   doba1, doba2: Datum; {pr…bØ§n‚ doby prohled v n¡ obØma smØry}
   odjezd1, odjezd2: Datum; {Ÿasy odjezd… v obou smØrech}
   priznak: ShortInt;
begin
  if odkud = kam then begin {odkud a kam splìvaj¡ v jednu stanici}
      doba := 0;
      exit;
    end;
  P := DS[odkud]; {P, Q budou proj¡§dØt seznamy hran vedouc¡ch z vrchol… odkud a kam}
  Q := DS[kam];
  minDoba := NEKONECNO;
  minLinka := MaxL + 1;
  minOdjezd := NEKONECNO;
  doba := NEKONECNO;
  if (P = nil) or (Q = nil) then
      {odkud nebo kam nevede § dn  linka} exit;
  while (P <> nil) and (Q <> nil) do begin {dokud ani jeden seznam hran nedojel do konce}
      if P^.linka > Q^.linka  then Q := Q^.dalsi
      else if Q^.linka > P^.linka then P := P^.dalsi
      else {P^.linka = Q^.linka} begin
          {nalezena spoleŸn  linka stanic odkud a kam, spustit PROHLEDµVµNÖ}
          if (P^.dalsi <> nil) and (P^.dalsi^.linka = P^.linka) then begin {prohled vat
            obØma smØry}
             R := P;
             S := P^.dalsi;
             iz := indexSCilemSpojene(odkud); {zjistit zda je odkud spojen  s c¡lem}
             if (iz > 0) and (PPS[odkud]^.linka2 = P^.linka) and (smeryPrijezdu[iz] = P^.smer) then begin {umo§nit pokraŸovat linkou, kterou prohled v n¡ do dan‚ho vrcholu dorazilo v I. f zi}
                odjezd1 := dalsiSpoj(P^.linka, P^.smer, rozdilCasu(kdy, P^.vok*IMZ+1, priznak)) + P^.vok*IMZ;
                if (priznak < 0) and (odjezd1 > 1440) then odjezd1 := odjezd1 - 1440; {p…lnoc byla pýekroŸena jen kv…li pýenesen¡ vyhled v n¡ spoje na koneŸnou}
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
             {nastavit dobu1 a dobu2 zat¡m na dobu Ÿek n¡ pýed pý¡jezdem spoje}
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
             {prohled v n¡ - v§dycky se pohnu v tom smØru, kterì m  menç¡ dobu}
             while (R <> nil) and (S <> nil) do begin
                 if doba1 < doba2 then {pohnout se v 1. smØru} begin
                     R := najdiHranu(R^.doZast, P^.linka, P^.smer);
                     doba1 := doba1 + IMZ;
                     if (R <> nil) and (R^.doZast = kam) then break;
                   end
                 else {pohnout se v 2. smØru} begin
                     S := najdiHranu(S^.doZast, P^.linka, P^.dalsi^.smer);
                     doba2 := doba2 + IMZ;
                     if (S <> nil) and (S^.doZast = kam) then break;
                    end;
               end; {of while}
             if (R <> nil) and (R^.doZast <> kam) then {1. smØrem jsme nedojeli do konce}
                while (R <> nil) and (R^.doZast <> kam) do begin {proj¡§dØt trasu linky ve smØru P^.smer}
                R := najdiHranu(R^.doZast, P^.linka, P^.smer);
                doba1 := doba1 + IMZ;
                end
             else if (S <> nil) and (S^.doZast <> kam) then {2. smØrem jsme nedojeli do konce}
                while (S <> nil) and (S^.doZast <> kam) do begin {proj¡§dØt trasu linky ve smØru P^.smer}
                S := najdiHranu(S^.doZast, P^.linka, P^.dalsi^.smer);
                doba2 := doba2 + IMZ;
                end;
             if (R <> nil) and (R^.doZast = kam) then begin {podaýilo se dojet odkud kam}
                  doba1 := doba1 + IMZ;
                  if doba1 < minDoba then begin {nalezeno nov‚ nejrychlejç¡ spojen¡}
                      minDoba := doba1;
                      minLinka := P^.linka;
                      minOdjezd := odjezd1;
                    end;
                end
             else if (S <> nil) and (S^.doZast = kam) then begin {podaýilo se dojet odkud kam}
                  doba2 := doba2 + IMZ;
                  if doba2 < minDoba then begin {nalezeno nov‚ nejrychlejç¡ spojen¡}
                      minDoba := doba2;
                      minLinka := P^.linka;
                      minOdjezd := odjezd2;
                    end;
                end;
             P := P^.dalsi; {oba smØry prohled ny, m…§u se posunout jeçtØ jednou}
            end
          else {prohled vat jen jedn¡m smØrem, proto§e linka jinudy nejede} begin
               R := P;
               odjezd1 := dalsiSpoj(P^.linka, P^.smer, rozdilCasu(kdy, P^.vok*IMZ, priznak)) + P^.vok*IMZ;
               if (priznak < 0) and (odjezd1 > 1440) then odjezd1 := odjezd1 - 1440;
               doba1 := rozdilCasu(odjezd1, kdy, priznak); {inicializovat prom. doba1 na dobu Ÿek n¡ pýed pý¡jezdem linky}
               while (R <> nil) and (R^.doZast <> kam) do begin {proj¡§dØt trasu linky ve smØru P^.smer}
                R := najdiHranu(R^.doZast, P^.linka, P^.smer);
                doba1 := doba1 + IMZ;
                end;
               if (R <> nil) and (R^.doZast = kam) then begin {podaýilo se dojet odkud kam}
                  doba1 := doba1 + IMZ;
                  if doba1 < minDoba then begin {nalezeno nov‚ nejrychlejç¡ spojen¡}
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
{Pýestupy propoj¡ do line rn¡ho spojov‚ho seznamu, aby je bylo mo§no vypsat.
Po ukonŸen¡ procedury je v promØnn‚ dalsiPrestup ukazatel na prvn¡ pýestup pýi nalezen‚ nejkratç¡ cestØ}
var i: Integer;
begin
   dalsiPrestup := PPS[Cil];
   i := PPS[Cil]^.misto;
   PPS[Cil]^.nasl := nil;
   repeat {sledovat cestu v obr cen‚m poýad¡}
         PPS[i]^.nasl := dalsiPrestup;
         dalsiPrestup := PPS[i];
         i := PPS[i]^.misto;
   until dalsiPrestup^.misto = Start;

   if PPS[Cil]^.misto = Start then dalsiPrestup := dalsiPrestup^.nasl;
end; {procedure sestavPrestupy()}

function nazev(zast: tCisloZ): String;
{vrac¡ n zev pýestupn¡ stanice s Ÿ¡slem zast}
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
{vyp¡çe informace o nalezen‚m spojen¡ a pýestupech}
begin
  writeln('Odjezd linkou ', prestup^.linka2, ' v ', vytvorString(prestup^.odj));
   while prestup^.nasl <> nil do begin
       prestup := prestup^.nasl;
       write('Ve stanici ', nazev(prestup^.misto),' z linky ', prestup^.linka1);
       write(' v ', vytvorString(prestup^.prij), ' na linku ', prestup^.linka2);
       write(' s odj. ', vytvorString(prestup^.odj), #13#10);
    end;
   writeln('Pý¡jezd ', vytvorString(Doba[Cil]+Cas), ' linkou ', prestup^.linka2);
end; {procedure vypisVysledek()}

begin
{{-------------T·LO PROGRAMU----------------------------------------}}
{--------------VSTUP U¦IVATELE---------------------------------------}
write('Odkud: ');
readLn(StartString);
write('Kam: ');
readLn(CilString);
write('¬as: ');
readLn(CasString);
Cas := datumZeStringu(CasString);
writeLn('--------------------'); {vodorovnì oddØlovaŸ}
if Cas = NEPLATNE_DATUM then begin writeLn('Neplatn‚ datum.'); halt; end;
if StartString = CilString then begin writeLn('Vìchoz¡ a c¡lov  stanice jsou toto§n‚.'); halt; end;

{--------------NA¬TENÖ GRAFU DOPRAVNÖ SÖT·---------------------------}
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
    while TRUE do begin {do prvn¡ mezery naŸ¡t m Ÿ¡slo linky}
        read(F, z);
        if z = ' ' then break;
        udaj := udaj + z;
      end;
    Val(udaj, zast, errCode);
    udaj := '';
    while TRUE do begin {naŸ¡st jm‚no stanice}
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
{naŸ¡st poŸet zast vek v dopravn¡ s¡ti}
z := '/';
while (z <> ' ') and (z <> #13) do begin
     read(F, z);
     udaj := udaj + z;
    end;
if z = #13 then readLn(F);
Delete(udaj, 1, 1); {smazat z vorky okolo Ÿ¡sla}
Delete(udaj, Length(udaj)-1, 2);
Val(udaj, N, errCode);
udaj := '';
for i := 1 to N do DS[i] := nil;
while not eof(F) do begin
      read(F, z);
      if (z <> ' ') and (z <> #13) then udaj := udaj + z {prodlu§ovat Ÿten‚ slovo}
      else begin
            udaj := UpCase(udaj);   {vyhodnotit slovo}
            if z = #13 then readLn(F);
            if Copy(udaj, 1, 1) = 'L' then begin
                    {nejprve um¡stit pýedeçl‚ lince 2. z voru}
                    if (lin > 0) and (hranice > 0) then
                        pridejZavoru(hranice, lin, A);
                    Delete(udaj, 1, 1); {smazat 'L'}
                    Delete(udaj, Length(udaj), 1); {smazat i syntax¡ vy§adovanou dvojteŸku}
                    lZav := Pos('(', udaj);
                    pZav := Pos(')', udaj);
                    Val(Copy(udaj, lZav + 1, pZav - lZav - 1), delkaLinky, errCode); {zjiçtØna d‚lka linky v zast vk ch}
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
                    {pýid v n¡ z vory}
                    if not hrNalezena then {hled m, kam um¡stit 1. z voru} begin
                        if jePrestupni(zast) or (zast = Cil) then begin {pokud je pýestupn¡, um¡stit z voru ve smØru B}
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
      {kdy§ slovo zaŸ¡n  elkem, konŸ¡m se Ÿten¡m minul‚ linky (prom. lin) a Ÿtu novou linku
      kdy§ slovo nezaŸ¡n  elkem, je to Ÿ¡slo zast vky (prom. zast), tak§e pýid m n sledn¡ka
      t‚to i minul‚ zast vky a do DS pýid m linku}
  end;
{pýidat 2. z voru i posledn¡ lince}
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
                    Delete(udaj, Length(udaj), 1); {smazat i syntax¡ vy§adovanou dvojteŸku}
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

{vytvoýit seznam stanic spojenìch s c¡lem}
nSS := 0;
for i := 1 to nP do begin
  if jeSpolecnaLinka(seznamPrestupnich[i], Cil) then begin
      nSS := nSS + 1;
      sCilemSpojene[nSS] := seznamPrestupnich[i];
   end;
end;

{--------------HLEDµNÖ NEJKRATæÖ CESTY-------------------------------}
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
PPS[Start]^.linka2 := MaxL + 1; {do startu jsme § dnou linkou nepýijeli}
{pr…chod grafem}
while (not ncdc) and (not ncdps) do begin
    {naj¡t vrchol s nejmenç¡ doŸasnou hodnotou}
    nejblizsi := Cil;
    for i := 1 to N do
        if Doc[i] and (Doba[i] < Doba[nejblizsi]) then nejblizsi := i;
    {zde by se oçetýila pý¡padn  nedostupnost c¡le}
    Doc[nejblizsi] := FALSE; {prohl sit hodnotu Doba nejbli§ç¡ho vrcholu za trvalou}
    {ovØýit, zda byla nalezena nejkratç¡ cesta do s c¡lem spojenìch pýestupn¡ch stanic}
    ncdps := TRUE;
    for i := 1 to nSS do
        if Doc[sCilemSpojene[i]] then begin
            ncdps := FALSE;
            break;
          end;
    if (nejblizsi = Cil) then
        ncdc := TRUE;
    {proj¡t vçechny sousedy vrcholu nejblizsi}
    P := DS[nejblizsi];
    casProhledavani := Cas + Doba[nejblizsi];
    while P <> nil do begin
        if not P^.zavora then begin
            c := dalsiSpoj(P^.linka, P^.smer, rozdilCasu(casProhledavani, P^.vok*IMZ, priznak));
            if (priznak < 0) and (c > 1440) then c := c - 1440;
            c := c + P^.vok*IMZ; {c bude Ÿas, kdy pýijede dalç¡ spoj do zast. nejblizsi}
            w := rozdilCasu(c, casProhledavani, priznak); {w nyn¡ pýedstavuje dobu Ÿek n¡}
            w := w + IMZ;
            if (w + Doba[nejblizsi]) < Doba[P^.doZast] then begin
               Doba[P^.doZast] := w + Doba[nejblizsi];
               {pýidat pýestup, pokud se linka, kterou cestujeme, mØnila}
               if (PPS[nejblizsi] <> nil) then begin
                if (PPS[nejblizsi]^.linka2 <> P^.linka) then{mØnila se linka}
                    pridejPrestup(P^.doZast, nejblizsi, casProhledavani, casProhledavani + w - IMZ, PPS[nejblizsi]^.linka2, P^.linka)
                else PPS[P^.doZast] := PPS[nejblizsi]; {jinak jedeme stejnou linkou}
                 end;
               {pokud prohled v n¡ dorazilo do nØkter‚ s c¡lem spojen‚ pýestupn¡ stanice, zapamatovat si smØr}
               {pro index stanice v poli sCilemSpojene[] si vyp…jŸ¡m promØnnou iz}
               iz := indexSCilemSpojene(P^.doZast);
               if iz > 0 then {stanice je s c¡lem spojen } begin
                  smeryPrijezdu[iz] := P^.smer;
                end;
             end;
          end;
        P := P^.dalsi;
      end;
    end;

{--------------VíPIS VíSLEDKU----------------------------------------}
if ncdc then begin
   sestavPrestupy(dalsiPrestup);
   vypisVysledek(dalsiPrestup);
 end
else if ncdps then begin
    (**writeLn('¬asy pý¡jezd… do pýestupn¡ch stanic:');
    for i := 1 to nP do
      if not Doc[seznamPrestupnich[i]] then begin
      write(seznamPrestupnich[i]);
      write(' ', vytvorString(Doba[seznamPrestupnich[i]] + Cas), #13#10);
     end;**)
     {cykl¡m pýes vçechny s c¡lem spojen‚ stanice a hled m nejkratç¡ pý¡m‚ spojen¡ }
     Doba[Cil] := NEKONECNO;
     for i := 1 to nSS do begin
         poslPrestup := primeSpojeni(sCilemSpojene[i], Cil, Cas + Doba[sCilemSpojene[i]], c);
         {c je nyn¡ d‚lka trv n¡ pý¡m‚ho spojen¡ z dan‚ s c¡lem spojen‚ stanice do c¡le}
         if (Doba[sCilemSpojene[i]] + c) < Doba[Cil] then begin
             Doba[Cil] := Doba[sCilemSpojene[i]] + c;
             minLinka := poslPrestup.linka2;
             minOdjezd := poslPrestup.odj;
             nejblizsi := sCilemSpojene[i]; {nejblizsi nyn¡ poslou§¡ k uchov n¡ stanice, v n¡§ se realizuje nejvìhodnØjç¡ posledn¡ pýestup}
           end;
       end;
     {ovØýit, zda neexistuje rychlejç¡ pý¡m‚ spojen¡ do c¡le}
     poslPrestup := primeSpojeni(Start, Cil, Cas, c);
     if c <= Doba[Cil] then begin
          Doba[Cil] := c;
          minLinka := poslPrestup.linka2;
          minOdjezd := poslPrestup.odj;
          nejblizsi := Start;
        end;
     if (Doba[Cil] = 0) or (PPS[nejblizsi]^.linka2 = minLinka) then
        PPS[Cil] := PPS[nejblizsi] {linka se pr…jezdem stanic¡ nejblizsi nemØnila}
     else {mØnila se linka}
        pridejPrestup(Cil, nejblizsi, Cas + Doba[nejblizsi], minOdjezd, PPS[nejblizsi]^.linka2, minLinka);
     sestavPrestupy(dalsiPrestup);
     vypisVysledek(dalsiPrestup);
    end;
end.
