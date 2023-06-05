program OdjezdovaTabule;

uses modulDatum;

const SOUBOR_ODJEZDY = 'Odjezdy.txt';
      SOUBOR_LINKY = 'Linky.txt';
      SOUBOR_ZASTAVKY = 'Zastavky.txt';
      SOUBOR_PRESTUPNI = 'Prestupni.txt';
      IMZ = 5; {kolik minut Ÿin¡ interval mezi zast vkami}
      MaxN = MaxInt - 1; {maxim ln¡ poŸet zast vek v dopravn¡ s¡ti}
      MaxL = 200; {maxim ln¡ poŸet linek v dopravn¡ s¡ti}
      MaxnP = 100; {maxim ln¡ poŸet pýestupn¡ch stanic}
type tSmer = (A, B);
     tCisloZ = 1..MaxN; {Ÿ¡slo zast vky}
     tCisloL = 1..MaxL+1;
     uHrana = ^tHrana;
     tHrana = record
                linka: Integer;
                vok: Integer; {vzd lenost od koneŸn‚ stanice}
                smer: tSmer;
                doZast: tCisloZ;
                dalsi: uHrana;
               end;
     uMinuta = ^tMinuta;
     tMinuta = record
                m: 0..59;
                dalsi: uMinuta;
               end;
     uPrestup = ^tPrestup;
     tPrestup = record
                    misto: tCisloZ;
                    prij, odj: Datum;
                    linka1, linka2: 1..MaxL + 1;
                    nasl: uPrestup; {n sleduj¡c¡ pýestup, POU¦ÖVAT A¦ pýi vypisov n¡ nejkratç¡ cesty!}
                end;
var Start, Cil: tCisloZ; {Ÿ¡sla vìchoz¡ a c¡lov‚ stanice}
    Cas, c: Datum;
    CasString: String[5];
    F: Text;
    lin, delkaLinky, iz: Integer; {iz...index zast vky v r mci linky}
    lZav, pZav: Byte;
    i, j: Integer;
    zast, predZast: Integer; {zast vka, pýedchoz¡ pýeŸten  zast vka}
    udaj: String;
    (**V, E: array[1..MaxN + 1] of 1..MaxN + 1; {pole pro seznam n sledn¡k…}**)
    DS: array[1..MaxN] of uHrana; {dopravn¡ s¡œ}
    Odjezdy: array[1..MaxL, tSmer, 0..23] of uMinuta;
    L, N, nP: Integer; {poŸet linek, zast vek a pýest. stanic v dopravn¡ s¡ti}
    seznamPrestupnich: array[1..MaxnP] of tCisloZ; {seznam pýestupn¡ch stanic}
    z: Char;
    errCode: Integer;
    P: uHrana;
    uM: uMinuta;
    ctenySmer: tSmer;
    nalezenStrednik: Boolean;
    priznak, bilanceDni: ShortInt;
    Doba: array[1..MaxN] of Datum; {hodnoty vrchol… v DijkstrovØ algoritmu}
    Doc: array[1..MaxN] of Boolean; {ud v , zda je Doba pro danì vrchol jeçtØ doŸasn }
    ncdcndps: Boolean; {nalezena cesta do c¡le nebo do pýestupn¡ch stanic}
    nejblizsi: tCisloZ; {Ÿ¡slo vrcholu, jeho§ hodnota se bude prohlaçovat za trvalou}
    w, casProhledavani: Datum; {v ha hrany}
    PPS: array[1..MaxN] of uPrestup; {jakì pýestup stanici pýedch zel}
    dalsiPrestup: uPrestup;

procedure pridejHranu(odkud, kam: tCisloZ; l: Integer; s: tSmer; vzdOdKon: Integer);
var P, Q: uHrana;
begin
 new(P);
 P^.linka := l;
 P^.smer := s;
 P^.doZast := kam;
 P^.vok := vzdOdKon;
 P^.dalsi := nil;
 if DS[odkud] = nil then DS[odkud] := P
 else begin
         Q := DS[odkud];
         while Q^.dalsi <> nil do
           Q := Q^.dalsi;
         Q^.dalsi := P;
  end;
end; {procedure pridejHranu()}

procedure pridejOdjezd(l: Integer; s: tSmer; c: Datum);
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
var hy, my: ShortInt;
    uM: uMinuta;
    zitra: Boolean;
begin
  hy := hodiny(c);
  my := minuty(c);
  uM := Odjezdy[l][s][hy];
  zitra := FALSE;
  if uM <> nil then begin
     while (uM <> nil) and (uM^.m < my) do
        uM := uM^.dalsi;
     if uM <> nil then begin
         dalsiSpoj := hy*60 + uM^.m;
         exit;
        end;
    end;
  while TRUE do begin
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
var prestup: uPrestup;
    i: Integer;
    posledni: Boolean; {indikuje, zda stanice proStanici je posledn¡, kter  ukazuje na nahrazovanì pýestup}
begin
  prestup := PPS[proStanici];
  posledni := TRUE;
  if prestup <> nil then {pokud stanice ji§ m  pýedchoz¡ urŸen pýestup, nahradit ho} begin
       for i := 1 to N do
            if PPS[i] = prestup then posledni := FALSE;
       if posledni then dispose(prestup);
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

begin
{{-------------T·LO PROGRAMU----------------------------------------}}
{--------------VSTUP U¦IVATELE---------------------------------------}
readLn(Start); {readLn(Cil);}
readLn(CasString);
Cas := datumZeStringu(CasString);
if Cas = NEPLATNE_DATUM then begin writeLn('Neplatn‚ datum.'); halt; end;

{--------------NA¬TENÖ GRAFU DOPRAVNÖ SÖT·---------------------------}
assign(F, SOUBOR_LINKY);
reset(F);
udaj := '';
zast := 0;
predZast := 0;
L := 0;
{naŸ¡st poŸet zast vek v dopravn¡ s¡ti}
z := '/';
while (z <> ' ') and (z <> #13) do begin
     read(F, z);
     udaj := udaj + z;
    end;
if z = #13 then readLn(F);
Delete(udaj, 1, 1);
Delete(udaj, Length(udaj)-1, 2);
Val(udaj, N, errCode);
udaj := '';
for i := 1 to N do DS[i] := nil;
while not eof(F) do begin
      read(F, z);
      if (z <> ' ') and (z <> #13) then udaj := udaj + z
      else begin
            udaj := UpCase(udaj);
            if z = #13 then readLn(F);
            if Copy(udaj, 1, 1) = 'L' then begin
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
                end
            else begin
                    iz := iz + 1;
                    predZast := zast;
                    Val(udaj, zast, errCode);
                    if predZast > 0 then begin
                          pridejHranu(predZast, zast, lin, A, iz - 1);
                          pridejHranu(zast, predZast, lin, B, delkaLinky - iz - 1);
                        end;
                end;
            udaj := '';
        end;
      {kdy§ slovo zaŸ¡n  elkem, konŸ¡m se Ÿten¡m minul‚ linky (prom. lin) a Ÿtu novou linku
      kdy§ slovo nezaŸ¡n  elkem, je to Ÿ¡slo zast vky (prom. zast), tak§e pýid m n sledn¡ka
      t‚to i minul‚ zast vky a do DS pýid m linku}
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


P := DS[Start];
writeLn('Odjezdov  tabule');
while P <> nil do begin
     write('Linka Ÿ. ', P^.linka, ' ve smØru ', P^.smer, ' do zast vky ', P^.doZast);
     c := dalsiSpoj(P^.linka, P^.smer, rozdilCasu(Cas, P^.vok*IMZ, priznak));
     c := soucetCasu(c, P^.vok*IMZ, priznak);
     write(': odjezd ', hodiny(c), ':', minuty(c));
     writeLn;
     P := P^.dalsi;
    end;

end.
