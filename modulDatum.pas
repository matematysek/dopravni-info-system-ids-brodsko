unit modulDatum;
{modul obsahuje datov� typ Datum pou��van� v programu DopravniInformacniSystem}
interface
const NEPLATNE_DATUM = 65534;
      NEKONECNO = 65535;
type Datum = Word;
function datumZeStringu(s: String): Datum;
function vytvorString(dat: Datum): String;
function dny(d: Datum): Byte;
function hodiny(d: Datum): Byte;
function minuty(d: Datum): Byte;
function rozdilCasu(d1: Datum; d2: Datum; var priznak: ShortInt): Datum;
function soucetCasu(d1: Datum; d2: Datum; var priznak: ShortInt): Datum;

implementation
function datumZeStringu(s: String): Datum;
{�et�zec s ve form�tu dd:hh:mm p�evede na Datum}
var d, h, m: Byte; {dny, hodiny, minuty}
    pd: array[1..2] of Byte; {prvn� a druh� v�skyt dvojte�ky}
    i, j: Byte;
    z: Char;
    errCode, tec: Integer; {total error code}
    dat: Datum; {n�vratov� hodnota}
begin
   dat := NEPLATNE_DATUM;
   pd[1] := 0;
   pd[2] := 0;
   i := 0;
   for j := 1 to Length(s) do begin
         z := s[j];
         if not ((z in ['0'..'9']) or (z = ':')) then break;
         if z = ':' then begin
                i := i + 1;
                if i > 2 then {v�ce dvojte�ek se nep�ipou�t�} break;
                pd[i] := j;
            end;
    end;
    tec := 0;
    errCode := 0;
   if (pd[2] > 0) and (i <= 2) then {nalezeny dv� dvojte�ky} begin
        Val(Copy(s, 1, pd[1] - 1), d, errCode);
        tec := errCode;
        Val(Copy(s, pd[1] + 1, pd[2] - pd[1] - 1), h, errCode);
        tec := tec + errCode;
        Val(Copy(s, pd[2] + 1, Length(s) - pd[2]), m, errCode);
        tec := tec + errCode;
        if tec = 0 then dat := m + h*60 + d*1440;
      end
   else if (pd[1] > 0) and (i <= 2) then {jedna dvojte�ka} begin
       Val(Copy(s, 1, pd[1] - 1), h, errCode);
       tec := errCode;
       Val(Copy(s, pd[1] + 1, Length(s) - pd[1]), m, errCode);
       tec := tec + errCode;
       if tec = 0 then dat := m + h*60;
      end;
   if dat > NEPLATNE_DATUM then dat := NEPLATNE_DATUM; {proti p�ete�en�}
   datumZeStringu := dat;
end; {function datumZeStringu()}

function doplnNulou(k: Integer): String;
{jednocifern� ��slo k dopln� nulou, del� nech� beze zm�ny}
var s: String;
begin
  Str(k, s);
  if (k > -10) and (k < 10) then
    s := '0' + s;
  doplnNulou := s;
end; {function doplnNulou()}

function vytvorString(dat: Datum): String;
{Datum dat p�evede na �et�zec ve form�tu hh:mm. P�esahuje-li toto datum hranici 24 hodin, p�ipoj�
'n�sl. dne'}
var d, h, m: Byte;
    s: String;
begin
  d := dny(dat);
  h := hodiny(dat);
  m := minuty(dat);
  if d > 0 then
     (**s := doplnNulou(d) + ':' + doplnNulou(h) + ':' + doplnNulou(m)**)
     s := doplnNulou(h) + ':' + doplnNulou(m) + ' n�sl. dne'
  else s := doplnNulou(h) + ':' + doplnNulou(m);
  vytvorString := s;
end; {function vytvorString()}

function dny(d: Datum): Byte;
{vrac� po�et dn� v Datu d}
var v: Byte;
begin
  v := d div 1440;
  dny := v;
end; {function dny()}

function hodiny(d: Datum): Byte;
{vrac� po�et hodin v Datu d}
var v: Byte;
begin
  v := (d mod 1440) div 60;
  hodiny := v;
end; {function hodiny()}

function minuty(d: Datum): Byte;
{vrac� po�et minut v Datu d}
var v: Byte;
begin
  v := (d mod 1440) mod 60;
  minuty := v;
end; {function minuty()}

function rozdilCasu(d1: Datum; d2: Datum; var priznak: ShortInt): Datum;
{Vrac� rozd�l d1-d2. Pokud by m�l b�t z�porn�, p�i��t� 24 hodin, dokud nep�ekro�� nulu.}
{priznak...ud�v�, zda a kolikr�t byla p�ekro�ena p�lnoc, jde o nekladn� ��slo}
var rozdil: Integer;
begin
  priznak := 0;
  rozdil := d1 - d2;
  while rozdil < 0 do begin
    priznak := priznak - 1;
    rozdil := rozdil + 1440;
  end;
end; {function rozdilCasu()}

function soucetCasu(d1: Datum; d2: Datum; var priznak: ShortInt): Datum;
{vrac� sou�et d1+d2}
{priznak...ud�v�, zda a kolikr�t byl p�ekro�en den, jde o nez�porn� ��slo}
var soucet: Integer;
begin
  priznak := 0;
  soucet := d1 + d2;
  while soucet >= 1440 do begin
    priznak := priznak + 1;
    soucet :=  soucet - 1440;
  end;
end; {function soucetCasu()}

begin

end.
