program zpracovaniDat;
var inF, outF: Text;
    udaj: String;
    cislo, i, errCode: Integer;
    novyRadek: Boolean;
function readWord(var F: Text; var novyRadek: Boolean): String;
var z: Char;
    udaj: String;
begin
  novyRadek := FALSE;
  udaj := '';
  repeat
    read(F, z);
    if (z <> ' ') and (z <> #13) then udaj := udaj + z;
  until (z = ' ') or (z = #13);
  if z = #13 then begin
     novyRadek := TRUE;
     readLn(F);
    end;
  readWord := udaj;
end;

begin
  assign(inF, 'in.txt');
  reset(inF);
  assign(outF, 'out.txt');
  rewrite(outF);
  i := 4; {4 hodiny}
  while not eof(inF) do begin
       udaj := readWord(inF, novyRadek);
       Val(udaj, cislo, errCode);
       if errCode = 0 then begin
         write(outF, i, ':', cislo, ' ');
       end;
       if novyRadek then i := i + 1;
     end;
  close(inF);
  close(outF);
end.
