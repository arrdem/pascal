{ pasrec.pas      program to test record operations      06 Aug 09 }

program graph1(output);
label 1492, 1776;
type complex = record re, im: real end;
     color = (red, white, blue);
     pp = ^ person;
     person = record age:      integer;
                     friend:   pp;
                     location: complex;
                     favorite: color;
                     salary:   real end;
var c,d: complex; i, sum: integer;
    ac: array[1..10] of complex;
    aco: array[1..10, color] of color;
    john, mary, fred, ptr: pp;
    people: array[1..20] of person;
begin
   new(john);
   new(mary);
   new(fred);
   john^.favorite := blue;
   john^.age := 19;
1492:
   john^.friend := mary;
   john^.salary := 40000.0;
   john^.location.re := 3;
   mary^.age := 21;
   mary^.friend := fred;
1776:
   fred^.age := 20;
   fred^.friend := nil;
   john^.friend^.friend^.location.im := 4.5;
   ac[7].re := john^.salary;
   ptr := john;
   sum := 0;
   i := 1;
   while ptr <> nil do
     begin
       sum := sum + ptr^.age;
       people[i].age := ptr^.age;
       aco[i,white] := john^.favorite;
       ptr := ptr^.friend;
       i := i + 1
     end;
   write('i = ');
   writeln(i);
   write('Sum of ages = ');
   writeln(sum);
   write('Fred loc im = ');
   writeln(fred^.location.im);
   if sum < 3 then goto 1776
end.
