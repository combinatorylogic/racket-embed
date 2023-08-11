#lang peg

iinfix <-- iexpr0 cSP EOI;
iexpr0 <- leftass / iexpr1;
leftass <-- iadd / isub;
iadd <-- iexpr1 PLUS iexpr0;
isub <-- iexpr1 MINUS iexpr0;
iexpr1 <- imul / idiv / iexpr2;
imul <-- iexpr2 MUL iexpr1;
idiv <-- iexpr2 DIV iexpr1;
iexpr2 <- iterm;
iterm <- ilet / ilambda / ifnapp  / iwrap / ivar / iconst;
iwrap <- LP iexpr0 RP;
ivar <-- Xident;
iconst <- Xnumvalue / Xcornumvalue;
ifnapp <-- ivar LP fnargs RP;
ilet <-- LET ivar EQ iexpr0 IN iexpr0;
ilambda <-- FN LP iargs RP iexpr0;
fnargs <- fnpair / fnone;
fnone <-- iexpr0;
fnpair <-- iexpr0 COMMA fnargs;
iargs <- fnapair / fnaone;
fnaone <-- ivar;
fnapair <-- ivar COMMA iargs;
Xident <- cSP ([a-z] / [A-Z] / '_' / '.')+;
Xnumvalue <-- cSP ('-'/'+')?[0-9]+ (('.' [0-9]*)? ([e/E] ('+'/'-')? [0-9]+));
Xcornumvalue <-- cSP ('-'/'+')?[0-9]+ ('.' [0-9]*)?;
cSP < [ \t\n]*;
LP < cSP '(';
RP < cSP ')';
COMMA < cSP ',';
PLUS < cSP '+';
MINUS < cSP '-';
MUL < cSP '*';
DIV < cSP '/';
FN < cSP 'fn';
LET < cSP 'let';
IN < cSP 'in';
EQ < cSP '=';


EOI < ! . ;


