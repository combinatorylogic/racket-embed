#lang peg

fundefs <-- fundefsrest cSP EOI;
fundefsrest <- fundef fundefsrest / fundef;
fundef <-- FUN LB sstr COMMA ident COMMA type (COMMA arglist)? RB;

FUN < cSP 'FUN';
LB < cSP '(';
RB < cSP ')';
COMMA < cSP ',';
ident <-- cSP identTk;
identTk <- ([a-zA-Z]/'_') ([a-zA-Z0-9]/'_'/'-')*;
sstr <-- cSP QU (!'"' .)+ QU;
QU < '"';
cSP < ([ \t\n]/cComment)*;
cComment < slComment / mlComment;
slComment < '//' (! [\n] .)* [\n];
mlComment < '/*' (! '*/' .)* '*/';

type <- ident;

arglist <- (arg COMMA arglist) / arg;

arg <-- ident ident;


EOI < ! . ;

           
