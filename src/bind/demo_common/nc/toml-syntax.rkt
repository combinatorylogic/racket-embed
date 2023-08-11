#lang peg

Xtoml <-- cSP Xentry+ cSP lastCOMMENT? EOI;
Xentry <- Xgroup / Xpair;
Xgroup <-- Xheader Xpair*;
Xheader <- LP (! ']' .)* RP;
Xpair <-- Xident EQ Xvalue;
Xident <- ([a-z] / [A-Z] / '_' / '-' / '.') ([a-z] / [A-Z] / [0-9] / '_' / '-' / '.')* cSP;
Xvalue <- Xnumvalue / Xcornumvalue / Xlistvalue / Xtablevalue / Xstringvalue / Xboolvalue;
Xnumvalue <-- ('-'/'+')?[0-9]+ (('.' [0-9]*)? ([e/E] ('+'/'-')? [0-9]+)) cSP;
Xcornumvalue <-- ('-'/'+')?[0-9]+ ('.' [0-9]*)? cSP;
Xboolvalue <- Xbooltrue / Xboolfalse;
Xbooltrue <-- 'true' cSP;
Xboolfalse <-- 'false' cSP;
Xlistvalue <-- LP Xvalues COMMA? RP;
Xvalues <- (Xvalue COMMA Xvalues) / Xvalue;
Xtablevalue <-- LCP Xpairs COMMA? RCP;
Xpairs <- (Xpair COMMA Xpairs) / Xpair;
Xstringvalue <-- QT (! QT .)* QT cSP;
cSPx < [ \t\n]*;
EQ < '=' cSP;
LP < '[' cSP;
RP < ']' cSP;
LCP < '{' cSP;
RCP < '}' cSP;
COMMA < ',' cSP;
cSP < cSPx (cSPx '#' (! CR .)* CR cSPx)*;
lastCOMMENT < (cSPx '#' (! CR .)*);
CR < [\n];
QT < '"';

EOI < ! . ;



