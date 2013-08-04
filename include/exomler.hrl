-define(IS_BLANK(Blank), 
    Blank == $\s;
    Blank == $\n;
    Blank == $\t;
    Blank == $\r
).

-define(IS_QUOTE(Quote), 
    Quote == $"; 
    Quote == $'
).
