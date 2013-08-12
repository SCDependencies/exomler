-define(IS_BLANK(Blank), 
    Blank == $\s;
    Blank == $\t;
    Blank == $\n;
    Blank == $\r
).

-define(IS_QUOTE(Quote), 
    Quote == $"; 
    Quote == $'
).
