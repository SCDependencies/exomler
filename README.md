Exomler
========

It is a very simple DOM XML parser that can work only with valid and well-formed XML.

It is fast enough, convenient and has very low memory footprint due to binary usage. Really!

Usage:

```
{Tag, Attrs, Content} = exomler:decode(XML). 
```

Where Tag is binary name of root tag, Attrs is a {Key,Value} list of attrs and Content is
list of inner tags or Text which is binary.

For example:

```
XML = <<"<html key=\"value\">Body</html>">>.
{<<"html">>, [{<<"key">>, <<"value">>}], [<<"Body">>]} = exomler:decode(XML).
XML = exomler:encode({<<"html">>, [{<<"key">>, <<"value">>}], [<<"Body">>]}).

```


Benchmarking
------------

```
./exomler_bench test_01.xml 1000
-------------------------------------------------------
     parser    min time    all time    memory     speed
-------------------------------------------------------
      xmerl      166mcs      193mls      73KB     1MB/s
     erlsom       34mcs       69mls      20KB     4MB/s
    exomler       23mcs       50mls      16KB     6MB/s
-------------------------------------------------------
./exomler_bench test_02.xml 1000
-------------------------------------------------------
     parser    min time    all time    memory     speed
-------------------------------------------------------
      xmerl      398mcs      451mls     159KB     2MB/s
     erlsom       98mcs      150mls     106KB     7MB/s
    exomler       56mcs       89mls      41KB    12MB/s
-------------------------------------------------------
./exomler_bench test_03.xml 1000
-------------------------------------------------------
     parser    min time    all time    memory     speed
-------------------------------------------------------
      xmerl     1742mcs     1869mls     672KB     2MB/s
     erlsom      618mcs      720mls     171KB     6MB/s
    exomler      385mcs      448mls      53KB    10MB/s
-------------------------------------------------------
./exomler_bench test_04.xml 1000
-------------------------------------------------------
     parser    min time    all time    memory     speed
-------------------------------------------------------
      xmerl     3612mcs     3935mls    2845KB     5MB/s
     erlsom     1676mcs     1924mls    1087KB    11MB/s
    exomler      319mcs      370mls     139KB    57MB/s
-------------------------------------------------------
```
