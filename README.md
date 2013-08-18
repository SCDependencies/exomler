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
     parser    min time  total time    memory     speed
-------------------------------------------------------
      xmerl      146mcs      200mls      73KB     1MB/s
     erlsom       34mcs       78mls      20KB     3MB/s
    exomler       22mcs       57mls      20KB     5MB/s
   mochiweb       56mcs       95mls       8KB     3MB/s
-------------------------------------------------------
./exomler_bench test_02.xml 1000
-------------------------------------------------------
     parser    min time  total time    memory     speed
-------------------------------------------------------
      xmerl      400mcs      453mls     159KB     2MB/s
     erlsom       98mcs      140mls     106KB     7MB/s
    exomler       59mcs       91mls      41KB    12MB/s
   mochiweb      190mcs      249mls      53KB     4MB/s
-------------------------------------------------------
./exomler_bench test_03.xml 1000
-------------------------------------------------------
     parser    min time  total time    memory     speed
-------------------------------------------------------
      xmerl     1720mcs     1933mls     672KB     2MB/s
     erlsom      634mcs      707mls     171KB     6MB/s
    exomler      397mcs      519mls     139KB     9MB/s
   mochiweb      993mcs     1135mls     277KB     4MB/s
-------------------------------------------------------
./exomler_bench test_04.xml 1000
-------------------------------------------------------
     parser    min time  total time    memory     speed
-------------------------------------------------------
      xmerl     3622mcs     3953mls    2845KB     5MB/s
     erlsom     1735mcs     1926mls    1087KB    11MB/s
    exomler      336mcs      380mls     139KB    56MB/s
   mochiweb     1712mcs     1883mls     257KB    11MB/s
-------------------------------------------------------
```

Here we can see that several files of different sizes and content parsed 1000 times by different parsers.
