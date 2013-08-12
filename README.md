Exomler
========


It is a very simple and limited DOM XML parser that can work only with valid, well-formed and "good" XML.

There are more than hundred ways to crush it down with a proper XML, but it was written for a "good" XML
to parse feeds and machine-generated content.

It is fast enough, convenient and has very low memory footprint due to binary usage. Really!

Usage:


```
{Tag, Attrs, Content} = exomler:decode(XML). 
```

Where Tag is binary name of root tag, Attrs is a {Key,Value} list of attrs and Content is
list of inner tags or Text which is binary.

```
XML = <<"<html key=\"value\">Body</html>">>.
{<<"html">>, [{<<"key">>, <<"value">>}], [<<Body>>]} = exomler:decode(XML).
XML = exomler:encode({<<"html">>, [{<<"key">>, <<"value">>}], [<<Body>>]}).

```


Benchmarking
------------

```
./exomler_bench test_01.xml 1000
-------------------------------------------------------
     parser    min time    all time    memory     speed
-------------------------------------------------------
      xmerl      167mcs      217mls      73KB     1MB/s
     erlsom       34mcs       85mls      20KB     3MB/s
    exomler       22mcs       62mls      16KB     4MB/s
libexpat.so       29mcs       58mls      28KB     5MB/s
 libxml2.so       26mcs       65mls      20KB     4MB/s
-------------------------------------------------------
./exomler_bench test_02.xml 1000
-------------------------------------------------------
     parser    min time    all time    memory     speed
-------------------------------------------------------
      xmerl      405mcs      509mls     159KB     2MB/s
     erlsom       97mcs      178mls     106KB     6MB/s
    exomler       57mcs      113mls      41KB     9MB/s
libexpat.so       53mcs      129mls      53KB     8MB/s
 libxml2.so       44mcs       90mls      33KB    12MB/s
-------------------------------------------------------
./exomler_bench test_03.xml 1000
-------------------------------------------------------
     parser    min time    all time    memory     speed
-------------------------------------------------------
      xmerl     1749mcs     2098mls     672KB     2MB/s
     erlsom      622mcs      927mls     171KB     5MB/s
    exomler      389mcs      512mls      53KB     9MB/s
libexpat.so      265mcs      350mls     310KB    13MB/s
 libxml2.so      211mcs      299mls     224KB    15MB/s
-------------------------------------------------------
./exomler_bench test_04.xml 1000
-------------------------------------------------------
     parser    min time    all time    memory     speed
-------------------------------------------------------
      xmerl     3583mcs     4144mls    2845KB     5MB/s
     erlsom     1679mcs     2043mls    1087KB    10MB/s
    exomler      328mcs      622mls     139KB    34MB/s
libexpat.so      290mcs      429mls     310KB    49MB/s
 libxml2.so      248mcs      316mls     191KB    67MB/s
-------------------------------------------------------
```

Here we can see, that exomler very memory-efficient, and shows good speed, even when compared with the C-libs.

But like a NIFs, Erlang ports prevent Erlang VM from efficiently handling many processes, and if we run this test several times, we note that the results are varied.

If we exclude parsers that use Erlang-ports, we note that the results of parsers written in the pure Erlang will be a little better, and if we run the test several times the results are not varied.

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
