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
--------------------------------------------------------
      parser    min time    all time    memory     speed
--------------------------------------------------------
       xmerl      165mcs      222mls      73KB     1MB/s
      erlsom       34mcs       93mls      20KB     3MB/s
     exomler       30mcs       94mls      13KB     3MB/s
 libexpat.so       28mcs       69mls      28KB     4MB/s
  libxml2.so       26mcs       76mls      20KB     3MB/s
--------------------------------------------------------
./exomler_bench test_02.xml 1000
--------------------------------------------------------
      parser    min time    all time    memory     speed
--------------------------------------------------------
       xmerl      402mcs      494mls     159KB     2MB/s
      erlsom       98mcs      166mls     106KB     6MB/s
     exomler       74mcs      121mls      20KB     9MB/s
 libexpat.so       52mcs       87mls      53KB    12MB/s
  libxml2.so       44mcs       98mls      33KB    11MB/s
--------------------------------------------------------
./exomler_bench test_03.xml 1000
--------------------------------------------------------
      parser    min time    all time    memory     speed
--------------------------------------------------------
       xmerl     1753mcs     1951mls     672KB     2MB/s
      erlsom      624mcs      920mls     171KB     5MB/s
     exomler      503mcs      812mls     139KB     5MB/s
 libexpat.so      266mcs      347mls     310KB    13MB/s
  libxml2.so      213mcs      294mls     224KB    15MB/s
--------------------------------------------------------
./exomler_bench test_04.xml 1000
--------------------------------------------------------
      parser    min time    all time    memory     speed
--------------------------------------------------------
       xmerl     3599mcs     4153mls    2845KB     5MB/s
      erlsom     1681mcs     2064mls    1087KB    10MB/s
     exomler      415mcs      703mls      86KB    30MB/s
 libexpat.so      285mcs      467mls     310KB    45MB/s
  libxml2.so      248mcs      316mls     191KB    67MB/s
--------------------------------------------------------
```

Here we can see, that exomler very memory-efficient, and shows good speed, even when compared with the C-libs
