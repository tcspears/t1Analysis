# t1Fundamentals
R package to read in financial fundamentals from Thomson One Banker excel files.

## Installing

This package depends on the XLConnect package, which is available on CRAN.

To install this package directly from github, use the install_github() function within the devtools package (available on CRAN):
```
install_github('t1Fundamentals','tcspears')
```

## Examples
Read in T1 excel files from a directory, and drop redundant filings from those files.
```
> sheets <- ReadT1Directory("./T1files")
> sheets <- lapply(sheets,FUN=DropRedundantFilings)
```
Extract fundamentals from a firm's balance sheet, income statement, and cash flow statement:
```
> GetFundamentals("Aberdeen Asset Management PLC",fundamentals.names=c("Net Income","Total Debt"),sheets)
     Net Income Total Debt
2014     285.50       0.00
2013     307.70       0.00
2012     208.70      81.50
2011     169.70      82.00
2010      92.60     158.50
2009      -8.07     255.89
2008      35.24     218.87
2007      26.96     133.65
2006      43.87     140.74
2005      25.69      51.92
2004     -80.77     186.67
2003     -14.86     208.34
2002       6.88     266.94
2001      14.96     230.10
2000      24.13      25.37
1999      10.49      22.78
1998       6.07      22.07
1997       2.31      23.21
1996       4.37      23.99
1995       2.72       2.05
1994       3.39       8.13
1993       2.07      11.30
1992       1.28      11.93
attr(,"firm")
[1] "Aberdeen Asset Management PLC"
```
Calculate fundamentals ratios (e.g. Return on Equity) for a particular firm:

```
> GetRatios("Aberdeen Asset Management PLC",sheets,ratio.names=c("Return on Equity","Current Ratio"))
     Return on Equity Current Ratio
2014      0.166549994     1.1890052
2013      0.271077438     1.1470659
2012      0.191749357     1.1008777
2011      0.166339933     1.1580394
2010      0.095150021     1.1086050
2009     -0.009878206     1.0750125
2008      0.064617867     1.0534536
2007      0.063871121     1.0405412
2006      0.097467230     1.0065380
2005      0.059702533     1.0096380
2004     -0.607658742     0.3616974
2003     -0.083202688     0.4602776
2002      0.033206236     0.6816665
2001      0.068649046     0.8334182
2000      0.242683295     1.1252610
1999      0.129666255     1.2143069
1998      0.079942052     1.3841730
1997      0.028976417     1.4862918
1996      0.147286822     1.6187862
1995      0.101568335     1.0623013
1994      0.137025061     1.2477612
1993      0.097228746     1.4560531
1992      0.063116371     1.4188034
attr(,"firm")
[1] "Aberdeen Asset Management PLC"
```
