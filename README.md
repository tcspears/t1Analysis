# t1Fundamentals
R package to read in financial fundamentals from Thomson One Banker excel files.

## Installing

This package depends on the XLConnect, lubridate, xts, zoo, and quantmod packages. These are available on CRAN.

To install this package (and its dependencies) directly from github, use the install_github() function within the devtools package (also available on CRAN):
```
install_github('t1Fundamentals','tcspears')
```

## Examples
Read in T1 excel files from a directory, and then print a list of firms contained in those files.
```
> sheets <- ReadT1Directory("./T1files")
> GetFirms(sheets)
[1] "3I Infrastructure PLC"                 "AA PLC"                                "Aberdeen Asset Management PLC"        
[4] "Acacia Mining PLC"                     "Alent PLC"                             "Allied Minds PLC"                     
[7] "Aberforth Smaller Companies Trust PLC" "AVEVA Group PLC"                       "Barclays PLC"

```
Extract fundamentals from a firm's balance sheet, income statement, and cash flow statement. `GetFundamentals` will automatically drop redundant filings.
```
> GetFundamentals(GetFirms(sheets)[3],sheets,fundamentals.codes=c("Net Income","Total Debt","Stock Price"))
           Net Income Total Debt    ADN
2015-03-31     141.60       0.00 459.60
2014-09-30     159.60       0.00 399.81
2014-03-31     125.90       0.00 390.30
2013-09-30     162.00       0.00 378.60
2011-09-30      86.70      82.00 173.00
2011-03-31      83.00     158.80 210.80
2010-09-30      50.75     158.50 160.50
2010-03-31      41.85     180.47 130.00
2009-09-30     -15.31     255.89 150.00
2009-03-31       7.24     255.64 127.50
2008-09-30      13.22     218.87 126.00
2008-03-31      21.99     203.92 138.50
2006-03-31      15.33      47.38 190.00
2005-09-30      13.69      51.92 114.00
2005-03-31      11.86     190.80  84.50
2004-09-30     -68.95     113.47  57.67
2004-03-31     -11.82     112.86  65.00
2003-09-30     -45.62     117.09  46.58
2003-03-31      30.76     120.88  41.31
2002-09-30       1.56      96.79  52.00
2000-03-31       8.91      23.55 390.00
attr(,"firm")
[1] "Aberdeen Asset Management PLC"

```
Calculate specific fundamentals ratios (e.g. Return on Equity) for a particular firm:

```
> GetRatios(GetFirms(sheets)[3],sheets,ratio.names=c("Return on Equity","Current Ratio"))
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
Calculate all available fundamentals ratios for a given firm:

```
> GetRatios(GetFirms(sheets)[3],sheets)
$`Net Margin`
 2015-03-31  2014-09-30  2014-03-31  2013-09-30  2013-03-31  2012-09-30  2012-03-31  2011-09-30  2011-03-31  2010-09-30 
 0.23397224  0.25989253  0.25004965  0.28800000  0.28236434  0.26923920  0.24400871  0.21778448  0.21508163  0.14784711 
 2010-03-31  2009-09-30  2009-03-31  2008-09-30  2008-03-31  2007-09-30  2007-03-31  2006-09-30  2006-03-31  2005-09-30 
 0.14189327 -0.06664635  0.03767301  0.05782774  0.10914776  0.21946483 -0.08443597  0.18449803  0.10397450  0.13346983 
 2005-03-31  2004-09-30  2004-03-31  2003-09-30  2003-03-31  2002-09-30  2002-03-31  2001-09-30  2001-03-31  2000-09-30 
 0.13198308 -1.03497448 -0.16105736 -0.67735709  0.41057128  0.01595745  0.05640373  0.08379175  0.08008469  0.22056265 
 2000-03-31 
 0.19054748 

$`Return on Assets`
  2015-03-31   2014-09-30   2014-03-31   2013-09-30   2013-03-31   2012-09-30   2012-03-31   2011-09-30   2011-03-31   2010-09-30 
 0.024699110  0.029671494  0.024529956  0.036099474  0.030100818  0.030161615  0.032800755  0.030038458  0.026437331  0.016128520 
  2010-03-31   2009-09-30   2009-03-31   2008-09-30   2008-03-31   2007-09-30   2007-03-31   2006-09-30   2006-03-31   2005-09-30 
 0.013127394 -0.004978052  0.003298962  0.005575636  0.009643003  0.017878490 -0.006307843  0.012825179  0.006290986  0.004104172 
  2005-03-31   2004-09-30   2004-03-31   2003-09-30   2003-03-31   2002-09-30   2002-03-31   2001-09-30   2001-03-31   2000-09-30 
 0.020556374 -0.099845056 -0.016105078 -0.064523429  0.042547894  0.001925641  0.005389361  0.013249662  0.012740999  0.055480576 
  2000-03-31 
 0.025992590 

$`Sales to Total Assets`
2015-03-31 2014-09-30 2014-03-31 2013-09-30 2013-03-31 2012-09-30 2012-03-31 2011-09-30 2011-03-31 2010-09-30 2010-03-31 
0.10556428 0.11416832 0.09810034 0.12534540 0.10660276 0.11202535 0.13442452 0.13792745 0.12291766 0.10908918 0.09251597 
2009-09-30 2009-03-31 2008-09-30 2008-03-31 2007-09-30 2007-03-31 2006-09-30 2006-03-31 2005-09-30 2005-03-31 2004-09-30 
0.07469355 0.08756829 0.09641801 0.08834815 0.08146403 0.07470564 0.06951391 0.06050508 0.03074981 0.15575006 0.09647103 
2004-03-31 2003-09-30 2003-03-31 2002-09-30 2002-03-31 2001-09-30 2001-03-31 2000-09-30 2000-03-31 
0.09999591 0.09525763 0.10363096 0.12067348 0.09554972 0.15812610 0.15909406 0.25154113 0.13641005 

$`Current Ratio`
2015-03-31 2014-09-30 2014-03-31 2013-09-30 2013-03-31 2012-09-30 2012-03-31 2011-09-30 2011-03-31 2010-09-30 2010-03-31 
 1.1798588  1.1890052  1.1312500  1.1470659  1.2255352  1.1008777  1.1476317  1.1580394  1.1418642  1.1086050  1.0781016 
2009-09-30 2009-03-31 2008-09-30 2008-03-31 2007-09-30 2007-03-31 2006-09-30 2006-03-31 2005-09-30 2005-03-31 2004-09-30 
 1.0750125  1.0624935  1.0534536  1.0527362  1.0405412  0.9941769  1.0065380  0.9603740  1.0096380  0.8779161  0.3616974 
2004-03-31 2003-09-30 2003-03-31 2002-09-30 2002-03-31 2001-09-30 2001-03-31 2000-09-30 2000-03-31 
 0.4283316  0.4602776  0.4090810  0.6816665  1.0263177  0.8334182  0.8959859  1.1252610  1.0071800 

$`Long Term Debt to Total Assets`
2015-03-31 2014-09-30 2014-03-31 2013-09-30 2013-03-31 2012-09-30 2012-03-31 2011-09-30 2011-03-31 2010-09-30 2010-03-31 
0.00000000 0.00000000 0.00000000 0.00000000 0.00000000 0.00000000 0.02704110 0.02841008 0.05058130 0.05037183 0.05660934 
2009-09-30 2009-03-31 2008-09-30 2008-03-31 2007-09-30 2007-03-31 2006-09-30 2006-03-31 2005-09-30 2005-03-31 2004-09-30 
0.07399122 0.09852230 0.07268571 0.07694669 0.03750176 0.05606256 0.04052918 0.01005819 0.01538840 0.32600745 0.14230274 
2004-03-31 2003-09-30 2003-03-31 2002-09-30 2002-03-31 2001-09-30 2001-03-31 2000-09-30 2000-03-31 
0.13339147 0.13794323 0.13439380 0.11947613 0.12436052 0.03960919 0.05237086 0.09254058 0.06870095 

$`Leverage Ratio`
2015-03-31 2014-09-30 2014-03-31 2013-09-30 2013-03-31 2012-09-30 2012-03-31 2011-09-30 2011-03-31 2010-09-30 2010-03-31 
0.00000000 0.00000000 0.00000000 0.00000000 0.00000000 0.07488056 0.08257975 0.08037640 0.16009678 0.16286478 0.18757340 
2009-09-30 2009-03-31 2008-09-30 2008-03-31 2007-09-30 2007-03-31 2006-09-30 2006-03-31 2005-09-30 2005-03-31 2004-09-30 
0.31322602 0.46775964 0.40133123 0.44733032 0.31663113 0.43322057 0.31268607 0.10636913 0.12066000 1.51536812 0.85367138 
2004-03-31 2003-09-30 2003-03-31 2002-09-30 2002-03-31 2001-09-30 2001-03-31 2000-09-30 2000-03-31 
0.54802370 0.65559910 0.53078071 0.46715575 0.58563114 0.11701542 0.19808139 0.25515438 0.27735249 

$`Operating Cash Flow Ratio`
  2015-03-31   2014-09-30   2014-03-31   2013-09-30   2013-03-31   2012-09-30   2012-03-31   2011-09-30   2011-03-31   2010-09-30 
 0.047623131  0.146169161  0.061441799  0.162001987  0.073907518  0.136577417  0.084377758  0.245675205  0.090239411  0.124875184 
  2010-03-31   2009-09-30   2009-03-31   2008-09-30   2008-03-31   2007-09-30   2007-03-31   2006-09-30   2006-03-31   2005-09-30 
 0.042951788  0.010154037 -0.018098447  0.047248940  0.016177017 -0.019694806 -0.015062613  0.002121915 -0.005528605  0.006270087 
  2005-03-31   2004-09-30   2004-03-31   2003-09-30   2003-03-31   2002-09-30   2002-03-31   2001-09-30   2001-03-31   2000-09-30 
-0.061013908 -0.083015267 -0.022245540 -0.073795076 -0.022702467  0.102527986 -0.082795621  0.155820173  0.031495368  0.520078595 
  2000-03-31 
 0.433435051 

$`Long Term Debt To Total Equity Ratio`
2015-03-31 2014-09-30 2014-03-31 2013-09-30 2013-03-31 2012-09-30 2012-03-31 2011-09-30 2011-03-31 2010-09-30 2010-03-31 
0.00000000 0.00000000 0.00000000 0.00000000 0.00000000 0.00000000 0.08257975 0.08037640 0.16009678 0.16286478 0.18757340 
2009-09-30 2009-03-31 2008-09-30 2008-03-31 2007-09-30 2007-03-31 2006-09-30 2006-03-31 2005-09-30 2005-03-31 2004-09-30 
0.27854826 0.39563054 0.31601144 0.38492081 0.20215589 0.29051317 0.20037769 0.05502548 0.11928887 1.49384481 0.73931688 
2004-03-31 2003-09-30 2003-03-31 2002-09-30 2002-03-31 2001-09-30 2001-03-31 2000-09-30 2000-03-31 
0.47538118 0.54608063 0.42662686 0.46715575 0.58563114 0.11701542 0.19808139 0.25515438 0.27735249 

$`Earnings Per Share`
  2015-03-31   2014-09-30   2014-03-31   2013-09-30   2013-03-31   2012-09-30   2012-03-31   2011-09-30   2011-03-31   2010-09-30 
          NA           NA           NA  0.141373593  0.128132970  0.111181473  0.091477788  0.077045837  0.073611480  0.046662607 
  2010-03-31   2009-09-30   2009-03-31   2008-09-30   2008-03-31   2007-09-30   2007-03-31   2006-09-30   2006-03-31   2005-09-30 
 0.041391141 -0.015085473  0.015073473  0.022617813  0.041120530  0.073148523 -0.018313557  0.052027646  0.030064030           NA 
  2005-03-31   2004-09-30   2004-03-31   2003-09-30   2003-03-31   2002-09-30   2002-03-31   2001-09-30   2001-03-31   2000-09-30 
          NA -0.191575672 -0.035841530 -0.164993087  0.113600643  0.007730251  0.021824225  0.038413879  0.027478695  0.063443731 
  2000-03-31 
 0.037303747 

$`Return on Capital Employed`
  2015-03-31   2014-09-30   2014-03-31   2013-09-30   2013-03-31   2012-09-30   2012-03-31   2011-09-30   2011-03-31   2010-09-30 
 0.084575111  0.082306707  0.079819777  0.130889385  0.108088277  0.107067318  0.093040186  0.084593878  0.077942590  0.049705699 
  2010-03-31   2009-09-30   2009-03-31   2008-09-30   2008-03-31   2007-09-30   2007-03-31   2006-09-30   2006-03-31   2005-09-30 
 0.044161219 -0.001497800  0.022221157  0.032040321  0.037556823  0.064062355 -0.031683630  0.058056121  0.042246244  0.040580242 
  2005-03-31   2004-09-30   2004-03-31   2003-09-30   2003-03-31   2002-09-30   2002-03-31   2001-09-30   2001-03-31   2000-09-30 
 0.049419261 -0.145254776 -0.005482419 -0.073288862 -0.007941659  0.022910712  0.021596797  0.042937853  0.036875192  0.109329597 
  2000-03-31 
 0.074917454 

$`Return on Equity`
  2015-03-31   2014-09-30   2014-03-31   2013-09-30   2013-03-31   2012-09-30   2012-03-31   2011-09-30   2011-03-31   2010-09-30 
 0.083269627  0.093104655  0.082314482  0.142718703  0.124721794  0.112826167  0.100168936  0.084983337  0.083677790  0.052147554 
  2010-03-31   2009-09-30   2009-03-31   2008-09-30   2008-03-31   2007-09-30   2007-03-31   2006-09-30   2006-03-31   2005-09-30 
 0.043497240 -0.018740437  0.013247457  0.024240868  0.048238494  0.096375267 -0.032686901  0.063408132  0.034416182  0.031815013 
  2005-03-31   2004-09-30   2004-03-31   2003-09-30   2003-03-31   2002-09-30   2002-03-31   2001-09-30   2001-03-31   2000-09-30 
 0.094194266 -0.518733073 -0.057395358 -0.255431131  0.135066304  0.007529321  0.025379258  0.039142805  0.048190062  0.152971940 
  2000-03-31 
 0.104934637 

$`Book to Market Value`
2015-03-31 2014-09-30 2014-03-31 2013-09-30 2011-09-30 2011-03-31 2010-09-30 2010-03-31 2009-09-30 2009-03-31 2008-09-30 
 359.89883  306.53966  333.86663  399.97651  194.10356  243.24565  188.71449  149.37036  184.17345  185.60894  165.65109 
2008-03-31 2006-03-31 2005-09-30 2005-03-31 2004-09-30 2004-03-31 2003-09-30 2003-03-31 2002-09-30 2000-03-31 
 197.84542  255.81303  160.28352  246.34592  159.26066  103.64839   71.70347   49.51432   68.50929 1100.82794 

$`Price Earnings Ratio`
2015-03-31 2014-09-30 2014-03-31 2013-09-30 2011-09-30 2011-03-31 2010-09-30 2010-03-31 2009-09-30 2009-03-31 2008-09-30 
 4178.1818  3331.7500  3548.1818  2704.2857  2162.5000  3011.4286  4012.5000  3250.0000 -7500.0000 12750.0000 12600.0000 
2008-03-31 2006-03-31 2005-09-30 2005-03-31 2004-09-30 2004-03-31 2003-09-30 2003-03-31 2002-09-30 2000-03-31 
 4616.6667  9500.0000  2850.0000  2816.6667  -303.5263 -1625.0000  -274.0000   375.5455        Inf  9750.0000 

$`Price to Sales Ratio`
2015-03-31 2014-09-30 2014-03-31 2013-09-30 2011-09-30 2011-03-31 2010-09-30 2010-03-31 2009-09-30 2009-03-31 2008-09-30 
 1011.2491   855.6754  1014.1986   807.1348   497.4239   625.2277   535.0374   487.2642   654.9734   527.8333   395.1685 
2008-03-31 2006-03-31 2005-09-30 2005-03-31 2004-09-30 2004-03-31 2003-09-30 2003-03-31 2002-09-30 2000-03-31 
  447.6588   772.8351   672.4188   345.1749   317.7563   290.8482   190.1446   150.5124   145.1968  1998.9585 

attr(,"firm")
[1] "Aberdeen Asset Management PLC"
