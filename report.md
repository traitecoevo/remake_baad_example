# A quick look at BAAD


The Biomass And Allometry Database for woody plants (BAAD) contains 259634 measurements collected in 176 different studies, from 21084 individuals across 678 species.


![](figure/unnamed-chunk-2-1.png) 

**Figure 1** Global distribution of studies included in the BAAD. Symbols are sized relative to the number of individual plants in each dataset.

## Variables

**Number of observations by variable**


|Variable           |Label                                    |Units          |N              |Studies          |Min                   |Median            |Max             |
|:------------------|:----------------------------------------|:--------------|:--------------|:----------------|:---------------------|:-----------------|:---------------|
|latitude           |latitude                                 |deg            |21021          |175              |-52                   |10                |62              |
|longitude          |longitude                                |deg            |21021          |175              |-126                  |-66               |175             |
|age                |age                                      |yr             |7074           |73               |0.17                  |13                |1847            |
|a.lf               |leaf area                                |m2             |13846          |108              |0.000018              |0.047             |6018            |
|a.ssba             |sapwood area at base                     |m2             |168            |5                |0.000015              |0.0012            |0.088           |
|a.ssbh             |sapwood area at breast height            |m2             |1475           |16               |0.00000018            |0.0044            |0.67            |
|a.ssbc             |sapwood area at crown base               |m2             |277            |7                |0.00016               |0.011             |0.19            |
|a.shba             |heartwood area at base                   |m2             |24             |1                |0                     |0.006             |0.28            |
|a.shbh             |heartwood area at breast height          |m2             |421            |4                |0                     |0.00075           |0.2             |
|a.shbc             |heartwood area at crown base             |m2             |36             |1                |0                     |0.00036           |0.0068          |
|a.sbbh             |bark area at breast height               |m2             |141            |1                |0.000059              |0.0014            |0.0063          |
|a.stba             |stem area at base                        |m2             |9704           |56               |0.0000000038          |0.000026          |1.5             |
|a.stbh             |stem area at breast height               |m2             |8626           |129              |0.00000079            |0.0068            |33              |
|a.stbc             |stem area at crown base                  |m2             |1325           |18               |0.000000006           |0.000012          |0.13            |
|a.cp               |crown area                               |m2             |7414           |55               |0.0002                |0.63              |1257            |
|a.cs               |crown surface area                       |m2             |1187           |9                |0.00047               |0.086             |652             |
|h.t                |height                                   |m              |19586          |169              |0.004                 |1.2               |113             |
|h.c                |height to crown base                     |m              |10214          |90               |0                     |0.77              |40              |
|d.ba               |basal diameter                           |m              |9704           |56               |0.00007               |0.0058            |1.4             |
|d.bh               |dbh                                      |m              |8626           |129              |0.001                 |0.093             |6.5             |
|h.bh               |height of d.bh measurement               |m              |10114          |128              |0.03                  |1.3               |4.8             |
|d.cr               |crown width                              |m              |7414           |55               |0.014                 |0.88              |40              |
|c.d                |crown depth                              |m              |9551           |89               |0                     |1.7               |41              |
|m.lf               |leaf mass                                |kg             |17069          |163              |0.0000002             |0.0057            |992             |
|m.ss               |sapwood mass                             |kg             |239            |4                |0.021                 |31                |2089            |
|m.sh               |heartwood mass                           |kg             |212            |3                |0                     |3.4               |1759            |
|m.sb               |bark mass                                |kg             |1323           |13               |0.00008               |5.4               |1236            |
|m.st               |total stem mass                          |kg             |14421          |138              |0.0000001             |0.0038            |322566          |
|m.so               |aboveground mass                         |kg             |14972          |143              |0.0000008             |0.011             |323200          |
|m.br               |branch mass                              |kg             |6552           |114              |0                     |0.86              |9586            |
|m.rf               |fine root mass                           |kg             |1672           |16               |0.000001              |0.00019           |20              |
|m.rc               |coarse root mass                         |kg             |2095           |20               |0                     |0.0011            |459             |
|m.rt               |total root mass                          |kg             |9061           |68               |0.0000001             |0.00065           |505             |
|m.to               |total mass                               |kg             |8878           |70               |0.000001              |0.002             |7410            |
|a.ilf              |area of individual leaf                  |m2             |5898           |26               |0.0000011             |0.0025            |0.19            |
|ma.ilf             |leaf mass per area                       |kg/m2          |9225           |37               |0.0015                |0.045             |0.81            |
|r.st               |wood density                             |kg/m3          |3529           |12               |61                    |410               |1603            |
|r.ss               |sapwood density                          |kg/m3          |53             |1                |300                   |410               |590             |
|r.sh               |heartwood density                        |kg/m3          |53             |1                |310                   |370               |500             |
|n.lf               |leaf [nitrogen]                          |kg/kg          |1303           |11               |0.0044                |0.023             |0.045           |
|n.ss               |sapwood [nitrogen]                       |kg/kg          |425            |7                |0                     |0.0016            |0.0082          |
|n.sb               |bark [nitrogen]                          |kg/kg          |261            |3                |0.0021                |0.0037            |0.011           |
|n.sh               |heartwood [nitrogen]                     |kg/kg          |143            |2                |0                     |0.00094           |0.0029          |
|n.rf               |fine root [nitrogen]                     |kg/kg          |148            |5                |0                     |0.0047            |0.0083          |
|n.rc               |coarse root [nitrogen]                   |kg/kg          |207            |6                |0                     |0.003             |0.01            |

## Some plots


![](figure/unnamed-chunk-4-1.png) 

**Figure 2** Plot of most commonly recorded variables in BAAD.


![](figure/unnamed-chunk-5-1.png) 

**Figure 3** Plot of leaf area vs height in BAAD.

For more info on the BAAD see the paper in [Ecology]() or the [github repo](https://github.com/dfalster/baad).