
Input/Output/Shiny Group
========================================================
author: Elle Holbrook, Katie Key, Brian Kim, Lizette Van Zyl
date: 
autosize: true

Outline for Presentation:
========================================================

- what we did
- overview of each data set and how we "tidyed" it
- Shiny app
- challenges
- do differently
- interesting
- conclusion


Our group was tasked with:
========================================================

- inputting the Excel files received from the research group
- outputting the data in "tidy" formats
- developing the Shiny app

Data sets:
========================================================

- Efficacy (Katie)
- Plasma (Brian)
- Tissue Laser (Lizette)
- Tissue Standard PK (Elle)

Efficacy Template
========================================================


```
# A tibble: 6 x 19
         StudyID MouseID Compound Group Protocol_Animal Drug_Dose
           <chr>   <chr>    <chr> <chr>           <chr>     <chr>
1 Gates_18 effic     001       NA PreRX               A        NA
2 Gates_18 effic     002       NA PreRX               B        NA
3 Gates_18 effic     003       NA PreRX               C        NA
4 Gates_18 effic     004       NA PreRX               D        NA
5 Gates_18 effic     005       NA PreRX               E        NA
6 Gates_18 effic     006       NA PreRX               F        NA
# ... with 13 more variables: Dose_Units <chr>, Formulation <chr>,
#   Dose_Frequency <chr>, Days_Treatment <dbl>, Treatment_Interval <chr>,
#   Elung <chr>, `Elung LLOD` <dbl>, Elung_Units <chr>, Espleen <chr>,
#   `Espleen LLOD` <dbl>, Espleen_Units <chr>, `% Res` <lgl>,
#   `42794` <lgl>
```

Tidy Efficacy
========================================================


```
# A tibble: 6 x 8
# Groups:   Protocol_Animal, drug, Group, dosage [4]
  Protocol_Animal  drug Group dosage days_treatment dose_interval
            <chr> <chr> <chr>  <chr>          <dbl>         <chr>
1               A    NA PreRX     NA              0      Baseline
2               A   RBT     5     10             20            QD
3               A   RBT     5     10             40            QD
4               A   RIF     2     10             20            QD
5               A   RIF     2     10             40            QD
6               A   RIF     3     20             20            QD
# ... with 2 more variables: lung_efficacy_log <dbl>,
#   spleen_efficacy_log <dbl>
```


Plasma Template
========================================================


```r
summary(cars)
```

```
     speed           dist       
 Min.   : 4.0   Min.   :  2.00  
 1st Qu.:12.0   1st Qu.: 26.00  
 Median :15.0   Median : 36.00  
 Mean   :15.4   Mean   : 42.98  
 3rd Qu.:19.0   3rd Qu.: 56.00  
 Max.   :25.0   Max.   :120.00  
```

Tidy Plasma
========================================================

![plot of chunk unnamed-chunk-4](Presentation-figure/unnamed-chunk-4-1.png)

Tissue Laser Template
========================================================


```r
summary(cars)
```

```
     speed           dist       
 Min.   : 4.0   Min.   :  2.00  
 1st Qu.:12.0   1st Qu.: 26.00  
 Median :15.0   Median : 36.00  
 Mean   :15.4   Mean   : 42.98  
 3rd Qu.:19.0   3rd Qu.: 56.00  
 Max.   :25.0   Max.   :120.00  
```

Tidy Tissue Laser
========================================================

![plot of chunk unnamed-chunk-6](Presentation-figure/unnamed-chunk-6-1.png)

Tissue Standard PK Template
========================================================


```r
summary(cars)
```

```
     speed           dist       
 Min.   : 4.0   Min.   :  2.00  
 1st Qu.:12.0   1st Qu.: 26.00  
 Median :15.0   Median : 36.00  
 Mean   :15.4   Mean   : 42.98  
 3rd Qu.:19.0   3rd Qu.: 56.00  
 Max.   :25.0   Max.   :120.00  
```

Tidy Tissue Standard PK
========================================================

![plot of chunk unnamed-chunk-8](Presentation-figure/unnamed-chunk-8-1.png)


Shiny App
========================================================




