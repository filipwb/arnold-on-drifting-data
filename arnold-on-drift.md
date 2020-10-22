Hey Arnold
================
Filip Wästberg
2020-10-22

I en tidigare Notebook introducerade vi en alternativ algoritm till K2.
Den heter `arnold` och är byggd på ett modellramverk som heter
`Generalized Additive Models (GAM)`. Modellen funkar på ungefär samma
sätt som K2s nuvarande algoritm. Men istället för att anpassa en bruten
linjär regression anpassar vi med hjälp av `GAM` *en* regressionslinje,
som inte är linjär. Utöver det använder vi också en mer standardiserade
metod än *standardavvikelsen* (som används i K2 idag) för att
identifiera larm, den heter `Inter Quartile Range (iqr)`.

Men i vår förra Notebook utgick vi från ganska enkel data. Nu är vi
nyfikna på hur den här algoritmen fungerar på fjärrvärmeanvändning som
driftar. Ett av skälen till att vi i K2 använder *referensdata* är att
vi ska kunna upptäcka fjärrvärmeanvändning som driftar, så en ny
algoritm måste kunna upptäcka det. En annan aspekt som `arnold` behöver
kunna hantera är att det finns flera effektsignaturer. Ofta beror det
här på olika användning under olika veckodagar.

Vi kommer utgå från ett exempeldata där en drift av flöde uppstår:

![](arnold-on-drift_files/figure-gfm/unnamed-chunk-1-1.png)<!-- -->

Vi ser det här i effektsignaturen också:

![](arnold-on-drift_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

Vi ser att det finns två olika effektsignaturer i diagrammet. Just den
här typen kallas ofta för “tvåstråkare”. K2 identiferar de två stråken
genom att anpassa en bruten regressionslinje på hela datasettet och
delar sedan in data i två grupper: de som är under linjen och de som är
över linjen. Den anpassar sedan en ny bruten regression till respektive
grupp och itererar. Det här finns utförligt beskrivet i en Notebook av
Ida Lundholm Benzi på Data Science-portalen.

![](arnold-on-drift_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

`arnold` hanterar det här annorlunda. Istället för att identifiera
grupperna i effektsignaturen utgår den från att det här i regel beror på
olika konsumtion under olika dagar.

Men det bör dock tilläggas att en referensperiod kan vara nödvändig för
`arnold` också.

![](arnold-on-drift_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

Vi kan visualisera skillnaden mellan dagar:

![](arnold-on-drift_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

Vi kan även kolla fördelningen, där vi ser att söndag och måndag har i
princip samma fördelning, medan alla andra dagar förutom lördag har i
princip samma fördelning.

![](arnold-on-drift_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

Det här löser vi helt enkelt genom att anpassa en modell per veckodag.

![](arnold-on-drift_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

Ett problem som dyker upp här är att modellen i sin grundläggande form
är känslig för outliers. Det här är precis skälet till varför vi
använder referensperiod i K2. Men i en GAM går det här justera genom
att ändra underliggande fördelning till Students T.

``` r
library(mgcv)
tds5_model <- gam(consumption_value ~ s(temperature, by = weekday) + weekday,
                             data = tds5,  method = "REML", family = scat)
```

![](arnold-on-drift_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

I nästa steg identifierar vi outliers/larm med `iqr`.

![](arnold-on-drift_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->
