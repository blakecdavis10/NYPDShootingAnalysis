NYPDShootingAnalysis
================
Blake Davis
2023-03-07

### What is the safest Borough in New York?

In this R markdown I will attempt to hypothesize which borough is the
safest in Ney York in regards to shooting violence by looking at an NYPD
data set that chronicles every shooting from 2006 to 2021. New York is
subdivided into 5 boroughs, they are called The Bronx, Brooklyn,
Manhattan, Queens and Staten Island. I will use a combination of visuals
to support my hypothesis and model what appears to be the safest
location given the limitations of this data set to see if I can predict
what the shooting violence will look like in the future for that
respective borough. Pitfalls and criticisms of the data set can be found
at the bottom of my RMD.

### Libraries

``` r
library(tidyverse)
```

    ## ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.2 ──
    ## ✔ ggplot2 3.4.0      ✔ purrr   0.3.5 
    ## ✔ tibble  3.1.8      ✔ dplyr   1.0.10
    ## ✔ tidyr   1.2.1      ✔ stringr 1.4.1 
    ## ✔ readr   2.1.3      ✔ forcats 0.5.2 
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()

``` r
library(lubridate)
```

    ## 
    ## Attaching package: 'lubridate'
    ## 
    ## The following objects are masked from 'package:base':
    ## 
    ##     date, intersect, setdiff, union

``` r
library(ggrepel)
```

### Reading Data

``` r
url_in <- "https://data.cityofnewyork.us/api/views/833y-fsy8/rows.csv?accessType=DOWNLOAD"
myData <- read_csv(url_in)
```

#### Summary of NYPD Shooting Data 2006-2021

``` r
summary(shooting) 
```

    ##    OCCUR_DATE                    BORO       STATISTICAL_MURDER_FLAG
    ##  Min.   :2006-01-01   BRONX        : 7402   Mode :logical          
    ##  1st Qu.:2009-05-10   BROOKLYN     :10365   FALSE:20668            
    ##  Median :2012-08-26   MANHATTAN    : 3265   TRUE :4928             
    ##  Mean   :2013-06-13   QUEENS       : 3828                          
    ##  3rd Qu.:2017-07-01   STATEN ISLAND:  736                          
    ##  Max.   :2021-12-31                                                
    ##                                                                    
    ##  PERP_AGE_GROUP     PERP_SEX              PERP_RACE     VIC_AGE_GROUP     
    ##  Length:25596       F   :  371   BLACK         :10668   Length:25596      
    ##  Class :character   M   :14416   WHITE HISPANIC: 2164   Class :character  
    ##  Mode  :character   U   : 1499   UNKNOWN       : 1836   Mode  :character  
    ##                     NA's: 9310   BLACK HISPANIC: 1203                     
    ##                                  WHITE         :  272                     
    ##                                  (Other)       :  143                     
    ##                                  NA's          : 9310                     
    ##  VIC_SEX                             VIC_RACE    
    ##  F: 2403   AMERICAN INDIAN/ALASKAN NATIVE:    9  
    ##  M:23182   ASIAN / PACIFIC ISLANDER      :  354  
    ##  U:   11   BLACK                         :18281  
    ##            BLACK HISPANIC                : 2485  
    ##            UNKNOWN                       :   65  
    ##            WHITE                         :  660  
    ##            WHITE HISPANIC                : 3742

### Data Analyzation: Total Number of Shootings from 2006-2021 Per Borough

``` r
shooting_by_area <- shooting %>% count(BORO)
colnames(shooting_by_area)[colnames((shooting_by_area)) == 'n'] <- 'ShootingIncidents'
shooting_by_area
```

    ## # A tibble: 5 × 2
    ##   BORO          ShootingIncidents
    ##   <fct>                     <int>
    ## 1 BRONX                      7402
    ## 2 BROOKLYN                  10365
    ## 3 MANHATTAN                  3265
    ## 4 QUEENS                     3828
    ## 5 STATEN ISLAND               736

``` r
barHeight <- as.vector(shooting_by_area$ShootingIncidents)
barplot(barHeight,
 main = "Shootings in NY",
 xlab = "Neighborhood",
 ylab = "Shooting Incidents",
 names.arg = as.vector(shooting_by_area$BORO),
 col = "purple",
 horiz = FALSE,
 cex.names=.7)
```

![](NYPDShootingAnalysisRMD_files/figure-gfm/bar_graph_shooting_count-1.png)<!-- -->

This bar graph is a visualization of my table that summarizes the number
of shootings per borough in New York from 2006-2021. From this graph we
can see that Brooklyn has had the most shootings, followed by the Bronx,
Queens and Manhattan. Staten Island has had the fewest shootings. From
this data we can hypothesize that Staten Island is the safest borough in
New York when it comes to shootings.

### Data Analyzation: Breakdown of Ethnicity Per Shooting Count

``` r
victims <- shooting %>% count(VIC_RACE)
colnames(victims)[colnames((victims)) == 'n'] <- 'NumberOfVictims'      #rename column

victimVector = as.vector(victims$NumberOfVictims)
victimsPieValues <- round(prop.table(victimVector) * 100, digits=2) #Calculate the pie values (in %)
ggplot(victims, aes(x = "" , y = NumberOfVictims, fill = fct_inorder(VIC_RACE))) +
  geom_col(width = 1, color = 1) +
  coord_polar(theta = "y") +
  scale_fill_brewer(palette = "Pastel1") +
  geom_label_repel(data = victims,
                   aes(label = paste0(victimsPieValues, "%")),
                   size = 4.5, nudge_x = 1, show.legend = FALSE) +
  guides(fill = guide_legend(title = "Ethnicity")) +
  theme_void()
```

![](NYPDShootingAnalysisRMD_files/figure-gfm/pie_chart_ethnicity-1.png)<!-- -->

A pie chart depicting the victim ethnicity data from the data summary
above. This chart shows the majority of the victims of shootings in New
York from 2006 to 2021 where “black”, comprising 71.42% of the data set.
Followed by “white Hispanics” at 14.62% and “black Hispanics” at 9.71%.

### Data Analyzation: Number of Shootings Per Year Per Borough

``` r
shooting_by_date <- shooting %>% count(BORO, OCCUR_DATE)
colnames(shooting_by_date)[colnames(shooting_by_date) == 'n'] <- 'NumberIncidents'

bronx <- shooting_by_date[shooting_by_date$BORO == 'BRONX', ]
brooklyn <- shooting_by_date[shooting_by_date$BORO == 'BROOKLYN', ]
manhattan <- shooting_by_date[shooting_by_date$BORO == 'MANHATTAN', ]
queens <- shooting_by_date[shooting_by_date$BORO == 'QUEENS', ]
staten <- shooting_by_date[shooting_by_date$BORO == 'STATEN ISLAND', ]

bronx <- bronx %>% group_by(Year = as.integer(format(OCCUR_DATE, '%Y'))) %>%
                     mutate(TotalIncidents = sum(NumberIncidents)) %>%
                     select(BORO, Year, TotalIncidents)
brooklyn <- brooklyn %>% group_by(Year = as.integer(format(OCCUR_DATE, '%Y'))) %>%
                     mutate(TotalIncidents = sum(NumberIncidents)) %>%
                     select(BORO, Year, TotalIncidents)
manhattan <- manhattan %>% group_by(Year = as.integer(format(OCCUR_DATE, '%Y'))) %>%
                     mutate(TotalIncidents = sum(NumberIncidents)) %>%
                     select(BORO, Year, TotalIncidents)
queens <- queens %>% group_by(Year = as.integer(format(OCCUR_DATE, '%Y'))) %>%
                     mutate(TotalIncidents = sum(NumberIncidents)) %>%
                     select(BORO, Year, TotalIncidents)
staten <- staten %>% group_by(Year = as.integer(format(OCCUR_DATE, '%Y'))) %>%
                     mutate(TotalIncidents = sum(NumberIncidents)) %>%
                     select(BORO, Year, TotalIncidents)

bronx <- unique(bronx[, c('BORO', 'Year', 'TotalIncidents')])
brooklyn <- unique(brooklyn[, c('BORO', 'Year', 'TotalIncidents')])
manhattan <- unique(manhattan[, c('BORO', 'Year', 'TotalIncidents')])
queens <- unique(queens[, c('BORO', 'Year', 'TotalIncidents')])
staten <- unique(staten[, c('BORO', 'Year', 'TotalIncidents')])

allboro <- rbind(brooklyn, bronx, manhattan, staten, queens)

ggplot(allboro, aes(x=Year, y=TotalIncidents, colour = BORO)) + geom_line() +
     ggtitle("Shootings per Year per Borough") +
     xlab("Years") +
     ylab("Number of Shootings") + 
     theme(plot.title = element_text(hjust = 0.5))
```

![](NYPDShootingAnalysisRMD_files/figure-gfm/linear_graph-1.png)<!-- -->

This graph plots the number of shootings per year per borough. While the
first visualization showed the total number of shootings in each
borough, this graph breaks the data down further and summarizes by year.
This is helpful because it will show outliers in the data set and show
where each borough is trending. From these trends we can see the Bronx,
Manhattan and Queens have been getting more dangerous over time, with an
increase in the number of shootings while Brooklyn and Staten Island
have been getting safer with a decline in the number of shootings. From
the limited data set available, Staten Island is still the safest
borough with the fewest number of shootings and the data trend
decreasing.

### Creating a Model

``` r
mod <- lm(TotalIncidents ~ Year, data = staten)
prediction <- predict(mod)
prediction <- as.vector(prediction)
staten_with_predict <- cbind(staten, prediction)
colnames(staten_with_predict)[colnames(staten_with_predict) == '...4'] <- 'pred'
staten_with_predict %>% ggplot() +
                        geom_line(aes(x=Year, y =TotalIncidents), color = "blue" ) +
                        geom_line(aes(x=Year, y =pred), color = "red" )+
    ggtitle("Staten Island Shooting Data") +
     xlab("Years") +
     ylab("Number of Shootings") + 
     theme(plot.title = element_text(hjust = 0.5))
```

![](NYPDShootingAnalysisRMD_files/figure-gfm/model-1.png)<!-- -->

My linear regression model attempts to show the trend of the shooting
data in Staten Island and predict where it is going in the future. My
model confirms that Staten Island is the safest of the boroughs of New
York in regards to shooting violence given the confines of the data set.

#### Summary of Linear Regression Model

``` r
summary(lm(TotalIncidents ~ Year, data = staten))
```

    ## 
    ## Call:
    ## lm(formula = TotalIncidents ~ Year, data = staten)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -16.581  -3.239   1.154   4.772  15.802 
    ## 
    ## Coefficients:
    ##              Estimate Std. Error t value Pr(>|t|)  
    ## (Intercept) 2681.3162  1019.1856   2.631   0.0198 *
    ## Year          -1.3088     0.5062  -2.586   0.0216 *
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 9.333 on 14 degrees of freedom
    ## Multiple R-squared:  0.3232, Adjusted R-squared:  0.2749 
    ## F-statistic: 6.686 on 1 and 14 DF,  p-value: 0.02157

Looking at my model trend line the a value is 2681.3162 and the b value
is -1.3088. These a and b values are used to calculate the slope of my
model (in red). My model predicts that per year their will be a decrease
in shootings by 1.3088. The accuracy of my model can be tested with the
R squared value given in the lm summary table. High accuracy models will
be near 1 while low accuracy models will be near 0. My R squared value
is .3232 which means my model explains 32.32% of the data set for Staten
Island. They key areas of divergence are 2007-2010 and 2017-2020. In
both of these areas the model peaks and troughs before regressing to a
more median value.

### Conclusions & Biases

For this RMD analysis I chose to look into the safest areas of New York.
My bias is inherently in the topic that I chose to look into because
safety in itself can be very subjective. Gun violence is high on the
list of my fears due to the seemingly unpredictable nature of it. I
investigated the violence per borough and divided them up to determine
where I would like to live if I had to choose one option. Based on my
graphs and charts it appears that Staten Island is by far the safest in
respect to gun violence. This hypothesis is made with a few different
additional counts of bias. For example, there is an assumption that gun
violence and shootings are a good approximation for overall violence and
while that might be true there is nothing in the dataset that confirms
or denies this. Additionally presenting the data in this way gives bias
towards smaller boroughs. While population isn’t given in this dataset,
it is known that Staten Island has a much smaller population then
Brooklyn. Choosing to breakdown race violence can introduce bias as
well. If the population of a borough is predominantly one race, that
race will be more heavily represented in crimes. Unfortunately
population per ethnicity was not present in the dataset to break things
into per hundred or thousand to make it evenly distributed across the
represented ethnicities. There is bias in everything we do, I tried to
mitigate the bias I introduced into my report by looking at the data
from multiple angles. For example in chart one I looked at the safest
borough by taking a sum total of all the gun violence and then in chart
3 I graphed the gun violence sum per year. It would have been possible
that one borough had a terrible year skewing the data upward.
