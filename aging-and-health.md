Median Age Vs. Healthcare Expense Across Countries 2019 Comparative
Analysis
================
Grant Gealy

health expenditure from world bank median age from kaggle via UN gdp per
capita data from kaggle This project will focus on the relationship
between the age of a society and other economic factors in order to
better understand the economics of specifically how income level and
average age influence factors such as gdp growth and government
spending. Note (age data is from 2020 due to lack of data for every year
)

``` r
#load library 
#install.packages("reshape2")
library(tidyverse)
```

    ## Warning: package 'tidyverse' was built under R version 4.1.3

    ## -- Attaching packages --------------------------------------- tidyverse 1.3.1 --

    ## v ggplot2 3.3.6     v purrr   0.3.4
    ## v tibble  3.1.6     v dplyr   1.0.9
    ## v tidyr   1.2.0     v stringr 1.4.0
    ## v readr   2.1.2     v forcats 0.5.1

    ## Warning: package 'ggplot2' was built under R version 4.1.3

    ## Warning: package 'tibble' was built under R version 4.1.3

    ## Warning: package 'tidyr' was built under R version 4.1.3

    ## Warning: package 'readr' was built under R version 4.1.3

    ## Warning: package 'purrr' was built under R version 4.1.3

    ## Warning: package 'dplyr' was built under R version 4.1.3

    ## Warning: package 'stringr' was built under R version 4.1.3

    ## Warning: package 'forcats' was built under R version 4.1.3

    ## -- Conflicts ------------------------------------------ tidyverse_conflicts() --
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
library(reshape2)
```

    ## Warning: package 'reshape2' was built under R version 4.1.3

    ## 
    ## Attaching package: 'reshape2'

    ## The following object is masked from 'package:tidyr':
    ## 
    ##     smiths

``` r
library(colorspace)
```

    ## Warning: package 'colorspace' was built under R version 4.1.3

``` r
#load  data 
age= read.csv("data/MedianAge.csv")%>% melt()%>%rename(year = variable )%>%rename(age =value)%>%rename(country = Country)
```

    ## Using Country as id variables

``` r
#get rid of x in the year col
age$year= as.numeric(substr(age$year,2,5))
# pick to analyze 
nage=age%>%filter(year==2020) 
head(age)
```

    ##       country year    age
    ## 1 Afghanistan 1950 18.597
    ## 2     Albania 1950 20.640
    ## 3     Algeria 1950 19.927
    ## 4      Angola 1950 19.401
    ## 5   Argentina 1950 25.657
    ## 6     Armenia 1950 22.423

``` r
##load economic freedom data 
fre = read.csv("data/economic_freedom_index2019_data.csv")%>%rename(country = Country.Name)
fre$WEBNAME= NULL

#take out names to make all data numeric then put the names back in 
numfre=sapply(fre[,-c(2,3)], function(x) as.numeric(gsub(
        "$","",x,fixed=TRUE))) 
```

    ## Warning in FUN(X[[i]], ...): NAs introduced by coercion

    ## Warning in FUN(X[[i]], ...): NAs introduced by coercion

    ## Warning in FUN(X[[i]], ...): NAs introduced by coercion

    ## Warning in FUN(X[[i]], ...): NAs introduced by coercion

    ## Warning in FUN(X[[i]], ...): NAs introduced by coercion

    ## Warning in FUN(X[[i]], ...): NAs introduced by coercion

    ## Warning in FUN(X[[i]], ...): NAs introduced by coercion

    ## Warning in FUN(X[[i]], ...): NAs introduced by coercion

    ## Warning in FUN(X[[i]], ...): NAs introduced by coercion

    ## Warning in FUN(X[[i]], ...): NAs introduced by coercion

    ## Warning in FUN(X[[i]], ...): NAs introduced by coercion

    ## Warning in FUN(X[[i]], ...): NAs introduced by coercion

    ## Warning in FUN(X[[i]], ...): NAs introduced by coercion

    ## Warning in FUN(X[[i]], ...): NAs introduced by coercion

    ## Warning in FUN(X[[i]], ...): NAs introduced by coercion

    ## Warning in FUN(X[[i]], ...): NAs introduced by coercion

    ## Warning in FUN(X[[i]], ...): NAs introduced by coercion

    ## Warning in FUN(X[[i]], ...): NAs introduced by coercion

    ## Warning in FUN(X[[i]], ...): NAs introduced by coercion

    ## Warning in FUN(X[[i]], ...): NAs introduced by coercion

    ## Warning in FUN(X[[i]], ...): NAs introduced by coercion

    ## Warning in FUN(X[[i]], ...): NAs introduced by coercion

    ## Warning in FUN(X[[i]], ...): NAs introduced by coercion

    ## Warning in FUN(X[[i]], ...): NAs introduced by coercion

    ## Warning in FUN(X[[i]], ...): NAs introduced by coercion

    ## Warning in FUN(X[[i]], ...): NAs introduced by coercion

    ## Warning in FUN(X[[i]], ...): NAs introduced by coercion

    ## Warning in FUN(X[[i]], ...): NAs introduced by coercion

    ## Warning in FUN(X[[i]], ...): NAs introduced by coercion

    ## Warning in FUN(X[[i]], ...): NAs introduced by coercion

``` r
frename=cbind(fre[,2:3],numfre) 
#check data type for every col
sapply(fre,class) 
```

    ##                   CountryID                     country 
    ##                   "integer"                 "character" 
    ##                      Region                  World.Rank 
    ##                 "character"                 "character" 
    ##                 Region.Rank                 X2019.Score 
    ##                 "character"                 "character" 
    ##             Property.Rights       Judical.Effectiveness 
    ##                 "character"                 "character" 
    ##        Government.Integrity                  Tax.Burden 
    ##                 "character"                 "character" 
    ##              Gov.t.Spending               Fiscal.Health 
    ##                 "character"                 "character" 
    ##            Business.Freedom               Labor.Freedom 
    ##                 "character"                 "character" 
    ##            Monetary.Freedom               Trade.Freedom 
    ##                 "character"                 "character" 
    ##          Investment.Freedom           Financial.Freedom 
    ##                 "character"                 "character" 
    ##             Tariff.Rate....         Income.Tax.Rate.... 
    ##                 "character"                 "character" 
    ##      Corporate.Tax.Rate....         Tax.Burden...of.GDP 
    ##                 "character"                 "character" 
    ##  Gov.t.Expenditure...of.GDP                     Country 
    ##                 "character"                 "character" 
    ##       Population..Millions.         GDP..Billions..PPP. 
    ##                 "character"                 "character" 
    ##         GDP.Growth.Rate.... X5.Year.GDP.Growth.Rate.... 
    ##                 "character"                 "character" 
    ##        GDP.per.Capita..PPP.            Unemployment.... 
    ##                 "character"                 "character" 
    ##               Inflation....       FDI.Inflow..Millions. 
    ##                 "character"                 "character" 
    ##      Public.Debt....of.GDP. 
    ##                 "character"

``` r
head(fre)
```

    ##   CountryID     country                       Region World.Rank Region.Rank
    ## 1         1 Afghanistan                 Asia-Pacific        152          39
    ## 2         2     Albania                       Europe         52          27
    ## 3         3     Algeria Middle East and North Africa        171          14
    ## 4         4      Angola           Sub-Saharan Africa        156          33
    ## 5         5   Argentina                     Americas        148          26
    ## 6         6     Armenia                       Europe         47          24
    ##   X2019.Score Property.Rights Judical.Effectiveness Government.Integrity
    ## 1        51.5            19.6                  29.6                 25.2
    ## 2        66.5            54.8                  30.6                 40.4
    ## 3        46.2            31.6                  36.2                 28.9
    ## 4        50.6            35.9                  26.6                 20.5
    ## 5        52.2            47.8                  44.5                 33.5
    ## 6        67.7            57.2                  46.3                 38.6
    ##   Tax.Burden Gov.t.Spending Fiscal.Health Business.Freedom Labor.Freedom
    ## 1       91.7           80.3          99.3             49.2          60.4
    ## 2       86.3           73.9          80.6             69.3          52.7
    ## 3       76.4           48.7          18.7             61.6          49.9
    ## 4       83.9           80.7          58.2             55.7          58.8
    ## 5       69.3           49.5          33.0             56.4          46.9
    ## 6       84.7           79.0          53.0             78.3          71.4
    ##   Monetary.Freedom Trade.Freedom Investment.Freedom Financial.Freedom
    ## 1             76.7          66.0                 10                10
    ## 2             81.5          87.8                 70                70
    ## 3             74.9          67.4                 30                30
    ## 4             55.4          61.2                 30                40
    ## 5             60.2          70.0                 55                60
    ## 6             77.8          80.8                 75                70
    ##   Tariff.Rate.... Income.Tax.Rate.... Corporate.Tax.Rate....
    ## 1             7.0                20.0                   20.0
    ## 2             1.1                23.0                   15.0
    ## 3             8.8                35.0                   23.0
    ## 4             9.4                17.0                   30.0
    ## 5             7.5                35.0                   30.0
    ## 6             2.1                26.0                   20.0
    ##   Tax.Burden...of.GDP Gov.t.Expenditure...of.GDP     Country
    ## 1                 5.0                       25.6 Afghanistan
    ## 2                24.9                       29.5     Albania
    ## 3                24.5                       41.4     Algeria
    ## 4                20.6                       25.3      Angola
    ## 5                30.8                       41.0   Argentina
    ## 6                21.3                       26.4     Armenia
    ##   Population..Millions. GDP..Billions..PPP. GDP.Growth.Rate....
    ## 1                  35.5              $69.6                  2.5
    ## 2                   2.9              $36.0                  3.9
    ## 3                  41.5             $632.9                  2.0
    ## 4                  28.2             $190.3                  0.7
    ## 5                  44.1             $920.2                  2.9
    ## 6                   3.0              $28.3                  7.5
    ##   X5.Year.GDP.Growth.Rate.... GDP.per.Capita..PPP. Unemployment....
    ## 1                         2.9               $1,958              8.8
    ## 2                         2.5              $12,507             13.9
    ## 3                         3.1              $15,237             10.0
    ## 4                         2.9               $6,753              8.2
    ## 5                         0.7              $20,876              8.7
    ## 6                         3.6               $9,456             18.2
    ##   Inflation.... FDI.Inflow..Millions. Public.Debt....of.GDP.
    ## 1           5.0                  53.9                    7.3
    ## 2           2.0               1,119.1                   71.2
    ## 3           5.6               1,203.0                   25.8
    ## 4          31.7              -2,254.5                   65.3
    ## 5          25.7              11,857.0                   52.6
    ## 6           0.9                 245.7                   53.5

``` r
##load health data 
health = read.csv("data/health.csv")%>%select(-c(Country.Code,Indicator.Name, Indicator.Code))%>%rename(country =ï..Country.Name)%>%melt(id.vars= "country")%>%rename(year = variable)%>%rename(hexpenspergdp =value)%>%na.omit()
#get rid of x in the year col 
health$year= as.numeric(substr(health$year,2,5)) 
head(health)
```

    ##                           country year hexpenspergdp
    ## 10642 Africa Eastern and Southern 2000      6.252428
    ## 10644  Africa Western and Central 2000      3.771294
    ## 10645                      Angola 2000      1.908599
    ## 10646                     Albania 2000      7.233370
    ## 10647                     Andorra 2000      5.960511
    ## 10648                  Arab World 2000      4.000923

``` r
#get the values just for 2020  
healthyear=health[health$year==2019,]

#health$var = as.numeric(gsub("([0-9]+).*$", "\\1", health$var))
#health =sapply(health,function(x) as.numeric(gsub("([0-9]+).*$", "\\1", x)))
```

``` r
#make one data set From the separate data sets 

data=merge(frename,healthyear,"country",all.x = TRUE)
fulldata=merge(data,nage,"country",all.x = TRUE)%>%drop_na(c(age,hexpenspergdp))
```

start analysis exploratory

``` r
#we found the summary statistics for age an, health expenses 
#and gdp growth rate 
fulldata%>%select(age,hexpenspergdp,GDP.Growth.Rate....
                  )%>%summary() 
```

    ##       age        hexpenspergdp    GDP.Growth.Rate....
    ##  Min.   :14.99   Min.   : 1.798   Min.   :-4.400     
    ##  1st Qu.:22.75   1st Qu.: 4.349   1st Qu.: 2.000     
    ##  Median :29.82   Median : 6.411   Median : 3.100     
    ##  Mean   :30.74   Mean   : 6.589   Mean   : 3.348     
    ##  3rd Qu.:39.30   3rd Qu.: 8.437   3rd Qu.: 4.850     
    ##  Max.   :48.58   Max.   :16.767   Max.   :10.900

``` r
#make histograms for variable of interest 
sapply(fulldata%>%select(age,hexpenspergdp,GDP.Growth.Rate....
                  ),hist) 
```

![](aging-and-health_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->![](aging-and-health_files/figure-gfm/unnamed-chunk-6-2.png)<!-- -->![](aging-and-health_files/figure-gfm/unnamed-chunk-6-3.png)<!-- -->

    ##          age       hexpenspergdp GDP.Growth.Rate....
    ## breaks   integer,9 numeric,10    numeric,10         
    ## counts   integer,8 integer,9     integer,9          
    ## density  numeric,8 numeric,9     numeric,9          
    ## mids     numeric,8 numeric,9     numeric,9          
    ## xname    "X[[i]]"  "X[[i]]"      "X[[i]]"           
    ## equidist TRUE      TRUE          TRUE

``` r
#find cor of variables of interest save it to a variable so we 
#can graph it 
test=fulldata%>%select(age,hexpenspergdp,GDP.Growth.Rate.... )%>%na.omit()%>%cor()%>%as.data.frame()
test=test%>%mutate(variable =row.names(test))%>%pivot_longer(cols = 0:3,names_to= "var2",values_to = "cor")
```

``` r
selected=fulldata[fulldata$country=="Japan",]
#make a graph plotting age and health expenses/gdp to illustrate the relationship between the age of a society and the amount they investorsinvest in Healthcare 
ggplot(fulldata,aes(age,hexpenspergdp))+geom_point()+geom_smooth(method = lm)+geom_text(data = selected,mapping=aes(x=age,y=hexpenspergdp,label=country ),color="red")
```

    ## `geom_smooth()` using formula 'y ~ x'

![](aging-and-health_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

``` r
#make a graph plotting government integrity and health expenses I did this to show how corruption in poorer countries might exaggerate the health expenses actually going towards the people
ggplot(fulldata,aes(fulldata$Government.Integrity,hexpenspergdp))+geom_point(aes(size=GDP..Billions..PPP.,color=Region))+geom_smooth(method=lm)+labs(title = "health care spending vs government integrity 2019",y="Healthcare spending",x="government integrity ",size= "gdp in billions" ,color= "Region")+theme_bw()#+geom_text(data = selected,mapping=aes(x=Government.Integrity,y=hexpenspergdp,label=country ))
```

    ## Warning: Use of `fulldata$Government.Integrity` is discouraged. Use `Government.Integrity` instead.
    ## Use of `fulldata$Government.Integrity` is discouraged. Use `Government.Integrity` instead.

    ## `geom_smooth()` using formula 'y ~ x'

    ## Warning: Removed 20 rows containing missing values (geom_point).

![](aging-and-health_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

``` r
  #geom_text(label= fulldata$country)
```

``` r
#make a graph showing the cor 
ggplot(test,aes(y=var2,x= variable, fill =cor))+geom_tile()+geom_text(aes(label= round(cor,4)), color ="blue ")+labs(title ="cor title ",y="y",x="x")+theme(legend.position = "none")+scale_fill_continuous_sequential(palette="red")
```

![](aging-and-health_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

``` r
ggplot(fulldata,aes(age,Business.Freedom))+geom_point()+geom_smooth(method="lm")+labs(title = "age vs. Business freedom",x="age",y=" Business freedom ")
```

    ## `geom_smooth()` using formula 'y ~ x'

![](aging-and-health_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

``` r
agegdp=lm(fulldata$hexpenspergdp~age,data = fulldata)
summary(agegdp)
```

    ## 
    ## Call:
    ## lm(formula = fulldata$hexpenspergdp ~ age, data = fulldata)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -4.6332 -1.6417 -0.2878  1.6220  9.1659 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  2.22394    0.68803   3.232  0.00152 ** 
    ## age          0.14200    0.02149   6.606 6.99e-10 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 2.327 on 145 degrees of freedom
    ## Multiple R-squared:  0.2313, Adjusted R-squared:  0.226 
    ## F-statistic: 43.64 on 1 and 145 DF,  p-value: 6.99e-10

``` r
agegdp$coefficients
```

    ## (Intercept)         age 
    ##   2.2239435   0.1419986

``` r
usa=which(fulldata$country=="United States")
agegdp$residuals[usa]
```

    ##      141 
    ## 9.165917

``` r
#as a society ages they spend more on health 

#can we also do the growth rate 
#as yes that's what I am thinking 
```

\|
