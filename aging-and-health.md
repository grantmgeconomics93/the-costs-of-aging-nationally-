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
#load data 
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
fre
```

    ##     CountryID                                 country
    ## 1           1                             Afghanistan
    ## 2           2                                 Albania
    ## 3           3                                 Algeria
    ## 4           4                                  Angola
    ## 5           5                               Argentina
    ## 6           6                                 Armenia
    ## 7           7                               Australia
    ## 8           8                                 Austria
    ## 9           9                              Azerbaijan
    ## 10         10                                 Bahamas
    ## 11         11                                 Bahrain
    ## 12         12                              Bangladesh
    ## 13         13                                Barbados
    ## 14         14                                 Belarus
    ## 15         15                                 Belgium
    ## 16         16                                  Belize
    ## 17         17                                   Benin
    ## 18         18                                  Bhutan
    ## 19         19                                 Bolivia
    ## 20         20                  Bosnia and Herzegovina
    ## 21         21                                Botswana
    ## 22         22                                  Brazil
    ## 23        186                       Brunei Darussalam
    ## 24         23                                Bulgaria
    ## 25         24                            Burkina Faso
    ## 26         25                                   Burma
    ## 27         26                                 Burundi
    ## 28         27                                Cambodia
    ## 29         28                                Cameroon
    ## 30         29                                  Canada
    ## 31         30                              Cabo Verde
    ## 32         31                Central African Republic
    ## 33         32                                    Chad
    ## 34         33                                   Chile
    ## 35         34                                   China
    ## 36         35                                Colombia
    ## 37         36                                 Comoros
    ## 38         37 Congo, Democratic Republic of the Congo
    ## 39         38                      Congo, Republic of
    ## 40         39                              Costa Rica
    ## 41         40                           Côte d'Ivoire
    ## 42         41                                 Croatia
    ## 43         42                                    Cuba
    ## 44         43                                  Cyprus
    ## 45         44                          Czech Republic
    ## 46         45                                 Denmark
    ## 47         46                                Djibouti
    ## 48         47                                Dominica
    ## 49         48                      Dominican Republic
    ## 50         49                                 Ecuador
    ## 51         50                                   Egypt
    ## 52         51                             El Salvador
    ## 53         52                       Equatorial Guinea
    ## 54         53                                 Eritrea
    ## 55         54                                 Estonia
    ## 56        156                                Eswatini
    ## 57         55                                Ethiopia
    ## 58         56                                    Fiji
    ## 59         57                                 Finland
    ## 60         58                                  France
    ## 61         59                                   Gabon
    ## 62         60                                  Gambia
    ## 63         61                                 Georgia
    ## 64         62                                 Germany
    ## 65         63                                   Ghana
    ## 66         64                                  Greece
    ## 67         65                               Guatemala
    ## 68         66                                  Guinea
    ## 69         67                           Guinea-Bissau
    ## 70         68                                  Guyana
    ## 71         69                                   Haiti
    ## 72         70                                Honduras
    ## 73         71                               Hong Kong
    ## 74         72                                 Hungary
    ## 75         73                                 Iceland
    ## 76         74                                   India
    ## 77         75                               Indonesia
    ## 78         76                                    Iran
    ## 79         77                                    Iraq
    ## 80         78                                 Ireland
    ## 81         79                                  Israel
    ## 82         80                                   Italy
    ## 83         81                                 Jamaica
    ## 84         82                                   Japan
    ## 85         83                                  Jordan
    ## 86         84                              Kazakhstan
    ## 87         85                                   Kenya
    ## 88         86                                Kiribati
    ## 89         87                           Korea, North 
    ## 90         88                            Korea, South
    ## 91        185                                  Kosovo
    ## 92         89                                  Kuwait
    ## 93         90                         Kyrgyz Republic
    ## 94         91                                    Laos
    ## 95         92                                  Latvia
    ## 96         93                                 Lebanon
    ## 97         94                                 Lesotho
    ## 98         95                                 Liberia
    ## 99         96                                   Libya
    ## 100        97                           Liechtenstein
    ## 101        98                               Lithuania
    ## 102        99                              Luxembourg
    ## 103       100                                   Macau
    ## 104       101                               Macedonia
    ## 105       102                              Madagascar
    ## 106       103                                  Malawi
    ## 107       104                                Malaysia
    ## 108       105                                Maldives
    ## 109       106                                    Mali
    ## 110       107                                   Malta
    ## 111       108                              Mauritania
    ## 112       109                               Mauritius
    ## 113       110                                  Mexico
    ## 114       111                              Micronesia
    ## 115       112                                 Moldova
    ## 116       113                                Mongolia
    ## 117       114                              Montenegro
    ## 118       115                                 Morocco
    ## 119       116                              Mozambique
    ## 120       117                                 Namibia
    ## 121       118                                   Nepal
    ## 122       119                             Netherlands
    ## 123       120                             New Zealand
    ## 124       121                               Nicaragua
    ## 125       122                                   Niger
    ## 126       123                                 Nigeria
    ## 127       124                                  Norway
    ## 128       125                                    Oman
    ## 129       126                                Pakistan
    ## 130       127                                  Panama
    ## 131       128                        Papua New Guinea
    ## 132       129                                Paraguay
    ## 133       130                                    Peru
    ## 134       131                             Philippines
    ## 135       132                                  Poland
    ## 136       133                                Portugal
    ## 137       134                                   Qatar
    ## 138       135                                 Romania
    ## 139       136                                  Russia
    ## 140       137                                  Rwanda
    ## 141       138                             Saint Lucia
    ## 142       139        Saint Vincent and the Grenadines
    ## 143       140                                   Samoa
    ## 144       141                   São Tomé and Príncipe
    ## 145       142                            Saudi Arabia
    ## 146       143                                 Senegal
    ## 147       144                                  Serbia
    ## 148       145                              Seychelles
    ## 149       146                            Sierra Leone
    ## 150       147                               Singapore
    ## 151       148                                Slovakia
    ## 152       149                                Slovenia
    ## 153       150                         Solomon Islands
    ## 154       184                                 Somalia
    ## 155       151                            South Africa
    ## 156       152                                   Spain
    ## 157       153                               Sri Lanka
    ## 158       154                                   Sudan
    ## 159       155                                Suriname
    ## 160       157                                  Sweden
    ## 161       158                             Switzerland
    ## 162       159                                   Syria
    ## 163       160                                 Taiwan 
    ## 164       161                              Tajikistan
    ## 165       162                                Tanzania
    ## 166       163                                Thailand
    ## 167       164                             Timor-Leste
    ## 168       165                                    Togo
    ## 169       166                                   Tonga
    ## 170       167                     Trinidad and Tobago
    ## 171       168                                 Tunisia
    ## 172       169                                  Turkey
    ## 173       170                            Turkmenistan
    ## 174       171                                  Uganda
    ## 175       172                                 Ukraine
    ## 176       173                    United Arab Emirates
    ## 177       174                          United Kingdom
    ## 178       175                           United States
    ## 179       176                                 Uruguay
    ## 180       177                              Uzbekistan
    ## 181       178                                 Vanuatu
    ## 182       179                               Venezuela
    ## 183       180                                 Vietnam
    ## 184       181                                   Yemen
    ## 185       182                                  Zambia
    ## 186       183                                Zimbabwe
    ##                           Region World.Rank Region.Rank X2019.Score
    ## 1                   Asia-Pacific        152          39        51.5
    ## 2                         Europe         52          27        66.5
    ## 3   Middle East and North Africa        171          14        46.2
    ## 4             Sub-Saharan Africa        156          33        50.6
    ## 5                       Americas        148          26        52.2
    ## 6                         Europe         47          24        67.7
    ## 7                   Asia-Pacific          5           4        80.9
    ## 8                         Europe         31          16        72.0
    ## 9                   Asia-Pacific         60          13        65.4
    ## 10                      Americas         76          15        62.9
    ## 11  Middle East and North Africa         54           5        66.4
    ## 12                  Asia-Pacific        121          27        55.6
    ## 13                      Americas         67          13        64.7
    ## 14                        Europe        104          42        57.9
    ## 15                        Europe         48          25        67.3
    ## 16                      Americas        123          24        55.4
    ## 17            Sub-Saharan Africa        127          21        55.3
    ## 18                  Asia-Pacific         74          16        62.9
    ## 19                      Americas        173          30        42.3
    ## 20                        Europe         83          37        61.9
    ## 21            Sub-Saharan Africa         36           3        69.5
    ## 22                      Americas        150          27        51.9
    ## 23                  Asia-Pacific         63          14        65.1
    ## 24                        Europe         37          19        69.0
    ## 25            Sub-Saharan Africa         96           9        59.4
    ## 26                  Asia-Pacific        139          35        53.6
    ## 27            Sub-Saharan Africa        162          39        48.9
    ## 28                  Asia-Pacific        105          22        57.8
    ## 29            Sub-Saharan Africa        145          29        52.4
    ## 30                      Americas          8           1        77.7
    ## 31            Sub-Saharan Africa         73           4        63.1
    ## 32            Sub-Saharan Africa        161          38        49.1
    ## 33            Sub-Saharan Africa        159          36        49.9
    ## 34                      Americas         18           3        75.4
    ## 35                  Asia-Pacific        100          20        58.4
    ## 36                      Americas         49           8        67.3
    ## 37            Sub-Saharan Africa        124          20        55.4
    ## 38            Sub-Saharan Africa        157          34        50.3
    ## 39            Sub-Saharan Africa        176          46        39.7
    ## 40                      Americas         61          11        65.3
    ## 41            Sub-Saharan Africa         78           5        62.4
    ## 42                        Europe         86          38        61.4
    ## 43                      Americas        178          31        27.8
    ## 44                        Europe         44          22        68.1
    ## 45                        Europe         23          13        73.7
    ## 46                        Europe         14           6        76.7
    ## 47            Sub-Saharan Africa        169          43        47.1
    ## 48                      Americas         72          14        63.6
    ## 49                      Americas         89          19        61.0
    ## 50                      Americas        170          29        46.9
    ## 51  Middle East and North Africa        144          11        52.5
    ## 52                      Americas         84          17        61.8
    ## 53            Sub-Saharan Africa        174          44        41.0
    ## 54            Sub-Saharan Africa        177          47        38.9
    ## 55                        Europe         15           7        76.6
    ## 56            Sub-Saharan Africa        132          23        54.7
    ## 57            Sub-Saharan Africa        137          26        53.6
    ## 58                  Asia-Pacific         81          18        62.2
    ## 59                        Europe         20          11        74.9
    ## 60                        Europe         71          35        63.8
    ## 61            Sub-Saharan Africa        118          17        56.3
    ## 62            Sub-Saharan Africa        146          30        52.4
    ## 63                        Europe         16           8        75.9
    ## 64                        Europe         24          14        73.5
    ## 65            Sub-Saharan Africa        109          13        57.5
    ## 66                        Europe        106          43        57.7
    ## 67                      Americas         77          16        62.6
    ## 68            Sub-Saharan Africa        120          19        55.7
    ## 69            Sub-Saharan Africa        135          25        54.0
    ## 70                      Americas        113          23        56.8
    ## 71                      Americas        143          25        52.7
    ## 72                      Americas         93          20        60.2
    ## 73                  Asia-Pacific          1           1        90.2
    ## 74                        Europe         64          31        65.0
    ## 75                        Europe         11           4        77.1
    ## 76                  Asia-Pacific        129          31        55.2
    ## 77                  Asia-Pacific         56          11        65.8
    ## 78  Middle East and North Africa        155          13        51.1
    ## 79  Middle East and North Africa        N/A         N/A         N/A
    ## 80                        Europe          6           2        80.5
    ## 81  Middle East and North Africa         27           2        72.8
    ## 82                        Europe         80          36        62.2
    ## 83                      Americas         39           5        68.6
    ## 84                  Asia-Pacific         30           8        72.1
    ## 85  Middle East and North Africa         53           4        66.5
    ## 86                  Asia-Pacific         59          12        65.4
    ## 87            Sub-Saharan Africa        130          22        55.1
    ## 88                  Asia-Pacific        168          41        47.3
    ## 89                  Asia-Pacific        180          43         5.9
    ## 90                  Asia-Pacific         29           7        72.3
    ## 91                        Europe         51          26        67.0
    ## 92  Middle East and North Africa         90           8        60.8
    ## 93                  Asia-Pacific         79          17        62.3
    ## 94                  Asia-Pacific        110          24        57.4
    ## 95                        Europe         35          18        70.4
    ## 96  Middle East and North Africa        154          12        51.1
    ## 97            Sub-Saharan Africa        142          28        53.1
    ## 98            Sub-Saharan Africa        160          37        49.7
    ## 99  Middle East and North Africa        N/A         N/A         N/A
    ## 100                       Europe        N/A         N/A         N/A
    ## 101                       Europe         21          12        74.2
    ## 102                       Europe         17           9        75.9
    ## 103                 Asia-Pacific         34           9        71.0
    ## 104                       Europe         33          17        71.1
    ## 105           Sub-Saharan Africa        114          15        56.6
    ## 106           Sub-Saharan Africa        153          32        51.4
    ## 107                 Asia-Pacific         22           6        74.0
    ## 108                 Asia-Pacific        141          37        53.2
    ## 109           Sub-Saharan Africa        103          12        58.1
    ## 110                       Europe         41          20        68.6
    ## 111           Sub-Saharan Africa        119          18        55.7
    ## 112           Sub-Saharan Africa         25           1        73.0
    ## 113                     Americas         66          12        64.7
    ## 114                 Asia-Pacific        149          38        51.9
    ## 115                       Europe         97          40        59.1
    ## 116                 Asia-Pacific        126          29        55.4
    ## 117                       Europe         92          39        60.5
    ## 118 Middle East and North Africa         75           6        62.9
    ## 119           Sub-Saharan Africa        163          40        48.6
    ## 120           Sub-Saharan Africa         99          10        58.7
    ## 121                 Asia-Pacific        136          34        53.8
    ## 122                       Europe         13           5        76.8
    ## 123                 Asia-Pacific          3           3        84.4
    ## 124                     Americas        107          21        57.7
    ## 125           Sub-Saharan Africa        151          31        51.6
    ## 126           Sub-Saharan Africa        111          14        57.3
    ## 127                       Europe         26          15        73.0
    ## 128 Middle East and North Africa         88           7        61.0
    ## 129                 Asia-Pacific        131          32        55.0
    ## 130                     Americas         50           9        67.2
    ## 131                 Asia-Pacific        101          21        58.4
    ## 132                     Americas         85          18        61.8
    ## 133                     Americas         45           7        67.8
    ## 134                 Asia-Pacific         70          15        63.8
    ## 135                       Europe         46          23        67.8
    ## 136                       Europe         62          30        65.3
    ## 137 Middle East and North Africa         28           3        72.6
    ## 138                       Europe         42          21        68.6
    ## 139                       Europe         98          41        58.9
    ## 140           Sub-Saharan Africa         32           2        71.1
    ## 141                     Americas         38           4        68.7
    ## 142                     Americas         55          10        65.8
    ## 143                 Asia-Pacific         82          19        62.2
    ## 144           Sub-Saharan Africa        134          24        54.0
    ## 145 Middle East and North Africa         91           9        60.7
    ## 146           Sub-Saharan Africa        117          16        56.3
    ## 147                       Europe         69          34        63.9
    ## 148           Sub-Saharan Africa         87           6        61.4
    ## 149           Sub-Saharan Africa        167          42        47.5
    ## 150                 Asia-Pacific          2           2        89.4
    ## 151                       Europe         65          32        65.0
    ## 152                       Europe         58          29        65.5
    ## 153                 Asia-Pacific        133          33        54.6
    ## 154           Sub-Saharan Africa        N/A         N/A         N/A
    ## 155           Sub-Saharan Africa        102          11        58.3
    ## 156                       Europe         57          28        65.7
    ## 157                 Asia-Pacific        115          25        56.4
    ## 158           Sub-Saharan Africa        166          41        47.7
    ## 159                     Americas        165          28        48.1
    ## 160                       Europe         19          10        75.2
    ## 161                       Europe          4           1        81.9
    ## 162 Middle East and North Africa        N/A         N/A         N/A
    ## 163                 Asia-Pacific         10           5        77.3
    ## 164                 Asia-Pacific        122          28        55.6
    ## 165           Sub-Saharan Africa         94           7        60.2
    ## 166                 Asia-Pacific         43          10        68.3
    ## 167                 Asia-Pacific        172          42        44.2
    ## 168           Sub-Saharan Africa        158          35        50.3
    ## 169                 Asia-Pacific        108          23        57.7
    ## 170                     Americas        112          22        57.0
    ## 171 Middle East and North Africa        125          10        55.4
    ## 172                       Europe         68          33        64.6
    ## 173                 Asia-Pacific        164          40        48.4
    ## 174           Sub-Saharan Africa         95           8        59.7
    ## 175                       Europe        147          44        52.3
    ## 176 Middle East and North Africa          9           1        77.6
    ## 177                       Europe          7           3        78.9
    ## 178                     Americas         12           2        76.8
    ## 179                     Americas         40           6        68.6
    ## 180                 Asia-Pacific        140          36        53.3
    ## 181                 Asia-Pacific        116          26        56.4
    ## 182                     Americas        179          32        25.9
    ## 183                 Asia-Pacific        128          30        55.3
    ## 184 Middle East and North Africa        N/A         N/A         N/A
    ## 185           Sub-Saharan Africa        138          27        53.6
    ## 186           Sub-Saharan Africa        175          45        40.4
    ##     Property.Rights Judical.Effectiveness Government.Integrity Tax.Burden
    ## 1              19.6                  29.6                 25.2       91.7
    ## 2              54.8                  30.6                 40.4       86.3
    ## 3              31.6                  36.2                 28.9       76.4
    ## 4              35.9                  26.6                 20.5       83.9
    ## 5              47.8                  44.5                 33.5       69.3
    ## 6              57.2                  46.3                 38.6       84.7
    ## 7              79.1                  86.5                 79.9       62.8
    ## 8              84.2                  71.3                 77.4       50.5
    ## 9              59.1                  53.1                 44.7       87.5
    ## 10             42.2                  46.9                 43.7       97.3
    ## 11             63.5                  50.7                 53.6       99.7
    ## 12             36.1                  34.5                 24.4       72.7
    ## 13             52.9                  59.9                 53.8       70.1
    ## 14             55.2                  51.7                 37.7       89.4
    ## 15             81.3                  61.6                 72.5       47.1
    ## 16             41.7                  46.9                 27.2       79.9
    ## 17             37.2                  32.8                 28.1       69.3
    ## 18             62.5                  55.4                 54.5       83.0
    ## 19             20.5                  12.3                 19.7       82.4
    ## 20             40.2                  37.9                 30.2       84.3
    ## 21             58.1                  45.7                 52.4       82.7
    ## 22             57.3                  51.7                 28.1       70.5
    ## 23             64.0                  56.0                 43.7       90.7
    ## 24             62.5                  41.9                 35.1       90.2
    ## 25             49.1                  42.9                 36.6       81.9
    ## 26             34.7                  18.1                 30.6       86.6
    ## 27             20.6                  31.0                 26.2       74.0
    ## 28             37.4                  27.6                 16.7       89.7
    ## 29             42.5                  31.3                 25.5       74.4
    ## 30             87.0                  69.4                 84.6       76.8
    ## 31             44.1                  49.0                 43.7       76.4
    ## 32             19.6                  29.6                 23.2       65.2
    ## 33             26.7                  24.6                 23.2       46.1
    ## 34             68.7                  56.3                 62.3       77.3
    ## 35             49.9                  75.2                 49.1       70.4
    ## 36             59.2                  34.3                 33.5       74.3
    ## 37             36.5                  29.6                 24.4       63.9
    ## 38             25.3                  30.7                 26.2       73.8
    ## 39             33.2                  29.6                 25.3       59.5
    ## 40             58.3                  54.0                 54.5       79.2
    ## 41             40.9                  47.8                 38.1       77.5
    ## 42             66.0                  42.9                 38.6       66.4
    ## 43             31.6                  10.0                 37.7       48.8
    ## 44             73.1                  48.1                 43.7       74.9
    ## 45             74.8                  47.6                 52.1       82.6
    ## 46             86.2                  77.8                 85.8       42.0
    ## 47             29.7                  18.1                 28.1       76.2
    ## 48             49.2                  63.8                 54.5       72.1
    ## 49             50.6                  18.1                 23.2       84.6
    ## 50             35.9                  20.2                 25.3       77.0
    ## 51             37.0                  48.3                 29.2       85.2
    ## 52             37.6                  29.1                 23.4       78.1
    ## 53             29.7                  18.1                 15.8       71.3
    ## 54             35.5                  18.1                 19.7       81.4
    ## 55             81.5                  76.0                 73.1       79.9
    ## 56             41.7                  42.9                 35.0       74.8
    ## 57             32.6                  40.9                 35.1       77.2
    ## 58             67.3                  42.9                 23.4       81.1
    ## 59             89.6                  81.2                 92.5       66.8
    ## 60             82.5                  66.1                 67.9       48.4
    ## 61             28.1                  30.6                 35.5       75.8
    ## 62             39.9                  42.5                 41.2       74.3
    ## 63             65.9                  54.6                 58.5       87.1
    ## 64             79.9                  75.4                 81.3       60.8
    ## 65             49.1                  44.2                 35.5       78.8
    ## 66             52.4                  49.5                 37.7       59.1
    ## 67             40.3                  32.3                 26.4       79.2
    ## 68             34.7                  32.6                 25.5       69.4
    ## 69             32.6                  42.9                 25.3       88.8
    ## 70             41.7                  42.9                 33.2       67.0
    ## 71             10.4                  25.3                 20.3       79.9
    ## 72             43.4                  31.0                 25.3       82.8
    ## 73             93.3                  75.3                 83.8       93.1
    ## 74             60.9                  45.2                 35.3       78.6
    ## 75             87.4                  63.8                 83.8       72.7
    ## 76             57.3                  61.6                 47.8       79.4
    ## 77             52.2                  53.5                 39.5       83.7
    ## 78             33.5                  41.3                 35.0       80.9
    ## 79             37.0                  12.3                 20.3        N/A
    ## 80             85.8                  68.4                 78.0       76.3
    ## 81             80.0                  73.4                 67.9       61.9
    ## 82             71.7                  49.8                 43.7       55.6
    ## 83             60.7                  49.2                 45.0       80.2
    ## 84             84.1                  68.5                 78.0       68.2
    ## 85             58.4                  52.6                 50.3       91.4
    ## 86             59.3                  56.1                 40.3       93.4
    ## 87             53.8                  46.9                 32.1       79.5
    ## 88             44.1                  34.3                 35.1       73.0
    ## 89             31.6                   5.0                 24.4        0.0
    ## 90             79.3                  57.5                 50.5       64.2
    ## 91             57.2                  53.5                 44.7       92.5
    ## 92             52.9                  43.3                 35.3       97.7
    ## 93             49.9                  27.9                 27.2       94.1
    ## 94             38.8                  42.5                 33.5       86.9
    ## 95             67.3                  48.4                 35.5       77.0
    ## 96             39.5                  26.6                 18.2       91.8
    ## 97             41.5                  45.7                 30.9       59.4
    ## 98             26.7                  39.0                 24.2       82.7
    ## 99              7.6                  24.4                 15.8        N/A
    ## 100             N/A                   N/A                  N/A        N/A
    ## 101            73.6                  61.2                 47.8       86.4
    ## 102            83.0                  72.4                 85.8       65.4
    ## 103            60.0                  60.0                 33.2       77.1
    ## 104            65.1                  60.7                 44.7       91.8
    ## 105            33.2                  24.4                 14.3       91.0
    ## 106            35.8                  40.1                 25.2       79.8
    ## 107            84.1                  68.2                 55.4       85.6
    ## 108            43.9                  36.4                 33.5       95.8
    ## 109            33.7                  33.4                 29.6       68.7
    ## 110            69.8                  50.4                 50.3       64.2
    ## 111            27.5                  30.6                 30.6       78.0
    ## 112            69.5                  62.1                 40.3       92.1
    ## 113            59.1                  34.9                 26.3       75.8
    ## 114             7.6                  26.6                 36.6       92.8
    ## 115            55.2                  29.6                 25.4       85.4
    ## 116            48.2                  23.8                 29.8       88.5
    ## 117            55.4                  51.8                 39.5       85.3
    ## 118            57.2                  47.1                 39.2       72.2
    ## 119            33.9                  35.2                 28.1       75.5
    ## 120            55.9                  54.7                 49.8       66.5
    ## 121            39.2                  34.7                 26.2       84.0
    ## 122            88.0                  74.7                 89.1       51.6
    ## 123            95.0                  83.5                 96.7       71.0
    ## 124            33.4                  18.7                 20.3       76.9
    ## 125            37.2                  31.0                 34.1       76.9
    ## 126            36.5                  34.3                 20.5       85.0
    ## 127            86.1                  81.2                 92.3       57.4
    ## 128            58.1                  51.6                 53.8       97.8
    ## 129            41.5                  40.2                 30.6       80.5
    ## 130            60.4                  30.1                 34.1       85.0
    ## 131            37.4                  49.0                 37.2       71.8
    ## 132            39.5                  30.0                 25.5       96.3
    ## 133            56.1                  34.0                 31.8       80.6
    ## 134            48.7                  36.4                 30.9       76.9
    ## 135            62.3                  44.0                 49.8       74.9
    ## 136            71.5                  64.3                 59.5       59.9
    ## 137            64.5                  60.0                 77.4       99.7
    ## 138            66.7                  51.9                 39.8       89.7
    ## 139            52.4                  45.1                 36.6       89.4
    ## 140            72.2                  83.2                 67.9       79.8
    ## 141            65.9                  63.8                 50.3       76.2
    ## 142            36.5                  63.8                 50.5       71.2
    ## 143            53.8                  31.0                 37.7       79.9
    ## 144            37.4                  26.6                 35.5       87.2
    ## 145            55.0                  62.7                 49.8       99.8
    ## 146            47.8                  40.4                 40.3       70.8
    ## 147            50.1                  44.8                 37.2       82.0
    ## 148            58.2                  37.5                 39.2       76.3
    ## 149            35.5                  34.5                 26.2       87.3
    ## 150            97.4                  92.4                 95.1       90.4
    ## 151            68.5                  37.2                 37.7       78.6
    ## 152            76.4                  46.5                 53.6       58.4
    ## 153            49.9                  51.7                 33.5       65.5
    ## 154            33.7                  26.6                  7.9        N/A
    ## 155            58.8                  39.3                 39.7       62.1
    ## 156            72.9                  51.4                 51.9       62.3
    ## 157            44.7                  39.4                 28.9       84.9
    ## 158            27.5                  22.2                 26.2       86.3
    ## 159            49.1                  22.2                 35.5       70.9
    ## 160            89.5                  84.0                 88.0       43.2
    ## 161            85.3                  82.0                 88.0       70.5
    ## 162            37.0                  24.4                 20.3        N/A
    ## 163            85.4                  70.1                 69.2       75.0
    ## 164            47.8                  52.1                 36.4       91.8
    ## 165            35.4                  41.4                 33.2       80.5
    ## 166            53.7                  45.9                 36.4       81.3
    ## 167            29.7                  13.1                 32.1       96.3
    ## 168            35.5                  29.6                 28.1       67.8
    ## 169            59.2                  26.6                 38.1       85.5
    ## 170            52.3                  40.6                 32.9       82.3
    ## 171            49.2                  42.7                 36.6       74.4
    ## 172            55.8                  49.8                 41.2       76.4
    ## 173            31.6                  29.8                 20.3       95.9
    ## 174            42.2                  38.5                 25.4       73.3
    ## 175            43.9                  31.5                 29.6       81.8
    ## 176            81.8                  87.1                 78.8       99.2
    ## 177            92.3                  85.9                 83.8       64.7
    ## 178            79.3                  78.6                 77.4       75.1
    ## 179            68.3                  58.9                 69.2       77.2
    ## 180            49.8                  34.3                 25.2       91.3
    ## 181            65.9                  36.4                 51.9       97.3
    ## 182             7.6                  13.1                  7.9       74.7
    ## 183            49.8                  40.3                 34.0       79.7
    ## 184            19.6                  22.2                 20.3        N/A
    ## 185            45.0                  35.6                 32.3       72.3
    ## 186            29.7                  24.8                 15.8       62.3
    ##     Gov.t.Spending Fiscal.Health Business.Freedom Labor.Freedom
    ## 1             80.3          99.3             49.2          60.4
    ## 2             73.9          80.6             69.3          52.7
    ## 3             48.7          18.7             61.6          49.9
    ## 4             80.7          58.2             55.7          58.8
    ## 5             49.5          33.0             56.4          46.9
    ## 6             79.0          53.0             78.3          71.4
    ## 7             60.1          86.2             88.3          84.1
    ## 8             24.5          85.5             74.9          68.7
    ## 9             59.5          89.4             69.5          63.9
    ## 10            86.8          65.7             68.5          67.5
    ## 11            62.7           3.7             71.4          71.1
    ## 12            94.5          77.6             50.9          68.2
    ## 13            65.0          79.5             69.8          59.9
    ## 14            41.3          85.4             75.0          75.3
    ## 15            15.2          73.4             78.1          61.0
    ## 16            65.9          39.1             61.8          54.8
    ## 17            83.4          27.9             62.4          53.8
    ## 18            71.6          77.6             68.7          79.5
    ## 19            49.3          17.6             58.8          52.9
    ## 20            46.1          96.6             49.7          67.0
    ## 21            65.9          94.6             68.7          68.2
    ## 22            55.2           5.9             57.9          51.9
    ## 23            59.9          20.0             80.2          90.8
    ## 24            63.9          98.8             62.7          68.4
    ## 25            80.0          61.8             51.6          52.3
    ## 26            85.4          78.3             52.8          65.7
    ## 27            83.3          23.3             50.3          67.5
    ## 28            85.9          89.1             29.9          63.0
    ## 29            87.5          58.4             44.4          47.8
    ## 30            51.3          83.1             81.9          73.7
    ## 31            71.2          59.7             65.2          55.7
    ## 32            94.2          94.3             24.2          40.1
    ## 33            92.4          85.2             28.1          43.2
    ## 34            81.0          89.0             76.6          65.0
    ## 35            70.1          76.0             56.2          64.2
    ## 36            75.0          79.2             71.4          78.5
    ## 37            73.4          91.7             57.2          60.3
    ## 38            93.9          96.9             53.2          41.9
    ## 39            40.6           0.0             38.2          35.8
    ## 40            88.4          42.5             67.2          55.2
    ## 41            83.9          74.3             61.0          52.5
    ## 42            33.4          85.4             60.7          44.0
    ## 43             0.0          15.6             20.0          20.0
    ## 44            55.2          80.3             76.9          59.5
    ## 45            52.1          97.6             72.4          78.1
    ## 46            14.4          96.7             90.7          86.4
    ## 47            27.3          18.1             54.7          60.4
    ## 48            53.5          84.7             70.7          60.4
    ## 49            90.3          89.9             51.9          57.6
    ## 50            55.5          32.1             54.1          48.2
    ## 51            68.1           0.0             65.9          51.6
    ## 52            86.3          81.9             57.2          53.1
    ## 53            67.6          16.4             37.6          32.7
    ## 54            73.9           0.0             17.7          70.0
    ## 55            51.1          99.8             75.3          57.2
    ## 56            65.6          18.3             59.2          67.5
    ## 57            90.4          83.3             48.6          58.0
    ## 58            71.7          82.4             63.0          72.9
    ## 59             7.2          86.4             89.4          50.3
    ## 60             3.9          64.9             81.2          45.2
    ## 61            86.6          82.1             52.1          53.0
    ## 62            70.7           0.0             54.0          67.4
    ## 63            73.6          93.9             85.8          76.6
    ## 64            42.3          91.8             83.3          52.8
    ## 65            82.0          23.9             56.5          59.9
    ## 66            23.3          79.0             74.1          52.5
    ## 67            95.6          96.2             53.6          48.7
    ## 68            89.8          87.2             54.6          54.9
    ## 69            86.7          81.4             35.9          61.2
    ## 70            69.4          77.6             59.3          62.0
    ## 71            88.3          95.9             36.2          62.6
    ## 72            78.2          95.9             56.9          32.0
    ## 73            90.3         100.0             96.4          89.2
    ## 74            31.7          85.0             61.1          64.7
    ## 75            44.0          96.7             88.4          64.1
    ## 76            77.3          14.7             57.1          41.8
    ## 77            91.4          88.1             69.3          49.3
    ## 78            89.8          89.5             62.2          50.7
    ## 79            52.8          13.3             54.4          53.1
    ## 80            77.4          89.0             83.1          75.3
    ## 81            52.4          85.3             71.4          65.1
    ## 82            26.5          71.3             71.7          51.1
    ## 83            76.0          80.0             78.0          73.6
    ## 84            55.0          55.7             80.5          79.0
    ## 85            73.4          60.6             61.8          52.7
    ## 86            83.7          41.0             73.9          86.2
    ## 87            77.8          13.8             55.8          63.4
    ## 88             0.0          98.6             41.9          50.7
    ## 89             0.0           0.0              5.0           5.0
    ## 90            68.6          96.8             91.3          57.4
    ## 91            77.7          96.0             73.8          64.9
    ## 92            17.3          99.1             57.4          61.7
    ## 93            54.2          78.4             73.4          79.8
    ## 94            85.3          66.5             60.1          60.1
    ## 95            57.1          96.9             77.5          73.3
    ## 96            75.6           0.0             47.9          46.5
    ## 97            33.0          63.5             53.3          58.8
    ## 98            62.1          69.1             50.6          38.3
    ## 99               0          20.0             40.2          51.3
    ## 100            N/A           N/A              N/A           N/A
    ## 101           65.1          97.3             75.2          63.6
    ## 102           46.6          98.9             68.8          45.9
    ## 103           90.4         100.0             60.0          50.0
    ## 104           70.0          82.9             80.2          71.5
    ## 105           91.8          85.5             47.3          44.6
    ## 106           70.3          19.1             41.7          64.0
    ## 107           83.2          82.4             83.9          74.4
    ## 108           60.8          10.7             78.3          70.8
    ## 109           85.4          84.2             53.8          52.2
    ## 110           56.1          94.5             67.1          61.3
    ## 111           74.2          80.6             61.9          51.5
    ## 112           80.3          73.6             79.8          60.8
    ## 113           78.2          83.2             67.8          58.6
    ## 114            0.0          98.8             57.4          71.9
    ## 115           59.1          92.0             67.0          39.0
    ## 116           63.1           6.2             66.0          75.0
    ## 117           32.6          23.2             73.3          73.4
    ## 118           72.7          66.9             70.3          33.1
    ## 119           66.9          16.6             57.1          42.0
    ## 120           48.9          15.7             65.8          85.1
    ## 121           83.7          98.5             61.8          47.9
    ## 122           42.9          93.3             81.4          60.3
    ## 123           50.4          98.6             91.0          86.7
    ## 124           79.1          93.9             56.0          55.8
    ## 125           75.6          22.2             56.3          48.2
    ## 126           96.5          68.2             51.2          83.3
    ## 127           25.3          97.3             89.4          53.7
    ## 128           32.5          16.1             75.2          57.3
    ## 129           87.6          49.2             56.1          41.8
    ## 130           85.3          91.3             73.6          43.4
    ## 131           89.1          75.2             62.2          72.6
    ## 132           78.9          96.3             61.5          29.2
    ## 133           86.1          88.5             67.8          63.5
    ## 134           88.7          97.1             61.3          57.9
    ## 135           48.8          86.4             65.4          63.9
    ## 136           35.6          69.8             79.7          44.3
    ## 137           56.8          94.0             71.2          65.9
    ## 138           69.0          89.3             63.1          64.5
    ## 139           62.3          86.6             78.4          52.5
    ## 140           79.4          86.3             56.2          82.2
    ## 141           79.3          81.3             76.3          69.2
    ## 142           74.3          85.0             76.5          73.5
    ## 143           62.3          93.6             77.0          78.2
    ## 144           67.0          62.3             65.1          42.7
    ## 145           57.5          19.4             72.3          63.3
    ## 146           73.3          60.0             53.3          39.4
    ## 147           45.1          90.1             72.9          67.4
    ## 148           60.3          92.0             63.3          63.2
    ## 149           84.4          13.2             44.9          29.3
    ## 150           90.7          80.0             90.8          91.0
    ## 151           46.1          87.2             61.3          53.4
    ## 152           38.3          82.6             79.3          61.2
    ## 153           36.5          89.4             68.6          72.0
    ## 154            N/A           N/A             31.7           N/A
    ## 155           67.6          62.6             64.3          59.1
    ## 156           46.2          51.1             66.8          57.8
    ## 157           88.3          30.4             75.1          58.8
    ## 158           96.6          76.1             52.1          59.0
    ## 159           77.2           9.6             48.3          73.5
    ## 160           26.7          96.6             88.0          53.9
    ## 161           64.8          96.3             75.4          72.5
    ## 162            N/A           N/A             49.6          58.2
    ## 163           90.6          91.6             93.2          60.9
    ## 164           64.6          60.3             67.3          49.2
    ## 165           90.3          85.2             46.6          66.2
    ## 166           85.8          96.5             82.5          63.9
    ## 167            0.9          20.0             60.5          58.8
    ## 168           77.0          24.5             50.4          46.7
    ## 169           40.9          93.4             75.3          69.9
    ## 170           61.9          16.6             67.8          75.6
    ## 171           74.4          37.9             76.7          50.3
    ## 172           65.1          92.2             66.0          49.2
    ## 173           92.0          92.3             30.0          20.0
    ## 174           88.7          68.6             46.3          83.2
    ## 175           46.9          82.6             66.1          46.7
    ## 176           68.8          88.9             79.9          81.1
    ## 177           48.2          68.6             92.9          73.5
    ## 178           57.1          53.1             83.8          89.4
    ## 179           67.5          69.9             74.3          71.9
    ## 180           67.4          98.7             72.5          58.7
    ## 181           54.1          15.3             52.4          58.8
    ## 182           58.1          17.6             33.9          28.0
    ## 183           74.1          40.7             63.5          62.8
    ## 184           83.7           0.0             45.1          49.8
    ## 185           80.1          12.3             71.1          46.0
    ## 186           74.5          23.7             33.4          43.3
    ##     Monetary.Freedom Trade.Freedom Investment.Freedom Financial.Freedom
    ## 1               76.7          66.0                 10                10
    ## 2               81.5          87.8                 70                70
    ## 3               74.9          67.4                 30                30
    ## 4               55.4          61.2                 30                40
    ## 5               60.2          70.0                 55                60
    ## 6               77.8          80.8                 75                70
    ## 7               86.6          87.6                 80                90
    ## 8               81.5          86.0                 90                70
    ## 9               63.0          74.6                 60                60
    ## 10              78.1          47.8                 50                60
    ## 11              81.6          83.8                 75                80
    ## 12              69.9          63.6                 45                30
    ## 13              78.3          56.6                 70                60
    ## 14              67.0          76.4                 30                10
    ## 15              76.1          86.0                 85                70
    ## 16              78.7          64.0                 55                50
    ## 17              86.4          61.8                 70                50
    ## 18              72.6          79.4                 20                30
    ## 19              68.8          70.4                 15                40
    ## 20              83.1          82.6                 65                60
    ## 21              78.8          83.8                 65                70
    ## 22              75.5          69.0                 50                50
    ## 23              76.5          84.0                 65                50
    ## 24              88.0          86.0                 70                60
    ## 25              86.2          65.2                 65                40
    ## 26              69.6          70.8                 30                20
    ## 27              62.2          68.2                 50                30
    ## 28              79.4          65.4                 60                50
    ## 29              84.0          53.4                 30                50
    ## 30              77.2          86.8                 80                80
    ## 31              84.1          68.2                 80                60
    ## 32              72.3          51.0                 45                30
    ## 33              82.3          47.2                 60                40
    ## 34              84.5          88.8                 85                70
    ## 35              71.9          73.0                 25                20
    ## 36              75.6          76.0                 80                70
    ## 37              82.8          70.0                 45                30
    ## 38              49.1          62.6                 30                20
    ## 39              82.6          56.8                 45                30
    ## 40              83.2          81.4                 70                50
    ## 41              74.2          73.6                 75                50
    ## 42              78.5          86.0                 75                60
    ## 43              65.6          64.0                 10                10
    ## 44              84.0          86.0                 75                60
    ## 45              81.5          86.0                 80                80
    ## 46              84.1          86.0                 90                80
    ## 47              72.7          50.4                 80                50
    ## 48              85.7          68.2                 70                30
    ## 49              79.7          75.8                 70                40
    ## 50              73.5          66.4                 35                40
    ## 51              62.3          71.8                 60                50
    ## 52              79.0          81.4                 75                60
    ## 53              83.7          48.8                 40                30
    ## 54              61.0          69.2                  0                20
    ## 55              79.6          86.0                 90                70
    ## 56              73.7          87.6                 50                40
    ## 57              60.8          60.8                 35                20
    ## 58              73.5          62.8                 55                50
    ## 59              84.8          86.0                 85                80
    ## 60              79.1          81.0                 75                70
    ## 61              80.0          51.2                 60                40
    ## 62              62.4          61.6                 65                50
    ## 63              76.0          88.6                 80                70
    ## 64              77.9          86.0                 80                70
    ## 65              66.3          63.4                 70                60
    ## 66              79.1          81.0                 55                50
    ## 67              77.0          82.2                 70                50
    ## 68              66.4          63.2                 50                40
    ## 69              78.1          55.6                 30                30
    ## 70              76.9          66.8                 55                30
    ## 71              66.5          72.0                 45                30
    ## 72              73.0          79.4                 65                60
    ## 73              86.4          95.0                 90                90
    ## 74              81.8          86.0                 80                70
    ## 75              81.7          87.0                 85                70
    ## 76              72.4          72.4                 40                40
    ## 77              77.4          79.8                 45                60
    ## 78              60.1          54.6                  5                10
    ## 79              81.4           N/A                N/A               N/A
    ## 80              87.0          86.0                 90                70
    ## 81              86.2          84.4                 75                70
    ## 82              84.0          86.0                 85                50
    ## 83              82.6          68.4                 80                50
    ## 84              85.9          80.0                 70                60
    ## 85              85.0          81.4                 70                60
    ## 86              70.9          80.0                 50                50
    ## 87              72.7          60.4                 55                50
    ## 88              81.1          53.2                 25                30
    ## 89               0.0           0.0                  0                 0
    ## 90              82.0          80.4                 70                70
    ## 91              78.3          70.8                 65                30
    ## 92              70.6          79.0                 55                60
    ## 93              74.4          78.6                 60                50
    ## 94              78.5          81.8                 35                20
    ## 95              81.1          86.0                 85                60
    ## 96              78.1          79.0                 60                50
    ## 97              75.0          81.0                 55                40
    ## 98              68.9          60.1                 55                20
    ## 99              52.8           N/A                  5               N/A
    ## 100              N/A           N/A                 85                80
    ## 101             84.6          86.0                 80                70
    ## 102             82.6          86.0                 95                80
    ## 103             76.5          90.0                 85                70
    ## 104             78.7          82.0                 65                60
    ## 105             72.4          69.2                 55                50
    ## 106             65.5          75.4                 50                50
    ## 107             78.6          82.0                 60                50
    ## 108             81.0          62.6                 35                30
    ## 109             81.6          69.8                 65                40
    ## 110             78.2          86.0                 85                60
    ## 111             81.2          62.6                 50                40
    ## 112             79.4          88.4                 80                70
    ## 113             75.9          81.4                 75                60
    ## 114             85.8          80.6                 35                30
    ## 115             73.5          78.0                 55                50
    ## 116             77.8          75.8                 50                60
    ## 117             81.6          84.7                 75                50
    ## 118             83.5          77.4                 65                70
    ## 119             65.4          78.0                 35                50
    ## 120             74.4          83.0                 65                40
    ## 121             69.4          60.4                 10                30
    ## 122             84.0          86.0                 90                80
    ## 123             87.5          92.4                 80                80
    ## 124             72.7          76.0                 60                50
    ## 125             76.7          65.8                 55                40
    ## 126             65.0          62.4                 45                40
    ## 127             75.4          83.2                 75                60
    ## 128             77.7          87.0                 65                60
    ## 129             72.6          64.8                 55                40
    ## 130             79.4          79.2                 75                70
    ## 131             70.0          80.9                 25                30
    ## 132             72.8          76.6                 75                60
    ## 133             83.9          86.4                 75                60
    ## 134             69.6          78.2                 60                60
    ## 135             82.1          86.0                 80                70
    ## 136             83.0          86.0                 70                60
    ## 137             78.4          83.2                 60                60
    ## 138             82.7          86.0                 70                50
    ## 139             65.1          77.8                 30                30
    ## 140             76.1          70.4                 60                40
    ## 141             83.9          73.2                 65                40
    ## 142             82.2          66.6                 70                40
    ## 143             83.5          63.8                 55                30
    ## 144             70.5          64.2                 60                30
    ## 145             78.1          76.0                 45                50
    ## 146             78.2          72.0                 60                40
    ## 147             80.0          77.0                 70                50
    ## 148             80.0          81.4                 55                30
    ## 149             65.0          69.4                 60                20
    ## 150             85.3          94.8                 85                80
    ## 151             78.6          86.0                 75                70
    ## 152             83.6          86.0                 70                50
    ## 153             86.0          56.8                 15                30
    ## 154              N/A           N/A                N/A               N/A
    ## 155             75.2          76.0                 45                50
    ## 156             87.5          86.0                 85                70
    ## 157             70.1          76.2                 40                40
    ## 158             56.9          45.0                  5                20
    ## 159             56.0          64.6                 40                30
    ## 160             82.0          86.0                 85                80
    ## 161             85.2          87.4                 85                90
    ## 162             48.3          47.0                  0               N/A
    ## 163             84.4          87.0                 60                60
    ## 164             68.5          73.6                 25                30
    ## 165             70.4          67.8                 55                50
    ## 166             75.2          83.0                 55                60
    ## 167             79.5          75.0                 45                20
    ## 168             79.1          69.4                 65                30
    ## 169             69.4          73.6                 40                20
    ## 170             75.1          68.4                 60                50
    ## 171             76.0          71.4                 45                30
    ## 172             70.0          79.6                 70                60
    ## 173             73.4          76.0                 10                10
    ## 174             80.1          75.4                 55                40
    ## 175             58.6          75.0                 35                30
    ## 176             80.9          84.4                 40                60
    ## 177             81.2          86.0                 90                80
    ## 178             76.6          86.6                 85                80
    ## 179             72.9          78.6                 85                30
    ## 180             58.9          62.6                 10                10
    ## 181             75.0          64.4                 65                40
    ## 182              0.0          60.0                  0                10
    ## 183             68.9          79.2                 30                40
    ## 184             61.5          71.4                 50               N/A
    ## 185             70.3          72.6                 55                50
    ## 186             72.4          70.0                 25                10
    ##     Tariff.Rate.... Income.Tax.Rate.... Corporate.Tax.Rate....
    ## 1               7.0                20.0                   20.0
    ## 2               1.1                23.0                   15.0
    ## 3               8.8                35.0                   23.0
    ## 4               9.4                17.0                   30.0
    ## 5               7.5                35.0                   30.0
    ## 6               2.1                26.0                   20.0
    ## 7               1.2                45.0                   30.0
    ## 8               2.0                50.0                   25.0
    ## 9               5.2                25.0                   20.0
    ## 10             18.6                 0.0                    0.0
    ## 11              3.1                 0.0                    0.0
    ## 12             10.7                25.0                   45.0
    ## 13             14.2                35.0                   25.0
    ## 14              1.8                13.0                   18.0
    ## 15              2.0                50.0                   29.0
    ## 16             10.5                25.0                   25.0
    ## 17             11.6                45.0                   30.0
    ## 18              2.8                25.0                   30.0
    ## 19              4.8                13.0                   25.0
    ## 20              1.2                10.0                   10.0
    ## 21              0.6                25.0                   22.0
    ## 22              8.0                27.5                   34.0
    ## 23              0.5                 0.0                   18.5
    ## 24              2.0                10.0                   10.0
    ## 25              7.4                27.5                   28.0
    ## 26              4.6                20.0                   30.0
    ## 27              5.9                35.0                   35.0
    ## 28              9.8                20.0                   20.0
    ## 29             15.8                35.0                   33.0
    ## 30              1.6                33.0                   15.0
    ## 31             10.9                35.0                   24.0
    ## 32             14.5                50.0                   30.0
    ## 33             16.4                60.0                   45.0
    ## 34              0.6                35.0                   25.0
    ## 35              3.5                45.0                   25.0
    ## 36              7.0                33.0                   33.0
    ## 37              5.0                30.0                   50.0
    ## 38             11.2                30.0                   40.0
    ## 39             11.6                45.0                   34.0
    ## 40              1.8                25.0                   30.0
    ## 41              8.2                36.0                   25.0
    ## 42              2.0                40.0                   18.0
    ## 43              8.0                50.0                   30.0
    ## 44              2.0                35.0                   12.5
    ## 45              2.0                15.0                   19.0
    ## 46              2.0                56.0                   23.5
    ## 47             17.3                30.0                   25.0
    ## 48              8.4                35.0                   30.0
    ## 49              4.6                25.0                   27.0
    ## 50              6.8                35.0                   25.0
    ## 51              6.6                25.0                   23.0
    ## 52              1.8                30.0                   30.0
    ## 53             15.6                35.0                   35.0
    ## 54              5.4                30.0                   30.0
    ## 55              2.0                20.0                   20.0
    ## 56              1.2                33.0                   28.0
    ## 57             12.1                35.0                   30.0
    ## 58             11.1                29.0                   20.0
    ## 59              2.0                31.3                   20.0
    ## 60              2.0                45.0                   33.0
    ## 61             16.9                35.0                   30.0
    ## 62             14.2                35.0                   32.0
    ## 63              0.7                20.0                   15.0
    ## 64              2.0                47.5                   15.8
    ## 65             10.8                35.0                   25.0
    ## 66              2.0                42.0                   29.0
    ## 67              1.4                31.0                   31.0
    ## 68             10.9                40.0                   35.0
    ## 69             12.2                20.0                   25.0
    ## 70              6.6                33.3                   40.0
    ## 71              6.5                30.0                   30.0
    ## 72              2.8                25.0                   25.0
    ## 73              0.0                15.0                   16.5
    ## 74              2.0                15.0                   19.0
    ## 75              1.5                31.8                   20.0
    ## 76              6.3                30.9                   32.4
    ## 77              2.6                30.0                   25.0
    ## 78             15.2                35.0                   25.0
    ## 79              N/A                15.0                   15.0
    ## 80              2.0                41.0                   12.5
    ## 81              2.8                48.0                   23.0
    ## 82              2.0                43.0                   27.5
    ## 83             10.8                25.0                   25.0
    ## 84              2.5                40.8                   23.9
    ## 85              4.3                14.0                   20.0
    ## 86              2.5                10.0                   20.0
    ## 87             12.3                30.0                   30.0
    ## 88             15.9                35.0                   35.0
    ## 89             50.0                 N/A                    N/A
    ## 90              4.8                46.2                   27.5
    ## 91              7.1                10.0                   10.0
    ## 92              3.0                 0.0                   15.0
    ## 93              3.2                10.0                   10.0
    ## 94              1.6                24.0                   24.0
    ## 95              2.0                31.4                   20.0
    ## 96              3.0                20.0                   15.0
    ## 97              2.0                35.0                   25.0
    ## 98             12.4                25.0                   25.0
    ## 99              N/A                10.0                   20.0
    ## 100             N/A                 7.0                   12.5
    ## 101             2.0                15.0                   15.0
    ## 102             2.0                42.0                   18.0
    ## 103             0.0                12.0                   39.0
    ## 104             4.0                10.0                   10.0
    ## 105             7.9                20.0                   20.0
    ## 106             4.8                30.0                   30.0
    ## 107             4.0                25.0                   25.0
    ## 108            11.2                 0.0                    0.0
    ## 109             7.6                40.0                   35.0
    ## 110             2.0                35.0                   35.0
    ## 111             8.7                30.0                   25.0
    ## 112             0.8                15.0                   15.0
    ## 113             4.3                35.0                   30.0
    ## 114             2.2                10.0                   21.0
    ## 115             3.5                18.0                   12.0
    ## 116             4.6                10.0                   25.0
    ## 117             2.6                 9.0                    9.0
    ## 118             3.8                38.0                   30.0
    ## 119             3.5                32.0                   32.0
    ## 120             1.0                37.0                   34.0
    ## 121            12.3                25.0                   25.0
    ## 122             2.0                52.0                   25.0
    ## 123             1.3                33.0                   28.0
    ## 124             2.0                30.0                   30.0
    ## 125             9.6                35.0                   30.0
    ## 126            11.3                24.0                   30.0
    ## 127             3.4                47.8                   23.0
    ## 128             1.5                 0.0                   12.0
    ## 129            10.1                30.0                   30.0
    ## 130             5.4                25.0                   25.0
    ## 131             2.0                42.0                   30.0
    ## 132             4.2                10.0                   10.0
    ## 133             1.8                30.0                   28.0
    ## 134             3.4                35.0                   30.0
    ## 135             2.0                32.0                   19.0
    ## 136             2.0                48.0                   23.0
    ## 137             3.4                 0.0                    0.0
    ## 138             2.0                10.0                   16.0
    ## 139             3.6                13.0                   20.0
    ## 140             7.3                30.0                   30.0
    ## 141             5.9                30.0                   30.0
    ## 142             9.2                32.5                   33.0
    ## 143            10.6                27.0                   27.0
    ## 144            10.4                20.0                   25.0
    ## 145             4.5                 2.5                    2.5
    ## 146             9.0                40.0                   30.0
    ## 147             6.5                10.0                   15.0
    ## 148             4.3                15.0                   33.0
    ## 149            10.3                15.0                   30.0
    ## 150             0.1                22.0                   17.0
    ## 151             2.0                25.0                   21.0
    ## 152             2.0                50.0                   17.0
    ## 153            14.1                40.0                   30.0
    ## 154             N/A                 N/A                    N/A
    ## 155             4.5                45.0                   28.0
    ## 156             2.0                45.0                   25.0
    ## 157             4.4                24.0                   28.0
    ## 158            17.5                10.0                   35.0
    ## 159            10.2                38.0                   36.0
    ## 160             2.0                57.0                   22.0
    ## 161             1.3                40.0                   24.0
    ## 162            16.5                22.0                   28.0
    ## 163             1.5                45.0                   20.0
    ## 164             5.7                13.0                   15.0
    ## 165             8.6                30.0                   30.0
    ## 166             3.5                35.0                   20.0
    ## 167             2.5                10.0                   10.0
    ## 168            10.3                45.0                   27.0
    ## 169             5.7                20.0                   25.0
    ## 170             8.3                25.0                   25.0
    ## 171             9.3                35.0                   30.0
    ## 172             2.7                35.0                   22.0
    ## 173             2.0                10.0                    8.0
    ## 174             7.3                40.0                   30.0
    ## 175             2.5                20.0                   18.0
    ## 176             2.8                 0.0                    0.0
    ## 177             2.0                45.0                   20.0
    ## 178             1.7                37.0                   21.0
    ## 179             5.7                30.0                   25.0
    ## 180             8.7                22.0                    7.5
    ## 181             7.8                 0.0                    0.0
    ## 182            10.0                34.0                   34.0
    ## 183             2.9                35.0                   22.0
    ## 184             4.3                 N/A                    N/A
    ## 185             6.2                35.0                   35.0
    ## 186             5.0                51.5                   25.0
    ##     Tax.Burden...of.GDP Gov.t.Expenditure...of.GDP
    ## 1                   5.0                       25.6
    ## 2                  24.9                       29.5
    ## 3                  24.5                       41.4
    ## 4                  20.6                       25.3
    ## 5                  30.8                       41.0
    ## 6                  21.3                       26.4
    ## 7                  28.2                       36.5
    ## 8                  42.7                       50.2
    ## 9                  15.0                       36.7
    ## 10                 16.3                       21.0
    ## 11                  5.6                       35.3
    ## 12                  8.8                       13.6
    ## 13                 33.7                       34.1
    ## 14                 23.8                       44.2
    ## 15                 44.2                       53.2
    ## 16                 27.6                       33.7
    ## 17                 11.9                       23.5
    ## 18                 13.4                       30.7
    ## 19                 31.1                       41.1
    ## 20                 37.0                       42.4
    ## 21                 24.9                       33.7
    ## 22                 32.2                       38.6
    ## 23                 24.2                       36.6
    ## 24                 28.0                       34.7
    ## 25                 16.3                       25.8
    ## 26                  6.5                       22.1
    ## 27                 12.3                       23.6
    ## 28                 15.0                       21.7
    ## 29                 15.6                       20.4
    ## 30                 31.7                       40.3
    ## 31                 23.7                       31.0
    ## 32                  9.0                       13.9
    ## 33                  5.3                       15.9
    ## 34                 20.4                       25.2
    ## 35                 17.5                       31.6
    ## 36                 19.9                       28.9
    ## 37                 14.5                       29.8
    ## 38                 10.8                       14.2
    ## 39                 29.5                       44.5
    ## 40                 23.6                       19.6
    ## 41                 18.2                       23.1
    ## 42                 37.9                       47.1
    ## 43                 41.5                       64.2
    ## 44                 33.6                       38.7
    ## 45                 34.0                       40.0
    ## 46                 45.9                       53.4
    ## 47                 29.2                       49.2
    ## 48                 25.8                       39.4
    ## 49                 13.6                       17.9
    ## 50                 21.2                       38.5
    ## 51                 18.0                       32.6
    ## 52                 19.7                       21.4
    ## 53                 20.4                       32.8
    ## 54                  8.0                       29.5
    ## 55                 34.7                       40.4
    ## 56                 25.5                       33.9
    ## 57                 12.4                       17.9
    ## 58                 25.5                       30.7
    ## 59                 44.1                       55.6
    ## 60                 45.3                       56.6
    ## 61                 17.3                       21.1
    ## 62                 17.9                       31.3
    ## 63                 25.8                       29.6
    ## 64                 37.6                       43.9
    ## 65                 16.5                       24.5
    ## 66                 38.6                       50.6
    ## 67                 12.6                       12.1
    ## 68                 15.3                       18.4
    ## 69                  9.6                       21.0
    ## 70                 24.4                       32.0
    ## 71                 14.5                       19.7
    ## 72                 21.6                       27.0
    ## 73                 14.0                       17.9
    ## 74                 39.4                       47.7
    ## 75                 36.4                       43.2
    ## 76                  7.2                       27.5
    ## 77                 10.4                       16.9
    ## 78                  8.0                       18.5
    ## 79                  N/A                       39.7
    ## 80                 23.0                       27.4
    ## 81                 31.2                       39.8
    ## 82                 42.9                       49.5
    ## 83                 27.1                       28.3
    ## 84                 30.7                       38.7
    ## 85                 16.3                       29.8
    ## 86                 12.8                       23.3
    ## 87                 15.7                       27.2
    ## 88                 15.7                      117.9
    ## 89                  N/A                      100.0
    ## 90                 26.3                       32.4
    ## 91                 23.5                       27.2
    ## 92                  1.6                       52.5
    ## 93                 19.7                       39.1
    ## 94                 12.8                       22.1
    ## 95                 30.2                       37.8
    ## 96                 13.8                       28.5
    ## 97                 47.0                       47.3
    ## 98                 21.8                       35.5
    ## 99                  N/A                      139.2
    ## 100                 N/A                        N/A
    ## 101                30.2                       34.1
    ## 102                37.1                       42.2
    ## 103                25.0                       17.9
    ## 104                24.8                       31.6
    ## 105                10.1                       16.6
    ## 106                14.8                       31.5
    ## 107                13.8                       23.7
    ## 108                20.5                       36.1
    ## 109                17.6                       22.1
    ## 110                33.6                       38.3
    ## 111                25.9                       29.3
    ## 112                18.4                       25.6
    ## 113                17.2                       26.9
    ## 114                13.2                       59.2
    ## 115                31.5                       36.9
    ## 116                20.7                       35.1
    ## 117                36.1                       47.4
    ## 118                20.9                       30.2
    ## 119                20.2                       33.2
    ## 120                28.6                       41.3
    ## 121                18.7                       23.3
    ## 122                38.8                       43.6
    ## 123                32.1                       40.7
    ## 124                22.6                       26.4
    ## 125                13.5                       28.5
    ## 126                 5.2                       10.8
    ## 127                38.0                       49.9
    ## 128                 8.5                       47.4
    ## 129                12.4                       20.3
    ## 130                15.8                       22.2
    ## 131                12.5                       19.1
    ## 132                13.0                       26.5
    ## 133                16.0                       21.5
    ## 134                13.7                       19.4
    ## 135                33.6                       41.3
    ## 136                34.4                       46.3
    ## 137                 5.7                       37.9
    ## 138                26.0                       32.1
    ## 139                22.2                       35.4
    ## 140                15.0                       26.2
    ## 141                24.0                       26.3
    ## 142                27.1                       29.3
    ## 143                23.6                       35.5
    ## 144                15.9                       33.2
    ## 145                 3.4                       37.6
    ## 146                20.4                       29.8
    ## 147                38.4                       42.8
    ## 148                32.6                       36.4
    ## 149                12.2                       22.8
    ## 150                13.7                       17.6
    ## 151                32.7                       42.4
    ## 152                37.0                       45.4
    ## 153                30.8                       46.0
    ## 154                 N/A                        N/A
    ## 155                31.3                       32.8
    ## 156                33.5                       42.3
    ## 157                12.3                       19.8
    ## 158                 6.7                       10.6
    ## 159                13.2                       27.6
    ## 160                44.1                       49.4
    ## 161                27.8                       34.3
    ## 162                 N/A                        N/A
    ## 163                 8.9                       17.7
    ## 164                20.6                       34.3
    ## 165                12.4                       18.0
    ## 166                15.6                       21.8
    ## 167                13.1                       57.5
    ## 168                21.5                       27.7
    ## 169                20.7                       44.4
    ## 170                22.8                       35.7
    ## 171                20.8                       29.2
    ## 172                25.5                       34.1
    ## 173                15.6                       16.4
    ## 174                12.9                       19.4
    ## 175                33.1                       42.1
    ## 176                 8.9                       32.3
    ## 177                33.2                       41.6
    ## 178                26.0                       37.8
    ## 179                27.4                       32.9
    ## 180                18.2                       33.0
    ## 181                16.3                       39.1
    ## 182                14.9                       37.4
    ## 183                18.0                       29.4
    ## 184                 N/A                        N/A
    ## 185                17.9                       25.8
    ## 186                22.3                       29.2
    ##                                     Country Population..Millions.
    ## 1                               Afghanistan                  35.5
    ## 2                                   Albania                   2.9
    ## 3                                   Algeria                  41.5
    ## 4                                    Angola                  28.2
    ## 5                                 Argentina                  44.1
    ## 6                                   Armenia                   3.0
    ## 7                                 Australia                  24.8
    ## 8                                   Austria                   8.8
    ## 9                                Azerbaijan                   9.8
    ## 10                                  Bahamas                   0.4
    ## 11                                  Bahrain                   1.5
    ## 12                               Bangladesh                 163.2
    ## 13                                 Barbados                   0.3
    ## 14                                  Belarus                   9.5
    ## 15                                  Belgium                  11.4
    ## 16                                   Belize                   0.4
    ## 17                                    Benin                  11.1
    ## 18                                   Bhutan                   0.8
    ## 19                                  Bolivia                  11.1
    ## 20                   Bosnia and Herzegovina                   3.5
    ## 21                                 Botswana                   2.2
    ## 22                                   Brazil                 207.7
    ## 23                        Brunei Darussalam                   0.4
    ## 24                                 Bulgaria                   7.1
    ## 25                             Burkina Faso                  18.9
    ## 26                                    Burma                  52.6
    ## 27                                  Burundi                  10.9
    ## 28                                 Cambodia                  16.0
    ## 29                                 Cameroon                  24.3
    ## 30                                   Canada                  36.7
    ## 31                               Cabo Verde                   0.5
    ## 32                 Central African Republic                   5.0
    ## 33                                     Chad                  12.2
    ## 34                                    Chile                  18.4
    ## 35                                    China                1390.1
    ## 36                                 Colombia                  49.3
    ## 37                                  Comoros                   0.8
    ## 38  Congo, Democratic Republic of the Congo                  86.7
    ## 39                       Congo, Republic of                   4.3
    ## 40                               Costa Rica                   5.0
    ## 41                            Côte d'Ivoire                  25.0
    ## 42                                  Croatia                   4.1
    ## 43                                     Cuba                  11.5
    ## 44                                   Cyprus                   0.9
    ## 45                           Czech Republic                  10.6
    ## 46                                  Denmark                   5.7
    ## 47                                 Djibouti                   1.0
    ## 48                                 Dominica                   0.1
    ## 49                       Dominican Republic                  10.2
    ## 50                                  Ecuador                  16.8
    ## 51                                    Egypt                  94.8
    ## 52                              El Salvador                   6.4
    ## 53                        Equatorial Guinea                   0.8
    ## 54                                  Eritrea                   5.9
    ## 55                                  Estonia                   1.3
    ## 56                                 Eswatini                   1.1
    ## 57                                 Ethiopia                  92.7
    ## 58                                     Fiji                   0.9
    ## 59                                  Finland                   5.5
    ## 60                                   France                  64.8
    ## 61                                    Gabon                   1.9
    ## 62                                   Gambia                   2.1
    ## 63                                  Georgia                   3.7
    ## 64                                  Germany                  82.7
    ## 65                                    Ghana                  28.3
    ## 66                                   Greece                  10.8
    ## 67                                Guatemala                  16.9
    ## 68                                   Guinea                  13.0
    ## 69                            Guinea-Bissau                   1.7
    ## 70                                   Guyana                   0.8
    ## 71                                    Haiti                  11.0
    ## 72                                 Honduras                   8.3
    ## 73                            Hong Kong SAR                   7.4
    ## 74                                  Hungary                   9.8
    ## 75                                  Iceland                   0.3
    ## 76                                    India                1316.9
    ## 77                                Indonesia                 262.0
    ## 78                                     Iran                  81.4
    ## 79                                     Iraq                  38.9
    ## 80                                  Ireland                   4.7
    ## 81                                   Israel                   8.7
    ## 82                                    Italy                  60.6
    ## 83                                  Jamaica                   2.8
    ## 84                                    Japan                 126.7
    ## 85                                   Jordan                   7.1
    ## 86                               Kazakhstan                  18.2
    ## 87                                    Kenya                  46.7
    ## 88                                 Kiribati                   0.1
    ## 89                            Korea, North                   25.4
    ## 90                             Korea, South                  51.5
    ## 91                                   Kosovo                   1.9
    ## 92                                   Kuwait                   4.4
    ## 93                          Kyrgyz Republic                   6.3
    ## 94                               Lao P.D.R.                   6.7
    ## 95                                   Latvia                   2.0
    ## 96                                  Lebanon                   4.5
    ## 97                                  Lesotho                   1.9
    ## 98                                  Liberia                   4.5
    ## 99                                    Libya                   6.4
    ## 100                           Liechtenstein           38,000 ppl.
    ## 101                               Lithuania                   2.8
    ## 102                              Luxembourg                   0.6
    ## 103                                   Macau                   0.6
    ## 104                               Macedonia                   2.1
    ## 105                              Madagascar                  25.6
    ## 106                                  Malawi                  19.2
    ## 107                                Malaysia                  32.1
    ## 108                                Maldives                   0.4
    ## 109                                    Mali                  18.9
    ## 110                                   Malta                   0.5
    ## 111                              Mauritania                   3.9
    ## 112                               Mauritius                   1.3
    ## 113                                  Mexico                 123.5
    ## 114                              Micronesia                   0.1
    ## 115                                 Moldova                   3.5
    ## 116                                Mongolia                   3.1
    ## 117                              Montenegro                   0.6
    ## 118                                 Morocco                  34.9
    ## 119                              Mozambique                  29.5
    ## 120                                 Namibia                   2.3
    ## 121                                   Nepal                  29.3
    ## 122                             Netherlands                  17.1
    ## 123                             New Zealand                   4.8
    ## 124                               Nicaragua                   6.2
    ## 125                                   Niger                  18.8
    ## 126                                 Nigeria                 188.7
    ## 127                                  Norway                   5.3
    ## 128                                    Oman                   4.1
    ## 129                                Pakistan                 197.3
    ## 130                                  Panama                   4.1
    ## 131                        Papua New Guinea                   8.3
    ## 132                                Paraguay                   7.0
    ## 133                                    Peru                  31.8
    ## 134                             Philippines                 105.3
    ## 135                                  Poland                  38.0
    ## 136                                Portugal                  10.3
    ## 137                                   Qatar                   2.7
    ## 138                                 Romania                  19.6
    ## 139                                  Russia                 144.0
    ## 140                                  Rwanda                  11.8
    ## 141                             Saint Lucia                   0.2
    ## 142        Saint Vincent and the Grenadines                   0.1
    ## 143                                   Samoa                   0.2
    ## 144                   São Tomé and Príncipe                   0.2
    ## 145                            Saudi Arabia                  32.4
    ## 146                                 Senegal                  15.9
    ## 147                                  Serbia                   7.0
    ## 148                              Seychelles                   0.1
    ## 149                            Sierra Leone                   7.4
    ## 150                               Singapore                   5.6
    ## 151                         Slovak Republic                   5.4
    ## 152                                Slovenia                   2.1
    ## 153                         Solomon Islands                   0.6
    ## 154                                 Somalia                  14.3
    ## 155                            South Africa                  56.5
    ## 156                                   Spain                  46.3
    ## 157                               Sri Lanka                  21.4
    ## 158                                   Sudan                  40.8
    ## 159                                Suriname                   0.6
    ## 160                                  Sweden                  10.1
    ## 161                             Switzerland                   8.4
    ## 162                                   Syria                  18.4
    ## 163                                 Taiwan                   23.6
    ## 164                              Tajikistan                   8.8
    ## 165                                Tanzania                  50.0
    ## 166                                Thailand                  69.1
    ## 167                             Timor-Leste                   1.2
    ## 168                                    Togo                   7.8
    ## 169                                   Tonga                   0.1
    ## 170                     Trinidad and Tobago                   1.4
    ## 171                                 Tunisia                  11.5
    ## 172                                  Turkey                  80.8
    ## 173                            Turkmenistan                   5.7
    ## 174                                  Uganda                  37.7
    ## 175                                 Ukraine                  42.3
    ## 176                    United Arab Emirates                  10.1
    ## 177                          United Kingdom                  66.1
    ## 178                           United States                 325.9
    ## 179                                 Uruguay                   3.5
    ## 180                              Uzbekistan                  32.1
    ## 181                                 Vanuatu                   0.3
    ## 182                               Venezuela                  31.4
    ## 183                                 Vietnam                  93.6
    ## 184                                   Yemen                  30.0
    ## 185                                  Zambia                  17.2
    ## 186                                Zimbabwe                  14.9
    ##     GDP..Billions..PPP. GDP.Growth.Rate.... X5.Year.GDP.Growth.Rate....
    ## 1                $69.6                  2.5                         2.9
    ## 2                $36.0                  3.9                         2.5
    ## 3               $632.9                  2.0                         3.1
    ## 4               $190.3                  0.7                         2.9
    ## 5               $920.2                  2.9                         0.7
    ## 6                $28.3                  7.5                         3.6
    ## 7             $1,246.5                  2.3                         2.4
    ## 8               $439.6                  2.9                         1.3
    ## 9               $171.8                  0.1                         1.2
    ## 10               $11.6                  1.3                        -0.7
    ## 11               $70.4                  3.2                         3.8
    ## 12              $687.1                  7.1                         6.7
    ## 13                $5.2                  0.9                         0.6
    ## 14              $178.9                  2.4                        -0.3
    ## 15              $528.5                  1.7                         1.2
    ## 16                $3.2                  0.8                         1.8
    ## 17               $25.3                  5.6                         5.0
    ## 18                $7.0                  6.0                         5.2
    ## 19               $83.6                  4.2                         5.1
    ## 20               $44.6                  2.7                         2.5
    ## 21               $38.9                  2.2                         4.1
    ## 22            $3,240.3                  1.0                        -0.5
    ## 23               $33.5                  0.5                        -1.4
    ## 24              $153.1                  3.6                         2.7
    ## 25               $35.8                  6.4                         5.3
    ## 26              $328.7                  6.7                         7.2
    ## 27                $8.0                  0.0                         1.1
    ## 28               $64.3                  6.9                         7.1
    ## 29               $88.9                  3.2                         4.9
    ## 30            $1,769.3                  3.0                         2.1
    ## 31                $3.7                  4.0                         2.0
    ## 32                $3.4                  4.0                        -4.5
    ## 33               $28.6                 -3.1                         1.0
    ## 34              $451.1                  1.5                         2.2
    ## 35           $23,159.1                  6.9                         7.1
    ## 36              $714.0                  1.8                         3.2
    ## 37                $1.3                  2.5                         2.2
    ## 38               $68.5                  3.4                         6.1
    ## 39               $28.9                 -4.6                         1.1
    ## 40               $83.9                  3.2                         3.4
    ## 41               $96.9                  7.8                         8.6
    ## 42              $101.3                  2.8                         1.5
    ## 43              $148.0                  0.9                         1.9
    ## 44               $31.6                  3.9                         0.3
    ## 45              $375.7                  4.3                         2.9
    ## 46              $286.8                  2.1                         1.6
    ## 47                $3.6                  6.7                         6.1
    ## 48                $0.8                 -4.2                         0.0
    ## 49              $172.4                  4.6                         6.1
    ## 50              $192.6                  2.7                         2.0
    ## 51            $1,201.2                  4.2                         3.8
    ## 52               $57.0                  2.4                         2.1
    ## 53               $30.4                 -4.4                        -5.6
    ## 54                $9.4                  5.0                         3.4
    ## 55               $41.6                  4.9                         2.7
    ## 56               $11.3                  0.2                         2.0
    ## 57              $200.2                 10.9                         9.9
    ## 58                $8.7                  3.8                         3.7
    ## 59              $244.0                  3.0                         0.8
    ## 60            $2,835.8                  1.8                         1.1
    ## 61               $36.7                  0.8                         3.4
    ## 62                $3.6                  3.5                         3.1
    ## 63               $39.7                  4.8                         3.7
    ## 64            $4,170.8                  2.5                         1.7
    ## 65              $133.7                  8.4                         5.5
    ## 66              $298.7                  1.4                        -0.3
    ## 67              $137.8                  2.8                         3.6
    ## 68               $26.5                  6.7                         4.9
    ## 69                $3.1                  5.5                         4.3
    ## 70                $6.3                  2.1                         3.5
    ## 71               $19.9                  1.2                         2.2
    ## 72               $46.2                  4.8                         3.6
    ## 73              $454.9                  3.8                         2.8
    ## 74              $289.0                  4.0                         3.2
    ## 75               $17.6                  3.6                         4.4
    ## 76            $9,459.0                  6.7                         7.2
    ## 77            $3,242.8                  5.1                         5.1
    ## 78            $1,644.7                  4.3                         3.6
    ## 79              $658.8                 -0.8                         4.7
    ## 80              $357.2                  7.8                         9.7
    ## 81              $316.5                  3.3                         3.5
    ## 82            $2,310.9                  1.5                         0.3
    ## 83               $26.1                  1.0                         0.8
    ## 84            $5,428.8                  1.7                         1.3
    ## 85               $89.1                  2.3                         2.5
    ## 86              $477.6                  4.0                         3.3
    ## 87              $163.1                  4.8                         5.5
    ## 88                $0.2                  3.1                         3.6
    ## 89    $40.0 (2015 est.)                 1.1                         N/A
    ## 90            $2,029.0                  3.1                         3.0
    ## 91               $19.6                  4.1                         3.4
    ## 92              $291.5                 -2.5                        -0.1
    ## 93               $23.0                  4.5                         5.4
    ## 94               $49.2                  6.8                         7.4
    ## 95               $53.9                  4.5                         2.8
    ## 96               $87.7                  1.2                         1.5
    ## 97                $7.0                  3.1                         2.8
    ## 98                $6.1                  2.5                         2.1
    ## 99               $64.4                 70.8                        -7.9
    ## 100    $6.1 CHF (2014 )                 N/A                         N/A
    ## 101              $91.2                  3.8                         3.0
    ## 102              $62.7                  3.5                         3.8
    ## 103              $71.8                  9.3                        -0.6
    ## 104              $31.0                  0.0                         2.7
    ## 105              $39.7                  4.1                         3.4
    ## 106              $22.4                  4.0                         4.0
    ## 107             $930.8                  5.9                         5.2
    ## 108               $6.9                  4.8                         5.2
    ## 109              $41.0                  5.3                         5.3
    ## 110              $19.3                  6.6                         6.9
    ## 111              $17.3                  3.2                         3.5
    ## 112              $27.5                  3.9                         3.6
    ## 113           $2,458.4                  2.0                         2.5
    ## 114               $0.3                  2.0                         0.7
    ## 115              $20.1                  4.0                         4.4
    ## 116              $39.7                  5.1                         5.7
    ## 117              $11.0                  4.2                         3.2
    ## 118             $298.6                  4.2                         3.4
    ## 119              $36.7                  3.0                         5.6
    ## 120              $26.5                 -1.2                         3.6
    ## 121              $78.6                  7.5                         4.3
    ## 122             $916.1                  3.1                         1.8
    ## 123             $188.6                  3.0                         3.3
    ## 124              $36.4                  4.9                         4.8
    ## 125              $21.8                  5.2                         5.4
    ## 126           $1,118.8                  0.8                         2.7
    ## 127             $380.0                  1.8                         1.6
    ## 128             $186.6                 -0.3                         2.7
    ## 129           $1,057.0                  5.3                         4.3
    ## 130             $103.9                  5.4                         5.8
    ## 131              $30.3                  2.5                         5.8
    ## 132              $68.3                  4.3                         6.0
    ## 133             $424.4                  2.5                         3.6
    ## 134             $875.6                  6.7                         6.6
    ## 135           $1,121.0                  4.6                         3.2
    ## 136             $313.4                  2.7                         1.2
    ## 137             $340.6                  2.1                         3.3
    ## 138             $481.5                  7.0                         4.5
    ## 139           $4,007.8                  1.5                         0.3
    ## 140              $24.6                  6.1                         6.7
    ## 141               $2.5                  3.0                         1.9
    ## 142               $1.3                  1.0                         1.1
    ## 143               $1.1                  2.4                         2.1
    ## 144               $0.7                  4.0                         4.1
    ## 145           $1,773.6                 -0.7                         2.3
    ## 146              $43.2                  7.2                         5.6
    ## 147             $105.5                  1.8                         1.2
    ## 148               $2.7                  4.2                         4.9
    ## 149              $11.5                  3.5                         2.9
    ## 150             $527.0                  3.6                         3.5
    ## 151             $179.4                  3.4                         3.0
    ## 152              $71.1                  5.0                         2.5
    ## 153               $1.3                  3.2                         2.9
    ## 154             $18.70                  1.8                         2.2
    ## 155             $765.6                  1.3                         1.5
    ## 156           $1,773.9                  3.1                         1.9
    ## 157             $274.7                  3.1                         4.2
    ## 158             $187.0                  3.2                         3.0
    ## 159               $8.5                  0.0                        -0.9
    ## 160             $520.9                  2.4                         2.8
    ## 161             $517.2                  1.1                         1.6
    ## 162                 N/A                 N/A                         N/A
    ## 163           $1,185.5                  2.8                         2.2
    ## 164              $28.4                  7.1                         6.8
    ## 165             $162.2                  6.0                         6.8
    ## 166           $1,233.7                  3.9                         2.8
    ## 167               $6.8                 -0.5                         3.1
    ## 168              $12.9                  4.4                         5.5
    ## 169               $0.6                  3.1                         2.4
    ## 170              $43.0                 -2.6                        -1.3
    ## 171             $135.4                  1.9                         1.7
    ## 172           $2,173.2                  7.0                         6.0
    ## 173             $103.5                  6.5                         7.9
    ## 174              $88.7                  4.5                         4.3
    ## 175             $368.8                  2.5                        -2.3
    ## 176             $686.8                  0.5                         3.3
    ## 177           $2,914.0                  1.8                         2.2
    ## 178          $19,390.6                  2.3                         2.2
    ## 179              $78.1                  3.1                         2.6
    ## 180             $222.6                  5.3                         7.4
    ## 181               $0.8                  4.2                         2.4
    ## 182             $380.7                -14.0                        -7.8
    ## 183             $647.4                  6.8                         6.2
    ## 184              $38.6                -13.8                       -16.1
    ## 185              $68.9                  3.6                         4.0
    ## 186              $34.0                  3.0                         2.6
    ##     GDP.per.Capita..PPP. Unemployment.... Inflation.... FDI.Inflow..Millions.
    ## 1                 $1,958              8.8           5.0                  53.9
    ## 2                $12,507             13.9           2.0               1,119.1
    ## 3                $15,237             10.0           5.6               1,203.0
    ## 4                 $6,753              8.2          31.7              -2,254.5
    ## 5                $20,876              8.7          25.7              11,857.0
    ## 6                 $9,456             18.2           0.9                 245.7
    ## 7                $50,334              5.6           2.0              46,368.0
    ## 8                $49,869              5.5           2.2               9,629.6
    ## 9                $17,492              5.0          13.0               2,867.0
    ## 10               $31,139             12.6           1.4                 927.7
    ## 11               $48,505              1.2           1.4                 518.9
    ## 12                $4,211              4.4           5.7               2,151.6
    ## 13               $18,664              9.7           4.4                 286.2
    ## 14               $18,931              0.5           6.0               1,276.4
    ## 15               $46,553              7.4           2.2                 740.4
    ## 16                $8,324              9.3           1.1                  77.0
    ## 17                $2,277              2.5           0.1                 184.4
    ## 18                $8,744              2.4           3.4                  10.3
    ## 19                $7,547              3.1           2.8                 724.7
    ## 20               $12,724             25.6           1.3                 425.2
    ## 21               $17,828             17.4           3.3                 400.6
    ## 22               $15,603             13.3           3.4              62,712.6
    ## 23               $78,196              7.1          -0.1                 -46.3
    ## 24               $21,687              6.2           1.2               1,070.7
    ## 25                $1,889              6.3           0.4                 485.9
    ## 26                $6,244              0.8           5.1               4,341.0
    ## 27                  $735              1.6          16.6                   0.3
    ## 28                $4,012              0.2           2.9               2,784.4
    ## 29                $3,660              4.2           0.6                 672.5
    ## 30               $48,265              6.3           1.6              24,243.8
    ## 31                $6,944             10.3           0.8                 108.6
    ## 32                  $677              6.0           3.8                  17.2
    ## 33                $2,344              5.9          -0.9                 335.0
    ## 34               $24,537              7.0           2.2               6,729.6
    ## 35               $16,660              4.7           1.6             136,320.0
    ## 36               $14,485              8.9           4.3              14,518.0
    ## 37                $1,588              4.3           1.0                   8.6
    ## 38                  $790              3.7          41.5               1,340.2
    ## 39                $6,642             11.0           0.5               1,158.8
    ## 40               $16,877              8.1           1.6               3,007.1
    ## 41                $3,883              2.6           0.8                 674.7
    ## 42               $24,424             11.2           1.1               2,104.2
    ## 43               $12,920              2.6           5.5                   N/A
    ## 44               $37,023             11.0           0.7               6,343.3
    ## 45               $35,512              2.9           2.4               7,412.2
    ## 46               $49,883              5.7           1.1              -3,114.7
    ## 47                $3,559              5.8           0.7                 165.0
    ## 48               $11,102              N/A           0.6                  18.9
    ## 49               $16,944              5.5           3.3               3,570.0
    ## 50               $11,482              3.8           0.4                 606.4
    ## 51               $12,671             12.1          23.5               7,391.7
    ## 52                $8,948              4.5           1.0                 791.9
    ## 53               $36,017              6.9           0.7                 304.1
    ## 54                $1,581              6.4           9.0                  55.5
    ## 55               $31,750              5.8           3.7                 784.4
    ## 56                $9,884             26.4           6.3                -136.8
    ## 57                $2,161              5.2           9.9               3,586.4
    ## 58                $9,777              6.3           3.4                 299.0
    ## 59               $44,333              8.6           0.8               1,327.9
    ## 60               $43,761              9.4           1.2              49,794.9
    ## 61               $19,254             19.7           3.0               1,498.0
    ## 62                $1,713              9.5           8.0                  87.5
    ## 63               $10,747             11.6           6.0               1,861.9
    ## 64               $50,425              3.8           1.7              34,726.3
    ## 65                $4,729              2.4          12.4               3,255.0
    ## 66               $27,737             21.5           1.1               4,046.0
    ## 67                $8,145              2.7           4.4               1,146.7
    ## 68                $2,041              4.5           8.9                 576.5
    ## 69                $1,845              6.1           1.1                  16.6
    ## 70                $8,161             12.0           2.1                 212.2
    ## 71                $1,815             14.0          14.7                 374.9
    ## 72                $5,562              4.5           3.9               1,185.7
    ## 73               $61,393              3.1           1.5             104,333.0
    ## 74               $29,474              4.2           2.4               2,491.6
    ## 75               $51,842              2.8           1.8                  -5.4
    ## 76                $7,183              3.5           3.6              39,916.1
    ## 77               $12,377              4.2           3.8              23,063.1
    ## 78               $20,200             12.5           9.9               5,019.0
    ## 79               $16,954              8.2           0.1              -5,032.4
    ## 80               $75,538              6.4           0.3              28,974.6
    ## 81               $36,340              4.3           0.2              18,954.0
    ## 82               $38,140             11.2           1.3              17,077.1
    ## 83                $9,163             12.5           4.4                 888.0
    ## 84               $42,832              2.8           0.5              10,429.8
    ## 85               $12,494             14.9           3.3               1,664.8
    ## 86               $26,252              4.9           7.4               4,633.7
    ## 87                $3,491             11.5           8.0                 671.7
    ## 88                $1,976              N/A           2.2                   1.4
    ## 89    $1,700 (2015 est.)              4.8           N/A                  63.4
    ## 90               $39,434              3.7           1.9              17,052.8
    ## 91               $10,515              N/A           1.5                   3.6
    ## 92               $66,163              2.1           1.5                 300.5
    ## 93                $3,667              7.3           3.2                  93.8
    ## 94                $7,366              0.7           0.8                 813.0
    ## 95               $27,644              8.7           2.9                 721.2
    ## 96               $19,439              6.6           4.5               2,628.0
    ## 97                $3,581             27.3           5.6                 135.0
    ## 98                $1,354              2.4          12.4                 247.8
    ## 99                $9,986             17.7          28.0                   N/A
    ## 100 $139,100 (2009 est.)       2.1 (2016)           N/A                   N/A
    ## 101              $32,299              7.1           3.7                 595.4
    ## 102             $106,374              5.5           2.1               6,622.7
    ## 103             $111,629              2.0           1.2               1,996.8
    ## 104              $14,914             22.4           1.4                 256.3
    ## 105               $1,551              1.8           8.1                 389.1
    ## 106               $1,167              5.9          11.5                 277.1
    ## 107              $29,041              3.4           3.8               9,543.4
    ## 108              $19,151              5.0           2.8                 517.5
    ## 109               $2,170              7.9           1.8                 265.6
    ## 110              $41,945              4.0           1.3               3,185.2
    ## 111               $4,444              9.9           2.3                 329.6
    ## 112              $21,640              7.1           3.7                 292.7
    ## 113              $19,903              3.4           6.0              29,695.0
    ## 114               $3,393              N/A           0.5                   N/A
    ## 115               $5,661              4.5           6.6                 213.8
    ## 116              $12,979              7.0           4.6               1,494.4
    ## 117              $17,736             16.1           2.4                 545.9
    ## 118               $8,567              9.3           0.8               2,651.4
    ## 119               $1,244             25.0          15.3               2,293.1
    ## 120              $11,312             23.3           6.1                 416.0
    ## 121               $2,679              2.7           4.5                 198.0
    ## 122              $53,635              4.8           1.3              57,956.7
    ## 123              $38,934              4.9           1.9               3,572.0
    ## 124               $5,849              4.4           3.9                 896.6
    ## 125               $1,164              0.4           2.4                 334.3
    ## 126               $5,929              7.0          16.5               3,503.0
    ## 127              $71,831              4.2           1.9              -8,296.9
    ## 128              $45,157              3.3           1.6               1,867.4
    ## 129               $5,358              4.0           4.1               2,806.0
    ## 130              $25,351              4.5           0.9               5,319.2
    ## 131               $3,675              2.7           5.2                -200.5
    ## 132               $9,826              4.6           3.6                 355.8
    ## 133              $13,334              3.6           2.8               6,769.5
    ## 134               $8,315              2.4           3.2               9,524.3
    ## 135              $29,521              4.9           2.0               6,433.5
    ## 136              $30,417              8.9           1.6               6,945.6
    ## 137             $124,529              0.1           0.4                 986.0
    ## 138              $24,508              4.9           1.3               5,160.0
    ## 139              $27,834              5.2           3.7              25,284.0
    ## 140               $2,080              1.3           4.8                 366.2
    ## 141              $14,450             21.0           0.1                  92.4
    ## 142              $11,491             18.3           2.0                  87.1
    ## 143               $5,740              8.2           1.3                   9.0
    ## 144               $3,180             13.5           5.5                  41.0
    ## 145              $54,777              5.5          -0.9               1,421.0
    ## 146               $2,727              4.9           1.4                 532.3
    ## 147              $15,000             14.1           3.1               2,866.7
    ## 148              $28,779              N/A           2.9                 191.9
    ## 149               $1,553              4.5          18.0                 560.0
    ## 150              $93,906              2.0           0.6              62,006.0
    ## 151              $33,025              8.1           1.3               2,276.7
    ## 152              $34,407              6.6           1.4                 702.0
    ## 153               $2,157              2.1          -0.4                  36.5
    ## 154                  N/A              6.0           N/A                 384.0
    ## 155              $13,545             27.3           5.3               1,324.7
    ## 156              $38,286             17.2           2.0              19,086.1
    ## 157              $12,811              4.1           6.5               1,374.9
    ## 158               $4,586             12.7          32.4               1,065.3
    ## 159              $14,606              8.1          22.0                 -87.3
    ## 160              $51,475              6.7           1.9              15,395.7
    ## 161              $61,422              4.8           0.5              40,986.1
    ## 162                  N/A             14.9           N/A                   N/A
    ## 163              $50,294              3.8           0.6               3,255.0
    ## 164               $3,212             10.3           7.3                 141.3
    ## 165               $3,240              2.2           5.3               1,180.4
    ## 166              $17,856              1.1           0.7               7,635.2
    ## 167               $5,444              3.4           0.6                   6.7
    ## 168               $1,659              1.8          -0.7                 145.6
    ## 169               $5,608              1.1           8.0                  13.8
    ## 170              $31,367              4.8           1.9                 178.7
    ## 171              $11,755             15.4           5.3                 879.5
    ## 172              $26,893             11.3          11.1              10,864.0
    ## 173              $18,126              3.4           8.0               2,313.5
    ## 174               $2,354              2.1           5.6                 699.7
    ## 175               $8,713              9.5          14.4               2,202.0
    ## 176              $67,741              1.7           2.0              10,354.2
    ## 177              $44,118              4.3           2.7              15,090.0
    ## 178              $59,501              4.4           2.1             275,381.0
    ## 179              $22,371              7.9           6.2                -124.6
    ## 180               $6,929              7.2          12.5                  95.8
    ## 181               $2,739              5.2           3.1                  24.7
    ## 182              $12,114              7.7        1087.5                 -68.0
    ## 183               $6,913              2.1           3.5              14,100.0
    ## 184               $1,287             14.0           4.9                -269.9
    ## 185               $3,996              7.8           6.6               1,091.2
    ## 186               $2,283              5.0           1.3                 289.4
    ##     Public.Debt....of.GDP.
    ## 1                      7.3
    ## 2                     71.2
    ## 3                     25.8
    ## 4                     65.3
    ## 5                     52.6
    ## 6                     53.5
    ## 7                     41.6
    ## 8                     78.8
    ## 9                     54.7
    ## 10                    57.2
    ## 11                    90.3
    ## 12                    32.4
    ## 13                   132.9
    ## 14                    51.0
    ## 15                   103.2
    ## 16                    99.0
    ## 17                    54.6
    ## 18                   102.4
    ## 19                    50.9
    ## 20                    41.0
    ## 21                    15.6
    ## 22                    84.0
    ## 23                     2.7
    ## 24                    23.9
    ## 25                    38.3
    ## 26                    34.7
    ## 27                    56.7
    ## 28                    35.1
    ## 29                    33.8
    ## 30                    89.7
    ## 31                   126.0
    ## 32                    53.4
    ## 33                    52.5
    ## 34                    23.6
    ## 35                    47.8
    ## 36                    49.4
    ## 37                    28.4
    ## 38                    15.7
    ## 39                   119.1
    ## 40                    49.1
    ## 41                    46.4
    ## 42                    78.4
    ## 43                    47.7
    ## 44                    99.3
    ## 45                    34.7
    ## 46                    36.4
    ## 47                    30.6
    ## 48                    87.6
    ## 49                    37.7
    ## 50                    45.0
    ## 51                   103.3
    ## 52                    59.3
    ## 53                    42.7
    ## 54                   131.2
    ## 55                     8.8
    ## 56                    29.2
    ## 57                    56.2
    ## 58                    46.6
    ## 59                    61.4
    ## 60                    97.0
    ## 61                    61.1
    ## 62                   123.2
    ## 63                    44.9
    ## 64                    64.1
    ## 65                    71.8
    ## 66                   181.9
    ## 67                    24.4
    ## 68                    39.7
    ## 69                    42.0
    ## 70                    50.7
    ## 71                    31.1
    ## 72                    43.9
    ## 73                     0.1
    ## 74                    69.9
    ## 75                    40.9
    ## 76                    70.2
    ## 77                    28.9
    ## 78                    40.9
    ## 79                    58.0
    ## 80                    68.5
    ## 81                    61.0
    ## 82                   131.5
    ## 83                   104.1
    ## 84                   236.4
    ## 85                    95.6
    ## 86                    21.2
    ## 87                    55.6
    ## 88                    26.3
    ## 89                     N/A
    ## 90                    39.8
    ## 91                    20.9
    ## 92                    20.6
    ## 93                    59.1
    ## 94                    62.8
    ## 95                    34.8
    ## 96                   152.8
    ## 97                    34.7
    ## 98                    34.4
    ## 99                     4.7
    ## 100                    N/A
    ## 101                   36.5
    ## 102                   23.0
    ## 103                    0.0
    ## 104                   39.3
    ## 105                   37.3
    ## 106                   59.3
    ## 107                   54.2
    ## 108                   68.1
    ## 109                   35.6
    ## 110                   52.6
    ## 111                   91.1
    ## 112                   60.2
    ## 113                   54.2
    ## 114                   24.5
    ## 115                   37.7
    ## 116                   91.4
    ## 117                   67.5
    ## 118                   64.4
    ## 119                  102.2
    ## 120                   46.1
    ## 121                   27.2
    ## 122                   56.7
    ## 123                   26.4
    ## 124                   33.6
    ## 125                   46.5
    ## 126                   23.4
    ## 127                   36.7
    ## 128                   44.2
    ## 129                   67.2
    ## 130                   38.2
    ## 131                   32.6
    ## 132                   25.6
    ## 133                   25.5
    ## 134                   37.8
    ## 135                   51.4
    ## 136                  125.6
    ## 137                   54.0
    ## 138                   36.9
    ## 139                   17.4
    ## 140                   40.6
    ## 141                   71.3
    ## 142                   80.8
    ## 143                   49.1
    ## 144                   83.3
    ## 145                   17.3
    ## 146                   61.2
    ## 147                   61.5
    ## 148                   63.3
    ## 149                   58.4
    ## 150                  110.9
    ## 151                   50.4
    ## 152                   75.4
    ## 153                   10.0
    ## 154                    N/A
    ## 155                   52.7
    ## 156                   98.4
    ## 157                   79.4
    ## 158                  126.0
    ## 159                   72.1
    ## 160                   40.9
    ## 161                   42.8
    ## 162                    N/A
    ## 163                   35.2
    ## 164                   47.8
    ## 165                   38.2
    ## 166                   41.9
    ## 167                    0.0
    ## 168                   78.6
    ## 169                   48.0
    ## 170                   41.3
    ## 171                   71.3
    ## 172                   28.5
    ## 173                   28.8
    ## 174                   39.0
    ## 175                   75.6
    ## 176                   19.5
    ## 177                   87.0
    ## 178                  107.8
    ## 179                   66.2
    ## 180                   24.5
    ## 181                   48.4
    ## 182                   34.9
    ## 183                   58.2
    ## 184                  141.0
    ## 185                   62.2
    ## 186                   78.4

``` r
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
