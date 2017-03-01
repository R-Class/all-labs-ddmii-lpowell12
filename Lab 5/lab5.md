lab 5 - create a research database of home values in Syracuse
================
Linnea Powell
February 21, 2017

``` r
#install.packages( "RCurl" )

#install.packages( "ggmap" )

#install.packages( "jsonlite" )

#install.packages( "memisc" )
```

1. Start with the dataset of home prices and assets collected from Zillow
-------------------------------------------------------------------------

``` r
# load package to read URL data
library( RCurl )

# address of Housing Price In-Class Exercise spreadsheet
my.url <- "https://docs.google.com/spreadsheets/d/1W0vM5sCRhZjkQh6A0WGV8j1rhQAjecCrQW7PguHft-E/pub?gid=1989082857&single=true&output=csv"

housing.raw <- getURL( my.url, ssl.verifypeer=FALSE )

dat <- read.csv( textConnection(housing.raw), stringsAsFactors=FALSE )

head( dat )
```

    ##            Timestamp House.Price  X X.1 Square.Feet      Your.Name
    ## 1 1/15/2015 16:11:46  179,900.00 NA  NA    1,600.00 Emily Simonson
    ## 2 1/15/2015 16:17:10  128,000.00 NA  NA    1,992.00 Emily Simonson
    ## 3 1/15/2015 16:25:52  114,900.00 NA  NA    1,378.00 Emily Simonson
    ## 4 1/15/2015 16:34:22  107,500.00 NA  NA    1,452.00 Emily Simonson
    ## 5 1/15/2015 16:41:35   43,000.00 NA  NA      850.00 Emily Simonson
    ## 6 1/15/2015 16:52:22   85,000.00 NA  NA    1,639.00 Emily Simonson
    ##   Lot.Size..in.SQUARE.FEET. Number.of.Bedrooms Number.of.Bathrooms
    ## 1                 43,560.00                  3                 2.0
    ## 2                  6,969.00                  4                 2.5
    ## 3                  5,227.00                  4                 1.0
    ## 4                  5,227.00                  3                 1.0
    ## 5                  6,098.00                  2                 1.0
    ## 6                  7,840.00                  4                 1.0
    ##   Does.it.have.a.garage. Year.Built Elementary.School.Score
    ## 1                    Yes       1994                       9
    ## 2                    Yes       1950                       2
    ## 3                    Yes       1930                       2
    ## 4                    Yes       1931                       2
    ## 5                    Yes       1955                       2
    ## 6                     No       1915                       2
    ##   Middle.School.Score High.School.Score Walk.Score Property.Taxes
    ## 1                   4                 2         15       3,182.00
    ## 2                   4                 2         43       1,393.00
    ## 3                   4                 2         50       1,331.00
    ## 4                   4                 1         42         157.00
    ## 5                   9                 1         57       1,525.00
    ## 6                   4                 1         36       2,184.00
    ##   Is.it.within.two.blocks.of.a.highway.or.interstate.
    ## 1                                                  No
    ## 2                                                  No
    ## 3                                                  No
    ## 4                                                  No
    ## 5                                                  No
    ## 6                                                  No
    ##   Walking.distance.to.the.nearest.good.restaurant...in.MINUTES...
    ## 1                                                              22
    ## 2                                                               7
    ## 3                                                               6
    ## 4                                                              12
    ## 5                                                               8
    ## 6                                                              32
    ##   Driving.distance.to.the.nearest.Starbucks...in.MILES...
    ## 1                                                     3.2
    ## 2                                                     2.6
    ## 3                                                     2.6
    ## 4                                                     2.6
    ## 5                                                     2.3
    ## 6                                                     1.9
    ##   Walking.distance.to.the.nearest.large.park..at.least.4.square.blocks..in.MINUTES.
    ## 1                                                                                18
    ## 2                                                                                 5
    ## 3                                                                                 7
    ## 4                                                                                 8
    ## 5                                                                                20
    ## 6                                                                                11
    ##   Driving.distance.to.the.nearest.strip.or.shopping.mall...in.MILES...
    ## 1                                                                  1.3
    ## 2                                                                  0.6
    ## 3                                                                  0.5
    ## 4                                                                  0.8
    ## 5                                                                  0.6
    ## 6                                                                  2.5
    ##   Street.Address.of.House Zip.Code
    ## 1      504 Winkworth Pkwy    13219
    ## 2          136 Austin Ave    13207
    ## 3          701 Velasko Rd    13207
    ## 4         518 Wolcott Ave    13207
    ## 5         112 Wolcott Ave    13207
    ## 6         212 Roberts Ave    13207
    ##   The.Census.Tract.in.which.the.house.resides.
    ## 1                                           NA
    ## 2                                           NA
    ## 3                                           NA
    ## 4                                           NA
    ## 5                                           NA
    ## 6                                           NA

``` r
# RENAME VARIABLES
names( dat ) <- c("timestamp","price","X1","X2","sqft","your.name","lot.size","beds",
                  "bath","garage","year","elementary","middle","high","walk","tax","highway",
                  "restaurant","starbucks","park","mall","address","zip","tract" )
# remove commas from numbers
dat$price <- as.numeric( gsub( ",","", dat$price ) )
dat$tax <- as.numeric( gsub( ",","", dat$tax ) )
dat$lot.size <- as.numeric( gsub( ",","", dat$lot.size ) )
dat$sqft <- as.numeric( gsub( ",","", dat$sqft ) )

dat$lot.size[ is.na( dat$lot.size ) ] <- mean( dat$lot.size, na.rm=T )

# clean up
rm( housing.raw )
rm( my.url )
```

``` r
houses <- dat[ , c("address","zip") ]

houses$address <- gsub( ",", "", houses$address )
houses$address <- gsub( "\\.", "", houses$address )

addresses <- paste( houses$address, "Syracuse, NY", houses$zip, sep=", " )

head( addresses )
```

    ## [1] "504 Winkworth Pkwy, Syracuse, NY, 13219"
    ## [2] "136 Austin Ave, Syracuse, NY, 13207"    
    ## [3] "701 Velasko Rd, Syracuse, NY, 13207"    
    ## [4] "518 Wolcott Ave, Syracuse, NY, 13207"   
    ## [5] "112 Wolcott Ave, Syracuse, NY, 13207"   
    ## [6] "212 Roberts Ave, Syracuse, NY, 13207"

``` r
library( ggmap )

# translate street address to latitude longitude coordinates

#lat.long <- geocode( addresses [1:6] )
#lat.long

# pre-geocoded version of dataset for demo

lat.long <- read.csv ("https://raw.githubusercontent.com/lecy/hedonic-prices/master/Data/lat.long.csv")

head(lat.long)
```

    ##         lon      lat
    ## 1 -76.19918 43.02561
    ## 2 -76.18854 43.02895
    ## 3 -76.18540 43.02957
    ## 4 -76.18301 43.02757
    ## 5 -76.18331 43.03206
    ## 6 -76.17198 43.03111

2. Add a census tracts FIPS ID to each home (spatial join to census tracts)
---------------------------------------------------------------------------

``` r
library( maptools )
library( sp )
library( spatialEco )
download.file("ftp://ftp2.census.gov/geo/tiger/TIGER2010/TRACT/2010/tl_2010_36067_tract10.zip", "onondaga census tracts.zip" )

unzip( "onondaga census tracts.zip" )

file.remove( "onondaga census tracts.zip" )
```

    ## [1] TRUE

``` r
onondoga <- readShapePoly( fn="tl_2010_36067_tract10", proj4string=CRS("+proj=longlat +datum=WGS84") )
#syr <- readShapePoly( fn="01-05-2015", proj4string=CRS("+proj=longlat +datum=WGS84") )

lat.long2<-SpatialPoints(lat.long, proj4string=CRS("+proj=longlat +datum=WGS84"))

poly.data.matched.to.points <- over(lat.long2, onondoga )

all.dat <- cbind( houses, lat.long, poly.data.matched.to.points )
```

3. Add census data to each home
-------------------------------

``` r
devtools::install_github("hrecht/censusapi")
censuskey <- "b06146bd7469912826a96e117fcf4a8ab8bc36de"
library(censusapi)
library(jsonlite)
```

``` r
# get variables for employment and travel time to work

myvars <- c("NAME", "B23025_004E", "B08136_001E")
acs5_2015 <- getCensus(name="acs5", vintage=2015, key=censuskey,
    vars=myvars, region="tract:*", regionin="state:36")
head(acs5_2015)
```

    ##                                         NAME state county  tract
    ## 1    Census Tract 1, Albany County, New York    36    001 000100
    ## 2    Census Tract 2, Albany County, New York    36    001 000200
    ## 3    Census Tract 3, Albany County, New York    36    001 000300
    ## 4 Census Tract 4.01, Albany County, New York    36    001 000401
    ## 5 Census Tract 4.03, Albany County, New York    36    001 000403
    ## 6 Census Tract 4.04, Albany County, New York    36    001 000404
    ##   B23025_004E B08136_001E
    ## 1         641          NA
    ## 2        1866          NA
    ## 3        2125          NA
    ## 4         885          NA
    ## 5        2678          NA
    ## 6        1067       13950

``` r
acs5_2015_onondaga <- subset(acs5_2015, county == "067")

census.house.dat <- merge (all.dat, acs5_2015_onondaga, by.x="TRACTCE10", by.y="tract", all.x=T)
head(census.house.dat)
```

    ##   TRACTCE10            address   zip       lon      lat STATEFP10
    ## 1    000100     139 Pulaski St 13204 -76.17168 43.05846        36
    ## 2    000100 106 Giminski Drive 13204 -76.17251 43.05800        36
    ## 3    000100     103 Pulaski St 13204 -76.17074 43.05790        36
    ## 4    000200    103 Arnts Place 13208 -76.15634 43.07583        36
    ## 5    000200  805 Turtle Street 13208 -76.15631 43.07448        36
    ## 6    000200   619 2nd N Street 13208 -76.16063 43.07537        36
    ##   COUNTYFP10     GEOID10 NAME10     NAMELSAD10 MTFCC10 FUNCSTAT10 ALAND10
    ## 1        067 36067000100      1 Census Tract 1   G5020          S 4842958
    ## 2        067 36067000100      1 Census Tract 1   G5020          S 4842958
    ## 3        067 36067000100      1 Census Tract 1   G5020          S 4842958
    ## 4        067 36067000200      2 Census Tract 2   G5020          S 1095299
    ## 5        067 36067000200      2 Census Tract 2   G5020          S 1095299
    ## 6        067 36067000200      2 Census Tract 2   G5020          S 1095299
    ##   AWATER10  INTPTLAT10   INTPTLON10
    ## 1  1284980 +43.0691355 -076.1730170
    ## 2  1284980 +43.0691355 -076.1730170
    ## 3  1284980 +43.0691355 -076.1730170
    ## 4        0 +43.0747759 -076.1583997
    ## 5        0 +43.0747759 -076.1583997
    ## 6        0 +43.0747759 -076.1583997
    ##                                        NAME state county B23025_004E
    ## 1 Census Tract 1, Onondaga County, New York    36    067         335
    ## 2 Census Tract 1, Onondaga County, New York    36    067         335
    ## 3 Census Tract 1, Onondaga County, New York    36    067         335
    ## 4 Census Tract 2, Onondaga County, New York    36    067        1325
    ## 5 Census Tract 2, Onondaga County, New York    36    067        1325
    ## 6 Census Tract 2, Onondaga County, New York    36    067        1325
    ##   B08136_001E
    ## 1          NA
    ## 2          NA
    ## 3          NA
    ## 4       23680
    ## 5       23680
    ## 6       23680

4. Aggregate crimes by census tract and add to the dataset
----------------------------------------------------------

``` r
library(dplyr)
crimes.dat <- read.csv ("https://raw.githubusercontent.com/lecy/hedonic-prices/master/Data/crime.lat.lon.csv")

head(crimes.dat)
```

    ##         lon      lat
    ## 1 -76.16636 43.05239
    ## 2 -76.11191 43.02917
    ## 3 -76.11991 43.04385
    ## 4 -76.12334 43.06962
    ## 5 -76.12238 43.15594
    ## 6 -76.14150 43.09776

``` r
crimes.dat2<-SpatialPoints(crimes.dat, proj4string=CRS("+proj=longlat +datum=WGS84"))

crimes.combine <- over(crimes.dat2, onondoga )

crimes.dat3<- cbind(crimes.dat, crimes.combine)

crimes.dat4<- group_by(crimes.dat3, TRACTCE10)

crimes.dat5<- as.data.frame(summarise(crimes.dat4, crimecount = n()))

census.house.crime.dat <- merge (census.house.dat, crimes.dat5, by.x="TRACTCE10", by.y="TRACTCE10", all.x=T)
```
