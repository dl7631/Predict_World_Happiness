library(tidyverse)
library(ggplot2)

###########################################################################
# In this code, I am merging 2014 indicators with 2015 Happiness index data
###########################################################################

dim(wdi14) # 85,495 rows
# View(count(wdi14, IndicatorName))
# View(wdi14 %>% distinct(CountryName))

# ------------------------------------------------------
# Counting number of countries that have each indicator
# ------------------------------------------------------

counts <- wdi14 %>% count(IndicatorCode) %>% arrange(-n)
# View(counts)

# ------------------------------------------------------
# Leaving in the data set only indicators that have 
# data from at least 160 countries
# ------------------------------------------------------

goodcodes <- counts %>% filter(n > 159) %>% 
  select(IndicatorCode) %>% unlist
dim(wdi14)  # 85,495
wdi14 <-  wdi14 %>% filter(IndicatorCode %in% goodcodes)
dim(wdi14)  # 19,193

# ------------------------------------------------------
# Leaving in the data set only countries
# that have the same (large) set of indicators
# ------------------------------------------------------

counts <- wdi14 %>% count(CountryName) %>% arrange(-n)
# View(counts)

badones <- c("Tuberculosis case detection rate (%, all forms)",
             "Foreign direct investment, net inflows (BoP, current US$)",
             "Proportion of seats held by women in national parliaments (%)",
             "Fixed broadband subscriptions")

wdi14 <- wdi14 %>% filter(!IndicatorName %in% badones)
dim(wdi14)  # 18,538

counts <- wdi14 %>% count(CountryName) %>% arrange(-n)
goodones <- counts %>% filter(n > 111) %>% 
  select(CountryName) %>% unlist
wdi14 <- wdi14 %>% filter(CountryName %in% goodones)
dim(wdi14)  # 15,344
counts <- wdi14 %>% count(CountryName) %>% arrange(-n)
# View(counts)   # Now, each country has data on 112 indicators

# ------------------------------------------------------
# Shortening indicator names
# ------------------------------------------------------

library(stringr)
unique(wdi14$IndicatorName)
wdi14$IndicatorName <- str_replace_all(wdi14$IndicatorName, 
                                       pattern = " \\(modeled ILO estimate\\)",
                                       replacement = "")
wdi14$IndicatorName <- str_replace_all(wdi14$IndicatorName, 
                                       pattern = "total merchandise exports",
                                       replacement = "total")
wdi14$IndicatorName <- str_replace_all(wdi14$IndicatorName, 
                                       pattern = "Merchandise imports by the reporting economy, residual",
                                       replacement = "Merchandies imports by economy, residual")
wdi14$IndicatorName <- str_replace_all(wdi14$IndicatorName, 
                                       pattern = "Merchandise imports by the reporting economy",
                                       replacement = "Merchandise imports by economy")
wdi14$IndicatorName <- str_replace_all(wdi14$IndicatorName, 
                                       pattern = " of total merchandise imports",
                                       replacement = "of total")
wdi14$IndicatorName <- str_replace_all(wdi14$IndicatorName, 
                                       pattern = "population",
                                       replacement = "pop.")
wdi14$IndicatorName <- str_replace_all(wdi14$IndicatorName, 
                                       pattern = "by the reporting economy",
                                       replacement = "by economy")
wdi14$IndicatorName <- str_replace_all(wdi14$IndicatorName, 
                                       pattern = "pop\\. with access",
                                       replacement = "pop\\.")

wdi14$IndicatorName <- str_replace_all(wdi14$IndicatorName, 
                                       pattern = "0=less disclosure to 10=more disclosure",
                                       replacement = "0=less to 10=more")
wdi14$IndicatorName <- str_replace_all(wdi14$IndicatorName, 
                                       pattern = "participation",
                                       replacement = "particip\\.")
wdi14$IndicatorName <- str_replace_all(wdi14$IndicatorName, 
                                       pattern = "Unemployment",
                                       replacement = "Unemp\\.")
wdi14$IndicatorName <- str_replace_all(wdi14$IndicatorName, 
                                       pattern = "modeled estimate\\, ",
                                       replacement = "")
wdi14$IndicatorName <- str_replace_all(wdi14$IndicatorName, 
                                       pattern = "of total",
                                       replacement = "of ttl\\.")
# unique(wdi14$IndicatorName)


# ------------------------------------------------------
# Reshaping the data by spreading 'IndicatorName'
# ------------------------------------------------------

wdi14 <- wdi14 %>% select(CountryName, IndicatorName, Value)
wdi14_spr <- spread(wdi14, key = IndicatorName, value = Value)
dim(wdi14_spr)
# names(wdi14_spr)

# ------------------------------------------------------
# Checking that all indicators have variance > 0
# ------------------------------------------------------

min(sapply(wdi14_spr[-1], mean))
max(sapply(wdi14_spr[-1], mean))
mean(sapply(wdi14_spr[-1], mean))
min(sapply(wdi14_spr[-1], sd))
max(sapply(wdi14_spr[-1], sd))
mean(sapply(wdi14_spr[-1],sd))

# We are good - all predictors have some variance

# --------------------------------------------------------------------------------
# Reading in world happiness report data for 2015
# I downloaded from here: https://www.kaggle.com/unsdsn/world-happiness/data
# file "2015.csv"
# --------------------------------------------------------------------------------

hap <- read_csv("Happiness 2015.csv")
# str(hap)
head(hap)


# Removing some columns I don't need:
hap <- hap[c(1, 3, 4)]
names(hap)

# ------------------------------------------------------
# Merging with wdi14_spr by country name, but
# checking those names first
# ------------------------------------------------------

names14 <- sort(wdi14_spr$CountryName)
length(names14)   # 137
namesHap <- sort(hap$Country)
matching14names <- names14[names14 %in% namesHap]
nonmatching14names <- names14[!names14 %in% namesHap]

# Replacing some country names in wdi14 to match those in Happiness:
wdi14_spr$CountryName[wdi14_spr$CountryName %in% "Bosnia and Herz."] <- "Bosnia and Herzegovina"
wdi14_spr$CountryName[wdi14_spr$CountryName %in% "Central African Rep."] <- "Central African Republic" 
wdi14_spr$CountryName[wdi14_spr$CountryName %in% "Congo"] <- "Congo (Brazzaville)"
wdi14_spr$CountryName[wdi14_spr$CountryName %in% nonmatching14names[6]] <- "Ivory Coast"
wdi14_spr$CountryName[wdi14_spr$CountryName %in% "Czech Rep."] <- "Czech Republic"
wdi14_spr$CountryName[wdi14_spr$CountryName %in% "Dem. Rep. Congo"] <- "Congo (Kinshasa)"
wdi14_spr$CountryName[wdi14_spr$CountryName %in% "Dominican Rep."] <- "Dominican Republic"
wdi14_spr$CountryName[wdi14_spr$CountryName %in% "Lao PDR"] <- "Laos"

names14 <- sort(wdi14_spr$CountryName)
length(names14)   # 137
namesHap <- sort(hap$Country)
matching14names <- names14[names14 %in% namesHap]
nonmatching14names <- names14[!names14 %in% namesHap]
nonmatching14names
# We will have to loose these 6 countries because we have no happiness index for them
# I am removing their rows

nrow(wdi14_spr)  # 137
wdi14_spr <- wdi14_spr %>% filter(!CountryName %in% nonmatching14names)
nrow(wdi14_spr)  # 131

# Updating some var names a bit:
names(hap) <- c("Country", "Happiness_Rank", "Happiness_Score")
names(wdi14_spr)[1] <- "Country"

nrow(wdi14_spr)  # 131
happy <- left_join(wdi14_spr, hap, by = "Country")
nrow(happy)  # 131
# head(happy)

# Reorder the columns a bit:
# names(happy)
happy <- happy[c(1, (ncol(happy) - 1), ncol(happy), 2:(ncol(happy) - 2))]

# Checking correlations among variables:
dim(happy)
View(round(cor(happy[-1]), 2))

# Removing some highly colinear predictors based on these correlations:
badcolumns <- c(-5, -6, -7, -12, -16, -20, -21, -22, -23, -24, -25, -28,
                -31, -32, -34, -41, -47, -48, -49, -52, -54, -57, -58, 
                -61, -64, -65, -68, -69, -78, -88, -104, -109, -110, -111, -112,
                -114)
happy <- happy[, badcolumns]
# Removing a couple more columns
happy <- happy[, c(-25, -26)]
dim(happy)  # 77 COLUMNS LEFT

# write.csv(happy, "Happiness Final.csv", row.names = F, na = "")

