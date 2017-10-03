library(tidyverse)
md <- read_csv("PWT.csv")
names(md)

dim(md) # 11,830 rows, 47 vars

### Exploring what is what:
count(md, country) %>% count(n)
# 182 countries with 65 rows each (182 * 65)

View(count(md, year))
# year 1950 - 2014

summary(md)

# There are NAs,
# View(filter(md, is.na(pop)))

# I want to remove all years. Let's kill years before 1970
md <- md %>% filter(year > 1969)
sum(is.na(md))
summary(md)
# View(filter(md,is.na(pop)))

# Former USSR has no data before 1990, so let's cut off before 1990
# md <- md %>% filter(year > 1989)
sum(is.na(md))
summary(md)
# View(filter(md,is.na(pop)))

# Removing Curacao & St. Martin - too many NAs
dim(md)
md <- filter(md, !countrycode %in% c("CUW", "SXM"))
summary(md)
dim(md)  # 4,500
# Still have many NAs the following variables:
# emp - 193 - millions of people engaged (in employment?)
# avh - 2842 - average # of hours worked per person engaged
# hc - 900 - human capital index
# ctfp - 1601   - Total Factor Productivity - the portion of output 
# not explained by the amount of inputs used in production
# cwtfp - 1601 - also TFP
# rtfpna - 1601 - also TFP
# rwtfpna - 1601 - also TFP
# labsh - 1175 - Share of labour compensation in GDP at current national prices
# cor_exp - 4064 - Correlation between expenditure shares of the country and the US 
# statcap - 2994 - Statistical capacity indicator

# I shold not be using the following variables:
c("avh", "hc", "ctfp", "cwtfp", "cwtfp", "rtfpna", "rwtfpna",
  "labsh", "cor_exp", "statcap")

View(filter(md,is.na(emp)))

unlist(filter(md,is.na(emp)) %>% select(country) %>% distinct())
# Grab countries for which emp is NA now:
bad <- unlist(filter(md,is.na(emp)) %>% select(country) %>% distinct())
md <- md %>% filter(!country %in%  bad)
dim(md)
summary(md)
View(count(md,year))

### - Reading in 2015 happiness data
hap <- read.csv("2015.csv")
names(hap)
fit <- lm(data = hap, formula = Happiness.Score ~ GDP.per.Capita + Family + Life.Expectancy + 
            Freedom + Government.Corruption + Generosity)
summary(fit)







