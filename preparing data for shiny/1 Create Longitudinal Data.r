library(tidyverse)
library(ggplot2)

###########################################################################
# In this code, I take the World Develompent Indicator data set I downloaded
# from https://www.kaggle.com/worldbank/world-development-indicators/data
# ("Indicators.csv)
#  and select some indicators and some countries to use in my App
###########################################################################


# wdi <- read_csv("indicators.csv")
dim(wdi)   # 5,656,458 rows
names(wdi)
#------------------------------------------------------
# Grabbing 177 countries whose coords we have
# in our maps polygons
#------------------------------------------------------

# rm(list(temp, temp1, temp2))

library(rnaturalearth)
mynames <- rnaturalearth::countries110$name
length(mynames)  # 177
mynames[32] <- "Ivory Coast"
wdinames <- unique(wdi$CountryName)
length(wdinames)  # 247

# good wdi names:
goodwdinames <- wdinames[wdinames %in% mynames]
length(goodwdinames) # 145
# not matched mynames:
notmatched <- mynames[!mynames %in% goodwdinames]

# Changing country names in wdi to match those of 177 names
# that have a differently written name for the same country:
wdi$CountryName[wdi$CountryName %in% "Bahamas, The"] <- notmatched[3]
wdi$CountryName[wdi$CountryName %in% "Bosnia and Herzegovina"] <- notmatched[4]
wdi$CountryName[wdi$CountryName %in% "Brunei Darussalam"] <- notmatched[5]
wdi$CountryName[wdi$CountryName %in% "Central African Republic"] <- notmatched[6]
wdi$CountryName[wdi$CountryName %in% "Cote d'Ivoire"] <- "Ivory Coast"
wdi$CountryName[wdi$CountryName %in% "Congo, Dem. Rep."] <- notmatched[8]
wdi$CountryName[wdi$CountryName %in% "Congo, Rep."] <- notmatched[9]
wdi$CountryName[wdi$CountryName %in% "Czech Republic"] <- notmatched[11]
wdi$CountryName[wdi$CountryName %in% "Dominican Republic"] <- notmatched[12]
wdi$CountryName[wdi$CountryName %in% "Egypt, Arab Rep."] <- notmatched[13]
wdi$CountryName[wdi$CountryName %in% "Gambia, The"] <- notmatched[15]
wdi$CountryName[wdi$CountryName %in% "Equatorial Guinea"] <- notmatched[16]
wdi$CountryName[wdi$CountryName %in% "Iran, Islamic Rep."] <- notmatched[17]
wdi$CountryName[wdi$CountryName %in% "Kyrgyz Republic"] <- notmatched[18]
wdi$CountryName[wdi$CountryName %in% "Korea, Rep."] <- notmatched[19]
wdi$CountryName[wdi$CountryName %in% "Macedonia, FYR"] <- notmatched[20]
wdi$CountryName[wdi$CountryName %in% "Korea, Dem. Rep."] <- notmatched[21]
wdi$CountryName[wdi$CountryName %in% "West Bank and Gaza"] <- notmatched[22]
wdi$CountryName[wdi$CountryName %in% "Russian Federation"] <- notmatched[23]
wdi$CountryName[wdi$CountryName %in% "South Sudan"] <- notmatched[25]
wdi$CountryName[wdi$CountryName %in% "Solomon Islands"] <- notmatched[26]
wdi$CountryName[wdi$CountryName %in% "Slovak Republic"] <- notmatched[28]
wdi$CountryName[wdi$CountryName %in% "Syrian Arab Republic"] <- notmatched[29]
wdi$CountryName[wdi$CountryName %in% "Venezuela, RB"] <- notmatched[31]
wdi$CountryName[wdi$CountryName %in% "Yemen, Rep."] <- notmatched[32]

# good wdi names again:
wdinames2 <- sort(unique(wdi$CountryName))
goodwdinames2 <- wdinames2[wdinames2 %in% mynames]
length(goodwdinames2)  # 170
# not matched mynames - 7 Countries I have in polygons, but no indicators for them
notmatchedmynames <- mynames[!mynames %in% goodwdinames2]

# What wdi names are not part of mynames?
notmatchedwdinames <- wdinames2[!wdinames2 %in% mynames]

#------------------------------------------------------
# Deleting from wdi all countries for which I have
# no polygons for on the map
#------------------------------------------------------

dim(wdi)   # 5,656,458
wdi <- wdi %>% filter(!CountryName %in% notmatchedwdinames)
dim(wdi)   # 4,393,635
length(unique(wdi$CountryName))  # 170 countries

#------------------------------------------------------
# Looking at available indicators, their counts
#------------------------------------------------------

counts <- wdi %>%
  group_by(IndicatorCode, IndicatorName) %>%
  summarise(NumCountries = n_distinct(CountryName),
            NumYears     = n_distinct(Year),
            FirstYear    = min(Year),
            LastYear     = max(Year))

# Note - $ being replaced with dollar to avoid MathJax issue
counts$IndicatorName <- gsub("\\$", "dollar", counts$IndicatorName)
# write.csv(counts, "indicator counts.csv", row.names = F)
# View(counts)

#------------------------------------------------------
# Removing indicators with LastYear < 2014
#------------------------------------------------------

badcodes <- counts$IndicatorCode[counts$LastYear < 2014]
wdi <- wdi %>% filter(!IndicatorCode %in% badcodes)
dim(wdi)      # 3,222,358
unique(wdi$Year)
names(wdi)

#------------------------------------------------------
# Selecting only indicators for 2014 and
# saving as a separate object - to be processed in 2nd script
#------------------------------------------------------

wdi14 <- wdi %>% filter(Year == 2014)
names(wdi14)
# unique(wdi14$Year)
dim(wdi14) # 85,495 rows


#------------------------------------------------------
# Removing years before 1993
#------------------------------------------------------

dim(wdi)      # 3,222,358
count(wdi, Year) %>% arrange(-Year)
wdi <- wdi %>% filter(Year > 1992)
dim(wdi)      # 1,952,185

#------------------------------------------------------
# Removing year 2015
#------------------------------------------------------

wdi <- wdi %>% filter(Year < 2015)
dim(wdi)  # 1,940,922

#------------------------------------------------------
# Removing indicators with counts < 151
#------------------------------------------------------

dim(wdi)  # 1,940,922
badcodes <- counts$IndicatorCode[counts$NumCountries < 151]
wdi <- wdi %>% filter(!IndicatorCode %in% badcodes)
dim(wdi)      # 1,140,686

#------------------------------------------------------
# Removing indicators with FirstYear > 2004
#------------------------------------------------------

badcodes <- counts$IndicatorCode[counts$FirstYear > 2004]
wdi <- wdi %>% filter(!IndicatorCode %in% badcodes)
dim(wdi)      # 1,062,670

#------------------------------------------------------
# Removing indicators with NumYear < 22
#------------------------------------------------------

badcodes <- counts$IndicatorCode[counts$NumYears < 22]
wdi <- wdi %>% filter(!IndicatorCode %in% badcodes)
dim(wdi)      # 1,028,131

#------------------------------------------------------
# Looking at available indicators, their counts AGAIN
#------------------------------------------------------

counts <- wdi %>%
  group_by(IndicatorCode, IndicatorName) %>%
  summarise(NumCountries = n_distinct(CountryName),
            NumYears     = n_distinct(Year),
            FirstYear    = min(Year),
            LastYear     = max(Year))

# Note - $ being replaced with dollar to avoid MathJax issue
counts$IndicatorName <- gsub("\\$", "dollar", counts$IndicatorName)
# View(counts)

#------------------------------------------------------
# Selecting indicators to focus on and removing
# rows for the other indicators.
#------------------------------------------------------
ind1 <- c('AG.LND.TOTL.K2','BX.KLT.DINV.CD.WD','BX.KLT.DINV.WD.GD.ZS','EN.POP.DNST',
          'FP.CPI.TOTL','FP.CPI.TOTL.ZG','IT.CEL.SETS.P2','IT.MLT.MAIN.P2','IT.NET.USER.P2',
          'MS.MIL.XPND.GD.ZS','NE.CON.GOVT.ZS','NE.CON.PETC.ZS','NE.CON.TETC.CD','NE.CON.TETC.KD.ZG',
          'NE.DAB.TOTL.ZS','NE.EXP.GNFS.ZS','NE.IMP.GNFS.KD.ZG','NE.IMP.GNFS.ZS','NE.TRD.GNFS.ZS',
          'NV.AGR.TOTL.ZS','NV.IND.TOTL.ZS','NV.SRV.TETC.ZS','NY.GDP.DEFL.KD.ZG','NY.GDP.MKTP.KD.ZG',
          'NY.GDP.PCAP.CD','NY.GDP.PCAP.KD.ZG','SE.PRM.AGES','SE.PRM.CMPT.FE.ZS','SE.PRM.CMPT.MA.ZS',
          'SE.PRM.DURS','SE.PRM.ENRL.FE.ZS','SE.PRM.ENRR.FE','SE.PRM.ENRR.MA','SE.PRM.TCHR.FE.ZS',
          'SE.SEC.ENRR.FE','SE.SEC.ENRR.MA','SE.SEC.TCHR.FE.ZS','SE.TER.ENRR.FE','SE.TER.ENRR.MA',
          'SE.XPD.TOTL.GD.ZS','SH.DYN.MORT','SH.DYN.NMRT','SH.H2O.SAFE.RU.ZS','SH.H2O.SAFE.UR.ZS',
          'SH.H2O.SAFE.ZS','SH.IMM.IDPT','SH.IMM.MEAS','SH.MED.PHYS.ZS','SH.MMR.RISK.ZS','SH.STA.ACSN',
          'SH.STA.ACSN.RU','SH.STA.ACSN.UR','SH.STA.BRTC.ZS','SH.STA.MMRT','SH.TBS.INCD','SL.AGR.EMPL.ZS',
          'SL.IND.EMPL.ZS','SL.SRV.EMPL.ZS','SL.TLF.ACTI.1524.FE.ZS','SL.TLF.ACTI.1524.MA.ZS',
          'SL.TLF.CACT.FM.ZS','SL.TLF.TOTL.FE.ZS','SL.UEM.1524.FE.ZS','SL.UEM.1524.MA.ZS',
          'SL.UEM.TOTL.FE.ZS','SL.UEM.TOTL.MA.ZS','SP.POP.0014.TO.ZS','SP.POP.1564.TO.ZS',
          'SP.POP.65UP.TO.ZS','SP.POP.GROW','SP.URB.GROW','SP.URB.TOTL.IN.ZS')

dim(wdi)      # 1,028,131
wdi <- wdi %>% filter(IndicatorCode %in% ind1)
dim(wdi)      # 229,227

#------------------------------------------------------
# Leaving only indicators I want in my Shiny App
#------------------------------------------------------
ind2 <- c('BX.KLT.DINV.WD.GD.ZS','FP.CPI.TOTL','FP.CPI.TOTL.ZG','IT.CEL.SETS.P2',
          'IT.MLT.MAIN.P2','IT.NET.USER.P2','NE.CON.TETC.KD.ZG','NE.EXP.GNFS.ZS',
          'NE.IMP.GNFS.ZS','NE.TRD.GNFS.ZS','NY.GDP.DEFL.KD.ZG','NY.GDP.MKTP.KD.ZG',
          'NY.GDP.PCAP.CD','SE.PRM.ENRL.FE.ZS','SE.PRM.ENRR.FE','SE.PRM.ENRR.MA',
          'SH.DYN.MORT','SH.DYN.NMRT','SH.H2O.SAFE.ZS','SH.IMM.IDPT','SH.MED.PHYS.ZS',
          'SH.MMR.RISK.ZS','SH.STA.ACSN','SH.STA.MMRT','SH.TBS.INCD','SL.TLF.TOTL.FE.ZS',
          'SL.UEM.TOTL.FE.ZS','SL.UEM.TOTL.MA.ZS','SP.POP.0014.TO.ZS','SP.POP.1564.TO.ZS',
          'SP.POP.65UP.TO.ZS','SP.POP.GROW','SP.URB.GROW','SP.URB.TOTL.IN.ZS')

dim(wdi)      # 229,227
wdi <- wdi %>% filter(IndicatorCode %in% ind2)
dim(wdi)      # 116,636

#------------------------------------------------------
# Shorten names of some indicators
#------------------------------------------------------

unique(wdi$IndicatorName)
wdi$IndicatorName[wdi$IndicatorName %in% unique(wdi$IndicatorName)[5]] <-
  "Improved sanitation facilities (% of pop. with access)"
wdi$IndicatorName[wdi$IndicatorName %in% unique(wdi$IndicatorName)[6]] <-
  "Improved water source (% of pop. with access)"
wdi$IndicatorName[wdi$IndicatorName %in% unique(wdi$IndicatorName)[10]] <-
  "Maternal mortality ratio per 100K live births"
wdi$IndicatorName[wdi$IndicatorName %in% unique(wdi$IndicatorName)[14]] <-
  "% of students in primary ed. who are female"
wdi$IndicatorName[wdi$IndicatorName %in% unique(wdi$IndicatorName)[33]] <-
  "Final consumption expenditure (annual % growth)"
unique(wdi$IndicatorName)

#==================================================================================
# Standardizing all indeces by country (score - mean)/sd - 
# and just score-mean
# it'll be used for some of the visuals
# Also - updating the data and notmatchedmynames based on observations:
#==================================================================================

# notmatchedmynames 
# View(wdi %>% group_by(IndicatorName, CountryName) %>% 
#    summarize(n = n(), nr_of_nas = sum(is.na(Value))))


#--------------------------------------
# Removing additional indicators:
# length(unique(wdi$IndicatorCode))
# badindic <- c("% of students in primary ed. who are female",
#               "Consumer price index (2010 = 100)",
#               "Exports of goods and services (% of GDP)",
#               "Gross enrolment ratio, primary, female (%)",
#               "Gross enrolment ratio, primary, male (%)",
#               "Physicians (per 1,000 people)",
#               "Population ages 65 and above (% of total)",
#               "Population, ages 0-14 (% of total)",
#               "Population, ages 15-64 (% of total)",
#               "Final consumption expenditure (annual % growth)",
#               "Foreign direct investment, net inflows (% of GDP)",
#               "GDP per capita (current US$)",
#               "Imports of goods and services (% of GDP)",
#               "Trade (% of GDP)",
#               "Immunization, DPT (% of children ages 12-23 months)",
#               "Improved water source (% of pop. with access)",
#               "Internet users (per 100 people)",
#               "Improved sanitation facilities (% of pop. with access)",
#               "Incidence of tuberculosis (per 100,000 people)"
#              )
# 
# dim(wdi)      # 116,636
# wdi <- wdi %>% filter(!IndicatorName %in% badindic)
# dim(wdi)      # 54,250
# length(unique(wdi$IndicatorCode)) # 15

### Calculating Value Z Score and Centered Score by country:
wdi <- wdi %>% group_by(IndicatorName, CountryName) %>% 
  mutate(Value.z = (Value - mean(Value, na.rm = T))/sd(Value, na.rm = T),
         Value.Centered = Value - mean(Value, na.rm = T)) %>% 
  ungroup

# Check that the sum of Value.z for each indicator/contry is 0:
temp <- wdi %>% group_by(IndicatorName, CountryName) %>% 
       summarize(summa = sum(Value.z, na.rm = T),
                 std = sd(Value.z, na.rm = T)) %>% 
  mutate(summa = round(summa, 1), std = round(std, 1))
# View(temp)
sum(temp$summa) == 0  # it should be 0 even without na.rm = T
sum(temp$std); length(unique(wdi$IndicatorName)) *
                    length(unique(wdi$CountryName))  # it should be 0 even without na.rm = T
### Replacing NA in Value.z for all cases when Value is not NA with a 0
wdi$Value.z[is.na(wdi$Value.z) & !is.na(wdi$Value)] <- 0

# write.csv(wdi, "x test.csv", row.names = F)
#------------------------------------------------------
# Making implicit NAs explicit so that every country
# has a value (or NA) for every indicator, every year
#------------------------------------------------------

names(wdi)
dim(wdi)  # 116,636
wdi <- complete(wdi, CountryName, Year, IndicatorName)
dim(wdi)  # 127,160
# View(count(wdi, IndicatorName))
# View(count(wdi, CountryName))
# View(count(wdi, Year))

#------------------------------------------------------
# Finalizing variable names in 2 final files
#------------------------------------------------------

names(wdi)[c(1, 3)] <- c("Country", "Indicator")

# write.csv(wdi[c(1:3, 6:8)], "World Dev Indic Final.csv", row.names = F, na = "")
