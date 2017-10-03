# building one nice line graph:
# BY COUNTRY
names(fortrends)
unique(fortrends$Year)
unique(fortrends$Country)
unique(fortrends$Indicator)
library(ggplot2)

names(fortrends)
names(formaps)
mycountry <- trends_countries[131]
# myindicator1 <- "Fixed telephone subscriptions (per 100 people)"
temp <- fortrends %>% 
  filter(Country %in% mycountry) %>% 
  filter(Indicator %in% c(trends_indicators[1], trends_indicators[8])) %>% 
  select(Indicator, Year, c("Z_Score","Mean_Centered_Score")) %>% 
  arrange(Indicator)

# View(temp)
# temp$Z_Score[1:22] <- NA
temp <- formaps %>% 
  filter(Country %in% mycountry) %>% 
  filter(Indicator %in% c(map_indicators[1], map_indicators[7])) %>% 
  select(Indicator, Year, Value) %>% 
  arrange(Indicator)

View(temp)

# I want to have no more than 2 trends on one graph:

library(ggplot2)
my7colors <- c("#d53e4f", "#f46d43", "#fdae61",
               "#abdda4", "#66c2a5", "#3288bd")
mycolor1 <- my7colors[6]
mycolor2 <- my7colors[1]


temp %>% ggplot(aes(x = Year, y = Z_Score)) +
  geom_point(aes(color = Indicator, 
                 shape = Indicator)) + 
  geom_line(aes(color = Indicator,
                linetype = Indicator)) +
  scale_colour_manual(values = c(mycolor1, mycolor2)) +
  scale_x_continuous(limits = c(1992, 2015), 
                     breaks = seq(1992, 2015, 1),
                     expand = c(0, 0)) + 
  ggtitle(mycountry) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90,hjust = 1),
        legend.position = "bottom",
        # axis.title.y = element_blank(),
        # axis.text.y = element_blank(),
        # axis.ticks.y = element_blank(),
        legend.title = element_blank()) +
  guides(Indicator = guide_legend(nrow = 2))



trend1 <- temp %>% ggplot(aes(x = Year, y = Mean_Centered_Score)) +
  geom_point() +
  geom_line(color = mycolor1)  +
  scale_x_continuous(limits = c(1992, 2016), 
                     breaks = seq(1992, 2016, 1),
                     expand = c(0, 0)) +
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 90,hjust = 1)) +
  #  theme_economist() + 
  # scale_fill_economist()
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()
  ) +
  
trend1 + geom_point(aes(x = Year, y = Mean_Centered_Score)) 

trend1 %>% ggplot(aes(x = Year, y = Mean_Centered_Score)) +
  geom_point() +
  geom_line(color = mycolor2) + 
  scale_x_continuous(limits = c(1992, 2016), 
                     breaks = seq(1992, 2016, 1),
                     expand = c(0, 0)) +
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 90,hjust = 1)) +
  #  theme_economist() + 
  # scale_fill_economist()
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()
  )



temp[2,2] <- NA

View(temp)
?geom_line
head(cumulative)

#-----------------------------------------------------------------------------
# Random Forests:
dv <- names(forhappy)[2]
selected <- names(forhappy)[4:6]
tempdf <- forhappy %>% select(one_of(c(dv, selected)))
names(tempdf)[-1] <- paste0("predictor", 1:length(selected))
myformula <- as.formula(paste0(dv, " ~ ."))
fit <- randomForest(formula = myformula, data = tempdf, importance = TRUE)
output <- importance(fit, scale = T)[,1]
output[output < 0] <- 0
output <- output * 100/sum(output)
output <- data.frame(Predictors=selected, Importance = output, stringsAsFactors=F)
row.names(output) <- NULL
names(fit)
rmse = sqrt(mean((fit$y-fit$predicted)^2))
cbind(fit$y, fit$predicted)


pasted <- paste0("`", paste(selected, collapse = "`+`"), "`")
form <- as.formula(paste0(dv, "~", paste0("`", paste(selected, collapse = "`+`"), "`")))
fit <- lm(formula = form, data = forhappy)
sapply(forhappy[-1], function(x) sum(is.na(x)))
fit <- randomForest(forhappy, formula = form, ntree = 500, importance = TRUE)
cor(forhappy[-1])[,1]

set.seed(123)
temp <- tibble(dv = runif(1000),
               `predictor 1` = runif(1000),
               `predictor 2` = runif(1000),
               `predictor 3` = runif(1000),
               `predictor 4` = runif(1000),
               `predictor 5` = runif(1000),
               `predictor 6` = runif(1000))

selected <- names(temp)[2:7]
dv = "dv"
form <- as.formula(paste0(dv, "~", paste0("`", paste(selected, collapse = "`+`"), "`")))
fit <- lm(formula = form, data = temp)
# grab original names:
fornames <- selected

#--------------------------------------------------------
# Unsolved:
# 
# How to make one input widget dependent on another
# in my case I select one KPI, list of others should be shorter
# How to generate 2 columns insead of 1?
# Right now if I change the DV, the analysis is ran automatically
# If I select new predictors, it runs automatically