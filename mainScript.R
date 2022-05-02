library(readr)
library(tidyverse)
library(AER)
library(stargazer)
library(cobalt)

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, ggplot2, dplyr, lubridate, readr, readxl,
               scales, cobalt, ivpack, stargazer, ggthemes, AER, fixest)

CigData <- readRDS("~/Desktop/ECON 470/tobaccoLaptop/TaxBurden_Data.rds")
ObesityPercentOver18 <- read_csv("~/Desktop/ECON470FINAL/ObesityPercentOver18.csv")

# CONVERTING TO 2012 REAL DOLLARS
CigData <- CigData %>%
  mutate(tax_change = tax_state - lag(tax_state),
         tax_change_d = ifelse(tax_change==0,0,1),
         price_cpi_2012 = cost_per_pack*(229.5939/index),
         total_tax_cpi_2012=tax_dollar*(229.5939/index),
         ln_sales=log(sales_per_capita),
         ln_price_2012=log(price_cpi_2012))

# COMBINING DATA
CigObesity <- inner_join(CigData,ObesityPercentOver18, by=c("Year"="YearStart","state"="LocationDesc"))

# SUMMARIES
# Cigarette Sales Over Time
cigSalesByYear <- CigData %>% group_by(Year) %>% summarise(sales=mean(sales_per_capita,na.rm=TRUE)) %>% mutate(obesity=NA)
ggplot(cigSalesByYear, aes(x=Year,y=sales)) + geom_line() + ggtitle("Cigarette Sales per Capita Over Time")

# Obesity Over Time
obesityByYear <- ObesityPercentOver18 %>% filter(Question=="Percent of adults aged 18 years and older who have obesity") %>% group_by(YearStart) %>% summarise(obesity=mean(Data_Value,na.rm=TRUE)) %>% mutate(sales=NA, Year=YearStart) %>% select(Year,sales,obesity)
ggplot(obesityByYear, aes(x=YearStart,y=obesity)) + geom_line() + ggtitle("Obesity Over Time")

# both of above on same graph?

bothByYear <- rbind(cigSalesByYear,obesityByYear) %>% filter(Year>2010)
bothByYear <- bothByYear %>% mutate(percChangeCig=NA, percChangeObesity=NA)

for(i in 2:nrow(bothByYear)){
  thisYearSales <- bothByYear[i,]$sales
  lastYearSales <- bothByYear[i-1,]$sales
  percChangeCig <- 100*(thisYearSales-lastYearSales)/lastYearSales
 
  thisYearObesity <- bothByYear[i,]$obesity
  lastYearObesity <- bothByYear[i-1,]$obesity
  percChangeObesity <- 100*(thisYearObesity-lastYearObesity)/lastYearObesity
  
  bothByYear[i,]$percChangeCig = percChangeCig
  bothByYear[i,]$percChangeObesity = percChangeObesity
}

ggplot(bothByYear, aes(x=Year,y=percChangeCig, color="Cigarette Sales")) +
  geom_point() +
  geom_point(data=bothByYear,aes(x=Year,y=percChangeObesity, color="Obesity")) +
  ylim(-10,10) +
  ggtitle("CigaretteSales and Obesity Over Time")

bothByYearCombined <- bothByYear %>% group_by(Year) %>% summarize(percChangeCig = mean(percChangeCig,na.rm=TRUE), percChangeObesity = mean(percChangeObesity, na.rm=TRUE)) %>% filter(Year>=2012 & Year<=2018)
ggplot(bothByYear, aes(x=percChangeCig, y=percChangeObesity)) + geom_point()

# other variables that affect obesity
  # income, race, gender, education


# adding ln to dataset
regressionData <- CigObesity %>%
  filter(Question=="Percent of adults aged 18 years and older who have obesity") %>%
  group_by(Year,state) %>%
  summarize(sales_per_capita = mean(sales_per_capita,na.rm=TRUE), cost_per_pack = mean(cost_per_pack,na.rm=TRUE),
            obesity=mean(Data_Value,na.rm=TRUE)) %>%
  mutate(ln_obesity = log(obesity)) %>%
  mutate(ln_sales = log(sales_per_capita)) %>%
  mutate(ln_cost = log(cost_per_pack))

# simple regression to see relationship between cigarette sales and obesity
ols <- lm(ln_obesity ~ ln_sales, data=regressionData)
summary(ols)

# CHOOSING INSTRUMENT
# first stage for prices on sales
firstStagePrice <- lm(formula = sales_per_capita ~ cost_per_pack, data = regressionData)
summary(firstStagePrice)

# first stage for taxes on sales
firstStageTaxes <- lm(formula = sales_per_capita ~ tax_dollar, data = regressionData)
summary(firstStageTaxes)

# IV Regression
iv <- ivreg(formula = ln_obesity ~ ln_sales | cost_per_pack , data = regressionData)
summary(iv)

stargazer(ols, iv, type="text", model.names=TRUE, object.names=TRUE)

## check the first stage
firstStage <- lm(ln_sales~ln_cost, data=regressionData)

## check the reduced form
reducedForm <- lm(ln_obesity~ln_cost, data=regressionData)

stargazer(firstStage,reducedForm, type="text", model.names=TRUE, object.names=TRUE)

# STATE AND YEAR FIXED EFFECTS

#stateFE <- summary(feols(obesity ~ sales_per_capita | state, data=regressionData))
stateFEols <- lm(obesity ~ sales_per_capita + factor(state) - 1, data=regressionData)
#yearFE <- summary(feols(obesity ~ sales_per_capita | Year, data=regressionData))
yearFEols <- lm(obesity ~ sales_per_capita + factor(Year) - 1, data=regressionData)
#stateYearFE <- summary(feols(obesity ~ sales_per_capita | state + Year, data=regressionData))
stateYearFEols <- lm(obesity ~ sales_per_capita + factor(state) - 1 + factor(Year) -1, data=regressionData)

stateFEiv <- ivreg(obesity ~ sales_per_capita + factor(state) - 1, data=regressionData)
yearFEiv <- ivreg(obesity ~ sales_per_capita + factor(Year) - 1, data=regressionData)
stateYearFEiv <- ivreg(obesity ~ sales_per_capita + factor(state) - 1 + factor(Year) -1, data=regressionData)

stargazer(ols, stateFEols, stateYearFEols, iv, stateFEiv, stateYearFEiv, type="text", model.names=TRUE, 
          object.names=TRUE,  omit=c("state","Year"),
          add.lines = c("State Fixed Effects & No & Yes & Yes & No & Yes & Yes \\\\",
                             "Year Fixed Effects & No & No & Yes & No & No & Yes \\\\")
)















## in two stages
step1 <- lm(obesity ~ sales_per_capita, data=regressionData)
d.hat <- predict(step1)
step2 <- lm(obesity ~ d.hat, data=regressionData)
summary(step2)

# state and year fixed effects
fixedEffects = fixest::fixef(ols)
summary(fixedEffects)

















