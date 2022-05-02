
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
cigSalesByYear <- CigData %>% group_by(Year) %>%
  summarise(sales=mean(sales_per_capita,na.rm=TRUE)) %>% mutate(obesity=NA)
ggplot(cigSalesByYear,aes(x=Year, y=sales)) + geom_line() +
  ggtitle("Average Cigarette Packs Sold per Capita 1970-2018") +
  xlab("Year") + ylab("Sales per Capita")

# Obesity Over Time
obesityByYear <- ObesityPercentOver18 %>% filter(Question=="Percent of adults aged 18 years and older who have obesity") %>% group_by(YearStart) %>% summarise(obesity=mean(Data_Value,na.rm=TRUE)) %>% mutate(sales=NA, Year=YearStart) %>% select(Year,sales,obesity)
ggplot(obesityByYear, aes(x=Year,y=obesity)) + geom_line() +
  ggtitle("Obesity Over Time") +
  xlab("Year") + ylab("Percent Obesity")

# both of above on same graph?

bothByYear2 <- inner_join(cigSalesByYear,obesityByYear, by="Year") %>% filter(Year>2010)
bothByYear2 <- bothByYear2 %>% mutate(percChangeCig=NA, percChangeObesity=NA, propCig=NA, propObesity=NA)

for(i in 2:nrow(bothByYear2)){
  thisYearSales <- bothByYear2[i,]$sales.x
  lastYearSales <- bothByYear2[i-1,]$sales.x
  percChangeCig <- 100*(thisYearSales-lastYearSales)/lastYearSales
  propCig <- thisYearSales/lastYearSales
  
  thisYearObesity <- bothByYear2[i,]$obesity.y
  lastYearObesity <- bothByYear2[i-1,]$obesity.y
  percChangeObesity <- 100*(thisYearObesity-lastYearObesity)/lastYearObesity
  propObesity <- thisYearObesity/lastYearObesity
  
  bothByYear2[i,]$percChangeCig = percChangeCig
  bothByYear2[i,]$percChangeObesity = percChangeObesity
  bothByYear2[i,]$propCig = propCig
  bothByYear2[i,]$propObesity = propObesity
}

ggplot(bothByYear2, aes(x=Year,y=sales.x, color="Cigarette Sales")) +
  geom_line() +
  geom_line(data=bothByYear2,aes(x=Year,y=obesity.y, color="Obesity")) +
  ggtitle("Cigarette Sales and Obesity Over Time") +
  xlab("Year") + ylab("Sales per Capita or Percent Obesity")

ggplot(bothByYear2, aes(x=Year,y=propCig, color="Cigarette Sales")) +
  geom_line() +
  geom_line(data=bothByYear2,aes(x=Year,y=propObesity, color="Obesity")) +
  ggtitle("Cigarette Sales and Obesity (as proportion of last year) Over Time") +
  xlab("Year") + ylab("Proportion")

ggplot(bothByYear2, aes(x=sales.x, y=obesity.y)) + geom_line() +
  ggtitle("Obesity Rates Increase as Cigarette Sales Decrease") +
  xlab("Cigarette Sales per Capita") + ylab("Obesity Rate")

ggplot(CigData, aes(x=Year,y=price_cpi_2012)) +
  stat_summary(fun.y="mean",geom="line") +
  labs(
    x="Year",
    y="Cost per Pack ($)",
    title="Cost per Cigarette Pack in 2012 Real Dollars"
  ) + theme_bw() +
  scale_x_continuous(breaks=seq(1970, 2020, 5))

# other variables that affect obesity
  # income, race, gender, education

# adding ln to dataset
regressionData <- CigObesity %>%
  filter(Question=="Percent of adults aged 18 years and older who have obesity") %>%
  group_by(Year,state) %>%
  summarize(sales_per_capita = mean(sales_per_capita,na.rm=TRUE), 
            cost_per_pack = mean(cost_per_pack,na.rm=TRUE),
            tax_dollar=mean(tax_dollar,na.rm=TRUE), 
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

#stargazer(ols, iv, type="text", model.names=TRUE, object.names=TRUE)

# OLS and IV state and year fixed effects

stateFEols <- lm(ln_obesity ~ ln_sales + factor(state) - 1, data=regressionData)
yearFEols <- lm(ln_obesity ~ ln_sales + factor(Year) - 1, data=regressionData)
stateYearFEols <- lm(ln_obesity ~ ln_sales + factor(state) - 1 + factor(Year) -1, data=regressionData)

stateFEiv <- ivreg(ln_obesity ~ ln_sales + factor(state) - 1, data=regressionData)
yearFEiv <- ivreg(ln_obesity ~ ln_sales + factor(Year) - 1, data=regressionData)
stateYearFEiv <- ivreg(ln_obesity ~ ln_sales + factor(state) - 1 + factor(Year) -1, data=regressionData)

stargazer(ols, stateFEols, stateYearFEols, iv, stateFEiv, stateYearFEiv, type="text", model.names=TRUE, 
          object.names=TRUE,  omit=c("state","Year"),
          add.lines = c("State Fixed Effects & No & Yes & Yes & No & Yes & Yes \\\\",
                        "Year Fixed Effects & No & No & Yes & No & No & Yes \\\\")
)

## check the first stage
firstStage <- lm(ln_sales~ln_cost, data=regressionData)

## check the reduced form
reducedForm <- lm(ln_obesity~ln_cost, data=regressionData)

# First-stage and reduced form state and year fixed effects
#stateFEfirst <- lm(ln_sales~ln_cost, + factor(state) - 1, data=regressionData)
#stateYearFEfirst <- lm(ln_sales~ln_cost + factor(state) - 1 + factor(Year) -1, data=regressionData)

#stateFEreduced <- ivreg(ln_obesity~ln_cost, + factor(state) - 1, data=regressionData)
#stateYearFEreduced <- ivreg(ln_obesity~ln_cost, + factor(state) - 1 + factor(Year) -1, data=regressionData)

#stargazer(firstStage, stateFEfirst, stateYearFEfirst, reducedForm, stateFEreduced, stateYearFEreduced, type="text", model.names=TRUE, 
#          object.names=TRUE,  omit=c("state","Year"),
#          add.lines = c("State Fixed Effects & No & Yes & Yes & No & Yes & Yes \\\\",
#                        "Year Fixed Effects & No & No & Yes & No & No & Yes \\\\"))

stargazer(firstStage,reducedForm, type="text", model.names=TRUE, object.names=TRUE)


save.image("workspace.RData")






