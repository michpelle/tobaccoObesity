library(readr)
library(tidyverse)
library(AER)
library(stargazer)
library(cobalt)

CigData <- readRDS("~/Desktop/ECON 470/tobaccoLaptop/TaxBurden_Data.rds")
ObesityPercentOver18 <- read_csv("~/Desktop/ECON470FINAL/ObesityPercentOver18.csv")

summary(TaxBurden_Data)

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

CigObesity <- inner_join(CigData,ObesityPercentOver18, by=c("Year"="YearStart","state"="LocationDesc"))

# other variables that affect obesity
  # income, race, gender, education

# CONVERTING TO 2012 REAL DOLLARS
CigObesity<- CigObesity %>% mutate(cpiTwelve = NA)

for(n in 1:nrow(CigObesity)){
  for(m in 1:nrow(CigObesity)){
    if(CigObesity[m,]$state==CigObesity[n,]$state){
      if(CigObesity[n,]$Year==2012){
        #q3v2[n,]$cpiTwelve <- q3v2[n,]$price_cpi
        CigObesity[m,]$cpiTwelve <- CigObesity[n,]$price_cpi
      }
    }
  }
}

CigObesity <- CigObesity %>%
  mutate(real2012tax_dollar = tax_dollar*(price_cpi/cpiTwelve)) %>%
  mutate(real2012cost_per_pack = cost_per_pack*(price_cpi/cpiTwelve))

# adding ln to dataset
regressionData <- CigObesity %>%
  filter(Question=="Percent of adults aged 18 years and older who have obesity") %>%
  mutate(ln_obesity = log(Data_Value)) %>%
  mutate(ln_sales = log(sales_per_capita))

# simple regression to see relationship between cigarette sales and obesity
regress <- lm(ln_obesity ~ ln_sales, data=regressionData)
summary(regress)

# CHOOSING INSTRUMENT
# first stage for prices on sales
lm(formula = sales_per_capita ~ price_per_pack, data = regressionData)

# first stage for taxes on sales
lm(formula = sales_per_capita ~ tax_dollar, data = regressionData)

























# regress log obesity on log sales to estimate association between cigarette sales and obesity
regress <- lm(ln_obesity ~ ln_sales, data=regressionData)
summary(regress)

# regress log obesity on log sales using the total (federal and state) cigarette tax (in dollars) as an instrument for log sales. Interpret your results and compare your estimates to those without an instrument. Are they different? If so, why?

# iv <- ivreg(formula = ln_obesity ~ ln_sales | price OR tax 2012 dollars? , data = CigObesity)
iv <- ivreg(formula = ln_obesity ~ ln_sales | cost_per_pack , data = regressionData)
summary(iv)

ols <- lm(y~d, data=regressionData)
iv <- ivreg(y ~ d | z, data=regressionData)

## check the first stage
summary(lm(d~z, data=regressionData))

## check the reduced form
summary(lm(y~z, data=regressionData))

## in two stages
step1 <- lm(d ~ z, data=regressionData)
d.hat <- predict(step1)
step2 <- lm(y ~ d.hat, data=regressionData)
summary(step2)

# first stage and reduced-form results from the instrument
# first stage
firstStage <- lm(formula = ln_sales ~ tax_dollar, data = regressionData)
FirstStage <- summary(firstStage)
FirstStage

#reduced form
reducedForm <- lm(formula = ln_sales ~ tax_dollar, data=regressionData)
ReducedForm <- summary(reducedForm)

ivStargazer <- stargazer(firstStage,reducedForm, type="text", title="First Stage and Reduced Form", model.names=TRUE, object.names=TRUE)
