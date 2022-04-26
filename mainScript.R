library(readr)
library(tidyverse)
library(cobalt)

TaxBurden_Data <- readRDS("~/Desktop/ECON 470/tobaccoLaptop/TaxBurden_Data.rds")
ObesityPercentOver18 <- read_csv("~/Desktop/ECON470FINAL/ObesityPercentOver18.csv")

summary(TaxBurden_Data)

# SUMMARIES
# Cigarette Sales Over Time
cigSalesByYear <- TaxBurden_Data %>% group_by(Year) %>% summarise(sales=mean(sales_per_capita,na.rm=TRUE)) %>% mutate(obesity=NA)
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
