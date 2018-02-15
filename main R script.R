#working sirectory office:
setwd("U:/FDRS/main data files")

#working directory home: 
#setwd ("D:/Work/IFRC/FDRS2/main data files")


#install package for loading excel files:
#install.packages("xlsx", dependencies=TRUE)
library("xlsx")

#-------loading data files--------------

countriesAndZones <- read.xlsx("countries and zones.xlsx",1)
currencyRates <- read.xlsx("currency rates.xlsx",1, as.data.frame=TRUE, stringsAsFactors = FALSE)
disasters <- read.xlsx("disaster data.xlsx",1, as.data.frame=TRUE, stringsAsFactors = FALSE)
socioEconomic <- read.xlsx("economic demographic and social indicators.xlsx",1, as.data.frame=TRUE, stringsAsFactors = FALSE)
fdrs <- read.xlsx("Main data set_FDRS KPI 20140109.xlsx",1, as.data.frame=TRUE, stringsAsFactors = FALSE)
countryCodes <- read.xlsx("NSO_CountryCodes_ISOalpha2.xlsx",1)
countryIncomeGroups <- read.xlsx("World Bank country classifications.xlsx",1)
WorldGivingVolunteeringData <- read.xlsx("volunteering index data.xlsx",1)

#----processing disasters data----------------------

#delete superfluous country column in disasters data
disasters$NA. <- NULL
#convert value variables to numeric
disasters[, c(4:15)] <- sapply(disasters[, c(4:15)], as.numeric)

#install package for reshaping data
#install.packages("reshape", dependencies = TRUE)
library("reshape")

disastersMo <- melt(disasters, id=(c("country.name", "country.code"  ,"country.code..including.overseas.territories...use.sum.to.aggregate.")))
disastersMo$country.name <-NULL
disastersMo$country.code <-NULL
disastersMo[, 3] <- sapply(disastersMo[, 3], as.numeric)

disastersCa <- as.data.frame(cast(disastersMo, country.code..including.overseas.territories...use.sum.to.aggregate. ~ variable,value="value", fun.aggregate=sum))

#processing fdrs data
#convert value variables to numeric
fdrs[, c(7:16,18:21,35:61)] <- sapply(fdrs[, c(7:16,18:21,35:61 )], as.numeric)


#processing of currency data
currencyRates[, c(3:4)] <- sapply(currencyRates[, c(3:4)], as.numeric)

#processing country income groups data
countryIncomeGroups$NA. <-NULL

#processing socio economic data
socioEconomic[, c(6:13)] <- sapply(socioEconomic[, c(6:13)], as.numeric)


#---merge files-------------
allData <- merge(fdrs, countriesAndZones, by="KPI_DON_Code", all.x=TRUE)
allData <- merge(allData, currencyRates, by="KPI_DON_Code", all.x=TRUE)
allData <- merge(allData, countryIncomeGroups, by.x="ISO.country.code", by.y="ISO2.country.code", all.x=TRUE)
allData <- merge(allData, socioEconomic, by="KPI_DON_Code", all.x=TRUE)
allData <- merge(allData, disastersCa, by.x="ISO.country.code", by.y="country.code..including.overseas.territories...use.sum.to.aggregate.", all.x=TRUE)
allData <- merge (allData, WorldGivingVolunteeringData, by.x="ISO.country.code", by.y="iso2.country.code", all.x=TRUE)

######### -------------create calculated variables-------------
#financial in CHF
allData$expenditureInCHF <- with(allData, KPI_expenditureLC*Exchange.rate.end.of.year.2012)
allData$incomeInCHF <- with(allData, KPI_IncomeLC*Exchange.rate.end.of.year.2012)
#in relation to GDP
allData$incomeInCHFinRelationToGDP <- with(allData,incomeInCHF/(GDP.per.capita..current.US..*Population..Total.))
allData$incomeInCHFinRelationToGDPperCap <- with(allData,incomeInCHF/GDP.per.capita..current.US..)
allData$expenditureInCHFinRelationToGDP <- with(allData,expenditureInCHF/(GDP.per.capita..current.US..*Population..Total.))
allData$expenditureInCHFinRelationToGDPperCap <- with(allData,expenditureInCHF/GDP.per.capita..current.US..)

#disasters 2012 vs previous decade
allData$reportedKilledIn2012vsPreviousdecade <- with(allData, Total..number.of.people.reported.killed..2012./Total..number.of.people.reported.killed..1992.2001.)
allData$reportedAffectedIn2012vsPreviousdecade <- with(allData, Total..number.of.people.reported.affected..2012./Total..number.of.people.reported.affected..1992.2001.)


# per capita variables
allData$KPI_noPeopleVolunteeringPerCap <- with(allData, KPI_noPeopleVolunteering/Population..Total.)
allData$KPI_noPaidStaffPerCap <- with(allData,KPI_noPaidStaff/Population..Total.)
allData$KPI_noPeopleDonatingBloodPerCap <- with(allData,KPI_noPeopleDonatingBlood/Population..Total.)
allData$KPI_noLocalUnitsPerCap <- with(allData,KPI_noLocalUnits/Population..Total.)
allData$KPI_noPeopleReachedDisasterPerCap <- with(allData,KPI_noPeopleReachedDisaster/Population..Total.)
allData$KPI_noPeopleReachedAllServicesPerCap <- with(allData,KPI_noPeopleReachedAllServices/Population..Total.)
allData$KPI_noPeopleCoveredPreparednessPerCap <- with(allData,KPI_noPeopleCoveredPreparedness/Population..Total.)

allData$ReportedKilled1992to2001PerCap <- with(allData,Total..number.of.people.reported.killed..1992.2001./Population..Total.)
allData$ReportedAffected1992to2001PerCap <- with(allData,Total..number.of.people.reported.affected..1992.2001./Population..Total.)
allData$ReportedKilled2002to2011PerCap <- with(allData,Total..number.of.people.reported.killed..2002.to.2011./Population..Total.)
allData$ReportedAffected2002to2011PerCap <- with(allData,Total..number.of.people.reported.affected..2002.to.2011./Population..Total.)
allData$ReportedKilled2012PerCap <- with(allData,Total..number.of.people.reported.killed..2012./Population..Total.)
allData$ReportedAffected2002to2011PerCap <- with(allData,Total..number.of.people.reported.affected..2012./Population..Total.)

allData$incomeInCHFPerCap <- with(allData,incomeInCHF/Population..Total.)
allData$expenditureInCHFPerCap <- with(allData,expenditureInCHF/Population..Total.)


#logs

allData$KPI_noPeopleVolunteeringLog <- with(allData, log(KPI_noPeopleVolunteering+1))
allData$KPI_noPeopleVolunteeringPerCapLog <- with(allData, log(KPI_noPeopleVolunteeringPerCap+1))

allData$KPI_noPaidStaffLog <- with(allData,log(KPI_noPaidStaff+1))
allData$KPI_noPeopleDonatingBloodLog <- with(allData,log(KPI_noPeopleDonatingBlood+1))
allData$KPI_noLocalUnitsLog <- with(allData,log(KPI_noLocalUnits+1))
allData$KPI_noPeopleReachedDisasterLog <- with(allData,log(KPI_noPeopleReachedDisaster+1))
allData$KPI_noPeopleReachedAllServicesLog <- with(allData,log(KPI_noPeopleReachedAllServices+1))
allData$KPI_noPeopleCoveredPreparednessLog <- with(allData,log(KPI_noPeopleCoveredPreparedness+1))

allData$expenditureInCHFLog <- with(allData,log(expenditureInCHF+1))
allData$incomeInCHFLog <- with(allData,log(incomeInCHF+1))

allData$Total..number.of.people.reported.killed..1992.2001.Log <- with(allData,log(Total..number.of.people.reported.killed..1992.2001.+1))
allData$Total..number.of.people.reported.affected..1992.2001.Log <- with(allData,log(Total..number.of.people.reported.affected..1992.2001.+1))
allData$Total..number.of.people.reported.killed..2002.to.2011.Log <- with(allData,log(Total..number.of.people.reported.killed..2002.to.2011.+1))
allData$Total..number.of.people.reported.affected..2002.to.2011.Log <- with(allData,log(Total..number.of.people.reported.affected..2002.to.2011.+1))
allData$Total..number.of.people.reported.killed..2012.Log <- with(allData,log(Total..number.of.people.reported.killed..2012.+1))
allData$Total..number.of.people.reported.affected..2012.Log <- with(allData,log(Total..number.of.people.reported.affected..2012.+1))
  
allData$Total..number.of.people.reported.killed..2011.Log <- with(allData,log(Total..number.of.people.reported.killed..2011.+1))
allData$Total..number.of.people.reported.affected..2011.Log <- with(allData,log(Total..number.of.people.reported.affected..2011.+1))
  

allData$reportedKilledIn2012vsPreviousdecadeLog <- with(allData, log(reportedKilledIn2012vsPreviousdecade+1))
allData$reportedAffectedIn2012vsPreviousdecadeLog <- with(allData, log(reportedAffectedIn2012vsPreviousdecade+1))

allData$incomeInCHFPerCapLog <- with(allData,log(1+incomeInCHFPerCap))
allData$expenditureInCHFPerCapLog <- with(allData,log(1+expenditureInCHFPerCap))

allData$PopulationLog <- with(allData,log(Population..Total.+1))
allData$GDPperCapLog <- with(allData,log(GDP.per.capita..current.US..+1))
allData$LifeExpectancyLog <- with(allData,log(Life.expectancy.at.birth..total..years...2010.data.+1))
allData$MortalityLog <- with(allData,log(Mortality.rate..infant..per.1.000.live.births.+1))
allData$UrbanizationLog <- with(allData,log(Urban.population....of.total.+1))
allData$HDILog <- with(allData,log(HDI+1))
allData$LiteracyLog <- with(allData,log(Literacy+1))
allData$GIILog <- with(allData,log(Gender.Inequality.Index+1))


####volunteering data

## percentages - men - women
allData$VolunteersFractionMen <- with(allData,KPI_noPeopleVolunteeringM/(KPI_noPeopleVolunteeringM+KPI_noPeopleVolunteeringF))
allData$VolunteersFractionWomen <- with(allData,KPI_noPeopleVolunteeringF/(KPI_noPeopleVolunteeringM+KPI_noPeopleVolunteeringF))



## Z scores
allData$PercentVolunteeringZscores <- with(allData,scale(percent.volunteering, center = TRUE, scale = TRUE))
allData$KPI_noPeopleVolunteeringPerCapZscores <- with(allData,scale(KPI_noPeopleVolunteeringPerCap, center = TRUE, scale = TRUE))

###-RCRC volunteers vs total

allData$totalNoVolunteersCountry <- with(allData,percent.volunteering*Population..Total.)
allData$RCRCvolunteersFractionOfTotal <- with(allData,KPI_noPeopleVolunteering/totalNoVolunteersCountry)

###volunteers to staf ratio
allData$VolunteersToStaff <- with(allData,KPI_noPeopleVolunteering/KPI_noPaidStaff)
allData$VolunteersToStaffLog <- with(allData,log(VolunteersToStaff+1))

allData$VolunteersToStaffForDisplay <- with(allData, ifelse(VolunteersToStaff>0, paste(round(VolunteersToStaff,digits=2),":1"), ifelse(VolunteersToStaff>0, paste("1:",round(1/VolunteersToStaff,digits=2)), 0)))



)################# -------------save data as excel files------------------

write.xlsx(allData, file="allDataMergedInR.xlsx")

#reduce no of variables
allDataMultivariate <- allData[c(1,2,3,8,11,14,17,19,20,36,41,61,66,70,75,76,77,78,79,80,82,83, 96, 97, 98:ncol(allData))]
write.xlsx(allDataMultivariate, file="allDataMultivariate.xlsx")
sevenKPIs <- allData[c(2,8,11,14,17,96,97,36,41,96:ncol(allData))]
write.xlsx(sevenKPIs, file="allDataSevenKPIs.xlsx")

################----------data output-----------

#####correlations


#cor(allData[sapply(allData, is.numeric)])
allNumericData <- allData[sapply(allData, is.numeric)]
#install.packages("corrgram")
library("corrgram")
#corrgram(allNumericData)
#corrgram(allNumericData[3-12,15-42,67-68])

corrgram(sevenKPIs)

allNumericKPIperCap <- sevenKPIs[c(56,57, 16:22)]
allNumericKPIperCap <- allNumericKPIperCap[sapply(allNumericKPIperCap, is.numeric)]
corrgram(allNumericKPIperCaporGDP, order=TRUE)
#library("Hmisc")
cor(allNumericKPIperCap, use="complete.obs")
pairs(allNumericKPIperCap, use="complete.obs")  
      
#histogramms
install.packages("Hmisc")
library("Hmisc")


hist(allDataMultivariate[8:15])


### regresions



### volunteers as output, testing different input KPIS


#volunteers vs expenditure


RegNoOfVolunteers1 <- 
  lm(KPI_noPeopleVolunteeringLog~expenditureInCHFLog, data=sevenKPIs)

RegNoOfVolunteers2 = update(RegNoOfVolunteers1, .~.+GDPperCapLog+PopulationLog)

RegNoOfVolunteers3 = update(RegNoOfVolunteers1, .~.+KPI_noPaidStaffLog+KPI_noLocalUnitsLog+GDPperCapLog+PopulationLog)


mtable123 <- mtable("Model 1"=RegNoOfVolunteers1,"Model 2"=RegNoOfVolunteers2,"Model 3"=RegNoOfVolunteers3, summary.stats=c("sigma","R-squared","F","p","N"))
stargazer("Model 1"=RegNoOfVolunteers1,"Model 2"=RegNoOfVolunteers2,"Model 3"=RegNoOfVolunteers3, type="text")

mtable123

#volunteers vs paid staff


RegNoOfVolunteers4 <- lm(KPI_noPeopleVolunteeringLog~+KPI_noPaidStaffLog, data=sevenKPIs)
RegNoOfVolunteers5 <- update(RegNoOfVolunteers4, .~.+GDPperCapLog+PopulationLog)
RegNoOfVolunteers6 <- update(RegNoOfVolunteers5, .~.+expenditureInCHFLog+KPI_noLocalUnitsLog+GDPperCapLog+PopulationLog)

mtable("Model 1"=RegNoOfVolunteers4,"Model 2"=RegNoOfVolunteers5,"Model 3"=RegNoOfVolunteers6, summary.stats=TRUE)





#### volunteers to staff ratio

RegVolStaffRatio1 <- lm(VolunteersToStaffLog~expenditureInCHFLog, data=allData)

RegVolStaffRatio2 <- update(RegVolStaffRatio1, .~.+KPI_noLocalUnitsLog+GDPperCapLog+PopulationLog)
RegVolStaffRatio3 <- update(RegVolStaffRatio1, .~.+KPI_noLocalUnitsLog+PopulationLog+HDI)
mtable("Model 1"=RegVolStaffRatio1,"Model 2"=RegVolStaffRatio2,"Model 3"=RegVolStaffRatio3, summary.stats=TRUE)

#testing for effect of disasters on volunteers

cor(allData$Total..number.of.people.reported.affected..2011.Log,allData$Total..number.of.people.reported.killed..2011.Log, use="complete.obs")
RegModel.16 <- 
  lm(KPI_noPeopleVolunteeringLog~expenditureInCHFLog+KPI_noPaidStaffLog+KPI_noLocalUnitsLog+KPI_noPeopleDonatingBloodLog+KPI_noPeopleReachedAllServicesLog+KPI_noPeopleReachedDisasterLog+KPI_noPeopleCoveredPreparednessLog+GDPperCapLog+PopulationLog+Total..number.of.people.reported.affected..2011.Log, data=sevenKPIs)
summary(RegModel.16)

RegModel.17 <- 
  lm(KPI_noPeopleVolunteeringLog~expenditureInCHFLog+KPI_noPaidStaffLog+KPI_noLocalUnitsLog+KPI_noPeopleReachedAllServicesLog+KPI_noPeopleReachedDisasterLog+KPI_noPeopleCoveredPreparednessLog+GDPperCapLog+PopulationLog+Total..number.of.people.reported.affected..2011.Log, data=sevenKPIs)
summary(RegModel.17)


RegModel.19 <- 
  lm(KPI_noPeopleVolunteeringLog~expenditureInCHFLog+KPI_noPaidStaffLog+KPI_noLocalUnitsLog+GDPperCapLog+PopulationLog+Total..number.of.people.reported.affected..2012.Log, data=sevenKPIs)
summary(RegModel.19)

RegModel.21 <- 
  lm(KPI_noPeopleVolunteeringLog~expenditureInCHFLog+KPI_noPaidStaffLog+KPI_noLocalUnitsLog+GDPperCapLog+PopulationLog+Total..number.of.people.reported.killed..2012.Log, data=sevenKPIs)
summary(RegModel.21)

RegModel.24 <- 
  lm(KPI_noPeopleVolunteeringLog~expenditureInCHFLog+KPI_noPaidStaffLog+KPI_noLocalUnitsLog+GDPperCapLog+PopulationLog+Total..number.of.people.reported.affected..2002.to.2011.Log+Total..number.of.people.reported.affected..2012.Log, data=sevenKPIs)
summary(RegModel.24)

####several dependent variables

RegModel<-lm(cbind(KPI_noPeopleDonatingBloodLog,KPI_noPeopleReachedAllServicesLog,KPI_noPeopleReachedDisasterLog,KPI_noPeopleCoveredPreparednessLog)~KPI_noPeopleVolunteeringLog+KPI_noLocalUnitsLog+expenditureInCHFLog+KPI_noPaidStaffLog+GDPperCapLog+PopulationLog, data=sevenKPIsComplete)
summary(
Manova(RegModel)

View(RegModel)
class(RegModel)
Anova(RegModel)
View(RegModel$lm)

sevenKPIsComplete <- sevenKPIs[complete.cases(sevenKPIs),]

