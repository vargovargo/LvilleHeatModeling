library(dplyr)
library(tidyr)

# you'll need to set this path to whatever folder you are using for the data that this script calls
setwd("~/Box Sync/work/Louisville/Dallas/DallasHealthAnalysis/")

#read in all the months for a particular temperature metric and combine them into a single file

############
#functions
############

# this function will read in the mortality data from CDC and parse it
readWONDER <- function(file){
  
  foo<-readLines(file)
  foo<-foo[-1]
  
  if(length(grep("---",foo))>0){
    foo<-foo[-c(grep("---",foo):length(foo))]
  }
  n <- as.numeric(length(foo))
  
  wonder <- read.table(file, sep="\t", as.is = T, nrows = n, skip=1)
  header <- read.table(file, sep="\t", as.is = T, nrows = 1)
  colnames(wonder) <- unlist(header)
  
  wonder$Deaths <- ifelse(wonder$Population== "Suppressed" | wonder$Population== "Missing" , 0, wonder$Deaths)
  
  return(wonder)
  
}

#################################################
########   Population Mortality import   ########
#################################################
# read in CDC wonder 
# the data it is reading in here is each county in the US and the total mortality (non-accidental, same as Gasparrini uses) for 2015
# the metadata for the actual WONDER query can be found at the end of the raw text file
# here is the website for the query
#https://wonder.cdc.gov/controller/saved/D76/D10F619

DallasWonder <- readWONDER("./Dallas_county_nonAcc_2009-13.txt")[1:22,c(4,6,8,9,10)]
names(DallasWonder) <- c("Age","Gender","Deaths","Population","CrudeRate")
DallasWonder$CrudeRate <- as.numeric(DallasWonder$CrudeRate)
DallasWonder$Population <- as.numeric(DallasWonder$Population)

#############################################
########  WORK WITH CLIMATE DATA  ###########
#############################################

# list your scenario names here and make sure you use them in the file names for the respective temperature data
# The AG health response function uses daily mean (actual, not apparent, temps)

pop <- read.csv("./Gridded_pop_Dallas_only.csv", header=T)[,c(1,3:22)] %>%  gather(., variable, value, -FID)

WONDERageKey <-
  c(
    "1-4 years",
    "1-4 years",
    "15-24 years",
    "15-24 years",
    "25-34 years",
    "25-34 years",
    "35-44 years",
    "35-44 years",
    "45-54 years",
    "45-54 years",
    "5-14 years",
    "5-14 years",
    "55-64 years",
    "55-64 years",
    "65-74 years",
    "65-74 years",
    "75-84 years",
    "75-84 years",
    "85+ years",
    "85+ years"
  )
names(WONDERageKey) <- unique(pop$variable)
pop$Age <- WONDERageKey[as.character(pop$variable)]

WONDERsexKey <-
  c(
    "Female",
    "Male",
    "Female",
    "Male",
    "Female",
    "Male",
    "Female",
    "Male",
    "Female",
    "Male",
    "Female",
    "Male",
    "Female",
    "Male",
    "Female",
    "Male",
    "Female",
    "Male",
    "Female",
    "Male"
  )
names(WONDERsexKey) <- unique(pop$variable)
pop$Gender <- WONDERsexKey[as.character(pop$variable)]

pop <- pop %>% inner_join(DallasWonder, by=c("Age","Gender")) %>% inner_join(read.csv("./DallasGrid.csv", header=T), by = "FID")



##################################
########Loop thru scenarios#######
##################################

# list your parameters here and make sure you use them in the file names for the respective temperature data
scenarios <- c("Base","Albedo","Combined","Green", "TreeLoss", "noUHI")
HWdays <- c(59,53,50,54,61,49)  
names(HWdays) <- scenarios

noUHItemp <- 70.532

# set beta from Medina-Ramon http://oem.bmj.com/content/64/12/827.short
beta <- 0.0042908

# HW increase from Anderson paper http://search.proquest.com/docview/853759302?pq-origsite=gscholar
anderson <- 0.0374

# set threshold temp for heat-related deaths
threshold <- 17

# establish a place to store our results
all_resultsCULE <- data.frame()
for(scenario in scenarios){ 

  # read in temps
  temps <- read.csv(paste("./Dallas_Temperature_",scenario,".csv", sep=""), header=T)[,c(2,3)]
  names(temps) <- c("FID", "minF")
  temps$minC <- (temps$minF - 32) * 5/9 
  temps$cut <- ifelse(temps$minC < threshold, temps$minC, threshold)
  
  # join the county moratlity info and the temps
  # this is the same formula used in BenMap http://pubs.acs.org/doi/abs/10.1021/es102820y
  # and in the PlosOne paper from CULE http://journals.plos.org/plosone/article?id=10.1371/journal.pone.0100852
  # please verify the calc beta set above, outside loop
 
  est <- pop %>% full_join(temps, by="FID") %>%mutate(., delta = minC - cut) %>%
    mutate(., Medina_WSdeaths = ((CrudeRate/100000)*(153/365))*(exp(beta*delta)-1)*value) %>%
    mutate(., HW_WSdeaths = (CrudeRate/100000)/365*anderson*HWdays[scenario]) %>%
    mutate(., Scenario = scenario)
  
  est$Age <- factor(est$Age, levels = c( "1-4 years", "5-14 years", "15-24 years", "25-34 years", "35-44 years", "45-54 years", "55-64 years","65-74 years", "75-84 years", "85+ years" ))

  # add the results from the current scenario to all the results from all scenarios
  all_resultsCULE <- rbind(all_resultsCULE, est)

}


#####################################
############ SAVE results ##########
#####################################

#total deaths by scenario
all_resultsCULE %>% group_by(., Scenario) %>% summarise(., Medina_WSdeaths = sum(Medina_WSdeaths, na.rm=T), HW_WSdeaths = sum(HW_WSdeaths, na.rm=T), Total_WSdeaths = sum(Medina_WSdeaths + HW_WSdeaths, na.rm=T))  %>%
  write.csv(., "~/Desktop/totalDeathsByScenario.csv", row.names=F)

#total deaths by location
all_resultsCULE %>% group_by(., Scenario, FID, Lat, Lon) %>% summarise(., Medina_WSdeaths = sum(Medina_WSdeaths, na.rm=T), HW_WSdeaths = sum(HW_WSdeaths, na.rm=T), Total_WSdeaths = sum(Medina_WSdeaths + HW_WSdeaths, na.rm=T))  %>%
  write.csv(., "~/Desktop/totalDeathsByGridScenario.csv", row.names=F)

#total deaths by Age
all_resultsCULE %>% group_by(., Scenario, FID, Age) %>% summarise(., Medina_WSdeaths = sum(Medina_WSdeaths, na.rm=T), HW_WSdeaths = sum(HW_WSdeaths, na.rm=T), Total_WSdeaths = sum(Medina_WSdeaths + HW_WSdeaths, na.rm=T))  %>%
  write.csv(., "~/Desktop/totalDeathsByAgeScenario.csv", row.names=F)

#total deaths by Gender
all_resultsCULE %>% group_by(., Scenario, FID, Gender) %>% summarise(., Medina_WSdeaths = sum(Medina_WSdeaths, na.rm=T), HW_WSdeaths = sum(HW_WSdeaths, na.rm=T), Total_WSdeaths = sum(Medina_WSdeaths + HW_WSdeaths, na.rm=T))  %>%
  write.csv(., "~/Desktop/totalDeathsByGenderScenario.csv", row.names=F)


#####################################
############  plot results ##########
#####################################
library(ggplot2)

#total deaths by scenario
  all_resultsCULE %>% group_by(., Scenario) %>% summarise(., Medina_WSdeaths = sum(Medina_WSdeaths, na.rm=T), HW_WSdeaths = sum(HW_WSdeaths, na.rm=T), Total_WSdeaths = sum(Medina_WSdeaths + HW_WSdeaths, na.rm=T))  %>%
  ggplot(., aes(x=Scenario, y=Total_WSdeaths)) + geom_bar(stat = "identity")
  
#total deaths by location
  all_resultsCULE %>% group_by(., Scenario, FID, Lat, Lon) %>% summarise(., Medina_WSdeaths = sum(Medina_WSdeaths, na.rm=T), HW_WSdeaths = sum(HW_WSdeaths, na.rm=T), Total_WSdeaths = sum(Medina_WSdeaths + HW_WSdeaths, na.rm=T))  %>%
    ggplot(., aes(x=Lon, y=Lat, color=Total_WSdeaths)) + geom_point(stat = "identity", size=.4) + facet_wrap(~Scenario) + scale_colour_gradient(limits=c(0, .2), low="white", high="red")
  
  #total deaths by Age
  all_resultsCULE %>% group_by(., Scenario, FID, Age) %>% summarise(., Medina_WSdeaths = sum(Medina_WSdeaths, na.rm=T), HW_WSdeaths = sum(HW_WSdeaths, na.rm=T), Total_WSdeaths = sum(Medina_WSdeaths + HW_WSdeaths, na.rm=T))  %>%
    ggplot(., aes(x=Scenario, y=Total_WSdeaths, fill=factor(Age))) + geom_bar(stat = "identity", position="stack") 
  
  #total deaths by Gender
  all_resultsCULE %>% group_by(., Scenario, FID, Gender) %>% summarise(., Medina_WSdeaths = sum(Medina_WSdeaths, na.rm=T), HW_WSdeaths = sum(HW_WSdeaths, na.rm=T), Total_WSdeaths = sum(Medina_WSdeaths + HW_WSdeaths, na.rm=T))  %>%
    ggplot(., aes(x=Scenario, y=Total_WSdeaths, fill=factor(Gender))) + geom_bar(stat = "identity") 
