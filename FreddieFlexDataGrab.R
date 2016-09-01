library(rgdal)
library(sp)
library(readr)
library(dplyr)
library(tidyr)
library(lubridate)
library(stringr)
library(readxl)
library(zoo)
library(reshape2)
library(xts)


##load the cbsa shapefile
metros500k<-readOGR(".", "cb_2015_us_cbsa_500k", verbose = FALSE)

## 1) remove the micros because we are building a map of metros only
## 2) remove Puerto Rico because freddie's data doesn't include it
## 3) alphabetize so can synch with the housing price data
Metros_Only_500k <- subset(metros500k, metros500k$LSAD %in% c("M1"))
Metros_Only_500k<-Metros_Only_500k[!grepl(", PR", Metros_Only_500k$NAME),]
Metros_Only_500k <-Metros_Only_500k[order(Metros_Only_500k$NAME),]

##get the metro county data sheet 1, remove first 4 rows and last blank rows
msas_A_L <- read_excel("~/FreddieMac/metros.xls", 
                       sheet = "MSA Indices A-L", skip = 4)
colnames(msas_A_L)=msas_A_L[1,]
msas_A_L=msas_A_L[c(-1, -495:-551),]

msas_new_AL <- read_excel("msas_new.xls", 
                       sheet = "MSA Indices A-L", skip = 4)
colnames(msas_new_AL)=msas_new_AL[1,]
msas_new_AL=msas_new_AL[c(-1, -500:-555),]

msas_new_MZ <- read_excel("msas_new.xls", 
                          sheet = "MSA Indices M-Z", skip = 4)
colnames(msas_new_MZ)=msas_new_MZ[1,]
msas_new_MZ=msas_new_MZ[c(-1, -500:-516),-1]

##get the metro county data sheet 2, remove first 4 rows, blank last AND
##remove the date column
msas_M_Z <- read_excel("~/FreddieMac/metros.xls", 
                       sheet = "MSA Indices M-Z", skip = 4)
colnames(msas_M_Z)=msas_M_Z[1,]
msas_M_Z=msas_M_Z[c(-1, -495:-512),-1]

##combine metros areas into one dataframe and conver to numeric
metros_All <- cbind(msas_A_L,msas_M_Z)
cols = c(2:382)    
metros_All[,cols] = apply(metros_All[,cols], 2, function(x) as.numeric(as.character(x)))

##get rid of the cycle column, make it into a date and rename it 'date'
metros_All$Month<-seq(mdy('01/01/1975'),mdy('02/01/2016'),by='months')
colnames(metros_All)[1] <- "date"

metros_gathered<- metros_All %>% 
  gather("metro", "hpi", -date) %>% 
  separate(date, into = c("year", "month"), sep = '-', convert = TRUE, remove = FALSE) %>% 
  separate(metro, into = c("metro", "state"), remove = FALSE)

##change metros_All column names to just first word of metro and state abbreviation
##this is so can match with the map clicking
##Need to think when this should be done, it affects the separating into metro and state
StateAbb<-word(colnames(metros_All)[2:382], -1, sep = ", ")
metroFirstWord<-word(colnames(metros_All)[2:382], 1, sep = "-")
metroFirstWord<-word(metroFirstWord, 1, sep = ",")
colnames(metros_All)[2:382]<-paste(metroFirstWord,StateAbb,sep=" ")

metros_01_08<- metros_All %>%  
  gather("metro", "maxHPI", -date) %>% 
  filter(date > "2000-01-01" & date <"2008-06-01") %>%
  group_by(metro) %>% 
  top_n(1, maxHPI) 
  
Metros_Only_500k$CurrentDiff <- round(MetrosHpa$`2016-01-01` - metros_01_08$maxHPI, digits =2)


##add HPA to shapefile
Metros_Only_500k$hpaTest <-  
  
  metros_All %>%  
  filter(date == ymd("2016-01-01") | date == ymd("2015-01-01")) %>%
  gather(metro, value, -date) %>%
  spread(date, value) %>% 
  mutate(hpa = (((1+(.[[3]]-.[[2]])/.[[2]])^1)-1)*100) %>%
  transmute(hpa = round(hpa, digits =2)) 

Metros_Only_500k$hpa <- round(MetrosHpa$hpa, digits = 2)



##misc
metros_MinPostCrash <- metros_melted %>% group_by(metro) %>% 
  filter(date > "2008-01-01" & date <"2016-02-01") %>%
  top_n(-1, hpi)

df$maxDiffPre2008 <- df$Year2 - metros_MaxHpi$hpi
df$troughDiffPost2008 <- df$Year2 - metros_MinPostCrash$hpi

Metros_Only_500k$percentChange08Peak <- round(df$maxDiffPre2008, digits = 2)
Metros_Only_500k$percentChange08_16Trough <- round(df$troughDiffPost2008, digits = 2)



###################STATES########################

##get state shapefile
states500k<-readOGR(".", "cb_2015_us_state_500k", verbose = FALSE)
removeTerrs<- c("Islands", "Puerto", "Samoa", "Guam")
states500k<-states500k[!grepl(paste(removeTerrs, collapse = '|'), states500k$NAME),]
colnames(states500k@data)[5] <- "STATEABB"
states500k <-states500k[order(states500k$STATEABB),]

##get the state data to start playing with
states   <- read_excel("~/FreddieMac/states.xls", 
                       skip = 4)

##rename the columns by state abbreviation and remove the unneeded rows at the bottom
colnames(states)<-states[1,]
states<-states[c(-1,-497:-512),-53:-54]

##replace the month column with a better named/formatted date column
colnames(states)[1] <- "date"
states$date<-seq(mdy('01/01/1975'),mdy('03/01/2016'),by='months')

##make sure the columns are numeric. Why necessary? read_excel imported the data as characters
##because of the characters in the original excel file
states[,c(2:52)] = apply(states[,c(2:52)], 2, function(x) as.numeric(x))

##another way to accomplish the same thing turn to numeric first, then add date
##states <- lapply(states[,2:54], as.numeric)
##states$date<-seq(mdy('01/01/1975'),mdy(names <- c("United", "DC")

##remove the last two columns, because those united states adujusted. 
##states_DC<-states[,-53:-54]

StatesHpa <-states %>%  
  filter(date == ymd("2016-03-01") | date == ymd("2015-03-01")) %>%
  gather(state,value, -date) %>%
  spread(date,value) %>% 
  mutate(hpa= (((1+(.[[3]]-.[[2]])/.[[2]])^1)-1)*100) %>% 
  mutate(hpa = round(hpa, digits =2))

states500k$hpa <- StatesHpa$hpa

states_00_08<- states %>%  
  gather("state", "maxHPI", -date) %>% 
  filter(date > "2000-01-01" & date <"2008-06-01") %>%
  group_by(state) %>% 
  top_n(1, maxHPI) 

states500k$PeakDiff <- round(StatesHpa$`2016-03-01` - states_00_08$maxHPI, digits =2)


##create an xts object. why? Dygraphs needs one, that's why.
metros_All_xts <-xts(metros_All, order.by=as.Date(metros_All$date))
states_xts <-xts(states, order.by = as.Date(states$date))
### Save
save(metros_All_xts, Metros_Only_500k, states_xts, states500k, dfmetro, metros_All, file = 'SourceData.RDat')
