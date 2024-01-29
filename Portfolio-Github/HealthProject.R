#Import libraries
library(tidyverse)
library(readxl)
library(stringr) 
library(openxlsx)
library(writexl)


# Reading Data from original files. Used getwd() to find your directory

dirProject<-"/Users/andreslopezcontreras/Downloads/"
strBedsData<-"Req-95752-Beds-Table.xlsx"
strPopulationsData<- "Req-95752-Populations-Table.xlsx"
BedsTable <- read_excel(paste(dirProject,"Req-95752-Beds-Table.xlsx",sep=""),sheet = "Beds Data")
PopulationsTable <- read_excel(paste(dirProject,strPopulationsData,sep = ""),sheet = "Populations Data")

#Splitting LOCAL_HEALTH_AREA and HEALTH_AUTHORITY into the codes and areas
BedsTable[c("LOCAL_HEALTH_AREA_CODE","LOCAL_HEALTH_AREA1")]<- str_split_fixed(BedsTable$LOCAL_HEALTH_AREA," ",2)
BedsTable[c("HEALTH_AUTHORITY_CODE","HEALTH_AUTHORITY1")]<- str_split_fixed(BedsTable$HEALTH_AUTHORITY," ",2)

# Merging Population Data and Beds Data by the id LOCAL_HEALTH_AREA_CODE
BedsTable<-left_join(BedsTable, PopulationsTable, by = "LOCAL_HEALTH_AREA_CODE")

# Remove Unknown LHA, beds that are not allocated to a specific population
BedsTableMerged<-BedsTable %>% 
  filter(LOCAL_HEALTH_AREA1 != "Unknown LHA" )%>%
  group_by(HEALTH_AUTHORITY1,LOCAL_HEALTH_AREA1)%>%
  select(BEDS,POPULATION)

# Getting the number of beds by area, and the number of facilities by area, only for detailed analysis purposes.
BedsTableMerged<-BedsTableMerged%>%
  group_by(LOCAL_HEALTH_AREA1) %>%
  mutate(BEDS_BY_AREA= sum(BEDS, na.rm = TRUE),
         BEDS_PER_100KP_BY_AREA= (BEDS_BY_AREA*100000)/POPULATION,
         COUNT_OF_FACILITIES_BY_AREA=n())

# Calculating the population by authority region in a new df
pop_authority_df<-BedsTableMerged %>%
  group_by(HEALTH_AUTHORITY1)%>%
  distinct(POPULATION) %>%
  mutate(POPULATION_BY_AUTHORITY = sum(POPULATION,na.rm = TRUE))

# Merging the Tables to have the population by authority in the summarized table
BedsTableMerged<-left_join(BedsTableMerged, pop_authority_df, by = c("HEALTH_AUTHORITY1","POPULATION"))

# Calculating the number of beds per 100K people in each region. Adding it to the summarized table
BedsTableMerged<-BedsTableMerged%>%
  group_by(HEALTH_AUTHORITY1) %>%
  mutate(BEDS_BY_AUTHORITY = sum(BEDS, na.rm = TRUE),
         BEDS_PER_100KP_BY_AUTHORITY= (BEDS_BY_AUTHORITY*100000)/POPULATION_BY_AUTHORITY,
         COUNT_OF_AREAS=n())

# Creating a summarized table by health area
Summarized_by_area<- BedsTableMerged %>%
  distinct(LOCAL_HEALTH_AREA1,COUNT_OF_FACILITIES_BY_AREA,POPULATION,BEDS_BY_AREA,BEDS_PER_100KP_BY_AREA)%>%
  select(LOCAL_HEALTH_AREA1,COUNT_OF_FACILITIES_BY_AREA,POPULATION,BEDS_BY_AREA,BEDS_PER_100KP_BY_AREA)%>%
  rename("LOCAL_HEALTH_AREA"="LOCAL_HEALTH_AREA1")

# Getting a consolidated table of health authority regions with the total number of beds, population and beds per 100k people
Summarized_by_authority<- BedsTableMerged %>%
  distinct(HEALTH_AUTHORITY1,POPULATION_BY_AUTHORITY,BEDS_BY_AUTHORITY,BEDS_PER_100KP_BY_AUTHORITY)%>%
  rename("HEALTH_AUTHORITY"="HEALTH_AUTHORITY1")

# Creating a data frame summarizing the data for the province
BC_consolidated=data.frame(HEALTH_AUTHORITY= "PROVINCE OF B.C.",
           POPULATION_BY_AUTHORITY = sum(Summarized_by_authority$POPULATION_BY_AUTHORITY),
           BEDS_BY_AUTHORITY = sum(Summarized_by_authority$BEDS_BY_AUTHORITY),
           BEDS_PER_100KP_BY_AUTHORITY = sum(Summarized_by_authority$BEDS_BY_AUTHORITY)*100000/sum(Summarized_by_authority$POPULATION_BY_AUTHORITY))

# Merging the health authority regions with summarized data for the province
Summarized_by_authority<- rbind(Summarized_by_authority,BC_consolidated)


# Create excel file with the merge data, the summarized data per area and the beds per 100k people per Authority
datasets <- list('MergedData' = BedsTable, 'DatabyArea' = Summarized_by_area, 'DatabyAuthority' = Summarized_by_authority)
write_xlsx(datasets,paste(dirProject,"HealthProject.xlsx",sep=""),col_names = TRUE,)
  
