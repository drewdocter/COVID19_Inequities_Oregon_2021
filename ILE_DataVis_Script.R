#MPH ILE - Epi Summary and Inequities of COVID-19 in Oregon



# Saved Plot Theme for this Report ----------------------------------------

library(extrafont)
ILE.Theme <- theme_minimal() +
      theme(text = element_text(family = "Calibri"),
      axis.title = element_text(size=13, face = "bold"),
      axis.text = element_text(size=13, face = "bold"),
      legend.position = "right",
      legend.title = element_text(face = "bold", size = 13),
      legend.text = element_text(size = 12),
      panel.grid.minor = element_blank(),
      axis.line = element_line(color = "grey50"),
      plot.title = element_text(size=16, face = "bold"),
      plot.subtitle = element_text(size = 14),
      axis.ticks = element_line())


# Daily Incidence Analysis, Since February 2020 (Updated 5/15) -----------------

#Load in Incidence and Hospitalization Data
library(tidyverse)
epicurve <- read_csv("CaseCount_Hospitalized_Daily_5.13.csv")
# Source: Oregon's Epi Curve Summary Table'

#Format & Dataset
epicurve <- epicurve %>% select(`Hospitalization Status`, `Onset Date`, `Case Count`) %>% 
  rename(hosp_status = `Hospitalization Status`,
         date_onset = `Onset Date`,
         cases = `Case Count`)
library(lubridate) #To clean up and sort dates 
epicurve <- epicurve %>% 
  mutate(Date = lubridate::parse_date_time(date_onset, "mdy")) %>% 
  arrange(Date)
epicurve

#7-Day Rolling Average Incidence Subset
library(zoo)
epitrail <- epicurve %>% 
  group_by(Date) %>% #Condense Table by Date
  summarise(tot.case = sum(cases)) %>% #Created total cases column
  mutate(roll_avg = zoo::rollmean(tot.case, k = 7, fill = NA)) #Computed 7-Day Rolling Average
epitrail

#Plot 7-Day Rolling Average (Use in Final Report)
Inc.Roll.Plot <- ggplot(epitrail, aes(x=Date, y=tot.case)) + 
  geom_bar(stat="identity", color ="mediumseagreen", fill = "mediumseagreen") +
  geom_line(aes(Date, roll_avg), size=1.3) +
  labs(title = "Daily Incidence of COVID-19 in Oregon, since January 2020",
       subtitle = "Plotted with 7-Day Rolling Average",
       y = "Daily Case Count", x = NULL) +
  ILE.Theme +
  scale_y_continuous(expand = c(0,0))
  # scale_x_date(date_breaks = "1 month")
Inc.Roll.Plot #Use this in final version (Figure 1)

#7-Day Trailing Average Incidence Plot (Don't use on Final Report)
library(RcppRoll) #Load package to use roll_mean function, allowing to compute
#a 7 Day-trailing average.
epitrail <- epitrail %>% 
  mutate(trail_avg = roll_mean(epitrail$tot.case, n = 7, 
                               align = "right", fill = NA)) #Computed 7-Day Trailing Average
epitrail

Inc.Trail.Plot <- ggplot(epitrail, aes(x=Date, y=tot.case)) + 
  geom_bar(stat="identity", color ="palegreen3") +
  geom_line(aes(Date, trail_avg), size=1.5) +
  theme_minimal() +
  labs(title = "Daily Incidence of COVID-19 in Oregon since January 2020",
       subtitle = "Includes 7-Day Trailing Average",
       y = "Daily Case Count", x = NULL) +
  scale_y_continuous(expand = c(0,0)) +
  ILE.Theme
Inc.Trail.Plot #Don't use this in final report. Trailing average doesn't quite get 




# Daily Hospitalization Analysis (Updated 5/15)  -----------------------------------


#Plot of Only Hospitalizations & 7-Day Rolling Average (Include on Final Report)
hosp.only <- epicurve %>% 
  filter(hosp_status == "Hospitalized") %>% #Subset only includes hospitalized cases
  mutate(roll_avg = zoo::rollmean(cases, k = 7, fill = NA))
hosp.only

hosp.plot <- ggplot(hosp.only, aes(x=Date, y=cases)) + 
  geom_bar(stat="identity", color = "mediumseagreen", fill = "mediumseagreen") +
  geom_line(aes(Date, roll_avg), size=1.3) +
  labs(title = "Daily COVID-19 Hospitalizations in Oregon, since January 2020",
       subtitle = "Plotted with 7-Day Rolling Average",
       y = "Daily Hospitalizations", x = NULL) +
   ILE.Theme +
  scale_y_continuous(expand = c(0, 0))
hosp.plot


#Plot of Total Cases per Day, by Hospitalization Status (Use in Final Report, as supplement)
library(colorspace) #Better Color Scales
inc.plot.tot <- ggplot(epicurve, 
                       aes(x=Date, y=cases, 
                           fill=reorder(hosp_status, -cases))) + #Invert order of fill on plot.
  geom_bar(stat="identity") +
  labs(title = "Daily COVID-19 Cases in Oregon by Hospitalization Status, since January 2020",
       y = "Daily Case Count", fill = "Hospitalization Status", x = NULL) +
  ILE.Theme +
  scale_y_continuous(expand = c(0, 0)) +
  scale_fill_manual(values=c("lightskyblue3", "#E69F00", "mediumseagreen")) #Manual scale for this plot
  # scale_fill_discrete_qualitative("Harmonic")
inc.plot.tot


# Daily Testing Counts & Positivity, Since March 2020 ---------------------------------------

testing <- read_csv("TestingStatus_Daily.csv")
testing <- testing %>% rename(date_ELR = `ELR Date`,
         result = `Test Result`,
         count = `ELR Count`)
testing

#Clean and sort dates for testing counts.
require(lubridate) #To clean up and sort dates 
testing <- testing %>% 
  mutate(Date = lubridate::parse_date_time(date_ELR, "mdy")) %>% 
  arrange(Date)
testing

#Plot of Total Tests Per Day, Negative and Positive
testplot1 <- ggplot(testing, aes(x=Date, y=count, fill=result)) + 
  geom_bar(stat="identity") +
  theme_minimal() +
  labs(title = "Testing in Oregon, since March 2020",
       y = "Daily Test Counts", x = NULL) +
  scale_fill_manual(values = c("palegreen3", "red"))
testplot1


#Data Table Computing Positive Tests - Need to work on still
test.state <- read_csv("Testing_Statewide_Daily_5.13.csv")
test.state <- test.state %>% 
  mutate(Date = lubridate::parse_date_time(`ELR Date`, "mdy")) %>%
  mutate(pct.pos = ((`Positive Tests`/`Total Tests`)*100)) %>% 
  arrange(Date) %>% 
  mutate(roll_avg = zoo::rollmean(pct.pos, k = 7, fill = NA)) #Added 7-Day Rolling Average
test.state

#Plot of positivity, daily
PosTest.Plot <- test.state %>% 
  filter(!`Negative Tests` %in% 31) %>% #Trying to filter above this date due to outlier.
  ggplot(aes(x=Date, y=pct.pos)) + 
  geom_line(size=1.2, color = "palegreen3") +
  theme_minimal() +
  labs(title = "Daily COVID-19 Test Positivity Rate in Oregon since March 2020",
       subtitle = "Number of Positive Tests/Number of Total Tests",
       y = "Test Positivity Percentage (%)", x = NULL) +
  theme(axis.title = element_text(size=13, face = "bold"),
        axis.text = element_text(size=11, face = "bold"),
        legend.position = "none",
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        axis.line = element_line(color = "grey50"),
        plot.title = element_text(size=14, face = "bold"),
        plot.subtitle = element_text(size = 12)) +
  scale_y_continuous(expand = c(0, 0))
PosTest.Plot #Can include in Final Version, if appropriate

#Plot of Test Positivity, rolling average
PosAvg.Plot <- test.state %>% 
  # filter(`ELR Date` >= "3/30/20") %>% #Trying to filter above this date due to outlier.
  ggplot(aes(x=Date, y=roll_avg)) + 
  geom_line(size=1.2, color = "palegreen3") +
  theme_minimal() +
  labs(title = "COVID-19 Test Positivity Rate in Oregon since March 2020",
       subtitle = "Number of positive tests/number of total tests. 7-Day rolling average plotted.",
       y = "Test Positivity Percentage (%)", x = NULL) +
  theme(axis.title = element_text(size=13, face = "bold"),
        axis.text = element_text(size=11, face = "bold"),
        legend.position = "none",
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        axis.line = element_line(color = "grey50"),
        plot.title = element_text(size=14, face = "bold"),
        plot.subtitle = element_text(size = 12)) +
  scale_y_continuous(expand = c(0, 0))
PosAvg.Plot #Can include in Final Version, if appropriate

# Weekly Mortality, since March 2020 ------------------------------------

library(tidyverse)
mortality <- read_csv("Deaths_Hospitalization_Weekly.csv")

#Now, clean the data Intput
library(lubridate) #To clean up and sort dates 
mortality <- mortality %>% 
  mutate(Date = lubridate::parse_date_time(Week, "mdy")) %>% 
  arrange(Date) %>% 
  mutate(sum_mort = (`Unknown Hospitalization` + 
                       `Not Hospitalized` +
                       Hospitalized))
mortality

#Plot of Total Mortalities Per Week in the State
weekmort.plot <- ggplot(mortality, aes(x=Date, y=sum_mort)) + 
  geom_bar(stat="identity", fill = "mediumseagreen") +
  labs(title = "Weekly Deaths from COVID-19 in Oregon, since March 2020",
       y = "Weekly Death Count", x = NULL) +
  ILE.Theme +
  scale_fill_discrete_qualitative("Harmonic") +
  scale_y_continuous(expand = c(0,0))
weekmort.plot

#Analyze Mortality by Hospital Status
require(reshape2) #To melt dataframe to long format
mort.melt <- mortality %>% 
  select(Week, Date, `Unknown Hospitalization`, Hospitalized, `Not Hospitalized`) %>% 
  melt(id = c("Date", "Week"), variable.name = "Hosp",
       value.name = "Count") 
head(mort.melt)

#Plot of Weekly Deaths per Day, by Hospitalization Status (Include on Final)
mort.plot1 <- ggplot(mort.melt, aes(x=Date, y=Count, fill=reorder(Hosp, Count))) + 
  geom_bar(stat="identity") +
  labs(title = "Weekly Deaths from COVID-19 in Oregon, since March 2020",
       subtitle = "Deaths reported by hospitalization status",
       fill = "Hospitalization Status",
       y = "Weekly Death Count", x = NULL) +
  scale_fill_manual(values=c("#E69F00", "lightskyblue3", "mediumseagreen")) +
  ILE.Theme +
  scale_y_continuous(expand = c(0,0))
mort.plot1 #Include on Final Plot

# Vaccination Totals, by Day, Since December 2020 (Updated 5/15) ----------------------------------------------

vax.daily <- read_csv("Vaccine_ByDay_5.15.csv")

#Clean and sort dates for vaccination counts.
require(lubridate) #To clean up and sort dates 
vax.daily <- vax.daily %>% 
  mutate(Date_sort = lubridate::parse_date_time(Date, "mdy")) %>% 
  arrange(Date_sort)
vax.daily

#Plot of Vaccine Total Per Day
vaxplot1 <- ggplot(vax.daily, aes(x=Date_sort, y=Count)) + 
  geom_bar(stat="identity", fill = "palegreen3", color = "palegreen3") +
  theme_minimal() +
  labs(title = "Vaccination Trends in Oregon, since December 2020",
       y = "Daily Vaccinations", x = NULL)
vaxplot1

#Plot of Vaccine Total with 7 Day Rolling Average
require(zoo)
vax.roll <- vax.daily %>% 
  group_by(Date_sort) %>% #Condense Table by Date
  summarise(tot.count = sum(Count)) %>% #Created total cases column
  mutate(roll_avg = zoo::rollmean(tot.count, k = 7, fill = NA)) #Computed 7-Day Rolling Average
vax.roll

vaxplot.roll <- ggplot(vax.roll, aes(x=Date_sort, y=tot.count)) + 
  geom_bar(stat="identity", fill = "mediumseagreen", color = "mediumseagreen") +
  geom_line(aes(Date_sort, roll_avg), size=1.5) +
  theme_minimal() +
  labs(title = "Daily Number of Vaccines Distributed in Oregon, since December 2020",
       subtitle = "Plotted with 7-Day Rolling Average",
       y = "Daily Vaccinations", x = NULL) +
  ILE.Theme +
  scale_y_continuous(expand = c(0,0))
vaxplot.roll #Include this on Final Report


#Plot of Vaccine Totals Per Day, by Producer
vaxplot2 <- ggplot(vax.daily, aes(x=Date_sort, y=Count, fill=Vaccine)) + 
  geom_bar(stat="identity") +
  theme_minimal() +
  labs(title = "Vaccination Trends in Oregon, since December 2020",
       y = "Daily Vaccinations", x = NULL) +
  scale_fill_discrete_qualitative("Harmonic")
vaxplot2 #Don't include in final report
  

# Disparities in Incidence (Updated 5/15) ---------------------------------------------

library(tidyverse)
democase <- read_csv("Demo_CasesPer100K_5.15.csv")
democase <- na.omit(democase)

#Plot of Population-Adjusted Incidence by Race
require(colorspace) #Better color scales

raceinc.plot <- democase %>% 
  filter(Demographic == "Race" | Demographic == "Ethnicity") %>%
  filter(!Categories %in% "Other") %>% #Filtered out "Other"
  filter(!Categories %in%  "Not Hispanic") %>% #Filtered out "Not Hispanic"
  ggplot(aes(x=reorder(Categories, RatePer100k), y=RatePer100k)) +
  geom_bar(stat="identity", fill="mediumseagreen") + #Reverted from qualitative scale for simplicity
  geom_text(aes(label=RatePer100k, fontface=2), hjust = -.2, size=3.75) +
  theme_minimal() +
  labs(title = "Cumulative Incidence of COVID-19 in Oregon, by Race and Ethnicity",
       subtitle = "Rates calculated by total cases since January 2020, per 100000 people",
       y = "Cases per 100000 people", x = NULL) +
  ILE.Theme + 
  theme(legend.position = "none",
        panel.grid.major.y = element_blank(),
        axis.ticks.y = element_blank()) +
  coord_flip() +
  scale_y_continuous(limits = c(0, 11500), expand = c(0,0))
raceinc.plot

#Plot of Population Adjusted Incidence, by Age Group
sec <- seq(1:9)
ageinc.plot <- democase %>% 
  filter(Demographic == "Age Group") %>% 
  mutate(ID = seq(1:9)) %>% 
  arrange(Categories, ID) %>% 
  ggplot(aes(x=reorder(Categories, ID), y=RatePer100k)) +
  geom_bar(stat="identity", fill="mediumseagreen") +
  geom_text(aes(label=RatePer100k, fontface = 2), hjust = -.2, size=3.75) +
  labs(title = "Cumulative Incidence of COVID-19 in Oregon, by Age Group",
       subtitle = "Rates calculated by total cases since January 2020, per 100000 people",
       y = "Cases per 100000 people", x = NULL) +
  ILE.Theme +
  theme(legend.position = "none",
        panel.grid.major.y = element_blank(),
        axis.ticks.y = element_blank()) +
  coord_flip() +
  scale_y_continuous(limits = c(0,9000), expand = c(0, 0))
  # scale_fill_discrete_qualitative("Harmonic")
ageinc.plot

library(gridExtra)
grid.arrange(raceinc.plot, ageinc.plot, nrow = 2, ncol =1)

#Incidence Plot by Gender
genderinc.plot <- democase %>% 
  filter(Demographic == "Sex Group") %>% 
  ggplot(aes(x=Categories, y=RatePer100k)) +
  geom_bar(stat="identity", fill="mediumseagreen", width = .5) +
  geom_text(aes(label=RatePer100k, fontface = 2), hjust = -.2, size=3.75) +
  labs(title = "Cumulative Incidence of COVID-19 in Oregon, by Gender*",
       subtitle = "Rates calculated by total cases since January 2020, per 100000 people",
       y = "Cases per 100000 people", x = NULL) +
  ILE.Theme +
  theme(legend.position = "none",
        panel.grid.major.y = element_blank(),
        axis.ticks.y = element_blank()) +
  coord_flip() +
  scale_y_continuous(limits = c(0,6500), expand = c(0, 0))
# scale_fill_discrete_qualitative("Harmonic")
genderinc.plot



#Create Geographic Map for Cumulative Incidence, by County
county.case <- read_csv("Cases_County_5.15.csv")
county.case <- na.omit(county.case)


#Source: https://urban-institute.medium.com/how-to-create-state-
# and-county-maps-easily-in-r-577d29300bb2
library(devtools)
library(urbnmapr) #Mapping software from urbnmapr

#Now, merge my data with urbanmpr code.
OR.case.map <- counties %>% 
  mutate(County = str_remove_all(county_name, " County")) %>% 
  filter(state_name == "Oregon") %>% 
  left_join(county.case, counties, by = "County") #Join my data, dvax1, with "counties"
#dataset from urbanmapr
OR.case.map

#Create Geographic Distribution Map for Incidence, by County
OR.case.map %>% 
  ggplot(mapping = aes(long, lat, group = group, fill = `Cases per 100,000`)) +
  geom_polygon(color = "#ffffff", size = .25) +
  scale_fill_gradient(guide = guide_colorbar(title.position = "top"),
                      high = "darkgreen", low = "seagreen2") +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  theme_minimal() +
  # geom_sf_text(aes(label = county_name), size = 3) + #Figure out how to add labels
  theme(legend.title = element_text(size = 12, face = "bold"),
        legend.key.width = unit(.5, "in"),
        legend.text = element_text(size = 12),
        panel.grid = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        plot.title = element_text(face = "bold",
                                  size = 15, hjust = .5)) +
  labs(fill = "Cases per 100000", 
       title = "Map of Case Count per 100000, by County in Oregon")




#Create Geographic Distribution Map for 2-Week Test Positivity, by County
twoweek.case <- read_csv("2Week_CountyTest_5.15.csv") #Read in two-week test data
twoweek.case <- na.omit(twoweek.case)
twoweek.case <- twoweek.case %>% 
  group_by(County) %>% #Condense Table by County
     summarise(tot.test = sum(Test_tot), #Summarise Total Tests and Positive Tests
               tot.pos = sum(Test_pos)) %>% 
  mutate(prop.pos = ((tot.pos/tot.test)))
twoweek.case
  
#Source: https://urban-institute.medium.com/how-to-create-state-
# and-county-maps-easily-in-r-577d29300bb2
library(devtools)
library(urbnmapr) #Mapping software from urbnmapr

#Now, merge my data with urbanmpr code.
OR.twoweektest.map <- counties %>% 
  mutate(County = str_remove_all(county_name, " County")) %>% 
  filter(state_name == "Oregon") %>% 
  left_join(twoweek.case, counties, by = "County") #Join my data, twoweek.case, with "counties"
#dataset from urbanmapr
OR.twoweektest.map

#Create Geographic Distribution Map for Incidence, by County (Include on Final)
OR.twoweektest.map %>% 
  ggplot(mapping = aes(long, lat, group = group, fill = `prop.pos`)) +
  geom_polygon(color = "#ffffff", size = .25) +
  scale_fill_gradient(guide = guide_colorbar(title.position = "top"),
                      high = "darkgreen", low = "seagreen2",
                      labels = scales::percent) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  theme_minimal() +
  # geom_sf_text(aes(label = county_name), size = 3) + #Figure out how to add labels
  theme(legend.title = element_text(size = 12, face = "bold"),
        legend.key.width = unit(.5, "in"),
        legend.key.height = unit(.3, "in"),
        legend.text = element_text(size = 12),
        panel.grid = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        plot.title = element_text(face = "bold",
                                  size = 15, hjust = .5),
        plot.subtitle = element_text(size = 13, hjust = .5)) +
  labs(fill = "Test Positivity Percentage",
       subtitle  = "Number of positive tests out of total number of tests since May 2nd, 2021", 
       title = "Two Week Test Positivity Percentage, by County in Oregon")



# Disparities in Hospitalization (Updated 5/15) ------------------------------------------

demohosp <- read_csv("Demo_Hospitalization_5.15.csv")
demohosp <- na.omit(demohosp)

#Add calculation for % of each group hospitalized
demohosp <- demohosp %>% 
  mutate(total = (Hospitalized + Not_Hospitalized + Unknown)) %>% 
  mutate(hosp.perct = ((Hospitalized/total)*100)) %>% 
  mutate(hosp.pct.rd = round(hosp.perct,2))

#Plot of Hospitalization Rates, by Race & Ethnicity (Include on Final Report)
racehosp.plot <- demohosp %>% 
  filter(Demographic == "Race" | Demographic == "Ethnicity") %>% 
  filter(!Category %in% "Refused/Unknown") %>% #Filtered out "Refuesed/Unknown"
  filter(!Category %in%  "Not Hispanic") %>% #Filtered out "Not Hispanic"
  filter(!Category %in%  "Other") %>% #Filtered out "Other"
  ggplot(aes(x=reorder(Category, hosp.pct.rd), y=hosp.pct.rd, fill=hosp.pct.rd)) +
  geom_bar(stat="identity", fill = "mediumseagreen") +
  geom_text(aes(label=hosp.pct.rd, fontface = 2), hjust = -.2, size=3.5) +
  theme_minimal() +
  labs(title = "Percentage of COVID-19 Cases Resulting in Hospitalization, by Race and Ethnicity",
       subtitle = "Number of hospitalizations divided by total case count in each demographic subgroup",
       y = "Percentage of Cases Hospitalized", x = NULL) +
  ILE.Theme +
  theme(legend.position = "none",
        panel.grid.major.y = element_blank(),
        axis.ticks.y = element_blank()) +
  coord_flip() +
  scale_y_continuous(limits = c(0,13), expand = c(0, 0),
                     labels = function(x) paste0(x, "%"))
racehosp.plot


#Plot of Hospitalization Rates, by Age (Include in Final)
agehosp.plot <- demohosp %>% 
  filter(Demographic == "Age Group") %>% 
  filter(!Category %in% "Refused/Unknown") %>%
  mutate(ID = seq(1:9)) %>% 
  ggplot(aes(x=reorder(Category, ID), y=hosp.pct.rd)) +
  geom_bar(stat="identity", fill = "mediumseagreen") +
  geom_text(aes(label=hosp.pct.rd, fontface = 2), hjust = -.2, size=3.5) +
  labs(title = "Percentage of COVID-19 Cases Resulting in Hospitalization, by Age Group",
       subtitle = "Number of hospitalizations divided by total case count in each age group",
       y = "Percentage of Cases Hospitalized", x = NULL) +
  ILE.Theme +
  theme(legend.position = "none",
        panel.grid.major.y = element_blank(),
        axis.ticks.y = element_blank()) +
  coord_flip() +
  scale_y_continuous(limits = c(0,35), expand = c(0, 0),
                     labels = function(x) paste0(x, "%"))
agehosp.plot



#Plot of Hospitalization Rates, by Gender (Include in Final)
sexhosp.plot <- demohosp %>% 
  filter(Demographic == "Sex Group") %>% 
  filter(!Category %in% "Refused/Unknown") %>%
  ggplot(aes(x=Category, y=hosp.pct.rd)) +
  geom_bar(stat="identity", fill = "mediumseagreen", width = .75) +
  geom_text(aes(label=hosp.pct.rd, fontface = 2), hjust = -.2, size=3.5) +
  labs(title = "Percentage of COVID-19 Cases Resulting in Hospitalization, by Gender*",
       subtitle = "Number of hospitalizations divided by total case count per Gender",
       y = "Percentage of Cases Hospitalized", x = NULL) +
  ILE.Theme +
  theme(legend.position = "none",
        panel.grid.major.y = element_blank(),
        axis.ticks.y = element_blank()) +
  coord_flip() +
  scale_y_continuous(limits = c(0,7.5), expand = c(0, 0),
                     labels = function(x) paste0(x, "%"),
                     breaks = c(0,2.5,5))
sexhosp.plot


# #Create Map to Display Geographic Disparities in Hospitalization
# county.case <- read_csv("Cases_County_5.15.csv")
# county.case <- na.omit(county.case)
# county.case

#Source: https://urban-institute.medium.com/how-to-create-state-
# and-county-maps-easily-in-r-577d29300bb2
# library(devtools)
# library(urbnmapr) #Mapping software from urbnmapr
# 
# #Now, merge my data with urbanmpr code.
# OR.case.map <- counties %>% 
#   mutate(County = str_remove_all(county_name, " County")) %>% 
#   filter(state_name == "Oregon") %>% 
#   left_join(county.case, counties, by = "County") #Join my data, dvax1, with "counties"
# #dataset from urbanmapr
# OR.case.map
# 
# #Create Geographic Distribution Map for Incidence, by County
# OR.case.map %>% 
#   ggplot(mapping = aes(long, lat, group = group, fill = `Cases per 100,000`)) +
#   geom_polygon(color = "#ffffff", size = .25) +
#   scale_fill_gradient(guide = guide_colorbar(title.position = "top"),
#                       high = "darkgreen", low = "seagreen2") +
#   coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
#   theme_minimal() +
#   # geom_sf_text(aes(label = county_name), size = 3) + #Figure out how to add labels
#   theme(legend.title = element_text(size = 12, face = "bold"),
#         legend.key.width = unit(.5, "in"),
#         legend.text = element_text(size = 12),
#         panel.grid = element_blank(),
#         axis.title = element_blank(),
#         axis.text = element_blank(),
#         plot.title = element_text(face = "bold",
#                                   size = 15, hjust = .5)) +
#   labs(fill = "Cases per 100000", 
#        title = "Map of Case Count per 100000, by County in Oregon")


#Determine if any other data is needed

# Disparities in Mortality (Updated 5/15)------------------------------------------------

demodeath <- read_csv("Demo_Deaths_5.15.csv")
demodeath <- na.omit(demodeath)

#Add Case Fatality Rate (CFR)
demodeath <- demodeath %>% 
  mutate(CFR = ((Died/Total)*100)) %>% 
  mutate(CFR.rd = round(CFR, 2))

#Plot CFR, by Race (Include on Final Report)
racefatal.plot <- demodeath %>% 
  filter(Demographic == "Race" | Demographic == "Ethnicity") %>% 
  filter(!Categories %in% "Refused/Unknown") %>% #Filtered out "Refuesed/Unknown"
  filter(!Categories %in%  "Not Hispanic") %>% #Filtered out "Not Hispanic"
  filter(!Categories %in%  "Other") %>% #Filtered out "Other"
  ggplot(aes(x=reorder(Categories, CFR.rd), y=CFR.rd)) +
  geom_bar(stat="identity", fill="mediumseagreen") +
  geom_text(aes(label=CFR.rd, fontface = 2), hjust = -.2, size=3.5) +
  labs(title = "COVID-19 Case Fatality Rate, by Race and Ethnicity",
       subtitle = "Number of deaths divided by total number of cases in each racial and ethnic subgroup",
       y = "Case Fatality Rate", x = NULL) +
  ILE.Theme +
  theme(legend.position = "none",
        panel.grid.major.y = element_blank(),
        axis.ticks.y = element_blank()) +
  coord_flip() +
  scale_y_continuous(limits = c(0, 2.2), expand = c(0, 0),
                     labels = function(x) paste0(x, "%"))
racefatal.plot


#Plot CFR, by Age Group (Include on Final Report)
agefatal.plot <- demodeath %>% 
  filter(Demographic == "Age Group") %>% 
  mutate(ID = seq(1:10)) %>% 
  ggplot(aes(x=reorder(Categories, ID), y=CFR.rd)) +
  geom_bar(stat="identity", fill = "mediumseagreen") +
  geom_text(aes(label=CFR.rd, fontface = 2), hjust = -.2, size=3.5) +
  labs(title = "COVID-19 Case Fatality Rate, by Age Group",
       subtitle = "Number of deaths divided by total number of cases in each age group",
       y = "Case Fatality Rate", x = NULL) +
  ILE.Theme +
  theme(legend.position = "none",
        panel.grid.major.y = element_blank(),
        axis.ticks.y = element_blank()) +
  coord_flip() +
  scale_y_continuous(limits = c(0, 24), expand = c(0, 0),
                     labels = function(x) paste0(x, "%"))
agefatal.plot



#Plot CFR, by Gender (Include on Final Report)
sexfatal.plot <- demodeath %>% 
  filter(Demographic == "Sex Group") %>% 
  filter(!Categories %in% "Refused/Unknown") %>%
  filter(!Categories %in% "Total") %>% 
  ggplot(aes(x=Categories, y=CFR.rd)) +
  geom_bar(stat="identity", fill = "mediumseagreen", width = .75) +
  geom_text(aes(label=CFR.rd, fontface = 2), hjust = -.2, size=3.5) +
  labs(title = "COVID-19 Case Fatality Rate, by Gender*",
       subtitle = "Number of deaths divided by total number of cases per gender group",
       y = "Case Fatality Rate", x = NULL) +
  ILE.Theme +
  theme(legend.position = "none",
        panel.grid.major.y = element_blank(),
        axis.ticks.y = element_blank()) +
  coord_flip() +
  scale_y_continuous(limits = c(0, 1.8), expand = c(0, 0),
                     labels = function(x) paste0(x, "%"),
                     breaks = c(0, .5, 1, 1.5))
sexfatal.plot



#Mortality by County Calculations

#Compute Case Fatality Rate (CFR) & Mortality Rate
# countydeath <- read_csv("Cases_County_5.11.csv")
# countydeath <- na.omit(countydeath)
county.case #Loaded in updated data from before
pop.OR <- read_csv("OregonPopulationbyCounty.csv")
pop.OR <- pop.OR %>% select(County, Population) #Loaded in population by county

countydeath <- county.case %>% 
  rename(cases = `Case Count`) %>% 
  mutate(CFR = ((Deaths/cases)*100)) %>% #Calculate CFR
  mutate(CFR.rd = round(CFR, 2)) %>% #Round CFR to 2 decimals
  arrange(County) #Sort by County
countydeath

countydeath <- inner_join(countydeath,
    pop.OR,
    by = "County") #Join Population DF to CountyDeath Statistics

countydeath <- countydeath %>% 
  mutate(mort.rt = ((Deaths/Population)*1000)) %>% #Added Mortality Rate
  mutate(mort.rt.rd = round(mort.rt, 2)) #Rounded Mortality Rate

#Plot County Mortality Rates 
county.mort.plot <- countydeath %>% 
  # arrange(desc(mort.rt.rd)) %>% 
  ggplot(aes(x=reorder(County, mort.rt.rd), y=mort.rt.rd)) +
  geom_bar(stat="identity", fill = "mediumseagreen") +
  geom_text(aes(label=mort.rt.rd, fontface = 2), hjust = -.2, size=3.5) +
  labs(title = "COVID-19 Mortality Rate, by County",
       subtitle = "Mortality Rate per 1000 People, 
       adjusted for County Population: Includes Data since January 2020",
       y = "Mortality Rate per 1000 Population", x = NULL) +
  ILE.Theme +
  theme(legend.position = "none",
        panel.grid.major.y = element_blank(),
        axis.ticks.y = element_blank()) +
  coord_flip() +
  scale_y_continuous(limits = c(0, 2.2), expand = c(0, 0))
county.mort.plot #Don't use on final graphic, change to a map.



#Create a Map of CFR Rate
#use my "countydeath" dataframe
#Source: https://urban-institute.medium.com/how-to-create-state-
# and-county-maps-easily-in-r-577d29300bb2
library(devtools)
library(urbnmapr) #Mapping software from urbnmapr

#Now, merge my data with urbanmpr code.
OR.death.map <- counties %>% 
  mutate(County = str_remove_all(county_name, " County")) %>% 
  filter(state_name == "Oregon") %>% 
  left_join(countydeath, counties, by = "County") #Join my data, dvax1, with "counties"
#dataset from urbanmapr
OR.death.map

#Create CFR Map, by County
OR.death.map %>% 
  ggplot(mapping = aes(long, lat, group = group, fill = `CFR.rd`)) +
  geom_polygon(color = "#ffffff", size = .25) +
  scale_fill_gradient(guide = guide_colorbar(title.position = "top"),
                      high = "darkgreen", low = "seagreen2") +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  theme_minimal() +
  # geom_sf_text(aes(label = county_name), size = 3) + #Figure out how to add labels
  theme(legend.title = element_text(size = 12, face = "bold"),
        legend.key.width = unit(.5, "in"),
        legend.key.height = unit(.25, "in"),
        legend.text = element_text(size = 12),
        panel.grid = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        plot.title = element_text(face = "bold",
                                  size = 15, hjust = .5),
        plot.subtitle = element_text(size = 13, hjust = .5)) +
  labs(fill = "Case Fatality Rate", 
       title = "Case Fatality Rate by County in Oregon, since January 2020",
       subtitle = "Rate computed as the number of total deaths divided 
       by the total number of cases in each county")




#Create a Map of Mortality Rate
  #use my "countydeath" dataframe
#Source: https://urban-institute.medium.com/how-to-create-state-
# and-county-maps-easily-in-r-577d29300bb2
library(devtools)
library(urbnmapr) #Mapping software from urbnmapr

#Now, merge my data with urbanmpr code.
OR.death.map <- counties %>% 
  mutate(County = str_remove_all(county_name, " County")) %>% 
  filter(state_name == "Oregon") %>% 
  left_join(countydeath, counties, by = "County") #Join my data, dvax1, with "counties"
#dataset from urbanmapr
OR.death.map

#Create Mortality Rate Map, by County
OR.death.map %>% 
  ggplot(mapping = aes(long, lat, group = group, fill = `mort.rt.rd`)) +
  geom_polygon(color = "#ffffff", size = .25) +
  scale_fill_gradient(guide = guide_colorbar(title.position = "top"),
                      high = "darkgreen", low = "seagreen2") +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  theme_minimal() +
  # geom_sf_text(aes(label = county_name), size = 3) + #Figure out how to add labels
  theme(legend.title = element_text(size = 13, face = "bold"),
        legend.key.width = unit(.5, "in"),
        legend.text = element_text(size = 12),
        panel.grid = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        plot.title = element_text(face = "bold",
                                  size = 15, hjust = .5),
        plot.subtitle = element_text(size = 13, hjust = .5)) +
  labs(fill = "Deaths per 1000 population", 
       title = "Mortality Rates by County in Oregon, since January 2020",
       subtitle = "Rate computed as the number of total deaths divided 
by the total population of each county, times 1000")





# Disparities in Vaccination (Data not Updated since 5/10)----------------------------------------------


#Vaccination by Race
demovax.agerace <- read_csv("Vaccine_DemoStatewideSum_5.11.csv") #Prior to inputting,
#changed % columns to numeric, to input in R as dbl instead of chr
library(magrittr) #Use to manimulate column names
names(demovax.agerace) <- gsub(" ", "_", names(demovax.agerace)) #Remove space
names(demovax.agerace) <- names(demovax.agerace) %>% tolower() #make lowercase
demovax.agerace
demovax.agerace <- demovax.agerace %>% 
  rename(prop.complet = `%_population_series_complete`,
         prop.inprog = `%_population_in_progress`,
         prop.vax = `%_population_vaccinated`)
demovax.agerace <- demovax.agerace %>% 
  mutate(pct.complet = (prop.complet*100),
         pct.inprog = (prop.inprog*100),
         pct.vax = (prop.vax*100))


#Plot of Completed Vaccination Rates, by Race/Ethnicity (Use on Final)
race.vax.plot <- demovax.agerace %>% 
  filter(demographic_type == "Race") %>% 
  filter(!demographic_category %in% c('Unknown', 'Other Race')) %>% 
  #Exluded Unknown and Other, no data
  ggplot(aes(x=fct_reorder(demographic_category, pct.complet), y=pct.complet)) +
  geom_bar(stat="identity", fill="mediumseagreen") +
  geom_text(aes(label=pct.complet, fontface = 2), hjust = -.2, size=3.75) +
  theme_minimal() +
  labs(title = "Percentage of Population Completely Vaccinated in Oregon, by Race and Ethnicity",
       subtitle = "Individuals are considered completely vaccinated if they have recieved two doses
       of either Pfizer or Moderna, or 1 dose of the Johnson & Johnson vaccine",
       y = "Percentage of Population Completely Vaccinated", x = NULL) +
  ILE.Theme +
  theme(legend.position = "none",
        panel.grid.major.y = element_blank(),
        axis.ticks.y = element_blank()) +
  coord_flip() +
  scale_y_continuous(limits = c(0, 42), expand = c(0, 0),
                     labels = function(x) paste0(x, "%"),
                     breaks = c(0,10,20,30,40))
race.vax.plot



#Plot of Completed Vaccination Rates, by Age Group
age.vax.plot <- demovax.agerace %>% 
  filter(demographic_type == "Age Groups") %>% 
  mutate(ID = seq(1:10)) %>% 
  #Exluded Unknown and Other, no data
  ggplot(aes(x=reorder(demographic_category, ID), y=pct.complet)) +
  geom_bar(stat="identity", fill="mediumseagreen") +
  geom_text(aes(label=pct.complet, fontface = 2), hjust = -.2, size=3.75) +
  theme_minimal() +
  labs(title = "Percentage of Population Completely Vaccinated in Oregon, by Age Group",
       subtitle = "Individuals are considered completely vaccinated if they have recieved two doses
       of either Pfizer or Moderna, or 1 dose of the Johnson & Johnson vaccine",
       y = "Percentage of Population Completely Vaccinated", x = NULL) +
  ILE.Theme +
  theme(legend.position = "none",
        panel.grid.major.y = element_blank(),
        axis.ticks.y = element_blank()) +
  coord_flip() +
  scale_y_continuous(limits = c(0, 80), expand = c(0, 0),
                     labels = function(x) paste0(x, "%"),
                     breaks = c(0,10,20,30,40,50,60,70))
age.vax.plot



demovax.agerace
#Plot of Completed Vaccination Rates, by Gender
gender.vax.plot <- demovax.agerace %>% 
  filter(demographic_type == "Sex") %>% 
  ggplot(aes(x=demographic_category, y=pct.vax)) +
  geom_bar(stat="identity", fill="mediumseagreen", width = .75) +
  geom_text(aes(label=pct.vax, fontface = 2), hjust = -.2, size=3.75) +
  theme_minimal() +
  labs(title = "Percentage of Population Partially or Fully Vaccinated in Oregon, by Gender",
       subtitle = "Includes all individuals who have recieved atleast one dose of any type of vaccine",
       y = "Percentage Partially or Fully Vaccinated", x = NULL) +
  ILE.Theme +
  theme(legend.position = "none",
        panel.grid.major.y = element_blank(),
        axis.ticks.y = element_blank()) +
  coord_flip() +
  scale_y_continuous(limits = c(0, 60), expand = c(0, 0),
                     labels = function(x) paste0(x, "%"),
                     breaks = c(0,10,20,30,40,50))
gender.vax.plot




#Vaccination by Geography
demovax <- read_csv("Vaccination_CountySum_5.11.csv") #Prior to inputting,
#changed % columns to numeric, to input in R as dbl instead of chr
demovax <- na.omit(demovax)
library(magrittr) #Use to manimulate column names
names(demovax) <- gsub(" ", "_", names(demovax)) #Remove space
names(demovax) <- names(demovax) %>% tolower() #make lowercase
demovax <- demovax %>% 
  rename(prop.complet = `%_population_series_complete`,
         prop.inprog = `%_population_in_progress`,
         prop.vax = `%_population_vaccinated`)
demovax <- demovax %>% 
  mutate(pct.complet = (prop.complet*100),
         pct.inprog = (prop.inprog*100),
         pct.vax = (prop.vax*100))

demovax.county <- demovax %>% 
  filter(demographic_type == "Countywide") %>% 
  arrange(pct.complet)

#Plot of County Vaccination Rates - by Completed Series
county.vax.plot <- demovax.county %>% 
  ggplot(aes(x=fct_reorder(county, pct.complet), y=pct.complet)) +
  geom_bar(stat="identity") +
  geom_text(aes(label=pct.complet), hjust = -.2, size=3.5) +
  theme_minimal() +
  labs(title = "Percentage of Population Completely Vaccinated, by Oregon County",
       subtitle = "Individuals are considered completely vaccinated if they have two doses
        of either Pfizer or Moderna, or 1 dose of Johnson & Johnson",
       y = "Percentage of Population Completely Vaccinated", x = NULL) +
  theme(axis.title = element_text(size=13, face = "bold"),
        axis.text = element_text(size=11, face = "bold"),
        legend.position = "none",
        panel.grid.major.y = element_blank(),
        axis.line = element_line(color = "grey50"),
        plot.title = element_text(face = "bold")) +
  coord_flip() +
  scale_y_continuous(limits = c(0, 55), expand = c(0, 0), 
                     labels = function(x) paste0(x, "%"))
  # scale_fill_discrete_qualitative("Harmonic") #Revert from this color scale
county.vax.plot




#Creating Map of Geographic Distribution of Vaccination

#Source: https://urban-institute.medium.com/how-to-create-state-
# and-county-maps-easily-in-r-577d29300bb2
library(devtools)
devtools::install_github("UrbanInstitute/urbnmapr")
library(urbnmapr) #Mapping software from urbnmapr

#Subset the demographic vaccine data to include selected variables.
dvax1 <- demovax.county %>% 
  select(county, pct.complet, pct.inprog, pct.vax,
         prop.vax, prop.inprog, prop.complet) #Created subset w/ only req'd info.

#Now, merge my data with urbanmpr code.
OR.county <- counties %>% 
  mutate(county = str_remove_all(county_name, " County")) %>% 
  filter(state_name == "Oregon") %>% 
  left_join(dvax1, counties, by = "county") #Join my data, dvax1, with "counties"
  #dataset from urbanmapr



#Geographic Distribution Map for Completed Vaccination
vax.comp.map <- OR.county %>% 
  ggplot(mapping = aes(long, lat, group = group, fill = prop.complet)) +
  geom_polygon(color = "#ffffff", size = .25) +
  scale_fill_gradient(guide = guide_colorbar(title.position = "top"),
                      high = "darkgreen", low = "seagreen2",
                      labels = scales::percent) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  theme_minimal() +
  # geom_sf_text(aes(label = county_name), size = 3) + #Figure out how to add labels
  theme(legend.title = element_text(size = 12, face = "bold"),
        legend.key.width = unit(.5, "in"),
        legend.key.height = unit(.3, "in"),
        legend.text = element_text(size = 12),
        panel.grid = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        plot.title = element_text(face = "bold",
                                  size = 15, hjust = .5),
        plot.subtitle = element_text(size = 13, hjust = .5)) +
  labs(fill = "Percent Fully Vaccinated", 
       title = "a) Percent of Population Completely Vaccinated, by County",
       subtitle = "Individuals who are completely vaccinated have completed one 
J&J dose or both doses of the Pfizer or Moderna vaccine")
vax.comp.map


#Geographic Distribution Map for Partially Complete/Complete Vaccination
vax.part.map <- OR.county %>% 
  ggplot(mapping = aes(long, lat, group = group, fill = prop.vax)) +
  geom_polygon(color = "#ffffff", size = .25) +
  scale_fill_gradient(guide = guide_colorbar(title.position = "top"),
                      high = "darkgreen", low = "seagreen2",
                      labels = scales::percent) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  theme_minimal() +
  # geom_sf_text(aes(label = county_name), size = 3) + #Figure out how to add labels
  theme(legend.title = element_text(size = 12, face = "bold"),
        legend.key.width = unit(.5, "in"),
        legend.key.height = unit(.3, "in"),
        legend.text = element_text(size = 12),
        panel.grid = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        plot.title = element_text(face = "bold",
                                  size = 14.5, hjust = .5),
        plot.subtitle = element_text(size = 13, hjust = .5)) +
  labs(fill = "Percent Partially or \nFully Vaccinated", 
       title = "b) Percent of Population Partially or Fully Vaccinated, by County",
       subtitle = "Includes all individuals who have recieved atleast one dose of 
any type of vaccine")
vax.part.map

grid.arrange(vax.comp.map, vax.part.map, nrow=2, ncol=1)


