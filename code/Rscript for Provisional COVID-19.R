### USA-COIVD19 Deaths by Sex and Age

### Importing Libraries ###
library("dplyr")
library("tidyr")
library("ggplot2")

## Data Importing and Cleaning
covidDatasetSexAndAge <- read.csv("Provisional_COVID-19_Deaths_by_Sex_and_Age.csv")
## 118422 observations with 16 variables
# Deselecting the Footnote column from the data frame
covidSexAge <- covidDatasetSexAndAge %>%
  select(!c("Footnote"))

# Changing NA Values to 0 as all NA are data points (for 0) for specific time period
covidSexAge[is.na(covidSexAge)] <- 0 
summary(covidSexAge) # 118422 observations with 15 variables

## Data Analysis

# Calculating the total number of deaths by influenza, pneumonia and Covid19
covidSexAge$Influenza.Pneumonia.COVID.19.Deaths <- 
  rowSums(covidSexAge[,c("COVID.19.Deaths","Pneumonia.Deaths","Influenza.Deaths")])

# Calculating the total number of deaths by influenza and Covid19
covidSexAge$Influenza.and.COVID.19.Deaths <-
  covidSexAge$Influenza.Pneumonia.COVID.19.Deaths - 
  covidSexAge$Pneumonia..Influenza..or.COVID.19.Deaths - 
  covidSexAge$Pneumonia.and.COVID.19.Deaths

summary(covidSexAge) # 118422 observations with 17 variables after calculation

# Reduce/filter Age group as below as their are some overlaps.
# AgeGroup: All Ages, Under 1 year,1-4 years, 5-14 years, 15-24 years, 
# 25-34 years, 35-44 years, 45-54 years, 55-64 years, 65-74 years, 
# 75-84 years,85 years and over

usa_covid19 <- filter(covidSexAge,covidSexAge$Age.Group %in% 
                        c("All Ages", "Under 1 year","1-4 years", "5-14 years", 
                          "15-24 years", "25-34 years", "35-44 years", 
                          "45-54 years", "55-64 years", "65-74 years", 
                          "75-84 years", "85 years and over"))
# Change character value names: 
# All Ages: All # Under 1 year: < 1
# 1-4 years: 1-4.. # 85 years and over: >=85

usa_covid19$Age.Group <- sub("years","",usa_covid19$Age.Group) # remove "years"
usa_covid19["Age.Group"][usa_covid19["Age.Group"] == "All Ages"] <- "All"
usa_covid19["Age.Group"][usa_covid19["Age.Group"] == "Under 1 year"] <- "< 1"
usa_covid19["Age.Group"][usa_covid19["Age.Group"] == "85  and over"] <- ">= 85"
# 83592 observations with 17 variables 
unique(usa_covid19["Age.Group"]) # Total 12 Age Groups


## Data Analysis and Visualization for United States (Tomomi)
library(scales)

# Making a new data frame by whole united USA
usa_covid19_whole <- usa_covid19[usa_covid19$State == "United States", ]
# 1548 observations with 17 variables

# Changing the format to long format
usa_covid19_whole_l <- gather(usa_covid19_whole, key ="Causes", value = "Cases",
                              COVID.19.Deaths, Pneumonia.and.COVID.19.Deaths,
                              Influenza.and.COVID.19.Deaths)

# Eliminating Age.Group = All
usa_covid19_whole_l <- usa_covid19_whole_l[!usa_covid19_whole_l[,"Age.Group"] == "All",]

# Eliminating Sex = All Sexes
usa_covid19_whole_l <- usa_covid19_whole_l[!usa_covid19_whole_l[,"Sex"] == "All Sexes",]


# Comparing Covid19 Deaths / Covid19 + Pneumonia Deaths/ 
# Covid19 + Influenza Deaths by age group
ggplot(data = usa_covid19_whole_l, 
       mapping = aes(x = Age.Group, y = Cases, fill = Causes)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_y_continuous(labels = label_comma())

# Comparing Covid19 Deaths / Covid19 + Pneumonia Deaths/ 
# Covid19 + Influenza Deaths by gender
ggplot(data = usa_covid19_whole_l, 
       mapping = aes(x = Sex, y = Cases, fill = Causes)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_y_continuous(labels = label_comma())

# Comments
# By age, 85 or over has the most Covid19 Deaths, while 75-84 has 
# the most Pneumonia and COVID19. 
# Influenza and Covid19 is very few and almost same number.
# By gender, Female has the most Covid19 Deaths, while Male has the most
# Pneumonia Deaths.
# The same as by age, Influenza and Covid19 is very few and almost same number.


# Eliminating inacurate data
usa_covid19_whole_l <- usa_covid19_whole_l[!(usa_covid19_whole_l$Year == 0 |
                                             usa_covid19_whole_l$Month == 0),] 
# Adding a variable
usa_covid19_whole_l <- mutate(usa_covid19_whole_l, 
                               YearMonth = paste(Year, Month, sep = "/"))

ggplot(usa_covid19_whole_l, aes(x = YearMonth, y = Cases, color = Causes)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x)
  scale_y_continuous(labels = label_comma())

##### There are no regression line... 
# Covid19 Deaths and Pneumonia and Covid19 deaths are up and down,
# while Influenza Deaths almost flat.
  