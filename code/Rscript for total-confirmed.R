
TotalConfirmedCases <- read.csv("D:/total-confirmed-cases-of-covid-19-per-million-people-vs-gdp-per-capita.csv")

#Required packages
library(lubridate)
library(dplyr)

mainDf <- as.data.frame(TotalConfirmedCases)

#changing the column names for easy access

colnames(mainDf)[1] <- "Location"
colnames(mainDf)[2] <- "iso_code"
colnames(mainDf)[3] <- "Year"
colnames(mainDf)[7] <- "GDP"

#changing the chr type date to a regular date format 
#mainDf$Day <- strptime(as.character(mainDf$Day), "%d/%m/%Y")

#changing the regular date format to a POSIXct in order to use it later
#(You cant change it directly into a POSIX format thats why we change it to a date format first)
#mainDf$Day <- as.POSIXct(mainDf$Day, format = "%Y-%m-%d %H:%M:%S")


#Dividing months to the seasons in order to group them later
mainDf$Seasons <- case_when(month(mainDf$Day) %in% c(12, 1, 2) ~ "Winter",
                       month(mainDf$Day) %in% c(3, 4, 5) ~ "Spring",
                       month(mainDf$Day) %in% c(6, 7, 8) ~ "Summer",
                       month(mainDf$Day) %in% c(9, 10, 11) ~ "Fall")

#Selecting the necessary columns for the final table
mainDf <- mainDf %>%
  select(c("iso_code","Continent", "Location","Year","Seasons", "GDP" ))

#check for NA values
sum(is.na(mainDf)) #we see that there are alot of NA values in the GDP section because of Continent data

#change them to 0
mainDf[is.na(mainDf)] <- 0 

#Filtering out the unnecessary columns from the mainDf
mainDf <- mainDf %>%
  filter(Location != "High income") %>%
  filter(Location != "Low income") %>%
  filter(Location != "Upper middle income") %>%
  filter(Location != "World excl. China") %>%
  filter(Location != "Lower middle income")


# Create a finalDf with appropriate columns from the 
# mainDf and group by their respective counterparts

finalDf <- mainDf %>% group_by(iso_code, Continent, Location, Year, Seasons, ) %>% 
  summarise(GDP_mean=mean(GDP))
#I used "mean" on the GDP because I think that represents the best of that country's GDP around that time period

View(finalDf)

# LIMITATIONS AND POSSIBLE IMPROVEMENTS ON THE DATA

# 1- So as you can see on the data there are some empty values in "ISO" "Continent" that is because
# in this data those are calculated as a Continent itself and has its own data and continents
# do not have a specific ISO code. We could potentially change them into NA but because 
# the column itself is not a "numeric" so it does not shows as 0 after the NA cleaning
# I did earlier in the code

# 2- Some GDP was N/A on the data but mostly those countries are either very small as a 
# population or as a size. Mostly islands etc... I decided not to touch them but we can
# also remove them since they are not very important to the data itself. 

# 3- I used "mean" function while grouping on the GDP because that was the only
# decent function I found in "group_by" and also I think that represents the best of that country's
# GDP around that time period


# Define is_blank
# is_blank <- function(x) {is.na(x) | x == ""}

# Eliminating if the iso_code = "" and Continent = ""
finalDf2 <- finalDf[!finalDf[,"iso_code"] == "",]
finalDf2 <- finalDf2[!finalDf2[,"Continent"] == "",]
finalDf2 <- finalDf2[!finalDf2[,"GDP_mean"] == 0,]

unique(finalDf2$Continent)

# Scatter plot
NewDf <- subset(finalDf2, select = c(Continent, Year, GDP_mean))
NewDf %>%
  mutate(Continent2 = case_when(Continent == "NorthAmerica" ~ "NorthAmerica",
                                Continent == "Africa" ~ "Africa",
                                Continent == "Europe" ~ "Europe",
                                Continent == "South America" ~ "South America",
                                Continent == "Oceania" ~ "Oceania"),
         Continent2 = factor(Continent2,
                             levels = c("NorthAmerica", "Africa", "Europe",
                                        "South America", "Oceania"),
                             labels = c("NorthAmerica", "Africa", "Europe",
                                        "South America", "Oceania"),)) %>%
  ggplot() +
  geom_point(aes(x = Year, y = GDP_mean, color = Continent2)) +
  labs (x = "Year", y = "GDP_mean", color = "Continent") +
  theme_bw()


# Scatter plot2
NewDf2 <- subset(finalDf2, select = c(Continent, Year, GDP_mean))
NewDf2 %>%
  ggplot() +
  geom_boxplot(aes(x = Continent, y = GDP_mean, fill = Continent),
               show.legend = FALSE) +
  labs(x = "Continent", y = "GDP_mean")


# Scatter plot 3
NewDf3 <- subset(finalDf2, select = c(Continent, Seasons, GDP_mean))

NewDf3$Seasons[is.na(match(NewDf3$Seasons, "Winter")) == FALSE] <- 1
NewDf3$Seasons[is.na(match(NewDf3$Seasons, "Spring")) == FALSE] <- 2
NewDf3$Seasons[is.na(match(NewDf3$Seasons, "Summer")) == FALSE] <- 3
NewDf3$Seasons[is.na(match(NewDf3$Seasons, "Fall")) == FALSE] <- 4

NewDf3 %>%
  mutate(Continent3 = case_when(Continent == "NorthAmerica" ~ "NorthAmerica",
                                Continent == "Africa" ~ "Africa",
                                Continent == "Europe" ~ "Europe",
                                Continent == "South America" ~ "South America",
                                Continent == "Oceania" ~ "Oceania"),
         Continent3 = factor(Continent3,
                             levels = c("NorthAmerica", "Africa", "Europe",
                                        "South America", "Oceania"),
                             labels = c("NorthAmerica", "Africa", "Europe",
                                        "South America", "Oceania"),)) %>%
  ggplot() +
  geom_point(aes(x = Seasons, y = GDP_mean, color = Continent3)) +
  labs (x = "Seasons", y = "GDP_mean", color = "Continent") +
  theme_bw()

# Scatter plot4
NewDf4 <- subset(finalDf2, select = c(Continent, Seasons, GDP_mean))

NewDf4$Seasons[is.na(match(NewDf4$Seasons, "Winter")) == FALSE] <- 1
NewDf4$Seasons[is.na(match(NewDf4$Seasons, "Spring")) == FALSE] <- 2
NewDf4$Seasons[is.na(match(NewDf4$Seasons, "Summer")) == FALSE] <- 3
NewDf4$Seasons[is.na(match(NewDf4$Seasons, "Fall")) == FALSE] <- 4

NewDf4 %>%
  ggplot() +
  geom_boxplot(aes(x = Seasons, y = GDP_mean, fill = Continent),
               show.legend = FALSE) +
  labs(x = "Seasons", y = "GDP_mean")
