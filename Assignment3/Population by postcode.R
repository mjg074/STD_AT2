library(tidyverse)
library(readxl)
library(Amelia)
library(dplyr)
library(tidyr)
library(lubridate)

# Read Postcode to SA2 map
postcode_to_SA2 <- read_csv("POSTCODE_SA2.csv") # Downloaded from ABS
str(postcode_to_SA2)

# Read population by SA2
pop_by_SA2 <- read_csv("ABS_ANNUAL_ERP.csv") # Downloaded from ABS
str(pop_by_SA2)
head(pop_by_SA2)
names(pop_by_SA2)

# Select the columns needed and rename
pop_by_SA2 <- pop_by_SA2[,c(3,5,6,9,11)] 
pop_by_SA2 <-  filter(pop_by_SA2, REGIONTYPE == "SA2")
pop_by_SA2 <- rename(pop_by_SA2, "SA2" = "ASGS_2011", 
                     "Population" = "Value", 
                     "Year" = "TIME")
names(pop_by_SA2)
head(pop_by_SA2)
length(unique(pop_by_SA2$SA2))
length(unique(postcode_to_SA2$SA2))
head(postcode_to_SA2)

# Join population by SA2 to Postcode by SA2
pop_by_SA2_postcode <-pop_by_SA2 %>% 
  left_join(postcode_to_SA2)
pop_by_SA2_postcode

# Use the ratio per postcode to get the SA2 population in that postcode (I think thats the right thing to do, otherwise the national population adds up to over 30 million!!)
pop_by_SA2_postcode <- pop_by_SA2_postcode %>% 
  mutate(Population = round((Population * RATIO),digits=0))
str(pop_by_SA2_postcode)

# Summarise up to Postcode from SA2s
names(pop_by_SA2_postcode)
pop_by_postcode <- pop_by_SA2_postcode %>% 
  group_by(POSTCODE,Year) %>% 
  summarise(population=sum(Population)) %>% 
  ungroup()

write.csv(pop_by_postcode,"pop_by_postcode.csv")

# Rename the population variable as y to fit with the interpolation formulas further along, remove those with NAs focus on only "complete cases"
pop_by_postcode <- rename(pop_by_postcode, y = population)
pop_by_postcode <- pop_by_postcode[complete.cases(pop_by_postcode),]

# Check some individual postcodes
postcode_2000 <- filter(pop_by_postcode, POSTCODE == '2000')
postcode_2000 <- rename(postcode_2000, y = population)
postcode_2011 <- filter(pop_by_postcode, POSTCODE == '2011')
postcode_2042 <- filter(pop_by_postcode, POSTCODE == '2042')
postcode_2146 <- filter(pop_by_postcode, POSTCODE == '2146')
str(pop_by_SA2_postcode)


# Functions to interpolate from yearly population data into monthly splits
# taken from example at stack overflow here: https://stackoverflow.com/questions/32320727/interpolating-in-r-yearly-time-series-data-with-quarterly-values
expand_data <- function(x) {
  years <- min(x$Year):max(x$Year)
  months <- 1:12
  grid <- expand.grid(month=months, Year=years)
  x$month <- 1
  merged <- grid %>% left_join(x, by=c('Year', 'month'))
  merged$POSTCODE <- x$POSTCODE[1]
  return(merged)
}

interpolate_data <- function(data) {
  xout <- 1:nrow(data)
  y <- data$y
  interpolation <- approx(x=xout[!is.na(y)], y=y[!is.na(y)], xout=xout)
  data$yhat <- interpolation$y
  return(data)
}

expand_and_interpolate <- function(x) interpolate_data(expand_data(x))

monthly_data <- pop_by_postcode %>% group_by(POSTCODE) %>% do(expand_and_interpolate(.))

print(as.data.frame(monthly_data))

head(monthly_data)

# Rename back to column headers I want, and change back to characters etc
monthly_data <- rename(monthly_data, population = yhat)
monthly_data <- rename(monthly_data, Postcode = POSTCODE)
monthly_data <- rename(monthly_data, Month = month)

monthly_data <- monthly_data %>% 
  mutate (Year = as.character(Year),
          Month = as.character(Month))

# Format month as "01" instead of "1" for Jan for example ready for merging back with the cleaned crime data from bocsar 
monthly_data <- monthly_data %>% 
  mutate (Month = ifelse(nchar(Month)==1,paste0("0",Month),Month))

head(monthly_data)

monthly_data <- monthly_data[,c(1:3,5)]

str(monthly_data)

# Write out to csv for use in merging in other code with crime data
write.csv(monthly_data,"monthly_pop_by_postcode.csv")


# pop_by_year <- pop_by_postcode %>% 
#   group_by(Year) %>% 
#   summarise(Pop=sum(population))
# str(pop_by_year)

# pop_by_LGA <- read_csv("ABS_ERP_LGA2018.csv")
# str(pop_by_LGA)
