

#R code Crime .csv 







library(data.table) 
library(tidyverse) 
library(ggmap)
library(maps)
library(mapdata)
library(lubridate) 
library(ggrepel)
library(varhandle) 


crime_la <- as.data.frame(fread("Crime_Data_from_2010_to_Present.csv", na.strings = c("NA")))
head(crime_la,3)


crime_la_selected <- select(crime_la, `Date Occurred`, `Time Occurred`, `Area Name`, `Crime Code Description`, `Victim Age`, `Victim Sex`, `Victim Descent`, `Premise Description`, `Weapon Description`, `Status Description`, Location)



crime_la_selected$`Date Occurred` <- mdy(crime_la_selected$`Date Occurred`) 


location <- crime_la_selected$Location %>% 
  str_replace_all("[()]", "") %>%
  str_split_fixed(", ", n=2) %>% 
  as.data.frame %>% 
  transmute(lat=V1, long=V2)  
on
crime_la_selected <- cbind(crime_la_selected, location)

crime_la_selected <- subset(crime_la_selected, select = -c(Location))


crime_selected_years <- filter(crime_la_selected, `Date Occurred` >= as_date("2017-01-01"), `Date Occurred` <= as_date("2017-12-30"))


rm(crime_la, crime_la_selected, location)

crime_selected_years$year <- year(crime_selected_years$`Date Occurred`)
crime_selected_years$month <- month(crime_selected_years$`Date Occurred`)
crime_selected_years$days <- day(crime_selected_years$`Date Occurred`)

crime_selected_years$`Victim Sex` <- recode(crime_selected_years$`Victim Sex`, 'F' = 'Female', 'M' = 'Male', 'X' = 'Unknown')

crime_selected_years$`Victim Descent` <- recode(crime_selected_years$`Victim Descent`, "A" = "Other Asian", "B" = "Black", "C" = "Chinese", "D" = "Cambodian", "F" = "Filipino", "G" = "Guamanian", "H" = "Hispanci/Latin/Mexican", 'I' = "American Indian/Alaskan Native", "J" = "Japanese", "K" = "Korean", "L" = "Laotian", "O" = "Other", "P" = "Pacific Islander", "S" = "Somoan", "U" = "Hawaiian", "V" = "Vietnamese", "W" = "White", "X" = "Unknown", "Z" = "Asian Indian")


character_vars <- lapply(crime_selected_years, class) == "character"
crime_selected_years[, character_vars] <- lapply(crime_selected_years[, character_vars], as.factor)

glimpse(crime_selected_years)










age <- year_2017 %>%
  group_by(`Victim Age`) %>%
  summarise(total = n()) %>%
  na.omit()

age %>%
  ggplot(aes(x = `Victim Age`, y = total)) +
  geom_line(group = 1) +
  geom_point(size = 0.5) +
  labs(title = "Age Most Likely To Become Crime Victim", 
       x = "Victim Age", 
       y = "Total")



year_2017$age_group <- cut(year_2017$`Victim Age`, breaks = c(-Inf, 19, 35, 55, Inf), labels = c("Teenager", "Young Adult", "Middle Age", "Elderly"))

age.group <- year_2017 %>%
  group_by(age_group, `Crime Code Description`) %>%
  summarise(total = n()) %>%
  top_n(20) %>%
  na.omit()

age.group %>%
  ggplot(aes(reorder(x = `Crime Code Description`, total), y = total)) +
  geom_col(fill = 'red') +
  geom_text(aes(label=total), color='black', hjust = -0.1, size = 3) +
  coord_flip() +
  facet_wrap(~ age_group) +
  labs(x = 'Total', 
       y = "Crime Description")


crime_data_oct17 %>%
  group_by(month_occurred) %>%
  summarise(n = n_distinct(DR_number)) %>%
  
  #Plot the data  
  ggplot(aes(x = as.factor(month_occurred), y = n)) +
  geom_bar(stat = "identity", fill = "blue", alpha = 0.3) +
  
  #Format axes, titles and lables
  geom_text(aes(x = month_occurred, y = 1, label = comma(n)),
            hjust=0, vjust=.5, size = 3, colour = 'black', fontface = 'bold') +
  scale_y_continuous(labels = comma) +
  labs(y = "Number of crime occurred", x = "Month", 
       title = "Number of crimes by month") +
  coord_flip() +
  theme_bw() 
