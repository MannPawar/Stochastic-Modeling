rm(list=ls())

data <- read_csv('data/T_ONTIME_MARKETING.csv')

glimpse(data)
summary(data)

data %>% filter(MKT_UNIQUE_CARRIER=="WN") -> SW

Arrival_delays <- SW %>% select(ARR_TIME, ARR_DELAY, ORIGIN_CITY_NAME, ORIGIN_STATE_NM, DEST_CITY_NAME, DEST_STATE_NM, AIR_TIME, DISTANCE, FL_DATE) %>% 
     mutate(date = mdy_hms(FL_DATE))

# Add the new 'Region' column
Arrival_delays_by_region <- Arrival_delays %>%
     mutate(Region = case_when(
          DEST_STATE_NM %in% c("Connecticut", "Maine", "Massachusetts", "New Hampshire", "Rhode Island", "Vermont", "New Jersey", "New York", "Pennsylvania") ~ "Northeast",
          DEST_STATE_NM %in% c("Illinois", "Indiana", "Michigan", "Ohio", "Wisconsin", "Iowa", "Kansas", "Minnesota", "Missouri", "Nebraska", "North Dakota", "South Dakota") ~ "Midwest",
          DEST_STATE_NM %in% c("Delaware", "Florida", "Georgia", "Maryland", "North Carolina", "South Carolina", "Virginia", "District of Columbia", "West Virginia", "Alabama", "Kentucky", "Mississippi", "Tennessee", "Arkansas", "Louisiana", "Oklahoma", "Texas") ~ "South",
          DEST_STATE_NM %in% c("Arizona", "Colorado", "Idaho", "Montana", "Nevada", "New Mexico", "Utah", "Wyoming", "Alaska", "California", "Hawaii", "Oregon", "Washington") ~ "West",
          # This last line catches any values that don't match the above categories
          TRUE ~ "Other/Territory"
     ))

# View the first few rows of the new dataframe with the Region column
head(Arrival_delays_by_region)