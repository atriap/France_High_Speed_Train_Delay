#load tidyverse
library(tidyverse)


#import data from csv to df
df <- read.csv('C:/Users/ASUS/Desktop/Regularities_by_liaisons_Trains_France.csv')


#verify data frame, count columns and rows
print(is.data.frame(df))
print(ncol(df))
print(nrow(df))


#change df to tibble
df <- as_tibble(df)


#view imported data frame
View(df)


#columns names
print(colnames(df))


#rename columns
df <- df %>% 
  rename(year = Year, month = Month, departure_station = Departure.station, arrival_station = Arrival.station, travel_time_avg = Average.travel.time..min.,
         num_of_expected_trips = Number.of.expected.circulations, num_of_canceled_trains = Number.of.cancelled.trains, num_late_at_departure = Number.of.late.trains.at.departure,
         avg_delay_late_at_departure = Average.delay.of.late.departing.trains..min., avg_delay_all_departing = Average.delay.of.all.departing.trains..min.,
         comment_delays_at_departure = Comment..optional..delays.at.departure, num_arriving_late = Number.of.trains.late.on.arrival,
         avg_delay_late_on_arrival = Average.delay.of.late.arriving.trains..min., avg_delay_all_arriving = Average.delay.of.all.arriving.trains..min.,
         comment_delays_on_arrival = Comment..optional..delays.on.arrival,
         delay_cause_external_cause = X..trains.late.due.to.external.causes..weather..obstacles..suspicious.packages..malevolence..social.movements..etc..,
         delay_cause_rail_infrastructure = X..trains.late.due.to.railway.infrastructure..maintenance..works.,
         delay_cause_traffic_management = X..trains.late.due.to.traffic.management..rail.line.traffic..network.interactions.,
         delay_cause_rolling_stock = X..trains.late.due.to.rolling.stock,
         delay_cause_station_management = X..trains.late.due.to.station.management.and.reuse.of.material,
         delay_cause_travelers = X..trains.late.due.to.passenger.traffic..affluence..PSH.management..connections.,
         num_greater_15_min_late = Number.of.late.trains...15min, avg_delay_late_greater_15_min = Average.train.delay...15min,
         num_greater_30_min_late = Number.of.late.trains...30min, num_greater_60_min_late = Number.of.late.trains...60min)


#columns names
print(colnames(df))


#delay cause columns comparison
df[c(16:21, 27:32)]


#remove redundant columns, define to new dataframe
trains_df <- df %>% 
  select(year:num_greater_60_min_late)

print(ncol(trains_df))
print(nrow(trains_df))


#sort data frame by year, month
trains_df = trains_df %>%
  arrange(year,month)

print(colnames(trains_df))
trains_df


#count number of NA in each columns
na_count = data.frame(na_sum = colSums(is.na(trains_df))) %>%
  arrange(desc(na_sum))

na_count %>% 
  filter(na_sum != 0)


#count unique observations in several columns
unique(trains_df[c("departure_station")])

trains_df %>%
  count(departure_station) %>%
  arrange(desc(n))

View(trains_df %>%
  count(departure_station) %>%
  arrange(desc(n)))


#add new columns to data frame: total trips, late at departure rate, canceled trains rate
trains_df = trains_df %>%
  mutate(total_num_trips = num_of_expected_trips - num_of_canceled_trains,
         pct_late_departure = num_late_at_departure/total_num_trips,
         pct_late_arrival = num_arriving_late/total_num_trips,
         mean_monthly_pct_cancelled = num_of_canceled_trains/total_num_trips)
         
#which station has the most number of trips
trains_df %>%
  group_by(departure_station) %>%
  summarise(total_trip = sum(total_num_trips, na.rm = TRUE)) %>%
  arrange(desc(total_trip))

#number of trips and cancelled trips by year
trains_df %>%
  group_by(year) %>%
  summarise(total_trip = sum(total_num_trips, na.rm = TRUE))

trains_df %>%
  group_by(year) %>%
  summarise(total_canceled = sum(num_of_canceled_trains, na.rm = TRUE))


#delay cause by year
View(trains_df %>%
  group_by(year) %>%
  summarise(across(starts_with("delay"), mean, na.rm = TRUE)))


#average late by year
trains_df %>%
  group_by(year) %>%
  summarise(rate_departure_late = mean(pct_late_departure, na.rm = TRUE))

trains_df %>%
  group_by(year) %>%
  summarise(rate_arrival_late = mean(pct_late_arrival, na.rm = TRUE))


#define date
Sys.setlocale("LC_TIME", "C")
trains_df = trains_df %>%
  mutate(date = as.Date(sprintf("%d-%02d-01",year,month)))



#visualization of average late
#install.packages("scales")
library(scales)

trains_df %>%
  group_by(year, date) %>%
  summarise(rate_departure_late = mean(pct_late_departure, na.rm = TRUE)) %>% 
  ungroup() %>% 
  ggplot(aes(date,rate_departure_late,fill = rate_departure_late)) +
  scale_fill_gradient(low = "pink", high = "red") + 
  scale_y_continuous(labels = percent_format()) +
  geom_bar(stat = "identity")

trains_df %>%
  group_by(year, date) %>%
  summarise(rate_arrival_late = mean(pct_late_arrival, na.rm = TRUE)) %>% 
  ungroup() %>% 
  ggplot(aes(date,rate_arrival_late,fill = rate_arrival_late)) +
  scale_fill_gradient(low = "pink", high = "red") + 
  scale_y_continuous(labels = percent_format()) +
  geom_bar(stat = "identity")


#average late at departure visualized
trains_df %>%  
  group_by(date) %>%
  summarise(mean_monthly_pct_late_departure = mean(pct_late_departure)) %>%
  ggplot(aes(x = date, y = mean_monthly_pct_late_departure)) +
  geom_line(size = 1) +
  xlab("Year") +
  ylab("Average Departing Late per Total Trips")


#average late at arrival visualized
trains_df %>%  
  group_by(date) %>%
  summarise(mean_monthly_pct_late_arrival = mean(pct_late_arrival, na.rm = TRUE)) %>%
  ggplot(aes(x = date, y = mean_monthly_pct_late_arrival)) +
  geom_line(size = 1) +
  xlab("Tahun") +
  ylab("Average Arriving Late per Total Trips")


#average late at departure per month per years compared
trains_df %>%
  group_by(year,month) %>%
  summarise(monthly_pct_late_departure = mean(pct_late_departure, na.rm = TRUE)) %>% 
  ggplot(aes(x = month, y = monthly_pct_late_departure, color = factor(year))) +
  geom_line(size = 2) +
  labs(color = "Year") +
  xlab("Year") +
  ylab("Average Departing Late per Total Trips")


#average late at arrival per month per years compared
trains_df %>%
  group_by(year,month) %>%
  summarise(monthly_pct_late_arrival = mean(pct_late_arrival, na.rm = TRUE)) %>% 
  ggplot(aes(x = month, y = monthly_pct_late_arrival, color = factor(year))) +
  geom_line(size = 2) +
  labs(color = "Year") +
  xlab("Tahun") +
  ylab("Average Arriving Late per Total Trips")


#visualization of departure late in each station
trains_df %>% 
  group_by(departure_station) %>% 
  summarise(Avg_pct_departure_station_late = mean(pct_late_departure, na.rm = TRUE),
            Avg_circulating_trains = mean(total_num_trips, na.rm = TRUE)) %>% 
  arrange(desc(Avg_pct_departure_station_late)) %>% 
  filter(Avg_pct_departure_station_late > 0.2) %>% 
  ggplot(aes(x = reorder(departure_station, -Avg_pct_departure_station_late,order = FALSE),
             y = Avg_pct_departure_station_late,fill = Avg_circulating_trains)) +
  scale_fill_gradient(low = 'white', high = 'purple') +
  geom_bar(stat = "identity") +
  coord_flip()

#causes of departing late
trains_df %>%  
  group_by(year,month) %>% 
  summarise(across(starts_with("delay"), mean, na.rm = TRUE)) %>%  
  mutate(date = as.Date(sprintf("%d-%02d-01",year,month))) %>% 
  pivot_longer(cols = starts_with("delay"),names_to = "delay") %>%
  mutate(delay = substring(delay,nchar("delay_cause_") + 1)) %>% 
  rename(delay_cause = delay) %>% 
  ggplot(aes(date,value,fill = delay_cause)) +
  geom_area(alpha = 0.6) +
  xlab("Tahun") +
  scale_y_continuous(labels = percent_format())


