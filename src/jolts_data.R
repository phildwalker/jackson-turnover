# looking to capture the US labor statistics data (JOLTS: Job Openings and Labor Turnover Survey)
# This will help us provide some context around how Jackson is performing relative to the US market as a whole


# library(devtools)
# install_github('mikeasilva/blsAPI')

library(tidyverse)

# Series Id:    JTS520000000000000LDR

# Seasonally adjusted
# Industry:     Finance and insurance
# State/Region: Total US
# Area:         All areas
# Data Element: Layoffs and discharges
# Size Class:   All size classes
# Rate/Level:   Rate

layoffs <- read.table(text = "
Year,Jan,Feb,Mar,Apr,May,Jun,Jul,Aug,Sep,Oct,Nov,Dec
2015,0.4,0.3,0.7,0.7,0.6,0.6,0.8,0.7,0.6,0.4,0.5,0.6
2016,0.5,0.6,0.8,0.5,0.6,0.4,0.4,0.6,0.5,0.8,0.5,0.5
2017,0.4,0.7,0.5,0.3,0.4,0.7,0.5,0.5,0.3,0.8,0.5,0.4
2018,0.7,0.4,0.4,0.6,0.6,0.6,0.4,0.4,0.6,0.5,0.6,0.6
2019,0.5,0.4,0.3,0.5,0.4,0.5,0.5,0.5,0.4,0.3,0.3,0.4
2020,0.6,0.6,1.2,1.0,0.4,1.2,0.6,0.3,0.4,0.3,0.5,0.4
2021,0.3,0.4,0.7,0.4,0.3,0.5,0.5,0.4,0.4,0.3,0.2,0.4
2022,0.3,0.4,0.3,0.2,0.6,0.5,0.1,0.5,0.6,0.5,1.1,0.4
2023,0.3,0.3,0.6,0.4,0.4,0.3,0.4,0.4,0.4,0.6,0.5,0.4
2024,0.5,0.4,0.3,0.7,0.6,0.4,0.7,0.7,0.4,0.6,0.3,0.5
2025,0.5,0.5,0.6,1.0,0.3,0.5,0.5,0.6,NA,NA,NA,NA",
sep = ",", header = TRUE, fill = TRUE, stringsAsFactors = FALSE)



long_layoffs <- layoffs %>%
  pivot_longer(
    cols = -Year,
    names_to = "Month",
    values_to = "Value"
  ) |> 
  mutate(
    Date = ymd(paste(Year, Month, "01")) # Create the Date column.
  ) |> 
  select(Date, layoff_rate = Value)

# Series Id:    JTS520000000000000QUR

# Seasonally adjusted
# Industry:     Finance and insurance
# State/Region: Total US
# Area:         All areas
# Data Element: Quits
# Size Class:   All size classes
# Rate/Level:   Rate

quits <- read.table(text = "
Year,Jan,Feb,Mar,Apr,May,Jun,Jul,Aug,Sep,Oct,Nov,Dec
2015,1.3,1.1,1.1,1.2,1.1,1.2,1.0,0.9,1.0,1.1,1.0,1.4
2016,1.3,1.6,0.8,1.0,1.1,1.2,1.0,1.0,1.0,1.2,1.0,1.0
2017,1.4,1.0,1.1,1.2,1.3,1.0,1.2,1.0,1.2,1.3,1.4,1.3
2018,1.2,1.3,1.1,0.7,1.1,1.2,1.2,1.3,1.1,0.9,1.0,1.3
2019,1.0,1.2,1.1,1.4,1.3,1.3,1.4,1.4,1.4,1.4,1.6,1.3
2020,1.3,1.3,1.2,0.6,1.2,1.1,0.9,1.6,1.2,1.3,1.3,1.2
2021,1.3,1.2,1.2,1.4,1.3,1.3,1.3,1.3,1.8,1.3,1.4,1.3
2022,1.7,1.6,1.8,1.7,1.3,1.5,1.6,1.3,1.2,1.3,1.2,1.4
2023,1.4,1.0,0.9,1.5,1.4,1.1,1.0,1.3,1.3,1.4,1.3,1.2
2024,1.2,1.2,1.3,1.1,1.0,1.4,1.3,1.2,1.2,1.0,1.1,1.1
2025,1.1,1.1,1.4,1.3,1.4,1.2,1.4,1.2,NA,NA,NA,NA",
sep = ",", header = TRUE, fill = TRUE, stringsAsFactors = FALSE)

long_quits <- quits %>%
  pivot_longer(
    cols = -Year,
    names_to = "Month",
    values_to = "Value"
  ) |> 
  mutate(
    Date = ymd(paste(Year, Month, "01")) # Create the Date column.
  ) |> 
  select(Date, quit_rate = Value)


# Series Id:    JTU520000000000000JOR

# Not seasonally adjusted
# Industry:     Finance and insurance
# State/Region: Total US
# Area:         All areas
# Data Element: Job openings
# Size Class:   All size classes
# Rate/Level:   Rate

job_open <- read.table(text = "
Year,Jan,Feb,Mar,Apr,May,Jun,Jul,Aug,Sep,Oct,Nov,Dec
2015,4.4,3.6,3.1,5.1,4.1,3.2,3.9,3.8,4.2,4.5,4.3,4.8
2016,4.9,3.3,3.9,4.8,3.9,3.6,4.1,4.2,4.2,4.1,4.0,4.4
2017,4.3,4.0,4.2,5.8,3.9,4.4,4.1,3.7,5.4,4.6,4.1,4.2
2018,5.2,4.8,4.7,4.1,3.9,3.8,5.1,4.9,4.2,4.8,4.6,4.3
2019,4.7,3.9,3.4,4.3,3.7,3.7,3.9,4.2,4.2,5.1,3.7,3.3
2020,5.1,4.6,3.7,3.5,3.2,3.4,3.5,3.2,3.5,3.8,3.1,3.1
2021,3.7,3.8,3.7,4.8,3.7,3.3,5.8,4.7,5.2,5.4,5.4,5.3
2022,6.2,4.8,5.0,6.1,5.0,6.5,7.5,4.7,3.8,6.6,5.4,5.1
2023,5.0,4.1,4.6,5.7,3.7,4.3,4.7,5.8,7.0,4.3,3.9,4.0
2024,5.2,6.4,4.1,4.9,4.6,4.3,5.1,3.4,4.4,4.2,5.6,3.8
2025,5.3,3.6,4.2,4.4,5.2,3.8,4.7,3.6,NA,NA,NA,NA",
sep = ",", header = TRUE, fill = TRUE, stringsAsFactors = FALSE)

long_job <- job_open %>%
  pivot_longer(
    cols = -Year,
    names_to = "Month",
    values_to = "Value"
  ) |> 
  mutate(
    Date = ymd(paste(Year, Month, "01")) # Create the Date column.
  ) |> 
  select(Date, job_openings = Value)



#---- combine all the data together


bls_jolt <- 
  full_join(
    long_layoffs,
    long_quits,
    by = join_by(Date)
  ) |> 
  left_join( , y=long_job, by = join_by(Date)) |> 
  filter(Date >= as.Date("2018-01-01")) |> 
  pivot_longer(cols = 2:4)


bls_p1 <- bls_jolt |> 
  ggplot(aes(Date, value, color=name))+
      geom_rect(aes(xmin = as.Date("2022-01-01"), xmax = as.Date("2023-01-01"), ymin = -Inf, ymax = Inf), alpha = 0.01, fill = jcolor[6])+
    geom_rect(aes(xmin = as.Date("2023-01-01"), xmax = as.Date("2024-01-01"), ymin = -Inf, ymax = Inf), alpha = 0.01, fill = jcolor[4])+

   geom_point()+
   geom_line()+
  geom_smooth(se = F) +
  labs(title = "Trend of Job Opening Rate, Quit + Layoff Rates In Finance and insurance",
 caption = "Source: BLS JOLT")
