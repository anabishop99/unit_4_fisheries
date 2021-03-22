load('data/RAMLDB v4.491/DB Files With Assessment Data/R Data/DBdata[asmt][v4.491].RData')
library(tidyverse)
glimpse(timeseries)
glimpse(stock)
summary(stock)
unique(stock$state)
glimpse(tsmetrics)

fish = timeseries %>%
  left_join(stock, by = 'stockid') %>%
  left_join(tsmetrics, by = c("tsid" = "tsunique"))
glimpse(fish)

## TCbest-MT

fish_catch = fish %>%
  filter(tsid=="TCbest-MT",
         state == "Current")
glimpse(fish_catch)

###check assessment IDs
length(unique(fish_catch$assessid))
length(unique(fish_catch$stockid))

## Removing all 
fish_max_assess = fish_catch %>%
  group_by(stockid, assessid) %>%
  summarize(max_tsyear = max(tsyear), 
            min_tsyear = min(tsyear)) %>%
  mutate(assessment_length = max_tsyear - min_tsyear) %>%
  ungroup() %>%
  group_by(stockid) %>%
  filter(assessment_length == max(assessment_length)) %>%
  distinct(stockid, .keep_all=TRUE)

glimpse(fish_max_assess)

# filtering join
fish_catch_max_assess = fish_catch %>%
  semi_join(fish_max_assess, by="stockid")
glimpse(fish_catch_max_assess)
dim(fish_catch)

### COD

cod = fish_catch_max_assess %>% 
  filter(scientificname == "Gadus morhua",
         region == "Canada East Coast") %>%
  group_by(tsyear) %>%
  summarise(total_catch = sum(tsvalue, na.rm=TRUE)) 

glimpse(cod)

# plot cod total catch time series

ggplot(data=cod, aes(x=tsyear, y = total_catch)) +
  geom_line()

# Collapse ? 
dat = c(3,5,2,8,4,9,1)
cummax(dat)

cod_collapse = cod %>%
  mutate(historical_max_catch = cummax(total_catch),
         collapse = (total_catch <= 0.1*historical_max_catch))

#what year did the collapse happen?
cod_collapse_year = cod_collapse %>%
  filter(collapse == TRUE) %>%
  summarize(tsyear = min(tsyear))
  
glimpse(cod_collapse)

# plot cod total catch time series
ggplot(data=cod_collapse, aes(x=tsyear, y=total_catch, color=collapse)) +
  geom_line() + 
  geom_vline(xintercept = cod_collapse_year)

# check for collapse in ALL RAM stocks

collapse = fish_catch_max_assess %>%
  filter(!is.na(tsvalue)) %>%
  group_by(stockid) %>%
  mutate(historical_max_catch = cummax(tsvalue),
         current_collapse = (tsvalue <= 0.1*historical_max_catch),
         ever_collapse = cumsum(current_collapse) > 0 ) # treat booleans as numbers; 0 = false, 1 = true

glimpse(collapse)

# how many stocks collapsed each year
collapse_yr = collapse %>%
  group_by(stockid, stocklong.x, region) %>%
  filter(ever_collapse == TRUE) %>%
  summarize(first_collapse_yr = min(tsyear))

glimpse(collapse_yr)

ggplot(data=collapse_yr, aes(x=first_collapse_yr)) + 
  geom_histogram(color="black", fill="white", binwidth = 5)

# count cumulative stocks collapsed over time
n_stock_assessments = length(unique(collapse$stockid))

collapse_ts = collapse_yr %>%
  count(first_collapse_yr) %>%
  mutate(cum_first_collapse_yr = cumsum(n),
         ratio_ever_collapsed = cum_first_collapse_yr / n_stock_assessments)
glimpse(collapse_ts)


