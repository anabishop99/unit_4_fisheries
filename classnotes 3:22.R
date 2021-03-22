# generalized linear models

source("build_collapse_table.r")

glimpse(timeseries)
glimpse(collapse)

model_data = collapse %>%
  group_by(stockid, stocklong, region) %>%
  summarize(ever_collapsed = any(ever_collapsed)) %>%
  ungroup()

glimpse(model_data)

# logistic regression time
model_l = glm(ever_collapsed ~ region, data=model_data, family = "binomial") # tells r we're looking at logistic regression; transforms y variable to fit in binomial distribution
summary(model_l)
sort(unique(model_data$region))

# prediction plot
regions = model_data %>% distinct(region)
model_l_predict = predict(model_l, newdata = regions, se.fit=TRUE)

collapse_region_predictions = cbind(regions, model_l_predict)
head(collapse_region_predictions)

## plot it
ggplot(data=collapse_region_predictions, aes(x=region, y=fit, fill=region)) +
  geom_bar(stat="identity", show.legend=FALSE) + 
  geom_errorbar(aes(ymin)) +
  coord_flip()

## Poisson model
timeseries_values_views

#biomass and fishing pressure ratios over time
u_summary = timeseries_values_views %>%
  left_join(stock, by=c("stockid", "stocklong")) %>%
  filter(!is.na(BdivBmsypref),
         !is.na(UdivUmsypref)) %>%
  group_by(stockid, stocklong, region) %>%
  summarize(yrs_data = n(),
            ratio_yrs_overfished = (UdivUmsypref > 1)/yrs_data,
            ratio_yrs_low_stock = (BdivBmsypref < 1)/yrs_data) %>%
  select(~yrs_data)

collapse_summary = collapse %>%
  group_by(stockid, stocklong, region) %>%
  summarize(yrs_data = n(),
            yrs_collapsed = sum(current_collapse/yrs_data) %>%
              inner_join(u_summary, by = c("stockid", "stocklong", "region"))
table(collapse_summary)
