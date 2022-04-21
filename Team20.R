rm(list = ls())

library(tidyverse)
library(lubridate)
library(scales)
library(caret)
library(Matrix)
library(xgboost)

set.seed(2020)

# Load data 
sales_train <- read.csv('sales_train.csv')
testData <- read.csv('test.csv')
items <- read.csv('items.csv')
shops <- read.csv('shops.csv')
items_Cat <- read.csv('item_categories.csv')

# Preparing the data

sales_train$date <- as.Date(sales_train$date, "%d.%m.%Y")

sales_train <- sales_train %>% 
  mutate(month = month(date))

sales_train <- sales_train %>% 
  mutate(day = day(date))

Sys.setlocale("LC_TIME", "C")
sales_train <- sales_train %>% 
  mutate(weekdays = weekdays(date))

sales_train <- sales_train %>% 
  mutate(shop_id = as.character(shop_id))

sales_train <- sales_train %>% 
  mutate(item_id = as.character(item_id))

sales_train <- sales_train %>%
  mutate( revenue = ifelse((item_cnt_day < 0)|(item_price < 0), 0, item_price*item_cnt_day))

testData <- testData %>% 
  mutate(ID = as.character(ID))

testData <- testData %>% 
  mutate(shop_id = as.character(shop_id))

testData <- testData %>% 
  mutate(item_id = as.character(item_id))

items <- items %>% 
  mutate(item_id = as.character(item_id))

items <- items %>% 
  mutate(item_category_id = as.character(item_category_id))

items <- items %>% 
  mutate(item_name = as.character(item_name))

items_Cat <- items_Cat %>%
  mutate(item_category_id = as.character(item_category_id))

items_Cat <- items_Cat %>%
  mutate(item_category_name = as.character(item_category_name))

shops <- shops %>%
  mutate(shop_id = as.character(shop_id))

shops <- shops %>%
  mutate(shop_name  = as.character(shop_name))

# merge data

sales <- testData %>%
  mutate(tmp_id = 1) %>%
  left_join(data.frame(tmp_id = 1,
                       date_block_num = seq(0, 34, by = 1)), by = "tmp_id") %>%
  left_join(sales_train, by = c("shop_id", "item_id", "date_block_num")) %>%
  arrange(shop_id, item_id, date) %>%
  left_join(shops, by = "shop_id") %>%
  left_join(items, by = "item_id")

rm(sales_train)


# replace negative value and NA
sales <- sales %>%
  mutate( item_cnt_day = ifelse(item_cnt_day < 0, 0, item_cnt_day),
          item_price = ifelse(is.na(item_price), 0, item_price))

# Feature engineering
FeatureEngData<- function(df, period){

  # item_price summarized by shop_id and item_id 
  item_shop_price <- df %>%
    filter(!is.na(date)) %>%
    filter(date_block_num < period) %>%
    arrange(shop_id, item_id, date) %>%
    group_by(shop_id, item_id) %>%
    summarise(
      item_shop_price_min = min(item_price, na.rm = TRUE),
      item_shop_price_mean = mean(item_price, na.rm = TRUE),
      item_shop_price_median = median(item_price, na.rm = TRUE),
      item_shop_price_max = max(item_price, na.rm = TRUE),
      item_shop_price_sd = sd(item_price, na.rm = TRUE)
    ) %>%
    ungroup()
  
  # item_price summarized by item_id 
  item_price <- df %>%
    filter(!is.na(date)) %>%
    filter(date_block_num < period) %>%
    group_by(item_id) %>%
    summarise(
      item_price_min = min(item_price, na.rm = TRUE),
      item_price_mean = mean(item_price, na.rm = TRUE),
      item_price_median = median(item_price, na.rm = TRUE),
      item_price_max = max(item_price, na.rm = TRUE),
      item_price_sd = sd(item_price, na.rm = TRUE),
      item_first_month = min(date_block_num, na.rm = TRUE)
    ) %>%
    ungroup()
  
  # sales1 and price1 by shop, item, and month then lag it upto 6 months 
  Sales_6M <- df %>%
    group_by(shop_id, item_id, date_block_num) %>%
    summarise(
      lag0_sales_by_month = sum(item_cnt_day, na.rm = TRUE),
      lag0_price_by_month = max(item_price, na.rm = TRUE)
    ) %>%
    ungroup() %>%
    arrange(shop_id, item_id, date_block_num) %>%
    mutate(
      lag1_sales_by_month = lag(lag0_sales_by_month, 1),
      lag2_sales_by_month = lag(lag0_sales_by_month, 2),
      lag3_sales_by_month = lag(lag0_sales_by_month, 3),
      lag4_sales_by_month = lag(lag0_sales_by_month, 4),
      lag1_price_by_month = lag(lag0_price_by_month, 1),
      lag2_price_by_month = lag(lag0_price_by_month, 2),
      lag3_price_by_month = lag(lag0_price_by_month, 3),
      lag4_price_by_month = lag(lag0_price_by_month, 4),
      sales_3Month_avg = (lag1_sales_by_month + lag2_sales_by_month + lag3_sales_by_month)/3
    ) %>%
    filter(date_block_num == period - 1)
  
  # total num sales summarized by shop_id / item_id, count num of month with no sales
  total_sales <- df %>%
    filter(date_block_num < period) %>%
    mutate(
      is_zero_sales = ifelse(is.na(item_cnt_day), 1 ,0),
      item_cnt_day = ifelse(is.na(item_cnt_day), 0 ,item_cnt_day)
    ) %>%
    group_by(shop_id, item_id, date_block_num) %>%
    summarise(
      total_sales = sum(item_cnt_day, na.rm = TRUE),
      is_zero_sales = max(is_zero_sales, na.rm = TRUE)
    ) %>%
    ungroup() %>%
    group_by(shop_id, item_id) %>%
    summarise(
      total_sales_min = min(total_sales, na.rm = TRUE),
      total_sales_mean = mean(total_sales, na.rm = TRUE),
      total_sales_median = median(total_sales, na.rm = TRUE),
      total_sales_max = max(total_sales, na.rm = TRUE),
      total_sales_sd = sd(total_sales, na.rm = TRUE),
      zero_sales = sum(is_zero_sales, na.rm = TRUE)
    ) %>%
    ungroup()
  
  # revenue summarized by shop_id
  rev_by_shop <- df %>%
    filter(date_block_num < period) %>%
    mutate(
      revenue = ifelse(is.na(revenue), 0 ,revenue)
    ) %>%
    group_by(shop_id, date_block_num) %>%
    summarise(
      total_rev_sales = sum(revenue, na.rm = TRUE)
    ) %>%
    ungroup() %>%
    group_by(shop_id) %>%
    summarise(
      total_rev_sales_min = min(total_rev_sales, na.rm = TRUE),
      total_rev_sales_mean = mean(total_rev_sales, na.rm = TRUE),
      total_rev_sales_median = median(total_rev_sales, na.rm = TRUE),
      total_rev_sales_max = max(total_rev_sales, na.rm = TRUE),
      total_rev_sales_sd = sd(total_rev_sales, na.rm = TRUE)
    ) %>%
    ungroup()
  
  # items by shop
  item_by_shop <- df %>%
    filter(!is.na(date)) %>%
    filter(date_block_num < period) %>%
    group_by(shop_id) %>%
    summarise(
      n_item = n_distinct(item_id)
    ) %>%
    ungroup()
  
  # maximum sales by item categories
  sales_by_itemcat <- df %>%
    filter(!is.na(date)) %>%
    group_by(shop_id, item_id, date_block_num) %>%
    summarise(
      total_sales = sum(item_cnt_day, na.rm = TRUE)
    ) %>%
    ungroup() %>%
    arrange(shop_id, item_id, date_block_num) %>%
    distinct(shop_id, item_id, .keep_all = TRUE) %>%
    filter(date_block_num < period) %>%
    left_join(items, by = "item_id") %>%
    group_by(shop_id, item_category_id) %>%
    summarise(
      p_total_sales_max = max(total_sales, na.rm = TRUE)
    ) %>%
    ungroup()
  
  # total sales selected
  ThisPeriod_total_sales <- df %>%
    filter(date_block_num == period) %>%
    group_by(shop_id, item_id) %>%
    summarise(
      this_total_sales = sum(item_cnt_day, na.rm = TRUE),
      this_price_mean = mean(item_price, na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(
      this_total_sales = ifelse(is.na(this_total_sales), 0, this_total_sales),
      this_total_sales_20 = ifelse(this_total_sales > 20, 20, this_total_sales)
    )
  
  # merge data
  final <- ThisPeriod_total_sales %>%
    left_join(item_shop_price, by = c("shop_id", "item_id")) %>%
    left_join(item_price, by = c("item_id")) %>%
    left_join(Sales_6M, by = c("shop_id", "item_id")) %>%
    left_join(total_sales, by = c("shop_id", "item_id")) %>%
    left_join(rev_by_shop, by = c("shop_id")) %>%
    left_join(item_by_shop, by = c("shop_id")) %>%
    left_join(items, by = "item_id") %>%
    left_join(sales_by_itemcat, by = c("shop_id", "item_category_id")) %>%
    left_join(items_Cat, by = "item_category_id") %>%
    left_join(shops, by = "shop_id") %>%
    mutate(
      # replace na
      lag0_price_by_month = ifelse(lag0_price_by_month != 0, lag0_price_by_month,
                        ifelse(is.na(item_shop_price_median), item_price_median, item_shop_price_median)),
      lag1_price_by_month = ifelse(lag1_price_by_month != 0, lag1_price_by_month,
                        ifelse(is.na(item_shop_price_median), item_price_median, item_shop_price_median)),
      lag2_price_by_month = ifelse(lag2_price_by_month != 0, lag2_price_by_month,
                        ifelse(is.na(item_shop_price_median), item_price_median, item_shop_price_median)),
      lag3_price_by_month = ifelse(lag3_price_by_month != 0, lag3_price_by_month,
                        ifelse(is.na(item_shop_price_median), item_price_median, item_shop_price_median)),
      lag4_price_by_month = ifelse(lag4_price_by_month != 0, lag4_price_by_month,
                        ifelse(is.na(item_shop_price_median), item_price_median, item_shop_price_median)),
      
      item_shop_price_min = ifelse(is.na(item_shop_price_min), item_price_min, item_shop_price_min),
      item_shop_price_mean = ifelse(is.na(item_shop_price_mean), item_price_mean, item_shop_price_mean),
      item_shop_price_median = ifelse(is.na(item_shop_price_median), item_price_median, item_shop_price_median),
      item_shop_price_max = ifelse(is.na(item_shop_price_max), item_price_max, item_shop_price_max),
      item_shop_price_sd = ifelse(is.na(item_shop_price_sd), item_price_sd, item_shop_price_sd),
      
      # diff of item price
      d_Shop_Median_price_l1 = lag0_price_by_month - item_shop_price_median,
      d_Median_price_l1 = lag0_price_by_month - item_price_median,
      
      # total sales duration
      dur_total_sales = ifelse(is.na(item_first_month), 0, period - item_first_month),
      
      # zero sales period
      zero_sales = ifelse(zero_sales > dur_total_sales, dur_total_sales, zero_sales),
      
      # diff of sales
      d_sales_mean = lag0_sales_by_month - total_sales_mean,
      d_sales_max = lag0_sales_by_month - total_sales_max,
      d_sales1 = lag0_sales_by_month - lag1_sales_by_month,
      d_sales2 = lag0_sales_by_month - lag2_sales_by_month,
      d_sales3 = lag0_sales_by_month - lag3_sales_by_month,
      d_sales4 = lag0_sales_by_month - lag4_sales_by_month,
      
      # flg of release month 
      r_month = ifelse(dur_total_sales == 0, 1, 0),
      
      # new items num of sales
      lag0_sales_by_month = ifelse(r_month == 1, p_total_sales_max, lag0_sales_by_month),
      
      # month
      month = date_block_num %% 12
    ) %>%
    # replace na
    mutate_at(vars(starts_with("lag_"), starts_with("item_price_"), starts_with("d_")), list(~ifelse(is.na(.), 0, .)))
  
  # dummy colomuns
  dum_colums <- dummyVars(~., data = final %>% select(
    item_category_id, item_category_name, shop_name))
  final <- final %>%
    bind_cols(predict(dum_colums, final) %>% as.data.frame())
  
  return(final)
  
}

sales32 <- FeatureEngData(sales, 32)
sales33 <- FeatureEngData(sales, 33)
sales_pred34 <- FeatureEngData(sales, 34)

# Preparing data
x_sales32 <- sales32 %>%
  select(
    starts_with("item_price_"), -item_first_month, starts_with("lag_"), starts_with("total_sales_"),
    -this_total_sales_20, starts_with("total_rev_sales_"), n_item,
    starts_with("d_"), dur_total_sales, zero_sales, starts_with("d_sales_"), r_month, month,
    starts_with("item_category_id"),-item_category_id, -item_category_id0, -lag1_sales_by_month, 
    -lag2_sales_by_month, -item_shop_price_sd,
    -item_shop_price_min, -item_shop_price_mean, -item_shop_price_median, -item_price_mean,
    -item_price_median, -item_price_max, -lag0_price_by_month, -lag1_price_by_month, 
    -lag2_price_by_month,-lag3_price_by_month, -lag4_price_by_month
  )

y_sales32 <- sales32 %>%
  select(this_total_sales_20)

x_sales33 <- sales33 %>%
  select(
    starts_with("item_price_"), -item_first_month, starts_with("l_"), starts_with("total_sales_"),
    -this_total_sales_20, starts_with("total_rev_sales_"), n_item,
    starts_with("d_"), dur_total_sales, zero_sales, starts_with("d_sales_"), r_month, month,
    starts_with("item_category_id"), -item_category_id, -item_category_id0, -lag1_sales_by_month,
    -lag2_sales_by_month, -item_shop_price_sd,
    -item_shop_price_min, -item_shop_price_mean, -item_shop_price_median, -item_price_mean,
    -item_price_median, -item_price_max, -lag0_price_by_month, -lag1_price_by_month, -lag2_price_by_month,
    -lag3_price_by_month, -lag4_price_by_month
  )

y_sales33 <- sales33 %>%
  select(this_total_sales_20)

# Modeling

x_data <- x_sales32 %>% as.matrix()
Y_data <- y_sales32 %>% as.matrix()

set.seed(2020)
param<-list(
  max_depth = 4, 
  eta = 0.02, 
  gamma = 0, 
  colsample_bytree = 0.65, 
  subsample = 0.6, 
  min_child_weight = 3
)


# nrounds with cross-validation
xgbcv <- xgb.cv( param = param, data = x_data, label = Y_data, nrounds = 1000, 
                 nfold = 10, showsd = F, stratified = T, print_every_n = 250, 
                 early_stopping_rounds = 100, maximize = F)
#xgbcv$best_iteration 493

set.seed(2020)
model_xgb <- xgboost(param = param, data = x_data, label = Y_data,
                     nrounds = 998, importance = TRUE)


# check the most important features
mat <- xgb.importance(names(x_data), model = model_xgb)
ggplot(mat[1:40,])+
  geom_bar(aes(x=reorder(Feature, Gain), y=Gain), stat='identity', fill='blue')+
  xlab(label = "Features")+
  coord_flip() +
  ggtitle("Feature Importance")


# RMSE 32
pred_sales32 <- sales32 %>%
  bind_cols(pred = predict(model_xgb, newdata = x_sales32 %>% as.matrix(), type = "response")) %>%
  mutate(error = this_total_sales_20 - pred)

pred_sales32 %>%
  summarise(
    RMSE = sqrt(sum(abs(error^2))/n())
  )

# RMSE 33
pred_sales33 <- sales33 %>%
  bind_cols(pred = predict(model_xgb, newdata = x_sales33 %>% as.matrix(), type = "response")) %>%
  mutate(error = this_total_sales_20 - pred)

pred_sales33 %>%
  summarise(
    RMSE = sqrt(sum(abs(error^2))/n())
  )

# Final prediction

x_sales_pred34 <- sales_pred34 %>%
  select(
    starts_with("item_price_"), -item_first_month, starts_with("lag_"), starts_with("total_sales_"),
    -this_total_sales_20, starts_with("total_rev_sales_"), n_item,
    starts_with("d_"), dur_total_sales, zero_sales, starts_with("d_sales_"), r_month, month,
    starts_with("item_category_id"), -item_category_id, -item_category_id0,  -item_category_id0, -lag1_sales_by_month, 
    -item_shop_price_sd, -item_shop_price_min, -item_shop_price_mean, -item_shop_price_median, 
    -item_price_mean, -item_price_median, -item_price_max, 
    -lag1_price_by_month, -lag2_price_by_month, -lag3_price_by_month, -lag4_price_by_month
  )

finalPredictions <- sales_pred34 %>%
  bind_cols(item_cnt_month = predict(model_xgb, newdata = x_sales_pred34 %>% as.matrix(), type = "response")) %>%
  left_join(testData, by = c("shop_id", "item_id")) %>%
  select(ID, item_cnt_month) %>%
  mutate(item_cnt_month = ifelse(item_cnt_month > 20, 20, ifelse(item_cnt_month < 0, 0, item_cnt_month)))

summary(finalPredictions)

write.csv(finalPredictions, file="finalpredictions.csv", row.names = F)
