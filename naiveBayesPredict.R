require(tree)
require(dplyr)
require(e1071)
require(kernlab)
require(dplyr)
require(rpart)
require(readr)

get_subcategory <- function(psc)
{
  subcat <- psc_trans_table %>% filter(`4_Digit_PSC` == psc) %>% select(Level_2_Category) %>% .$Level_2_Category
  subcat
}

get_category <- function(psc)
{
  subcat <- psc_trans_table %>% filter(`4_Digit_PSC` == psc) %>% select(Level_1_Category) %>% .$Level_1_Category
  subcat
}


prep <- function(df)
{
  psc_trans_table <<- read_csv("pscTransTable.csv")
  #pull IT PSC vector from psc translation table
  cat_pscs <- psc_trans_table %>% filter(Level_1_Category == "Human Capital") %>% select(`4_Digit_PSC`) %>% .$'4_Digit_PSC'
  #load corresponding transactions from data frame
  load_df <- df %>% filter(product_or_service_code %in% cat_pscs) %>%select(product_or_service_code, naics_code, funding_agency_name, vendor_duns_number)
  load_df$subcategory <- unlist(lapply(load_df$product_or_service_code, get_subcategory))
  load_df$category <- unlist(lapply(load_df$product_or_service_code, get_category))
  attribute_names <- colnames(load_df)
  load_df[, attribute_names] <- lapply(load_df[, attribute_names], factor)
  load_df
}

train_on_trans <- function(df)
{
  set.seed(1000)
  df <- sample_n(df, size = 15000)
  df_count <- nrow(df)
  print(paste0("Using ", df_count, " rows"))
  train_count_max <- df_count * 0.75
  print(paste0(train_count_max, " training rows"))
  test_count_min <- train_count_max + 1
  print(paste0((df_count - test_count_min), " test rows"))
  df_train <- df[1:train_count_max, ]
  df_test <- df[test_count_min:df_count, ]
  test_labels_actual <- df_test %>% select(subcategory) %>% .$subcategory
  df_test <- df_test %>% select(-subcategory, category)
  label_model <- naiveBayes(subcategory~product_or_service_code+naics_code+funding_agency_name+vendor_duns_number, data=df_train)
  fit_values <- fitted(label_model)
  predictions <- predict(label_model, df_test)
  #predictions
  final_table <- table(test_labels_actual, predictions)
  accuracy <- (sum(diag(final_table)))/sum(final_table)
  print(paste("Accuracy of fit is ", accuracy))
  predictions
}


