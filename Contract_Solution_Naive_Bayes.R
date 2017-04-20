require(readr)
prep <- function(df)
{
  FPDS_ATOM_GW_19APR17_df <- read_tsv("FPDS_ATOM_GW_19APR17.tsv")
  Contract_Vehicle_Reference <- read.csv("Contract Vechicle Reference.csv")
  filtered_by_piid <- filter(FPDS_ATOM_GW_19APR17_df, FPDS_ATOM_GW_19APR17_df$reference_piid %in% Contract_Vehicle_Reference_DF$`Referenced PIID`)
  
  merged_vehicle_df <- merge(filtered_by_piid, Contract_Vehicle_Reference_DF, by.x = "reference_piid", by.y = "Referenced PIID", all.x = TRUE)
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
  test_labels_actual <- df_test %>% select(`Contract Vehicle`) %>% .$`Contract Vehicle`
  df_test <- df_test %>% select(-`Contract Vehicle`)
  label_model <- naiveBayes(`Contract Vehicle`~product_or_service_code+naics_code+funding_agency_name+vendor_duns_number, data=df_train)
  fit_values <- fitted(label_model)
  predictions <- predict(label_model, df_test)
  #predictions
  final_table <- table(test_labels_actual, predictions)
  accuracy <- (sum(diag(final_table)))/sum(final_table)
  print(paste("Accuracy of fit is ", accuracy))
  predictions
}