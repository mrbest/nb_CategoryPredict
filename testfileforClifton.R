require(tree)
require(e1071)
require(kernlab)
require(dplyr)
require(rpart)
require(readr)
require(kknn)

main <- function()
{
  print("Loading data")
  fpds_df <- read_tsv("FPDS_ATOM_GW_19APR17.tsv")
  print("Generating training data")
  training_data <- generate_training_data(fpds_df, "BMO")
  print("Generating test data")
  test_data <- generate_test_data(fpds_df, "BMO")
  print("Training model")
  os3_model <- produce_model(training_data)
  print("Predicting")
  addressable_spend <- produce_addressable_spend_prediction(os3_model, test_data)
  addressable_spend
}

generate_training_data <- function(transaction_df, bic_name)
{
  transaction_df <- transaction_df %>% filter(award_or_idv == "AWARD") 
  transaction_df <- transaction_df %>% filter(bsp_delete_flag == "N")
  #read in BIC list
  vehicle_df <- read_tsv("Official_BIC_April_26_17.tsv")
  #capture coresponding ref_idv piids
  bic_ref_idv_piids <- vehicle_df %>% filter(official_bic_contract == bic_name) %>% select(reference_piid)%>% .$reference_piid
  #capture all transactions from the target BIC
  bic_df <- transaction_df %>% filter(reference_piid %in% bic_ref_idv_piids)
  #generate all psc/naics combinations
  bic_df$psc_naics_combo <- paste0(bic_df$product_or_service_code, bic_df$naics_code)
  proportions <<- proportional_spend(bic_df)
  transaction_df$psc_naics_combo <- paste0(transaction_df$product_or_service_code, transaction_df$naics_code)
  #capture psc_naics_combos not present in the bic
  negative_bic_df <- transaction_df %>% filter(! psc_naics_combo %in% bic_df$psc_naics_combo)
  #retain only the distinct combinations of psc and naics
  distinct_negative_psc_naics <- negative_bic_df %>% distinct(product_or_service_code, naics_code, .keep_all = TRUE)
  #code distinct_negative_pscs as false and bic_df as true
  bic_df$class <- TRUE
  distinct_negative_psc_naics$class <- FALSE
  training_candidates <- rbind(bic_df, distinct_negative_psc_naics)
  training_candidates <- training_candidates %>% select(product_or_service_code, naics_code, funding_department_name, dollars_obligated, class)
  training_candidates <- na.omit(training_candidates)
  attribute_names <- colnames(training_candidates)
  training_candidates[, attribute_names] <- lapply(training_candidates[, attribute_names], factor)
  training_candidates
}

generate_test_data <- function(transaction_df, bic_name)
{
  #read in transaction history
  transaction_df <- transaction_df %>% filter(award_or_idv == "AWARD") 
  transaction_df <- transaction_df %>% filter(bsp_delete_flag == "N")
  transaction_df <- transaction_df %>% filter(as.Date(date_signed) >= as.Date("2015-10-01") & as.Date(date_signed) <= as.Date("2016-09-30"))
  test_candidates <- transaction_df %>% select(product_or_service_code, naics_code, funding_department_name, dollars_obligated)
  test_candidates <- na.omit(test_candidates)
  attribute_names <- colnames(test_candidates)
  test_candidates[, attribute_names] <- lapply(test_candidates[, attribute_names], factor)
  test_candidates
}

produce_model <- function(df)
{
  set.seed(3304)
  df_size <- nrow(df)
  df <- sample_n(df, size = df_size * 0.5)
  df_count <- nrow(df)
  print(paste0("Using ", df_count, " rows to train"))
  train_count_max <- df_count * 0.75
  print(paste0(train_count_max, " training rows"))
  test_count_min <- train_count_max + 1
  print(paste0((df_count - test_count_min), " test rows"))
  df_train <- df[1:train_count_max, ]
  df_test <- df[test_count_min:df_count, ]
  test_labels_actual <- df_test %>% select(class) %>% .$class
  df_test <- df_test %>% select(-class)
  label_model <- naiveBayes(class ~ product_or_service_code+naics_code, data=df_train)
  fit_values <- fitted(label_model)
  predictions <- predict(label_model, df_test)
  #predictions
  final_table <- table(test_labels_actual, predictions)
  accuracy <- (sum(diag(final_table)))/sum(final_table)
  print(paste("Accuracy of fit is ", accuracy))
  #predictions
  label_model
}

produce_addressable_spend_prediction <- function(label_model, df)
{
  df_test <- df
  attribute_names <- colnames(df_test)
  df_test[, attribute_names] <- lapply(df_test[, attribute_names], factor)
  predictions <- predict(label_model, df_test)
  results <- cbind(df_test, predictions)
  results$dollars_obligated <- as.numeric(results$dollars_obligated)
  results
}

proportional_spend <- function(bic_df)
{
  unique_psc_naics_bic_combo <- unique(bic_df$psc_naics_combo)
  storage_df <- as.data.frame(NULL)
  i <- length(unique_psc_naics_bic_combo)
  value <- as.data.frame(NULL)
for(j in 1:i)
   {
         storage_df <- filter(bic_df, bic_df$psc_naics_combo == unique_psc_naics_bic_combo[j])
         storage_df$dollars_obligated <- as.numeric(storage_df$dollars_obligated)
         value[j,1] <- unique_psc_naics_bic_combo[j]
         value[j,2] <- sum(storage_df$dollars_obligated)
         colnames(value) <- c("psc_naics_combo", "Dollars Obligated")
}
  value$proportion <- value$`Dollars Obligated`/sum(bic_df$dollars_obligated)
value
}
