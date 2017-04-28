require(tree)
require(e1071)
require(kernlab)
require(dplyr)
require(rpart)
require(readr)
require(kknn)

label_contract_solutions <- function(contract_soln)
{
  transaction_df <<- read_tsv("FPDS_ATOM_GW_19APR17.tsv")
  transaction_df <<- transaction_df %>% filter(as.Date(date_signed) >= as.Date("2015-10-01") & as.Date(date_signed) <= as.Date("2016-09-30"))
  vehicle_df <<- read_csv("BIC_IDV_Table.csv")
  #remove all transactions that dont map to BICs as they will not contribute to the training set
  training_df <<- transaction_df %>% filter(reference_piid %in% vehicle_df$reference_piid) 
  attribute_names <- colnames(transaction_df)
  #Use as.factor on transaction_df -- cannot do it on the list we create below, so we do it now.
  transaction_df[, attribute_names] <- lapply(transaction_df[, attribute_names], factor)
  prepped_df <<- prep()
  #create test df we use to produce the model
  binary_prepped_df <<- binary_classifier_df_Prep("OS3", prepped_df)
  #create model
  label_model <- produce_model(binary_prepped_df)
  #setting the size of each new df
  n <- 300000
  nr <- nrow(transaction_df)
  #splitting the large transaction_df into multiple dfs in a list, each new df will be size n
  store <- split(transaction_df, rep(1:ceiling(nr/n), each = n, length.out = nr))
  j = length(store)
  dollars_obligated <- 0
  
  #running a for-loop for the length of the list, with output being amount of dollars obligated expected in smaller dataset
  for(i in 1:j)
  {
    results <<- produce_addressable_spend_prediction(label_model, store[1])
    dollars_obligated = dollars_obligated + results
  }
#at end of for-loop, dollars_obligated should be total addressable spend for that BIC for the full transaction df. 
  
  print(paste("Addressable Spend is ", dollars_obligated))
  
}

binary_classifier_df_Prep <- function(contract_solution_target, prepped_df)
{
  prepped_df$this_vehicle <- as.logical(prepped_df$contract_soln == contract_solution_target)
  prepped_df$this_vehicle <- as.factor(prepped_df$this_vehicle)
  prepped_df
}


get_contract_soln_piids <- function(contract_name)
{
  idv_piids <- vehicle_df %>% filter(contract_vehicle == contract_name) %>% select(reference_piid) %>% .$reference_piid
  idv_piids
}

#note: one can lstest the validity of the contract vehicle df by checking to make sure the size of each object returned 
#from get_contract_solution() is at most 1

get_contract_solution <- function(reference_piid_target)
{
  contract_solution <- vehicle_df %>% filter(reference_piid == reference_piid_target) %>% select(official_bic_contract) 
  return_val <- as.character(contract_solution[1,1]) #I have specified posn [1,1] to select the first occurence if there are multiple hits
  return_val
}


prep <- function()
{
  #target_pscs <- vehicle_df %>% filter(Level_1_Category == category) %>% select(`4_Digit_PSC`) %>% .$`4_Digit_PSC`
  #load_df <- transation_df %>% filter( reference_piid  %in% reference_piids) #subset to a specific contract soln
  load_df <- training_df
  
  load_df$contract_soln <- unlist(lapply(load_df$reference_piid, get_contract_solution)) #get contract solns
  load_df <- load_df %>% filter(!is.na(contract_soln)) #get rid of rows with no contract solution
  load_df <- load_df %>% select(product_or_service_code, naics_code, funding_agency_name, funding_department_name, contracting_office_name, vendor_duns_number, contract_soln)
  load_df <- na.omit(load_df)
  #load_df <- load_df %>% 
  #most important to note: We need to convert all columns to factors for training and predicting to work
  
  attribute_names <- colnames(load_df)
  load_df[, attribute_names] <- lapply(load_df[, attribute_names], factor)
  load_df
}


produce_model <- function(df)
{
  set.seed(255)
  df_size <- nrow(df)
  if(df_size < 10000)
  {df <- sample_n(df, size = df_size * 0.80)}
  else
  {
    df <- sample_n(df, size = df_size * 0.5)
  }
  df_count <- nrow(df)
  print(paste0("Using ", df_count, " rows"))
  train_count_max <- df_count * 0.75
  print(paste0(train_count_max, " training rows"))
  test_count_min <- train_count_max + 1
  print(paste0((df_count - test_count_min), " test rows"))
  df_train <- df[1:train_count_max, ]
  df_test <- df[test_count_min:df_count, ]
  test_labels_actual <- df_test %>% select(this_vehicle) %>% .$this_vehicle
  df_test <- df_test %>% select(-this_vehicle)
  #label_model <- naiveBayes(this_vehicle ~ product_or_service_code+naics_code+funding_agency_name+vendor_duns_number, data=df_train)
  #label_model <- svm(this_vehicle ~ product_or_service_code+naics_code+funding_agency_name+vendor_duns_number ,data=df_train, probability=TRUE)
  label_model <- rpart(this_vehicle ~ product_or_service_code+naics_code+funding_agency_name+vendor_duns_number, data=df_train, method = "class")
  fit_values <- fitted(label_model)
  predictions <- predict(label_model, df_test)
  #predictions
  #final_table <- table(test_labels_actual, predictions)
  #accuracy <- (sum(diag(final_table)))/sum(final_table)
  #print(paste("Accuracy of fit is ", accuracy))
  #predictions
  label_model
}


produce_addressable_spend_prediction <- function(label_model, df)
{
    #creating predictions
    yes_soln <- predict(label_model, df_test)
    #binding it to original smaller data-set
    df <- cbind(df, yes_soln)
    #subsetting it to only those rows that ARE predicted as being BIC
    results <- subset(df, df$yes_soln == TRUE)
    #summing dollars obligated
    results$dollars_obligated <- as.numeric(results$dollars_obligated)
    dollars <- sum(results$dollars_obligated)
    dollars
}