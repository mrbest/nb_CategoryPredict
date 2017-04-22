require(tree)
require(e1071)
require(kernlab)
require(dplyr)
require(rpart)
require(readr)

main <- function()
{
  transation_df <<- read_tsv("FPDS_ATOM_GW_19APR17.tsv")
  vehicle_df <<- read_csv("Contract Vechicle Reference.csv")
  psc_trans_table <<- read_csv("pscTransTable.csv")
  prepped_df <- prep("JANSAN")
}


get_contract_soln_piids <- function(contract_name)
{
  idv_piids <- vehicle_df %>% filter(contract_vehicle == contract_name) %>% select(reference_piid) %>% .$reference_piid
  idv_piids
}

#note: one can test the validity of the contract vehicle df by checking to make sure the size of each object returned 
#from get_contract_solution() is at most 1

get_contract_solution <- function(reference_piid_target)
{
   contract_solution <- vehicle_df %>% filter(reference_piid == reference_piid_target) %>% select(contract_vehicle) 
   return_val <- as.character(contract_solution[1,1]) #I have specified posn [1,1] to select the first occurence if there are multiple hits
   return_val
}



prep <- function(reference_piids)
{
  #target_pscs <- vehicle_df %>% filter(Level_1_Category == category) %>% select(`4_Digit_PSC`) %>% .$`4_Digit_PSC`
  load_df <- transation_df %>% filter( reference_piid  %in% reference_piids) #subset to a specific contract soln
  load_df$contract_soln <- unlist(lapply(load_df$reference_piid, get_contract_solution)) #get contract solns
  load_df <- load_df %>% filter(!is.na(contract_soln)) #get rid of rows with no contract solution
  load_df <- load_df %>% select(product_or_service_code, naics_code, funding_agency_name, funding_department_name, contracting_office_name, vendor_duns_number, contract_soln, dollars_obligated)
  load_df <- na.omit(load_df)
  #most important to note: We need to convert all columns to factors for training and predicting to work
  attribute_names <- colnames(load_df)
  load_df[, attribute_names] <- lapply(load_df[, attribute_names], factor)
  load_df
}


train_on_trans <- function(df)
{
  set.seed(1000)
  df <- sample_n(df, size = 10000)
  df_count <- nrow(df)
  print(paste0("Using ", df_count, " rows"))
  train_count_max <- df_count * 0.75
  print(paste0(train_count_max, " training rows"))
  test_count_min <- train_count_max + 1
  print(paste0((df_count - test_count_min), " test rows"))
  df_train <- df[1:train_count_max, ]
  df_test <- df[test_count_min:df_count, ]
  test_labels_actual <- df_test %>% select(contract_soln) %>% .$contract_soln
  df_test <- df_test %>% select(-contract_soln)
  label_model <- naiveBayes(contract_soln ~ product_or_service_code+naics_code+funding_agency_name+funding_department_name+contracting_office_name+vendor_duns_number, data=df_train)
  fit_values <- fitted(label_model)
  predictions <- predict(label_model, df_test)
  #predictions
  final_table <- table(test_labels_actual, predictions)
  accuracy <- (sum(diag(final_table)))/sum(final_table)
  print(paste("Accuracy of fit is ", accuracy))
  predictions
}