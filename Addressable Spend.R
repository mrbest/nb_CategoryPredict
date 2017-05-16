require(tree)
require(e1071)
require(kernlab)
require(dplyr)
require(rpart)
require(readr)
require(kknn)

main <- function()
{
  options(scipen=999)
  print("Loading data")
  #fpds_df <<- read_tsv("FPDS_ATOM_GW_19APR17.tsv")
  vehicle_df <<- read_tsv("BIC List - Official - detail.tsv")
  bic_names <<- unique(vehicle_df$Contract)
  #Remove HCATS --- Remove if working with transactions that include HCATS values!!
  bic_names <- bic_names[-3]
  print("Generating training data")
  fpds_df <- fpds_df %>% filter(as.Date(date_signed) >= as.Date("2015-10-01") & as.Date(date_signed) <= as.Date("2016-09-30"))
  m <- length(bic_names)
  addressable_spend_values <- as.data.frame(NULL)
  for(k in 1:m)
  {
  testing_training_data <<- generate_training_data(fpds_df, bic_names[k])
  fpds_df$psc_naics_combo <- paste0(fpds_df$product_or_service_code, sep = "_", fpds_df$naics_code)
  filtered_fpds <- filter(fpds_df, fpds_df$psc_naics_combo %in% testing_training_data$psc_naics_combo)
  filtered_fpds <- distinct(filtered_fpds)
  new_proportions <<- calculate_predicted_obligation(filtered_fpds)
  addressable_spend_values[k,1] <- bic_names[k]
  addressable_spend_values[k,2] <- sum(new_proportions$Dollars_Predicted)
  print(paste0("Working on: ", bic_names[k]))
  }
  colnames(addressable_spend_values) <- c("BIC", "Dollars Predicted")
  addressable_spend_values
}

generate_training_data <- function(transaction_df, bic_name)
{
  transaction_df <- transaction_df %>% filter(award_or_idv == "AWARD") 
  transaction_df <- transaction_df %>% filter(bsp_delete_flag == "N")
  bic_ref_idv_piids <- vehicle_df %>% filter(Contract == bic_name) %>% select(`Contract ID`)%>% .$`Contract ID`
  transaction_df <- transaction_df %>% filter(reference_piid %in% bic_ref_idv_piids)
  transaction_df$psc_naics_combo <- paste0(transaction_df$product_or_service_code, sep = "_", transaction_df$naics_code)
  proportions <<- produce_training_proportion(transaction_df)
  transaction_df <- transaction_df %>% select(product_or_service_code, naics_code, funding_agency_name, dollars_obligated, psc_naics_combo)
 # transaction_df <- na.omit(transaction_df)
  transaction_df
  }

produce_training_proportion <- function(bic_df)
{
  unique_psc_naics_bic_combo <<- unique(bic_df$psc_naics_combo)
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

calculate_predicted_obligation <- function(matched_df)
{
  value <- sum(matched_df$dollars_obligated)
  proportions_new <- distinct(matched_df, psc_naics_combo, .keep_all = TRUE)
  proportions_new$proportions <- proportions$proportion
  proportions_new$Dollars_Predicted <- proportions_new$proportions * value
  proportions_new <- proportions_new %>% select(psc_naics_combo, proportions, Dollars_Predicted)
  proportions_new
}
