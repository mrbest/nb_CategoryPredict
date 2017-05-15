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
  fpds_df <- read_tsv("FPDS_ATOM_GW_19APR17.tsv")
  print("Generating training data")
  testing_training_data <<- generate_training_data(fpds_df, "OS3")
  new_proportions <<- proportional_spend(testing_training_data)
}

generate_training_data <- function(transaction_df, bic_name)
{
  transaction_df <- transaction_df %>% filter(award_or_idv == "AWARD") 
  transaction_df <- transaction_df %>% filter(bsp_delete_flag == "N")
  vehicle_df <- read_tsv("BIC List - Official - detail.tsv")
  bic_ref_idv_piids <- vehicle_df %>% filter(Contract == bic_name) %>% select(`Contract ID`)%>% .$`Contract ID`
  transaction_df <- transaction_df %>% filter(reference_piid %in% bic_ref_idv_piids)
  transaction_df$psc_naics_combo <- paste0(transaction_df$product_or_service_code, sep = "_", transaction_df$naics_code)
  proportions <<- proportional_spend(transaction_df)
  transaction_df <- transaction_df %>% select(product_or_service_code, naics_code, funding_department_name, dollars_obligated, psc_naics_combo)
  transaction_df <- na.omit(transaction_df)
  transaction_df
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