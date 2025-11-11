library(mice)
library(haven)
library(labelled)
library(dplyr)
library(ggplot2)

setwd(path)
df <- read_dta("processed_data_05_08_2025.dta")

df$K69_aggreg[df$K69_aggreg == ""] <- NA

freq(df$K69_aggreg)

#removing labels 
df <- df %>% mutate(across(everything(), haven::zap_labels))
df <- df %>% mutate(across(everything(), remove_var_label)) 

#########################################################
###################### Group 1 ##########################
#########################################################

binary_vars_g1 <- c("K1001", "K1003", "K1801", "K1802", "K1803", "K1804", 
                    "K1806", "K1807", "K1808", "K3901", "K3903", "K3906", 
                    "K4302", "K4303", "K4305", "K4306", "K5901", "K5902", 
                    "K5903", "K5905", "K5907", "K5908", "K5909", "K5910", 
                    "K5911", "K5912", "K49")

nominal_vars_g1 <- c("K07", "K1601", "K1602", "K3801", "K3802", "K69_aggreg")

ordinal_vars_g1 <- c("K09", "K1101", "K1102", "K1103", "K1104", "K1105", 
                     "K14", "K1509", "K1901", "K1902", "K1903", "K1904", 
                     "K2001", "K2002", "K2003", "K2004", "K2005", "K2007", 
                     "K23", "K25", "K2801", "K2802", "K2803", "K2901", 
                     "K2902", "K2903", "K2904", "K3001", "K3002", "K3003", 
                     "K3004", "K3005", "K3006", "K3007", "K3008", "K58", "K66_aggreg_num")

continuous_vars_g1 <- c("K4801", "K4802", "K4803", "K4804", "K4805", 
                        "K4806", "K4807")

binary_vars_g1b <- c("K03", "K3904")                   # use OPH2019TE_ratio
binary_vars_g1c <- c("K1002")                            # use Suuralueennimi
binary_vars_g1d <- c("K1805", "K5701")                   # use HOD2019TE_ratio and Householdslowestincome2019TR
binary_vars_g1e <- c("K3902", "K3905")                   # use HOD2019TE_ratio
binary_vars_g1f <- c("K4301", "K5904")                   # use Householdslowestincome2019TR
binary_vars_g1g <- c("K4304")                            # use Suuralueennimi, Kuntaryhmännimi, Eikotimaankieliset_ratio, PH2019TE_ratio, VD2019KO_ratio, Occupancyrate2019TE, and HOD2019TE_ratio
binary_vars_g1h <- c("K5705")                            # use HSmC2019TE_ratio
binary_vars_g1i <- c("K5906")                            # use OPH2019TE_ratio and Householdslowestincome2019TR

nominal_vars_g1b <- c("K3803")                           # use HOD2019TE_ratio
nominal_vars_g1c <- c("K05")                             # use Eikotimaankieliset_ratio
ordinal_vars_g1b <- c("K2006")                           # use Suuralueennimi

group1_impute_vars <- c(binary_vars_g1, nominal_vars_g1, ordinal_vars_g1, continuous_vars_g1,
                        binary_vars_g1b, binary_vars_g1c, binary_vars_g1d, binary_vars_g1e,
                        binary_vars_g1f, binary_vars_g1g, binary_vars_g1h, binary_vars_g1i,
                        nominal_vars_g1b, nominal_vars_g1c, ordinal_vars_g1b)

group1_aux_vars <- c("OPH2019TE_ratio", "Suuralueennimi", 
                     "HOD2019TE_ratio", "Householdslowestincome2019TR", "Kuntaryhmännimi", 
                     "Eikotimaankieliset_ratio", "PH2019TE_ratio", "VD2019KO_ratio", 
                     "Occupancyrate2019TE", "HSmC2019TE_ratio")

weight <- c("useweight")

group1_vars_all <- unique(c(group1_impute_vars, group1_aux_vars, weight))
df_group1 <- df[, group1_vars_all]

########## Changing binary and nominal into factors, ordinal into ordered factors and continuous to numeric ##############


binary_vars_all <- c(binary_vars_g1, binary_vars_g1b, binary_vars_g1c, binary_vars_g1d, 
                          binary_vars_g1e, binary_vars_g1f, binary_vars_g1g, 
                          binary_vars_g1h, binary_vars_g1i)

for (v in binary_vars_all) {
  df_group1[[v]] <- haven::as_factor(df_group1[[v]])
}

df_group1$Suuralueennimi <- as.factor(df_group1$Suuralueennimi)
df_group1$Kuntaryhmännimi <- as.factor(df_group1$Kuntaryhmännimi)

for (v in continuous_vars_g1) {
  df_group1[[v]] <- as.numeric(df_group1[[v]])
}

for (v in c(ordinal_vars_g1, ordinal_vars_g1b)) {
  df_group1[[v]] <- factor(df_group1[[v]], ordered = TRUE)
}

nominal_vars <- c(nominal_vars_g1, nominal_vars_g1b, nominal_vars_g1c)

for (v in nominal_vars) {  
  df_group1[[v]] <- haven::as_factor(df_group1[[v]])
}

########## Changing binary and nominal into factors, ordinal into ordered factors and continuous to numeric ##############


methods_g1 <- make.method(df_group1)
methods_g1[group1_aux_vars] <- ""


########## Seting imputation methods by type ##############
methods_g1[ c(binary_vars_g1, binary_vars_g1b, binary_vars_g1c,
              binary_vars_g1d, binary_vars_g1e, binary_vars_g1f, 
              binary_vars_g1g, binary_vars_g1h, binary_vars_g1i) ] <- "logreg"
methods_g1[ c(nominal_vars_g1, nominal_vars_g1b, nominal_vars_g1c) ] <- "polyreg"
methods_g1[ c(ordinal_vars_g1, ordinal_vars_g1b) ] <- "polr"
methods_g1[ continuous_vars_g1 ] <- "pmm"
methods_g1["K69_aggreg"] <- "cart" #because it produces error otherwise


pred_matrix_g1 <- make.predictorMatrix(df_group1)
pred_matrix_g1[,] <- 0

#creating matrix for all variables
for(v in group1_impute_vars) {
  pred_matrix_g1[v, setdiff(group1_impute_vars, v)] <- 1
  }

#adding extra auxiliary predictors where needed

#for K03 and K3904, adding OPH2019TE_ratio
for(v in binary_vars_g1b){
  pred_matrix_g1[v, "OPH2019TE_ratio"] <- 1
}

#for K1002, adding Suuralueennimi
for(v in binary_vars_g1c){
  pred_matrix_g1[v, "Suuralueennimi"] <- 1
}

#for K1805 and K5701, adding HOD2019TE_ratio and Householdslowestincome2019TR
for(v in binary_vars_g1d){
  pred_matrix_g1[v, "HOD2019TE_ratio"] <- 1
  pred_matrix_g1[v, "Householdslowestincome2019TR"] <- 1
}

#for K3902 and K3905, adding HOD2019TE_ratio
for(v in binary_vars_g1e){
  pred_matrix_g1[v, "HOD2019TE_ratio"] <- 1
}

#for K4301 and K5904, adding Householdslowestincome2019TR
for(v in binary_vars_g1f){
  pred_matrix_g1[v, "Householdslowestincome2019TR"] <- 1
}

#for K4304, adding Suuralueennimi, Kuntaryhmännimi, Eikotimaankieliset_ratio,
# PH2019TE_ratio, VD2019KO_ratio, Occupancyrate2019TE, and HOD2019TE_ratio
for(v in binary_vars_g1g){
  pred_matrix_g1[v, c("Suuralueennimi", "Kuntaryhmännimi", "Eikotimaankieliset_ratio",
                      "PH2019TE_ratio", "VD2019KO_ratio", "Occupancyrate2019TE", 
                      "HOD2019TE_ratio")] <- 1
}

#for K5705, adding HSmC2019TE_ratio
for(v in binary_vars_g1h){
  pred_matrix_g1[v, "HSmC2019TE_ratio"] <- 1
}

#for K5906, adding OPH2019TE_ratio and Householdslowestincome2019TR
for(v in binary_vars_g1i){
  pred_matrix_g1[v, "OPH2019TE_ratio"] <- 1
  pred_matrix_g1[v, "Householdslowestincome2019TR"] <- 1
}

#for K3803, adding HOD2019TE_ratio
for(v in nominal_vars_g1b){
  pred_matrix_g1[v, "HOD2019TE_ratio"] <- 1
}

#for K05, adding Eikotimaankieliset_ratio
for(v in nominal_vars_g1c){
  pred_matrix_g1[v, "Eikotimaankieliset_ratio"] <- 1
}

#for K2006, adding Suuralueennimi
for(v in ordinal_vars_g1b){
  pred_matrix_g1[v, "Suuralueennimi"] <- 1
}

############################# Imputatation for Group 1 #########################################

imp_group1 <- mice(df_group1, 
                      method = methods_g1, 
                      predictorMatrix = pred_matrix_g1, 
                      m = 1, 
                      seed = 123)
imp_group1_5m <- mice(df_group1, 
                   method = methods_g1, 
                   predictorMatrix = pred_matrix_g1, 
                   m = 5, 
                   seed = 123)

completed_data_group1 <- complete(imp_group1, action = "long", include = TRUE)
completed_data_group1_5m <- complete(imp_group1_5m, action = "long", include = TRUE)

dfSummary(completed_data_group1)


#########################################################
###################### Group 2 ##########################
#########################################################


binary_vars_g2 <- c("K2401", "K2402", "K2403", "K2404", "K2406", 
                    "K2407", "K2408", "K2409", "K2410")
nominal_vars_g2 <- c("K63")
continuous_vars_g2 <- c("K01")
binary_vars_g2b <- c("K2405")                # extra auxiliary: PH2019TE_ratio
nominal_vars_g2b <- c("K65")                  # extra auxiliary: Eikotimaankieliset_ratio and Occupancyrate2019TE

group2_impute_vars <- c(binary_vars_g2, nominal_vars_g2, continuous_vars_g2,
                        binary_vars_g2b, nominal_vars_g2b)


group2_aux_vars <- c("PH2019TE_ratio", "Eikotimaankieliset_ratio", "Occupancyrate2019TE")


group2_complete_vars <- c("K61","age")


group2_vars_all <- unique(c(group2_impute_vars, group2_aux_vars, group2_complete_vars))
df_group2 <- df[, group2_vars_all]



########## Changing binary and nominal into factors, ordinal into ordered factors and continuous to numeric ##############


binary_vars_g2_all<- c(binary_vars_g2, binary_vars_g2b)

for (v in binary_vars_g2_all) {
  df_group2[[v]] <- haven::as_factor(df_group2[[v]])  
}

for (v in continuous_vars_g2) {
  df_group2[[v]] <- as.numeric(df_group2[[v]])
}

for (v in group2_aux_vars) {
  df_group2[[v]] <- as.numeric(df_group2[[v]])
}

nominal_vars_g2_all <- c(nominal_vars_g2, nominal_vars_g2b)

for (v in nominal_vars_g2_all) {  
  df_group2[[v]] <- haven::as_factor(df_group2[[v]])
}



########## setting imputation method #############



methods_g2 <- make.method(df_group2)
methods_g2[group2_aux_vars] <- ""
methods_g2[group2_complete_vars] <- ""
methods_g2[binary_vars_g2] <- "logreg"
methods_g2[binary_vars_g2b] <- "logreg"
methods_g2[nominal_vars_g2] <- "polyreg"
methods_g2[nominal_vars_g2b] <- "polyreg"
methods_g2[continuous_vars_g2] <- "pmm"

pred_matrix_g2 <- make.predictorMatrix(df_group2)
pred_matrix_g2[,] <- 0

for(v in group2_impute_vars){
      pred_matrix_g2[v, setdiff(group2_impute_vars, v)] <- 1
  }
  

pred_matrix_g2["K2405", "PH2019TE_ratio"] <- 1
pred_matrix_g2["K65", c("Eikotimaankieliset_ratio", "Occupancyrate2019TE")] <- 1


############################# Imputatation for Group 2 ######################################### 

imp_group2 <- mice(df_group2, 
                      method = methods_g2, 
                      predictorMatrix = pred_matrix_g2, 
                      m = 1, 
                      seed = 456)

imp_group2_5m <- mice(df_group2, 
                   method = methods_g2, 
                   predictorMatrix = pred_matrix_g2, 
                   m = 5, 
                   seed = 456)

completed_data_group2 <- complete(imp_group2, action = "long", include = TRUE)
completed_data_group2_5m <- complete(imp_group2_5m, action = "long", include = TRUE)

freq(completed_data_group2)

###########################################################################################
################################# Inspecting MIs ##########################################
###########################################################################################

write.csv(completed_data_group1, "completed_data_group1.csv", row.names = FALSE)
write.csv(completed_data_group2, "completed_data_group2.csv", row.names = FALSE)

write.csv(completed_data_group2_5m, "completed_data_group2_5m.csv", row.names = FALSE)
write.csv(completed_data_group1_5m, "completed_data_group1_5m.csv", row.names = FALSE)

completed_data_group2_new <- completed_data_group2 %>% select(-.id, -.imp, -PH2019TE_ratio, -Eikotimaankieliset_ratio, -Occupancyrate2019TE)

completed_data_group2_5m_new <- completed_data_group2_5m %>% select(-.id, -.imp, -PH2019TE_ratio, -Eikotimaankieliset_ratio, -Occupancyrate2019TE)

combined_data <- cbind(completed_data_group1, completed_data_group2_new)
combined_data_5m <- cbind(completed_data_group1_5m, completed_data_group2_5m_new)

write.csv(combined_data, "combined_data.csv", row.names = FALSE)
write.csv(combined_data_5m, "combined_data_5m.csv", row.names = FALSE)



#list of imputed variables
imputed_vars <- c(group1_impute_vars, group2_impute_vars)

numeric_vars <- imputed_vars[sapply(combined_data[imputed_vars], is.numeric)]
categorical_vars <- imputed_vars[sapply(combined_data[imputed_vars], is.factor)]

#original data with NAs
df_with_NAs <- df %>% select(all_of(imputed_vars))  


summary_list_numeric <- list()

####################### Numeric variables ##############################################

# Analysis for 5 m 

for (var in numeric_vars) {
  for (imp_val in unique(combined_data_5m$.imp)) {
    
    if (imp_val == 0) {
      dataset <- df_with_NAs
      source_label <- "Original (with NAs)"
    } else {
      dataset <- combined_data_5m %>% filter(.imp == imp_val)
      source_label <- paste("Imputed (m =", imp_val, ")")
    }
    
    summary_numeric <- dataset %>%
      summarise(
        Mean = mean(.data[[var]], na.rm = TRUE),
        SD = sd(.data[[var]], na.rm = TRUE),
        Min = min(.data[[var]], na.rm = TRUE),
        Max = max(.data[[var]], na.rm = TRUE),
        NAs = sum(is.na(.data[[var]]))
      ) %>%
      mutate(Source = source_label, Variable = var)
    
    summary_list_numeric[[paste(var, imp_val, sep = "_")]] <- summary_numeric
  }
}

summary_table_numeric <- bind_rows(summary_list_numeric) %>%
  select(Variable, Source, Mean, SD, Min, Max, NAs) %>%
  arrange(Variable, desc(Source))

print(summary_table_numeric)

write.csv(summary_table_numeric, "imputation_summary_numeric_5m.csv", row.names = FALSE)


# Analysis for 1 m 

summary_list_numeric_1m <- list()

for (var in numeric_vars) {
  for (imp_val in unique(combined_data$.imp)) {
    
    if (imp_val == 0) {
      dataset <- df_with_NAs
      source_label <- "Original (with NAs)"
    } else {
      dataset <- combined_data %>% filter(.imp == imp_val)
      source_label <- paste("Imputed (m =", imp_val, ")")
    }
    summary_numeric_1m <- dataset %>%
      summarise(
        Mean = mean(.data[[var]], na.rm = TRUE),
        SD = sd(.data[[var]], na.rm = TRUE),
        Min = min(.data[[var]], na.rm = TRUE),
        Max = max(.data[[var]], na.rm = TRUE),
        NAs = sum(is.na(.data[[var]]))
      ) %>%
      mutate(Source = source_label, Variable = var)
    
    summary_list_numeric_1m[[paste(var, imp_val, sep = "_")]] <- summary_numeric_1m
  }
}

summary_table_numeric_1m <- bind_rows(summary_list_numeric_1m) %>%
  select(Variable, Source, Mean, SD, Min, Max, NAs) %>%
  arrange(Variable, desc(Source))


print(summary_table_numeric_1m)


write.csv(summary_table_numeric_1m, "imputation_summary_numeric_1m.csv", row.names = FALSE)

####################### Categorical variables ##############################################


# Analysis for 5 m


sink("imputation_categorical_summary_5m.txt") 

cat("\n**Categorical variables - 5m** \n\n")


for (var in categorical_vars) {
  
  cat("\n=========================\n")
  cat("\n Variable:", var, "\n")
  cat("=========================\n\n")
  
  original_counts <- df_with_NAs %>%
    filter(!is.na(.data[[var]])) %>%
    count(.data[[var]]) %>%
    mutate(Percentage = round(100 * n / sum(n), 2)) %>%
    arrange(desc(n))
  
  cat("\n **Original data (with NAs removed)**\n")
  for (i in 1:nrow(original_counts)) {
    cat("   ", original_counts[[var]][i], ": ", original_counts$n[i], "(", original_counts$Percentage[i], "%)\n")
  }

  for (imp_val in unique(combined_data_5m$.imp)) {
    
    if (imp_val == 0) next

    imputed_counts <- combined_data_5m %>%
      filter(.imp == imp_val) %>%
      count(.data[[var]]) %>%
      mutate(Percentage = round(100 * n / sum(n), 2)) %>%
      arrange(desc(n))
    
    cat("\n **Imputed data (m =", imp_val, ")**\n")
    for (i in 1:nrow(imputed_counts)) {
      cat("   ", imputed_counts[[var]][i], ": ", imputed_counts$n[i], "(", imputed_counts$Percentage[i], "%)\n")
    }
  }
  
  cat("\n-------------------------------------------------\n")
}

sink()

file.show("imputation_categorical_summary_5m.txt")



########################### Analysis for 1 m ################################################


sink("imputation_categorical_summary_1m.txt") 

cat("\n **Categorical variables - 1m** \n\n")


for (var in categorical_vars) {
  
  cat("\n=========================\n")
  cat("\n Variable:", var, "\n")
  cat("=========================\n\n")
  

  original_counts <- df_with_NAs %>%
    filter(!is.na(.data[[var]])) %>%
    count(.data[[var]]) %>%
    mutate(Percentage = round(100 * n / sum(n), 2)) %>%
    arrange(desc(n))
  
  cat("\n **Original data (with NAs removed)**\n")
  for (i in 1:nrow(original_counts)) {
    cat("   ", original_counts[[var]][i], ": ", original_counts$n[i], "(", original_counts$Percentage[i], "%)\n")
  }
  
  for (imp_val in unique(combined_data$.imp)) {
    
    if (imp_val == 0) next
    

    imputed_counts <- combined_data %>%
      filter(.imp == imp_val) %>%
      count(.data[[var]]) %>%
      mutate(Percentage = round(100 * n / sum(n), 2)) %>%
      arrange(desc(n))
    
    cat("\n **Imputed data (m =", imp_val, ")**\n")
    for (i in 1:nrow(imputed_counts)) {
      cat("   ", imputed_counts[[var]][i], ": ", imputed_counts$n[i], "(", imputed_counts$Percentage[i], "%)\n")
    }
  }
  
  cat("\n-------------------------------------------------\n")
}

sink()  

file.show("imputation_categorical_summary_1m.txt")

