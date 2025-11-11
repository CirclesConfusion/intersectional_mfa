
library(pacman)
p_load(tidytable, gridExtra, rstatix, FSA, survey, jtools, dplyr, haven, ggplot2, reshape2, knitr, kableExtra, summarytools, wCorr, corrplot, BiocManager, ggcorrplot, FactoMineR, labelled, factoextra, rcompanion, topicmodels, ggpubr, stats, tidyr, textshape, effsize, purr, cluster)


setwd(path)
combined_data <- read.csv("combined_data.csv")

summary(combined_data)

#To work only on the imputed dataset
imputed_df <- subset(combined_data, .imp == 1)
imputed_df <- select(imputed_df, -c(.imp,.id))

dfSummary(imputed_df)


###################################################################################################
####################### Investigating variable groups structure ###################################
###################################################################################################


#I decide to create Spearman correlation matrix, just to see rough associations of the data,
# although it may be not the most accurate approach for the diversity of variable types in the set


vars <- c(
  "K03","K05","K07","K09","K1001","K1002","K1003","K1101","K1102","K1103","K1104","K1105",
  "K14","K1509","K1601","K1602","K1801","K1802","K1803","K1804","K1805","K1806","K1807",
  "K1808","K1901","K1902","K1903","K1904","K2001","K2002","K2003","K2004","K2005","K2006",
  "K2007","K23","K25", "K3001",
  "K3002","K3003","K3004","K3005","K3006","K3007","K3008","K3801","K3802","K3803","K3901",
  "K3902","K3903","K3904","K3905","K3906","K4301","K4302","K4303","K4304","K4305","K4306",
  "K4801","K4802","K4803","K4804","K4805","K4806","K4807","K49","K5701","K5705","K58",
  "K5901","K5902","K5903","K5904","K5905","K5906","K5907","K5908","K5909","K5910","K5911", "K66_aggreg_num", "K69_aggreg"
)


df_sub <- imputed_df %>%
  select(all_of(vars), useweight)

#Transforming K69_aggreg as numeric temporarily just for a rough analysis
df_sub$K69_aggreg_code <- as.integer(factor(df_sub$K69_aggreg))
vars[vars == "K69_aggreg"] <- "K69_aggreg_code"


df_sub[vars] <- lapply(df_sub[vars], function(x) as.numeric(x))


n_vars <- length(vars)
corr_matrix <- matrix(NA, nrow = n_vars, ncol = n_vars,
                      dimnames = list(vars, vars))


for(i in seq_along(vars)) {
  for(j in seq(i, n_vars)) {
    if(i == j) {
     
      corr_matrix[i, j] <- 1
    } else {
      
      tmp <- df_sub %>%
        select(all_of(vars[i]), all_of(vars[j]), useweight)
      
      wcor <- weightedCorr(x       = tmp[[ vars[i] ]],
                           y       = tmp[[ vars[j] ]],
                           w       = tmp[["useweight"]],
                           method  = "spearman")
    
      corr_matrix[i, j] <- wcor
      corr_matrix[j, i] <- wcor
    }
  }
}



p<- ggcorrplot(corr_matrix, 
           hc.order = TRUE, 
           method = "square", 
           type = "lower", 
           lab = TRUE, 
           lab_size = 2.5,  
           tl.cex = 7.5,
           outline.color = "white",
           title = "Weighted Spearman Correlation Matrix")
p + theme(aspect.ratio = 0.5)

#Alphabetical display

var_names <- rownames(corr_matrix)

sorted_names <- sort(var_names)

corr_matrix_alpha <- corr_matrix[sorted_names, sorted_names]

b<- ggcorrplot(corr_matrix_alpha, 
               hc.order = FALSE, 
               method = "square", 
               #type = "lower", 
               lab = TRUE, 
               lab_size = 2.5,  
               tl.cex = 7.5,
               outline.color = "white",
               title = "Weighted Spearman Correlation Matrix")
b + theme(aspect.ratio = 0.5)



# Only above certain level of correlation

corr_thresh <- corr_matrix_alpha
corr_thresh[abs(corr_thresh) < 0.3] <- NA

c<- ggcorrplot(corr_thresh, 
               hc.order = FALSE, 
               method = "square", 
               #type = "lower", 
               lab = TRUE, 
               lab_size = 2.5,  
               tl.cex = 7.5,
               outline.color = "white",
               title = "Weighted Spearman Correlation Matrix")
c + theme(aspect.ratio = 0.5)


#So, I split the dataset into following groups:

#K03 K1601 K1602 K14 (binary and ordinal variables)
#K05 (nominal variable)
#K07 (ordinal variable)

#K1001 K1002 K1003 K09 (binary and ordinal variables)
#K1101 K1102 K1103 K1104 K1105 K23 K25 (ordinal variables)

#OR

#K1001 K1002 K1003 K09 K1101 K1102 K1103 K1104 K1105 K23 K25 (binary and ordinal variables)

#K1509 (ordinal variable)
#K1801 K1802 K1803 K1804 K1805 K1806 K1807 K1808 (binary variables)
#K1901 K1902 K1903 K1904 (ordinal variables)
#K2001 K2002 K2003 K2004 K2005 K2006 K2007 (ordinal variables)
#K3001 K3002 K3003 K3004 K3005 K3006 K3007 K3008 (ordinal variables)
#K3801 K3802 K3803 (nominal variables)
#K3901 K3902 K3903 K3904 K3905 K3906 (binary variables)
#K4301 K4302 K4303 K4304 K4305 K4306 (binary variables)
#K4801 K4802 K4803 K4804 K4805 K4806 K4807 (continuous variables)
#K49 (binary variable)
#K5701 K5705 K58 K5901 K5902 K5903 K5904 K5905 K5906 K5907 K5908 K5909 K5910 K5911 (binary and categorical variables)
#K66_aggreg_num (ordinal variable)
#K69_aggreg (nominal variable)


desired_order <- c(
  "K03", "K1601", "K1602", "K14",
  "K05",
  "K07",
  "K1001", "K1002", "K1003", "K09", "K1101", "K1102", "K1103", "K1104", "K1105", "K23", "K25",
  "K1509",
  "K1801", "K1802", "K1803", "K1804", "K1805", "K1806", "K1807", "K1808",
  "K1901", "K1902", "K1903", "K1904",
  "K2001", "K2002", "K2003", "K2004", "K2005", "K2006", "K2007",
  "K3001", "K3002", "K3003", "K3004", "K3005", "K3006", "K3007", "K3008",
  "K3801", "K3802", "K3803",
  "K3901", "K3902", "K3903", "K3904", "K3905", "K3906",
  "K4301", "K4302", "K4303", "K4304", "K4305", "K4306",
  "K4801", "K4802", "K4803", "K4804", "K4805", "K4806", "K4807",
  "K49",
  "K5701", "K5705", "K58", "K5901", "K5902", "K5903", "K5904", "K5905", "K5906", "K5907", "K5908", "K5909", "K5910", "K5911",
  "K66_aggreg_num",
  "K69_aggreg",
  "K2401", "K2402", "K2403", "K2404", "K2405", "K2406", "K2407", "K2408", "K2409", "K2410", "K61", "K63", "K65",
  "age", "K01",
  "useweight"
)

#reordering columns in imputed_df to match the desired sequence
imputed_df <- imputed_df[ , desired_order]

names(imputed_df)

before_MFA<-imputed_df

imputed_df<-before_MFA


write.csv(imputed_df, "imputed_df.csv", row.names = FALSE)

###################################################################################################
############################################ MFA ##################################################
###################################################################################################




first_model <- imputed_df

############################
# 1 - 18 variable groups
############################
orig_blocks <- list(
  c("K03","K1601","K1602","K14"),    # 1 mixed "Politics & Language",
  "K05",                            # 2 nominal "Housing",
  "K07",                            # 3 ordinal "Moving",
  c("K1001","K1002","K1003","K09"), # 4 mixed "Poverty",
  c("K1101","K1102","K1103","K1104","K1105","K23","K25"), #5 ordinal  "Satisfaction & Health",
  "K1509",                          #6 ordinal "Neighbourhood",
  paste0("K180",1:8),               #7 binary "Social participation",
  paste0("K190",1:4),               #8 ordinal "Culture",
  paste0("K200",1:7),               #9 ordinal "Discrimination",
  paste0("K300",1:8),               #10 ordinal "Depression",
  paste0("K380",1:3),               #11 nominal "Sufficient healthcare",
  paste0("K390",1:6),               #12 binary "Healthcare limits",
  paste0("K430",1:6),               #13 binary "Social services",
  paste0("K480",1:7),               #14 continuous "Relations",
  "K49",                            #15 binary "Giving care",
  c("K5701","K5705","K58",          #16 mixed "Internet"
    paste0("K590",1:9),"K5910","K5911"),
  c("K66_aggreg_num"),              #17 ordinal "Education"
  c("K69_aggreg")                   #18 nominal "Economic activity"
)



#   "Politics & Language",
#   "Housing",
#   "Moving",
#   "Poverty",
#   "Satisfaction & Health",
#   "Neighbourhood",
#   "Social participation",
#   "Culture",
#   "Discrimination",
#   "Depression",
#   "Sufficient healthcare",
#   "Healthcare limits",
#   "Social services",
#   "Relations",
#   "Giving care",
#   "Internet"
#   "Education"
#   "Economic activity

#  socio‑demo supplementary block:
socio_demographics_qual <- c(
  "K2401","K2402","K2403","K2404","K2405","K2406",
  "K2407","K2408","K2409","K2410","K61","K63","K65"
)
socio_demographics_quant <- c(
  "age","K01"
)


############################
# 2 - separating to numeric and categorical
############################
num_vars <- c(
  "K07","K09",paste0("K110",1:5),"K23","K25","K14","K1509",
  paste0("K190",1:4),paste0("K200",1:7),paste0("K300",1:8),
  paste0("K480",1:7), "K58","K66_aggreg_num","age","K01"
)
cat_vars <- c(
  "K03","K05",paste0("K100",1:3),"K1601","K1602",
  paste0("K180",1:8),paste0("K380",1:3),
  paste0("K390",1:6),paste0("K430",1:6),
  "K49","K5701","K5705",paste0("K590",1:9),"K5910","K5911",
  paste0("K240",1:9),"K2410","K61","K63","K65", "K69_aggreg"
)

# Ordinal-as-numeric 
safe_as_numeric <- function(x) {
  if (is.ordered(x)) return(as.integer(x))
  if (is.factor(x)) {
    xs <- as.character(x)
    ok <- grepl("^[-+]?[0-9]*\\.?[0-9]+$", xs) | is.na(xs)
    if (!all(ok)) stop("Non-numeric labels in a would-be numeric factor: ",
                       paste(unique(xs[!ok]), collapse=", "))
    return(as.numeric(xs))
  }
  as.numeric(x)
}

#coercing once:

imputed_df[ intersect(cat_vars, names(imputed_df)) ] <-
  lapply(imputed_df[ intersect(cat_vars, names(imputed_df)) ],
         function(x) factor(x, exclude = NULL))

imputed_df[ intersect(num_vars, names(imputed_df)) ] <-
  lapply(imputed_df[ intersect(num_vars, names(imputed_df)) ],
         safe_as_numeric)

############################
# 3 - buidling variable groups
############################

blocks_mixed  <- list()
types_mixed   <- character(0)
names_mixed   <- character(0)

for (j in seq_along(orig_blocks)) {
  blk_name <- c(
    "Politics & Language","Housing","Moving","Poverty",
    "Satisfaction & Health","Neighbourhood","Social participation",
    "Culture","Discrimination",
    "Depression","Sufficient healthcare","Healthcare limits",
    "Social services","Relations","Giving care","Internet","Education",
    "Economic activity", "Education"
  )[j]
  
  blk <- orig_blocks[[j]]
  nums <- intersect(blk, num_vars)
  cats <- intersect(blk, cat_vars)
  
  if (length(nums) > 0 && length(cats) > 0) {
    # mixed → two groups
    blocks_mixed[[length(blocks_mixed)+1]] <- nums
    types_mixed   <- c(types_mixed, "s")
    names_mixed   <- c(names_mixed, paste0(blk_name," (quant)"))
    
    blocks_mixed[[length(blocks_mixed)+1]] <- cats
    types_mixed   <- c(types_mixed, "n")
    names_mixed   <- c(names_mixed, paste0(blk_name," (qual)"))
  } else {
    # pure block
    blocks_mixed[[length(blocks_mixed)+1]] <- blk
    types_mixed   <- c(types_mixed, if (length(nums)>0) "s" else "n")
    names_mixed   <- c(names_mixed, blk_name)
  }
}

active_vars <- unlist(blocks_mixed, use.names = FALSE)
stopifnot(!any(duplicated(active_vars)))
group_sizes <- lengths(blocks_mixed)
group_types <- types_mixed
name_groups <- names_mixed

stopifnot(
  length(group_sizes) == length(group_types),
  sum(group_sizes)    == length(active_vars),
  length(name_groups) == length(group_sizes)
)

############################
# 4 - adding supplementary variable group
############################

sup_sizes <- c(length(socio_demographics_qual), length(socio_demographics_quant))
sup_types <- c("n", "s")
sup_names <- c("SocioDemographics (qual)", "SocioDemographics (quant)")

group_all <- c(group_sizes, sup_sizes)
type_all  <- c(group_types, sup_types)
name_all  <- c(name_groups, sup_names)


sup_index <- (length(group_all)-1):length(group_all)




############################
# Runing MFA
############################
res_mfa_mixed <- MFA(
  base           = imputed_df[, c(active_vars, socio_demographics_qual, socio_demographics_quant)],
  group          = group_all,
  type           = type_all,
  name.group     = name_all,
  row.w          = imputed_df$useweight,
  num.group.sup  = sup_index,
  graph          = FALSE,
  ncp            = 15
)
first_mfa <- res_mfa_mixed

plot(res_mfa_mixed, choix = "ind")
plot(res_mfa_mixed, choix = "var")
plot(res_mfa_mixed, choix = "group")
plot(res_mfa_mixed, choix = "axes")  

plot(res_mfa_mixed,
     choix     = "ind", 
     invisible = c("ind", "ind.sup"))


#############
# Ploting both categorical and quantiative variables

coord_num   <- as.data.frame(res_mfa_mixed$quanti.var$coord[, 1:2])
coord_cat   <- as.data.frame(res_mfa_mixed$quali.var$coord[, 1:2])
coord_num$type <- "quantitative"
coord_cat$type <- "categorical"
coords <- rbind(coord_num, coord_cat)


ggplot(coords, aes(Dim.1, Dim.2, colour = type)) +
  geom_point() +
  geom_text(aes(label = rownames(coords)), hjust = -0.2) +
  coord_equal() +
  theme_minimal()

############################
#inspecting the model


res_mfa_mixed$eig

barplot(res_mfa_mixed$eig[, 1], names.arg = seq_len(nrow(res_mfa_mixed$eig)))

factoextra::fviz_eig(res_mfa_mixed, addlabels = TRUE)

#having 21 groups, the eigenvalue of a first component is 4.495, which gives a ratio 0.21. 
#Perfect setup would be eigenvalue 21
#As a general comment, overall the eigenvalues are smaller than in usual PCA analysis - it is influenced by 
#MCA as a part of my MFA, which usually gives smaller values (Jérôme Pagès, 2015)


res_mfa_mixed$group

#coord[j, s] (group–axis coordinate) = relationship/alignment of group j with axis s. 
#In the Jérôme Pagès’s terms, the coordinate of a group on axis s is a “relationship 
#measurement” (max 1 on the relationship square). higher coord ⇒ the group is more 
#aligned/correlated with axis. Use this to say which groups are aligned with an axis.
res_mfa_mixed$group$coord

#_________________________________________
#calculating Quality of representation of cloud NJ
axes <- c(5)                                 # picking the plane/axis shown
num <- colSums(res_mfa_mixed$group$coord[, axes, drop = FALSE]^2)
den <- sum(res_mfa_mixed$group$dist2)
plane_ratio_NJ <- sum(num) / den       #Quality of representation of cloud NJ from (Jérôme Pagès, 2015, p.156), Quality(NJ) tells how well the plane represents the cloud of groups; It’s a single, 0–1 score for the plane: higher ⇒ the plane shows a big share of all groups together; lower ⇒ the groups’ structure lives beyond that plane. 
plane_ratio_NJ                         # bigger ⇒ the plane shows more of all groups together
#__________________________________
#cos2[j, s] (squared cosine) = quality of representation of group j on axis s 
#(or on a plane if you sum across axes). It’s computed by MFA and lives in 
#res$group$cos2. Use it to say how well a group is shown on an axis/plane.
res_mfa_mixed$group$cos2
#_____________
#contrib[j, s] (group contribution to axis s)
#In the FactoMineR output for groups, “the raw inertias are in coord … 
# and the percentages in contrib.” In other words, contrib gives the 
#percentage share (sums to 100% across groups for an axis) of how much 
#each group builds/constructs that axis. Use this when you want to say 
#what builds the axis. 
res_mfa_mixed$group$contrib



#_____________________________________________________
#investigating relations between groups

#Lg(Kj, Kl) — relationship (link) between two groups of variables
#It’s defined via the scalar product of the groups’ Gram matrices with the 
# MFA weighting; it can exceed 1 and has no fixed upper bound. It grows when 
# the two groups share more common directions and when those directions carry 
# high inertia. It is 0 iff every variable of one group is uncorrelated with 
# every variable of the other. 

res_mfa_mixed$group$Lg

#RV(Kj, Kl) — Escoufier’s RV coefficient between two groups
# When the groups are standardised in the groups’ space, the scalar product 
# behaves like a cosine, which gives RV; it is bounded in [0,1] and equals 
# 1 when the two groups’ clouds of individuals are homothetic. It is 0 under 
# complete lack of cross-group correlations.

res_mfa_mixed$group$RV

#no considerable relations between groups, except for some examples of groups split 
#into quant and qual (as they are covering the same topic, they will relate to each other)

#As a result, group structure will remain as it is. Possible changes will only involve which groups are included in the MFA


#_______________________________________________________
#Investigating groups to select those I will keep and those I will drop



mfa_group_axis_table <- function(res, axis = 1, sort_by = c("coord","cos2","contrib"),
                                 top_n = NULL, digits = 3) {
  sort_by <- match.arg(sort_by)
  stopifnot(axis >= 1, axis <= ncol(res$group$coord))
  

  coord  <- res$group$coord[,  axis]            # relationship/alignment with axis
  cos2   <- res$group$cos2[,   axis]            # quality of representation on axis
  contrib <- res$group$contrib[, axis]      # contribution
  

  out <- data.frame(
    group        = rownames(res$group$coord),
    coord        = coord,
    cos2         = cos2,
    contrib = contrib,
    stringsAsFactors = FALSE
  )
  

  ord_col <- sort_by
  out <- out[order(-out[[ord_col]]), , drop = FALSE]
  

  out$coord       <- round(out$coord, digits)
  out$cos2        <- round(out$cos2, digits)
  out$contrib <- round(out$contrib, digits)
  

  if (!is.null(top_n)) out <- head(out, top_n)
  
  rownames(out) <- NULL
  out
}


options(scipen = 999)  
tbl3 <- mfa_group_axis_table(res_mfa_mixed, axis = 15, sort_by = "contrib")
print(tbl3)

#1st dimension:
# Dimension dominated by health-related groups (Satisfaction & Health, Healthcare limits, Depression), also the strongest relationship with the axis (coord - 0.48-0.36)
# Very small contributions of Social participation, Politics & language (qual), Education, Relations, Giving care, Moving Neighbourhood

#2nd dimension:
# Dimension dominated by Discrimination, then also a high contribution of Politics & Language (quant), Internet (qual), Culture, and Politics & Language. However cos2 is much smaller than in Dimension 1 for every group and only Discrimination has coord higher than 0.3
# Small contribution of Relations, Giving care, Moving, Physical limits, Poverty (quant)

#3rd dimension
# Dimension dominated by Economic activity, Poverty (quant), and Internet (quant)
# Small contribution and coord of Politics & Language (qual), Cognitive skills, Discrimination, Politics & Language (quant), Giving care

#4th dimension
# Dimension dominated by Economic activity, Social participation, Relations. 
# Small contribution of Discrimination, Neighbourhood, Education

#5th dimension
# High contrib of Economic activity, Giving care, Housing, Neighbourhood, however Housing and Neighbourhood have coord smaller than 0.2; cos2 for Economic activity and Housing are also very low 
# Small coord and contrib of Poverty (qual), Social services, Education, Social participation, Discrimination, Depression, Moving, and Internet (quant)

#6th dimension
# Dominance of Economic activity, then aso Housing and Neighbourhood
# Small coord for Poverty (qual), Social participation, Healthcare limits, Giving care, Depression, Discrimination, Internet (quant)

#7th dimension
# Dominance of Economic activity
# Small coord for Neighbourhood, Discrimination, Culture, Satisfaction & Health, Moving, Internet (quant)

#8th dimension
# Dominance of Economic activity
# Small coord for Relations, Poverty (quant), Social services, Satisfaction & Health, Giving care, neighbourhood, Politics & Language

#9th dimension
# Dominance of Housing and Economic activity
# Small coord for Social services, Discrimination, Culture, Poverty (quant), Relations, Social Participation

#10th dimension
# Dominance of Housing and Economic activity
# Small coord for Social services, Poverty (quant), Politics & Language (qual), Discrimination


for (i in 1:10){
  print(paste("Dimension ", i))
  tbl3 <- mfa_group_axis_table(res_mfa_mixed, axis = i, sort_by = "contrib")
  print(tbl3)
}


#_________________________
#Analyzing quality of groups across larger amount of dimensions


get_mfa_eig <- function(res, axes = 1:res$call$ncp) {
  # ‘res$eig’ is the eigenvalue table: col 2 = % of variance, col 3 = cumulative %
  eig <- res$eig
  max_axis <- nrow(eig)
  
  if (any(axes < 1 | axes > max_axis)) {
    stop("Requested axes must be between 1 and ", max_axis, call. = FALSE)
  }
  
  var_pct <- eig[axes, "percentage of variance"]
  names(var_pct) <- paste0("Dim", axes)
  return(var_pct)
}

amount_of_dimensions = 4

w <- get_mfa_eig(res_mfa_mixed, axes = 1:amount_of_dimensions)

#extracting the contrib matrix (groups × axes)
C <- res_mfa_mixed$group$contrib[, 1:amount_of_dimensions] / 100

#computing weighted contribution index - to analyze importance of a group 
#considering their contribution to the axes and relevance of the axes themselves

wci_vec <- setNames(
  as.numeric(C %*% w),
  rownames(C)
)
wci_vec <- sort(wci_vec, decreasing = TRUE)


wci_df <- data.frame(
  group = names(wci_vec),
  WCI   = unname(wci_vec),
  row.names = NULL,
  stringsAsFactors = FALSE
)


wci_df
amount_of_dimensions


#Comments:
#Dropping Giving care (as it is just K49 which is also represented in the Economic activity group)
#Dropping Neighbourhood (feeling affilication with neighbourhood) and Moving, they perform poorly. However, they are two out of three groups from Living environment domain (B-SEM) and quite accurately measure what it concerns. Inspect Housing as the last group from this domain
#Relations are performing poorly but they’re the only indicator for Social resources domain so they will stay.


#___________________________________________________




######################################################################
######################### Repeating MFA ##############################
######################################################################
#Removing unnecessary variables rom the dataset

imputed_df <- imputed_df %>%
  select(-K49, -K1509, -K07)

second_model <- imputed_df


############################
# 1 · 16 variable groups
############################
orig_blocks <- list(
  c("K03","K1601","K1602","K14"),    # 1 mixed "Politics & Language",
  "K05",                            # 2 nominal "Housing",
  c("K1001","K1002","K1003","K09"), # 3 mixed "Poverty",
  c("K1101","K1102","K1103","K1104","K1105","K23","K25"), #4 ordinal  "Satisfaction & Health",
  paste0("K180",1:8),               #5 binary "Social participation",
  paste0("K190",1:4),               #6 ordinal "Culture",
  paste0("K200",1:7),               #7 ordinal "Discrimination",
  paste0("K300",1:8),               #8 ordinal "Depression",
  paste0("K380",1:3),               #9 nominal "Sufficient healthcare",
  paste0("K390",1:6),               #10 binary "Healthcare limits",
  paste0("K430",1:6),               #11 binary "Social services",
  paste0("K480",1:7),               #12 continuous "Relations",
  c("K5701","K5705","K58",          #13 mixed "Internet"
    paste0("K590",1:9),"K5910","K5911"),
  c("K69_aggreg"),                   #14 nominal "Economic activity"
  c("K66_aggreg_num")                #15 ordinal "Education"
)



#   "Politics & Language",
#   "Housing",
#   "Poverty",
#   "Satisfaction & Health",
#   "Social participation",
#   "Culture",
#   "Discrimination",
#   "Cognitive skills",
#   "Physical limits",
#   "Depression",
#   "Sufficient healthcare",
#   "Healthcare limits",
#   "Social services",
#   "Relations",
#   "Internet"
#   "Economic activity

# socio‑demo supplementary block:
socio_demographics_qual <- c(
  "K2401","K2402","K2403","K2404","K2405","K2406",
  "K2407","K2408","K2409","K2410","K61","K63","K65"
)
socio_demographics_quant <- c(
  "age","K01"
)


############################
# 2 - separating to numeric and categorical
############################
num_vars <- c(
  "K09",paste0("K110",1:5),"K23","K25","K14",
  paste0("K190",1:4),paste0("K200",1:7), paste0("K300",1:8),
  paste0("K480",1:7),"K58","K66_aggreg_num", "age","K01"
)
cat_vars <- c(
  "K03","K05",paste0("K100",1:3),"K1601","K1602",
  paste0("K180",1:8),paste0("K380",1:3),
  paste0("K390",1:6),paste0("K430",1:6),
  "K5701","K5705",paste0("K590",1:9),"K5910","K5911",
  paste0("K240",1:9),"K2410","K61","K63","K65", "K69_aggreg"
)

# Ordinal-as-numeric 
safe_as_numeric <- function(x) {
  if (is.ordered(x)) return(as.integer(x))
  if (is.factor(x)) {
    xs <- as.character(x)
    ok <- grepl("^[-+]?[0-9]*\\.?[0-9]+$", xs) | is.na(xs)
    if (!all(ok)) stop("Non-numeric labels in a would-be numeric factor: ",
                       paste(unique(xs[!ok]), collapse=", "))
    return(as.numeric(xs))
  }
  as.numeric(x)
}

#coercing once:

imputed_df[ intersect(cat_vars, names(imputed_df)) ] <-
  lapply(imputed_df[ intersect(cat_vars, names(imputed_df)) ],
         function(x) factor(x, exclude = NULL))

imputed_df[ intersect(num_vars, names(imputed_df)) ] <-
  lapply(imputed_df[ intersect(num_vars, names(imputed_df)) ],
         safe_as_numeric)

############################
# 3 - buidling variable groups
############################


# pure numeric blocks stay as 1 "s" group
# pure categorical blocks stay as 1 "n" group
# mixed blocks become TWO groups: one "s" (nums), one "n" (factors)
blocks_mixed  <- list()
types_mixed   <- character(0)
names_mixed   <- character(0)

for (j in seq_along(orig_blocks)) {
  blk_name <- c(
    "Politics & Language","Housing","Poverty",
    "Satisfaction & Health","Social participation",
    "Culture","Discrimination",
    "Depression","Sufficient healthcare","Healthcare limits",
    "Social services","Relations","Internet",
    "Economic activity", "Education"
  )[j]
  
  blk <- orig_blocks[[j]]
  nums <- intersect(blk, num_vars)
  cats <- intersect(blk, cat_vars)
  
  if (length(nums) > 0 && length(cats) > 0) {
    # mixed → two groups
    blocks_mixed[[length(blocks_mixed)+1]] <- nums
    types_mixed   <- c(types_mixed, "s")
    names_mixed   <- c(names_mixed, paste0(blk_name," (quant)"))
    
    blocks_mixed[[length(blocks_mixed)+1]] <- cats
    types_mixed   <- c(types_mixed, "n")
    names_mixed   <- c(names_mixed, paste0(blk_name," (qual)"))
  } else {
    # pure block
    blocks_mixed[[length(blocks_mixed)+1]] <- blk
    types_mixed   <- c(types_mixed, if (length(nums)>0) "s" else "n")
    names_mixed   <- c(names_mixed, blk_name)
  }
}

active_vars <- unlist(blocks_mixed, use.names = FALSE)
stopifnot(!any(duplicated(active_vars)))
group_sizes <- lengths(blocks_mixed)
group_types <- types_mixed
name_groups <- names_mixed


stopifnot(
  length(group_sizes) == length(group_types),
  sum(group_sizes)    == length(active_vars),
  length(name_groups) == length(group_sizes)
)
############################
# 4 - adding supplementary variable group
############################
sup_sizes <- c(length(socio_demographics_qual), length(socio_demographics_quant))
sup_types <- c("n", "s")
sup_names <- c("SocioDemographics (qual)", "SocioDemographics (quant)")

group_all <- c(group_sizes, sup_sizes)
type_all  <- c(group_types, sup_types)
name_all  <- c(name_groups, sup_names)


sup_index <- (length(group_all)-1):length(group_all)




############################
# Runing MFA
############################
res_mfa_mixed <- MFA(
  base           = imputed_df[, c(active_vars, socio_demographics_qual, socio_demographics_quant)],
  group          = group_all,
  type           = type_all,
  name.group     = name_all,
  row.w          = imputed_df$useweight,
  num.group.sup  = sup_index,
  graph          = FALSE,
  ncp            = 15
)

second_mfa <- res_mfa_mixed
######## Investigate how I can mix both quant and qual variables so I could have one block instead of two for mixed ones

# Plot results
plot(res_mfa_mixed, choix = "ind")
plot(res_mfa_mixed, choix = "var")
plot(res_mfa_mixed, choix = "group")
plot(res_mfa_mixed, choix = "axes")  

plot(res_mfa_mixed,
     choix     = "ind", 
     invisible = c("ind", "ind.sup"))


#############
# Ploting both categorical and quantiative variables

coord_num   <- as.data.frame(res_mfa_mixed$quanti.var$coord[, 1:2])
coord_cat   <- as.data.frame(res_mfa_mixed$quali.var$coord[, 1:2])
coord_num$type <- "quantitative"
coord_cat$type <- "categorical"
coords <- rbind(coord_num, coord_cat)


ggplot(coords, aes(Dim.1, Dim.2, colour = type)) +
  geom_point() +
  geom_text(aes(label = rownames(coords)), hjust = -0.2) +
  coord_equal() +
  theme_minimal()

#______________________________________
#inspecting the model


res_mfa_mixed$eig

barplot(res_mfa_mixed$eig[, 1], names.arg = seq_len(nrow(res_mfa_mixed$eig)))

factoextra::fviz_eig(res_mfa_mixed, addlabels = TRUE)

#having 18 groups, the eigenvalue of a first component is 4.46998 (compared to 4,495 last time), which gives a ratio 0.2483 (compared to 0.214 last time). 
#Perfect setup would be eigenvalue 18

res_mfa_mixed$group

#coord[j, s] (group–axis coordinate) = relationship/alignment of group j with axis s. 
#In the Jérôme Pagès’s terms, the coordinate of a group on axis s is a “relationship 
#measurement” (max 1 on the relationship square). higher coord ⇒ the group is more 
#aligned/correlated with axis. Use this to say which groups are aligned with an axis.
res_mfa_mixed$group$coord

#_________________________________________
#calculating Quality of representation of cloud NJ
axes <- c(5)                                 # picking the plane/axis shown
num <- colSums(res_mfa_mixed$group$coord[, axes, drop = FALSE]^2)
den <- sum(res_mfa_mixed$group$dist2)
plane_ratio_NJ <- sum(num) / den       #Quality of representation of cloud NJ from (Jérôme Pagès, 2015, p.156), Quality(NJ) tells how well the plane represents the cloud of groups; It’s a single, 0–1 score for the plane: higher ⇒ the plane shows a big share of all groups together; lower ⇒ the groups’ structure lives beyond that plane. 
plane_ratio_NJ                         # bigger ⇒ the plane shows more of all groups together
#__________________________________
#cos2[j, s] (squared cosine) = quality of representation of group j on axis s 
#(or on a plane if you sum across axes). It’s computed by MFA and lives in 
#res$group$cos2. Use it to say how well a group is shown on an axis/plane.
res_mfa_mixed$group$cos2
#_____________
#contrib[j, s] (group contribution to axis s)
#In the FactoMineR output for groups, “the raw inertias are in coord … 
# and the percentages in contrib.” In other words, contrib gives the 
#percentage share (sums to 100% across groups for an axis) of how much 
#each group builds/constructs that axis. Use this when you want to say 
#what builds the axis. 
res_mfa_mixed$group$contrib



#_____________________________________________________
#investigating relations between groups

#Lg(Kj, Kl) — relationship (link) between two groups of variables
#It’s defined via the scalar product of the groups’ Gram matrices with the 
# MFA weighting; it can exceed 1 and has no fixed upper bound. It grows when 
# the two groups share more common directions and when those directions carry 
# high inertia. It is 0 iff every variable of one group is uncorrelated with 
# every variable of the other. 

res_mfa_mixed$group$Lg

#RV(Kj, Kl) — Escoufier’s RV coefficient between two groups
# When the groups are standardised in the groups’ space, the scalar product 
# behaves like a cosine, which gives RV; it is bounded in [0,1] and equals 
# 1 when the two groups’ clouds of individuals are homothetic. It is 0 under 
# complete lack of cross-group correlations.

res_mfa_mixed$group$RV

#no considerable relations between groups, except for some examples of groups split 
#into quant and qual (as they are covering the same topic, they will relate to each other)

#As a result, group structure will remain as it is. Possible changes will only involve which groups are included in the MFA


#_______________________________________________________
#Investigating groups to select those I will keep and those I will drop



mfa_group_axis_table <- function(res, axis = 1, sort_by = c("coord","cos2","contrib"),
                                 top_n = NULL, digits = 3) {
  sort_by <- match.arg(sort_by)
  stopifnot(axis >= 1, axis <= ncol(res$group$coord))
  

  coord  <- res$group$coord[,  axis]            # relationship/alignment with axis
  cos2   <- res$group$cos2[,   axis]            # quality of representation on axis
  contrib <- res$group$contrib[, axis]      # contribution
  

  out <- data.frame(
    group        = rownames(res$group$coord),
    coord        = coord,
    cos2         = cos2,
    contrib = contrib,
    stringsAsFactors = FALSE
  )
  

  ord_col <- sort_by
  out <- out[order(-out[[ord_col]]), , drop = FALSE]
  

  out$coord       <- round(out$coord, digits)
  out$cos2        <- round(out$cos2, digits)
  out$contrib <- round(out$contrib, digits)
  

  if (!is.null(top_n)) out <- head(out, top_n)
  
  rownames(out) <- NULL
  out
}


options(scipen = 999)  
tbl3 <- mfa_group_axis_table(res_mfa_mixed, axis = 10, sort_by = "contrib")
print(tbl3)

#1st dimension:
# Dimension dominated by health-related groups (Satisfaction & Health, Healthcare limits, Depression)
# Small contributions of Relations, Politics & Language (qual), Education

#2nd dimension:
# Dimension dominated by Discrimination, then also a high contribution of Politics & Language (quant), Internet (qual), Culture, and Politics & Language (qual). However cos2 is much smaller than in Dimension 1 for every group and only Discrimination has coord higher than 0.3
# Small contribution of Relations, Education, Poverty (quant)

#3rd dimension
# Dimension dominated by Economic activity, Poverty (quant), and Internet (quant)
# Small contribution and coord of Politics & Language (qual), Discrimination, Politics & Language (quant), Housing

#4th dimension
# Dimension dominated by Economic activity, potentially also Internet (qual), Relations, Social participation, Politics & Language (qual)
# Small contribution of Discrimination

#5th dimension
# High contrib of Economic activity, Housing

#6th dimension
# Dominance of Economic activity, then aso Housing 

#7th dimension
# Dominance of Economic activity, Housing, then Education

#8th dimension
# Dominance of Housing, Economic activity, Sufficient healthcare

#9th dimension
# Dominance of Economic activity, Sufficient healthcare, Housing


#10th dimension
# Dominance of Housing and Economic activity

for (i in 1:10){
  print(paste("Dimension ", i))
  tbl3 <- mfa_group_axis_table(res_mfa_mixed, axis = i, sort_by = "contrib")
  print(tbl3)
}


#_________________________
#Analyzing quality of groups across larger amount of dimensions


get_mfa_eig <- function(res, axes = 1:res$call$ncp) {
  # ‘res$eig’ is the eigenvalue table: col 2 = % of variance, col 3 = cumulative %
  eig <- res$eig
  max_axis <- nrow(eig)
  
  if (any(axes < 1 | axes > max_axis)) {
    stop("Requested axes must be between 1 and ", max_axis, call. = FALSE)
  }
  
  var_pct <- eig[axes, "percentage of variance"]
  names(var_pct) <- paste0("Dim", axes)
  return(var_pct)
}

amount_of_dimensions = 4

w <- get_mfa_eig(res_mfa_mixed, axes = 1:amount_of_dimensions)

#extracting the contrib matrix (groups × axes)
C <- res_mfa_mixed$group$contrib[, 1:amount_of_dimensions] / 100

#computing weighted contribution index - to analyze importance of a group 
#considering their contribution to the axes and relevance of the axes themselves

wci_vec <- setNames(
  as.numeric(C %*% w),
  rownames(C)
)
wci_vec <- sort(wci_vec, decreasing = TRUE)


wci_df <- data.frame(
  group = names(wci_vec),
  WCI   = unname(wci_vec),
  row.names = NULL,
  stringsAsFactors = FALSE
)


wci_df
amount_of_dimensions


#Comments:
#The model hasn't improved a lot, eigenvalues and explained intertia are quite similar. 
#The groups repetitively exhibiting WCI low is Relations and Education groups which I want to keep because of theoretical relevancy.


#I've also compared high WCI to cos2 of a group. The groups with WCI, 
#Economic activity performs very small cos2 values (despite being 
#high in the contributions) in several dimensions (Economic activity in 1st, 
#2nd, 3rd, 4th, 9th; Housing in 1st, 2nd, 4th, 6th). I will try to restructure variables and see impact of it.

#Moreover, housing variable is dominating dimension structure. I'll try to restructure it as well.


#___________________________________________________

#Repeating MFA

# Investigating Economic activity variable

freq(imputed_df$K69_aggreg)

design <- svydesign(
  ids = ~1,                  
  weights = ~useweight,  
  data = imputed_df
)

svy_table <- svytable(~K69_aggreg + K09, design)
svy_table

fisher.test(svy_table, simulate.p.value = TRUE)

#There's significant association between economic activity and financial situation

# ─────────────────────────────────────────────────────────────
# 1. OVERALL Kruskal–Wallis for all K69_aggreg levels
# ─────────────────────────────────────────────────────────────
overall_kw <- svyranktest(
  K09 ~ K69_aggreg,
  design = design,
  test   = "KruskalWallis"
)


# Show the overall result
cat("Overall survey-weighted Kruskal–Wallis:\n",
    "χ² = ", round(overall_kw$statistic, 3),
    ", df = ", overall_kw$parameter,
    ", p = ", format.pval(overall_kw$p.value), "\n\n", sep = "")

# ─────────────────────────────────────────────────────────────
# 2. Pairwise survey-weighted Kruskal–Wallis (two groups each)
# ─────────────────────────────────────────────────────────────
pairwise_svyrank <- function(formula, design,
                             test = "KruskalWallis",
                             p.adjust.method = "bonferroni") {
  vars  <- all.vars(formula)
  g_var <- vars[2]
  
  groups <- na.omit(unique(design$variables[[g_var]]))
  pairs  <- combn(groups, 2, simplify = FALSE)
  
  map_dfr(pairs, function(pr) {
    d_sub <- subset(design, get(g_var) %in% pr)
    ht    <- svyranktest(formula, design = d_sub, test = test)
    
    tibble(
      group1    = pr[1],
      group2    = pr[2],
      statistic = unname(ht$statistic),   # χ² for 2-group test
      df        = unname(ht$parameter),   # should be 1
      p.value   = ht$p.value
    )
  }) %>%
    mutate(p.adj = p.adjust(p.value, method = p.adjust.method))
}

# Run the pairwise tests
pairwise_results <- pairwise_svyrank(K09 ~ K69_aggreg, design) %>%
  dplyr::mutate(across(c(group1, group2), as.character))


# all significant pairs (p.adj < 0.05)
sig_pairs <- pairwise_results %>% 
  filter(p.value < 0.05)

# all non-significant pairs (p.adj >= 0.05)
nonsig_pairs <- pairwise_results %>% 
  filter(p.value >= 0.05)

# view them
sig_pairs
nonsig_pairs


# get the full list of categories
cats <- sort(unique(c(pairwise_results$group1, pairwise_results$group2)))

for(cat in cats) {
  # find all significant comparisons involving this category
  sig_others <- sig_pairs %>%
    filter(group1 == cat | group2 == cat) %>%
    transmute(other = ifelse(group1 == cat, group2, group1)) %>%
    pull(other)
  
  # find all non–significant comparisons involving it
  sim_others <- nonsig_pairs %>%
    filter(group1 == cat | group2 == cat) %>%
    transmute(other = ifelse(group1 == cat, group2, group1)) %>%
    pull(other)
  
  # format and print
  cat("Category:", cat, "\n")
  cat("Different:", if (length(sig_others)) paste(sig_others, collapse = ", ") else "— none —", "\n")
  cat("Similar:  ", if (length(sim_others)) paste(sim_others, collapse = ", ") else "— none —", "\n\n")
}




prop.table(svy_table, 1) * 100

svyboxplot(K09 ~ K69_aggreg, design, all.outliers = TRUE)


summary_list <- lapply(levels(imputed_df$K69_aggreg), function(g) {
  # Subset design to this group
  d_sub <- subset(design, K69_aggreg == g)
  
  # 1) weighted mean
  mean_g <- coef(svymean(~K09, d_sub, na.rm = TRUE))
  
  # 2) weighted variance → sd
  var_g  <- coef(svyvar(~K09, d_sub, na.rm = TRUE))
  sd_g   <- sqrt(var_g)
  
  # 3) weighted quantiles: 0%, 50%, 100%
  q_g    <- svyquantile(
    ~K09,
    d_sub,
    quantiles = c(0, 0.5, 1),
    ci        = FALSE,
    keep.var  = FALSE,
    na.rm     = TRUE
  )
  q_vals <- as.numeric(coef(q_g))   # extract the numeric vector
  
  data.frame(
    K69_aggreg = g,
    min        = q_vals[1],
    median     = q_vals[2],
    max        = q_vals[3],
    mean       = as.numeric(mean_g),
    sd         = as.numeric(sd_g),
    row.names  = NULL
  )
})

summary_table <- do.call(rbind, summary_list)
print(summary_table)

# 1. Compute percentages
prop_df <- as.data.frame(prop.table(svy_table, 1) * 100)
names(prop_df) <- c("Category", "K09", "Percent")

# ensure K09 is treated as an ordered factor
prop_df$K09 <- factor(prop_df$K09, levels = sort(unique(prop_df$K09)))

# 2. Build one ggplot per category
plots <- lapply(unique(prop_df$Category), function(cat) {
  dat <- subset(prop_df, Category == cat)
  ggplot(dat, aes(x = K09, y = Percent)) +
    geom_col() +
    scale_y_continuous(limits = c(0, 100), expand = c(0,0)) +
    labs(title = cat, x = "K09", y = "Percent (%)") +
    theme_minimal(base_size = 12) +
    theme(
      plot.title   = element_text(face = "bold", hjust = 0.5),
      axis.title.x = element_text(margin = margin(t = 8)),
      axis.title.y = element_text(margin = margin(r = 8))
    )
})

# 3. Arrange them one below another
do.call(grid.arrange, c(plots, ncol = 1))


# Use groups from the design to avoid misalignment with external data frames
groups <- levels(droplevels(as.factor(design$variables$K69_aggreg)))

summary_list <- lapply(groups, function(g) {
  # Subset to this group, keep finite Age
  d_sub <- subset(design, !is.na(K69_aggreg) & K69_aggreg == g & !is.na(age) & is.finite(age))
  
  # If subgroup is empty, return NA row
  if (nrow(d_sub$variables) == 0) {
    return(data.frame(
      K69_aggreg = g, min = NA_real_, median = NA_real_, max = NA_real_,
      mean = NA_real_, sd = NA_real_, row.names = NULL
    ))
  }
  
  # 1) weighted mean
  mean_g <- as.numeric(coef(svymean(~age, d_sub, na.rm = TRUE)))
  
  # 2) weighted variance → sd
  var_g  <- as.numeric(coef(svyvar(~age, d_sub, na.rm = TRUE)))
  sd_g   <- ifelse(is.na(var_g), NA_real_, sqrt(var_g))
  
  # 3) weighted quantiles: 0%, 50%, 100%
  q_vals <- tryCatch(
    as.numeric(coef(svyquantile(
      ~age, d_sub,
      quantiles = c(0, 0.5, 1),
      ci = FALSE, keep.var = FALSE, na.rm = TRUE
    ))),
    error = function(e) rep(NA_real_, 3)
  )
  
  data.frame(
    K69_aggreg = g,
    min    = q_vals[1],
    median = q_vals[2],
    max    = q_vals[3],
    mean   = mean_g,
    sd     = sd_g,
    row.names = NULL
  )
})

summary_table <- do.call(rbind, summary_list)
print(summary_table, row.names = FALSE)



freq(imputed_df$K69_aggreg)


#Based on pairwise Kruskal Wallis test, I will combine following categories:
#Entrepreneur & Full-time working
#In a long-term sick leave (more than 6 months) & In disability pension or in rehabilitation

#“Providing care …” and “Studying …” are small but not different to any other group, both in the test result as in the general descriptive statistics. I will keep them as for now as separate



imputed_df <- imputed_df %>%
  mutate(
    K69_aggreg = case_when(
      K69_aggreg %in% c("Entrepreneur", "Full-time working") ~ "Full-time working / Entrepreneur",
      K69_aggreg %in% c(
        "In a long-term sick leave (more than 6 months)",
        "In disability pension or in rehabilitation"
      ) ~ "Sick leave, disability, rehabilitation",
      TRUE ~ as.character(K69_aggreg)
    ),
    K69_aggreg = factor(
      K69_aggreg,
      levels = c(
        "Full-time working / Entrepreneur",
        "Sick leave, disability, rehabilitation",
        "In pension on the basis of age",
        "Part-time working",
        "Providing care for relatives, children or other family members",
        "Studying (including professional education at the workplace)",
        "Unemployed or temporarily laid off"
      )
    )
  )
freq(imputed_df$K69_aggreg)

#____________________________________
#Investigating housing variable

freq(imputed_df$K05)



svy_table <- svytable(~K05 + K09, design)
svy_table

fisher.test(svy_table, simulate.p.value = TRUE)

#There's significant association between housing and financial situation

prop.table(svy_table, 1) * 100

#People who struggle the most financially were dominating in options 4 and 5 (4 - In a flat that is rented from municipality or 
#other non-private organisation, 5 - A housing complex with services for older people (residential home or sheltered housing)), 
#while people who are getting along easily were having similar distribution for 1 and 2 (1 - Own flat or house, 2 - In a flat 
#with partial ownership or with the right to occupancy (asumisoikeus)). I will then aggregate 4 with 5 and 2 with 1, as they are also somehow similar

imputed_df <- imputed_df %>%
  mutate(K05 = case_when(
    K05 %in% c(1, 2) ~ 1,  # 1 and 2 become 1
    K05 == 3 ~ 2,          # 3 becomes 2
    K05 %in% c(4, 5) ~ 3,   # 4 and 5 becomes 3
    
  ))

freq(imputed_df$K05)

# ############################
# # 0 · Helper: drop constants + reorder numeric→factor inside a mixed block
# ############################
# .prepare_m_block <- function(df, vars, cat_set) {
#   # drop any column that doesn’t vary
#   keep <- vapply(df[vars], function(x) {
#     if (is.factor(x))   nlevels(x) > 1
#     else                var(x, na.rm = TRUE) > 0
#   }, logical(1))
#   vars <- vars[keep]
#   # numeric first, then factors
#   nums <- setdiff(vars, cat_set)
#   cats <- intersect(vars, cat_set)
#   c(nums, cats)
# }

third_model <- imputed_df
############################
# 1 · 16 variable groups
############################
orig_blocks <- list(
  c("K03","K1601","K1602","K14"),    # 1 mixed "Politics & Language",
  "K05",                            # 2 nominal "Housing",
  c("K1001","K1002","K1003","K09"), # 3 mixed "Poverty",
  c("K1101","K1102","K1103","K1104","K1105","K23","K25"), #4 ordinal  "Satisfaction & Health",
  paste0("K180",1:8),               #5 binary "Social participation",
  paste0("K190",1:4),               #6 ordinal "Culture",
  paste0("K200",1:7),               #7 ordinal "Discrimination",
  paste0("K300",1:8),               #8 ordinal "Depression",
  paste0("K380",1:3),               #9 nominal "Sufficient healthcare",
  paste0("K390",1:6),               #10 binary "Healthcare limits",
  paste0("K430",1:6),               #11 binary "Social services",
  paste0("K480",1:7),               #12 continuous "Relations",
  c("K5701","K5705","K58",          #13 mixed "Internet"
    paste0("K590",1:9),"K5910","K5911"),
  c("K69_aggreg"),                   #14 nominal "Economic activity"
  c("K66_aggreg_num")                #15 ordinal "Education"
)



#   "Politics & Language",
#   "Housing",
#   "Poverty",
#   "Satisfaction & Health",
#   "Social participation",
#   "Culture",
#   "Discrimination",
#   "Depression",
#   "Sufficient healthcare",
#   "Healthcare limits",
#   "Social services",
#   "Relations",
#   "Internet"
#   "Economic activity

# socio‑demo supplementary block:
socio_demographics_qual <- c(
  "K2401","K2402","K2403","K2404","K2405","K2406",
  "K2407","K2408","K2409","K2410","K61","K63","K65"
)
socio_demographics_quant <- c(
  "age","K01"
)


############################
# 2 - separating to numeric and categorical
############################
num_vars <- c(
  "K09",paste0("K110",1:5),"K23","K25","K14",
  paste0("K190",1:4),paste0("K200",1:7), paste0("K300",1:8),
  paste0("K480",1:7), "K58","K66_aggreg_num","age","K01"
)
cat_vars <- c(
  "K03","K05",paste0("K100",1:3),"K1601","K1602",
  paste0("K180",1:8),paste0("K380",1:3),
  paste0("K390",1:6),paste0("K430",1:6),
  "K5701","K5705",paste0("K590",1:9),"K5910","K5911",
  paste0("K240",1:9),"K2410","K61","K63","K65", "K69_aggreg"
)

# Ordinal-as-numeric 
safe_as_numeric <- function(x) {
  if (is.ordered(x)) return(as.integer(x))
  if (is.factor(x)) {
    xs <- as.character(x)
    ok <- grepl("^[-+]?[0-9]*\\.?[0-9]+$", xs) | is.na(xs)
    if (!all(ok)) stop("Non-numeric labels in a would-be numeric factor: ",
                       paste(unique(xs[!ok]), collapse=", "))
    return(as.numeric(xs))
  }
  as.numeric(x)
}

data.table::setDF(imputed_df)
#coercing once:

imputed_df[ intersect(cat_vars, names(imputed_df)) ] <-
  lapply(imputed_df[ intersect(cat_vars, names(imputed_df)) ],
         function(x) factor(x, exclude = NULL))

imputed_df[ intersect(num_vars, names(imputed_df)) ] <-
  lapply(imputed_df[ intersect(num_vars, names(imputed_df)) ],
         safe_as_numeric)

############################
# 3 - buidling variable groups
############################


# pure numeric blocks stay as 1 "s" group
# pure categorical blocks stay as 1 "n" group
# mixed blocks become TWO groups: one "s" (nums), one "n" (factors)
blocks_mixed  <- list()
types_mixed   <- character(0)
names_mixed   <- character(0)

for (j in seq_along(orig_blocks)) {
  blk_name <- c(
    "Politics & Language","Housing","Poverty",
    "Satisfaction & Health","Social participation",
    "Culture","Discrimination",
    "Depression","Sufficient healthcare","Healthcare limits",
    "Social services","Relations","Internet",
    "Economic activity", "Education"
  )[j]
  
  blk <- orig_blocks[[j]]
  nums <- intersect(blk, num_vars)
  cats <- intersect(blk, cat_vars)
  
  if (length(nums) > 0 && length(cats) > 0) {
    # mixed → two groups
    blocks_mixed[[length(blocks_mixed)+1]] <- nums
    types_mixed   <- c(types_mixed, "s")
    names_mixed   <- c(names_mixed, paste0(blk_name," (quant)"))
    
    blocks_mixed[[length(blocks_mixed)+1]] <- cats
    types_mixed   <- c(types_mixed, "n")
    names_mixed   <- c(names_mixed, paste0(blk_name," (qual)"))
  } else {
    # pure block
    blocks_mixed[[length(blocks_mixed)+1]] <- blk
    types_mixed   <- c(types_mixed, if (length(nums)>0) "s" else "n")
    names_mixed   <- c(names_mixed, blk_name)
  }
}

active_vars <- unlist(blocks_mixed, use.names = FALSE)
stopifnot(!any(duplicated(active_vars)))
group_sizes <- lengths(blocks_mixed)
group_types <- types_mixed
name_groups <- names_mixed


stopifnot(
  length(group_sizes) == length(group_types),
  sum(group_sizes)    == length(active_vars),
  length(name_groups) == length(group_sizes)
)

# 4 - adding supplementary variable group
sup_sizes <- c(length(socio_demographics_qual), length(socio_demographics_quant))
sup_types <- c("n", "s")
sup_names <- c("SocioDemographics (qual)", "SocioDemographics (quant)")

group_all <- c(group_sizes, sup_sizes)
type_all  <- c(group_types, sup_types)
name_all  <- c(name_groups, sup_names)


sup_index <- (length(group_all)-1):length(group_all)




############################
# Runing MFA
############################
res_mfa_mixed <- MFA(
  base           = imputed_df[, c(active_vars, socio_demographics_qual, socio_demographics_quant)],
  group          = group_all,
  type           = type_all,
  name.group     = name_all,
  row.w          = imputed_df$useweight,
  num.group.sup  = sup_index,
  graph          = FALSE,
  ncp            = 15
)

third_mfa <- res_mfa_mixed
######## Investigate how I can mix both quant and qual variables so I could have one block instead of two for mixed ones

# Plot results
plot(res_mfa_mixed, choix = "ind")
plot(res_mfa_mixed, choix = "var")
plot(res_mfa_mixed, choix = "group")
plot(res_mfa_mixed, choix = "axes")  

plot(res_mfa_mixed,
     choix     = "ind", 
     invisible = c("ind", "ind.sup"))


#############
# Ploting both categorical and quantiative variables

coord_num   <- as.data.frame(res_mfa_mixed$quanti.var$coord[, 1:2])
coord_cat   <- as.data.frame(res_mfa_mixed$quali.var$coord[, 1:2])
coord_num$type <- "quantitative"
coord_cat$type <- "categorical"
coords <- rbind(coord_num, coord_cat)


ggplot(coords, aes(Dim.1, Dim.2, colour = type)) +
  geom_point() +
  geom_text(aes(label = rownames(coords)), hjust = -0.2) +
  coord_equal() +
  theme_minimal()

#______________________________________
#inspecting the model


res_mfa_mixed$eig

barplot(res_mfa_mixed$eig[, 1], names.arg = seq_len(nrow(res_mfa_mixed$eig)))

factoextra::fviz_eig(res_mfa_mixed, addlabels = TRUE)

#having 18 groups, the eigenvalue of a first component is 4.46145 (compared to 4.46998 last time), which gives a ratio 0.2479 (compared to 0.2483 last time). 
#Perfect setup would be eigenvalue 18

res_mfa_mixed$group

#coord[j, s] (group–axis coordinate) = relationship/alignment of group j with axis s. 
#In the Jérôme Pagès’s terms, the coordinate of a group on axis s is a “relationship 
#measurement” (max 1 on the relationship square). higher coord ⇒ the group is more 
#aligned/correlated with axis. Use this to say which groups are aligned with an axis.
res_mfa_mixed$group$coord

#_________________________________________
#calculating Quality of representation of cloud NJ
axes <- c(5)                                 # picking the plane/axis shown
num <- colSums(res_mfa_mixed$group$coord[, axes, drop = FALSE]^2)
den <- sum(res_mfa_mixed$group$dist2)
plane_ratio_NJ <- sum(num) / den       #Quality of representation of cloud NJ from (Jérôme Pagès, 2015, p.156), Quality(NJ) tells how well the plane represents the cloud of groups; It’s a single, 0–1 score for the plane: higher ⇒ the plane shows a big share of all groups together; lower ⇒ the groups’ structure lives beyond that plane. 
plane_ratio_NJ                         # bigger ⇒ the plane shows more of all groups together
#__________________________________
#cos2[j, s] (squared cosine) = quality of representation of group j on axis s 
#(or on a plane if you sum across axes). It’s computed by MFA and lives in 
#res$group$cos2. Use it to say how well a group is shown on an axis/plane.
res_mfa_mixed$group$cos2
#_____________
#contrib[j, s] (group contribution to axis s)
#In the FactoMineR output for groups, “the raw inertias are in coord … 
# and the percentages in contrib.” In other words, contrib gives the 
#percentage share (sums to 100% across groups for an axis) of how much 
#each group builds/constructs that axis. Use this when you want to say 
#what builds the axis. 
res_mfa_mixed$group$contrib



#_____________________________________________________
#investigating relations between groups

#Lg(Kj, Kl) — relationship (link) between two groups of variables
#It’s defined via the scalar product of the groups’ Gram matrices with the 
# MFA weighting; it can exceed 1 and has no fixed upper bound. It grows when 
# the two groups share more common directions and when those directions carry 
# high inertia. It is 0 iff every variable of one group is uncorrelated with 
# every variable of the other. 

res_mfa_mixed$group$Lg

#RV(Kj, Kl) — Escoufier’s RV coefficient between two groups
# When the groups are standardised in the groups’ space, the scalar product 
# behaves like a cosine, which gives RV; it is bounded in [0,1] and equals 
# 1 when the two groups’ clouds of individuals are homothetic. It is 0 under 
# complete lack of cross-group correlations.

res_mfa_mixed$group$RV

#no considerable relations between groups, except for some examples of groups split 
#into quant and qual (as they are covering the same topic, they will relate to each other)

#As a result, group structure will remain as it is. Possible changes will only involve which groups are included in the MFA


#_______________________________________________________
#Investigating groups to select those I will keep and those I will drop



mfa_group_axis_table <- function(res, axis = 1, sort_by = c("coord","cos2","contrib"),
                                 top_n = NULL, digits = 3) {
  sort_by <- match.arg(sort_by)
  stopifnot(axis >= 1, axis <= ncol(res$group$coord))
  

  coord  <- res$group$coord[,  axis]            # relationship/alignment with axis
  cos2   <- res$group$cos2[,   axis]            # quality of representation on axis
  contrib <- res$group$contrib[, axis]      # contribution
  

  out <- data.frame(
    group        = rownames(res$group$coord),
    coord        = coord,
    cos2         = cos2,
    contrib = contrib,
    stringsAsFactors = FALSE
  )
  

  ord_col <- sort_by
  out <- out[order(-out[[ord_col]]), , drop = FALSE]
  

  out$coord       <- round(out$coord, digits)
  out$cos2        <- round(out$cos2, digits)
  out$contrib <- round(out$contrib, digits)
  

  if (!is.null(top_n)) out <- head(out, top_n)
  
  rownames(out) <- NULL
  out
}

options(scipen = 999)  

for (i in 1:2){
  print(paste("Dimension ", i))
  tbl3 <- mfa_group_axis_table(res_mfa_mixed, axis = i, sort_by = "contrib")
  print(tbl3)
}


# Dimension 1
# Satisfaction & Health, Poverty (qual), Healthcare limits, Depression
# Check Economic activity

# Dimension 2
# Discrimination, Internet (qual), Politics & Language (quant), Culture, Politics & Language (qual)
# Check Economic activity

# Dimension 3
# Economic activity, Poverty (quant), Internet (quant), Social participation

# Dimension 4
# Economic activity, Relations, Internet (qual)

# Dimension 5
# Economic activity, Housing, Politics & Language (qual)

# Dimension 6
# Economic activity, Sufficient healthcare, Education

# Dimension 7
# Economic activity, Sufficient healthcare, Education

# Dimension 8
# Economic activity, Housing, Education, Sufficient healthcare

# Dimension 9
# Economic activity, Housing

# Dimension 10
# Economic activity, Education, Sufficient healthcare, Relations

#_________________________
#Analyzing quality of groups across larger amount of dimensions


get_mfa_eig <- function(res, axes = 1:res$call$ncp) {
  # ‘res$eig’ is the eigenvalue table: col 2 = % of variance, col 3 = cumulative %
  eig <- res$eig
  max_axis <- nrow(eig)
  
  if (any(axes < 1 | axes > max_axis)) {
    stop("Requested axes must be between 1 and ", max_axis, call. = FALSE)
  }
  
  var_pct <- eig[axes, "percentage of variance"]
  names(var_pct) <- paste0("Dim", axes)
  return(var_pct)
}

amount_of_dimensions = 4

w <- get_mfa_eig(res_mfa_mixed, axes = 1:amount_of_dimensions)

#extracting the contrib matrix (groups × axes)
C <- res_mfa_mixed$group$contrib[, 1:amount_of_dimensions] / 100

#computing weighted contribution index - to analyze importance of a group 
#considering their contribution to the axes and relevance of the axes themselves

wci_vec <- setNames(
  as.numeric(C %*% w),
  rownames(C)
)
wci_vec <- sort(wci_vec, decreasing = TRUE)


wci_df <- data.frame(
  group = names(wci_vec),
  WCI   = unname(wci_vec),
  row.names = NULL,
  stringsAsFactors = FALSE
)


wci_df
amount_of_dimensions


#Comments:

#The model improved a bit and Economic activity although still happening to have high contrib and low cos2 definitely decreased this discrepancy.

#I'll investigate now the very variables, within the group level.

#_____________________________________________________________________


#######################
#number of dimensions produced

n_axes <- ncol(res_mfa_mixed$quanti.var$coord)

#######################
# Building a mapping: each MFA variable → its varaible group

#identifying active variable group names
sup_idx      <- res_mfa_mixed$call$num.group.sup %||% integer(0)
active_blks  <- res_mfa_mixed$call$name.group[-sup_idx]

# for each block, grabbing the rownames it produced in res$separate.analyses
group_vars <- setNames(
  lapply(seq_along(active_blks), function(i) {
    sep <- res_mfa_mixed$separate.analyses[[i]]
    rownames(sep$var$coord)
  }),
  active_blks
)

#inverting that list into a named vector var → variable group
var2group <- unlist(lapply(names(group_vars), function(g) {
  setNames(rep(g, length(group_vars[[g]])), group_vars[[g]])
}))

#######################
# Pulling global MFA metrics for every variable & axis

build_df <- function(vl) {
  df <- data.frame(variable = rownames(vl$coord),
                   stringsAsFactors = FALSE)
  for (ax in seq_len(n_axes)) {
    df[[paste0("coord",   ax)]] <- vl$coord[,   ax]
    df[[paste0("cos2",    ax)]] <- vl$cos2[,    ax]
    df[[paste0("contrib", ax)]] <- vl$contrib[, ax]
  }
  df
}

df_q <- build_df(res_mfa_mixed$quanti.var)
df_l <- build_df(res_mfa_mixed$quali.var)
metrics <- rbind(df_q, df_l)

#######################
# Assigning the correct block to each row via var2group

metrics$group <- var2group[metrics$variable]

 check: no missing groups
stopifnot(!any(is.na(metrics$group)))

#######################
# Fixing format

metrics_long <- metrics %>%
  pivot_longer(
    cols = -c(variable, group),
    names_to  = c(".value", "axis"),
    names_pattern = "(coord|cos2|contrib)(\\d+)"
  ) %>%
  mutate(axis = as.integer(axis))




metrics_long %>%
  filter(group == "Education", axis == 7) %>%
  arrange(desc(contrib)) %>%
  print(n = Inf)



# Dimension 1
# Satisfaction & Health, Physical limits, Depression, Healthcare limits, Cognitive skills - all seems to be valid
# Check Economic activity - Unemployed, Providing care, Studying and Part-Time with very low contrib and cos2

# Dimension 2
# Discrimination - all valid
#Internet (qual) - multiple variables with certain categories high on the list and other low. However, K5903 has very 
#low cos2 and has contrib 0.257. Same results for K5701, K5705. Consider dropping if not relevant on any other axis
#Politics & Language (quant) - all valid
#Culture - I think to drop K1903 (museums/galleries) as it has low cos2 and theoretically is problematic compared to 
#the other in the group
#Politics & Language (qual) - No in the questions of voting in elections has very low cos2, contrib, and coord but 
#the rest of the values of those variables are very high and I can't just drop the NOs, so I will keep all variables here.
# Check Economic activity - Providin care, Unemployed with very low cos2 and contrib. Potentially also studying, full-time working, sick leave

# Dimension 3
# Economic activity - very low cos2 for Providing care, part-time, full-time
#Poverty (quant), Internet (quant), Social participation - all seems valid

# Dimension 4
# Economic activity - Providing care, Age pension, unemployed, sick leave, full-time very low cos2
# Social participation, Poverty (qual) - K1003 with very low cos2, potentially to drop


# Dimension 5
# Economic activity - very low cos2 for everything but Studying, despite quite considerable contrib
# Housing - K05_01 with extremely low cos2 and contrib (ownership or partial ownership of residence)
# Politics & Language (qual) - as above, problematic NOs

# Dimension 6
# Economic activity - studying, part-time, full-time, age pension with very low cos2
# Sufficient healthcare - Not receiving enough service as categories with very low cos2 but I can't separate them so I will keep them

# Dimension 7
# Economic activity, Sufficient healthcare - opposite to the Dim6, Yes and No need as categories with low cos2; Housing - housing for elder people and rented from municipality as the ones with low cos2

# Dimension 8
# Economic activity, Housing - 3 and 1 with low cos2; Relations, Sufficient healthcare - very similar to Dim7

# Dimension 9
# Economic activity, Housing - 1 and 3 with very low cos2

# Dimension 10
# Economic activity, Relations

# As a summary - I will drop K5903, K5701, K5705, K1903, and K1003!!!!!!!!!!

#investigating Economic activity one more time

metrics_long %>%
  filter(group == "Economic activity", axis == 1) %>%
  arrange(desc(contrib)) %>%
  print(n = Inf)


freq(imputed_df$K3801)


metrics_long$metric


econ_stats_long <- metrics_long %>%
  filter(group == "Economic activity", axis %in% 1:10) %>%
  pivot_longer(cols = c(coord, cos2, contrib),
               names_to = "metric",
               values_to = "value") %>%
  group_by(variable, metric) %>%
  summarise(
    min    = min(value, na.rm = TRUE),
    max    = max(value, na.rm = TRUE),
    avg    = mean(value, na.rm = TRUE),
    median = median(value, na.rm = TRUE),
    sd     = sd(value, na.rm = TRUE),
    n_axes = n_distinct(axis),
    .groups = "drop"
  ) %>%
  arrange(variable, metric) %>%
  print(n = Inf)

econ_stats_long


econ_avg_diff_full <- econ_stats_long %>%
  filter(metric %in% c("cos2", "contrib")) %>%
  select(variable, metric, avg) %>%
  pivot_wider(names_from = metric, values_from = avg) %>%
  mutate(
    avg_contrib_minus_avg_cos2 = contrib - cos2,
    across(c(contrib, cos2, avg_contrib_minus_avg_cos2), ~round(.x, 4))
  ) %>%
  arrange(desc(avg_contrib_minus_avg_cos2))

econ_avg_diff_full

#the most disturbing categories are providing care and studying, then sick leave, part-time and unemployed

weighted_freq <- imputed_df %>%
  filter(!is.na(K69_aggreg), !is.na(useweight)) %>%     # drop NAs (remove this line if you want them included)
  group_by(K69_aggreg) %>%
  summarise(w_freq = sum(useweight, na.rm = TRUE), .groups = "drop") %>%
  mutate(w_percent = 100 * w_freq / sum(w_freq)) %>%
  arrange(desc(w_freq))

weighted_freq

#the smaller the group, the bigger disruptions it has. 


# Investigating Economic activity variable

freq(imputed_df$K69_aggreg)

design <- svydesign(
  ids = ~1,                  
  weights = ~useweight,  
  data = imputed_df
)

svy_table <- svytable(~K69_aggreg + K09, design)
svy_table

fisher.test(svy_table, simulate.p.value = TRUE)

#there's significant association between economic activity and financial situation


# Kruskal–Wallis for all K69_aggreg levels

overall_kw <- svyranktest(
  K09 ~ K69_aggreg,
  design = design,
  test   = "KruskalWallis"
)

cat("Overall survey-weighted Kruskal–Wallis:\n",
    "χ² = ", round(overall_kw$statistic, 3),
    ", df = ", overall_kw$parameter,
    ", p = ", format.pval(overall_kw$p.value), "\n\n", sep = "")

pairwise_svyrank <- function(formula, design,
                             test = "KruskalWallis",
                             p.adjust.method = "bonferroni") {
  vars  <- all.vars(formula)
  g_var <- vars[2]
  
  groups <- na.omit(unique(design$variables[[g_var]]))
  pairs  <- combn(groups, 2, simplify = FALSE)
  
  map_dfr(pairs, function(pr) {
    d_sub <- subset(design, get(g_var) %in% pr)
    ht    <- svyranktest(formula, design = d_sub, test = test)
    
    tibble(
      group1    = pr[1],
      group2    = pr[2],
      statistic = unname(ht$statistic),  
      df        = unname(ht$parameter), 
      p.value   = ht$p.value
    )
  }) %>%
    mutate(p.adj = p.adjust(p.value, method = p.adjust.method))
}


pairwise_results <- pairwise_svyrank(K09 ~ K69_aggreg, design) %>%
  dplyr::mutate(across(c(group1, group2), as.character))


sig_pairs <- pairwise_results %>% 
  filter(p.value < 0.05)

nonsig_pairs <- pairwise_results %>% 
  filter(p.value >= 0.05)

sig_pairs
nonsig_pairs


cats <- sort(unique(c(pairwise_results$group1, pairwise_results$group2)))

for(cat in cats) {
  sig_others <- sig_pairs %>%
    filter(group1 == cat | group2 == cat) %>%
    transmute(other = ifelse(group1 == cat, group2, group1)) %>%
    pull(other)
  
  sim_others <- nonsig_pairs %>%
    filter(group1 == cat | group2 == cat) %>%
    transmute(other = ifelse(group1 == cat, group2, group1)) %>%
    pull(other)
  

  cat("Category:", cat, "\n")
  cat("Different:", if (length(sig_others)) paste(sig_others, collapse = ", ") else "— none —", "\n")
  cat("Similar:  ", if (length(sim_others)) paste(sim_others, collapse = ", ") else "— none —", "\n\n")
}




prop.table(svy_table, 1) * 100

svyboxplot(K09 ~ K69_aggreg, design, all.outliers = TRUE)


summary_list <- lapply(levels(imputed_df$K69_aggreg), function(g) {

  d_sub <- subset(design, K69_aggreg == g)
  

  mean_g <- coef(svymean(~K09, d_sub, na.rm = TRUE))
  

  var_g  <- coef(svyvar(~K09, d_sub, na.rm = TRUE))
  sd_g   <- sqrt(var_g)
  

  q_g    <- svyquantile(
    ~K09,
    d_sub,
    quantiles = c(0, 0.5, 1),
    ci        = FALSE,
    keep.var  = FALSE,
    na.rm     = TRUE
  )
  q_vals <- as.numeric(coef(q_g))
  
  data.frame(
    K69_aggreg = g,
    min        = q_vals[1],
    median     = q_vals[2],
    max        = q_vals[3],
    mean       = as.numeric(mean_g),
    sd         = as.numeric(sd_g),
    row.names  = NULL
  )
})

summary_table <- do.call(rbind, summary_list)
print(summary_table)

prop_df <- as.data.frame(prop.table(svy_table, 1) * 100)
names(prop_df) <- c("Category", "K09", "Percent")


prop_df$K09 <- factor(prop_df$K09, levels = sort(unique(prop_df$K09)))


plots <- lapply(unique(prop_df$Category), function(cat) {
  dat <- subset(prop_df, Category == cat)
  ggplot(dat, aes(x = K09, y = Percent)) +
    geom_col() +
    scale_y_continuous(limits = c(0, 100), expand = c(0,0)) +
    labs(title = cat, x = "K09", y = "Percent (%)") +
    theme_minimal(base_size = 12) +
    theme(
      plot.title   = element_text(face = "bold", hjust = 0.5),
      axis.title.x = element_text(margin = margin(t = 8)),
      axis.title.y = element_text(margin = margin(r = 8))
    )
})


do.call(grid.arrange, c(plots, ncol = 1))


groups <- levels(droplevels(as.factor(design$variables$K69_aggreg)))

summary_list <- lapply(groups, function(g) {
  d_sub <- subset(design, !is.na(K69_aggreg) & K69_aggreg == g & !is.na(age) & is.finite(age))
  
  if (nrow(d_sub$variables) == 0) {
    return(data.frame(
      K69_aggreg = g, min = NA_real_, median = NA_real_, max = NA_real_,
      mean = NA_real_, sd = NA_real_, row.names = NULL
    ))
  }
  
  mean_g <- as.numeric(coef(svymean(~age, d_sub, na.rm = TRUE)))
  
  var_g  <- as.numeric(coef(svyvar(~age, d_sub, na.rm = TRUE)))
  sd_g   <- ifelse(is.na(var_g), NA_real_, sqrt(var_g))
  
  q_vals <- tryCatch(
    as.numeric(coef(svyquantile(
      ~age, d_sub,
      quantiles = c(0, 0.5, 1),
      ci = FALSE, keep.var = FALSE, na.rm = TRUE
    ))),
    error = function(e) rep(NA_real_, 3)
  )
  
  data.frame(
    K69_aggreg = g,
    min    = q_vals[1],
    median = q_vals[2],
    max    = q_vals[3],
    mean   = mean_g,
    sd     = sd_g,
    row.names = NULL
  )
})

summary_table <- do.call(rbind, summary_list)
print(summary_table, row.names = FALSE)



freq(imputed_df$K69_aggreg)



#checking also relation to age

df <- within(design$variables, {
  w <- weights(design)  
})


df_plot <- subset(df, !is.na(K69_aggreg) & !is.na(age) & is.finite(age))

plots <- lapply(levels(droplevels(df_plot$K69_aggreg)), function(cat) {
  dat <- subset(df_plot, K69_aggreg == cat)
  ggplot(dat, aes(x = age, weight = w)) +
    geom_histogram(aes(y = after_stat(density)), bins = 30,
                   fill = "grey70", colour = "white", alpha = 0.6) +
    geom_density(linewidth = 1) +
    labs(title = cat, x = "Age", y = "Density") +
    theme_minimal(base_size = 12) +
    theme(
      plot.title   = element_text(face = "bold", hjust = 0.5),
      axis.title.x = element_text(margin = margin(t = 8)),
      axis.title.y = element_text(margin = margin(r = 8))
    )
})

do.call(grid.arrange, c(plots, ncol = 1))



#“Providing care …” as similar to all other groups will be joined with Unemployed - even though the mean of K09 
#is bigger in Unemployed, there's no significant difference. Moreover, they are the most similar age-wise and 
# also one person in the survey selected two categories - both unemployed and providing care. In opposite, it 
# hasn't been declared together with Part-time working.

#Studying will be also joined to Unemployed because of similarity to its K09 structure as well age one. 
#It has also been declared both with part-time working (once) and unemployment (once).


###########
#Collapsing the categories in K69_aggreg and dropping K5903, K5701, K5705, K1903, and K1003

imputed_df <- imputed_df %>%
  mutate(
    K69_aggreg = case_when(
      K69_aggreg %in% c("Unemployed or temporarily laid off", "Providing care for relatives, children or other family members", "Studying (including professional education at the workplace)") ~ "Unemployed / Providing care / Studying",
      TRUE ~ as.character(K69_aggreg)
    ),
    K69_aggreg = factor(
      K69_aggreg,
      levels = c(
        "Full-time working / Entrepreneur",
        "Sick leave, disability, rehabilitation",
        "In pension on the basis of age",
        "Part-time working",
        "Unemployed / Providing care / Studying"
      )
    )
  )

imputed_df <- select(imputed_df, -c("K5903", "K5701", "K5705", "K1903", "K1003"))




#___________________________________________

fourth_model <- imputed_df

write.csv(fourth_model, "fourth_model.csv", row.names = FALSE)


############################
# 1 · 16 variable groups
############################
orig_blocks <- list(
  c("K03","K1601","K1602","K14"),    # 1 mixed "Politics & Language",
  "K05",                            # 2 nominal "Housing",
  c("K1001","K1002","K09"), # 3 mixed "Poverty",
  c("K1101","K1102","K1103","K1104","K1105","K23","K25"), #4 ordinal  "Satisfaction & Health",
  paste0("K180",1:8),               #5 binary "Social participation",
  c("K1901","K1902","K1904"),       #6 ordinal "Culture",
  paste0("K200",1:7),               #7 ordinal "Discrimination",
  paste0("K300",1:8),               #10 ordinal "Depression",
  paste0("K380",1:3),               #11 nominal "Sufficient healthcare",
  paste0("K390",1:6),               #12 binary "Healthcare limits",
  paste0("K430",1:6),               #13 binary "Social services",
  paste0("K480",1:7),               #14 continuous "Relations",
  c("K58", setdiff(paste0("K590",1:9), "K5903"), "K5910","K5911"), #15 mixed "Internet"
  c("K69_aggreg"),                   #16 nominal "Economic activity"
  c("K66_aggreg_num")                #17 ordinal "Education
)



#   "Politics & Language",
#   "Housing",
#   "Poverty",
#   "Satisfaction & Health",
#   "Social participation",
#   "Culture",
#   "Discrimination",
#   "Depression",
#   "Sufficient healthcare",
#   "Healthcare limits",
#   "Social services",
#   "Relations",
#   "Internet"
#   "Economic activity

# socio‑demo supplementary block:
socio_demographics_qual <- c(
  "K2401","K2402","K2403","K2404","K2405","K2406",
  "K2407","K2408","K2409","K2410","K61","K63","K65"
)
socio_demographics_quant <- c(
  "age","K01"
)

############################
# 2 - separating to numeric and categorical
############################
num_vars <- c(
  "K09",paste0("K110",1:5),"K23","K25","K14",
  "K1901","K1902","K1904",paste0("K200",1:7),
   paste0("K300",1:8),paste0("K480",1:7),
  "K58","age","K01", "K66_aggreg_num"
)
cat_vars <- c(
  "K03","K05","K1001","K1002", "K1601","K1602",
  paste0("K180",1:8),paste0("K380",1:3),
  paste0("K390",1:6),paste0("K430",1:6),
  "K5901", "K5902", paste0("K590",4:9),"K5910","K5911",
  paste0("K240",1:9),"K2410","K61","K63","K65", "K69_aggreg"
)

# Ordinal-as-numeric 
safe_as_numeric <- function(x) {
  if (is.ordered(x)) return(as.integer(x))
  if (is.factor(x)) {
    xs <- as.character(x)
    ok <- grepl("^[-+]?[0-9]*\\.?[0-9]+$", xs) | is.na(xs)
    if (!all(ok)) stop("Non-numeric labels in a would-be numeric factor: ",
                       paste(unique(xs[!ok]), collapse=", "))
    return(as.numeric(xs))
  }
  as.numeric(x)
}
data.table::setDF(imputed_df)
#coercing once:

imputed_df[ intersect(cat_vars, names(imputed_df)) ] <-
  lapply(imputed_df[ intersect(cat_vars, names(imputed_df)) ],
         function(x) factor(x, exclude = NULL))

imputed_df[ intersect(num_vars, names(imputed_df)) ] <-
  lapply(imputed_df[ intersect(num_vars, names(imputed_df)) ],
         safe_as_numeric)

############################
# 3 - buidling variable groups
############################


# pure numeric blocks stay as 1 "s" group
# pure categorical blocks stay as 1 "n" group
# mixed blocks become TWO groups: one "s" (nums), one "n" (factors)
blocks_mixed  <- list()
types_mixed   <- character(0)
names_mixed   <- character(0)

for (j in seq_along(orig_blocks)) {
  blk_name <- c(
    "Politics & Language","Housing","Poverty",
    "Satisfaction & Health","Social participation",
    "Culture","Discrimination",
    "Depression","Sufficient healthcare","Healthcare limits",
    "Social services","Relations","Internet",
    "Economic activity", "Education"
  )[j]
  
  blk <- orig_blocks[[j]]
  nums <- intersect(blk, num_vars)
  cats <- intersect(blk, cat_vars)
  
  if (length(nums) > 0 && length(cats) > 0) {
    # mixed → two groups
    blocks_mixed[[length(blocks_mixed)+1]] <- nums
    types_mixed   <- c(types_mixed, "s")
    names_mixed   <- c(names_mixed, paste0(blk_name," (quant)"))
    
    blocks_mixed[[length(blocks_mixed)+1]] <- cats
    types_mixed   <- c(types_mixed, "n")
    names_mixed   <- c(names_mixed, paste0(blk_name," (qual)"))
  } else {
    # pure block
    blocks_mixed[[length(blocks_mixed)+1]] <- blk
    types_mixed   <- c(types_mixed, if (length(nums)>0) "s" else "n")
    names_mixed   <- c(names_mixed, blk_name)
  }
}

active_vars <- unlist(blocks_mixed, use.names = FALSE)
stopifnot(!any(duplicated(active_vars)))
group_sizes <- lengths(blocks_mixed)
group_types <- types_mixed
name_groups <- names_mixed


stopifnot(
  length(group_sizes) == length(group_types),
  sum(group_sizes)    == length(active_vars),
  length(name_groups) == length(group_sizes)
)

# 4 - adding supplementary variable group
sup_sizes <- c(length(socio_demographics_qual), length(socio_demographics_quant))
sup_types <- c("n", "s")
sup_names <- c("SocioDemographics (qual)", "SocioDemographics (quant)")

group_all <- c(group_sizes, sup_sizes)
type_all  <- c(group_types, sup_types)
name_all  <- c(name_groups, sup_names)


sup_index <- (length(group_all)-1):length(group_all)



############################
# Runing MFA
############################
res_mfa_mixed <- MFA(
  base           = imputed_df[, c(active_vars, socio_demographics_qual, socio_demographics_quant)],
  group          = group_all,
  type           = type_all,
  name.group     = name_all,
  row.w          = imputed_df$useweight,
  num.group.sup  = sup_index,   # now two supplementary groups
  graph          = FALSE,
  ncp            = 15
)

fourth_mfa <- res_mfa_mixed

#______________________________________
#inspecting the model


res_mfa_mixed$eig

barplot(res_mfa_mixed$eig[, 1], names.arg = seq_len(nrow(res_mfa_mixed$eig)))

factoextra::fviz_eig(res_mfa_mixed, addlabels = TRUE)

#having 18 groups, the eigenvalue of a first component is 4.35311 (compared to 4.4614 in the last model), which gives a ratio 0.24183 (compared to 0.24786 in the last model). 
#Perfect setup would be eigenvalue 18

res_mfa_mixed$group

#coord[j, s] (group–axis coordinate) = relationship/alignment of group j with axis s. 
#In the Jérôme Pagès’s terms, the coordinate of a group on axis s is a “relationship 
#measurement” (max 1 on the relationship square). higher coord ⇒ the group is more 
#aligned/correlated with axis. Use this to say which groups are aligned with an axis.
res_mfa_mixed$group$coord

#_________________________________________
#calculating Quality of representation of cloud NJ
axes <- c(5)                                 # picking the plane/axis shown
num <- colSums(res_mfa_mixed$group$coord[, axes, drop = FALSE]^2)
den <- sum(res_mfa_mixed$group$dist2)
plane_ratio_NJ <- sum(num) / den       #Quality of representation of cloud NJ from (Jérôme Pagès, 2015, p.156), Quality(NJ) tells how well the plane represents the cloud of groups; It’s a single, 0–1 score for the plane: higher ⇒ the plane shows a big share of all groups together; lower ⇒ the groups’ structure lives beyond that plane. 
plane_ratio_NJ                         # bigger ⇒ the plane shows more of all groups together
#0.007596847 now
#0,006339717 in the first model
#__________________________________
#cos2[j, s] (squared cosine) = quality of representation of group j on axis s 
#(or on a plane if you sum across axes). It’s computed by MFA and lives in 
#res$group$cos2. Use it to say how well a group is shown on an axis/plane.
res_mfa_mixed$group$cos2
#_____________
#contrib[j, s] (group contribution to axis s)
#In the FactoMineR output for groups, “the raw inertias are in coord … 
# and the percentages in contrib.” In other words, contrib gives the 
#percentage share (sums to 100% across groups for an axis) of how much 
#each group builds/constructs that axis. Use this when you want to say 
#what builds the axis. 
res_mfa_mixed$group$contrib



#_____________________________________________________
#investigating relations between groups

#Lg(Kj, Kl) — relationship (link) between two groups of variables
#It’s defined via the scalar product of the groups’ Gram matrices with the 
# MFA weighting; it can exceed 1 and has no fixed upper bound. It grows when 
# the two groups share more common directions and when those directions carry 
# high inertia. It is 0 iff every variable of one group is uncorrelated with 
# every variable of the other. 

res_mfa_mixed$group$Lg

#RV(Kj, Kl) — Escoufier’s RV coefficient between two groups
# When the groups are standardised in the groups’ space, the scalar product 
# behaves like a cosine, which gives RV; it is bounded in [0,1] and equals 
# 1 when the two groups’ clouds of individuals are homothetic. It is 0 under 
# complete lack of cross-group correlations.

res_mfa_mixed$group$RV

#no considerable relations between groups, except for some examples of groups split 
#into quant and qual (as they are covering the same topic, they will relate to each other)

#As a result, group structure will remain as it is. Possible changes will only involve which groups are included in the MFA


#_______________________________________________________
#Investigating groups to select those I will keep and those I will drop



mfa_group_axis_table <- function(res, axis = 1, sort_by = c("coord","cos2","contrib"),
                                 top_n = NULL, digits = 3) {
  sort_by <- match.arg(sort_by)
  stopifnot(axis >= 1, axis <= ncol(res$group$coord))
  

  coord  <- res$group$coord[,  axis]            # relationship/alignment with axis
  cos2   <- res$group$cos2[,   axis]            # quality of representation on axis
  contrib <- res$group$contrib[, axis]      # contribution
  

  out <- data.frame(
    group        = rownames(res$group$coord),
    coord        = coord,
    cos2         = cos2,
    contrib = contrib,
    stringsAsFactors = FALSE
  )
  

  ord_col <- sort_by
  out <- out[order(-out[[ord_col]]), , drop = FALSE]
  

  out$coord       <- round(out$coord, digits)
  out$cos2        <- round(out$cos2, digits)
  out$contrib <- round(out$contrib, digits)
  

  if (!is.null(top_n)) out <- head(out, top_n)
  
  rownames(out) <- NULL
  out
}

options(scipen = 999)  

for (i in 1:10){
  print(paste("Dimension ", i))
  tbl3 <- mfa_group_axis_table(res_mfa_mixed, axis = i, sort_by = "contrib")
  print(tbl3)
}


#Dimension 1
# Satisfaction & Health, Healthcare limits, Depression

#Dimension 2
# Discrimination, Internet (qual), Culture, Politics & Language (quant), Politics & Language (qual)

#Dimension 3
# Economic activity, Social participation, Satisfaction & Health 

#Dimension 4
# Economic activity, Internet (qual), Internet (quant)

#Dimension 5
# Housing, Politics & Language (qual), Economic activity, Sufficient healthcare, Politics & Language (quant)

#Dimension 6
# Economic activity, Education

#Dimension 7
# Economic activity, Sufficient healthcare

#Dimension 8
# Housing, Education, Economic activity

#Dimension 9
# Sufficient healthcare, Education, Economic activity

#Dimension 10
# Relations, Economic activity


# This is a clear improvement compared to the previous model - last time Economic activity took the first place in 8 out of 10 first axes
# Here it is only 4 out of 10 first.


#_________________________
#Analyzing quality of groups across larger amount of dimensions

get_mfa_eig <- function(res, axes = 1:res$call$ncp) {
  # ‘res$eig’ is the eigenvalue table: col 2 = % of variance, col 3 = cumulative %
  eig <- res$eig
  max_axis <- nrow(eig)
  
  if (any(axes < 1 | axes > max_axis)) {
    stop("Requested axes must be between 1 and ", max_axis, call. = FALSE)
  }
  
  var_pct <- eig[axes, "percentage of variance"]
  names(var_pct) <- paste0("Dim", axes)
  return(var_pct)
}

amount_of_dimensions = 4

w <- get_mfa_eig(res_mfa_mixed, axes = 1:amount_of_dimensions)

#extracting the contrib matrix (groups × axes)
C <- res_mfa_mixed$group$contrib[, 1:amount_of_dimensions] / 100

#computing weighted contribution index - to analyze importance of a group 
#considering their contribution to the axes 

wci_vec <- setNames(
  as.numeric(C %*% w),
  rownames(C)
)
wci_vec <- sort(wci_vec, decreasing = TRUE)


wci_df <- data.frame(
  group = names(wci_vec),
  WCI   = unname(wci_vec),
  row.names = NULL,
  stringsAsFactors = FALSE
)


wci_df
amount_of_dimensions


#I'll investigate now the very variables, within the group level.

#_____________________________________________________________________


#######################
#number of dimensions produced

n_axes <- ncol(res_mfa_mixed$quanti.var$coord)

#######################
# Building a mapping: each MFA variable → its varaible group

#identifying active variable group names
sup_idx      <- res_mfa_mixed$call$num.group.sup %||% integer(0)
active_blks  <- res_mfa_mixed$call$name.group[-sup_idx]

# for each block, grabbing the rownames it produced in res$separate.analyses
group_vars <- setNames(
  lapply(seq_along(active_blks), function(i) {
    sep <- res_mfa_mixed$separate.analyses[[i]]
    rownames(sep$var$coord)
  }),
  active_blks
)

#inverting that list into a named vector var → variable group
var2group <- unlist(lapply(names(group_vars), function(g) {
  setNames(rep(g, length(group_vars[[g]])), group_vars[[g]])
}))

#######################
# Pulling global MFA metrics for every variable & axis

build_df <- function(vl) {
  df <- data.frame(variable = rownames(vl$coord),
                   stringsAsFactors = FALSE)
  for (ax in seq_len(n_axes)) {
    df[[paste0("coord",   ax)]] <- vl$coord[,   ax]
    df[[paste0("cos2",    ax)]] <- vl$cos2[,    ax]
    df[[paste0("contrib", ax)]] <- vl$contrib[, ax]
  }
  df
}

df_q <- build_df(res_mfa_mixed$quanti.var)
df_l <- build_df(res_mfa_mixed$quali.var)
metrics <- rbind(df_q, df_l)

#######################
# Assigning the correct block to each row via var2group

metrics$group <- var2group[metrics$variable]

 check: no missing groups
stopifnot(!any(is.na(metrics$group)))

#######################
# Fixing format


metrics_long <- metrics %>%
  pivot_longer(
    cols = -c(variable, group),
    names_to  = c(".value", "axis"),
    names_pattern = "(coord|cos2|contrib)(\\d+)"
  ) %>%
  mutate(axis = as.integer(axis))


metrics_long %>%
  filter(group == "Relations", axis == 3) %>%
  arrange(desc(contrib)) %>%
  print(n = Inf)


metrics_long %>%
  filter(group == "Economic activity", axis == 1) %>%
  arrange(desc(contrib)) %>%
  print(n = Inf)



econ_stats_long <- metrics_long %>%
  filter(group == "Economic activity", axis %in% 1:10) %>%
  pivot_longer(cols = c(coord, cos2, contrib),
               names_to = "metric",
               values_to = "value") %>%
  group_by(variable, metric) %>%
  summarise(
    min    = min(value, na.rm = TRUE),
    max    = max(value, na.rm = TRUE),
    avg    = mean(value, na.rm = TRUE),
    median = median(value, na.rm = TRUE),
    sd     = sd(value, na.rm = TRUE),
    n_axes = n_distinct(axis),
    .groups = "drop"
  ) %>%
  arrange(variable, metric) %>%
  print(n = Inf)

econ_stats_long


econ_avg_diff_full <- econ_stats_long %>%
  filter(metric %in% c("cos2", "contrib")) %>%
  select(variable, metric, avg) %>%
  pivot_wider(names_from = metric, values_from = avg) %>%
  mutate(
    avg_contrib_minus_avg_cos2 = contrib - cos2,
    across(c(contrib, cos2, avg_contrib_minus_avg_cos2), ~round(.x, 4))
  ) %>%
  arrange(desc(avg_contrib_minus_avg_cos2))

econ_avg_diff_full


weighted_freq <- imputed_df %>%
  filter(!is.na(K69_aggreg), !is.na(useweight)) %>%     # drop NAs (remove this line if you want them included)
  group_by(K69_aggreg) %>%
  summarise(w_freq = sum(useweight, na.rm = TRUE), .groups = "drop") %>%
  mutate(w_percent = 100 * w_freq / sum(w_freq)) %>%
  arrange(desc(w_freq))

weighted_freq


###########################################################
############ Comparing quality of 4 models ################
###########################################################

mfa_objects <- list(
  first_mfa  = first_mfa,
  second_mfa = second_mfa,
  third_mfa  = third_mfa,
  fourth_mfa = fourth_mfa
)

for (nm in names(mfa_objects)) {
  mfa <- mfa_objects[[nm]]
  cat("\n==== Eigenvalues for", nm, "====\n\n")
  
  print(mfa$eig)
  cat("\n\n")
}

#The eigenvalues keep decreasing however the explained variance is growing for the components decreasing their amount from 106 in the first model (50 % of explained variance on 14th/15th component) to 85 in the fourth one (50 % of explained variance on 11th/12th component).
#The fourth model seems to be the best one

for (nm in names(mfa_objects)) {
  mfa <- mfa_objects[[nm]]
  cat("\n==== Eigenvalues for", nm, "====\n")
  ev1 <- mfa$eig[1, 1]
  n_groups <- nrow(mfa$group$contrib)
  ratio <- ev1 / n_groups
  
  cat("First-axis eigenvalue: ", ev1, "\n", sep = "")
  cat("Number of groups:      ", n_groups, "\n", sep = "")
  cat("EV1 / #groups:         ", ratio, "\n", sep = "")
}

#Also ratio between the number of axes and number of groups is bigger compared to 1st model which is a desirable indicator (however model 3 and 4 have slightly smaller values than model 2)


#plane_ratio_NJ (Pagès, 2015) per MFA on axis 10 

mfa_objects <- list(
  first_mfa  = first_mfa,
  second_mfa = second_mfa,
  third_mfa  = third_mfa,
  fourth_mfa = fourth_mfa
)

# NJ metrics per MFA: per-axis, plane (1,2), cumulative
for (nm in names(mfa_objects)) {
  mfa <- mfa_objects[[nm]]
  cat("\n=============== NJ quality for", nm, "===============\n")
  
  # Safety checks
  if (is.null(mfa$group$coord) || is.null(mfa$group$dist2)) {
    cat("Missing group coordinates or dist2; skipping.\n")
    next
  }
  
  max_axis <- ncol(mfa$group$coord)
  A <- seq_len(min(10, max_axis))
  if (length(A) == 0) {
    cat("No axes available; skipping.\n")
    next
  }
  
  den <- sum(mfa$group$dist2)
  if (!is.finite(den) || den == 0) {
    cat("Denominator is zero or non-finite; skipping.\n")
    next
  }
  
  per_axis_NJ <- sapply(A, function(a) {
    num <- sum(mfa$group$coord[, a, drop = FALSE]^2)
    num / den
  })
  cat("\n-- Per-axis NJ (1..", max(A), ") --\n", sep = "")
  print(data.frame(axis = A, NJ = as.numeric(per_axis_NJ), row.names = NULL))
  
  cat("\n-- Plane (1:10) NJ --\n")
  if (max_axis >= 10) {
    num110 <- sum(colSums(mfa$group$coord[, 1:10, drop = FALSE]^2))
    plane110 <- num110 / den
    print(plane110)
  } else {
    cat("Model has < 2 axes; plane (1,2) not available.\n")
  }

  cum_NJ <- sapply(A, function(k) {
    num_k <- sum(colSums(mfa$group$coord[, 1:k, drop = FALSE]^2))
    num_k / den
  })
  cat("\n-- Cumulative NJ S_sp(1..k) --\n")
  print(data.frame(k = A, NJ_cumulative = as.numeric(cum_NJ), row.names = NULL))
  

  if (max_axis < 10) {
    cat("(Requested up to axis 10; this model has only", max_axis, "axes.)\n")
  }
}

# NJ for each model is growing which is a desirable sign. For model 3 however the cumulative growth is higher for 1:10 than in model 4. But model 4 has slightly better performance on the more important dimensions, 1:6 so it is still performing better than the model 3

# ---  Summarizing metrics per group ---


mfa_names   <- c("first_mfa", "second_mfa", "third_mfa", "fourth_mfa")

# How many axes to use
amount_of_dimensions <- 10
axes <- 1:amount_of_dimensions

for (i in seq_along(mfa_objects)) {
  mfa <- mfa_objects[[i]]
  cat("\n--- Results for", mfa_names[i], "---\n")
  

  max_axis <- ncol(mfa$group$coord)
  axes_i <- axes[axes <= max_axis]
  if (length(axes_i) == 0) {
    cat("No available axes. Skipping.\n")
    next
  }
  
  #weights (% variance of each axis)
  w <- get_mfa_eig(mfa, axes = axes_i)
  
  #group × axis contributions
  C <- mfa$group$contrib[, axes_i, drop = FALSE]
  
  #group × axis cos2 (normalise if needed)
  G <- mfa$group$cos2[, axes_i, drop = FALSE]
  if (max(G, na.rm = TRUE) > 1) G <- G / 100
  
  # Weighted Contribution Index (WCI)
  wci_vec <- setNames(as.numeric(C %*% (w / 100)), rownames(C))
  
  #aggregates
  contrib_sum   <- rowSums(C, na.rm = TRUE)
  cos2_sum      <- rowSums(G, na.rm = TRUE)
  diff_sum      <- contrib_sum - cos2_sum
  contrib_mean  <- contrib_sum / length(axes_i)
  cos2_mean     <- cos2_sum / length(axes_i)
  ratio_sum     <- ifelse(cos2_sum == 0, NA_real_, contrib_sum / cos2_sum)
  
  ratio_df <- data.frame(
    group              = rownames(C),
    WCI                = unname(wci_vec),
    contrib_sum        = contrib_sum,
    cos2_sum           = cos2_sum,
    contrib_minus_cos2 = diff_sum,
    contrib_over_cos2  = ratio_sum,
    contrib_mean       = contrib_mean,
    cos2_mean          = cos2_mean,
    row.names = NULL,
    stringsAsFactors = FALSE
  )
  
  ratio_df <- ratio_df[order(ratio_df$contrib_over_cos2, decreasing = TRUE), ]
  print(ratio_df)
  
  cat("\n--- Averages across groups for", mfa_names[i], "---\n")
  numeric_cols <- setdiff(names(ratio_df), "group")
  summaries <- sapply(ratio_df[, numeric_cols, drop = FALSE], function(x) mean(x, na.rm = TRUE))
  summary_row <- data.frame(mfa = mfa_names[i], t(summaries), row.names = NULL, check.names = FALSE)
  print(summary_row)
}


#distance between contrib and cos2 has significantly decreased between model 1 and 4. The cos2, meaning the amount of explained variability from groups, is growing across all models. All these metrics show a good progress with model 4 compared to all previous ones.

#mfa_group_axis_table over axes 1:10 for each MFA

options(scipen = 999)

for (nm in names(mfa_objects)) {
  mfa <- mfa_objects[[nm]]
  cat("\n==== Group axis tables for", nm, "====\n\n")
  
  max_axis <- ncol(mfa$group$coord)
  for (i in 1:10) {
    cat("\n--- Dimension", i, "---\n")
    if (i > max_axis) {
      cat("Axis", i, "not available (max =", max_axis, "). Skipping.\n")
      next
    }
    tbl3 <- mfa_group_axis_table(mfa, axis = i, sort_by = "contrib")
    print(tbl3)
    cat("\n\n")
  }
}

#Model 4 is the only one where Economic activity group do not dominate the majority of the dimensions
