library(pacman)
p_load(ade4, writexl, openxlsx, MatrixCorrelation, psych, vegan, ggtext, tikzDevice, grid, showtext, sysfonts,  ggrepel, gridExtra, rstatix, FSA, survey, jtools, dplyr, haven, ggplot2, reshape2, knitr, kableExtra, summarytools, wCorr, corrplot, BiocManager, ggcorrplot, FactoMineR, labelled, factoextra, rcompanion, topicmodels, ggpubr, stats, tidyr, textshape, effsize, purr, cluster)
setwd("path")

intersectional_data <- read.csv("fourth_model.csv")

intersectional_data$K61 <- as.factor(intersectional_data$K61)


font_add(family = "LMRoman",
         regular    = "C:/Users/sierp/AppData/Local/Programs/MiKTeX/fonts/opentype/public/lm/lmroman10-regular.otf",
         bold       = "C:/Users/sierp/AppData/Local/Programs/MiKTeX/fonts/opentype/public/lm/lmroman10-bold.otf",
         italic     = "C:/Users/sierp/AppData/Local/Programs/MiKTeX/fonts/opentype/public/lm/lmroman10-italic.otf",
         bolditalic = "C:/Users/sierp/AppData/Local/Programs/MiKTeX/fonts/opentype/public/lm/lmroman10-bolditalic.otf")

showtext_auto()

update_geom_defaults("text_repel",  list(family = "LMRoman"))

update_geom_defaults("label_repel", list(family = "LMRoman"))


intersectional_data <- intersectional_data %>%
  mutate(K69_aggreg = case_when(
    K69_aggreg == "Full-time working / Entrepreneur" ~ 1,
    K69_aggreg == "Sick leave, disability, rehabilitation" ~ 2,
    K69_aggreg == "In pension on the basis of age" ~ 3,
    K69_aggreg == "Part-time working" ~ 4,
    K69_aggreg == "Unemployed / Providing care / Studying" ~ 5,
    TRUE ~ NA_real_   # fallback to avoid type clash
  ))

freq(intersectional_data$K69_aggreg)

####################################################################################################################
####################################################################################################################
######################### Return to basic data processing before performing MFA ####################################
####################################################################################################################
####################################################################################################################


#Changing variable labels and values into more readible ones (according to Kimmo Vehkalahti's comments)

# 0 → "Y", 1 → "N"
cols_zero_to_YN <- c("K1001","K1002", paste0("K390",1:6), paste0("K430",1:6))
intersectional_data <- intersectional_data %>%
  mutate(across(all_of(cols_zero_to_YN),
                ~ if_else(.x == 0, "y",
                          if_else(.x == 1, "n", NA_character_))))

# 1 → "Y", 0 → "N"
cols_one_to_YN <- c(paste0("K180",1:8), paste0("K590",1:2), paste0("K590",4:9), "K5910","K5911", "K03")
intersectional_data <- intersectional_data %>%
  mutate(across(all_of(cols_one_to_YN),
                ~ if_else(.x == 1, "y",
                          if_else(.x == 0, "n", NA_character_))))

# K05: 1 > own, 2 > rent_priv, 3 > rent_non_priv
intersectional_data <- intersectional_data %>%
  mutate(K05 = recode(K05,
                      `1` = "own",
                      `2` = "rent_priv",
                      `3` = "rent_non_priv",
                      .default = NA_character_))


# K1601, K1602: 1 > no_right, 2 > n, 3 > y
k16 <- c("K1601","K1602")
intersectional_data <- intersectional_data %>%
  mutate(across(all_of(k16),
                ~ recode(.x,
                         `1` = "no_right",
                         `2` = "n",
                         `3` = "y",
                         .default = NA_character_)))

# K3801:K3803: 1 > y, 2 > n, 3 > no_need
k38 <- paste0("K380", 1:3)
intersectional_data <- intersectional_data %>%
  mutate(across(all_of(k38),
                ~ recode(.x,
                         `1` = "y",
                         `2` = "n",
                         `3` = "no_need",
                         .default = NA_character_)))

# K69_aggreg: 1..5 > labels
intersectional_data <- intersectional_data %>%
  mutate(K69_aggreg = recode(K69_aggreg,
                             `1` = "full_working",
                             `2` = "sick_disability_rehab",
                             `3` = "age_pension",
                             `4` = "part_working",
                             `5` = "not_working_care_study",
                             .default = NA_character_))


intersectional_data <- intersectional_data %>%
  rename(
    fin_citizen = K03,
    housing = K05,
    econ_status = K09,
    food_runout = K1001,
    food_aid = K1002,
    sat_health = K1101,
    sat_daily_cap = K1102,
    sat_self = K1103,
    sat_relations = K1104,
    sat_living_std = K1105,
    lang_skill = K14,
    vote_parl = K1601,
    vote_pres = K1602,
    part_sport = K1801,
    part_hobby = K1802,
    part_volunt = K1803,
    part_culture = K1804,
    part_senior = K1805,
    part_party_union = K1806,
    part_relig = K1807,
    part_rus = K1808,
    cinema_freq = K1901,
    restaurant_freq = K1902,
    theater_freq = K1904,
    disc_less_polite = K2001,
    disc_worse_srvc = K2002,
    disc_treated_infer = K2003,
    disc_threatened = K2004,
    disc_fear_public = K2005,
    disc_worse_srvc_hc = K2006,
    disc_worse_srvc_pub = K2007,
    self_rated_health = K23,
    health_limit_long = K25,
    dep_depressed = K3001,
    dep_effort = K3002,
    dep_restless_slp = K3003,
    dep_happy = K3004,
    dep_lonely = K3005,
    dep_enjoy_life = K3006,
    dep_sad = K3007,
    dep_cant_get_go = K3008,
    suff_gp = K3801,
    suff_spec = K3802,
    suff_nurse = K3803,
    hl_wait_time = K3901,
    hl_transport = K3902,
    hl_expensive = K3903,
    hl_poor_srvc = K3904,
    hl_lang = K3905,
    hl_sys_complex = K3906,
    ben_basic_assist = K4301,
    ben_home_srvc = K4302,
    ben_inf_care = K4303,
    ben_housing = K4304,
    ben_kela_taxi = K4305,
    ben_social_worker = K4306,
    relatives_meet = K4801,
    relatives_personal = K4802,
    relatives_help = K4803,
    friends_meet = K4804,
    friends_personal = K4805,
    friends_help = K4806,
    friends_born_fi = K4807,
    net_freq = K58,
    net_text_call = K5901,
    net_banking = K5902,
    net_so_me = K5904,
    net_soc_hc_srvc = K5905,
    net_other_srvc = K5906,
    net_omakanta = K5907,
    net_search_health = K5908,
    net_hc_abroad = K5909,
    net_health_peer = K5910,
    net_cmp_hc_price = K5911,
    education = K66_aggreg_num,
    activity = K69_aggreg)


design <- svydesign(ids = ~1, data = intersectional_data, weights = ~useweight)

##############################################################################
#Investigating age and gender variables

#As retirement age for those born in 1965 or earlier is 65, it means that 
#all individuals in the sample who are 65 years old, are eligible for retirement 
#as in 2019, during data collection, 65-year-olds were born in 1954

#Thus, for simplicity reasons, I'm planning to create 4 intersectional positions:
#female younger than 65, female aged 65 and more, male younger than 65 and male aged 65 or more.

#First, I investigate data

svyboxplot(age ~ K61, design, all.outliers = TRUE)
svyby(~age, ~K61, design, svyquantile, quantiles=0.5)

gender_age <- svytable(~age + K61, design)

df <- within(design$variables, {
  w <- weights(design)  
})


df_plot <- subset(df, !is.na(K61) & !is.na(age) & is.finite(age))


plots <- lapply(levels(droplevels(df_plot$K61)), function(cat) {
  dat <- subset(df_plot, K61 == cat)
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


summary_list <- lapply(levels(intersectional_data$K61), function(g) {
  d_sub <- subset(design, K61 == g)
  mean_g <- coef(svymean(~age, d_sub, na.rm = TRUE))
  var_g  <- coef(svyvar(~age, d_sub, na.rm = TRUE))
  sd_g   <- sqrt(var_g)
  q_g    <- svyquantile(
    ~age,
    d_sub,
    quantiles = c(0, 0.5, 1),
    ci        = FALSE,
    keep.var  = FALSE,
    na.rm     = TRUE
  )
  q_vals <- as.numeric(coef(q_g))  
  
  data.frame(
    K61        = g,
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


# Binary vs Continuous: T-test or Wilcoxon test
normality_test <- shapiro.test(intersectional_data$age)$p.value #it's smaller than 0.05, so I will perform wilcoxon test
continuous_var <- "age"
binary_var <- "K61"

design_subset <- svydesign(ids = ~1, data = intersectional_data, weights = ~useweight)

rank_test <- svyranktest(as.formula(paste(continuous_var, "~", binary_var)), design_subset, test="wilcoxon")
p_value <- rank_test$p.value

#There's no enough evidence to conclude a significant difference in medians

###############################################################################
###############################################Creating intersectional variable

intersectional_data <- intersectional_data %>%
  mutate(age_gender = case_when(
    K61 == 1 & age < 65 ~ "female <65",
    K61 == 1 & age >= 65 ~ "female 65+",
    K61 == 2 & age < 65 ~ "male <65",
    K61 == 2 & age >= 65 ~ "male 65+",
    TRUE ~ NA_character_
  ))

freq(intersectional_data$age_gender)

intersectional_data %>%
  group_by(age_gender) %>%
  summarise(weighted_count = sum(useweight, na.rm = TRUE)) %>%
  arrange(age_gender)

intersectional_df <- intersectional_data

intersectional_df <- intersectional_df %>%
  mutate(ID = sprintf("ID_%05d", row_number()))


rownames(intersectional_df) <- intersectional_df$ID


id_lists <- split(rownames(intersectional_df),
                  intersectional_df$age_gender, drop = TRUE)


names(id_lists)



#Creating separate data sets for each intersectional position

split_dfs <- intersectional_df %>%
  group_split(age_gender) %>%
  setNames(unique(intersectional_df$age_gender)) %>%
  lapply(function(df) select(df, -age_gender))

female_younger65 <- as.data.frame(split_dfs[["female <65"]])
female_65plus    <- as.data.frame(split_dfs[["female 65+"]])
male_younger65   <- as.data.frame(split_dfs[["male <65"]])
male_65plus      <- as.data.frame(split_dfs[["male 65+"]])

rownames(female_younger65) <- female_younger65$ID
rownames(female_65plus) <- female_65plus$ID
rownames(male_younger65) <- male_younger65$ID
rownames(male_65plus) <- male_65plus$ID

intersectional_df <- select(intersectional_df, -c("ID"))
female_younger65 <- select(female_younger65, -c("ID"))
female_65plus <- select(female_65plus, -c("ID"))
male_younger65 <- select(male_younger65, -c("ID"))
male_65plus <- select(male_65plus, -c("ID"))

intersectional_df <- intersectional_df %>%
  mutate(gender = case_when(
    K61 == 1 ~ "female",
    K61 == 2 ~ "male",
    TRUE ~ NA_character_
  ))

intersectional_df <- intersectional_df %>%
  mutate(age_group = case_when(
    age < 65 ~ "<65",
    age >= 65 ~ "65+",
    TRUE ~ NA_character_
  ))


############### Calculating descriptives

freq(intersectional_df$age_group)
freq(intersectional_df$gender)


design <- svydesign(ids = ~1, data = intersectional_df, weights = ~useweight)

tbl_w <- svytable(~ age_gender, design = design)


tbl_w[] <- paste0(
  format(round(as.vector(tbl_w), 2), nsmall = 2),       
  " (",
  format(round(prop.table(tbl_w) * 100, 2), nsmall = 2),
  "%)"
)

tbl_w

#Performing MFA with age_gender as a supplementary variable

##########################################
# 1 - Variable groups
##########################################
orig_blocks <- list(
  c("fin_citizen","vote_parl", "vote_pres", "lang_skill"),    # 1 mixed "Politics & Language",
  "housing",                            # 2 nominal "Housing",
  c("food_runout","food_aid","econ_status"), # 3 mixed "Poverty",
  c("sat_health", "sat_daily_cap", "sat_self", "sat_relations", "sat_living_std","self_rated_health","health_limit_long"), #4 ordinal  "Satisfaction & Health",
  c("part_sport", "part_hobby", "part_volunt", "part_culture", "part_senior", "part_party_union", "part_relig", "part_rus"),               #5 binary "Social participation",
  c("cinema_freq","restaurant_freq","theater_freq"),       #6 ordinal "Culture",
  c("disc_less_polite", "disc_worse_srvc",  "disc_treated_infer", "disc_threatened", "disc_fear_public", "disc_worse_srvc_hc",  "disc_worse_srvc_pub"),               #7 ordinal "Discrimination",
  c("dep_depressed",  "dep_effort", "dep_restless_slp", "dep_happy",  "dep_lonely", "dep_enjoy_life", "dep_sad", "dep_cant_get_go"),               #8 ordinal "Depression",
  c("suff_gp", "suff_spec", "suff_nurse"),               #9 nominal "Sufficient healthcare",
  c("hl_wait_time", "hl_transport", "hl_expensive", "hl_poor_srvc", "hl_lang", "hl_sys_complex"),               #10 binary "Healthcare limits",
  c("ben_basic_assist", "ben_home_srvc", "ben_inf_care", "ben_housing", "ben_kela_taxi", "ben_social_worker"),               #11 binary "Social services",
  c("relatives_meet", "relatives_personal", "relatives_help", "friends_meet", "friends_personal", "friends_help", "friends_born_fi"),               #12 continuous "Relations",
  c("net_freq", "net_text_call", "net_banking", "net_so_me", "net_soc_hc_srvc", "net_other_srvc", "net_omakanta", "net_search_health", "net_hc_abroad", "net_health_peer", "net_cmp_hc_price"), #13 mixed "Internet"
  c("education"),                   #14 nominal "Economic activity"
  c("activity")               #15 ordinal "Education"
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
#   "Education"


supplementary <- c(
  "age_gender"
)

##########################################
#Separating categorical from numeric
##########################################
num_vars <- c(
  "econ_status","sat_health", "sat_daily_cap", "sat_self", "sat_relations", "sat_living_std","self_rated_health","health_limit_long","lang_skill",
  "cinema_freq","restaurant_freq","theater_freq","disc_less_polite", "disc_worse_srvc",  "disc_treated_infer", "disc_threatened", "disc_fear_public", "disc_worse_srvc_hc",  "disc_worse_srvc_pub",
  "dep_depressed",  "dep_effort", "dep_restless_slp", "dep_happy",  "dep_lonely", "dep_enjoy_life", "dep_sad", "dep_cant_get_go",
  "relatives_meet", "relatives_personal", "relatives_help", "friends_meet", "friends_personal", "friends_help", "friends_born_fi",
  "net_freq", "education"
)
cat_vars <- c(
  "fin_citizen","housing","food_runout","food_aid", "vote_parl", "vote_pres",
  "part_sport", "part_hobby", "part_volunt", "part_culture", "part_senior", "part_party_union", "part_relig", "part_rus",
  "suff_gp", "suff_spec", "suff_nurse",
  "hl_wait_time", "hl_transport", "hl_expensive", "hl_poor_srvc", "hl_lang", "hl_sys_complex",
  "ben_basic_assist", "ben_home_srvc", "ben_inf_care", "ben_housing", "ben_kela_taxi", "ben_social_worker",
  "net_text_call", "net_banking", "net_so_me", "net_soc_hc_srvc", "net_other_srvc", "net_omakanta", "net_search_health", "net_hc_abroad", "net_health_peer", "net_cmp_hc_price",
  "activity", "age_gender"
)




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

data.table::setDF(intersectional_df)
#coercing once:

intersectional_df[ intersect(cat_vars, names(intersectional_df)) ] <-
  lapply(intersectional_df[ intersect(cat_vars, names(intersectional_df)) ],
         function(x) factor(x, exclude = NULL))

intersectional_df[ intersect(num_vars, names(intersectional_df)) ] <-
  lapply(intersectional_df[ intersect(num_vars, names(intersectional_df)) ],
         safe_as_numeric)

##########################################
# 3 - Building variable groups
##########################################

#pure numeric blocks stay as 1 "s" group
#pure categorical blocks stay as 1 "n" group
#mixed blocks become TWO groups: one "s" (nums), one "n" (factors)
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

##########################################
# 4 - Adding supplementary variables
##########################################

sup_sizes <- c(length(supplementary))

sup_types <- c("n")#, "s")
sup_names <- c("Age_gender")


group_all <- c(group_sizes, sup_sizes)
type_all  <- c(group_types, sup_types)
name_all  <- c(name_groups, sup_names)


sup_index <- length(group_all)



##########################################
# Running MFA
##########################################
res_mfa_mixed <- MFA(
  base           = intersectional_df[, c(active_vars, supplementary)],#socio_demographics_qual, socio_demographics_quant)],
  group          = group_all,
  type           = type_all,
  name.group     = name_all,
  row.w          = intersectional_df$useweight,
  num.group.sup  = sup_index,
  graph          = FALSE,
  ncp            = 2
)


fviz_mfa_var(res_mfa_mixed, 
             repel = TRUE,
             choice = "group",
             axes = c(1, 2)) 

idx_active <- setdiff(seq_along(group_all), sup_index)

plot(res_mfa_mixed,
     choix = "group",
     select = idx_active,
     graph.type = "ggplot")


plot(res_mfa_mixed,
     choix = "ind",
     axes = c(1, 2),
     invisible = c("ind","ind.sup","quali"),
     graph.type = "ggplot")



res_mfa_mixed$quali.var.sup


#### Plotting either quant only or qual only


dims      <- c(1, 2)
plot_what <- "qual"  # "quant" or "qual"


stopifnot(length(dims) == 2, all(dims %in% seq_len(nrow(res_mfa_mixed$eig))))
xlab <- sprintf("Dimension %d (%.1f%%)", dims[1], res_mfa_mixed$eig[dims[1], 2])
ylab <- sprintf("Dimension %d (%.1f%%)", dims[2], res_mfa_mixed$eig[dims[2], 2])


pal <- c(
  "Dark blue" = "#12436D",
  "Turquoise" = "#28A197",
  "Dark pink" = "#801650",
  "Orange"    = "#F46A25"
)


font_family  <- "LMRoman"
axis_text_sz <- 8
axis_title_sz<- 8
lab_size_var <- 1.75 
lab_size_cat <- 1.75
sq_size_cat  <- 1 

# colours
col_var_text <- "#3D3D3D" 
col_arrow    <- "gray" 
col_axes     <- "gray"
sq_alpha <- 0.7

# arrow/circle thickness
circle_lwd    <- 0.40
axes_lwd      <- 0.20
arrow_lwd     <- 0.25
arrow_len_npc <- 0.005
arrow_angle   <- 20



pick <- function(x, slot = "coord") {
  if (!is.null(x) && !is.null(x[[slot]]) && nrow(x[[slot]]) > 0) as.data.frame(x[[slot]]) else NULL
}
axis_cols <- function(df, dims) {
  rn <- colnames(df)
  a <- character(2)
  for (i in 1:2) {
    c1 <- paste0("Dim ", dims[i]); c2 <- paste0("Dim.", dims[i])
    a[i] <- if (c1 %in% rn) c1 else if (c2 %in% rn) c2 else rn[dims[i]]
  }
  a
}
# unit circle (correlation circle)
circle_df <- data.frame(
  x = cos(seq(0, 2*pi, length.out = 360L)),
  y = sin(seq(0, 2*pi, length.out = 360L))
)



# ───────────────── QUANT: variables + age_gender (squares) ─────────────────
if (plot_what == "quant") {
  
  Qact <- pick(res_mfa_mixed$quanti.var, "coord");      if (!is.null(Qact)) Qact$status <- "active"
  Qsup <- pick(res_mfa_mixed$quanti.var.sup, "coord");  if (!is.null(Qsup)) Qsup$status <- "supplementary"
  Q <- do.call(rbind, Filter(Negate(is.null), list(Qact, Qsup)))
  stopifnot(!is.null(Q), nrow(Q) > 0)
  axQ <- axis_cols(Q, dims)
  Q$lab <- rownames(Q)
  
  Csup <- pick(res_mfa_mixed$quali.var.sup, "coord")
  if (is.null(Csup) || nrow(Csup) == 0) Csup <- pick(res_mfa_mixed$quali.var, "coord")
  has_age_gender <- !is.null(Csup) && nrow(Csup) > 0
  if (has_age_gender) {
    rows <- grepl("^age_gender=", rownames(Csup))
    if (any(rows)) Csup <- Csup[rows, , drop = FALSE]
    axC <- axis_cols(Csup, dims)
    levs <- levels(factor(rownames(Csup)))
    cat_cols <- setNames(unname(pal[seq_len(min(4, length(levs)))]), levs)
    C_plot <- data.frame(
      x = Csup[, axC[1]], y = Csup[, axC[2]],
      age_gender = factor(rownames(Csup), levels = levs),
      lab = rownames(Csup), row.names = rownames(Csup), check.names = FALSE
    )
  }
  
  rng <- c(as.numeric(Q[[axQ[1]]]), as.numeric(Q[[axQ[2]]]),
           if (has_age_gender) c(as.numeric(C_plot$x), as.numeric(C_plot$y)) else numeric(0))
  lim <- max(1, 1.05 * max(abs(rng), na.rm = TRUE))
  
  quant_1and2 <- ggplot() +
    geom_path(data = circle_df, aes(x = x, y = y), colour = "grey70", linewidth = circle_lwd) +
    geom_segment(
      data = Q,
      aes(x = 0, y = 0, xend = .data[[axQ[1]]], yend = .data[[axQ[2]]]),
      arrow  = arrow(length = unit(arrow_len_npc, "npc"), angle = arrow_angle, type = "closed"),
      colour = col_arrow, linewidth = arrow_lwd
    ) +
    geom_text_repel(
      data = Q, aes(x = .data[[axQ[1]]], y = .data[[axQ[2]]], label = lab),
      colour = col_var_text, family = font_family, size = lab_size_var,
      max.overlaps = Inf, segment.color = col_arrow, segment.size = arrow_lwd
    ) +
    { if (has_age_gender)
      list(
        geom_point(data = C_plot, aes(x = x, y = y, colour = pal["Orange"]),
                   shape = 15, size = sq_size_cat),
        geom_text_repel(data = C_plot, aes(x = x, y = y, label = lab, colour = pal["Orange"]),
                        family = font_family, size = lab_size_cat, max.overlaps = Inf)
      ) else NULL } +
    geom_hline(yintercept = 0, colour = col_axes, linewidth = axes_lwd) +
    geom_vline(xintercept = 0, colour = col_axes, linewidth = axes_lwd) +
    coord_equal(
      xlim   = c(-1.35, 1.10),
      ylim   = c(-1.10, 1.10),
      expand = FALSE
    ) +
    labs(x = xlab, y = ylab) +
    theme_minimal(base_family = font_family) +
    theme(text = element_text(family = font_family),
          axis.text  = element_text(size = axis_text_sz),
          axis.title = element_text(size = axis_title_sz), colour = col_var_text,
          legend.position = "none")
  
  # ───────────────── QUAL: categories only, shown as squares ─────────────────
} else if (plot_what == "qual") {
  
  A <- pick(res_mfa_mixed$quali.var, "coord");     if (!is.null(A)) A$status <- "active"
  S <- pick(res_mfa_mixed$quali.var.sup, "coord"); if (!is.null(S)) S$status <- "supplementary"
  QL <- do.call(rbind, Filter(Negate(is.null), list(A, S)))
  stopifnot(!is.null(QL), nrow(QL) > 0)
  ax <- axis_cols(QL, dims)
  QL$lab <- rownames(QL)
  
  qual_1and2<-ggplot(QL, aes(x = .data[[ax[1]]], y = .data[[ax[2]]])) +
    geom_point(data = subset(QL, status == "active"),
               shape = 15, colour = col_var_text, size = sq_size_cat, alpha = sq_alpha) +
    geom_point(data = subset(QL, status == "supplementary"),
               shape = 15, colour = pal["Orange"],    size = sq_size_cat) +
    geom_hline(yintercept = 0, colour = col_axes, linewidth = axes_lwd) +
    geom_vline(xintercept = 0, colour = col_axes, linewidth = axes_lwd) +
    geom_text_repel(data = subset(QL, status == "active"),
                    aes(label = lab), colour = col_var_text,
                    family = font_family, size = lab_size_cat, max.overlaps = Inf, segment.size = 0.15) +
    geom_text_repel(data = subset(QL, status == "supplementary"),
                    aes(label = lab), colour = pal["Orange"],
                    family = font_family, size = lab_size_cat, max.overlaps = Inf, segment.size = 0.15) +
    coord_equal() +
    labs(x = xlab, y = ylab) +
    theme_minimal(base_family = font_family) +
    theme(text = element_text(family = font_family),
          axis.text  = element_text(size = axis_text_sz),
          axis.title = element_text(size = axis_title_sz), colour = col_var_text,
          legend.position = "none")
  
} else stop("plot_what must be 'quant' or 'qual'")




#####Plotting just groups

# axes to show
dims   <- c(1, 2)
axcols <- paste0("Dim.", dims)


g_act <- as.data.frame(res_mfa_mixed$group$coord[, axcols, drop = FALSE])
g_act$set <- "active"
g_act$lab <- rownames(g_act)

g_sup <- NULL
if (!is.null(res_mfa_mixed$group$coord.sup)) {
  g_sup <- as.data.frame(res_mfa_mixed$group$coord.sup[, axcols, drop = FALSE])
  g_sup$set <- "supplementary"
  g_sup$lab <- rownames(g_sup)
}

g <- rbind(g_act, g_sup)

xlab <- sprintf("Dimension %d (%.1f%%)", dims[1], res_mfa_mixed$eig[dims[1], 2])
ylab <- sprintf("Dimension %d (%.1f%%)", dims[2], res_mfa_mixed$eig[dims[2], 2])


groups_1and2 <- ggplot(g, aes(x = .data[[axcols[1]]], y = .data[[axcols[2]]])) +
  geom_point(data = subset(g, set == "active"),        colour = "#3D3D3D") +
  geom_point(data = subset(g, set == "supplementary"), colour = "#F46A25") +
  ggrepel::geom_text_repel(
    data = subset(g, set == "active"),
    aes(label = lab), colour = "#3D3D3D",
    family = "LMRoman", size = lab_size_var,  # 9 pt labels
    max.overlaps = Inf
  ) +
  ggrepel::geom_text_repel(
    data = subset(g, set == "supplementary"),
    aes(label = lab), colour = "#F46A25",
    family = "LMRoman", size = lab_size_var,  # 9 pt labels
    max.overlaps = Inf
  ) +
  geom_hline(yintercept = 0, colour = "gray", linewidth = 0.5) +
  geom_vline(xintercept = 0, colour = "gray", linewidth = 0.5) +
  # Trim padding so the panel fills more space
  scale_x_continuous(expand = expansion(mult = 0.01)) +
  scale_y_continuous(expand = expansion(mult = 0.01)) +
  coord_equal() +
  labs(x = xlab, y = ylab) +
  theme_minimal(base_family = "LMRoman") +
  theme(
    text = element_text(family = "LMRoman"),
    axis.text  = element_text(size = axis_text_sz),
    axis.title = element_text(size = axis_title_sz), colour = col_var_text,
    plot.margin = margin(3, 3, 3, 3, "mm"),
    legend.position = "none"
  )

#Plotting individuals with age_gender categories

dims  <- c(1, 2)    
level <- 0.95       
means <- TRUE      


stopifnot(length(dims) == 2, all(dims %in% seq_len(nrow(res_mfa_mixed$eig))))
xlab <- sprintf("Dimension %d (%.1f%%)", dims[1], res_mfa_mixed$eig[dims[1], 2])
ylab <- sprintf("Dimension %d (%.1f%%)", dims[2], res_mfa_mixed$eig[dims[2], 2])

cats <- levels(factor(intersectional_df$age_gender))
stopifnot(length(cats) == 4)
cat_cols <- setNames(c("#12436D", "#28A197", "#801650", "#F46A25"), cats)


indc <- as.data.frame(res_mfa_mixed$ind$coord)                  
indc$age_gender <- factor(intersectional_df[rownames(indc), "age_gender"],
                          levels = cats)

aux <- cbind.data.frame(age_gender = indc$age_gender, indc)

ell <- FactoMineR::coord.ellipse(
  coord.simul = aux,
  bary       = means,     
  level.conf = level,
  axes       = dims,
  npoint     = 200
)


resolve_dim_col <- function(df, dim_idx) {
  cands <- c(paste0("Dim ", dim_idx), paste0("Dim.", dim_idx), paste0("Dim", dim_idx))
  hit <- intersect(cands, names(df))
  if (!length(hit)) stop("Couldn't find column for dimension ", dim_idx,
                         ". Available names: ", paste(names(df), collapse=", "))
  hit[1]
}

faccol <- names(ell$res)[1]
x_ell  <- names(ell$res)[2]
y_ell  <- names(ell$res)[3]

x_ind  <- resolve_dim_col(indc, dims[1])
y_ind  <- resolve_dim_col(indc, dims[2])

ell$res[[faccol]] <- factor(ell$res[[faccol]], levels = levels(indc$age_gender))

ind_1and2<- ggplot() +
  geom_point(
    data = indc,
    aes(x = .data[[x_ind]], y = .data[[y_ind]], colour = age_gender),
    size = 0.5
  ) +
  geom_path(
    data = ell$res,
    aes(x = .data[[x_ell]], y = .data[[y_ell]],
        colour = .data[[faccol]], group = .data[[faccol]]),
    linewidth = 0.8
  ) +
  geom_hline(yintercept = 0, colour = "gray", linewidth = 0.5) +
  geom_vline(xintercept = 0, colour = "gray", linewidth = 0.5) +
  scale_x_continuous(expand = expansion(mult = 0.01)) +
  scale_y_continuous(expand = expansion(mult = 0.01)) +
  coord_equal() +
  scale_colour_manual(values = cat_cols, name = NULL) +
  labs(x = xlab, y= ylab) +
  theme_minimal(base_family = "LMRoman") +
  theme(
    text = element_text(family = "LMRoman"),
    axis.text = element_text(size = axis_text_sz),
    axis.title = element_text(size = axis_title_sz), colour = col_var_text,
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.direction = "horizontal",
    legend.box.spacing = unit(1, "pt"),
    legend.box.margin  = margin(2, 0, 0, 0),
    legend.margin      = margin(2, 0, 0, 0),    
    legend.key.width   = unit(10, "pt"),
    legend.key.height  = unit(8, "pt"),
    legend.spacing.x   = unit(2, "pt"),
    legend.spacing.y   = unit(0, "pt"),
    legend.text        = element_text(size = axis_text_sz),
    plot.margin = margin(1, 1, 1, 1, "mm")
  ) +
  guides(colour = guide_legend(nrow = 1), size = "none")

###########################################################
######### Showng partial analysis for individuals #########
###########################################################


dims    <- c(1, 2)            
axnames <- paste0("Dim.", dims)


ind_res <- get_mfa_ind(res_mfa_mixed)               
G       <- as.data.frame(ind_res$coord[, axnames, drop = FALSE])
G$ind   <- rownames(ind_res$coord)

P <- ind_res$coord.partiel  


if (!is.null(dim(P)) && length(dim(P)) == 3) {
  P2   <- P[, dims, , drop = FALSE]                    
  Gmat <- as.matrix(G[, axnames, drop = FALSE])          

  d_mat <- sapply(seq_len(dim(P2)[3]), function(g) {
    Dg <- P2[, , g, drop = FALSE]                       
    sqrt(rowSums((Dg[, , 1] - Gmat)^2))
  })
  d_star <- rowMeans(d_mat, na.rm = TRUE)
  df_star <- data.frame(ind = rownames(ind_res$coord),
                        star_mean = d_star, row.names = NULL)
  
} else if (is.matrix(P) || is.data.frame(P)) {
  part_df <- as.data.frame(P[, axnames, drop = FALSE])
  rn <- rownames(P)
  part_df$ind   <- sub("\\.[^.]*$", "", rn)         
  part_df$group <- sub("^.*\\.",   "", rn)
  
  Gdf <- G                                            
  Gdf <- Gdf[, c("ind", axnames)]
  part_df <- part_df |>
    left_join(Gdf, by = "ind", suffix = c("", ".G")) |>
    mutate(dist = sqrt(rowSums((as.matrix(across(all_of(axnames))) -
                                  as.matrix(across(all_of(paste0(axnames, ".G")))))^2)))
  
  df_star <- part_df |>
    group_by(ind) |>
    summarise(star_mean = mean(dist, na.rm = TRUE), .groups = "drop")
  
} else {
  stop("Unsupported coord.partiel format; run str(get_mfa_ind(res_mfa_mixed)$coord.partiel).")
}

id_order <- rownames(ind_res$coord)
col_ag   <- if ("age_gender" %in% colnames(res_mfa_mixed$call$X)) "age_gender" else "Age_gender"
age_gender_vec <- res_mfa_mixed$call$X[id_order, col_ag, drop = TRUE]

df_star <- df_star |>
  mutate(ind = factor(ind, levels = id_order),
         age_gender = factor(age_gender_vec))


star_summary <- df_star |>
  group_by(age_gender) |>
  summarise(n = n(),
            mean_star   = mean(star_mean, na.rm = TRUE),
            median_star = median(star_mean, na.rm = TRUE),
            sd_star     = sd(star_mean, na.rm = TRUE),
            .groups = "drop")
print(star_summary)

ggplot(df_star, aes(x = age_gender, y = star_mean)) +
  geom_boxplot(outlier.alpha = 0.3, width = 0.7, colour = "#3D3D3D", fill = "grey85") +
  geom_point(position = position_jitter(width = 0.1, height = 0),
             size = 0.6, alpha = 0.5, colour = "#3D3D3D") +
  labs(x = "age_gender group",
       y = sprintf("Mean distance of partials to global point (Dims %s)", paste(dims, collapse = "–"))) +
  theme_minimal(base_family = "LMRoman") +
  theme(text = element_text(family = "LMRoman"))


################ With distinction to blocks ##################

dims    <- c(1, 2)
axnames <- paste0("Dim.", dims)

ind_res <- get_mfa_ind(res_mfa_mixed)                   
G       <- as.data.frame(ind_res$coord[, axnames, drop = FALSE])
G$ind   <- rownames(ind_res$coord)

P <- ind_res$coord.partiel  


if (!is.null(dim(P)) && length(dim(P)) == 3) {
  id_order    <- dimnames(P)[[1]]
  group_names <- dimnames(P)[[3]]
  stopifnot(!is.null(id_order), !is.null(group_names))
  G2 <- G[match(id_order, G$ind), , drop = FALSE]
  Gmat <- as.matrix(G2[, axnames, drop = FALSE])  
  dist_list <- lapply(seq_along(group_names), function(g) {
    Dg <- P[, dims, g, drop = FALSE]                    
    d  <- sqrt(rowSums((Dg[, , 1] - Gmat)^2))
    data.frame(ind = id_order, block = group_names[g], dist = d, row.names = NULL)
  })
  d_block <- bind_rows(dist_list)
  
} else if (is.matrix(P) || is.data.frame(P)) {
  part_df <- as.data.frame(P[, axnames, drop = FALSE])
  rn <- rownames(P)
  part_df$ind   <- sub("\\.[^.]*$", "", rn)
  part_df$block <- sub("^.*\\.",   "", rn)
  G2 <- G[, c("ind", axnames)]
  d_block <- part_df |>
    left_join(G2, by = "ind", suffix = c("", ".G")) |>
    mutate(dist = sqrt(rowSums((as.matrix(across(all_of(axnames))) -
                                  as.matrix(across(all_of(paste0(axnames, ".G")))))^2))) |>
    select(ind, block, dist)
  
} else {
  stop("Unsupported coord.partiel format; run str(get_mfa_ind(res_mfa_mixed)$coord.partiel).")
}

id_order <- rownames(ind_res$coord)
col_ag   <- if ("age_gender" %in% colnames(res_mfa_mixed$call$X)) "age_gender" else "Age_gender"
age_gender_vec <- res_mfa_mixed$call$X[id_order, col_ag, drop = TRUE]

d_block <- d_block |>
  mutate(ind = factor(ind, levels = id_order)) |>
  left_join(
    data.frame(ind = id_order, age_gender = factor(age_gender_vec), row.names = NULL),
    by = "ind"
  )

star_by_block <- d_block |>
  group_by(age_gender, block) |>
  summarise(n = n(),
            mean_dist   = mean(dist, na.rm = TRUE),
            median_dist = median(dist, na.rm = TRUE),
            .groups = "drop")
print(star_by_block)

ggplot(star_by_block, aes(x = age_gender, y = block, fill = mean_dist)) +
  geom_tile() +
  scale_fill_gradient(name = "Mean distance", low = "white", high = "#3D3D3D") +
  labs(x = "age_gender", y = "Block",
       title = sprintf("Block-wise partial distance to global point (Dims %s)", paste(dims, collapse = "–"))) +
  theme_minimal(base_family = "LMRoman") +
  theme(text = element_text(family = "LMRoman"),
        axis.text.x = element_text(angle = 30, hjust = 1))

ggplot(d_block, aes(x = age_gender, y = dist)) +
  geom_boxplot(outlier.alpha = 0.3, width = 0.7, colour = "#3D3D3D", fill = "grey85") +
  labs(x = "age_gender", y = "Distance (partial ↔ global)",
       title = sprintf("Distribution of per-block partial distances (Dims %s)", paste(dims, collapse = "–"))) +
  facet_wrap(~ block, scales = "free_y") +
  theme_minimal(base_family = "LMRoman") +
  theme(text = element_text(family = "LMRoman"),
        axis.text.x = element_text(angle = 30, hjust = 1))

dimdesc(res_mfa_mixed)



#Exporting plots to Latex

options(tikzMetricsDictionary = "tikz_metrics.cache")
options(tikzDefaultEngine = "pdftex")


options(tikzLwdUnit = 72.27/96)

export_tikz <- function(plot, layout = c("landscape","single","two","grid4_landscape"),
                        basename = "figure", base_size_pt = 12) {
  old <- theme_set(theme_get() + theme(legend.position = "bottom"))
  on.exit(theme_set(old), add = TRUE)
  
  layout <- match.arg(layout)
  dims <- switch(layout,
                 "landscape" = c(24, 16),
                 "single"        = c(16, 10.5),
                 "two"          = c(7.6, 6.0),
                 "grid4_landscape"  = c(11.6, 7.7 ))
  w <- dims[1]; h <- dims[2]
  
  pdffile <- sprintf("%s.pdf", basename)
  tikzfile <- sub("\\.pdf$", ".tex", pdffile)
  dir.create(dirname(pdffile), showWarnings = FALSE, recursive = TRUE)
  
  tikz(tikzfile, width = w/2.54, height = h/2.54, standAlone = FALSE, sanitize = TRUE)
  print(plot); dev.off()

  ggsave(pdffile, plot, width = w, height = h, units = "cm", device = cairo_pdf)
  
  message(sprintf("Wrote: %s and %s at %.1f×%.1f cm", tikzfile, pdffile, w, h))
}




export_tikz(ind_1and2, layout = "single", basename= "ind_1and2")
export_tikz(groups_1and2, layout = "single", basename= "groups_1and2")
export_tikz(qual_1and2, layout = "single", basename= "qual_1and2")
export_tikz(quant_1and2, layout = "single", basename= "quant_1and2")





#########################################################################################################
#########################################################################################################
#################### MFA models separately for each intersectional group ################################
#########################################################################################################
#########################################################################################################


##########################################
# 1 - Variable groups
##########################################
orig_blocks <- list(
  c("K03","K1601","K1602","K14"),                         #1 mixed "Politics & Language",
  "K05",                                                  #2 nominal "Housing",
  c("K1001","K1002","K09"),                               #3 mixed "Poverty",
  c("K1101","K1102","K1103","K1104","K1105","K23","K25"), #4 ordinal  "Satisfaction & Health",
  paste0("K180",1:8),                                     #5 binary "Social participation",
  c("K1901","K1902","K1904"),                             #6 ordinal "Culture",
  paste0("K200",1:7),                                     #7 ordinal "Discrimination",
  paste0("K300",1:8),                                     #8 ordinal "Depression",
  paste0("K380",1:3),                                     #9 nominal "Sufficient healthcare",
  paste0("K390",1:6),                                     #10 binary "Healthcare limits",
  paste0("K430",1:6),                                     #11 binary "Social services",
  paste0("K480",1:7),                                     #12 continuous "Relations",
  c("K58", setdiff(paste0("K590",1:9), "K5903"),          #13 mixed "Internet"
    "K5910","K5911"), 
  c("K69_aggreg"),                                        #14 nominal "Economic activity"
  c("K66_aggreg_num")                                     #15 ordinal "Education"
)


##########################################
#Separating categorical from numeric
##########################################
num_vars <- c(
  "K09",paste0("K110",1:5),"K23","K25","K14",
  "K1901","K1902","K1904",paste0("K200",1:7),
  paste0("K300",1:8),paste0("K480",1:7),
  "K58", "K66_aggreg_num"
)
cat_vars <- c(
  "K03","K05","K1001","K1002", "K1601","K1602",
  paste0("K180",1:8),paste0("K380",1:3),
  paste0("K390",1:6),paste0("K430",1:6),
  "K5901", "K5902", paste0("K590",4:9),"K5910","K5911",
  # "K63","K65", 
  "K69_aggreg"
)


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

data.table::setDF(female_younger65)
#coercing once:

female_younger65[ intersect(cat_vars, names(female_younger65)) ] <-
  lapply(female_younger65[ intersect(cat_vars, names(female_younger65)) ],
         function(x) factor(x, exclude = NULL))

female_younger65[ intersect(num_vars, names(female_younger65)) ] <-
  lapply(female_younger65[ intersect(num_vars, names(female_younger65)) ],
         safe_as_numeric)

data.table::setDF(male_younger65)
#coercing once:

male_younger65[ intersect(cat_vars, names(male_younger65)) ] <-
  lapply(male_younger65[ intersect(cat_vars, names(male_younger65)) ],
         function(x) factor(x, exclude = NULL))

male_younger65[ intersect(num_vars, names(male_younger65)) ] <-
  lapply(male_younger65[ intersect(num_vars, names(male_younger65)) ],
         safe_as_numeric)

data.table::setDF(female_65plus)
#coercing once:

female_65plus[ intersect(cat_vars, names(female_65plus)) ] <-
  lapply(female_65plus[ intersect(cat_vars, names(female_65plus)) ],
         function(x) factor(x, exclude = NULL))

female_65plus[ intersect(num_vars, names(female_65plus)) ] <-
  lapply(female_65plus[ intersect(num_vars, names(female_65plus)) ],
         safe_as_numeric)

data.table::setDF(male_65plus)
#coercing once:

male_65plus[ intersect(cat_vars, names(male_65plus)) ] <-
  lapply(male_65plus[ intersect(cat_vars, names(male_65plus)) ],
         function(x) factor(x, exclude = NULL))

male_65plus[ intersect(num_vars, names(male_65plus)) ] <-
  lapply(male_65plus[ intersect(num_vars, names(male_65plus)) ],
         safe_as_numeric)


##########################################
# 3 - Building variable groups
##########################################


#pure numeric blocks stay as 1 "s" group
#pure categorical blocks stay as 1 "n" group
#mixed blocks become TWO groups: one "s" (nums), one "n" (factors)
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
    blocks_mixed[[length(blocks_mixed)+1]] <- nums
    types_mixed   <- c(types_mixed, "s")
    names_mixed   <- c(names_mixed, paste0(blk_name," (quant)"))
    
    blocks_mixed[[length(blocks_mixed)+1]] <- cats
    types_mixed   <- c(types_mixed, "n")
    names_mixed   <- c(names_mixed, paste0(blk_name," (qual)"))
  } else {
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

##########################################
# Running MFA
##########################################


mfa_female_younger65 <- MFA(
  base           = female_younger65[, c(active_vars)],
  group          = group_sizes,
  type           = group_types,
  name.group     = name_groups,
  row.w          = female_younger65$useweight,
  graph          = FALSE,
  ncp            = 15
)

mfa_male_younger65 <- MFA(
  base           = male_younger65[, c(active_vars)],
  group          = group_sizes,
  type           = group_types,
  name.group     = name_groups,
  row.w          = male_younger65$useweight,
  graph          = FALSE,
  ncp            = 15
)

mfa_female_65plus <- MFA(
  base           = female_65plus[, c(active_vars)],
  group          = group_sizes,
  type           = group_types,
  name.group     = name_groups,
  row.w          = female_65plus$useweight,
  graph          = FALSE,
  ncp            = 15
)

mfa_male_65plus <- MFA(
  base           = male_65plus[, c(active_vars)],
  group          = group_sizes,
  type           = group_types,
  name.group     = name_groups,
  row.w          = male_65plus$useweight,
  graph          = FALSE,
  ncp            = 15
)





########################## PLOTS #############################

make_mfa_plot_rect_same_style <- function(mfa,
                                          xlim = c(0, 0.7), ylim = c(0, 0.5),
                                          lab_size_var, axis_text_sz, axis_title_sz,
                                          col_var_text = "black") {
  dims   <- c(1, 2)
  ax     <- paste0("Dim.", dims)
  
  g_act <- as.data.frame(mfa$group$coord[, ax, drop = FALSE]); g_act$set <- "active";        g_act$lab <- rownames(g_act)
  g <- g_act
  
  xlab <- sprintf("Dimension %d (%.1f%%)", dims[1], mfa$eig[dims[1], 2])
  ylab <- sprintf("Dimension %d (%.1f%%)", dims[2], mfa$eig[dims[2], 2])
  
  ggplot(g, aes(x = .data[[ax[1]]], y = .data[[ax[2]]])) +
    geom_point(data = subset(g, set == "active"),        colour = "#3D3D3D", size=0.75) +
    ggrepel::geom_text_repel(
      data = subset(g, set == "active"),
      aes(label = lab), colour = "#3D3D3D",
      family = "LMRoman", size = 2.15, max.overlaps = Inf
    ) +
    geom_hline(yintercept = 0, colour = "gray", linewidth = 0.5) +
    geom_vline(xintercept = 0, colour = "gray", linewidth = 0.5) +
    scale_x_continuous(breaks = seq(0, xlim[2], by = 0.1), expand = expansion(add = c(0, 0))) +
    scale_y_continuous(breaks = seq(0, ylim[2], by = 0.1), expand = expansion(add = c(0, 0))) +
    coord_cartesian(xlim = xlim, ylim = ylim, expand = FALSE, clip = "off") +
    labs(x = xlab, y = ylab) +
    theme_minimal(base_family = "LMRoman") +
    theme(
      text       = element_text(family = "LMRoman"),
      axis.text  = element_text(size = axis_text_sz),
      axis.title = element_text(size = axis_title_sz, colour = col_var_text),
      plot.margin = margin(3, 3, 3, 3, "mm"),
      legend.position = "none"
    )
}

p_f_uy65 <- make_mfa_plot_rect_same_style(mfa_female_younger65, axis_text_sz = axis_text_sz, axis_title_sz = axis_title_sz, col_var_text = col_var_text)
p_f_65p  <- make_mfa_plot_rect_same_style(mfa_female_65plus,    axis_text_sz = axis_text_sz, axis_title_sz = axis_title_sz, col_var_text = col_var_text)
p_m_uy65 <- make_mfa_plot_rect_same_style(mfa_male_younger65,   axis_text_sz = axis_text_sz, axis_title_sz = axis_title_sz, col_var_text = col_var_text)
p_m_65p  <- make_mfa_plot_rect_same_style(mfa_male_65plus,      axis_text_sz = axis_text_sz, axis_title_sz = axis_title_sz, col_var_text = col_var_text)

export_tikz(p_f_uy65, layout = "grid4_landscape", basename = "groups_1and2_female_younger65")
export_tikz(p_f_65p,  layout = "grid4_landscape", basename = "groups_1and2_female_65plus")
export_tikz(p_m_uy65, layout = "grid4_landscape", basename = "groups_1and2_male_younger65")
export_tikz(p_m_65p,  layout = "grid4_landscape", basename = "groups_1and2_male_65plus")


options(scipen = 999)  

for (i in 1:2){
  print(paste("Dimension ", i))
  tbl3 <- mfa_group_axis_table(res_mfa_mixed, axis = i, sort_by = "contrib")
  print(tbl3)
}

for (i in 2){
  print(paste("Dimension ", i))
  tbl3 <- mfa_group_axis_table(mfa_female_younger65, axis = i, sort_by = "contrib")
  print(tbl3)
}

for (i in 1){
  print(paste("Dimension ", i))
  tbl3 <- mfa_group_axis_table(mfa_female_65plus, axis = i, sort_by = "contrib")
  print(tbl3)
}

for (i in 2){
  print(paste("Dimension ", i))
  tbl3 <- mfa_group_axis_table(mfa_male_younger65, axis = i, sort_by = "contrib")
  print(tbl3)
}

for (i in 2){
  print(paste("Dimension ", i))
  tbl3 <- mfa_group_axis_table(mfa_male_65plus, axis = i, sort_by = "contrib")
  print(tbl3)
}





#### Calculating tables for the thesis (both for overall MFA model and 4 intersectionally specific MFA models) ###########


#########################################################################################################
# Summary for overall model, ANOVA results



make_dim12_exact <- function(res, var = "age_gender",
                             levels_exact = c("female <65","female 65+","male <65","male 65+")) {
 
  dd <- dimdesc(res, axes = 1:2, proba = 1)
  get_R2p <- function(k) {
    q <- dd[[k]]$quali
    if (!is.null(q) && var %in% rownames(q)) {
      c(R2 = as.numeric(q[var, "R2"]), p = as.numeric(q[var, "p.value"]))
    } else c(R2 = NA_real_, p = NA_real_)
  }
  d1 <- get_R2p(1)
  d2 <- get_R2p(2)
  

  x <- res$call$X[, var, drop = TRUE]; if (!is.factor(x)) x <- factor(x, exclude = NULL)
  w <- if (!is.null(res$call$row.w.init)) res$call$row.w.init else res$call$row.w
  if (is.null(w)) w <- rep(1, nrow(res$ind$coord))
  
  fetch_cats <- function(axis_index) {
    tab <- data.frame(axis = res$ind$coord[, axis_index, drop = TRUE], x = x)
    names(tab)[2] <- var
    cd <- condes(tab, num.var = 1, weights = w, proba = 1)  
    ct <- cd$category
    if (is.null(ct) || !nrow(ct)) stop("condes() returned no category table; check inputs.")
  
    keys <- paste0(var, "=", levels_exact)
    if (!all(keys %in% rownames(ct))) {
      missing <- keys[!keys %in% rownames(ct)]
      stop("These category keys were not found in condes(): ", paste(missing, collapse = ", "),
           "\nAvailable keys: ", paste(rownames(ct)[grepl(paste0("^", var, "="), rownames(ct))], collapse = " | "))
    }
    out <- as.matrix(ct[keys, c("Estimate","p.value"), drop = FALSE])
    rownames(out) <- levels_exact
    out
  }
  cats1 <- fetch_cats(1) 
  cats2 <- fetch_cats(2) 
  
  row_labels <- c(
    "overall MFA",
    "Female <65",
    "Female 65+",
    "Male <65",
    "Male 65+"
  )
  order_idx <- match(c("female <65","female 65+","male <65","male 65+"), rownames(cats1))
  
  out <- data.frame(
    Row = row_labels,
    `Dim1.Metric`   = c("R2", rep("Estimate", 4)),
    `Dim1.Value`    = c(d1["R2"], cats1[order_idx, 1]),
    `Dim1.p.value`  = c(d1["p"],  cats1[order_idx, 2]),
    `Dim2.Metric`   = c("R2", rep("Estimate", 4)),
    `Dim2.Value`    = c(d2["R2"], cats2[order_idx, 1]),
    `Dim2.p.value`  = c(d2["p"],  cats2[order_idx, 2]),
    check.names = FALSE
  )
  rownames(out) <- NULL
  out
}


tbl <- make_dim12_exact(res_mfa_mixed, var = "age_gender")

write.csv(tbl, "age_gender_dim12_ANOVA.csv", row.names = FALSE)





########################################################################################
# Summary of age_gender in the overal model: coord, cos2, v.test

build_quali_2col <- function(res, id_lists, supp_var = "age_gender", dims = 1:2) {
  q <- res$quali.var.sup
  if (is.null(q)) stop("res$quali.var.sup is NULL; ensure the variable is supplementary qualitative.")
  
  rn <- rownames(q$coord)
  pick_idx <- function(label) {
    w <- which(rn %in% c(label, paste0(supp_var, "=", label)))
    if (!length(w)) stop("Category not found in quali.var.sup: ", label)
    w[1]
  }
  
  fmt <- function(z) {
    z <- suppressWarnings(as.numeric(z)[1])
    if (is.na(z)) "NA" else formatC(z, digits = 4, format = "f")
  }
  
  cats <- names(id_lists)
  out <- matrix(NA_character_, nrow = length(cats), ncol = length(dims),
                dimnames = list(paste0(supp_var, "=", cats), paste0("Dim ", dims)))
  
  for (k in seq_along(cats)) {
    i <- pick_idx(cats[k])
    for (j in seq_along(dims)) {
      d <- dims[j]
      out[k, j] <- paste0(
        "coord=",  fmt(q$coord[i,  d, drop = TRUE]), "; ",
        "cos2=",   fmt(q$cos2[i,   d, drop = TRUE]), "; ",
        "v.test=", fmt(q$v.test[i, d, drop = TRUE])
      )
    }
  }
  as.data.frame(out, stringsAsFactors = FALSE)
}


tab <- build_quali_2col(res_mfa_mixed, id_lists, supp_var = "age_gender", dims = 1:2)
print(tab)
write.csv(tab, "mfa_age_gender_quali_dim1-2.csv")


########################################################################################
# Summarizing eigenvalues and explained variance across 5 models


get_mfa_stats <- function(res, dims = 1:10) {
  E <- as.data.frame(res$eig)  
  if (nrow(E) < max(dims)) {
    pad <- matrix(NA_real_, nrow = max(dims) - nrow(E), ncol = ncol(E))
    colnames(pad) <- colnames(E)
    E <- rbind(E, as.data.frame(pad))
  }
  cols <- tolower(colnames(E))
  eig_col <- if ("eigenvalue" %in% cols) which(cols == "eigenvalue")[1] else 1
  pct_col <- if (any(grepl("percent", cols))) which(grepl("percent", cols))[1] else min(2, ncol(E))
  cum_col <- if (any(grepl("cum", cols))) which(grepl("cum", cols))[1] else NA_integer_
  
  eig_full <- as.numeric(E[, eig_col])
  pct_full <- as.numeric(E[, pct_col])
  cum_full <- if (!is.na(cum_col)) as.numeric(E[, cum_col]) else cumsum(pct_full)
  
  data.frame(
    eigenvalue      = eig_full[dims],
    cum_inertia_pct = cum_full[dims]
  )
}

models <- list(
  res_mfa_mixed        = res_mfa_mixed,
  mfa_female_younger65 = mfa_female_younger65,
  mfa_male_younger65   = mfa_male_younger65,
  mfa_female_65plus    = mfa_female_65plus,
  mfa_male_65plus      = mfa_male_65plus
)

dims <- 1:10
tbl  <- data.frame(Dimension = paste0("Dim", dims), check.names = FALSE)

for (nm in names(models)) {
  s <- get_mfa_stats(models[[nm]], dims = dims)
  tbl[[paste0(nm, " | Eigenvalue")]]             <- round(s$eigenvalue, 4)
  tbl[[paste0(nm, " | Cumulative inertia (%)")]] <- round(s$cum_inertia_pct, 2)
}


file_out <- "MFA_eigenvalues_cuminertia_Dim1-10.xlsx"
wb <- createWorkbook()
addWorksheet(wb, "Eig+Cum")


sub_headers <- c("Dimension",
                 rep(c("Eigenvalue", "Cumulative inertia (%)"), length(models)))

colnames(tbl) <- sub_headers


writeData(wb, sheet = "Eig+Cum", x = tbl, startRow = 2, startCol = 1, rowNames = FALSE)


hs_top <- createStyle(textDecoration = "bold", halign = "center", valign = "center")
writeData(wb, "Eig+Cum", x = "", startRow = 1, startCol = 1, colNames = FALSE)

for (i in seq_along(models)) {
  start_col <- 2 + 2 * (i - 1)               
  end_col   <- start_col + 1                
  writeData(wb, "Eig+Cum", x = names(models)[i],
            startRow = 1, startCol = start_col, colNames = FALSE)
  mergeCells(wb, "Eig+Cum", rows = 1, cols = start_col:end_col)
  addStyle(wb, "Eig+Cum", style = hs_top, rows = 1, cols = start_col:end_col, gridExpand = TRUE)
}


hs_sub <- createStyle(textDecoration = "bold", halign = "center")
addStyle(wb, "Eig+Cum", style = hs_sub, rows = 2, cols = 1:ncol(tbl), gridExpand = TRUE)


setColWidths(wb, "Eig+Cum", cols = 1, widths = 12)                  
setColWidths(wb, "Eig+Cum", cols = 2:ncol(tbl), widths = 18)


freezePane(wb, "Eig+Cum", firstActiveRow = 3, firstActiveCol = 2)


saveWorkbook(wb, file_out, overwrite = TRUE)



##############################################################################################################################################################################
# RV and LG analysis per group with age_gender


age_gender_table <- function(res, age_name = "Age_gender", digits = 3) {
  rv <- res$group$RV
  lg <- res$group$Lg
  if (is.null(rv) || is.null(lg)) {
    stop("Could not find res$group$RV or res$group$Lg in the MFA result.")
  }
  
  gnames <- rownames(rv)
  if (is.null(gnames)) gnames <- colnames(rv)
  
  age_idx <- match(age_name, gnames)
  if (is.na(age_idx)) age_idx <- match(tolower(age_name), tolower(gnames))
  if (is.na(age_idx)) {
    cand <- grep("age[_\\. ]?gender", gnames, ignore.case = TRUE)
    if (length(cand) == 1) age_idx <- cand
  }
  if (is.na(age_idx)) {
    stop(sprintf("Could not find a group named '%s'. Available groups: %s",
                 age_name, paste(gnames, collapse = ", ")))
  }
  
 
  sup_idx <- res$call$num.group.sup
  if (is.null(sup_idx)) sup_idx <- integer(0)
  all_idx <- seq_len(nrow(rv))
  act_idx <- setdiff(all_idx, union(sup_idx, age_idx))
  
 
  rv_vals <- rv[act_idx, age_idx, drop = TRUE]
  lg_vals <- lg[act_idx, age_idx, drop = TRUE]
  

  out <- data.frame(
    Block = gnames[act_idx],
    RV    = round(as.numeric(rv_vals), digits),
    LG    = round(as.numeric(lg_vals), digits),
    check.names = FALSE, row.names = NULL
  )
  
  out
}

age_tbl <- age_gender_table(res_mfa_mixed, age_name = "Age_gender", digits = 3)
age_tbl

write.xlsx(age_tbl, file = "Age_gender_RV_LG.xlsx", rowNames = FALSE)



##################################################################################################################################################################################################
# Comparing group structure across models


build_mfa_group_table <- function(models,
                                  outfile = "MFA_top_groups.xlsx",
                                  dim = 1,
                                  top_n = 18,
                                  use_abs = TRUE) {
  if (!requireNamespace("openxlsx", quietly = TRUE)) {
    stop("Package 'openxlsx' is required. Please install it with install.packages('openxlsx').")
  }
  
 
  desired_order <- c("res_mfa_mixed",
                     "mfa_female_younger65",
                     "mfa_female_65plus",
                     "mfa_male_younger65",
                     "mfa_male_65plus")
  
  pretty_names <- c(
    res_mfa_mixed        = "Overall MFA",
    mfa_female_younger65 = "MFA: Female <65",
    mfa_female_65plus    = "MFA: Female 65+",
    mfa_male_younger65   = "MFA: Male <65",
    mfa_male_65plus      = "MFA: Male 65+"
  )
  
 
  model_keys <- intersect(desired_order, names(models))
  if (length(model_keys) == 0L) stop("No matching models found in 'models' list.")
  
 
  top_groups_for_model <- function(mfa_obj, dim, top_n, use_abs) {
    if (is.null(mfa_obj$group)) stop("MFA object has no $group component.")
    grp <- mfa_obj$group
    
    get_dim_vec <- function(x, nm) {
      if (is.null(x)) stop(sprintf("$group$%s is NULL", nm))
      x <- as.matrix(x)
      if (ncol(x) < dim) stop(sprintf("$group$%s has only %d dimensions; requested dim=%d",
                                      nm, ncol(x), dim))
      v <- x[, dim]
      names(v) <- rownames(x)
      v
    }
    
    coord  <- get_dim_vec(grp$coord,  "coord")
    contrib<- get_dim_vec(grp$contrib,"contrib")
    cos2   <- get_dim_vec(grp$cos2,   "cos2")
    
   
    score <- if (use_abs) abs(coord) else coord
    ord   <- order(score, decreasing = TRUE, na.last = NA)
    keep  <- head(ord, top_n)
    
    fmt <- function(x) formatC(x, digits = 3, format = "f")
    groups  <- names(coord)[keep]
    metrics <- sprintf("coord = %s, contrib = %s, cos2 = %s",
                       fmt(coord[keep]), fmt(contrib[keep]), fmt(cos2[keep]))
    
    n_found <- length(groups)
    if (n_found < top_n) {
      groups  <- c(groups,  rep("", top_n - n_found))
      metrics <- c(metrics, rep("", top_n - n_found))
    }
    
    data.frame(Group = groups, Metrics = metrics, stringsAsFactors = FALSE)
  }
  

  top_n_rows <- top_n
  num_models <- length(model_keys)
  data_mat <- matrix("", nrow = top_n_rows, ncol = 2 * num_models)
  

  for (j in seq_along(model_keys)) {
    key <- model_keys[j]
    res <- top_groups_for_model(models[[key]], dim = dim, top_n = top_n, use_abs = use_abs)
    col_start <- (j - 1) * 2 + 1
    data_mat[, col_start]     <- res$Group
    data_mat[, col_start + 1] <- res$Metrics
  }
  

  header_row1 <- unlist(lapply(model_keys, function(k) c(pretty_names[[k]], "")))
  header_row2 <- rep(c("Group", "Metrics"), times = num_models)
  

  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "Top groups by coord")
  

  openxlsx::writeData(wb, sheet = 1, x = t(as.matrix(header_row1)),
                      startCol = 1, startRow = 1, colNames = FALSE, rowNames = FALSE)
  openxlsx::writeData(wb, sheet = 1, x = t(as.matrix(header_row2)),
                      startCol = 1, startRow = 2, colNames = FALSE, rowNames = FALSE)
  

  for (j in seq_len(num_models)) {
    c1 <- (j - 1) * 2 + 1
    c2 <- c1 + 1
    openxlsx::mergeCells(wb, sheet = 1, cols = c(c1, c2), rows = 1)
  }
  
  openxlsx::writeData(wb, sheet = 1, x = data_mat,
                      startCol = 1, startRow = 3, colNames = FALSE, rowNames = FALSE)
  

  h1 <- openxlsx::createStyle(textDecoration = "bold", halign = "center")
  h2 <- openxlsx::createStyle(textDecoration = "bold")
  openxlsx::addStyle(wb, sheet = 1, style = h1, rows = 1, cols = 1:(2 * num_models), gridExpand = TRUE)
  openxlsx::addStyle(wb, sheet = 1, style = h2, rows = 2, cols = 1:(2 * num_models), gridExpand = TRUE)
  openxlsx::setColWidths(wb, sheet = 1, cols = 1:(2 * num_models), widths = c(rbind(20, 60)))
  
  openxlsx::saveWorkbook(wb, outfile, overwrite = TRUE)
  
  colnames(data_mat) <- as.vector(rbind(
    paste0(pretty_names[model_keys], " - Group"),
    paste0(pretty_names[model_keys], " - Metrics")
  ))
  invisible(as.data.frame(data_mat, stringsAsFactors = FALSE))
}

# ---- Usage ----
models <- list(
  res_mfa_mixed        = res_mfa_mixed,
  mfa_female_younger65 = mfa_female_younger65,
  mfa_male_younger65   = mfa_male_younger65,
  mfa_female_65plus    = mfa_female_65plus,
  mfa_male_65plus      = mfa_male_65plus
)
#
build_mfa_group_table(models,
                      outfile = "MFA_groups_dim1.xlsx",
                      dim = 1,       
                      top_n = 18,    
                      use_abs = TRUE 
)



##################################################################################################################################################################################################
# Partial analysis in the overall MFA


build_supquali_tables <- function(
    res,
    categories = c("female <65","female 65+","male <65","male 65+"),
    axis = 1,
    file = "mfa_supquali_tables.xlsx",
    sheet_overall = "Overall",
    sheet_blocks  = "By Block",
    debug = FALSE
){
  if (!requireNamespace("openxlsx", quietly = TRUE)) {
    stop("Package 'openxlsx' is required. install.packages('openxlsx')")
  }
  qsup <- res$quali.var.sup
  need <- c("coord","coord.partiel","within.inertia","within.partial.inertia")
  miss <- need[!need %in% names(qsup)]
  if (length(miss)) stop("Missing in res$quali.var.sup: ", paste(miss, collapse=", "))
  
  # ---- helpers ----
  strip_varprefix <- function(x) sub("^.*?=", "", x)
  to_matrix_axis  <- function(obj, axis){
    if (is.array(obj) && length(dim(obj)) == 3L) {
      if (axis < 1 || axis > dim(obj)[2]) stop("axis out of range for 3D array.")
      out <- obj[, axis, , drop = FALSE]
      m <- matrix(out, nrow = dim(obj)[1])
      rn <- if (!is.null(dimnames(obj)[[1]])) dimnames(obj)[[1]] else NULL
      cn <- if (!is.null(dimnames(obj)[[3]])) dimnames(obj)[[3]] else NULL
      if (!is.null(rn)) rownames(m) <- rn
      if (!is.null(cn)) colnames(m) <- cn
      return(m)
    }
    if (is.list(obj)) {
      mats <- lapply(obj, function(M){
        M <- as.matrix(M)
        if (axis < 1 || axis > ncol(M)) stop("axis out of range for list element.")
        M[, axis, drop = TRUE]
      })
      m <- do.call(cbind, mats)
      if (!is.null(rownames(obj[[1]]))) rownames(m) <- rownames(obj[[1]])
      return(m)
    }
    if (is.matrix(obj)) {
      rn <- rownames(obj)
      if (is.null(rn)) stop("Matrix input requires rownames to separate categories/blocks.")
      split_lastdot <- function(s){
        pos <- regexpr("\\.[^.]*$", s)
        if (pos < 0) return(c(s, NA_character_))
        c(substr(s, 1, pos-1), substr(s, pos+1, nchar(s)))
      }
      sp <- t(vapply(rn, split_lastdot, c("", "")))
      cat_names   <- sp[,1]
      block_names <- sp[,2]
      if (anyNA(block_names)) stop("Cannot parse blocks from rownames of stacked matrix.")
      if (axis < 1 || axis > ncol(obj)) stop("axis out of range for matrix.")
      df <- data.frame(cat = cat_names, block = block_names, val = obj[, axis], check.names = FALSE)
      blocks <- unique(block_names)
      cats   <- unique(cat_names)
      m <- matrix(NA_real_, nrow = length(cats), ncol = length(blocks),
                  dimnames = list(cats, blocks))
      for (b in blocks) {
        m[cats, b] <- df$val[df$block == b][match(cats, df$cat[df$block == b])]
      }
      return(m)
    }
    stop("Unsupported structure (expected array/list/matrix).")
  }
  
  coord_all <- as.matrix(qsup$coord)
  if (axis < 1 || axis > ncol(coord_all)) stop("axis out of range for quali.var.sup$coord.")
  labels_all <- strip_varprefix(rownames(coord_all))
  idx <- match(categories, labels_all)
  if (anyNA(idx)) stop("Categories not found in quali.var.sup$coord: ",
                       paste(categories[is.na(idx)], collapse=", "))
  coord_cat <- coord_all[idx, axis, drop = TRUE]      

  pc_mat_all    <- to_matrix_axis(qsup$coord.partiel, axis)         
  within_pb_all <- to_matrix_axis(qsup$within.partial.inertia, axis) 
  rn_pc <- rownames(pc_mat_all)
  if (!is.null(rn_pc)) {
    rownames(pc_mat_all)    <- strip_varprefix(rn_pc)
    rownames(within_pb_all) <- strip_varprefix(rownames(within_pb_all))
  }
  if (is.null(rownames(pc_mat_all))) stop("Rownames required on coord.partiel to filter categories.")
  if (!all(categories %in% rownames(pc_mat_all)))
    stop("Some categories are missing in coord.partiel rows: ",
         paste(setdiff(categories, rownames(pc_mat_all)), collapse=", "))
  pc_mat    <- pc_mat_all[categories, , drop = FALSE]      
  within_pb <- within_pb_all[categories, , drop = FALSE]   
  J <- ncol(pc_mat)
  

  within_all <- as.matrix(qsup$within.inertia)
  if (axis < 1 || axis > ncol(within_all)) stop("axis out of range for quali.var.sup$within.inertia.")
  labels_within <- strip_varprefix(rownames(within_all))
  idx_w <- match(categories, labels_within)
  if (anyNA(idx_w)) stop("Categories not found in within.inertia: ",
                         paste(categories[is.na(idx_w)], collapse=", "))
  within_inertia <- as.numeric(within_all[idx_w, axis])   
  

  block_names <- tryCatch(rownames(res$group$coord), error=function(e) NULL)
  if (!is.null(block_names) && length(block_names) == J) {
    colnames(pc_mat)    <- block_names
    colnames(within_pb) <- block_names
  }
  

  delta_mat       <- sweep(pc_mat, 1, coord_cat, FUN = "-")  
  pc_summed_dist  <- rowSums(delta_mat^2, na.rm = TRUE)              
  within_pct_blk  <- sweep(within_pb, 1, within_inertia,
                           FUN = function(a,b) ifelse(is.finite(b) & b>0, 100*a/b, NA_real_))

  within_inertia_pct_overall <-
    100 * within_inertia / sum(within_inertia, na.rm = TRUE)
  
  if (debug) {
    message(sprintf("K=%d categories; J=%d blocks; axis=%d", length(categories), J, axis))
  }
  

  overall_row <- unlist(lapply(seq_along(categories), function(i){
    c(
      `Coord`                               = coord_cat[i],
      `Partial Coord (summed distances)`    = pc_summed_dist[i],
      `Within Inertia`                      = within_inertia[i],           
      `Within Inertia (%)`                  = within_inertia_pct_overall[i] 
    )
  }))
  overall_df <- data.frame(`Overall`="Overall model", t(overall_row),
                           check.names=FALSE, row.names=NULL)
  

  byblock_mat <- do.call(rbind, lapply(seq_len(J), function(j){
    unlist(lapply(seq_along(categories), function(i){
      c(
        `Partial Coord`          = pc_mat[i, j],
        `Coord Shift`            = delta_mat[i, j],
        `Partial Within Inertia` = within_pb[i, j],       
        `Within Inertia (%)`     = within_pct_blk[i, j]  
      )
    }))
  }))
  byblock_df <- data.frame(`Active variable groups` = colnames(pc_mat), byblock_mat,
                           check.names=FALSE, row.names=NULL)
  

  wb <- openxlsx::createWorkbook()
  
 
  openxlsx::addWorksheet(wb, sheet_overall)
  top1 <- c("", rep(categories, each = 4))
  sub1 <- c("", rep(c("Coord",
                      "Partial Coord (summed distances)",
                      "Within Inertia",
                      "Within Inertia (%)"),
                    times = length(categories)))
  openxlsx::writeData(wb, sheet_overall, t(as.matrix(top1)), startRow = 1, startCol = 1, colNames = FALSE)
  openxlsx::writeData(wb, sheet_overall, t(as.matrix(sub1)), startRow = 2, startCol = 1, colNames = FALSE)
  openxlsx::writeData(wb, sheet_overall, overall_df, startRow = 3, startCol = 1, colNames = FALSE)
  for (k in seq_along(categories)) { c1 <- 2 + (k - 1) * 4; c2 <- c1 + 3; openxlsx::mergeCells(wb, sheet_overall, cols = c1:c2, rows = 1) }
  hs <- openxlsx::createStyle(textDecoration = "bold", halign = "center", valign = "center")
  openxlsx::addStyle(wb, sheet_overall, hs, rows = 1:2, cols = 1:(1 + 4*length(categories)), gridExpand = TRUE)
  openxlsx::setColWidths(wb, sheet_overall, cols = 1, widths = 22)
  openxlsx::setColWidths(wb, sheet_overall, cols = 2:(1 + 4*length(categories)), widths = 14)
  

  openxlsx::addWorksheet(wb, sheet_blocks)
  top2 <- c("Active variable groups", rep(categories, each = 4))
  sub2 <- c("", rep(c("Partial Coord", "Coord Shift", "Partial Within Inertia", "Within Inertia (%)"),
                    times = length(categories)))
  openxlsx::writeData(wb, sheet_blocks, t(as.matrix(top2)), startRow = 1, startCol = 1, colNames = FALSE)
  openxlsx::writeData(wb, sheet_blocks, t(as.matrix(sub2)), startRow = 2, startCol = 1, colNames = FALSE)
  openxlsx::writeData(wb, sheet_blocks, byblock_df, startRow = 3, startCol = 1, colNames = FALSE)
  for (k in seq_along(categories)) { c1 <- 2 + (k - 1) * 4; c2 <- c1 + 3; openxlsx::mergeCells(wb, sheet_blocks, cols = c1:c2, rows = 1) }
  openxlsx::addStyle(wb, sheet_blocks, hs, rows = 1:2, cols = 1:(1 + 4*length(categories)), gridExpand = TRUE)
  openxlsx::setColWidths(wb, sheet_blocks, cols = 1, widths = 28)
  openxlsx::setColWidths(wb, sheet_blocks, cols = 2:(1 + 4*length(categories)), widths = 14)
  
  openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
  if (debug) message(sprintf("Excel written to: %s", normalizePath(file, winslash = "/")))
  invisible(list(
    overall   = overall_df,
    by_block  = byblock_df,
    .internals = list(
      coord_cat                      = coord_cat,
      pc_mat                         = pc_mat,
      delta_mat                      = delta_mat,
      within_pb                      = within_pb,                    
      within_inertia                 = within_inertia,               
      within_inertia_pct_overall     = within_inertia_pct_overall,  
      within_inertia_pct_by_block    = within_pct_blk,              
      pc_summed_dist                 = pc_summed_dist
    )
  ))
}

tabs <- build_supquali_tables(
  res        = res_mfa_mixed,
  categories = c("female <65","female 65+","male <65","male 65+"),
  axis       = 2,
  file       = "mfa_supquali_tables_dim2.xlsx",
  debug      = TRUE
)




# -------- PERMUTATION-ONLY RV (ade4::RV.rtest) ---------------------------
.rv_perm <- function(X, Y, nrepet = 9999L, seed = NULL) {
  stopifnot(is.matrix(X) || is.data.frame(X), is.matrix(Y) || is.data.frame(Y))
  stopifnot(nrow(X) == nrow(Y))
  if (!is.null(seed)) set.seed(seed)

  Xc <- scale(X, center = TRUE, scale = FALSE)
  Yc <- scale(Y, center = TRUE, scale = FALSE)
  rt <- ade4::RV.rtest(as.data.frame(Xc), as.data.frame(Yc), nrepet = nrepet)
  list(RV = as.numeric(rt$obs), p.value = as.numeric(rt$pvalue),
       nrepet = nrepet, test = "Permutation (ade4::RV.rtest)")
}



group_coord_mat <- function(res, dims = 1:2) {
  G <- res$group$coord
  if (is.null(G)) stop("res$group$coord not found.")
  cols <- intersect(dims, seq_len(ncol(G)))
  if (!length(cols)) stop("No valid axes in `dims` for group coords.")
  G[, cols, drop = FALSE]
}

#A1: OVERALL vs SUBGROUP (individuals × dims)
rv_indiv_vs_global <- function(global_res, sub_res, dims = 1:2,
                               nrepet = 9999L, seed = NULL) {
  ids <- intersect(rownames(global_res$ind$coord), rownames(sub_res$ind$coord))
  if (length(ids) < 3L) stop("Fewer than 3 common individuals.")
  maxd <- min(ncol(global_res$ind$coord), ncol(sub_res$ind$coord))
  idx  <- intersect(dims, seq_len(maxd)); if (!length(idx)) stop("No valid axes.")
  X <- as.matrix(global_res$ind$coord[ids, idx, drop = FALSE])
  Y <- as.matrix(sub_res$ind$coord[ids,   idx, drop = FALSE])
  .rv_perm(X, Y, nrepet = nrepet, seed = seed)
}


#A2: OVERALL vs SUBGROUP (groups × dims)
rv_groups_vs_global <- function(global_res, sub_res, dims = 1:2,
                                nrepet = 9999L, seed = NULL) {
  GA <- group_coord_mat(global_res, dims); GB <- group_coord_mat(sub_res, dims)
  rows <- intersect(rownames(GA), rownames(GB))
  if (length(rows) < 3L) stop("Fewer than 3 common groups.")
  .rv_perm(GA[rows, , drop = FALSE], GB[rows, , drop = FALSE],
           nrepet = nrepet, seed = seed)
}


#B1: SUBGROUP vs SUBGROUP (groups × dims)
rv_groups_between <- function(resA, resB, dims = 1:2,
                              nrepet = 9999L, seed = NULL) {
  GA <- group_coord_mat(resA, dims); GB <- group_coord_mat(resB, dims)
  rows <- intersect(rownames(GA), rownames(GB))
  if (length(rows) < 3L) stop("Fewer than 3 shared groups.")
  .rv_perm(GA[rows, , drop = FALSE], GB[rows, , drop = FALSE],
           nrepet = nrepet, seed = seed)
}


## A1: individuals × dims
rv_indiv_vs_global(res_mfa_mixed, mfa_female_65plus, dims = 1:2, nrepet = 9999, seed=123)

## A2: groups × dims
rv_groups_vs_global(res_mfa_mixed, mfaemale_younger65, dims = 1:2, nrepet = 9999, seed=123)

## B1: subgroup vs subgroup (groups × dims)
rv_groups_between(mfa_male_younger65, mfa_male_65plus, dims = 1:2, nrepet = 9999, seed=123)


, nrepet=9999, seed=123


sub_mfas <- list(female65 = mfa_female_65plus, femaleU65 = mfa_female_younger65, male65 = mfa_male_65plus, maleU65 = mfa_male_younger65)




## RV summary tables -> 


.pretty_labels <- c(
  femaleU65 = "Female <65 MFA",
  female65  = "Female 65+ MFA",
  maleU65   = "Male <65 MFA",
  male65    = "Male 65+ MFA"
)


compute_overall_vs_sub <- function(global_res, sub_mfas, dims = 1:2,
                                   mode = c("indiv","groups"),
                                   nrepet = 9999L, seed = NULL,
                                   order_keys = c("femaleU65","female65","maleU65","male65")) {
  mode <- match.arg(mode)
  keys <- intersect(order_keys, names(sub_mfas))
  if (length(keys) == 0L) stop("No matching subgroup names in `sub_mfas`.")
  lbls <- .pretty_labels[keys]
  out <- lapply(keys, function(k) {
    sub_res <- sub_mfas[[k]]
    res <- switch(mode,
                  indiv  = rv_indiv_vs_global(global_res, sub_res, dims = dims, nrepet = nrepet, seed = seed),
                  groups = rv_groups_vs_global(global_res, sub_res, dims = dims, nrepet = nrepet, seed = seed)
    )
    data.frame(
      Subgroup = unname(lbls[[k]]),
      RV       = round(res$RV, 2),         
      `p-value`= round(res$p.value, 4),    
      check.names = FALSE
    )
  })
  do.call(rbind, out)
}

compute_groups_vs_groups_stacked <- function(
    sub_mfas, dims = 1:2, nrepet = 9999L, seed = NULL,
    order_keys = c("femaleU65","female65","maleU65","male65"),
    digits_rv = 2, digits_p = 4, dec = "."
) {
  keys <- intersect(order_keys, names(sub_mfas))
  if (length(keys) < 2L) stop("Need at least two subgroups in `sub_mfas`.")
  pretty <- unname(c(
    femaleU65 = "Female <65 MFA",
    female65  = "Female 65+ MFA",
    maleU65   = "Male <65 MFA",
    male65    = "Male 65+ MFA"
  )[keys])
  
  n <- length(keys)
  RV <- matrix(NA_real_, n, n, dimnames = list(pretty, pretty))
  P  <- matrix(NA_real_, n, n, dimnames = list(pretty, pretty))
  
  for (i in seq_len(n)) {
    for (j in i:n) {
      if (i == j) next
      res <- rv_groups_between(sub_mfas[[keys[i]]], sub_mfas[[keys[j]]],
                               dims = dims, nrepet = nrepet, seed = seed)
      RV[i,j] <- RV[j,i] <- res$RV
      P[i,j]  <- P[j,i]  <- res$p.value
    }
  }
  
  fmt_num <- function(x, digits) {
    s <- formatC(x, digits = digits, format = "f")
    if (dec == ",") s <- gsub("\\.", ",", s)
    s
  }
  fmt_cell <- function(rv, p) {
    if (is.na(rv) || is.na(p)) return("")
    paste0(fmt_num(rv, digits_rv), "\n(", fmt_num(p, digits_p), ")")
  }
  
  out <- data.frame(Subgroup = pretty, check.names = FALSE)
  for (j in seq_len(n)) {
    col <- vapply(seq_len(n), function(i) if (i == j) "—" else fmt_cell(RV[i,j], P[i,j]), "", USE.NAMES = FALSE)
    out[[ pretty[j] ]] <- col
  }
  out
}


write_rv_excel <- function(global_res, sub_mfas,
                           dims = 1:2, nrepet = 9999L, seed = NULL,
                           outfile = "rv_tables.xlsx",
                           overall_label = "Overall MFA") {
  

  tbl_1a <- compute_overall_vs_sub(global_res, sub_mfas, dims, mode = "indiv",
                                   nrepet = nrepet, seed = seed)
  tbl_1b <- compute_overall_vs_sub(global_res, sub_mfas, dims, mode = "groups",
                                   nrepet = nrepet, seed = seed)
  tbl_2a <- compute_groups_vs_groups_wide(sub_mfas, dims, nrepet, seed)
  

  wb <- createWorkbook()
  

  hdr_big <- createStyle(textDecoration = "bold", halign = "center", valign = "center")
  hdr_small <- createStyle(textDecoration = "bold", halign = "center", valign = "center")
  left_bold <- createStyle(textDecoration = "bold", halign = "left")
  num_rv <- createStyle(numFmt = "0.00")   
  num_p  <- createStyle(numFmt = "0.0000")   
  center <- createStyle(halign = "center", valign = "center")
  

  write_block_overall <- function(sheet_name, table_df, block_title) {
    addWorksheet(wb, sheet_name)
    writeData(wb, sheet_name, x = data.frame(Subgroup = "", A = block_title, B = ""),
              startCol = 1, startRow = 1, colNames = FALSE)
    mergeCells(wb, sheet_name, cols = 2:3, rows = 1)
    addStyle(wb, sheet_name, hdr_big, rows = 1, cols = 2, gridExpand = TRUE)
    
    writeData(wb, sheet_name, x = data.frame(Subgroup = "", RV = "RV", p = "p-value"),
              startCol = 1, startRow = 2, colNames = FALSE)
    addStyle(wb, sheet_name, hdr_small, rows = 2, cols = 2:3, gridExpand = TRUE)
    
    writeData(wb, sheet_name, x = table_df, startCol = 1, startRow = 3, colNames = FALSE)
    addStyle(wb, sheet_name, left_bold, rows = 3:(nrow(table_df)+2), cols = 1, gridExpand = TRUE)
    
    addStyle(wb, sheet_name, num_rv, rows = 3:(nrow(table_df)+2), cols = 2, gridExpand = TRUE)
    addStyle(wb, sheet_name, num_p,  rows = 3:(nrow(table_df)+2), cols = 3, gridExpand = TRUE)

    setColWidths(wb, sheet_name, cols = 1, widths = "auto")
    setColWidths(wb, sheet_name, cols = 2:3, widths = 12)
    freezePane(wb, sheet_name, firstActiveRow = 3, firstActiveCol = 2)
    addStyle(wb, sheet_name, center, rows = 1:2, cols = 2:3, gridExpand = TRUE)
  }
  

  write_block_groups_vs_groups_stacked <- function(wb, sheet_name, table_df) {
    addWorksheet(wb, sheet_name)
    
    hdr_big   <- openxlsx::createStyle(textDecoration = "bold", halign = "center", valign = "center")
    hdr_small <- openxlsx::createStyle(textDecoration = "bold", halign = "center", valign = "center")
    left_bold <- openxlsx::createStyle(textDecoration = "bold", halign = "left")
    wrap_mid  <- openxlsx::createStyle(halign = "center", valign = "center", wrapText = TRUE)
    
    ncols <- ncol(table_df)
    
    openxlsx::writeData(wb, sheet_name,
                        x = data.frame("", "Cell format: RV (top) / p-value (bottom)"),
                        startCol = 1, startRow = 1, colNames = FALSE)
    openxlsx::mergeCells(wb, sheet_name, cols = 2:ncols, rows = 1)
    openxlsx::addStyle(wb, sheet_name, hdr_big, rows = 1, cols = 2:ncols, gridExpand = TRUE)
    
    openxlsx::writeData(wb, sheet_name, x = table_df, startCol = 1, startRow = 3, colNames = TRUE)
    
    openxlsx::addStyle(wb, sheet_name, left_bold, rows = 4:(nrow(table_df) + 3), cols = 1, gridExpand = TRUE)
    
    openxlsx::addStyle(wb, sheet_name, wrap_mid, rows = 4:(nrow(table_df) + 3), cols = 2:ncols, gridExpand = TRUE)
    
    openxlsx::setColWidths(wb, sheet_name, cols = 1, widths = "auto")
    openxlsx::setColWidths(wb, sheet_name, cols = 2:ncols, widths = 16)
    openxlsx::freezePane(wb, sheet_name, firstActiveRow = 4, firstActiveCol = 2)
    openxlsx::addStyle(wb, sheet_name, hdr_small, rows = 3, cols = 1:ncols, gridExpand = TRUE)
  }
  
  block_title <- paste0(overall_label, " ×")
  write_block_overall("1a_Individuals_vs_Overall",
                      tbl_1a, block_title)
  
  write_block_overall("1b_Groups_vs_Overall",
                      tbl_1b, block_title)
  

  tbl_2a <- compute_groups_vs_groups_stacked(sub_mfas, dims, nrepet, seed, dec = ".") 
  write_block_groups_vs_groups_stacked(wb, "2a_Groups_vs_Groups", tbl_2a)
  
  saveWorkbook(wb, outfile, overwrite = TRUE)
  invisible(list(`1a` = tbl_1a, `1b` = tbl_1b, `2a` = tbl_2a, file = normalizePath(outfile)))
}


sub_mfas <- list(
  female65  = mfa_female_65plus,
  femaleU65 = mfa_female_younger65,
  male65    = mfa_male_65plus,
  maleU65   = mfa_male_younger65
)

write_rv_excel(
  global_res = res_mfa_mixed,
  sub_mfas   = sub_mfas,
  dims       = 1:2,
  nrepet     = 9999,
  seed       = 123,
  outfile    = "rv_tables.xlsx",
  overall_label = "Overall MFA"
)
