#########################################################
########## weighted correlations for missingness ########
#########################################################

library(survey)
library(haven)
library(wCorr)
library(coin)
library(confintr)
library(jtools)
library(janitor)
library(mice)
library(ggcorrplot)
library(reshape2)

setwd(path)
data_missingness <- read_dta("processed_data_05_08_2025.dta")


data_missingness$K69_aggreg[data_missingness$K69_aggreg == ""] <- NA

freq(data_missingness$K69_aggreg)

######### Updated svyranktest function to have information on ranks ############

is.calibrated<-function(design){ !is.null(design$postStrata)}


svyranktest<-function(formula,design,test=c('wilcoxon','vanderWaerden','median',"KruskalWallis"),...){
  UseMethod("svyranktest", design)
}

svyranktest.survey.design<-svyranktest.svyrep.design<-function(formula, design,
                                                               test=c('wilcoxon','vanderWaerden','median',"KruskalWallis"),...)
{
  mf<-model.frame(formula,model.frame(design),na.action=na.omit)
  if (!is.null(naa<-attr(mf,"na.action"))){
    design<-design[-naa,]
    mf<-model.frame(formula,model.frame(design))
  }
  y<-mf[,1]
  g<-mf[,2]
  
  if (length(unique(g))!=2) {
    return(multiranktest(formula,design, test,...))
  }
  
  if (is.character(test)) {
    test<-match.arg(test)
    testf<-switch(test, wilcoxon=,KruskalWallis=function(r,N) r/N,
                  vanderWaerden=function(r,N) qnorm(r/N),
                  median=function(r,N) as.numeric(r>N/2))
  } else{
    testf<-test
  }	
  if (identical(test,"wilcoxon")) test<-"KruskalWallis"
  
  ii<-order(y)
  n<-length(y)
  rankhat<-numeric(n)
  
  w<-weights(design,"sampling")

  na.fixup<-FALSE
  if (is.calibrated(design) && length(w)>n && !is.null(naa)){
    w<-w[-naa]
    na.fixup<-TRUE
  }
  
  
  N<-sum(w)
  rankhat[ii]<-ave(cumsum(w[ii])-w[ii]/2,factor(y[ii]))
  
  rankscore<-testf(rankhat,N)
  
  m <- lm(rankscore~g, weights=w)
  delta<-coef(m)[2]
  xmat<-model.matrix(m)
  
  if (na.fixup){
    infn<-matrix(0,nrow=nrow(xmat)+length(naa),ncol=ncol(xmat))
    infn[-naa,]<-(xmat*(rankscore-fitted(m)))%*%summary(m)$cov.unscaled
  } else {
    infn<- (xmat*(rankscore-fitted(m)))%*%summary(m)$cov.unscaled
  }
  tot.infn<-svytotal(infn,design)
  if (is.character(test))
    method<-paste("Design-based",test,"test")
  else if (!is.null(attr(test,"name")))
    method<-paste("Design-based",attr(test,"name"),"test")
  else method<-"Design-based rank test"
  
  rval <- list(statistic = coef(m)[2]/SE(tot.infn)[2], parameter = degf(design) - 
                 1, estimate = coef(m)[2], null.value = 0, alternative = "two.sided", 
               method = method, data.name = deparse(formula), ranks = rankhat, rankscore = rankscore)
  rval$p.value <- 2 * pt(-abs(rval$statistic), df = rval$parameter)
  names(rval$statistic) <- "t"
  names(rval$parameter) <- "df"
  names(rval$estimate) <- "difference in mean rank score"
  names(rval$null.value) <- "difference in mean rank score"
  class(rval) <- "htest"
  rval
}

multiranktest<-function(formula,design,test=c('wilcoxon','vanderWaerden','median','KruskalWallis'),...){
  mf<-model.frame(formula,model.frame(design),na.action=na.omit)
  if (!is.null(naa<-attr(mf,"na.action"))){
    design<-design[-naa,]
    #mf<-model.frame(formula,model.frame(design),na.action=na.fail)
  }
  y<-mf[,1]
  g<-mf[,2]
  
  if (is.character(test)) {
    test<-match.arg(test)
    testf<-switch(test, wilcoxon=,KruskalWallis=function(r,N) r/N,
                  vanderWaerden=function(r,N) qnorm(r/N),
                  median=function(r,N) as.numeric(r>N/2))
  } else{
    testf<-test
  }	
  if (identical(test,"wilcoxon")) test<-"KruskalWallis"
  
  ii<-order(y)
  n<-length(y)
  rankhat<-numeric(n)
  w<-weights(design,"sampling")

  if (is.calibrated(design) && length(w)>n && !is.null(naa)){
    w<-w[-naa]
  }
  
  N<-sum(w)
  rankhat[ii]<-ave(cumsum(w[ii])-w[ii]/2,factor(y[ii]))
  rankscore<-testf(rankhat,N)
  m <- glm(rankscore~factor(g),weights=w)
  m$na.action<-naa
  V<-svy.varcoef(m,design) 
  ndf<-length(unique(g))-1
  beta<-coef(m)[-1]
  V<-V[-1,-1]
  chisq<-beta%*%solve(V,beta)
  ddf<-degf(design)-ndf
  if (is.character(test))
    method<-paste("Design-based",test,"test")
  else if (!is.null(attr(test,"name")))
    method<-paste("Design-based",attr(test,"name"),"test")
  else method<-"Design-based rank test"
  names(chisq)<-"Chisq"
  names(ndf)<-"df"
  rval<-list(parameter=chisq,statistic=ndf,ddf=ddf,p.value=pf(chisq/ndf,ndf,ddf,lower.tail=FALSE),
             method=method, data.name = deparse(formula))
  class(rval)<-"htest"
  rval
}

cov_inffun_glm<-function(m, design){
  A<-summary(m)$cov.unscaled
  xmat <- model.matrix(m)
  U<-residuals(m, "working") * m$weights * xmat/weights(design,"sampling")
  h<-U%*%A
  vcov(svytotal(h, design))
}

svy.varcoef<-function(glm.object,design){
  Ainv<-summary(glm.object)$cov.unscaled
  nas<-glm.object$na.action
  estfun<-model.matrix(glm.object)*naa_shorter(nas, resid(glm.object,"working"))*glm.object$weights
  if (glm.object$rank<NCOL(estfun)){
    estfun<-estfun[,glm.object$qr$pivot[1:glm.object$rank]]
  }
  naa<-glm.object$na.action
  ## the design may still have rows with weight zero for missing values
  ## if there are weights or calibration. model.matrix will have removed them
  if (length(naa) && (NROW(estfun)!=nrow(design) )){
    if ((length(naa)+NROW(estfun))!=nrow(design) )
      stop("length mismatch: this can't happen.")
    n<-nrow(design)     
    inx <- (1:n)[-naa]
    ee <- matrix(0,nrow=n,ncol=NCOL(estfun))
    ee[inx,]<-estfun
    estfun<-ee
  }
  
  if (inherits(design,"survey.design2"))
    svyrecvar(estfun%*%Ainv,design$cluster,design$strata,design$fpc,postStrata=design$postStrata)
  else if (inherits(design, "twophase"))
    twophasevar(estfun%*%Ainv, design)
  else if (inherits(design, "twophase2"))
    twophase2var(estfun%*%Ainv, design)
  else if (inherits(design, "pps"))
    ppsvar(estfun%*%Ainv, design)
  else
    vcov(svytotal(estfun%*%Ainv/weights(design,"sampling"), design))
}

naa_longer<-function(naa, object,...) UseMethod("naa_longer",naa)
naa_shorter<-function(naa, object,...) UseMethod("naa_shorter",naa)

naa_longer.NULL<-function(naa, object,...) object
naa_shorter.NULL<-function(naa, object,...) object

naa_longer.default<-function(naa, object,...) stop("no default method (not psychic)")
naa_shorter.default<-function(naa, object,...) stop("no default method (not psychic)")

naa_longer.fail<-function(naa, object,...) stop("can't happen (na.fail)")
naa_shorter.fail<-function(naa, object,...) stop("can't happen (na.fail)")

naa_shorter.omit<-function(naa, object,...) object
naa_longer.omit<-function(naa,object,...){ ##from naresid.exclude
  if (length(naa) == 0 || !is.numeric(naa)) 
    stop("invalid argument 'naa'")
  if (is.null(object)) 
    return(object)
  n <- NROW(object)
  keep <- rep.int(NA, n + length(naa))
  keep[-naa] <- 1L:n
  if (is.matrix(object)) {
    object <- object[keep, , drop = FALSE]
    temp <- rownames(object)
    if (length(temp)) {
      temp[naa] <- names(naa)
      rownames(object) <- temp
    }
  }
  else if (is.array(object) && length(d <- dim(object)) > 2L) {
    object <- object[keep, , , drop = FALSE]
    temp <- (dn <- dimnames(object))[[1L]]
    if (!is.null(temp)) {
      temp[naa] <- names(naa)
      dimnames(object)[[1L]] <- temp
    }
  }
  else {
    object <- object[keep]
    temp <- names(object)
    if (length(temp)) {
      temp[naa] <- names(naa)
      names(object) <- temp
    }
  }
  object	
}

naa_longer.exclude<-function(naa,object,...) object
naa_shorter.exclude<-function(naa,object,...) {
  if (length(naa) == 0 || !is.numeric(naa)) 
    stop("invalid argument 'naa'")
  if (is.null(object)) 
    return(object)
  n <- NROW(object)
  keep <- (1:n)[-naa]
  if (is.matrix(object)) {
    object <- object[keep, , drop = FALSE]
    temp <- rownames(object)
  }
  else if (is.array(object) && length(d <- dim(object)) > 2L) {
    object <- object[keep, , , drop = FALSE]
    temp <- (dn <- dimnames(object))[[1L]]
  }
  else {
    object <- object[keep]
    temp <- names(object)
  }
  object		
}

######### /Updated svyranktest function to have information on ranks ############


#checking which auxiliary variables are highly associated to each other and removing them to limit the number of auxiliary variables used in the multiple imputation:


#https://www.wbc.poznan.pl/Content/325867/PDF/5_Trends_Vol21_2014_%20no1_20.pdf
#effect size metrics and the equations used

library(ggplot2)
library(tidyr)
library(dplyr)



# Group 3 variables
#group3_binary <- c("K5702", "K5703", "K5704", "K74")
#group3_ordinal <- c("K1201", "K1202", "K1203", 
#                    "K13", "K1501", "K1502", "K1503", "K1504", "K1505", "K1506", "K1507", 
#                    "K1508", "K1510", "K1511", "K1512", "K1513", "K31", "K32", "K34", 
#                    "K35", "K36", "K3701", "K3702", "K3703", "K3704", "K3705", "K3706", 
#                    "K3707", "K3708", "K7301", "K7302", "K7303", "K7304", "K7305", "K7306", 
#                    "K7307", "K7308")
#group3_nominal <- c("K33", "K5601", "K5602", "K5603")
#group3_continuous <- c("Medianincome2019HR", "Inhabitantspurchasingpower2019HR", 
#                       "Householdmedianincome2019TR", "Householdaverageincome2019TR", 
#                       "Householdpurchasingpower2019TR", "Averageage2019HE", 
#                       "Averagehouseholdssize2019TE", "Averageincome2019HR", 
#                       "Householdsotherdwellings2019TE", "Householdsownerdwellings2019TE", 
#                       "Householdsrenteddwellings2019TE", "Householdsschoolchildren2019TE", 
#                       "Householdssmallchildren2019TE")

# Group 3 variables
#group3_binary <- c("K5702", "K5703", "K74")
#group3_ordinal <- c("K1101", "K1201", "K1203", 
#                    "K13", "K1501", "K1502", "K1504", "K1506", 
#                    "K1508", "K1510", "K1511", "K1512", "K31", "K32", "K34", 
#                    "K35", "K3701", "K3702", "K3705", "K3706", 
#                    "K3707", "K3708", "K7301", "K7304", "K7305", "K7306", 
#                    "K7307", "K7308")
#group3_nominal <- c("K33", "K5601")
#group3_continuous <- c("Averageage2019HE","Averageincome2019HR")

group3_binary <- c()
group3_ordinal <- c()
group3_continuous <- c("ADHL2019KO_ratio", "ADLL2019KO_ratio", "aged18orover_2019KO", "Averageage2019HE", "Averagehouseholdssize2019TE", "Averageincome2019HR", "BLS2019KO_ratio", "Eikotimaankieliset_ratio", 
"Employed2019PT_ratio", "Females2019HE", "Householdaverageincome2019TR", "Householdmedianincome2019TR", "Householdpurchasingpower2019TR", "Householdshighestincome2019TR", "Householdslowestincome2019TR", 
"Householdsmiddleincome2019TR", "HOD2019TE_ratio", "HRD2019TE_ratio", "HSchC2019TE_ratio", "HSmC2019TE_ratio", "Inhabintantslowestincome2019HR", "Inhabitants2019PT", "Inhabitantshighestincome2019HR", 
"Inhabitantsmiddleincome2019HR", "Inhabitantspurchasingpower2019HR", "ME2019KO_ratio", "Medianincome2019HR", "Occupancyrate2019TE", "OPH2019TE_ratio", "PH2019TE_ratio", "Pensioners2019PT_ratio", 
"Students2019PT_ratio", "Unemployed2019PT_ratio", "VD2019KO_ratio", "WET2019KO_ratio")
group3_nominal <- c("Kuntaryhmännimi", "Suuralueennimi")


results_df <- data.frame(
  Variable1 = character(),
  Variable2 = character(),
  Type = character(),
  TestName = character(),
  PValue = numeric(),
  EffectSize = numeric(),
  stringsAsFactors = FALSE
)


test_association_pairwise <- function(group3_var1, group3_var2) {
  p_value <- NULL
  test_name <- NULL
  additional_info <- NULL
  effect_size <- NULL
  
  
  #pairwise deletion
  data_subset <- data_missingness[complete.cases(data_missingness[[group3_var1]], data_missingness[[group3_var2]]), ]
  
  if (nrow(data_subset) == 0) {
    cat(sprintf("Skipping %s vs. %s due to no complete cases.\n", group3_var1, group3_var2))
    return() 
  }
  
  design_subset <- svydesign(ids = ~1, data = data_subset, weights = ~useweight)
  
  tryCatch({
    if (group3_var1 %in% group3_binary && group3_var2 %in% group3_binary) {
      # Binary vs Binary: Chi-square test or Fisher's exact test
      variable_type = "binary"
      table_result <- svytable(as.formula(paste("~", group3_var1, "+", group3_var2)), design_subset)
      chisq_test <- chisq.test(table_result, correct = FALSE, simulate.p.value = TRUE)
      expected_freqs <- chisq_test$expected
      
      expected_freqs_values <- as.numeric(expected_freqs)
      
      #checking the assumptions for chi-square test
      if (nrow(table_result) == 2 && ncol(table_result) == 2) {
        # 2x2 table: all expected frequencies must be at least 5
        if (all(expected_freqs_values >= 5)) {
          p_value <- chisq_test$p.value
          test_name <- "Chi-square Test"
          additional_info <- sprintf("Chi-square statistic = %.3f", chisq_test$statistic)
          
          # Effect size: Phi coefficient for 2x2 table, Cramér's V for larger tables
          effect_size <- sqrt(chisq_test$statistic / length(as.numeric(expected_freqs)))
          additional_info <- paste(additional_info, sprintf(", Effect size (Phi) = %.3f", effect_size))
          
        } else {
          #assumptions not met: Fisher's Exact Test instead
          fisher_result <- fisher.test(table_result, simulate.p.value = TRUE)
          p_value <- fisher_result$p.value
          test_name <- "Fisher's Exact Test"
          additional_info <- sprintf("Odds Ratio = %.3f, Confidence Interval = [%.3f, %.3f]", 
                                     fisher_result$estimate, 
                                     fisher_result$conf.int[1], 
                                     fisher_result$conf.int[2])
          
          #checking if odds ratio is available
          if (!is.null(fisher_result$estimate)) {
            additional_info <- sprintf("Odds Ratio = %.3f, Confidence Interval = [%.3f, %.3f]", 
                                       fisher_result$estimate, 
                                       fisher_result$conf.int[1], 
                                       fisher_result$conf.int[2])
            effect_size <- fisher_result$estimate
          } else {
            additional_info <- "Odds Ratio not available."
            effect_size <- NA
          }
        }} else {
          # Larger table: all but one expected frequency >= 5, and the remaining one >= 1
          if (sum(expected_freqs_values < 5) <= 1 && all(expected_freqs_values >= 1)) {
            p_value <- chisq_test$p.value
            test_name <- "Chi-square Test"
            additional_info <- sprintf("Chi-square statistic = %.3f", chisq_test$statistic)
            
            # Effect size: Phi coefficient for 2x2 table, Cramér's V for larger tables
            effect_size <- cramersv(table_result)
            additional_info <- paste(additional_info, sprintf(", Effect size (Cramer's V) = %.3f", effect_size))
            
          } else {
            #assumptions not met: use Fisher's Exact Test (with hybrid option for large tables)
            fisher_result <- fisher.test(table_result, hybrid = TRUE, simulate.p.value = TRUE)
            p_value <- fisher_result$p.value
            test_name <- "Fisher's Exact Test (Hybrid)"
            
            #checking if odds ratio is available
            if (!is.null(fisher_result$estimate)) {
              additional_info <- sprintf("Odds Ratio = %.3f, Confidence Interval = [%.3f, %.3f]", 
                                         fisher_result$estimate, 
                                         fisher_result$conf.int[1], 
                                         fisher_result$conf.int[2])
              effect_size <- fisher_result$estimate
            } else {
              additional_info <- "Odds Ratio not available."
              effect_size <- NA
            }
          }
        }
      
    }  else if (group3_var1 %in% group3_nominal && group3_var2 %in% group3_nominal) {
      # Nominal vs Nominal: Chi-square test or Fisher's exact test
      variable_type = "nominal"
      table_result <- svytable(as.formula(paste("~", group3_var1, "+", group3_var2)), design_subset)
      chisq_test <- chisq.test(table_result, correct = FALSE, simulate.p.value = TRUE)
      expected_freqs <- chisq_test$expected
      
      expected_freqs_values <- as.numeric(expected_freqs)
      
      #checking the assumptions for chi-square test
      if (nrow(table_result) == 2 && ncol(table_result) == 2) {
        # 2x2 table: all expected frequencies must be at least 5
        if (all(expected_freqs_values >= 5)) {
          p_value <- chisq_test$p.value
          test_name <- "Chi-square Test"
          additional_info <- sprintf("Chi-square statistic = %.3f", chisq_test$statistic)
          
          # Effect size: Phi coefficient for 2x2 table, Cramér's V for larger tables
          effect_size <- sqrt(chisq_test$statistic / length(as.numeric(expected_freqs)))
          additional_info <- paste(additional_info, sprintf(", Effect size (Phi) = %.3f", effect_size))
          
        } else {
          #assumptions not met: Fisher's Exact Test instead
          fisher_result <- fisher.test(table_result, simulate.p.value = TRUE)
          p_value <- fisher_result$p.value
          test_name <- "Fisher's Exact Test"
          additional_info <- sprintf("Odds Ratio = %.3f, Confidence Interval = [%.3f, %.3f]", 
                                     fisher_result$estimate, 
                                     fisher_result$conf.int[1], 
                                     fisher_result$conf.int[2])
          
          # checking if odds ratio is available
          if (!is.null(fisher_result$estimate)) {
            additional_info <- sprintf("Odds Ratio = %.3f, Confidence Interval = [%.3f, %.3f]", 
                                       fisher_result$estimate, 
                                       fisher_result$conf.int[1], 
                                       fisher_result$conf.int[2])
            effect_size <- fisher_result$estimate
          } else {
            additional_info <- "Odds Ratio not available."
            effect_size <- NA
          }
        }
      }
      else {
        # Larger table: all but one expected frequency >= 5, and the remaining one >= 1
        if (sum(expected_freqs_values < 5) <= 1 && all(expected_freqs_values >= 1)) {
          p_value <- chisq_test$p.value
          test_name <- "Chi-square Test"
          additional_info <- sprintf("Chi-square statistic = %.3f", chisq_test$statistic)
          
          # Effect size: Phi coefficient for 2x2 table, Cramér's V for larger tables
          effect_size <- cramersv(table_result)
          additional_info <- paste(additional_info, sprintf(", Effect size (Cramer's V) = %.3f", effect_size))
          
        } else {
          #assumptions not met: use Fisher's Exact Test (with hybrid option for large tables)
          fisher_result <- fisher.test(table_result, hybrid = TRUE, simulate.p.value = TRUE)
          p_value <- fisher_result$p.value
          test_name <- "Fisher's Exact Test (Hybrid)"
          
          # checking if odds ratio is available
          if (!is.null(fisher_result$estimate)) {
            additional_info <- sprintf("Odds Ratio = %.3f, Confidence Interval = [%.3f, %.3f]", 
                                       fisher_result$estimate, 
                                       fisher_result$conf.int[1], 
                                       fisher_result$conf.int[2])
            effect_size <- fisher_result$estimate
          } else {
            additional_info <- "Odds Ratio not available."
            effect_size <- NA
          }
        }
      }
      
    }
    else if (group3_var1 %in% group3_ordinal && group3_var2 %in% group3_ordinal){
      # Ordinal vs Ordinal: Weighted Spearman's rank correlation using weightedCorr
      variable_type = "ordinal"
      spearman_corr <- weightedCorr(data_subset[[group3_var1]], data_subset[[group3_var2]], 
                                    weights = data_subset$useweight, method = "spearman")
      if (abs(spearman_corr) > 0.3) {
        n <- nrow(data_subset)
        z_value <- spearman_corr * sqrt(n - 1) #as sample is bigger than 30
        p_value <- 2 * (q=pnorm(abs(z_value), lower.tail=FALSE))
        test_name <- "Spearman's Rank Correlation"
        additional_info <- sprintf("Spearman's rho = %.3f, Z = %.3f", spearman_corr, z_value)
        effect_size <- spearman_corr
      } else {
        return()
      }
      gc()
    } else if (group3_var1 %in% group3_continuous && group3_var2 %in% group3_continuous) {
      # Continuous vs Continuous: Pearson's or Spearman's correlation
      variable_type = "continuous"
      normality_test1 <- shapiro.test(data_subset[[group3_var1]])$p.value
      normality_test2 <- shapiro.test(data_subset[[group3_var2]])$p.value
      if (normality_test1 > 0.05 && normality_test2 > 0.05) {
        cor_test <- svycor(~data_subset[[group3_var1]] + data_subset[[group3_var2]], design_subset)
        correlation_matrix <- cor_test[[1]]  
        correlation_value <- correlation_matrix[1, 2] 
        
        if (abs(correlation_value) > 0.3) {
          p_value <- cor_test$p.value
          test_name <- "Pearson's Correlation"
          additional_info <- sprintf("Pearson's r = %.3f", correlation_value)
          effect_size <- correlation_value
        } else {
          return()
        }
        
        
      } else {
        # Spearman's rank correlation for non-normal data
        spearman_corr <- weightedCorr(data_subset[[group3_var1]], data_subset[[group3_var2]], 
                                      weights = data_subset$useweight, method = "spearman")
        if (abs(spearman_corr) > 0.3) {
          n <- nrow(data_subset)
          z_value <- spearman_corr * sqrt(n - 1) #as sample is bigger than 30
          p_value <- 2 * (q=pnorm(abs(z_value), lower.tail=FALSE))
          test_name <- "Spearman's Rank Correlation"
          additional_info <- sprintf("Spearman's rho = %.3f, Z = %.3f", spearman_corr, z_value)
          effect_size <- spearman_corr
        } else {
          return()
        }
      }
      gc()
    }
    
    #saving the result if significant
    if (p_value < 0.05) {
      cat(sprintf("%s for %s vs. %s: p-value = %.3f", test_name, group3_var1, group3_var2, p_value))
      if (!is.null(additional_info)) {
        cat(sprintf(", %s", additional_info))
      }
      cat("\n")
      
      new_row <- data.frame(
        Variable1 = group3_var1,
        Variable2 = group3_var2,
        Type = variable_type,
        TestName = test_name,
        PValue = p_value,
        EffectSize = effect_size,
        stringsAsFactors = FALSE
      )
      results_df <<- rbind(results_df, new_row)
      
      
    }
  }, error = function(e) {
    cat(sprintf("Error in %s vs. %s: %s\n", group3_var1, group3_var2, e$message))
  })
}

# Function to loop through Group 3 variables
loop_same_group_tests <- function(group_list) {
  for (group3_var1 in group_list) {
    for (group3_var2 in group_list) {
      if (group3_var1 == group3_var2) {
        next
      }
      test_association_pairwise(group3_var1, group3_var2)
    }
  }
}

loop_same_group_tests(group3_nominal)
loop_same_group_tests(group3_continuous)


#removing duplicates
results_df <- results_df[!duplicated(results_df), ]

#creating a sorted combination of Variable1 and Variable2 to identify reverse duplicates
results_df$Combination <- apply(results_df[, c("Variable1", "Variable2")], 1, function(x) paste(sort(x), collapse = "_"))

#removing reverse duplicates
results_df <- results_df[!duplicated(results_df[, c("Combination")]), ]

#dropping the Combination column
results_df <- select(results_df, -c("Combination"))

print(results_df)

results_df <- results_df[order(results_df$Type, results_df$TestName, results_df$EffectSize), ]

auxiliary <- results_df

print(auxiliary)



#filtering available columns in the dataset
available_columns <- colnames(data_missingness)

#binary_vars <- intersect(group3_binary, available_columns)
#ordinal_vars <- intersect(group3_ordinal, available_columns)
nominal_vars <- intersect(group3_nominal, available_columns)
continuous_vars <- intersect(group3_continuous, available_columns)

#plotting binary variables
#data_missingness %>%
#  select(all_of(binary_vars), useweight) %>%
#  pivot_longer(cols = -useweight, names_to = "Variable", values_to = "Value") %>%
#  ggplot(aes(x = Value, weight = useweight, fill = Variable)) +
#  geom_histogram(position = "dodge", bins = 10, color = "black") +
#  facet_wrap(~ Variable, scales = "free") +
#  labs(title = "Binary Variables (Weighted)") +
#  theme_minimal() +
#  theme(legend.position = "none")

#ordinal
# data_missingness %>%
#  select(all_of(ordinal_vars), useweight) %>%
#  pivot_longer(cols = -useweight, names_to = "Variable", values_to = "Value") %>%
#  ggplot(aes(x = Value, weight = useweight, fill = Variable)) +
#  geom_histogram(position = "dodge", bins = 10, color = "black") +
#  facet_wrap(~ Variable, scales = "free") +
#  labs(title = "Ordinal Variables (Weighted)") +
#  theme_minimal() +
#  theme(legend.position = "none")

#nominal
data_missingness %>%
  select(all_of(nominal_vars), useweight) %>%
  pivot_longer(cols = -useweight, names_to = "Variable", values_to = "Value") %>%
  ggplot(aes(x = Value, weight = useweight, fill = Variable)) +
  geom_histogram(position = "dodge", bins = 10, color = "black") +
  facet_wrap(~ Variable, scales = "free") +
  labs(title = "Nominal Variables (Weighted)") +
  theme_minimal() +
  theme(legend.position = "none")

#nontinuous

data_missingness %>%
  select(all_of(continuous_vars), useweight) %>%
  pivot_longer(cols = -useweight, names_to = "Variable", values_to = "Value") %>%
  ggplot(aes(x = Value, weight = useweight, fill = Variable)) +
  geom_histogram(bins = 20, color = "black", alpha = 0.6) +
  facet_wrap(~ Variable, scales = "free") +
  labs(title = "Continuous Variables (Weighted)") +
  theme_minimal() +
  theme(legend.position = "none")


#investigating continuous variales
data_design <- svydesign(ids = ~1, weights = ~useweight, data = data_missingness)


spearman_corr_matrix <- cor(data_missingness[, group3_continuous], method = "spearman", use = "complete.obs")


ggcorrplot(spearman_corr_matrix, 
           hc.order = TRUE, 
           method = "square", 
         #  type = "lower", 
           lab = TRUE, 
           lab_size = 2.5,         
           outline.color = "white",
           title = "Weighted Spearman Correlation Matrix")



#I keep only "Householdshighestincome2019TR" and "Householdslowestincome2019TR". I remove "Inhabintantslowestincome2019HR", "Inhabitants2019PT", "Inhabitantshighestincome2019HR", "Inhabitantspurchasingpower2019HR", "Householdpurchasingpower2019TR",
#"Females2019HE","aged18orover_2019KO","Inhabitantsmiddleincome2019HR","Householdsmiddleincome2019TR"

#I keep only "Averageincome2019HR" and remove  "Householdaverageincome2019TR", "Householdmedianincome2019TR","Medianincome2019HR"

#I keep "HOD2019TE_ratio" and remove "HRD2019TE_ratio",


#I remove Averagehousehodsize2019TE, "WET2019KO_ratio", "ME2019KO_ratio", "HSchC2019TE_ratio",

#I keep "PH2019TE_ratio" and remove "Averageage2019HE","Pensioners2019PT_ratio", 



group3_continuous <- c("ADHL2019KO_ratio", "ADLL2019KO_ratio",   
                       "Averageincome2019HR", "BLS2019KO_ratio", "Eikotimaankieliset_ratio", 
                       "Employed2019PT_ratio",   
                       "Householdshighestincome2019TR", "Householdslowestincome2019TR", 
                       "HOD2019TE_ratio", "HSmC2019TE_ratio", 
                       "Occupancyrate2019TE", "OPH2019TE_ratio", 
                       "PH2019TE_ratio", 
                       "Students2019PT_ratio", "Unemployed2019PT_ratio", "VD2019KO_ratio")
group3_nominal <- c("Kuntaryhmännimi", "Suuralueennimi")


#checking associations of model and analysis variables with auxiliary variables used for multiple imputation

#group 1 and 2 variables

group1and2_binary <- c("K03", "K1001", "K1002", "K1003", "K1801", "K1802", "K1803", "K1804", 
                       "K1805", "K1806", "K1807", "K1808", "K3901", "K3902", "K3903", "K3904", 
                       "K3905", "K3906", "K4301", "K4302", "K4303", "K4304", "K4305", "K4306", 
                       "K5701", "K5705", "K5901", "K5902", "K5903", "K5904", "K5905", "K5906", "K5907", 
                       "K5908", "K5909", "K5910", "K5911", "K5912", 
                       "K49", "K2401", 
                       "K2402", "K2403", "K2404", "K2405", "K2406", "K2407", "K2408", "K2409", 
                       "K2410")

group1and2_nominal <- c("K05", "K07", "K1601", "K1602", "K63", "K65", "K3801", "K3802", "K3803", 
                        "K69_aggreg")

group1and2_ordinal <- c("K09", "K1101", "K1102", "K1103", "K1104", "K1105","K14", "K1509", "K1901", "K1902", "K1903", "K1904", "K2001", 
                        "K2002", "K2003", "K2004", "K2005", "K2006", "K2007", "K23", "K25", 
                        "K2801", "K2802", "K2803", "K2901", "K2902", "K2903", "K2904", "K3001", 
                        "K3002", "K3003", "K3004", "K3005", "K3006", "K3007", "K3008", "K58", 
                        "K66_aggreg_num")

group1and2_continuous <- c("K4801", "K4802", "K4803", "K4804", "K4805", "K4806", "K4807", "K01")

#group 3 variables

group3_binary <- c()
group3_ordinal <- c()

group3_continuous <- c("ADHL2019KO_ratio", "ADLL2019KO_ratio",   
                       "Averageincome2019HR", "BLS2019KO_ratio", "Eikotimaankieliset_ratio", 
                       "Employed2019PT_ratio",   
                       "Householdshighestincome2019TR", "Householdslowestincome2019TR", 
                       "HOD2019TE_ratio", "HSmC2019TE_ratio", 
                       "Occupancyrate2019TE", "OPH2019TE_ratio", 
                       "PH2019TE_ratio", 
                       "Students2019PT_ratio", "Unemployed2019PT_ratio", "VD2019KO_ratio")
group3_nominal <- c("Kuntaryhmännimi", "Suuralueennimi")

data_missingness <- read_dta("C:/Users/sierp/OneDrive/CHARM/Data/processed_data_05_08_2025.dta")

data_missingness$K69_aggreg[data_missingness$K69_aggreg == ""] <- NA

freq(data_missingness$K69_aggreg)

results_df <- data.frame(
  Variable1 = character(),
  Variable2 = character(),
  TestName = character(),
  PValue = numeric(),
  EffectSize = numeric(),
  stringsAsFactors = FALSE
)

#function to perform association tests with pairwise deletion and unified output
test_association_pairwise <- function(group1and2_var, group3_var) {
  p_value <- NULL
  test_name <- NULL
  additional_info <- NULL
  effect_size <- NULL
  
  data_subset <- data_missingness[complete.cases(data_missingness[[group1and2_var]], data_missingness[[group3_var]]), ]
  
  design_subset <- svydesign(ids = ~1, data = data_subset, weights = ~useweight)

    
  tryCatch({
    if ((group1and2_var %in% group1and2_binary && group3_var %in% group3_ordinal) ||
        (group1and2_var %in% group1and2_ordinal && group3_var %in% group3_binary)) {
      # Binary vs Ordinal: Wilcoxon test
      
      if (group1and2_var %in% group1and2_binary) {
        ordinal_var <- group3_var
        binary_var <- group1and2_var
      } else {
        ordinal_var <- group1and2_var
        binary_var <- group3_var
      }
      
      rank_test <- svyranktest(as.formula(paste(ordinal_var, "~", binary_var)), design_subset, test="wilcoxon")
      
      p_value <- rank_test$p.value
      test_name <- "Wilcoxon Test"
      additional_info <- sprintf("Statistic = %.3f", rank_test$statistic)
      
      # Effect size: r = Z / sqrt(n)
      
      group_values <- sort(unique(data_subset[[binary_var]]))
      group1_value <- group_values[1]  #lesser numeric value (Group 1)
      group2_value <- group_values[2]  #greater numeric value (Group 2)
      
      group1_ranks <- rank_test$ranks[data_subset[[binary_var]] == group1_value]
      group2_ranks <- rank_test$ranks[data_subset[[binary_var]] == group2_value]
      
      #the sample sizes for Group 1 and Group 2
      n1 <- length(group1_ranks)
      n2 <- length(group2_ranks)
      n <- n1 + n2
      
      #sum of ranks for Group 1 (R1)
      R1 <- sum(group1_ranks)
      
      #variance of all ranks (VR)
      VR <- var(rank_test$ranks)
      
      #calculating Z using the formula provided in this paper: https://journals.sagepub.com/doi/pdf/10.1177/1536867X1301300208
      Z <- (R1 - (n1 * (n + 1) / 2)) / sqrt(n1 * n2 * VR / n)
      
      #calculating the effect size r based on Tomczak & Tomczak
      effect_size <- Z / sqrt(n)
      
      additional_info <- paste(additional_info, sprintf(", Z = %.3f, r = %.3f", Z, effect_size))
      
    } else if ((group1and2_var %in% group1and2_binary && group3_var %in% group3_nominal) ||
               (group1and2_var %in% group1and2_nominal && group3_var %in% group3_binary) ||
               (group1and2_var %in% group1and2_binary && group3_var %in% group3_binary)  ||
               (group1and2_var %in% group1and2_nominal && group3_var %in% group3_nominal) ||
               (group1and2_var %in% group1and2_nominal && group3_var %in% group3_ordinal) ||
               (group1and2_var %in% group1and2_ordinal && group3_var %in% group3_nominal)){
      # Binary vs Nominal OR Binary vs Binary OR Nominal vs Nominal OR Nominal vs Ordinal: Chi-square test or Fisher's exact test
      table_result <- svytable(as.formula(paste("~", group1and2_var, "+", group3_var)), design_subset)
      chisq_test <- chisq.test(table_result, correct = FALSE, simulate.p.value = TRUE)
      expected_freqs <- chisq_test$expected
      
      expected_freqs_values <- as.numeric(expected_freqs)
      
      #checking the assumptions for chi-square test
      if (nrow(table_result) == 2 && ncol(table_result) == 2) {
        # 2x2 table: all expected frequencies must be at least 5
        if (all(expected_freqs_values >= 5)) {
          p_value <- chisq_test$p.value
          test_name <- "Chi-square Test"
          additional_info <- sprintf("Chi-square statistic = %.3f", chisq_test$statistic)
          
          # Effect size: Phi coefficient for 2x2 table, Cramér's V for larger tables
          effect_size <- sqrt(chisq_test$statistic / length(as.numeric(expected_freqs)))
          additional_info <- paste(additional_info, sprintf(", Effect size (Phi) = %.3f", effect_size))
          
        } else {
          #assumptions not met: Fisher's Exact Test instead
          fisher_result <- fisher.test(table_result, simulate.p.value = TRUE)
          p_value <- fisher_result$p.value
          test_name <- "Fisher's Exact Test"
          additional_info <- sprintf("Odds Ratio = %.3f, Confidence Interval = [%.3f, %.3f]", 
                                     fisher_result$estimate, 
                                     fisher_result$conf.int[1], 
                                     fisher_result$conf.int[2])
          
          #checking if odds ratio is available
          if (!is.null(fisher_result$estimate)) {
            additional_info <- sprintf("Odds Ratio = %.3f, Confidence Interval = [%.3f, %.3f]", 
                                       fisher_result$estimate, 
                                       fisher_result$conf.int[1], 
                                       fisher_result$conf.int[2])
            effect_size <- fisher_result$estimate
          } else {
            additional_info <- "Odds Ratio not available."
            effect_size <- NA
          }
        }
      } else {
        # Larger table: all but one expected frequency >= 5, and the remaining one >= 1
        if (sum(expected_freqs_values < 5) <= 1 && all(expected_freqs_values >= 1)) {
          p_value <- chisq_test$p.value
          test_name <- "Chi-square Test"
          additional_info <- sprintf("Chi-square statistic = %.3f", chisq_test$statistic)
          
          # Effect size: Phi coefficient for 2x2 table, Cramér's V for larger tables
          effect_size <- cramersv(table_result)
          additional_info <- paste(additional_info, sprintf(", Effect size (Cramer's V) = %.3f", effect_size))
          
        } else {
          #assumptions not met: use Fisher's Exact Test (with hybrid option for large tables)
          fisher_result <- fisher.test(table_result, hybrid = TRUE, simulate.p.value = TRUE)
          p_value <- fisher_result$p.value
          test_name <- "Fisher's Exact Test (Hybrid)"
          additional_info <- sprintf("Odds Ratio = %.3f, Confidence Interval = [%.3f, %.3f]", 
                                     fisher_result$estimate, 
                                     fisher_result$conf.int[1], 
                                     fisher_result$conf.int[2])
          #checking if odds ratio is available
          if (!is.null(fisher_result$estimate)) {
            additional_info <- sprintf("Odds Ratio = %.3f, Confidence Interval = [%.3f, %.3f]", 
                                       fisher_result$estimate, 
                                       fisher_result$conf.int[1], 
                                       fisher_result$conf.int[2])
            effect_size <- fisher_result$estimate
          } else {
            additional_info <- "Odds Ratio not available."
            effect_size <- NA
          }
        }
        
      }
    } else if ((group1and2_var %in% group1and2_binary && group3_var %in% group3_continuous) ||
               (group1and2_var %in% group1and2_continuous && group3_var %in% group3_binary)) {
      # Binary vs Continuous: T-test or Wilcoxon test
      
      if (group1and2_var %in% group1and2_binary) {
        normality_test <- shapiro.test(data_subset[[group3_var]])$p.value
        continuous_var <- group3_var
        binary_var <- group1and2_var
      } else {
        normality_test <- shapiro.test(data_subset[[group1and2_var]])$p.value
        continuous_var <- group1and2_var
        binary_var <- group3_var
      }
      if (normality_test > 0.05) {
        t_test <- svyttest(as.formula(paste(continuous_var, "~", binary_var)), design_subset)
        p_value <- t_test$p.value
        test_name <- "T-test"
        additional_info <- sprintf("T-statistic = %.3f", t_test$statistic)
        # Effect size: Cohen's
        group1_design <- subset(design_subset, data_subset[[binary_var]] == unique_values[1])
        group2_design <- subset(design_subset, data_subset[[binary_var]] == unique_values[2])
        
        group1_mean <- svymean(~get(continuous_var), group1_design)
        group2_mean <- svymean(~get(continuous_var), group2_design)
        
        mean_group1 <- coef(group1_mean)
        mean_group2 <- coef(group2_mean)
        
        sd <- svysd(~get(continuous_var), design_subset)
        effect_size <- abs(mean_group1-mean_group2) / sd
        additional_info <- paste(additional_info, sprintf(", Cohen's d = %.3f", effect_size))
      } else {
        rank_test <- svyranktest(as.formula(paste(continuous_var, "~", binary_var)), design_subset, test="wilcoxon")
        p_value <- rank_test$p.value
        test_name <- "Wilcoxon test"
        additional_info <- sprintf("Statistic = %.3f", rank_test$statistic)
        # Effect size: r = Z / sqrt(n) 
        
        group_values <- sort(unique(data_subset[[binary_var]]))
        group1_value <- group_values[1]  #lesser numeric value (Group 1)
        group2_value <- group_values[2]  #greater numeric value (Group 2)
        
        group1_ranks <- rank_test$ranks[data_subset[[binary_var]] == group1_value]
        group2_ranks <- rank_test$ranks[data_subset[[binary_var]] == group2_value]
        
        #calculating the sample sizes for Group 1 and Group 2
        n1 <- length(group1_ranks)
        n2 <- length(group2_ranks)
        n <- n1 + n2
        
        # Sum of ranks for Group 1 (R1)
        R1 <- sum(group1_ranks)
        
        # Variance of all ranks (VR)
        VR <- var(rank_test$ranks)
        
        #calculating Z using the formula provided in this paper: https://journals.sagepub.com/doi/pdf/10.1177/1536867X1301300208
        Z <- (R1 - (n1 * (n + 1) / 2)) / sqrt(n1 * n2 * VR / n)
        
        #calculating the effect size r based on Tomczak & Tomczak
        effect_size <- Z / sqrt(n)
        
        
        additional_info <- paste(additional_info, sprintf(", Z = %.3f, r = %.3f", Z, effect_size))
      }
      gc()
    } else if ((group1and2_var %in% group1and2_nominal && group3_var %in% group3_continuous) ||
               (group1and2_var %in% group1and2_continuous && group3_var %in% group3_nominal)) {
      # Nominal vs Continuous: ANOVA or Kruskal-Wallis
      
      #using the continuous variable as the dependent variable and nominal as the grouping
      if (group1and2_var %in% group1and2_nominal) {
        continuous_var <- group3_var
        nominal_var <- group1and2_var
      } else {
        continuous_var <- group1and2_var
        nominal_var <- group3_var
      }
      
      #normality_test <- shapiro.test(data_subset[[continuous_var]])$p.value
      # I skip performing ANOVA as I checked that all continuous variables deviate significantly from the normal distribution,
      # while making ANOVA test for weighted variables (using survey package) and calculating effect size metric (as suggested 
      # in Tomczak and Tomczak(2014)) is highly complex 
      rank_test <- svyranktest(as.formula(paste(continuous_var, "~", nominal_var)), design_subset, test = "KruskalWallis")
      p_value <- rank_test$p.value
      test_name <- "Kruskal-Wallis Test"
      additional_info <- sprintf("Statistic = %.3f", rank_test$statistic)
      
      # Effect size: Epsilon-squared
      h_stat <- rank_test$statistic
      n <- nrow(data_subset)
      effect_size <- h_stat / (n^2 - 1)/(n+1)
      additional_info <- paste(additional_info, sprintf(", Epsilon-squared = %.3f", effect_size))
      gc()
    } else if ((group1and2_var %in% group1and2_ordinal && group3_var %in% group3_ordinal) || 
               (group1and2_var %in% group1and2_ordinal && group3_var %in% group3_continuous) ||
               (group1and2_var %in% group1and2_continuous && group3_var %in% group3_ordinal)) {
      # Ordinal vs Ordinal OR Ordinal vs Continuous: Weighted Spearman's rank correlation using weightedCorr
      
      spearman_corr <- weightedCorr(data_subset[[group1and2_var]], data_subset[[group3_var]], 
                                    weights = data_subset$useweight, method = "spearman")
      if (abs(spearman_corr) > 0.3) {
        n <- nrow(data_subset)
        z_value <- spearman_corr * sqrt(n - 1) #as sample is bigger than 30
        p_value <- 2 * (q=pnorm(abs(z_value), lower.tail=FALSE))  
        test_name <- "Spearman's Rank Correlation"
        additional_info <- sprintf("Spearman's rho = %.3f, Z = %.3f", spearman_corr, z_value)
        effect_size <- spearman_corr
      } else {
        return()
      }
      gc()
    } else if (group1and2_var %in% group1and2_continuous && group3_var %in% group3_continuous) {
      # Continuous vs Continuous: Pearson's or Spearman's correlation
      normality_test1 <- shapiro.test(data_subset[[group1and2_var]])$p.value
      normality_test2 <- shapiro.test(data_subset[[group3_var]])$p.value
      if (normality_test1 > 0.05 && normality_test2 > 0.05) {
        cor_test <- svycor(~data_subset[[group1and2_var]] + data_subset[[group3_var]], design_subset)
        correlation_matrix <- cor_test[[1]]  
        correlation_value <- correlation_matrix[1, 2] 
        
        if (abs(correlation_value) > 0.3) {
          p_value <- cor_test$p.value
          test_name <- "Pearson's Correlation"
          additional_info <- sprintf("Pearson's r = %.3f", correlation_value)
          effect_size <- correlation_value
        } else {
          return()
        }
        
        
      } else {
        # Spearman's rank correlation for non-normal data
        spearman_corr <- weightedCorr(data_subset[[group1and2_var]], data_subset[[group3_var]], 
                                      weights = data_subset$useweight, method = "spearman")
        if (abs(spearman_corr) > 0.3) {
          n <- nrow(data_subset)
          z_value <- spearman_corr * sqrt(n - 1) #as sample is bigger than 30
          p_value <- 2 * (q=pnorm(abs(z_value), lower.tail=FALSE))  
          test_name <- "Spearman's Rank Correlation"
          additional_info <- sprintf("Spearman's rho = %.3f, Z = %.3f", spearman_corr, z_value)
          effect_size <- spearman_corr
        } else {
          return()
        }
      }
      gc()
    }
    

    if (p_value < 0.05) {
      cat(sprintf("%s for %s vs. %s: p-value = %.3f", test_name, group1and2_var, group3_var, p_value))
      if (!is.null(additional_info)) {
        cat(sprintf(", %s", additional_info))
      }
      cat("\n")
      
      new_row <- data.frame(
        Variable1 = group1and2_var,
        Variable2 = group3_var,
        TestName = test_name,
        PValue = p_value,
        EffectSize = effect_size,
        stringsAsFactors = FALSE
      )
      results_df <<- rbind(results_df, new_row)
      
      
    }
  }, error = function(e) {
    cat(sprintf("Error in %s vs. %s: %s\n", group1and2_var, group3_var, e$message))
  })
}

#looping through Group 1 and 2 variables and test against Group 3 variables
for (group1and2_var in c(group1and2_binary, group1and2_nominal, group1and2_ordinal, group1and2_continuous)) {
  for (group3_var in c(group3_binary, group3_ordinal, group3_nominal, group3_continuous)) {
    test_association_pairwise(group1and2_var, group3_var)
  }
}

#removing exact duplicates
results_df <- results_df[!duplicated(results_df), ]

results_df$Combination <- apply(results_df[, c("Variable1", "Variable2")], 1, function(x) paste(sort(x), collapse = "_"))

#removing reverse duplicates based on the Combination
results_df <- results_df[!duplicated(results_df[, c("Combination")]), ]


results_df <- select(results_df, -c("Combination"))

nrow(results_df)

results_df <- results_df[order(results_df$TestName, results_df$EffectSize), ]

print(results_df)

general_associations <- results_df


#####################################################################################
######### weighted correlations for missingness one variable by one variable ########
#####################################################################################


data <- read_dta("C:/Users/sierp/OneDrive/CHARM/Data/processed_data_05_08_2025.dta")

data$K69_aggreg[data$K69_aggreg == ""] <- NA

freq(data$K69_aggreg)

#creating binary indicators for missingness for Group 1 and Group 2 variables
group1_vars <- c("K03", "K05", "K07", "K09", "K1001", "K1002", "K1003", "K1101", "K1102",
                 "K1103", "K1104", "K1105", "K14", "K1509", 
                 "K1601", "K1602", "K1801", "K1802", "K1803", "K1804", "K1805", "K1806", 
                 "K1807", "K1808", "K1901", "K1902", "K1903", "K1904", "K2001", "K2002", 
                 "K2003", "K2004", "K2005", "K2006", "K2007", "K23", "K25", "K2801", 
                 "K2802", "K2803", "K2901", "K2902", "K2903", "K2904", "K3001", "K3002", 
                 "K3003", "K3004", "K3005", "K3006", "K3007", "K3008", "K3801", "K3802", 
                 "K3803", "K3901", "K3902", "K3903", "K3904", "K3905", "K3906", "K4301", 
                 "K4302", "K4303", "K4304", "K4305", "K4306", "K4801", "K4802", "K4803", 
                 "K4804", "K4805", "K4806", "K4807", "K49", "K5701", "K5705", "K58", "K5901", 
                 "K5902", "K5903", "K5904", "K5905", "K5906", "K5907", "K5908", "K5909", 
                 "K5910", "K5911", "K5912", "K66_aggreg_num", "K69_aggreg")

group2_vars <- c("K2401", "K2402", "K2403", "K2404", "K2405", "K2406", "K2407", 
                 "K2408", "K2409", "K2410", "K01", "K63", "K65")


#transforming to binary indicators for missingness
data_group1_missing <- data %>%
  select(all_of(group1_vars)) %>%
  mutate(across(everything(), ~ ifelse(is.na(.), 0, 1)))

data_group2_missing <- data %>%
  select(all_of(group2_vars)) %>%
  mutate(across(everything(), ~ ifelse(is.na(.), 0, 1)))

data <- data %>%
  select(-all_of(group1_vars), -all_of(group2_vars))

data_missingness <- cbind(data, data_group1_missing, data_group2_missing)


# Group 3 variables

group3_binary <- c()
group3_ordinal <- c() 
#                    "K13", "K1501", "K1502", "K1504", "K1506", 
#                    "K1508", "K1510", "K1511", "K1512", "K31", "K32", "K34", 
#                    "K35", "K3701", "K3702", "K3705", "K3706", 
#                    "K3707", "K3708", "K7301", "K7304", "K7305", "K7306", 
#                    "K7307", "K7308")
#group3_nominal <- c("K33", "K5601")
#group3_continuous <- c("Averageage2019HE","Averageincome2019HR")

group3_continuous <- c("ADHL2019KO_ratio", "ADLL2019KO_ratio",   
                       "Averageincome2019HR", "BLS2019KO_ratio", "Eikotimaankieliset_ratio", 
                       "Employed2019PT_ratio",   
                       "Householdshighestincome2019TR", "Householdslowestincome2019TR", 
                       "HOD2019TE_ratio", "HSmC2019TE_ratio", 
                       "Occupancyrate2019TE", "OPH2019TE_ratio", 
                       "PH2019TE_ratio", 
                       "Students2019PT_ratio", "Unemployed2019PT_ratio", "VD2019KO_ratio")
group3_nominal <- c("Kuntaryhmännimi", "Suuralueennimi")


results_df <- data.frame(
  Variable1 = character(),
  Variable2 = character(),
  TestName = character(),
  PValue = numeric(),
  EffectSize = numeric(),
  stringsAsFactors = FALSE
)


# Function to perform association tests with pairwise deletion and unified output
test_association_pairwise <- function(group_var, test_var) {
  p_value <- NULL
  test_name <- NULL
  additional_info <- NULL
  effect_size <- NULL
  
  #pairwise deletion:
  data_subset <- data_missingness[complete.cases(data_missingness[[group_var]], data_missingness[[test_var]]), ]
  

  design_subset <- svydesign(ids = ~1, data = data_subset, weights = ~useweight)
  
  tryCatch({
    if (test_var %in% group3_binary) {
      # Binary vs Binary: First try Chi-square, fallback to Fisher's exact test if assumptions not met
      table_result <- svytable(as.formula(paste("~", group_var, "+", test_var)), design_subset)
      
      #expected frequencies
      chisq_test <- chisq.test(table_result, correct = FALSE, simulate.p.value = TRUE)
      expected_freqs <- chisq_test$expected
      
      expected_freqs_values <- as.numeric(expected_freqs)
      
      if (nrow(table_result) == 2 && ncol(table_result) == 2) {
        # 2x2 table: all expected frequencies must be at least 5
        if (all(expected_freqs_values >= 5)) {
          p_value <- chisq_test$p.value
          test_name <- "Chi-square Test"
          additional_info <- sprintf("Chi-square statistic = %.3f", chisq_test$statistic)
          
          # Effect size: Phi coefficient for 2x2 table, Cramér's V for larger tables
          effect_size <- sqrt(chisq_test$statistic / length(as.numeric(expected_freqs)))
          additional_info <- paste(additional_info, sprintf(", Effect size (Phi) = %.3f", effect_size))
          
        } else {
          #assumptions not met: Fisher's Exact Test instead
          fisher_result <- fisher.test(table_result, simulate.p.value=TRUE)
          p_value <- fisher_result$p.value
          test_name <- "Fisher's Exact Test"
          additional_info <- sprintf("Odds Ratio = %.3f, Confidence Interval = [%.3f, %.3f]", 
                                     fisher_result$estimate, 
                                     fisher_result$conf.int[1], 
                                     fisher_result$conf.int[2])
          #checking if odds ratio is available
          if (!is.null(fisher_result$estimate)) {
            additional_info <- sprintf("Odds Ratio = %.3f, Confidence Interval = [%.3f, %.3f]", 
                                       fisher_result$estimate, 
                                       fisher_result$conf.int[1], 
                                       fisher_result$conf.int[2])
            effect_size <- fisher_result$estimate
          } else {
            additional_info <- "Odds Ratio not available."
            effect_size <- NA
          }
        }
      } else {
        cat(sprintf("Unexpected table size for %s vs. %s. Skipping this test.\n", group_var, test_var))
        return()
      }
      
    } else if (test_var %in% group3_ordinal) {
      # Binary vs Ordinal: Wilcoxon
      
      rank_test <- svyranktest(as.formula(paste(test_var, "~", group_var)), design_subset, test="wilcoxon")
      p_value <- rank_test$p.value
      test_name <- "Wilcoxon Test"
      additional_info <- sprintf("Statistic = %.3f, df = %.3f", rank_test$statistic, rank_test$parameter)
      
      # Effect size: r = Z / sqrt(n)
      
      group_values <- sort(unique(data_subset[[group_var]]))
      group1_value <- group_values[1]  #lesser numeric value (Group 1)
      group2_value <- group_values[2]  #greater numeric value (Group 2)
      
      group1_ranks <- rank_test$ranks[data_subset[[group_var]] == group1_value]
      group2_ranks <- rank_test$ranks[data_subset[[group_var]] == group2_value]
      
      #sample sizes for Group 1 and Group 2
      n1 <- length(group1_ranks)
      n2 <- length(group2_ranks)
      n <- n1 + n2  
      
      # Sum of ranks for Group 1 (R1)
      R1 <- sum(group1_ranks)
      
      # Variance of all ranks (VR)
      VR <- var(rank_test$ranks)
      
      #calculating Z using the formula provided in this paper: https://journals.sagepub.com/doi/pdf/10.1177/1536867X1301300208
      Z <- (R1 - (n1 * (n + 1) / 2)) / sqrt(n1 * n2 * VR / n)
      
      #calculating the effect size r based on Tomczak & Tomczak
      effect_size <- Z / sqrt(n)
      
      
      additional_info <- paste(additional_info, sprintf(", Z = %.3f, r = %.3f", Z, effect_size))
      
    } else if (test_var %in% group3_nominal) {
      # Binary vs Nominal: Chi-squared test
      table_result <- svytable(as.formula(paste("~", group_var, "+", test_var)), design_subset)
      
      chisq_test <- chisq.test(table_result, correct = FALSE, simulate.p.value = FALSE)
      expected_freqs <- chisq_test$expected
      
      expected_freqs_values <- as.numeric(expected_freqs)
      
      #checking the assumptions for chi-square test
      if (nrow(table_result) == 2 && ncol(table_result) == 2) {
        # 2x2 table: all expected frequencies must be at least 5
        if (all(expected_freqs_values >= 5)) {
          p_value <- chisq_test$p.value
          test_name <- "Chi-square Test"
          additional_info <- sprintf("Chi-square statistic = %.3f", chisq_test$statistic)
          # Effect size: Phi coefficient for 2x2 table, Cramér's V for larger tables
          effect_size <- sqrt(chisq_test$statistic / length(as.numeric(expected_freqs)))
          additional_info <- paste(additional_info, sprintf(", Effect size (Phi) = %.3f", effect_size))
        } else {
          #assumptions not met: Fisher's Exact Test instead
          fisher_result <- fisher.test(table_result)
          p_value <- fisher_result$p.value
          test_name <- "Fisher's Exact Test"
          additional_info <- sprintf("Odds Ratio = %.3f, Confidence Interval = [%.3f, %.3f]", 
                                     fisher_result$estimate, 
                                     fisher_result$conf.int[1], 
                                     fisher_result$conf.int[2])
          #checking if odds ratio is available
          if (!is.null(fisher_result$estimate)) {
            additional_info <- sprintf("Odds Ratio = %.3f, Confidence Interval = [%.3f, %.3f]", 
                                       fisher_result$estimate, 
                                       fisher_result$conf.int[1], 
                                       fisher_result$conf.int[2])
            effect_size <- fisher_result$estimate
          } else {
            additional_info <- "Odds Ratio not available."
            effect_size <- NA
          }
        }
      } else {
        # Larger table: all but one expected frequency >= 5, and the remaining one >= 1
        if (sum(expected_freqs_values < 5) <= 1 && all(expected_freqs_values >= 1)) {
          p_value <- chisq_test$p.value
          test_name <- "Chi-square Test"
          additional_info <- sprintf("Chi-square statistic = %.3f", chisq_test$statistic)
          # Effect size: Phi coefficient for 2x2 table, Cramér's V for larger tables
          effect_size <- cramersv(table_result)
          additional_info <- paste(additional_info, sprintf(", Effect size (Cramer's V) = %.3f", effect_size))
        } else {
          #assumptions not met: use Fisher's Exact Test (with hybrid option for large tables)
          fisher_result <- fisher.test(table_result, hybrid = TRUE)
          p_value <- fisher_result$p.value
          test_name <- "Fisher's Exact Test (Hybrid)"
          additional_info <- sprintf("Odds Ratio = %.3f, Confidence Interval = [%.3f, %.3f]", 
                                     fisher_result$estimate, 
                                     fisher_result$conf.int[1], 
                                     fisher_result$conf.int[2])
          #checking if odds ratio is available
          if (!is.null(fisher_result$estimate)) {
            additional_info <- sprintf("Odds Ratio = %.3f, Confidence Interval = [%.3f, %.3f]", 
                                       fisher_result$estimate, 
                                       fisher_result$conf.int[1], 
                                       fisher_result$conf.int[2])
            effect_size <- fisher_result$estimate
          } else {
            additional_info <- "Odds Ratio not available."
            effect_size <- NA
          }
        }
      }
      
    } else if (test_var %in% group3_continuous) {
      # Binary vs Continuous: Independent-samples t-test / Wilcoxon test
      normality_test <- shapiro.test(data_subset[[test_var]])$p.value
      if (normality_test > 0.05) {
        # If normal, t-test
        t_test <- svyttest(as.formula(paste(test_var, "~", group_var)), design_subset)
        p_value <- t_test$p.value
        test_name <- "T-test"
        additional_info <- sprintf("T-statistic = %.3f", t_test$statistic)
        
        # Effect size: Cohen's

        group1_design <- subset(design_subset, data_subset[[group_var]] == unique_values[1])
        group2_design <- subset(design_subset, data_subset[[group_var]] == unique_values[2])

        group1_mean <- svymean(~get(test_var), group1_design)
        group2_mean <- svymean(~get(test_var), group2_design)

        mean_group1 <- coef(group1_mean)
        mean_group2 <- coef(group2_mean)
        
        sd <- svysd(~get(test_var), design_subset)
        effect_size <- abs(mean_group1-mean_group2) / sd
        additional_info <- paste(additional_info, sprintf(", Cohen's d = %.3f", effect_size))
        
      } else {
        # If not normal, Wilcoxon test
        rank_test <- svyranktest(as.formula(paste(test_var, "~", group_var)), design_subset, test="wilcoxon")
        p_value <- rank_test$p.value
        test_name <- "Wilcoxon test"
        additional_info <- sprintf("Statistic = %.3f, df = %.3f", rank_test$statistic, rank_test$parameter)
        # Effect size: r = Z / sqrt(n) # do sprawdzenia to be checked

        group_values <- sort(unique(data_subset[[group_var]]))
        group1_value <- group_values[1]  #lesser numeric value (Group 1)
        group2_value <- group_values[2]  #greater numeric value (Group 2)
        
        group1_ranks <- rank_test$ranks[data_subset[[group_var]] == group1_value]
        group2_ranks <- rank_test$ranks[data_subset[[group_var]] == group2_value]
        
        #calculating the sample sizes for Group 1 and Group 2
        n1 <- length(group1_ranks)
        n2 <- length(group2_ranks)
        n <- n1 + n2  
        
        # Sum of ranks for Group 1 (R1)
        R1 <- sum(group1_ranks)
        
        # Variance of all ranks (VR)
        VR <- var(rank_test$ranks)
        
        #calculating Z using the formula provided in this paper: https://journals.sagepub.com/doi/pdf/10.1177/1536867X1301300208
        Z <- (R1 - (n1 * (n + 1) / 2)) / sqrt(n1 * n2 * VR / n)
        
        #calculating the effect size r based on Tomczak & Tomczak
        effect_size <- Z / sqrt(n)
        
        
        additional_info <- paste(additional_info, sprintf(", Z = %.3f, r = %.3f", Z, effect_size))
      }
    }
    

    if (!is.null(p_value) && p_value < 0.05) {
      cat(sprintf("%s for %s vs. %s: p-value = %.3f", test_name, group_var, test_var, p_value))
      if (!is.null(additional_info)) {
        cat(sprintf(", %s", additional_info))
      }
      cat("\n")

      new_row <- data.frame(
        Variable1 = group_var,
        Variable2 = test_var,
        TestName = test_name,
        PValue = p_value,
        EffectSize = effect_size,
        stringsAsFactors = FALSE
      )
      results_df <<- rbind(results_df, new_row)
    }
  }, error = function(e) {
    cat(sprintf("Error in %s vs. %s: %s\n", group_var, test_var, e$message))
  })
}


#looping through Group 1 and Group 2 variables and test against Group 3 variables
for (group_var in c(group1_vars, group2_vars)) {
  for (test_var in c(group3_binary, group3_ordinal, group3_nominal, group3_continuous)) {
    test_association_pairwise(group_var, test_var)
  }
}

nrow(results_df)
#removin exact duplicates
results_df <- results_df[!duplicated(results_df), ]
nrow(results_df)
results_df$Combination <- apply(results_df[, c("Variable1", "Variable2")], 1, function(x) paste(sort(x), collapse = "_"))

results_df <- results_df[!duplicated(results_df[, c("Combination")]), ]

results_df <- select(results_df, -c("Combination"))

nrow(results_df)

results_df <- results_df[order(results_df$TestName, results_df$EffectSize), ]

missingness_associations<-results_df

missingness_associations$Type <- "Missingness"
general_associations$Type <- "General"


all_associations <- rbind(general_associations, missingness_associations)


############### Selecting auxiliary variables to MI #####################

filtered_rows<-all_associations[grepl("K03", all_associations$Variable1), ]
# "OPH2019TE_ratio"
filtered_rows<-all_associations[grepl("K05", all_associations$Variable1), ]
# Eikotimaankieliset_ratio
filtered_rows<-all_associations[grepl("K07", all_associations$Variable1), ]
# None
filtered_rows<-all_associations[grepl("K09", all_associations$Variable1), ]
# None
filtered_rows<-all_associations[grepl("K1001", all_associations$Variable1), ]
# None
filtered_rows<-all_associations[grepl("K1002", all_associations$Variable1), ]
# Suuralueennimi
filtered_rows<-all_associations[grepl("K1003", all_associations$Variable1), ]
# None
filtered_rows<-all_associations[grepl("K1101", all_associations$Variable1), ]
# None
filtered_rows<-all_associations[grepl("K1102", all_associations$Variable1), ]
# None
filtered_rows<-all_associations[grepl("K1103", all_associations$Variable1), ]
# None
filtered_rows<-all_associations[grepl("K1104", all_associations$Variable1), ]
# None
filtered_rows<-all_associations[grepl("K1105", all_associations$Variable1), ]
# None
filtered_rows<-all_associations[grepl("K14", all_associations$Variable1), ]
# None 
filtered_rows<-all_associations[grepl("K1509", all_associations$Variable1), ]
#K5601
filtered_rows<-all_associations[grepl("K1601", all_associations$Variable1), ]
# None
filtered_rows<-all_associations[grepl("K1602", all_associations$Variable1), ]
# None
filtered_rows<-all_associations[grepl("K1801", all_associations$Variable1), ]
# None
filtered_rows<-all_associations[grepl("K1802", all_associations$Variable1), ]
# None
filtered_rows<-all_associations[grepl("K1803", all_associations$Variable1), ]
# None
filtered_rows<-all_associations[grepl("K1804", all_associations$Variable1), ]
# None
filtered_rows<-all_associations[grepl("K1805", all_associations$Variable1), ]
# "HOD2019TE_ratio" "Householdslowestincome2019TR"
filtered_rows<-all_associations[grepl("K1806", all_associations$Variable1), ]
# None
filtered_rows<-all_associations[grepl("K1807", all_associations$Variable1), ]
# None
filtered_rows<-all_associations[grepl("K1808", all_associations$Variable1), ]
# None
filtered_rows<-all_associations[grepl("K1901", all_associations$Variable1), ]
# None
filtered_rows<-all_associations[grepl("K1902", all_associations$Variable1), ]
# None
filtered_rows<-all_associations[grepl("K1903", all_associations$Variable1), ]
# None
filtered_rows<-all_associations[grepl("K1904", all_associations$Variable1), ]
# None
filtered_rows<-all_associations[grepl("K2001", all_associations$Variable1), ]
# None
filtered_rows<-all_associations[grepl("K2002", all_associations$Variable1), ]
# None
filtered_rows<-all_associations[grepl("K2003", all_associations$Variable1), ]
# None
filtered_rows<-all_associations[grepl("K2004", all_associations$Variable1), ]
# None
filtered_rows<-all_associations[grepl("K2005", all_associations$Variable1), ]
# None
filtered_rows<-all_associations[grepl("K2006", all_associations$Variable1), ]
# Suuralueennimi
filtered_rows<-all_associations[grepl("K2007", all_associations$Variable1), ]
# None
filtered_rows<-all_associations[grepl("K23", all_associations$Variable1), ]
# None
filtered_rows<-all_associations[grepl("K25", all_associations$Variable1), ]
# None
filtered_rows<-all_associations[grepl("K2801", all_associations$Variable1), ]
# None
filtered_rows<-all_associations[grepl("K2802", all_associations$Variable1), ]
# None
filtered_rows<-all_associations[grepl("K2803", all_associations$Variable1), ]
# None
filtered_rows<-all_associations[grepl("K2901", all_associations$Variable1), ]
# None
filtered_rows<-all_associations[grepl("K2902", all_associations$Variable1), ]
# None
filtered_rows<-all_associations[grepl("K2903", all_associations$Variable1), ]
# None
filtered_rows<-all_associations[grepl("K2904", all_associations$Variable1), ]
# None
filtered_rows<-all_associations[grepl("K3001", all_associations$Variable1), ]
# None
filtered_rows<-all_associations[grepl("K3002", all_associations$Variable1), ]
# None
filtered_rows<-all_associations[grepl("K3003", all_associations$Variable1), ]
# None
filtered_rows<-all_associations[grepl("K3004", all_associations$Variable1), ]
# None
filtered_rows<-all_associations[grepl("K3005", all_associations$Variable1), ]
# None
filtered_rows<-all_associations[grepl("K3006", all_associations$Variable1), ]
# None
filtered_rows<-all_associations[grepl("K3007", all_associations$Variable1), ]
# None
filtered_rows<-all_associations[grepl("K3008", all_associations$Variable1), ]
# None
filtered_rows<-all_associations[grepl("K3801", all_associations$Variable1), ]
# None
filtered_rows<-all_associations[grepl("K3802", all_associations$Variable1), ]
# None
filtered_rows<-all_associations[grepl("K3803", all_associations$Variable1), ]
# HOD2019TE_ratio
filtered_rows<-all_associations[grepl("K3901", all_associations$Variable1), ]
# None
filtered_rows<-all_associations[grepl("K3902", all_associations$Variable1), ]
# HOD2019TE_ratio
filtered_rows<-all_associations[grepl("K3903", all_associations$Variable1), ]
# None
filtered_rows<-all_associations[grepl("K3904", all_associations$Variable1), ]
# OPH2019TE_ratio
filtered_rows<-all_associations[grepl("K3905", all_associations$Variable1), ]
# HOD2019TE_ratio
filtered_rows<-all_associations[grepl("K3906", all_associations$Variable1), ]
# None
filtered_rows<-all_associations[grepl("K4301", all_associations$Variable1), ]
# Householdslowestincome2019TR
filtered_rows<-all_associations[grepl("K4302", all_associations$Variable1), ]
#None
filtered_rows<-all_associations[grepl("K4303", all_associations$Variable1), ]
# None
filtered_rows<-all_associations[grepl("K4304", all_associations$Variable1), ]
#"Suuralueennimi" "Kuntaryhmännimi" "Eikotimaankieliset_ratio" "PH2019TE_ratio" "VD2019KO_ratio" "Occupancyrate2019TE" "HOD2019TE_ratio"   
filtered_rows<-all_associations[grepl("K4305", all_associations$Variable1), ]
# None
filtered_rows<-all_associations[grepl("K4306", all_associations$Variable1), ]
# None 
filtered_rows<-all_associations[grepl("K4801", all_associations$Variable1), ]
# None
filtered_rows<-all_associations[grepl("K4802", all_associations$Variable1), ]
# None
filtered_rows<-all_associations[grepl("K4803", all_associations$Variable1), ]
# None
filtered_rows<-all_associations[grepl("K4804", all_associations$Variable1), ]
# None
filtered_rows<-all_associations[grepl("K4805", all_associations$Variable1), ]
# None
filtered_rows<-all_associations[grepl("K4806", all_associations$Variable1), ]
# None
filtered_rows<-all_associations[grepl("K4807", all_associations$Variable1), ]
# None
filtered_rows<-all_associations[grepl("K49", all_associations$Variable1), ]
# None
filtered_rows<-all_associations[grepl("K5701", all_associations$Variable1), ]
# "HOD2019TE_ratio" "Householdslowestincome2019TR" 
filtered_rows<-all_associations[grepl("K5705", all_associations$Variable1), ]
# HSmC2019TE_ratio
filtered_rows<-all_associations[grepl("K58", all_associations$Variable1), ]
#None
filtered_rows<-all_associations[grepl("K5901", all_associations$Variable1), ]
# None
filtered_rows<-all_associations[grepl("K5902", all_associations$Variable1), ]
# None
filtered_rows<-all_associations[grepl("K5903", all_associations$Variable1), ]
# None
filtered_rows<-all_associations[grepl("K5904", all_associations$Variable1), ]
# Householdslowestincome2019TR
filtered_rows<-all_associations[grepl("K5905", all_associations$Variable1), ]
# None
filtered_rows<-all_associations[grepl("K5906", all_associations$Variable1), ]
# "OPH2019TE_ratio" "Householdslowestincome2019TR"
filtered_rows<-all_associations[grepl("K5907", all_associations$Variable1), ]
# None
filtered_rows<-all_associations[grepl("K5908", all_associations$Variable1), ]
# None
filtered_rows<-all_associations[grepl("K5909", all_associations$Variable1), ]
# None
filtered_rows<-all_associations[grepl("K5910", all_associations$Variable1), ]
# None
filtered_rows<-all_associations[grepl("K5911", all_associations$Variable1), ]
# None
filtered_rows<-all_associations[grepl("K5912", all_associations$Variable1), ]
# None
filtered_rows<-all_associations[grepl("K66_aggreg_num", all_associations$Variable1), ]
# None
filtered_rows<-all_associations[grepl("K69_aggreg", all_associations$Variable1), ]
# None



filtered_rows<-all_associations[grepl("K2401", all_associations$Variable1), ]
# None
filtered_rows<-all_associations[grepl("K2402", all_associations$Variable1), ]
# None
filtered_rows<-all_associations[grepl("K2403", all_associations$Variable1), ]
# None
filtered_rows<-all_associations[grepl("K2404", all_associations$Variable1), ]
# None
filtered_rows<-all_associations[grepl("K2405", all_associations$Variable1), ]
# PH2019TE_ratio
filtered_rows<-all_associations[grepl("K2406", all_associations$Variable1), ]
# None
filtered_rows<-all_associations[grepl("K2407", all_associations$Variable1), ]
# None
filtered_rows<-all_associations[grepl("K2408", all_associations$Variable1), ]
# None
filtered_rows<-all_associations[grepl("K2409", all_associations$Variable1), ]
# None
filtered_rows<-all_associations[grepl("K2410", all_associations$Variable1), ]
# None
filtered_rows<-all_associations[grepl("K01", all_associations$Variable1), ]
#None
filtered_rows<-all_associations[grepl("K63", all_associations$Variable1), ]
# None
filtered_rows<-all_associations[grepl("K65", all_associations$Variable1), ]
# "Eikotimaankieliset_ratio" "Occupancyrate2019TE"  


filtered_rows %>%
  group_by(Variable2) %>%
  filter(n_distinct(Type) > 1) %>%
  pull(Variable2) %>%
  unique()



# So, I identified a few predictors for the multiple imputation:

#K03 = OPH2019TE_ratio
#K05 = Eikotimaankieliset_ratio
#K1002 = Suuralueennimi
#K1805 = HOD2019TE_ratio Householdslowestincome2019TR
#K2006 = Suuralueennimi
#K3803 = HOD2019TE_ratio
#K3902 = HOD2019TE_ratio
#K3904 = OPH2019TE_ratio
#K3905 = HOD2019TE_ratio
#K4301 = Householdslowestincome2019TR
#K4304 = Suuralueennimi Kuntaryhmännimi Eikotimaankieliset_ratio PH2019TE_ratio VD2019KO_ratio Occupancyrate2019TE HOD2019TE_ratio 
#K5701 = HOD2019TE_ratio Householdslowestincome2019TR 
#K5705 = HSmC2019TE_ratio
#K5904 = Householdslowestincome2019TR
#K5906 = OPH2019TE_ratio Householdslowestincome2019TR

#K2405 = PH2019TE_ratio
#K65 = Eikotimaankieliset_ratio Occupancyrate2019TE

