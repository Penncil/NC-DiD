### analysis for DiD paper

library(dplyr)
library(glmnet)
library(MatchIt)
library(EmpiricalCalibration)

source("source_functions_did_0925.R")
#to illustrate the analysis and perform codes for peer review, 
#we create a synthetic data, 
data = read.csv("demo_data.csv")

severe_c <- c("severe", "nonsevere")
severe_o <- c('_s', '_ns')
eth_c <- c("NHB", "Hispanic", "AAPI")
eth_o <- c('black', 'hispanic', 'aapi')
ratio_list <- c(2:5)

###################################################################################

###         Statistical  analysis          ### 

###################################################################################

for (i in 1:length(severe_c)){
  for (j in 1:length(eth_c)){
    mydata <- data %>% filter(eth_cat %in% c('NHW', eth_c[j]),severe%in%severe_c[i])
    mydata_medical <- mydata%>%dplyr::select(starts_with('medical_'))
    # low incidence chronic conditions
    if (i==1){
      cond1 <- colSums(mydata_medical)<0.01*nrow(mydata_medical)
    }else{
      cond1 <- colSums(mydata_medical)<0.001*nrow(mydata_medical)
    }
    mydata <- mydata %>% dplyr::select(-colnames(mydata_medical)[cond1])
    rm(mydata_medical)
    
    for(k in 1:ncol(mydata)){
      if(is.character(mydata[,k])){
        mydata[,k] =as.factor(mydata[,k])
      }
    }
    
    mydata$eth_cat=relevel(mydata$eth_cat, ref = "NHW")
    
    xvars = c("age_cat", "sex", "cohort_entry_month", "site", "obese",
              "pmca_index", "n_ed","n_inpatient","n_outpatient", "n_tests",
              "n_dosage","imm_date_diff_grp",
              colnames(mydata%>%dplyr::select(starts_with('medical_'))))
    
    f.all=as.formula(paste0("eth_cat~", paste0(xvars, collapse="+")))
    
    ###################################################################################
    ######          PS model            #######
    ###################################################################################
    set.seed(1)
    
    for (ratio in ratio_list){
      m.out3=matchit(f.all,
                     data=mydata,
                     caliper =0.2,
                     method="nearest",
                     distance="lasso",
                     ratio=ratio,
                     verbose=FALSE,
                     estimand="ATT",
                     distance.options = list(s="lambda.min"))
      
      ## SMD
      s3=summary(m.out3)
      tmp <- s3$sum.matched[,3]
      tmp <- tmp[-1] # delete distance
      criteria_num <- tmp[abs(tmp)>0.1]
      if (length(criteria_num)==0){break}
    }
    cat(eth_o[j], severe_o[i],' ratio=',ratio,'\n')
    
    ###################################################################################
    ######          Fit for outcomes and NCO            #######
    ###################################################################################
    
    m3=match.data(m.out3)
    
    ### original outcomes
    names_outcome <- colnames(data%>%dplyr::select(starts_with("visits_")))
    names_outcome_pre <- colnames(data%>%dplyr::select(starts_with("pre_visits_")))
    
    results_RR=find_did(m3, tolower(names_outcome))
    results_RR=na.omit(results_RR)
    
    ### NCO
    names_outcome <- colnames(data%>%dplyr::select(starts_with("nco_")))
    names_outcome_pre <- colnames(data%>%dplyr::select(starts_with("pre_nco_")))
    
    results_RR_nco=find_did_nco(m3, tolower(names_outcome))
    results_RR_nco=na.omit(results_RR_nco)
    
    rm(m3)
    
    ###################################################################################
    ######          NCO Calibration            #######
    ###################################################################################
    
    p.nc <- computeTraditionalP( results_RR_nco[,1],results_RR_nco[,2])
    fitnull <- fitNull( results_RR_nco[,1], results_RR_nco[,2])
    model <- fitSystematicErrorModel(results_RR_nco[,1],results_RR_nco[,2], rep(0, length(results_RR_nco[,1])))
    res.cal <- calibrateConfidenceInterval(logRr = results_RR[,1],
                                           seLogRr= results_RR[,2],
                                           model, ciWidth = 0.95)
    colnames(res.cal) <- c("RR", "ll", "ul", "seRR")
    
    res.cal.final <- data.frame(res.cal)
    #write.csv(res.cal.final,file=paste0(eth_o[j],severe_o[i],"_calibrate_RR.csv"),row.names = FALSE)
  }
}
