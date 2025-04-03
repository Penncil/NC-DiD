### main code, NC-DiD

### install packages
library(janitor)
library(MatchIt)
library(sandwich)
library(EmpiricalCalibration)

source("source_nc_did.R")

### readin data
data <- read.csv("demo_data.csv")

### original outcomes
names_outcome <- colnames(data%>%select(starts_with("visits_")))
names_outcome_pre <- colnames(data%>%select(starts_with("pre_visits_")))

names_outcome_nco <- grep("^nco_", names(data), value = TRUE)
names_outcome_pre_nco <- grep("^new_nco_", names(data), value = TRUE)

eth_c <- c('AAPI', 'Hispanic', 'NHB')
eth_o <- c('asian', 'hispanic', 'black')

for (j in 1:length(eth_c)){
  mydata <- data %>% filter(eth_cat %in% c('NHW', eth_c[j]))
  
  # propensity score matching
  xvars = c("site", "age_cat", "sex", "cohort_entry_month", "obese",
            "n_ed","n_inpatient","n_outpatient","n_dosage", "imm_date_diff_grp",
            "pmca_index", colnames(mydata%>%dplyr::select(starts_with('medical_'))))
  
  mydata <- mydata %>% mutate(treatment = case_when(eth_cat=="NHW"~0, TRUE~1))
  f.all = as.formula(paste0("treatment~", paste0(xvars, collapse="+")))
  set.seed(1)
  m.out3 = matchit(f.all, 
                 data=mydata,
                 caliper =0.2,
                 method="nearest",
                 distance = "glm",
                 ratio=5,
                 verbose=FALSE,
                 estimand="ATT",
                 distance.options = list(s="lambda.min"))
  m3 = match.data(m.out3)
  
  # outcome model, outcomes of interest
  df_rslt_rr <- data.frame()
  for (m in 1:length(names_outcome)){
    rslt_rr = find_did_RR_robust(m3, names_outcome[m])
    df_rslt_rr <- rbind(df_rslt_rr, rslt_rr)
  }
  
  # outcome model, NCO
  df_rslt_rr_nco <- data.frame()
  for (m in 1:length(names_outcome_nco)){
    rslt_rr_nco=find_did_RR_robust(m3, tolower(names_outcome_nco[m]))
    df_rslt_rr_nco <- rbind(df_rslt_rr_nco, rslt_rr_nco)
  }
  
  # NCO calibration
  fitnull <- fitNull(df_rslt_rr_nco$logRR, df_rslt_rr_nco$seLogRR)
  bias_se = sqrt(1/(sum(1/(fitnull[2]^2 + df_rslt_rr_nco$seLogRR^2))))
  bias_p = 2*min(pnorm(fitnull[1]/bias_se), pnorm(-fitnull[1]/bias_se))
  
  res.cal <- data.frame(outcome_name = df_rslt_rr$outcome_name,
                        bias_est = fitnull[1],
                        bias_se = bias_se,
                        bias_p = bias_p,
                        before_est = df_rslt_rr$logRR,
                        before_se  = df_rslt_rr$seLogRR,
                        after_est = df_rslt_rr$logRR - fitnull[1],
                        after_se = sqrt(df_rslt_rr$seLogRR^2 + bias_se^2))
  
  write.csv(res.cal, file = paste0(eth_o[j],"_res_cal.csv"), row.names=FALSE)
}

