### source functions, NC-DiD

find_did_RR_robust<-function(data, names_outcome){
  data_lm=data.frame(
    outcome = c(data[,names_outcome],data[,paste0("pre_",names_outcome)]),
    covid = c(rep(1,nrow(data)),rep(0,nrow(data))),
    eth_cat = data$eth_cat
  )
  m1 = glm(outcome~eth_cat+covid+eth_cat:covid,data=data_lm,family = "poisson")
  s1=summary(m1)
  m1_sandwich = sandwich(m1)
  
  rslt=s1$coefficients[4,c(1,2,4)]
  return(data.frame(outcome_name = names_outcome, 
                    logRR = rslt[1], 
                    seLogRR = sqrt(m1_sandwich[4,4]),
                    p = rslt[3],
                    RR = exp(rslt[1]),
                    LL = exp(rslt[1] - 1.96*rslt[2]),
                    UL = exp(rslt[1] + 1.96*rslt[2])))
}
