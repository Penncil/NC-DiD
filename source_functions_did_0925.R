### Source functions for DiD paper


find_did<-function(data, names_outcome){
  results=array(0,dim=c(length(names_outcome),3))
  for(i in 1:length(names_outcome)){
    data_lm=data.frame(
      outcome=c(data[,names_outcome[i]],data[,paste0("pre_",names_outcome[i])]),
      covid=c(rep(1,nrow(data)),rep(0,nrow(data))),
      eth_cat=data$eth_cat
    )
    s1=summary(glm(outcome~eth_cat+covid+eth_cat:covid,data=data_lm,family = "poisson"))
    
    results[i,]=s1$coefficients[4,c(1,2,4)]
  }
  return(results)
}


find_did_nco<-function(data, names_outcome){
  results=array(0,dim=c(length(names_outcome),3))
  for(i in 1:length(names_outcome)){
    data_lm=data.frame(
      outcome=c(data[,names_outcome[i]],data[,paste0("pre_",names_outcome[i])]),
      covid=c(rep(1,nrow(data)),rep(0,nrow(data))),
      eth_cat=data$eth_cat
    )
    s1=summary(glm(outcome~eth_cat+covid+eth_cat:covid,data=data_lm,family = "poisson"))
    
    results[i,]=s1$coefficients[4,c(1,2,4)]
  }
  return(results)
}


