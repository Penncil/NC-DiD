### data generation for NC-DiD paper

set.seed(2025)

n = 10000
n.medical = 5
n.visit.test = 4
n.visit = 24
n.nco = 30

# intervention
eth_list <- c("NHW", "NHB", "Hispanic", "AAPI")
eth_cat <- sample(eth_list, n, replace = TRUE, prob = c(0.6, 0.15, 0.2, 0.05))

# covariates
age <- floor(runif(n, min = 0, max = 20.99))
age_cat <- cut(age,
               breaks=c(-0.001, 5, 12, 21),
               labels=c('0-4', '5-11', '12-20'))

sex <- factor(rbinom(n, 1, 0.5))
levels(sex) <- c("Male", "Female")

month_year_vector <- seq.Date(from = as.Date("2020-03-01"), to = as.Date("2022-10-01"), by = "month")
cohort_entry_month_cat <- format(month_year_vector, "%Y, %m")
cohort_entry_month <- factor(sample(cohort_entry_month_cat, n, replace = TRUE))

site_cat <- c("A", "B", "C", "D", "E", "F")
site <- factor(sample(site_cat, n, replace = TRUE))

obese <- factor(rbinom(n, 1, 0.3))

pmca_index <- factor(sample(c(0,1,2), n, replace = TRUE, prob = c(0.5, 0.15, 0.35)))

lambda_visit_test <- runif(n.visit.test, min = 1, max = 3)
X_visit_test <- data.frame(sapply(1:n.visit.test, function(x) ifelse(runif(n) < 0.6, 0, rpois(n, lambda_visit_test))))
X_visit_test_cat <- data.frame(sapply(X_visit_test, function(x) {
  cut(x, breaks = c(-Inf, 0, 1, 2, Inf), labels = c("0", "1", "2", ">2"))
}))
colnames(X_visit_test_cat) <- c("n_ed", "n_inpatient", "n_outpatient", "n_tests")

n_dosage_raw <- ifelse(runif(n) < 0.7, 0, rpois(n, 1))
n_dosage <- cut(n_dosage_raw, breaks = c(-Inf, 0, 1, Inf), labels = c("0", "1", ">1"))
imm_date_diff_grp <- ifelse(n_dosage=="0", "no vaccine", 
                            ifelse(runif(n) < 0.5, "<4 months", ">=4 months"))

p_medical <- runif(n.medical, min = 0.2, max = 0.6)
X_medical <- data.frame(sapply(1:n.medical, function(x) rbinom(n, 1, p_medical[x])))
colnames(X_medical) <- c(paste0("medical_", 1:n.medical))

# outcome
visit_list <- c('acute_kidney_injury','acute_respiratory_distress_syndrome',
                'arrythmias','fluid_and_electrolyte','heart_disease',
                'myocarditis','myositis','pots','thrombophlebitis_and_thromboembolism',
                'abdominal_pain','abnormal_liver_enzymes',
                'cardiovascular_signs_and_sx','changes_in_taste_and_smell',
                'chest_pain','cognitive_function','fatigue_and_malaise',
                'fever_and_chills','generalized_pain','hair_loss',
                'headache','mental_health','musculoskeletal',
                'respiratory_signs_and_sx','skin')

p_visit <- runif(n.visit, min = 0.001, max = 0.04)
X_visits <- data.frame(sapply(1:n.visit, function(x) rbinom(n, 1, p_visit[x])))
colnames(X_visits) <- paste0("visits_", visit_list)

p_pre_visit <- runif(n.visit, min = 0.001, max = 0.02)
X_pre_visits <- data.frame(sapply(1:n.visit, function(x) rbinom(n, 1, p_pre_visit[x])))
colnames(X_pre_visits) <- paste0("pre_visits_", visit_list)

# NCO
p_nco_visit <- runif(n.nco, min = 0.001, max = 0.03)
NCO_visits <- data.frame(sapply(1:n.nco, function(x) rbinom(n, 1, p_nco_visit[x])))
colnames(NCO_visits) <- paste0("nco_", 1:n.nco)

p_pre_nco_visit <- runif(n.nco, min = 0.001, max = 0.02)
NCO_pre_visits <- data.frame(sapply(1:n.nco, function(x) rbinom(n, 1, p_pre_nco_visit[x])))
colnames(NCO_pre_visits) <- paste0("pre_nco_", 1:n.nco)

data <- data.frame(eth_cat, age_cat, sex, cohort_entry_month, site, obese,
                   pmca_index, X_visit_test_cat, n_dosage, imm_date_diff_grp, X_medical,
                   X_visits, X_pre_visits, NCO_visits, NCO_pre_visits)

### Add composite outcomes
data$visits_at_least_one_incident <- ifelse(rowSums(data%>%select(starts_with("visits_")))>0,1,0)
data$pre_visits_at_least_one_incident <- ifelse(rowSums(data%>%select(starts_with("pre_visits_")))>0,1,0)
data$base_visits_at_least_one_incident <- ifelse(rowSums(data%>%select(starts_with("base_visits_")))>0,1,0)

systematic_list <- c('acute_kidney_injury','acute_respiratory_distress_syndrome',
                     'arrythmias','fluid_and_electrolyte','heart_disease',
                     'myocarditis','myositis','pots','thrombophlebitis_and_thromboembolism')
syndromic_list <- c('abdominal_pain','abnormal_liver_enzymes',
                    'cardiovascular_signs_and_sx','changes_in_taste_and_smell',
                    'chest_pain','cognitive_function','fatigue_and_malaise',
                    'fever_and_chills','generalized_pain','hair_loss',
                    'headache','mental_health','musculoskeletal',
                    'respiratory_signs_and_sx','skin')

data$visits_systematic <- ifelse(rowSums(data%>%select(paste0('visits_',systematic_list)))>0,1,0)
data$pre_visits_systematic <- ifelse(rowSums(data%>%select(paste0('pre_visits_',systematic_list)))>0,1,0)
data$visits_syndromic <- ifelse(rowSums(data%>%select(paste0('visits_',syndromic_list)))>0,1,0)
data$pre_visits_syndromic <- ifelse(rowSums(data%>%select(paste0('pre_visits_',syndromic_list)))>0,1,0)

table(data$visits_at_least_one_incident)
table(data$pre_visits_at_least_one_incident)

write.csv(data, "demo_data.csv", row.names = FALSE)
