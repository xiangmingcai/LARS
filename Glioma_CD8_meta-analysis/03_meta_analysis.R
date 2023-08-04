
library(meta)
####00 input data####
output_dir_root <- choose.dir(default = "D:\\output_dir", caption = "choose the output folder")
output_dir<-output_dir_root
dir.create(paste0(output_dir_root,"//03_meta_analysis"))


data_dir <- choose.dir(default = "D:\\data_dir", caption = "choose the folder storing input_data.txt file")

# input_dataset.rds_path <- choose.files(default = data_dir, caption = "choose the input_dataset.rds file",
#                                        multi = TRUE, filters = Filters,
#                                        index = nrow(Filters))
# input_dataset<- readRDS(file = input_dataset.rds_path)
sum_table.rds_path <- choose.files(default = data_dir, caption = "choose the sum_table.rds file",
                                   multi = TRUE, filters = Filters,
                                   index = nrow(Filters))
sum_table<-readRDS(file = sum_table.rds_path)
# PMID33838625 was excluded for patients number less then 5
sum_table<-sum_table[(sum_table$PMID!="PMID33838625"),]
rownames(sum_table)<-c(1:nrow(sum_table))

colnames(sum_table)

pooled_table_templet <- data.frame(matrix(ncol = 15, nrow = 0))
colnames(pooled_table_templet) <- c('Subgroup', 'Outcome', 
                                 'Study_number',"Patient_numbers",
                                 "HR_Random_model","CI_lower_Random_model","CI_upper_Random_model","pval_Random_model",
                                 "HR_Common_model","CI_lower_Common_model","CI_upper_Common_model","pval_Common_model",
                                 "tau","I2","pval_Q")
pooled_table<-pooled_table_templet

output_dir<-paste0(output_dir_root,"//03_meta_analysis//01 All_population")
dir.create(output_dir)
#### 01 All_population start ####
#### 01_01_01 All_population; OS; dichotomous CD8 ####
meta_table<-sum_table[(c(1:25)),]
# 01_01 All_population; OS
meta_table<-subset(meta_table,Outcome=="OS")
# 01_01_01 All_population; OS; dichotomous CD8
meta_table<-subset(meta_table,PatientNumInCD8PositiveGroup!="Cox continue CD8")
# exclude results without convergence
#no exclusion
# meta-analysis
meta_result<-metagen(logHR, selogHR, sm = "HR",studlab = ResearchName,data=meta_table)
# forest plot
pdf(paste0(output_dir,"//ForestPlot_Allpopulation_OS_dichotomousCD8.pdf"),width=10, height=4)
p_forest<-forest(meta_result,print.Q =T,digits=4,digits.pval = 3,digits.pval.Q = 3,
                 test.overall.common = TRUE,
                 test.overall.random = TRUE)
print(p_forest)
dev.off()
# funnel plot
pdf(paste0(output_dir,"//FunnelPlot_Allpopulation_OS_dichotomousCD8.pdf"),width=6, height=6)
p_funnel<-funnel(meta_result)
print(p_funnel)
dev.off()

# add results to pooled table
pooled_table_templet_temp<-pooled_table_templet
pooled_table_templet_temp[1,"Subgroup"]<-"All_population"
pooled_table_templet_temp[1,"Outcome"]<-"OS"
pooled_table_templet_temp[1,"Study_number"]<-nrow(meta_table)
if(nrow(meta_table)>=1){
  pooled_table_templet_temp[1,"Patient_numbers"]<-sum(as.numeric(meta_table$PatientNumInCD8PositiveGroup),as.numeric(meta_table$PatientNumInCD8NegativeGroup))
  pooled_table_templet_temp[1,"HR_Random_model"]<-round(exp(meta_result$TE.random),digits = 4)
  pooled_table_templet_temp[1,"CI_lower_Random_model"]<-round(exp(meta_result$lower.random),digits = 4)
  pooled_table_templet_temp[1,"CI_upper_Random_model"]<-round(exp(meta_result$upper.random),digits = 4)
  pooled_table_templet_temp[1,"pval_Random_model"]<-round(meta_result$pval.random,digits = 4)
  pooled_table_templet_temp[1,"HR_Common_model"]<-round(exp(meta_result$TE.common),digits = 4)
  pooled_table_templet_temp[1,"CI_lower_Common_model"]<-round(exp(meta_result$lower.common),digits = 4)
  pooled_table_templet_temp[1,"CI_upper_Common_model"]<-round(exp(meta_result$upper.common),digits = 4)
  pooled_table_templet_temp[1,"pval_Common_model"]<-round(meta_result$pval.common,digits = 4)
  pooled_table_templet_temp[1,"tau"]<-round(meta_result$tau,digits = 4)
  pooled_table_templet_temp[1,"I2"]<-paste0(as.character(round(100*meta_result$I2,digits = 1)),"%")
  pooled_table_templet_temp[1,"pval_Q"]<-round(meta_result$pval.Q,digits = 4)
  pooled_table<-rbind(pooled_table,pooled_table_templet_temp)
} else {
  pooled_table_templet_temp[1,"Patient_numbers"]<-0
  pooled_table_templet_temp[1,"HR_Random_model"]<-NA
  pooled_table_templet_temp[1,"CI_lower_Random_model"]<-NA
  pooled_table_templet_temp[1,"CI_upper_Random_model"]<-NA
  pooled_table_templet_temp[1,"pval_Random_model"]<-NA
  pooled_table_templet_temp[1,"HR_Common_model"]<-NA
  pooled_table_templet_temp[1,"CI_lower_Common_model"]<-NA
  pooled_table_templet_temp[1,"CI_upper_Common_model"]<-NA
  pooled_table_templet_temp[1,"pval_Common_model"]<-NA
  pooled_table_templet_temp[1,"tau"]<-NA
  pooled_table_templet_temp[1,"I2"]<-NA
  pooled_table_templet_temp[1,"pval_Q"]<-NA
  pooled_table<-rbind(pooled_table,pooled_table_templet_temp)
}


#### 01_01_02 All_population; PFS; dichotomous CD8 ####
meta_table<-sum_table[(c(1:25)),]
# 01_01 All_population; PFS
meta_table<-subset(meta_table,Outcome=="PFS")
# 01_01_02 All_population; PFS; dichotomous CD8
meta_table<-subset(meta_table,PatientNumInCD8PositiveGroup!="Cox continue CD8")
# exclude results without convergence
#no exclusion
# meta-analysis
meta_result<-metagen(logHR, selogHR, sm = "HR",studlab = ResearchName,data=meta_table)
# forest plot
pdf(paste0(output_dir,"//ForestPlot_Allpopulation_PFS_continueCD8.pdf"),width=10, height=4)
p_forest<-forest(meta_result,print.Q =T,digits=4,digits.pval = 3,digits.pval.Q = 3,
                 test.overall.common = TRUE,
                 test.overall.random = TRUE)
print(p_forest)
dev.off()
# funnel plot
pdf(paste0(output_dir,"//FunnelPlot_Allpopulation_PFS_continueCD8.pdf"),width=6, height=6)
p_funnel<-funnel(meta_result)
print(p_funnel)
dev.off()

# add results to pooled table
pooled_table_templet_temp<-pooled_table_templet
pooled_table_templet_temp[1,"Subgroup"]<-"All_population"
pooled_table_templet_temp[1,"Outcome"]<-"PFS"
pooled_table_templet_temp[1,"Study_number"]<-nrow(meta_table)
if(nrow(meta_table)>=1){
  pooled_table_templet_temp[1,"Patient_numbers"]<-sum(as.numeric(meta_table$PatientNumInCD8PositiveGroup),as.numeric(meta_table$PatientNumInCD8NegativeGroup))
  pooled_table_templet_temp[1,"HR_Random_model"]<-round(exp(meta_result$TE.random),digits = 4)
  pooled_table_templet_temp[1,"CI_lower_Random_model"]<-round(exp(meta_result$lower.random),digits = 4)
  pooled_table_templet_temp[1,"CI_upper_Random_model"]<-round(exp(meta_result$upper.random),digits = 4)
  pooled_table_templet_temp[1,"pval_Random_model"]<-round(meta_result$pval.random,digits = 4)
  pooled_table_templet_temp[1,"HR_Common_model"]<-round(exp(meta_result$TE.common),digits = 4)
  pooled_table_templet_temp[1,"CI_lower_Common_model"]<-round(exp(meta_result$lower.common),digits = 4)
  pooled_table_templet_temp[1,"CI_upper_Common_model"]<-round(exp(meta_result$upper.common),digits = 4)
  pooled_table_templet_temp[1,"pval_Common_model"]<-round(meta_result$pval.common,digits = 4)
  pooled_table_templet_temp[1,"tau"]<-round(meta_result$tau,digits = 4)
  pooled_table_templet_temp[1,"I2"]<-paste0(as.character(round(100*meta_result$I2,digits = 1)),"%")
  pooled_table_templet_temp[1,"pval_Q"]<-round(meta_result$pval.Q,digits = 4)
  pooled_table<-rbind(pooled_table,pooled_table_templet_temp)
} else {
  pooled_table_templet_temp[1,"Patient_numbers"]<-0
  pooled_table_templet_temp[1,"HR_Random_model"]<-NA
  pooled_table_templet_temp[1,"CI_lower_Random_model"]<-NA
  pooled_table_templet_temp[1,"CI_upper_Random_model"]<-NA
  pooled_table_templet_temp[1,"pval_Random_model"]<-NA
  pooled_table_templet_temp[1,"HR_Common_model"]<-NA
  pooled_table_templet_temp[1,"CI_lower_Common_model"]<-NA
  pooled_table_templet_temp[1,"CI_upper_Common_model"]<-NA
  pooled_table_templet_temp[1,"pval_Common_model"]<-NA
  pooled_table_templet_temp[1,"tau"]<-NA
  pooled_table_templet_temp[1,"I2"]<-NA
  pooled_table_templet_temp[1,"pval_Q"]<-NA
  pooled_table<-rbind(pooled_table,pooled_table_templet_temp)
}

#### 01_01_03 All_population; Survival after immunotherapy; dichotomous CD8 ####
meta_table<-sum_table[(c(1:25)),]
# 01_01 All_population; Survival after immunotherapy
meta_table<-subset(meta_table,Outcome=="Survival after immunotherapy")
# 01_01_03 All_population; Survival after immunotherapy; dichotomous CD8
meta_table<-subset(meta_table,PatientNumInCD8PositiveGroup!="Cox continue CD8")
# exclude results without convergence
meta_table<-meta_table[(meta_table$PMID!="PMID37148198"),]
# meta-analysis
meta_result<-metagen(logHR, selogHR, sm = "HR",studlab = ResearchName,data=meta_table)
# forest plot
pdf(paste0(output_dir,"//ForestPlot_Allpopulation_SurvivalAfterImmunotherapy_continueCD8.pdf"),width=10, height=4)
p_forest<-forest(meta_result,print.Q =T,digits=4,digits.pval = 3,digits.pval.Q = 3,
                 test.overall.common = TRUE,
                 test.overall.random = TRUE)
print(p_forest)
dev.off()
# funnel plot
pdf(paste0(output_dir,"//FunnelPlot_Allpopulation_SurvivalAfterImmunotherapy_continueCD8.pdf"),width=6, height=6)
p_funnel<-funnel(meta_result)
print(p_funnel)
dev.off()

# add results to pooled table
pooled_table_templet_temp<-pooled_table_templet
pooled_table_templet_temp[1,"Subgroup"]<-"All_population"
pooled_table_templet_temp[1,"Outcome"]<-"Survival after immunotherapy"
pooled_table_templet_temp[1,"Study_number"]<-nrow(meta_table)
if(nrow(meta_table)>=1){
  pooled_table_templet_temp[1,"Patient_numbers"]<-sum(as.numeric(meta_table$PatientNumInCD8PositiveGroup),as.numeric(meta_table$PatientNumInCD8NegativeGroup))
  pooled_table_templet_temp[1,"HR_Random_model"]<-round(exp(meta_result$TE.random),digits = 4)
  pooled_table_templet_temp[1,"CI_lower_Random_model"]<-round(exp(meta_result$lower.random),digits = 4)
  pooled_table_templet_temp[1,"CI_upper_Random_model"]<-round(exp(meta_result$upper.random),digits = 4)
  pooled_table_templet_temp[1,"pval_Random_model"]<-round(meta_result$pval.random,digits = 4)
  pooled_table_templet_temp[1,"HR_Common_model"]<-round(exp(meta_result$TE.common),digits = 4)
  pooled_table_templet_temp[1,"CI_lower_Common_model"]<-round(exp(meta_result$lower.common),digits = 4)
  pooled_table_templet_temp[1,"CI_upper_Common_model"]<-round(exp(meta_result$upper.common),digits = 4)
  pooled_table_templet_temp[1,"pval_Common_model"]<-round(meta_result$pval.common,digits = 4)
  pooled_table_templet_temp[1,"tau"]<-round(meta_result$tau,digits = 4)
  pooled_table_templet_temp[1,"I2"]<-paste0(as.character(round(100*meta_result$I2,digits = 1)),"%")
  pooled_table_templet_temp[1,"pval_Q"]<-round(meta_result$pval.Q,digits = 4)
  pooled_table<-rbind(pooled_table,pooled_table_templet_temp)
} else {
  pooled_table_templet_temp[1,"Patient_numbers"]<-0
  pooled_table_templet_temp[1,"HR_Random_model"]<-NA
  pooled_table_templet_temp[1,"CI_lower_Random_model"]<-NA
  pooled_table_templet_temp[1,"CI_upper_Random_model"]<-NA
  pooled_table_templet_temp[1,"pval_Random_model"]<-NA
  pooled_table_templet_temp[1,"HR_Common_model"]<-NA
  pooled_table_templet_temp[1,"CI_lower_Common_model"]<-NA
  pooled_table_templet_temp[1,"CI_upper_Common_model"]<-NA
  pooled_table_templet_temp[1,"pval_Common_model"]<-NA
  pooled_table_templet_temp[1,"tau"]<-NA
  pooled_table_templet_temp[1,"I2"]<-NA
  pooled_table_templet_temp[1,"pval_Q"]<-NA
  pooled_table<-rbind(pooled_table,pooled_table_templet_temp)
}

#### 01 All_population end ####


output_dir<-paste0(output_dir_root,"//03_meta_analysis//02_01 Subgroup_Gender")
dir.create(output_dir)
#### 02_01 Subgroup_Gender start ####
#### 02_01_01_01 Male; OS; dichotomous CD8 ####
# 02_01_01 Male
meta_table<-subset(sum_table,Gender=="Male")
# 02_01_01_01 Male; OS
meta_table<-subset(meta_table,Outcome=="OS")
# 02_01_01_01 Male; OS; dichotomous CD8
meta_table<-subset(meta_table,PatientNumInCD8PositiveGroup!="Cox continue CD8")
# exclude results without convergence
meta_table<-meta_table[(meta_table$PMID!="PMID15452186"),]
# meta-analysis
meta_result<-metagen(logHR, selogHR, sm = "HR",studlab = ResearchName,data=meta_table)
# forest plot
pdf(paste0(output_dir,"//ForestPlot_Subgroup_Gender_Male_OS_dichotomousCD8.pdf"),width=10, height=4)
p_forest<-forest(meta_result,print.Q =T,digits=4,digits.pval = 3,digits.pval.Q = 3,
                 test.overall.common = TRUE,
                 test.overall.random = TRUE)
print(p_forest)
dev.off()
# funnel plot
pdf(paste0(output_dir,"//FunnelPlot_Subgroup_Gender_Male_OS_dichotomousCD8.pdf"),width=6, height=6)
p_funnel<-funnel(meta_result)
print(p_funnel)
dev.off()

# add results to pooled table
pooled_table_templet_temp<-pooled_table_templet
pooled_table_templet_temp[1,"Subgroup"]<-"Male"
pooled_table_templet_temp[1,"Outcome"]<-"OS"
pooled_table_templet_temp[1,"Study_number"]<-nrow(meta_table)
if(nrow(meta_table)>=1){
  pooled_table_templet_temp[1,"Patient_numbers"]<-sum(as.numeric(meta_table$PatientNumInCD8PositiveGroup),as.numeric(meta_table$PatientNumInCD8NegativeGroup))
  pooled_table_templet_temp[1,"HR_Random_model"]<-round(exp(meta_result$TE.random),digits = 4)
  pooled_table_templet_temp[1,"CI_lower_Random_model"]<-round(exp(meta_result$lower.random),digits = 4)
  pooled_table_templet_temp[1,"CI_upper_Random_model"]<-round(exp(meta_result$upper.random),digits = 4)
  pooled_table_templet_temp[1,"pval_Random_model"]<-round(meta_result$pval.random,digits = 4)
  pooled_table_templet_temp[1,"HR_Common_model"]<-round(exp(meta_result$TE.common),digits = 4)
  pooled_table_templet_temp[1,"CI_lower_Common_model"]<-round(exp(meta_result$lower.common),digits = 4)
  pooled_table_templet_temp[1,"CI_upper_Common_model"]<-round(exp(meta_result$upper.common),digits = 4)
  pooled_table_templet_temp[1,"pval_Common_model"]<-round(meta_result$pval.common,digits = 4)
  pooled_table_templet_temp[1,"tau"]<-round(meta_result$tau,digits = 4)
  pooled_table_templet_temp[1,"I2"]<-paste0(as.character(round(100*meta_result$I2,digits = 1)),"%")
  pooled_table_templet_temp[1,"pval_Q"]<-round(meta_result$pval.Q,digits = 4)
  pooled_table<-rbind(pooled_table,pooled_table_templet_temp)
} else {
  pooled_table_templet_temp[1,"Patient_numbers"]<-0
  pooled_table_templet_temp[1,"HR_Random_model"]<-NA
  pooled_table_templet_temp[1,"CI_lower_Random_model"]<-NA
  pooled_table_templet_temp[1,"CI_upper_Random_model"]<-NA
  pooled_table_templet_temp[1,"pval_Random_model"]<-NA
  pooled_table_templet_temp[1,"HR_Common_model"]<-NA
  pooled_table_templet_temp[1,"CI_lower_Common_model"]<-NA
  pooled_table_templet_temp[1,"CI_upper_Common_model"]<-NA
  pooled_table_templet_temp[1,"pval_Common_model"]<-NA
  pooled_table_templet_temp[1,"tau"]<-NA
  pooled_table_templet_temp[1,"I2"]<-NA
  pooled_table_templet_temp[1,"pval_Q"]<-NA
  pooled_table<-rbind(pooled_table,pooled_table_templet_temp)
}

#### 02_01_01_02 Male; PFS; dichotomous CD8 ####
# 02_01_01 Male
meta_table<-subset(sum_table,Gender=="Male")
# 02_01_01_02 Male; PFS
meta_table<-subset(meta_table,Outcome=="PFS")
# 02_01_01_02 Male; PFS; dichotomous CD8
meta_table<-subset(meta_table,PatientNumInCD8PositiveGroup!="Cox continue CD8")
# exclude results without convergence
meta_table<-meta_table[(meta_table$PMID!="PMID15452186"),]
# meta-analysis
meta_result<-metagen(logHR, selogHR, sm = "HR",studlab = ResearchName,data=meta_table)
# forest plot
pdf(paste0(output_dir,"//ForestPlot_Subgroup_Gender_Male_PFS_dichotomousCD8.pdf"),width=10, height=4)
p_forest<-forest(meta_result,print.Q =T,digits=4,digits.pval = 3,digits.pval.Q = 3,
                 test.overall.common = TRUE,
                 test.overall.random = TRUE)
print(p_forest)
dev.off()
# funnel plot
pdf(paste0(output_dir,"//FunnelPlot_Subgroup_Gender_Male_PFS_dichotomousCD8.pdf"),width=6, height=6)
p_funnel<-funnel(meta_result)
print(p_funnel)
dev.off()

# add results to pooled table
pooled_table_templet_temp<-pooled_table_templet
pooled_table_templet_temp[1,"Subgroup"]<-"Male"
pooled_table_templet_temp[1,"Outcome"]<-"PFS"
pooled_table_templet_temp[1,"Study_number"]<-nrow(meta_table)
if(nrow(meta_table)>=1){
  pooled_table_templet_temp[1,"Patient_numbers"]<-sum(as.numeric(meta_table$PatientNumInCD8PositiveGroup),as.numeric(meta_table$PatientNumInCD8NegativeGroup))
  pooled_table_templet_temp[1,"HR_Random_model"]<-round(exp(meta_result$TE.random),digits = 4)
  pooled_table_templet_temp[1,"CI_lower_Random_model"]<-round(exp(meta_result$lower.random),digits = 4)
  pooled_table_templet_temp[1,"CI_upper_Random_model"]<-round(exp(meta_result$upper.random),digits = 4)
  pooled_table_templet_temp[1,"pval_Random_model"]<-round(meta_result$pval.random,digits = 4)
  pooled_table_templet_temp[1,"HR_Common_model"]<-round(exp(meta_result$TE.common),digits = 4)
  pooled_table_templet_temp[1,"CI_lower_Common_model"]<-round(exp(meta_result$lower.common),digits = 4)
  pooled_table_templet_temp[1,"CI_upper_Common_model"]<-round(exp(meta_result$upper.common),digits = 4)
  pooled_table_templet_temp[1,"pval_Common_model"]<-round(meta_result$pval.common,digits = 4)
  pooled_table_templet_temp[1,"tau"]<-round(meta_result$tau,digits = 4)
  pooled_table_templet_temp[1,"I2"]<-paste0(as.character(round(100*meta_result$I2,digits = 1)),"%")
  pooled_table_templet_temp[1,"pval_Q"]<-round(meta_result$pval.Q,digits = 4)
  pooled_table<-rbind(pooled_table,pooled_table_templet_temp)
} else {
  pooled_table_templet_temp[1,"Patient_numbers"]<-0
  pooled_table_templet_temp[1,"HR_Random_model"]<-NA
  pooled_table_templet_temp[1,"CI_lower_Random_model"]<-NA
  pooled_table_templet_temp[1,"CI_upper_Random_model"]<-NA
  pooled_table_templet_temp[1,"pval_Random_model"]<-NA
  pooled_table_templet_temp[1,"HR_Common_model"]<-NA
  pooled_table_templet_temp[1,"CI_lower_Common_model"]<-NA
  pooled_table_templet_temp[1,"CI_upper_Common_model"]<-NA
  pooled_table_templet_temp[1,"pval_Common_model"]<-NA
  pooled_table_templet_temp[1,"tau"]<-NA
  pooled_table_templet_temp[1,"I2"]<-NA
  pooled_table_templet_temp[1,"pval_Q"]<-NA
  pooled_table<-rbind(pooled_table,pooled_table_templet_temp)
}
#### 02_01_01_03 Male; Survival after immunotherapy; dichotomous CD8 ####
# 02_01_01 Male
meta_table<-subset(sum_table,Gender=="Male")
# 02_01_01_02 Male; Survival after immunotherapy
meta_table<-subset(meta_table,Outcome=="Survival after immunotherapy")
# 02_01_01_02 Male; Survival after immunotherapy; dichotomous CD8
meta_table<-subset(meta_table,PatientNumInCD8PositiveGroup!="Cox continue CD8")
# exclude results without convergence
meta_table<-meta_table[(meta_table$PMID!="PMID18957964"),]
meta_table<-meta_table[(meta_table$PMID!="PMID37148198"),]
# meta-analysis
# no study left
# meta_result<-metagen(logHR, selogHR, sm = "HR",studlab = ResearchName,data=meta_table)
# # forest plot
# pdf(paste0(output_dir,"//ForestPlot_Subgroup_Gender_Male_SurvivalAfterImmunotherapy_dichotomousCD8.pdf"),width=10, height=4)
# p_forest<-forest(meta_result,print.Q =T,digits=4,digits.pval = 3,digits.pval.Q = 3,
#                  test.overall.common = TRUE,
#                  test.overall.random = TRUE)
# print(p_forest)
# dev.off()
# # funnel plot
# pdf(paste0(output_dir,"//FunnelPlot_Subgroup_Gender_Male_SurvivalAfterImmunotherapy_dichotomousCD8.pdf"),width=6, height=6)
# p_funnel<-funnel(meta_result)
# print(p_funnel)
# dev.off()

# add results to pooled table
pooled_table_templet_temp<-pooled_table_templet
pooled_table_templet_temp[1,"Subgroup"]<-"Male"
pooled_table_templet_temp[1,"Outcome"]<-"Survival after immunotherapy"
pooled_table_templet_temp[1,"Study_number"]<-nrow(meta_table)
if(nrow(meta_table)>=1){
  pooled_table_templet_temp[1,"Patient_numbers"]<-sum(as.numeric(meta_table$PatientNumInCD8PositiveGroup),as.numeric(meta_table$PatientNumInCD8NegativeGroup))
  pooled_table_templet_temp[1,"HR_Random_model"]<-round(exp(meta_result$TE.random),digits = 4)
  pooled_table_templet_temp[1,"CI_lower_Random_model"]<-round(exp(meta_result$lower.random),digits = 4)
  pooled_table_templet_temp[1,"CI_upper_Random_model"]<-round(exp(meta_result$upper.random),digits = 4)
  pooled_table_templet_temp[1,"pval_Random_model"]<-round(meta_result$pval.random,digits = 4)
  pooled_table_templet_temp[1,"HR_Common_model"]<-round(exp(meta_result$TE.common),digits = 4)
  pooled_table_templet_temp[1,"CI_lower_Common_model"]<-round(exp(meta_result$lower.common),digits = 4)
  pooled_table_templet_temp[1,"CI_upper_Common_model"]<-round(exp(meta_result$upper.common),digits = 4)
  pooled_table_templet_temp[1,"pval_Common_model"]<-round(meta_result$pval.common,digits = 4)
  pooled_table_templet_temp[1,"tau"]<-round(meta_result$tau,digits = 4)
  pooled_table_templet_temp[1,"I2"]<-paste0(as.character(round(100*meta_result$I2,digits = 1)),"%")
  pooled_table_templet_temp[1,"pval_Q"]<-round(meta_result$pval.Q,digits = 4)
  pooled_table<-rbind(pooled_table,pooled_table_templet_temp)
} else {
  pooled_table_templet_temp[1,"Patient_numbers"]<-0
  pooled_table_templet_temp[1,"HR_Random_model"]<-NA
  pooled_table_templet_temp[1,"CI_lower_Random_model"]<-NA
  pooled_table_templet_temp[1,"CI_upper_Random_model"]<-NA
  pooled_table_templet_temp[1,"pval_Random_model"]<-NA
  pooled_table_templet_temp[1,"HR_Common_model"]<-NA
  pooled_table_templet_temp[1,"CI_lower_Common_model"]<-NA
  pooled_table_templet_temp[1,"CI_upper_Common_model"]<-NA
  pooled_table_templet_temp[1,"pval_Common_model"]<-NA
  pooled_table_templet_temp[1,"tau"]<-NA
  pooled_table_templet_temp[1,"I2"]<-NA
  pooled_table_templet_temp[1,"pval_Q"]<-NA
  pooled_table<-rbind(pooled_table,pooled_table_templet_temp)
}
#### 02_01_02_01 Female; OS; dichotomous CD8 ####
# 02_01_02 Female
meta_table<-subset(sum_table,Gender=="Female")
# 02_01_02_01 Female; OS
meta_table<-subset(meta_table,Outcome=="OS")
# 02_01_02_01 Female; OS; dichotomous CD8
meta_table<-subset(meta_table,PatientNumInCD8PositiveGroup!="Cox continue CD8")
# exclude results without convergence
meta_table<-meta_table[(meta_table$PMID!="PMID15452186"),]
meta_table<-meta_table[(meta_table$PMID!="PMID33130898"),]
# meta-analysis
meta_result<-metagen(logHR, selogHR, sm = "HR",studlab = ResearchName,data=meta_table)
# forest plot
pdf(paste0(output_dir,"//ForestPlot_Subgroup_Gender_Female_OS_dichotomousCD8.pdf"),width=10, height=4)
p_forest<-forest(meta_result,print.Q =T,digits=4,digits.pval = 3,digits.pval.Q = 3,
                 test.overall.common = TRUE,
                 test.overall.random = TRUE)
print(p_forest)
dev.off()
# funnel plot
pdf(paste0(output_dir,"//FunnelPlot_Subgroup_Gender_Female_OS_dichotomousCD8.pdf"),width=6, height=6)
p_funnel<-funnel(meta_result)
print(p_funnel)
dev.off()

# add results to pooled table
pooled_table_templet_temp<-pooled_table_templet
pooled_table_templet_temp[1,"Subgroup"]<-"Female"
pooled_table_templet_temp[1,"Outcome"]<-"OS"
pooled_table_templet_temp[1,"Study_number"]<-nrow(meta_table)
if(nrow(meta_table)>=1){
  pooled_table_templet_temp[1,"Patient_numbers"]<-sum(as.numeric(meta_table$PatientNumInCD8PositiveGroup),as.numeric(meta_table$PatientNumInCD8NegativeGroup))
  pooled_table_templet_temp[1,"HR_Random_model"]<-round(exp(meta_result$TE.random),digits = 4)
  pooled_table_templet_temp[1,"CI_lower_Random_model"]<-round(exp(meta_result$lower.random),digits = 4)
  pooled_table_templet_temp[1,"CI_upper_Random_model"]<-round(exp(meta_result$upper.random),digits = 4)
  pooled_table_templet_temp[1,"pval_Random_model"]<-round(meta_result$pval.random,digits = 4)
  pooled_table_templet_temp[1,"HR_Common_model"]<-round(exp(meta_result$TE.common),digits = 4)
  pooled_table_templet_temp[1,"CI_lower_Common_model"]<-round(exp(meta_result$lower.common),digits = 4)
  pooled_table_templet_temp[1,"CI_upper_Common_model"]<-round(exp(meta_result$upper.common),digits = 4)
  pooled_table_templet_temp[1,"pval_Common_model"]<-round(meta_result$pval.common,digits = 4)
  pooled_table_templet_temp[1,"tau"]<-round(meta_result$tau,digits = 4)
  pooled_table_templet_temp[1,"I2"]<-paste0(as.character(round(100*meta_result$I2,digits = 1)),"%")
  pooled_table_templet_temp[1,"pval_Q"]<-round(meta_result$pval.Q,digits = 4)
  pooled_table<-rbind(pooled_table,pooled_table_templet_temp)
} else {
  pooled_table_templet_temp[1,"Patient_numbers"]<-0
  pooled_table_templet_temp[1,"HR_Random_model"]<-NA
  pooled_table_templet_temp[1,"CI_lower_Random_model"]<-NA
  pooled_table_templet_temp[1,"CI_upper_Random_model"]<-NA
  pooled_table_templet_temp[1,"pval_Random_model"]<-NA
  pooled_table_templet_temp[1,"HR_Common_model"]<-NA
  pooled_table_templet_temp[1,"CI_lower_Common_model"]<-NA
  pooled_table_templet_temp[1,"CI_upper_Common_model"]<-NA
  pooled_table_templet_temp[1,"pval_Common_model"]<-NA
  pooled_table_templet_temp[1,"tau"]<-NA
  pooled_table_templet_temp[1,"I2"]<-NA
  pooled_table_templet_temp[1,"pval_Q"]<-NA
  pooled_table<-rbind(pooled_table,pooled_table_templet_temp)
}


#### 02_01_02_02 Female; PFS; dichotomous CD8 ####
# 02_01_02 Female
meta_table<-subset(sum_table,Gender=="Female")
# 02_01_02_02 Female; PFS
meta_table<-subset(meta_table,Outcome=="PFS")
# 02_01_02_02 Female; PFS; dichotomous CD8
meta_table<-subset(meta_table,PatientNumInCD8PositiveGroup!="Cox continue CD8")
# exclude results without convergence
meta_table<-meta_table[(meta_table$PMID!="PMID15452186"),]
# meta-analysis
meta_result<-metagen(logHR, selogHR, sm = "HR",studlab = ResearchName,data=meta_table)
# forest plot
pdf(paste0(output_dir,"//ForestPlot_Subgroup_Gender_Female_PFS_dichotomousCD8.pdf"),width=10, height=4)
p_forest<-forest(meta_result,print.Q =T,digits=4,digits.pval = 3,digits.pval.Q = 3,
                 test.overall.common = TRUE,
                 test.overall.random = TRUE)
print(p_forest)
dev.off()
# funnel plot
pdf(paste0(output_dir,"//FunnelPlot_Subgroup_Gender_Female_PFS_dichotomousCD8.pdf"),width=6, height=6)
p_funnel<-funnel(meta_result)
print(p_funnel)
dev.off()

# add results to pooled table
pooled_table_templet_temp<-pooled_table_templet
pooled_table_templet_temp[1,"Subgroup"]<-"Female"
pooled_table_templet_temp[1,"Outcome"]<-"PFS"
pooled_table_templet_temp[1,"Study_number"]<-nrow(meta_table)
if(nrow(meta_table)>=1){
  pooled_table_templet_temp[1,"Patient_numbers"]<-sum(as.numeric(meta_table$PatientNumInCD8PositiveGroup),as.numeric(meta_table$PatientNumInCD8NegativeGroup))
  pooled_table_templet_temp[1,"HR_Random_model"]<-round(exp(meta_result$TE.random),digits = 4)
  pooled_table_templet_temp[1,"CI_lower_Random_model"]<-round(exp(meta_result$lower.random),digits = 4)
  pooled_table_templet_temp[1,"CI_upper_Random_model"]<-round(exp(meta_result$upper.random),digits = 4)
  pooled_table_templet_temp[1,"pval_Random_model"]<-round(meta_result$pval.random,digits = 4)
  pooled_table_templet_temp[1,"HR_Common_model"]<-round(exp(meta_result$TE.common),digits = 4)
  pooled_table_templet_temp[1,"CI_lower_Common_model"]<-round(exp(meta_result$lower.common),digits = 4)
  pooled_table_templet_temp[1,"CI_upper_Common_model"]<-round(exp(meta_result$upper.common),digits = 4)
  pooled_table_templet_temp[1,"pval_Common_model"]<-round(meta_result$pval.common,digits = 4)
  pooled_table_templet_temp[1,"tau"]<-round(meta_result$tau,digits = 4)
  pooled_table_templet_temp[1,"I2"]<-paste0(as.character(round(100*meta_result$I2,digits = 1)),"%")
  pooled_table_templet_temp[1,"pval_Q"]<-round(meta_result$pval.Q,digits = 4)
  pooled_table<-rbind(pooled_table,pooled_table_templet_temp)
} else {
  pooled_table_templet_temp[1,"Patient_numbers"]<-0
  pooled_table_templet_temp[1,"HR_Random_model"]<-NA
  pooled_table_templet_temp[1,"CI_lower_Random_model"]<-NA
  pooled_table_templet_temp[1,"CI_upper_Random_model"]<-NA
  pooled_table_templet_temp[1,"pval_Random_model"]<-NA
  pooled_table_templet_temp[1,"HR_Common_model"]<-NA
  pooled_table_templet_temp[1,"CI_lower_Common_model"]<-NA
  pooled_table_templet_temp[1,"CI_upper_Common_model"]<-NA
  pooled_table_templet_temp[1,"pval_Common_model"]<-NA
  pooled_table_templet_temp[1,"tau"]<-NA
  pooled_table_templet_temp[1,"I2"]<-NA
  pooled_table_templet_temp[1,"pval_Q"]<-NA
  pooled_table<-rbind(pooled_table,pooled_table_templet_temp)
}
#### 02_01_02_03 Female; Survival after immunotherapy; dichotomous CD8 ####
# 02_01_02 Female
meta_table<-subset(sum_table,Gender=="Female")
# 02_01_02_02 Female; Survival after immunotherapy
meta_table<-subset(meta_table,Outcome=="Survival after immunotherapy")
# 02_01_02_02 Female; Survival after immunotherapy; dichotomous CD8
meta_table<-subset(meta_table,PatientNumInCD8PositiveGroup!="Cox continue CD8")
# exclude results without convergence
meta_table<-meta_table[(meta_table$PMID!="PMID37148198"),]
# meta-analysis
meta_result<-metagen(logHR, selogHR, sm = "HR",studlab = ResearchName,data=meta_table)
# forest plot
pdf(paste0(output_dir,"//ForestPlot_Subgroup_Gender_Female_SurvivalAfterImmunotherapy_dichotomousCD8.pdf"),width=10, height=4)
p_forest<-forest(meta_result,print.Q =T,digits=4,digits.pval = 3,digits.pval.Q = 3,
                 test.overall.common = TRUE,
                 test.overall.random = TRUE)
print(p_forest)
dev.off()
# funnel plot
pdf(paste0(output_dir,"//FunnelPlot_Subgroup_Gender_Female_SurvivalAfterImmunotherapy_dichotomousCD8.pdf"),width=6, height=6)
p_funnel<-funnel(meta_result)
print(p_funnel)
dev.off()

# add results to pooled table
pooled_table_templet_temp<-pooled_table_templet
pooled_table_templet_temp[1,"Subgroup"]<-"Female"
pooled_table_templet_temp[1,"Outcome"]<-"Survival after immunotherapy"
pooled_table_templet_temp[1,"Study_number"]<-nrow(meta_table)
if(nrow(meta_table)>=1){
  pooled_table_templet_temp[1,"Patient_numbers"]<-sum(as.numeric(meta_table$PatientNumInCD8PositiveGroup),as.numeric(meta_table$PatientNumInCD8NegativeGroup))
  pooled_table_templet_temp[1,"HR_Random_model"]<-round(exp(meta_result$TE.random),digits = 4)
  pooled_table_templet_temp[1,"CI_lower_Random_model"]<-round(exp(meta_result$lower.random),digits = 4)
  pooled_table_templet_temp[1,"CI_upper_Random_model"]<-round(exp(meta_result$upper.random),digits = 4)
  pooled_table_templet_temp[1,"pval_Random_model"]<-round(meta_result$pval.random,digits = 4)
  pooled_table_templet_temp[1,"HR_Common_model"]<-round(exp(meta_result$TE.common),digits = 4)
  pooled_table_templet_temp[1,"CI_lower_Common_model"]<-round(exp(meta_result$lower.common),digits = 4)
  pooled_table_templet_temp[1,"CI_upper_Common_model"]<-round(exp(meta_result$upper.common),digits = 4)
  pooled_table_templet_temp[1,"pval_Common_model"]<-round(meta_result$pval.common,digits = 4)
  pooled_table_templet_temp[1,"tau"]<-round(meta_result$tau,digits = 4)
  pooled_table_templet_temp[1,"I2"]<-paste0(as.character(round(100*meta_result$I2,digits = 1)),"%")
  pooled_table_templet_temp[1,"pval_Q"]<-round(meta_result$pval.Q,digits = 4)
  pooled_table<-rbind(pooled_table,pooled_table_templet_temp)
} else {
  pooled_table_templet_temp[1,"Patient_numbers"]<-0
  pooled_table_templet_temp[1,"HR_Random_model"]<-NA
  pooled_table_templet_temp[1,"CI_lower_Random_model"]<-NA
  pooled_table_templet_temp[1,"CI_upper_Random_model"]<-NA
  pooled_table_templet_temp[1,"pval_Random_model"]<-NA
  pooled_table_templet_temp[1,"HR_Common_model"]<-NA
  pooled_table_templet_temp[1,"CI_lower_Common_model"]<-NA
  pooled_table_templet_temp[1,"CI_upper_Common_model"]<-NA
  pooled_table_templet_temp[1,"pval_Common_model"]<-NA
  pooled_table_templet_temp[1,"tau"]<-NA
  pooled_table_templet_temp[1,"I2"]<-NA
  pooled_table_templet_temp[1,"pval_Q"]<-NA
  pooled_table<-rbind(pooled_table,pooled_table_templet_temp)
}



#### 02_01 Subgroup_Gender end ####

output_dir<-paste0(output_dir_root,"//03_meta_analysis//02_02 Subgroup_IDH_status")
dir.create(output_dir)
#### 02_02 Subgroup_IDH_status start ####
#### 02_02_01_01 WT; OS; dichotomous CD8 ####
# 02_02_01 WT
meta_table<-subset(sum_table,IDH_status=="WT")
# 02_02_01_01 WT; OS
meta_table<-subset(meta_table,Outcome=="OS")
# 02_02_01_01 WT; OS; dichotomous CD8
meta_table<-subset(meta_table,PatientNumInCD8PositiveGroup!="Cox continue CD8")
# exclude results without convergence
# no exclusion
# meta-analysis
meta_result<-metagen(logHR, selogHR, sm = "HR",studlab = ResearchName,data=meta_table)
# forest plot
pdf(paste0(output_dir,"//ForestPlot_Subgroup_IDH_status_WT_OS_dichotomousCD8.pdf"),width=10, height=4)
p_forest<-forest(meta_result,print.Q =T,digits=4,digits.pval = 3,digits.pval.Q = 3,
                 test.overall.common = TRUE,
                 test.overall.random = TRUE)
print(p_forest)
dev.off()
# funnel plot
pdf(paste0(output_dir,"//FunnelPlot_Subgroup_IDH_status_WT_OS_dichotomousCD8.pdf"),width=6, height=6)
p_funnel<-funnel(meta_result)
print(p_funnel)
dev.off()


# add results to pooled table
pooled_table_templet_temp<-pooled_table_templet
pooled_table_templet_temp[1,"Subgroup"]<-"WT"
pooled_table_templet_temp[1,"Outcome"]<-"OS"
pooled_table_templet_temp[1,"Study_number"]<-nrow(meta_table)
if(nrow(meta_table)>=1){
  pooled_table_templet_temp[1,"Patient_numbers"]<-sum(as.numeric(meta_table$PatientNumInCD8PositiveGroup),as.numeric(meta_table$PatientNumInCD8NegativeGroup))
  pooled_table_templet_temp[1,"HR_Random_model"]<-round(exp(meta_result$TE.random),digits = 4)
  pooled_table_templet_temp[1,"CI_lower_Random_model"]<-round(exp(meta_result$lower.random),digits = 4)
  pooled_table_templet_temp[1,"CI_upper_Random_model"]<-round(exp(meta_result$upper.random),digits = 4)
  pooled_table_templet_temp[1,"pval_Random_model"]<-round(meta_result$pval.random,digits = 4)
  pooled_table_templet_temp[1,"HR_Common_model"]<-round(exp(meta_result$TE.common),digits = 4)
  pooled_table_templet_temp[1,"CI_lower_Common_model"]<-round(exp(meta_result$lower.common),digits = 4)
  pooled_table_templet_temp[1,"CI_upper_Common_model"]<-round(exp(meta_result$upper.common),digits = 4)
  pooled_table_templet_temp[1,"pval_Common_model"]<-round(meta_result$pval.common,digits = 4)
  pooled_table_templet_temp[1,"tau"]<-round(meta_result$tau,digits = 4)
  pooled_table_templet_temp[1,"I2"]<-paste0(as.character(round(100*meta_result$I2,digits = 1)),"%")
  pooled_table_templet_temp[1,"pval_Q"]<-round(meta_result$pval.Q,digits = 4)
  pooled_table<-rbind(pooled_table,pooled_table_templet_temp)
} else {
  pooled_table_templet_temp[1,"Patient_numbers"]<-0
  pooled_table_templet_temp[1,"HR_Random_model"]<-NA
  pooled_table_templet_temp[1,"CI_lower_Random_model"]<-NA
  pooled_table_templet_temp[1,"CI_upper_Random_model"]<-NA
  pooled_table_templet_temp[1,"pval_Random_model"]<-NA
  pooled_table_templet_temp[1,"HR_Common_model"]<-NA
  pooled_table_templet_temp[1,"CI_lower_Common_model"]<-NA
  pooled_table_templet_temp[1,"CI_upper_Common_model"]<-NA
  pooled_table_templet_temp[1,"pval_Common_model"]<-NA
  pooled_table_templet_temp[1,"tau"]<-NA
  pooled_table_templet_temp[1,"I2"]<-NA
  pooled_table_templet_temp[1,"pval_Q"]<-NA
  pooled_table<-rbind(pooled_table,pooled_table_templet_temp)
}

#### 02_02_01_02 WT; PFS; dichotomous CD8 ####
# 02_02_01 WT
meta_table<-subset(sum_table,IDH_status=="WT")
# 02_02_01_02 WT; PFS
meta_table<-subset(meta_table,Outcome=="PFS")
# 02_02_01_02 WT; PFS; dichotomous CD8
meta_table<-subset(meta_table,PatientNumInCD8PositiveGroup!="Cox continue CD8")
# exclude results without convergence
# no exclusion
# meta-analysis
meta_result<-metagen(logHR, selogHR, sm = "HR",studlab = ResearchName,data=meta_table)
# forest plot
pdf(paste0(output_dir,"//ForestPlot_Subgroup_IDH_status_WT_PFS_dichotomousCD8.pdf"),width=10, height=4)
p_forest<-forest(meta_result,print.Q =T,digits=4,digits.pval = 3,digits.pval.Q = 3,
                 test.overall.common = TRUE,
                 test.overall.random = TRUE)
print(p_forest)
dev.off()
# funnel plot
pdf(paste0(output_dir,"//FunnelPlot_Subgroup_IDH_status_WT_PFS_dichotomousCD8.pdf"),width=6, height=6)
p_funnel<-funnel(meta_result)
print(p_funnel)
dev.off()


# add results to pooled table
pooled_table_templet_temp<-pooled_table_templet
pooled_table_templet_temp[1,"Subgroup"]<-"WT"
pooled_table_templet_temp[1,"Outcome"]<-"PFS"
pooled_table_templet_temp[1,"Study_number"]<-nrow(meta_table)
if(nrow(meta_table)>=1){
  pooled_table_templet_temp[1,"Patient_numbers"]<-sum(as.numeric(meta_table$PatientNumInCD8PositiveGroup),as.numeric(meta_table$PatientNumInCD8NegativeGroup))
  pooled_table_templet_temp[1,"HR_Random_model"]<-round(exp(meta_result$TE.random),digits = 4)
  pooled_table_templet_temp[1,"CI_lower_Random_model"]<-round(exp(meta_result$lower.random),digits = 4)
  pooled_table_templet_temp[1,"CI_upper_Random_model"]<-round(exp(meta_result$upper.random),digits = 4)
  pooled_table_templet_temp[1,"pval_Random_model"]<-round(meta_result$pval.random,digits = 4)
  pooled_table_templet_temp[1,"HR_Common_model"]<-round(exp(meta_result$TE.common),digits = 4)
  pooled_table_templet_temp[1,"CI_lower_Common_model"]<-round(exp(meta_result$lower.common),digits = 4)
  pooled_table_templet_temp[1,"CI_upper_Common_model"]<-round(exp(meta_result$upper.common),digits = 4)
  pooled_table_templet_temp[1,"pval_Common_model"]<-round(meta_result$pval.common,digits = 4)
  pooled_table_templet_temp[1,"tau"]<-round(meta_result$tau,digits = 4)
  pooled_table_templet_temp[1,"I2"]<-paste0(as.character(round(100*meta_result$I2,digits = 1)),"%")
  pooled_table_templet_temp[1,"pval_Q"]<-round(meta_result$pval.Q,digits = 4)
  pooled_table<-rbind(pooled_table,pooled_table_templet_temp)
} else {
  pooled_table_templet_temp[1,"Patient_numbers"]<-0
  pooled_table_templet_temp[1,"HR_Random_model"]<-NA
  pooled_table_templet_temp[1,"CI_lower_Random_model"]<-NA
  pooled_table_templet_temp[1,"CI_upper_Random_model"]<-NA
  pooled_table_templet_temp[1,"pval_Random_model"]<-NA
  pooled_table_templet_temp[1,"HR_Common_model"]<-NA
  pooled_table_templet_temp[1,"CI_lower_Common_model"]<-NA
  pooled_table_templet_temp[1,"CI_upper_Common_model"]<-NA
  pooled_table_templet_temp[1,"pval_Common_model"]<-NA
  pooled_table_templet_temp[1,"tau"]<-NA
  pooled_table_templet_temp[1,"I2"]<-NA
  pooled_table_templet_temp[1,"pval_Q"]<-NA
  pooled_table<-rbind(pooled_table,pooled_table_templet_temp)
}
#### 02_02_01_03 WT; Survival after immunotherapy; dichotomous CD8 ####
# 02_02_01 WT
meta_table<-subset(sum_table,IDH_status=="WT")
# 02_02_01_03 WT; Survival after immunotherapy
meta_table<-subset(meta_table,Outcome=="Survival after immunotherapy")
# 02_02_01_03 WT; Survival after immunotherapy; dichotomous CD8
meta_table<-subset(meta_table,PatientNumInCD8PositiveGroup!="Cox continue CD8")
# exclude results without convergence
meta_table<-meta_table[(meta_table$PMID!="PMID37148198"),]
# exclude duplicate
meta_table<-subset(meta_table,Gender=="Mixed")
# meta-analysis
meta_result<-metagen(logHR, selogHR, sm = "HR",studlab = ResearchName,data=meta_table)
# forest plot
pdf(paste0(output_dir,"//ForestPlot_Subgroup_IDH_status_WT_SurvivalAfterImmunotherapy_dichotomousCD8.pdf"),width=10, height=4)
p_forest<-forest(meta_result,print.Q =T,digits=4,digits.pval = 3,digits.pval.Q = 3,
                 test.overall.common = TRUE,
                 test.overall.random = TRUE)
print(p_forest)
dev.off()
# funnel plot
pdf(paste0(output_dir,"//FunnelPlot_Subgroup_IDH_status_WT_SurvivalAfterImmunotherapy_dichotomousCD8.pdf"),width=6, height=6)
p_funnel<-funnel(meta_result)
print(p_funnel)
dev.off()


# add results to pooled table
pooled_table_templet_temp<-pooled_table_templet
pooled_table_templet_temp[1,"Subgroup"]<-"WT"
pooled_table_templet_temp[1,"Outcome"]<-"Survival after immunotherapy"
pooled_table_templet_temp[1,"Study_number"]<-nrow(meta_table)
if(nrow(meta_table)>=1){
  pooled_table_templet_temp[1,"Patient_numbers"]<-sum(as.numeric(meta_table$PatientNumInCD8PositiveGroup),as.numeric(meta_table$PatientNumInCD8NegativeGroup))
  pooled_table_templet_temp[1,"HR_Random_model"]<-round(exp(meta_result$TE.random),digits = 4)
  pooled_table_templet_temp[1,"CI_lower_Random_model"]<-round(exp(meta_result$lower.random),digits = 4)
  pooled_table_templet_temp[1,"CI_upper_Random_model"]<-round(exp(meta_result$upper.random),digits = 4)
  pooled_table_templet_temp[1,"pval_Random_model"]<-round(meta_result$pval.random,digits = 4)
  pooled_table_templet_temp[1,"HR_Common_model"]<-round(exp(meta_result$TE.common),digits = 4)
  pooled_table_templet_temp[1,"CI_lower_Common_model"]<-round(exp(meta_result$lower.common),digits = 4)
  pooled_table_templet_temp[1,"CI_upper_Common_model"]<-round(exp(meta_result$upper.common),digits = 4)
  pooled_table_templet_temp[1,"pval_Common_model"]<-round(meta_result$pval.common,digits = 4)
  pooled_table_templet_temp[1,"tau"]<-round(meta_result$tau,digits = 4)
  pooled_table_templet_temp[1,"I2"]<-paste0(as.character(round(100*meta_result$I2,digits = 1)),"%")
  pooled_table_templet_temp[1,"pval_Q"]<-round(meta_result$pval.Q,digits = 4)
  pooled_table<-rbind(pooled_table,pooled_table_templet_temp)
} else {
  pooled_table_templet_temp[1,"Patient_numbers"]<-0
  pooled_table_templet_temp[1,"HR_Random_model"]<-NA
  pooled_table_templet_temp[1,"CI_lower_Random_model"]<-NA
  pooled_table_templet_temp[1,"CI_upper_Random_model"]<-NA
  pooled_table_templet_temp[1,"pval_Random_model"]<-NA
  pooled_table_templet_temp[1,"HR_Common_model"]<-NA
  pooled_table_templet_temp[1,"CI_lower_Common_model"]<-NA
  pooled_table_templet_temp[1,"CI_upper_Common_model"]<-NA
  pooled_table_templet_temp[1,"pval_Common_model"]<-NA
  pooled_table_templet_temp[1,"tau"]<-NA
  pooled_table_templet_temp[1,"I2"]<-NA
  pooled_table_templet_temp[1,"pval_Q"]<-NA
  pooled_table<-rbind(pooled_table,pooled_table_templet_temp)
}
#### 02_02_02_01 MUT; OS; dichotomous CD8 ####
# 02_02_02 MUT
meta_table<-subset(sum_table,IDH_status=="MUT")
# 02_02_02_01 MUT; OS
meta_table<-subset(meta_table,Outcome=="OS")
# 02_02_02_01 MUT; OS; dichotomous CD8
meta_table<-subset(meta_table,PatientNumInCD8PositiveGroup!="Cox continue CD8")
# exclude results without convergence
meta_table<-meta_table[(meta_table$PMID!="PMID33130898"),]
# meta-analysis
# no study left
# meta_result<-metagen(logHR, selogHR, sm = "HR",studlab = ResearchName,data=meta_table)
# # forest plot
# pdf(paste0(output_dir,"//ForestPlot_Subgroup_IDH_status_MUT_OS_dichotomousCD8.pdf"),width=10, height=4)
# p_forest<-forest(meta_result,print.Q =T,digits=4,digits.pval = 3,digits.pval.Q = 3,
#                  test.overall.common = TRUE,
#                  test.overall.random = TRUE)
# print(p_forest)
# dev.off()
# # funnel plot
# pdf(paste0(output_dir,"//FunnelPlot_Subgroup_IDH_status_MUT_OS_dichotomousCD8.pdf"),width=6, height=6)
# p_funnel<-funnel(meta_result)
# print(p_funnel)
# dev.off()


# add results to pooled table
pooled_table_templet_temp<-pooled_table_templet
pooled_table_templet_temp[1,"Subgroup"]<-"MUT"
pooled_table_templet_temp[1,"Outcome"]<-"OS"
pooled_table_templet_temp[1,"Study_number"]<-nrow(meta_table)
if(nrow(meta_table)>=1){
  pooled_table_templet_temp[1,"Patient_numbers"]<-sum(as.numeric(meta_table$PatientNumInCD8PositiveGroup),as.numeric(meta_table$PatientNumInCD8NegativeGroup))
  pooled_table_templet_temp[1,"HR_Random_model"]<-round(exp(meta_result$TE.random),digits = 4)
  pooled_table_templet_temp[1,"CI_lower_Random_model"]<-round(exp(meta_result$lower.random),digits = 4)
  pooled_table_templet_temp[1,"CI_upper_Random_model"]<-round(exp(meta_result$upper.random),digits = 4)
  pooled_table_templet_temp[1,"pval_Random_model"]<-round(meta_result$pval.random,digits = 4)
  pooled_table_templet_temp[1,"HR_Common_model"]<-round(exp(meta_result$TE.common),digits = 4)
  pooled_table_templet_temp[1,"CI_lower_Common_model"]<-round(exp(meta_result$lower.common),digits = 4)
  pooled_table_templet_temp[1,"CI_upper_Common_model"]<-round(exp(meta_result$upper.common),digits = 4)
  pooled_table_templet_temp[1,"pval_Common_model"]<-round(meta_result$pval.common,digits = 4)
  pooled_table_templet_temp[1,"tau"]<-round(meta_result$tau,digits = 4)
  pooled_table_templet_temp[1,"I2"]<-paste0(as.character(round(100*meta_result$I2,digits = 1)),"%")
  pooled_table_templet_temp[1,"pval_Q"]<-round(meta_result$pval.Q,digits = 4)
  pooled_table<-rbind(pooled_table,pooled_table_templet_temp)
} else {
  pooled_table_templet_temp[1,"Patient_numbers"]<-0
  pooled_table_templet_temp[1,"HR_Random_model"]<-NA
  pooled_table_templet_temp[1,"CI_lower_Random_model"]<-NA
  pooled_table_templet_temp[1,"CI_upper_Random_model"]<-NA
  pooled_table_templet_temp[1,"pval_Random_model"]<-NA
  pooled_table_templet_temp[1,"HR_Common_model"]<-NA
  pooled_table_templet_temp[1,"CI_lower_Common_model"]<-NA
  pooled_table_templet_temp[1,"CI_upper_Common_model"]<-NA
  pooled_table_templet_temp[1,"pval_Common_model"]<-NA
  pooled_table_templet_temp[1,"tau"]<-NA
  pooled_table_templet_temp[1,"I2"]<-NA
  pooled_table_templet_temp[1,"pval_Q"]<-NA
  pooled_table<-rbind(pooled_table,pooled_table_templet_temp)
}
#### 02_02_02_02 MUT; PFS; dichotomous CD8 ####
# 02_02_02 MUT
meta_table<-subset(sum_table,IDH_status=="MUT")
# 02_02_02_02 MUT; PFS
meta_table<-subset(meta_table,Outcome=="PFS")
# 02_02_02_02 MUT; PFS; dichotomous CD8
meta_table<-subset(meta_table,PatientNumInCD8PositiveGroup!="Cox continue CD8")
# exclude results without convergence
# no exclusion
# meta-analysis
# no study left
# meta_result<-metagen(logHR, selogHR, sm = "HR",studlab = ResearchName,data=meta_table)
# # forest plot
# pdf(paste0(output_dir,"//ForestPlot_Subgroup_IDH_status_MUT_PFS_dichotomousCD8.pdf"),width=10, height=4)
# p_forest<-forest(meta_result,print.Q =T,digits=4,digits.pval = 3,digits.pval.Q = 3,
#                  test.overall.common = TRUE,
#                  test.overall.random = TRUE)
# print(p_forest)
# dev.off()
# # funnel plot
# pdf(paste0(output_dir,"//FunnelPlot_Subgroup_IDH_status_MUT_PFS_dichotomousCD8.pdf"),width=6, height=6)
# p_funnel<-funnel(meta_result)
# print(p_funnel)
# dev.off()

# add results to pooled table
pooled_table_templet_temp<-pooled_table_templet
pooled_table_templet_temp[1,"Subgroup"]<-"MUT"
pooled_table_templet_temp[1,"Outcome"]<-"PFS"
pooled_table_templet_temp[1,"Study_number"]<-nrow(meta_table)
if(nrow(meta_table)>=1){
  pooled_table_templet_temp[1,"Patient_numbers"]<-sum(as.numeric(meta_table$PatientNumInCD8PositiveGroup),as.numeric(meta_table$PatientNumInCD8NegativeGroup))
  pooled_table_templet_temp[1,"HR_Random_model"]<-round(exp(meta_result$TE.random),digits = 4)
  pooled_table_templet_temp[1,"CI_lower_Random_model"]<-round(exp(meta_result$lower.random),digits = 4)
  pooled_table_templet_temp[1,"CI_upper_Random_model"]<-round(exp(meta_result$upper.random),digits = 4)
  pooled_table_templet_temp[1,"pval_Random_model"]<-round(meta_result$pval.random,digits = 4)
  pooled_table_templet_temp[1,"HR_Common_model"]<-round(exp(meta_result$TE.common),digits = 4)
  pooled_table_templet_temp[1,"CI_lower_Common_model"]<-round(exp(meta_result$lower.common),digits = 4)
  pooled_table_templet_temp[1,"CI_upper_Common_model"]<-round(exp(meta_result$upper.common),digits = 4)
  pooled_table_templet_temp[1,"pval_Common_model"]<-round(meta_result$pval.common,digits = 4)
  pooled_table_templet_temp[1,"tau"]<-round(meta_result$tau,digits = 4)
  pooled_table_templet_temp[1,"I2"]<-paste0(as.character(round(100*meta_result$I2,digits = 1)),"%")
  pooled_table_templet_temp[1,"pval_Q"]<-round(meta_result$pval.Q,digits = 4)
  pooled_table<-rbind(pooled_table,pooled_table_templet_temp)
} else {
  pooled_table_templet_temp[1,"Patient_numbers"]<-0
  pooled_table_templet_temp[1,"HR_Random_model"]<-NA
  pooled_table_templet_temp[1,"CI_lower_Random_model"]<-NA
  pooled_table_templet_temp[1,"CI_upper_Random_model"]<-NA
  pooled_table_templet_temp[1,"pval_Random_model"]<-NA
  pooled_table_templet_temp[1,"HR_Common_model"]<-NA
  pooled_table_templet_temp[1,"CI_lower_Common_model"]<-NA
  pooled_table_templet_temp[1,"CI_upper_Common_model"]<-NA
  pooled_table_templet_temp[1,"pval_Common_model"]<-NA
  pooled_table_templet_temp[1,"tau"]<-NA
  pooled_table_templet_temp[1,"I2"]<-NA
  pooled_table_templet_temp[1,"pval_Q"]<-NA
  pooled_table<-rbind(pooled_table,pooled_table_templet_temp)
}

#### 02_02_02_03 MUT; Survival after immunotherapy; dichotomous CD8 ####
# 02_02_02 MUT
meta_table<-subset(sum_table,IDH_status=="MUT")
# 02_02_02_02 MUT; Survival after immunotherapy
meta_table<-subset(meta_table,Outcome=="Survival after immunotherapy")
# 02_02_02_02 MUT; Survival after immunotherapy; dichotomous CD8
meta_table<-subset(meta_table,PatientNumInCD8PositiveGroup!="Cox continue CD8")
# exclude results without convergence
# no exclusion
# meta-analysis
# no study left
# meta_result<-metagen(logHR, selogHR, sm = "HR",studlab = ResearchName,data=meta_table)
# # forest plot
# pdf(paste0(output_dir,"//ForestPlot_Subgroup_IDH_status_MUT_SurvivalAfterImmunotherapy_dichotomousCD8.pdf"),width=10, height=4)
# p_forest<-forest(meta_result,print.Q =T,digits=4,digits.pval = 3,digits.pval.Q = 3,
#                  test.overall.common = TRUE,
#                  test.overall.random = TRUE)
# print(p_forest)
# dev.off()
# # funnel plot
# pdf(paste0(output_dir,"//FunnelPlot_Subgroup_IDH_status_MUT_SurvivalAfterImmunotherapy_dichotomousCD8.pdf"),width=6, height=6)
# p_funnel<-funnel(meta_result)
# print(p_funnel)
# dev.off()

# add results to pooled table
pooled_table_templet_temp<-pooled_table_templet
pooled_table_templet_temp[1,"Subgroup"]<-"MUT"
pooled_table_templet_temp[1,"Outcome"]<-"Survival after immunotherapy"
pooled_table_templet_temp[1,"Study_number"]<-nrow(meta_table)
if(nrow(meta_table)>=1){
  pooled_table_templet_temp[1,"Patient_numbers"]<-sum(as.numeric(meta_table$PatientNumInCD8PositiveGroup),as.numeric(meta_table$PatientNumInCD8NegativeGroup))
  pooled_table_templet_temp[1,"HR_Random_model"]<-round(exp(meta_result$TE.random),digits = 4)
  pooled_table_templet_temp[1,"CI_lower_Random_model"]<-round(exp(meta_result$lower.random),digits = 4)
  pooled_table_templet_temp[1,"CI_upper_Random_model"]<-round(exp(meta_result$upper.random),digits = 4)
  pooled_table_templet_temp[1,"pval_Random_model"]<-round(meta_result$pval.random,digits = 4)
  pooled_table_templet_temp[1,"HR_Common_model"]<-round(exp(meta_result$TE.common),digits = 4)
  pooled_table_templet_temp[1,"CI_lower_Common_model"]<-round(exp(meta_result$lower.common),digits = 4)
  pooled_table_templet_temp[1,"CI_upper_Common_model"]<-round(exp(meta_result$upper.common),digits = 4)
  pooled_table_templet_temp[1,"pval_Common_model"]<-round(meta_result$pval.common,digits = 4)
  pooled_table_templet_temp[1,"tau"]<-round(meta_result$tau,digits = 4)
  pooled_table_templet_temp[1,"I2"]<-paste0(as.character(round(100*meta_result$I2,digits = 1)),"%")
  pooled_table_templet_temp[1,"pval_Q"]<-round(meta_result$pval.Q,digits = 4)
  pooled_table<-rbind(pooled_table,pooled_table_templet_temp)
} else {
  pooled_table_templet_temp[1,"Patient_numbers"]<-0
  pooled_table_templet_temp[1,"HR_Random_model"]<-NA
  pooled_table_templet_temp[1,"CI_lower_Random_model"]<-NA
  pooled_table_templet_temp[1,"CI_upper_Random_model"]<-NA
  pooled_table_templet_temp[1,"pval_Random_model"]<-NA
  pooled_table_templet_temp[1,"HR_Common_model"]<-NA
  pooled_table_templet_temp[1,"CI_lower_Common_model"]<-NA
  pooled_table_templet_temp[1,"CI_upper_Common_model"]<-NA
  pooled_table_templet_temp[1,"pval_Common_model"]<-NA
  pooled_table_templet_temp[1,"tau"]<-NA
  pooled_table_templet_temp[1,"I2"]<-NA
  pooled_table_templet_temp[1,"pval_Q"]<-NA
  pooled_table<-rbind(pooled_table,pooled_table_templet_temp)
}

#### 02_02 Subgroup_IDH_status end ####

output_dir<-paste0(output_dir_root,"//03_meta_analysis//02_03 Subgroup_MGMT_status")
dir.create(output_dir)
#### 02_03 Subgroup_MGMT_status start ####
#### 02_03_01_01 unmethylated; OS; dichotomous CD8 ####
# 02_03_01 unmethylated
meta_table<-subset(sum_table,MGMT_status=="unmethylated")
# 02_03_01_01 unmethylated; OS
meta_table<-subset(meta_table,Outcome=="OS")
# 02_03_01_01 unmethylated; OS; dichotomous CD8
meta_table<-subset(meta_table,PatientNumInCD8PositiveGroup!="Cox continue CD8")
# exclude results without convergence
meta_table<-meta_table[(meta_table$PMID!="PMID29910795"),]
# meta-analysis
meta_result<-metagen(logHR, selogHR, sm = "HR",studlab = ResearchName,data=meta_table)
# forest plot
pdf(paste0(output_dir,"//ForestPlot_Subgroup_MGMT_status_unmethylated_OS_dichotomousCD8.pdf"),width=10, height=4)
p_forest<-forest(meta_result,print.Q =T,digits=4,digits.pval = 3,digits.pval.Q = 3,
                 test.overall.common = TRUE,
                 test.overall.random = TRUE)
print(p_forest)
dev.off()
# funnel plot
pdf(paste0(output_dir,"//FunnelPlot_Subgroup_MGMT_status_unmethylated_OS_dichotomousCD8.pdf"),width=6, height=6)
p_funnel<-funnel(meta_result)
print(p_funnel)
dev.off()

# add results to pooled table
pooled_table_templet_temp<-pooled_table_templet
pooled_table_templet_temp[1,"Subgroup"]<-"unmethylated"
pooled_table_templet_temp[1,"Outcome"]<-"OS"
pooled_table_templet_temp[1,"Study_number"]<-nrow(meta_table)
if(nrow(meta_table)>=1){
  pooled_table_templet_temp[1,"Patient_numbers"]<-sum(as.numeric(meta_table$PatientNumInCD8PositiveGroup),as.numeric(meta_table$PatientNumInCD8NegativeGroup))
  pooled_table_templet_temp[1,"HR_Random_model"]<-round(exp(meta_result$TE.random),digits = 4)
  pooled_table_templet_temp[1,"CI_lower_Random_model"]<-round(exp(meta_result$lower.random),digits = 4)
  pooled_table_templet_temp[1,"CI_upper_Random_model"]<-round(exp(meta_result$upper.random),digits = 4)
  pooled_table_templet_temp[1,"pval_Random_model"]<-round(meta_result$pval.random,digits = 4)
  pooled_table_templet_temp[1,"HR_Common_model"]<-round(exp(meta_result$TE.common),digits = 4)
  pooled_table_templet_temp[1,"CI_lower_Common_model"]<-round(exp(meta_result$lower.common),digits = 4)
  pooled_table_templet_temp[1,"CI_upper_Common_model"]<-round(exp(meta_result$upper.common),digits = 4)
  pooled_table_templet_temp[1,"pval_Common_model"]<-round(meta_result$pval.common,digits = 4)
  pooled_table_templet_temp[1,"tau"]<-round(meta_result$tau,digits = 4)
  pooled_table_templet_temp[1,"I2"]<-paste0(as.character(round(100*meta_result$I2,digits = 1)),"%")
  pooled_table_templet_temp[1,"pval_Q"]<-round(meta_result$pval.Q,digits = 4)
  pooled_table<-rbind(pooled_table,pooled_table_templet_temp)
} else {
  pooled_table_templet_temp[1,"Patient_numbers"]<-0
  pooled_table_templet_temp[1,"HR_Random_model"]<-NA
  pooled_table_templet_temp[1,"CI_lower_Random_model"]<-NA
  pooled_table_templet_temp[1,"CI_upper_Random_model"]<-NA
  pooled_table_templet_temp[1,"pval_Random_model"]<-NA
  pooled_table_templet_temp[1,"HR_Common_model"]<-NA
  pooled_table_templet_temp[1,"CI_lower_Common_model"]<-NA
  pooled_table_templet_temp[1,"CI_upper_Common_model"]<-NA
  pooled_table_templet_temp[1,"pval_Common_model"]<-NA
  pooled_table_templet_temp[1,"tau"]<-NA
  pooled_table_templet_temp[1,"I2"]<-NA
  pooled_table_templet_temp[1,"pval_Q"]<-NA
  pooled_table<-rbind(pooled_table,pooled_table_templet_temp)
}
#### 02_03_01_02 unmethylated; PFS; dichotomous CD8 ####
# 02_03_01 unmethylated
meta_table<-subset(sum_table,MGMT_status=="unmethylated")
# 02_03_01_02 unmethylated; PFS
meta_table<-subset(meta_table,Outcome=="PFS")
# 02_03_01_02 unmethylated; PFS; dichotomous CD8
meta_table<-subset(meta_table,PatientNumInCD8PositiveGroup!="Cox continue CD8")
# exclude results without convergence
meta_table<-meta_table[(meta_table$PMID!="PMID29910795"),]
# meta-analysis
meta_result<-metagen(logHR, selogHR, sm = "HR",studlab = ResearchName,data=meta_table)
# forest plot
pdf(paste0(output_dir,"//ForestPlot_Subgroup_MGMT_status_unmethylated_PFS_dichotomousCD8.pdf"),width=10, height=4)
p_forest<-forest(meta_result,print.Q =T,digits=4,digits.pval = 3,digits.pval.Q = 3,
                 test.overall.common = TRUE,
                 test.overall.random = TRUE)
print(p_forest)
dev.off()
# funnel plot
pdf(paste0(output_dir,"//FunnelPlot_Subgroup_MGMT_status_unmethylated_PFS_dichotomousCD8.pdf"),width=6, height=6)
p_funnel<-funnel(meta_result)
print(p_funnel)
dev.off()

# add results to pooled table
pooled_table_templet_temp<-pooled_table_templet
pooled_table_templet_temp[1,"Subgroup"]<-"unmethylated"
pooled_table_templet_temp[1,"Outcome"]<-"PFS"
pooled_table_templet_temp[1,"Study_number"]<-nrow(meta_table)
if(nrow(meta_table)>=1){
  pooled_table_templet_temp[1,"Patient_numbers"]<-sum(as.numeric(meta_table$PatientNumInCD8PositiveGroup),as.numeric(meta_table$PatientNumInCD8NegativeGroup))
  pooled_table_templet_temp[1,"HR_Random_model"]<-round(exp(meta_result$TE.random),digits = 4)
  pooled_table_templet_temp[1,"CI_lower_Random_model"]<-round(exp(meta_result$lower.random),digits = 4)
  pooled_table_templet_temp[1,"CI_upper_Random_model"]<-round(exp(meta_result$upper.random),digits = 4)
  pooled_table_templet_temp[1,"pval_Random_model"]<-round(meta_result$pval.random,digits = 4)
  pooled_table_templet_temp[1,"HR_Common_model"]<-round(exp(meta_result$TE.common),digits = 4)
  pooled_table_templet_temp[1,"CI_lower_Common_model"]<-round(exp(meta_result$lower.common),digits = 4)
  pooled_table_templet_temp[1,"CI_upper_Common_model"]<-round(exp(meta_result$upper.common),digits = 4)
  pooled_table_templet_temp[1,"pval_Common_model"]<-round(meta_result$pval.common,digits = 4)
  pooled_table_templet_temp[1,"tau"]<-round(meta_result$tau,digits = 4)
  pooled_table_templet_temp[1,"I2"]<-paste0(as.character(round(100*meta_result$I2,digits = 1)),"%")
  pooled_table_templet_temp[1,"pval_Q"]<-round(meta_result$pval.Q,digits = 4)
  pooled_table<-rbind(pooled_table,pooled_table_templet_temp)
} else {
  pooled_table_templet_temp[1,"Patient_numbers"]<-0
  pooled_table_templet_temp[1,"HR_Random_model"]<-NA
  pooled_table_templet_temp[1,"CI_lower_Random_model"]<-NA
  pooled_table_templet_temp[1,"CI_upper_Random_model"]<-NA
  pooled_table_templet_temp[1,"pval_Random_model"]<-NA
  pooled_table_templet_temp[1,"HR_Common_model"]<-NA
  pooled_table_templet_temp[1,"CI_lower_Common_model"]<-NA
  pooled_table_templet_temp[1,"CI_upper_Common_model"]<-NA
  pooled_table_templet_temp[1,"pval_Common_model"]<-NA
  pooled_table_templet_temp[1,"tau"]<-NA
  pooled_table_templet_temp[1,"I2"]<-NA
  pooled_table_templet_temp[1,"pval_Q"]<-NA
  pooled_table<-rbind(pooled_table,pooled_table_templet_temp)
}

#### 02_03_01_03 unmethylated; Survival after immunotherapy; dichotomous CD8 ####
# 02_03_01 unmethylated
meta_table<-subset(sum_table,MGMT_status=="unmethylated")
# 02_03_01_03 unmethylated; Survival after immunotherapy
meta_table<-subset(meta_table,Outcome=="Survival after immunotherapy")
# 02_03_01_03 unmethylated; Survival after immunotherapy; dichotomous CD8
meta_table<-subset(meta_table,PatientNumInCD8PositiveGroup!="Cox continue CD8")
# exclude results without convergence
meta_table<-meta_table[(meta_table$PMID!="PMID37148198"),]
# meta-analysis
# no study left 
# meta_result<-metagen(logHR, selogHR, sm = "HR",studlab = ResearchName,data=meta_table)
# # forest plot
# pdf(paste0(output_dir,"//ForestPlot_Subgroup_MGMT_status_unmethylated_SurvivalAfterImmunotherapy_dichotomousCD8.pdf"),width=10, height=4)
# p_forest<-forest(meta_result,print.Q =T,digits=4,digits.pval = 3,digits.pval.Q = 3,
#                  test.overall.common = TRUE,
#                  test.overall.random = TRUE)
# print(p_forest)
# dev.off()
# # funnel plot
# pdf(paste0(output_dir,"//FunnelPlot_Subgroup_MGMT_status_unmethylated_SurvivalAfterImmunotherapy_dichotomousCD8.pdf"),width=6, height=6)
# p_funnel<-funnel(meta_result)
# print(p_funnel)
# dev.off()

# add results to pooled table
pooled_table_templet_temp<-pooled_table_templet
pooled_table_templet_temp[1,"Subgroup"]<-"unmethylated"
pooled_table_templet_temp[1,"Outcome"]<-"Survival after immunotherapy"
pooled_table_templet_temp[1,"Study_number"]<-nrow(meta_table)
if(nrow(meta_table)>=1){
  pooled_table_templet_temp[1,"Patient_numbers"]<-sum(as.numeric(meta_table$PatientNumInCD8PositiveGroup),as.numeric(meta_table$PatientNumInCD8NegativeGroup))
  pooled_table_templet_temp[1,"HR_Random_model"]<-round(exp(meta_result$TE.random),digits = 4)
  pooled_table_templet_temp[1,"CI_lower_Random_model"]<-round(exp(meta_result$lower.random),digits = 4)
  pooled_table_templet_temp[1,"CI_upper_Random_model"]<-round(exp(meta_result$upper.random),digits = 4)
  pooled_table_templet_temp[1,"pval_Random_model"]<-round(meta_result$pval.random,digits = 4)
  pooled_table_templet_temp[1,"HR_Common_model"]<-round(exp(meta_result$TE.common),digits = 4)
  pooled_table_templet_temp[1,"CI_lower_Common_model"]<-round(exp(meta_result$lower.common),digits = 4)
  pooled_table_templet_temp[1,"CI_upper_Common_model"]<-round(exp(meta_result$upper.common),digits = 4)
  pooled_table_templet_temp[1,"pval_Common_model"]<-round(meta_result$pval.common,digits = 4)
  pooled_table_templet_temp[1,"tau"]<-round(meta_result$tau,digits = 4)
  pooled_table_templet_temp[1,"I2"]<-paste0(as.character(round(100*meta_result$I2,digits = 1)),"%")
  pooled_table_templet_temp[1,"pval_Q"]<-round(meta_result$pval.Q,digits = 4)
  pooled_table<-rbind(pooled_table,pooled_table_templet_temp)
} else {
  pooled_table_templet_temp[1,"Patient_numbers"]<-0
  pooled_table_templet_temp[1,"HR_Random_model"]<-NA
  pooled_table_templet_temp[1,"CI_lower_Random_model"]<-NA
  pooled_table_templet_temp[1,"CI_upper_Random_model"]<-NA
  pooled_table_templet_temp[1,"pval_Random_model"]<-NA
  pooled_table_templet_temp[1,"HR_Common_model"]<-NA
  pooled_table_templet_temp[1,"CI_lower_Common_model"]<-NA
  pooled_table_templet_temp[1,"CI_upper_Common_model"]<-NA
  pooled_table_templet_temp[1,"pval_Common_model"]<-NA
  pooled_table_templet_temp[1,"tau"]<-NA
  pooled_table_templet_temp[1,"I2"]<-NA
  pooled_table_templet_temp[1,"pval_Q"]<-NA
  pooled_table<-rbind(pooled_table,pooled_table_templet_temp)
}

#### 02_03_02_01 methylated; OS; dichotomous CD8 ####
# 02_03_02 methylated
meta_table<-subset(sum_table,MGMT_status=="methylated")
# 02_03_02_01 methylated; OS
meta_table<-subset(meta_table,Outcome=="OS")
# 02_03_02_01 methylated; OS; dichotomous CD8
meta_table<-subset(meta_table,PatientNumInCD8PositiveGroup!="Cox continue CD8")
# exclude results without convergence
# no exclusion
# meta-analysis
meta_result<-metagen(logHR, selogHR, sm = "HR",studlab = ResearchName,data=meta_table)
# forest plot
pdf(paste0(output_dir,"//ForestPlot_Subgroup_MGMT_status_methylated_OS_dichotomousCD8.pdf"),width=10, height=4)
p_forest<-forest(meta_result,print.Q =T,digits=4,digits.pval = 3,digits.pval.Q = 3,
                 test.overall.common = TRUE,
                 test.overall.random = TRUE)
print(p_forest)
dev.off()
# funnel plot
pdf(paste0(output_dir,"//FunnelPlot_Subgroup_MGMT_status_methylated_OS_dichotomousCD8.pdf"),width=6, height=6)
p_funnel<-funnel(meta_result)
print(p_funnel)
dev.off()

# add results to pooled table
pooled_table_templet_temp<-pooled_table_templet
pooled_table_templet_temp[1,"Subgroup"]<-"methylated"
pooled_table_templet_temp[1,"Outcome"]<-"OS"
pooled_table_templet_temp[1,"Study_number"]<-nrow(meta_table)
if(nrow(meta_table)>=1){
  pooled_table_templet_temp[1,"Patient_numbers"]<-sum(as.numeric(meta_table$PatientNumInCD8PositiveGroup),as.numeric(meta_table$PatientNumInCD8NegativeGroup))
  pooled_table_templet_temp[1,"HR_Random_model"]<-round(exp(meta_result$TE.random),digits = 4)
  pooled_table_templet_temp[1,"CI_lower_Random_model"]<-round(exp(meta_result$lower.random),digits = 4)
  pooled_table_templet_temp[1,"CI_upper_Random_model"]<-round(exp(meta_result$upper.random),digits = 4)
  pooled_table_templet_temp[1,"pval_Random_model"]<-round(meta_result$pval.random,digits = 4)
  pooled_table_templet_temp[1,"HR_Common_model"]<-round(exp(meta_result$TE.common),digits = 4)
  pooled_table_templet_temp[1,"CI_lower_Common_model"]<-round(exp(meta_result$lower.common),digits = 4)
  pooled_table_templet_temp[1,"CI_upper_Common_model"]<-round(exp(meta_result$upper.common),digits = 4)
  pooled_table_templet_temp[1,"pval_Common_model"]<-round(meta_result$pval.common,digits = 4)
  pooled_table_templet_temp[1,"tau"]<-round(meta_result$tau,digits = 4)
  pooled_table_templet_temp[1,"I2"]<-paste0(as.character(round(100*meta_result$I2,digits = 1)),"%")
  pooled_table_templet_temp[1,"pval_Q"]<-round(meta_result$pval.Q,digits = 4)
  pooled_table<-rbind(pooled_table,pooled_table_templet_temp)
} else {
  pooled_table_templet_temp[1,"Patient_numbers"]<-0
  pooled_table_templet_temp[1,"HR_Random_model"]<-NA
  pooled_table_templet_temp[1,"CI_lower_Random_model"]<-NA
  pooled_table_templet_temp[1,"CI_upper_Random_model"]<-NA
  pooled_table_templet_temp[1,"pval_Random_model"]<-NA
  pooled_table_templet_temp[1,"HR_Common_model"]<-NA
  pooled_table_templet_temp[1,"CI_lower_Common_model"]<-NA
  pooled_table_templet_temp[1,"CI_upper_Common_model"]<-NA
  pooled_table_templet_temp[1,"pval_Common_model"]<-NA
  pooled_table_templet_temp[1,"tau"]<-NA
  pooled_table_templet_temp[1,"I2"]<-NA
  pooled_table_templet_temp[1,"pval_Q"]<-NA
  pooled_table<-rbind(pooled_table,pooled_table_templet_temp)
}

#### 02_03_02_02 methylated; PFS; dichotomous CD8 ####
# 02_03_02 methylated
meta_table<-subset(sum_table,MGMT_status=="methylated")
# 02_03_02_02 methylated; PFS
meta_table<-subset(meta_table,Outcome=="PFS")
# 02_03_02_02 methylated; PFS; dichotomous CD8
meta_table<-subset(meta_table,PatientNumInCD8PositiveGroup!="Cox continue CD8")
# exclude results without convergence
# no exclusion
# meta-analysis
meta_result<-metagen(logHR, selogHR, sm = "HR",studlab = ResearchName,data=meta_table)
# forest plot
pdf(paste0(output_dir,"//ForestPlot_Subgroup_MGMT_status_methylated_PFS_dichotomousCD8.pdf"),width=10, height=4)
p_forest<-forest(meta_result,print.Q =T,digits=4,digits.pval = 3,digits.pval.Q = 3,
                 test.overall.common = TRUE,
                 test.overall.random = TRUE)
print(p_forest)
dev.off()
# funnel plot
pdf(paste0(output_dir,"//FunnelPlot_Subgroup_MGMT_status_methylated_PFS_dichotomousCD8.pdf"),width=6, height=6)
p_funnel<-funnel(meta_result)
print(p_funnel)
dev.off()

# add results to pooled table
pooled_table_templet_temp<-pooled_table_templet
pooled_table_templet_temp[1,"Subgroup"]<-"methylated"
pooled_table_templet_temp[1,"Outcome"]<-"PFS"
pooled_table_templet_temp[1,"Study_number"]<-nrow(meta_table)
if(nrow(meta_table)>=1){
  pooled_table_templet_temp[1,"Patient_numbers"]<-sum(as.numeric(meta_table$PatientNumInCD8PositiveGroup),as.numeric(meta_table$PatientNumInCD8NegativeGroup))
  pooled_table_templet_temp[1,"HR_Random_model"]<-round(exp(meta_result$TE.random),digits = 4)
  pooled_table_templet_temp[1,"CI_lower_Random_model"]<-round(exp(meta_result$lower.random),digits = 4)
  pooled_table_templet_temp[1,"CI_upper_Random_model"]<-round(exp(meta_result$upper.random),digits = 4)
  pooled_table_templet_temp[1,"pval_Random_model"]<-round(meta_result$pval.random,digits = 4)
  pooled_table_templet_temp[1,"HR_Common_model"]<-round(exp(meta_result$TE.common),digits = 4)
  pooled_table_templet_temp[1,"CI_lower_Common_model"]<-round(exp(meta_result$lower.common),digits = 4)
  pooled_table_templet_temp[1,"CI_upper_Common_model"]<-round(exp(meta_result$upper.common),digits = 4)
  pooled_table_templet_temp[1,"pval_Common_model"]<-round(meta_result$pval.common,digits = 4)
  pooled_table_templet_temp[1,"tau"]<-round(meta_result$tau,digits = 4)
  pooled_table_templet_temp[1,"I2"]<-paste0(as.character(round(100*meta_result$I2,digits = 1)),"%")
  pooled_table_templet_temp[1,"pval_Q"]<-round(meta_result$pval.Q,digits = 4)
  pooled_table<-rbind(pooled_table,pooled_table_templet_temp)
} else {
  pooled_table_templet_temp[1,"Patient_numbers"]<-0
  pooled_table_templet_temp[1,"HR_Random_model"]<-NA
  pooled_table_templet_temp[1,"CI_lower_Random_model"]<-NA
  pooled_table_templet_temp[1,"CI_upper_Random_model"]<-NA
  pooled_table_templet_temp[1,"pval_Random_model"]<-NA
  pooled_table_templet_temp[1,"HR_Common_model"]<-NA
  pooled_table_templet_temp[1,"CI_lower_Common_model"]<-NA
  pooled_table_templet_temp[1,"CI_upper_Common_model"]<-NA
  pooled_table_templet_temp[1,"pval_Common_model"]<-NA
  pooled_table_templet_temp[1,"tau"]<-NA
  pooled_table_templet_temp[1,"I2"]<-NA
  pooled_table_templet_temp[1,"pval_Q"]<-NA
  pooled_table<-rbind(pooled_table,pooled_table_templet_temp)
}

#### 02_03_02_03 methylated; Survival after immunotherapy; dichotomous CD8 ####
# 02_03_02 methylated
meta_table<-subset(sum_table,MGMT_status=="methylated")
# 02_03_02_03 methylated; Survival after immunotherapy
meta_table<-subset(meta_table,Outcome=="Survival after immunotherapy")
# 02_03_02_03 methylated; Survival after immunotherapy; dichotomous CD8
meta_table<-subset(meta_table,PatientNumInCD8PositiveGroup!="Cox continue CD8")
# exclude results without convergence
meta_table<-meta_table[(meta_table$PMID!="PMID37148198"),]
# meta-analysis
# no study left 
# meta_result<-metagen(logHR, selogHR, sm = "HR",studlab = ResearchName,data=meta_table)
# # forest plot
# pdf(paste0(output_dir,"//ForestPlot_Subgroup_MGMT_status_methylated_SurvivalAfterImmunotherapy_dichotomousCD8.pdf"),width=10, height=4)
# p_forest<-forest(meta_result,print.Q =T,digits=4,digits.pval = 3,digits.pval.Q = 3,
#                  test.overall.common = TRUE,
#                  test.overall.random = TRUE)
# print(p_forest)
# dev.off()
# # funnel plot
# pdf(paste0(output_dir,"//FunnelPlot_Subgroup_MGMT_status_methylated_SurvivalAfterImmunotherapy_dichotomousCD8.pdf"),width=6, height=6)
# p_funnel<-funnel(meta_result)
# print(p_funnel)
# dev.off()

# add results to pooled table
pooled_table_templet_temp<-pooled_table_templet
pooled_table_templet_temp[1,"Subgroup"]<-"methylated"
pooled_table_templet_temp[1,"Outcome"]<-"Survival after immunotherapy"
pooled_table_templet_temp[1,"Study_number"]<-nrow(meta_table)
if(nrow(meta_table)>=1){
  pooled_table_templet_temp[1,"Patient_numbers"]<-sum(as.numeric(meta_table$PatientNumInCD8PositiveGroup),as.numeric(meta_table$PatientNumInCD8NegativeGroup))
  pooled_table_templet_temp[1,"HR_Random_model"]<-round(exp(meta_result$TE.random),digits = 4)
  pooled_table_templet_temp[1,"CI_lower_Random_model"]<-round(exp(meta_result$lower.random),digits = 4)
  pooled_table_templet_temp[1,"CI_upper_Random_model"]<-round(exp(meta_result$upper.random),digits = 4)
  pooled_table_templet_temp[1,"pval_Random_model"]<-round(meta_result$pval.random,digits = 4)
  pooled_table_templet_temp[1,"HR_Common_model"]<-round(exp(meta_result$TE.common),digits = 4)
  pooled_table_templet_temp[1,"CI_lower_Common_model"]<-round(exp(meta_result$lower.common),digits = 4)
  pooled_table_templet_temp[1,"CI_upper_Common_model"]<-round(exp(meta_result$upper.common),digits = 4)
  pooled_table_templet_temp[1,"pval_Common_model"]<-round(meta_result$pval.common,digits = 4)
  pooled_table_templet_temp[1,"tau"]<-round(meta_result$tau,digits = 4)
  pooled_table_templet_temp[1,"I2"]<-paste0(as.character(round(100*meta_result$I2,digits = 1)),"%")
  pooled_table_templet_temp[1,"pval_Q"]<-round(meta_result$pval.Q,digits = 4)
  pooled_table<-rbind(pooled_table,pooled_table_templet_temp)
} else {
  pooled_table_templet_temp[1,"Patient_numbers"]<-0
  pooled_table_templet_temp[1,"HR_Random_model"]<-NA
  pooled_table_templet_temp[1,"CI_lower_Random_model"]<-NA
  pooled_table_templet_temp[1,"CI_upper_Random_model"]<-NA
  pooled_table_templet_temp[1,"pval_Random_model"]<-NA
  pooled_table_templet_temp[1,"HR_Common_model"]<-NA
  pooled_table_templet_temp[1,"CI_lower_Common_model"]<-NA
  pooled_table_templet_temp[1,"CI_upper_Common_model"]<-NA
  pooled_table_templet_temp[1,"pval_Common_model"]<-NA
  pooled_table_templet_temp[1,"tau"]<-NA
  pooled_table_templet_temp[1,"I2"]<-NA
  pooled_table_templet_temp[1,"pval_Q"]<-NA
  pooled_table<-rbind(pooled_table,pooled_table_templet_temp)
}

#### 02_03 Subgroup_MGMT_status end ####

output_dir<-paste0(output_dir_root,"//03_meta_analysis//02_04 Subgroup_Complete_resection")
dir.create(output_dir)
#### 02_04 Subgroup_Complete_resection start ####
#### 02_04_01_01 Total excision; OS; dichotomous CD8 ####
# 02_04_01 Total excision
meta_table<-subset(sum_table,Complete_resection=="Total excision")
# 02_04_01_01 Total excision; OS
meta_table<-subset(meta_table,Outcome=="OS")
# 02_04_01_01 Total excision; OS; dichotomous CD8
meta_table<-subset(meta_table,PatientNumInCD8PositiveGroup!="Cox continue CD8")
# exclude results without convergence
# no exclusion
# meta-analysis
meta_result<-metagen(logHR, selogHR, sm = "HR",studlab = ResearchName,data=meta_table)
# forest plot
pdf(paste0(output_dir,"//ForestPlot_Subgroup_Complete_resection_Total excision_OS_dichotomousCD8.pdf"),width=10, height=4)
p_forest<-forest(meta_result,print.Q =T,digits=4,digits.pval = 3,digits.pval.Q = 3,
                 test.overall.common = TRUE,
                 test.overall.random = TRUE)
print(p_forest)
dev.off()
# funnel plot
pdf(paste0(output_dir,"//FunnelPlot_Subgroup_Complete_resection_Total excision_OS_dichotomousCD8.pdf"),width=6, height=6)
p_funnel<-funnel(meta_result)
print(p_funnel)
dev.off()

# add results to pooled table
pooled_table_templet_temp<-pooled_table_templet
pooled_table_templet_temp[1,"Subgroup"]<-"Total excision"
pooled_table_templet_temp[1,"Outcome"]<-"OS"
pooled_table_templet_temp[1,"Study_number"]<-nrow(meta_table)
if(nrow(meta_table)>=1){
  pooled_table_templet_temp[1,"Patient_numbers"]<-sum(as.numeric(meta_table$PatientNumInCD8PositiveGroup),as.numeric(meta_table$PatientNumInCD8NegativeGroup))
  pooled_table_templet_temp[1,"HR_Random_model"]<-round(exp(meta_result$TE.random),digits = 4)
  pooled_table_templet_temp[1,"CI_lower_Random_model"]<-round(exp(meta_result$lower.random),digits = 4)
  pooled_table_templet_temp[1,"CI_upper_Random_model"]<-round(exp(meta_result$upper.random),digits = 4)
  pooled_table_templet_temp[1,"pval_Random_model"]<-round(meta_result$pval.random,digits = 4)
  pooled_table_templet_temp[1,"HR_Common_model"]<-round(exp(meta_result$TE.common),digits = 4)
  pooled_table_templet_temp[1,"CI_lower_Common_model"]<-round(exp(meta_result$lower.common),digits = 4)
  pooled_table_templet_temp[1,"CI_upper_Common_model"]<-round(exp(meta_result$upper.common),digits = 4)
  pooled_table_templet_temp[1,"pval_Common_model"]<-round(meta_result$pval.common,digits = 4)
  pooled_table_templet_temp[1,"tau"]<-round(meta_result$tau,digits = 4)
  pooled_table_templet_temp[1,"I2"]<-paste0(as.character(round(100*meta_result$I2,digits = 1)),"%")
  pooled_table_templet_temp[1,"pval_Q"]<-round(meta_result$pval.Q,digits = 4)
  pooled_table<-rbind(pooled_table,pooled_table_templet_temp)
} else {
  pooled_table_templet_temp[1,"Patient_numbers"]<-0
  pooled_table_templet_temp[1,"HR_Random_model"]<-NA
  pooled_table_templet_temp[1,"CI_lower_Random_model"]<-NA
  pooled_table_templet_temp[1,"CI_upper_Random_model"]<-NA
  pooled_table_templet_temp[1,"pval_Random_model"]<-NA
  pooled_table_templet_temp[1,"HR_Common_model"]<-NA
  pooled_table_templet_temp[1,"CI_lower_Common_model"]<-NA
  pooled_table_templet_temp[1,"CI_upper_Common_model"]<-NA
  pooled_table_templet_temp[1,"pval_Common_model"]<-NA
  pooled_table_templet_temp[1,"tau"]<-NA
  pooled_table_templet_temp[1,"I2"]<-NA
  pooled_table_templet_temp[1,"pval_Q"]<-NA
  pooled_table<-rbind(pooled_table,pooled_table_templet_temp)
}


#### 02_04_01_02 Total excision; PFS; dichotomous CD8 ####
# 02_04_01 Total excision
meta_table<-subset(sum_table,Complete_resection=="Total excision")
# 02_04_01_02 Total excision; PFS
meta_table<-subset(meta_table,Outcome=="PFS")
# 02_04_01_02 Total excision; PFS; dichotomous CD8
meta_table<-subset(meta_table,PatientNumInCD8PositiveGroup!="Cox continue CD8")
# exclude results without convergence
# no exclusion
# meta-analysis
meta_result<-metagen(logHR, selogHR, sm = "HR",studlab = ResearchName,data=meta_table)
# forest plot
pdf(paste0(output_dir,"//ForestPlot_Subgroup_Complete_resection_Total excision_PFS_dichotomousCD8.pdf"),width=10, height=4)
p_forest<-forest(meta_result,print.Q =T,digits=4,digits.pval = 3,digits.pval.Q = 3,
                 test.overall.common = TRUE,
                 test.overall.random = TRUE)
print(p_forest)
dev.off()
# funnel plot
pdf(paste0(output_dir,"//FunnelPlot_Subgroup_Complete_resection_Total excision_PFS_dichotomousCD8.pdf"),width=6, height=6)
p_funnel<-funnel(meta_result)
print(p_funnel)
dev.off()

# add results to pooled table
pooled_table_templet_temp<-pooled_table_templet
pooled_table_templet_temp[1,"Subgroup"]<-"Total excision"
pooled_table_templet_temp[1,"Outcome"]<-"PFS"
pooled_table_templet_temp[1,"Study_number"]<-nrow(meta_table)
if(nrow(meta_table)>=1){
  pooled_table_templet_temp[1,"Patient_numbers"]<-sum(as.numeric(meta_table$PatientNumInCD8PositiveGroup),as.numeric(meta_table$PatientNumInCD8NegativeGroup))
  pooled_table_templet_temp[1,"HR_Random_model"]<-round(exp(meta_result$TE.random),digits = 4)
  pooled_table_templet_temp[1,"CI_lower_Random_model"]<-round(exp(meta_result$lower.random),digits = 4)
  pooled_table_templet_temp[1,"CI_upper_Random_model"]<-round(exp(meta_result$upper.random),digits = 4)
  pooled_table_templet_temp[1,"pval_Random_model"]<-round(meta_result$pval.random,digits = 4)
  pooled_table_templet_temp[1,"HR_Common_model"]<-round(exp(meta_result$TE.common),digits = 4)
  pooled_table_templet_temp[1,"CI_lower_Common_model"]<-round(exp(meta_result$lower.common),digits = 4)
  pooled_table_templet_temp[1,"CI_upper_Common_model"]<-round(exp(meta_result$upper.common),digits = 4)
  pooled_table_templet_temp[1,"pval_Common_model"]<-round(meta_result$pval.common,digits = 4)
  pooled_table_templet_temp[1,"tau"]<-round(meta_result$tau,digits = 4)
  pooled_table_templet_temp[1,"I2"]<-paste0(as.character(round(100*meta_result$I2,digits = 1)),"%")
  pooled_table_templet_temp[1,"pval_Q"]<-round(meta_result$pval.Q,digits = 4)
  pooled_table<-rbind(pooled_table,pooled_table_templet_temp)
} else {
  pooled_table_templet_temp[1,"Patient_numbers"]<-0
  pooled_table_templet_temp[1,"HR_Random_model"]<-NA
  pooled_table_templet_temp[1,"CI_lower_Random_model"]<-NA
  pooled_table_templet_temp[1,"CI_upper_Random_model"]<-NA
  pooled_table_templet_temp[1,"pval_Random_model"]<-NA
  pooled_table_templet_temp[1,"HR_Common_model"]<-NA
  pooled_table_templet_temp[1,"CI_lower_Common_model"]<-NA
  pooled_table_templet_temp[1,"CI_upper_Common_model"]<-NA
  pooled_table_templet_temp[1,"pval_Common_model"]<-NA
  pooled_table_templet_temp[1,"tau"]<-NA
  pooled_table_templet_temp[1,"I2"]<-NA
  pooled_table_templet_temp[1,"pval_Q"]<-NA
  pooled_table<-rbind(pooled_table,pooled_table_templet_temp)
}

#### 02_04_01_03 Total excision; Survival after immunotherapy; dichotomous CD8 ####
# 02_04_01 Total excision
meta_table<-subset(sum_table,Complete_resection=="Total excision")
# 02_04_01_03 Total excision; Survival after immunotherapy
meta_table<-subset(meta_table,Outcome=="Survival after immunotherapy")
# 02_04_01_03 Total excision; Survival after immunotherapy; dichotomous CD8
meta_table<-subset(meta_table,PatientNumInCD8PositiveGroup!="Cox continue CD8")
# exclude results without convergence
# no exclusion
# meta-analysis
# no study left 
# meta_result<-metagen(logHR, selogHR, sm = "HR",studlab = ResearchName,data=meta_table)
# # forest plot
# pdf(paste0(output_dir,"//ForestPlot_Subgroup_Complete_resection_Total excision_SurvivalAfterImmunotherapy_dichotomousCD8.pdf"),width=10, height=4)
# p_forest<-forest(meta_result,print.Q =T,digits=4,digits.pval = 3,digits.pval.Q = 3,
#                  test.overall.common = TRUE,
#                  test.overall.random = TRUE)
# print(p_forest)
# dev.off()
# # funnel plot
# pdf(paste0(output_dir,"//FunnelPlot_Subgroup_Complete_resection_Total excision_SurvivalAfterImmunotherapy_dichotomousCD8.pdf"),width=6, height=6)
# p_funnel<-funnel(meta_result)
# print(p_funnel)
# dev.off()

# add results to pooled table
pooled_table_templet_temp<-pooled_table_templet
pooled_table_templet_temp[1,"Subgroup"]<-"Total excision"
pooled_table_templet_temp[1,"Outcome"]<-"Survival after immunotherapy"
pooled_table_templet_temp[1,"Study_number"]<-nrow(meta_table)
if(nrow(meta_table)>=1){
  pooled_table_templet_temp[1,"Patient_numbers"]<-sum(as.numeric(meta_table$PatientNumInCD8PositiveGroup),as.numeric(meta_table$PatientNumInCD8NegativeGroup))
  pooled_table_templet_temp[1,"HR_Random_model"]<-round(exp(meta_result$TE.random),digits = 4)
  pooled_table_templet_temp[1,"CI_lower_Random_model"]<-round(exp(meta_result$lower.random),digits = 4)
  pooled_table_templet_temp[1,"CI_upper_Random_model"]<-round(exp(meta_result$upper.random),digits = 4)
  pooled_table_templet_temp[1,"pval_Random_model"]<-round(meta_result$pval.random,digits = 4)
  pooled_table_templet_temp[1,"HR_Common_model"]<-round(exp(meta_result$TE.common),digits = 4)
  pooled_table_templet_temp[1,"CI_lower_Common_model"]<-round(exp(meta_result$lower.common),digits = 4)
  pooled_table_templet_temp[1,"CI_upper_Common_model"]<-round(exp(meta_result$upper.common),digits = 4)
  pooled_table_templet_temp[1,"pval_Common_model"]<-round(meta_result$pval.common,digits = 4)
  pooled_table_templet_temp[1,"tau"]<-round(meta_result$tau,digits = 4)
  pooled_table_templet_temp[1,"I2"]<-paste0(as.character(round(100*meta_result$I2,digits = 1)),"%")
  pooled_table_templet_temp[1,"pval_Q"]<-round(meta_result$pval.Q,digits = 4)
  pooled_table<-rbind(pooled_table,pooled_table_templet_temp)
} else {
  pooled_table_templet_temp[1,"Patient_numbers"]<-0
  pooled_table_templet_temp[1,"HR_Random_model"]<-NA
  pooled_table_templet_temp[1,"CI_lower_Random_model"]<-NA
  pooled_table_templet_temp[1,"CI_upper_Random_model"]<-NA
  pooled_table_templet_temp[1,"pval_Random_model"]<-NA
  pooled_table_templet_temp[1,"HR_Common_model"]<-NA
  pooled_table_templet_temp[1,"CI_lower_Common_model"]<-NA
  pooled_table_templet_temp[1,"CI_upper_Common_model"]<-NA
  pooled_table_templet_temp[1,"pval_Common_model"]<-NA
  pooled_table_templet_temp[1,"tau"]<-NA
  pooled_table_templet_temp[1,"I2"]<-NA
  pooled_table_templet_temp[1,"pval_Q"]<-NA
  pooled_table<-rbind(pooled_table,pooled_table_templet_temp)
}



#### 02_04_02_01 Subtotal excision; OS; dichotomous CD8 ####
# 02_04_02 Subtotal excision
meta_table<-subset(sum_table,Complete_resection=="Subtotal excision")
# 02_04_02_01 Subtotal excision; OS
meta_table<-subset(meta_table,Outcome=="OS")
# 02_04_02_01 Subtotal excision; OS; dichotomous CD8
meta_table<-subset(meta_table,PatientNumInCD8PositiveGroup!="Cox continue CD8")
# exclude results without convergence
meta_table<-meta_table[(meta_table$PMID!="PMID29910795"),]
# meta-analysis
meta_result<-metagen(logHR, selogHR, sm = "HR",studlab = ResearchName,data=meta_table)
# forest plot
pdf(paste0(output_dir,"//ForestPlot_Subgroup_Complete_resection_Subtotal excision_OS_dichotomousCD8.pdf"),width=10, height=4)
p_forest<-forest(meta_result,print.Q =T,digits=4,digits.pval = 3,digits.pval.Q = 3,
                 test.overall.common = TRUE,
                 test.overall.random = TRUE)
print(p_forest)
dev.off()
# funnel plot
pdf(paste0(output_dir,"//FunnelPlot_Subgroup_Complete_resection_Subtotal excision_OS_dichotomousCD8.pdf"),width=6, height=6)
p_funnel<-funnel(meta_result)
print(p_funnel)
dev.off()

# add results to pooled table
pooled_table_templet_temp<-pooled_table_templet
pooled_table_templet_temp[1,"Subgroup"]<-"Subtotal excision"
pooled_table_templet_temp[1,"Outcome"]<-"OS"
pooled_table_templet_temp[1,"Study_number"]<-nrow(meta_table)
if(nrow(meta_table)>=1){
  pooled_table_templet_temp[1,"Patient_numbers"]<-sum(as.numeric(meta_table$PatientNumInCD8PositiveGroup),as.numeric(meta_table$PatientNumInCD8NegativeGroup))
  pooled_table_templet_temp[1,"HR_Random_model"]<-round(exp(meta_result$TE.random),digits = 4)
  pooled_table_templet_temp[1,"CI_lower_Random_model"]<-round(exp(meta_result$lower.random),digits = 4)
  pooled_table_templet_temp[1,"CI_upper_Random_model"]<-round(exp(meta_result$upper.random),digits = 4)
  pooled_table_templet_temp[1,"pval_Random_model"]<-round(meta_result$pval.random,digits = 4)
  pooled_table_templet_temp[1,"HR_Common_model"]<-round(exp(meta_result$TE.common),digits = 4)
  pooled_table_templet_temp[1,"CI_lower_Common_model"]<-round(exp(meta_result$lower.common),digits = 4)
  pooled_table_templet_temp[1,"CI_upper_Common_model"]<-round(exp(meta_result$upper.common),digits = 4)
  pooled_table_templet_temp[1,"pval_Common_model"]<-round(meta_result$pval.common,digits = 4)
  pooled_table_templet_temp[1,"tau"]<-round(meta_result$tau,digits = 4)
  pooled_table_templet_temp[1,"I2"]<-paste0(as.character(round(100*meta_result$I2,digits = 1)),"%")
  pooled_table_templet_temp[1,"pval_Q"]<-round(meta_result$pval.Q,digits = 4)
  pooled_table<-rbind(pooled_table,pooled_table_templet_temp)
} else {
  pooled_table_templet_temp[1,"Patient_numbers"]<-0
  pooled_table_templet_temp[1,"HR_Random_model"]<-NA
  pooled_table_templet_temp[1,"CI_lower_Random_model"]<-NA
  pooled_table_templet_temp[1,"CI_upper_Random_model"]<-NA
  pooled_table_templet_temp[1,"pval_Random_model"]<-NA
  pooled_table_templet_temp[1,"HR_Common_model"]<-NA
  pooled_table_templet_temp[1,"CI_lower_Common_model"]<-NA
  pooled_table_templet_temp[1,"CI_upper_Common_model"]<-NA
  pooled_table_templet_temp[1,"pval_Common_model"]<-NA
  pooled_table_templet_temp[1,"tau"]<-NA
  pooled_table_templet_temp[1,"I2"]<-NA
  pooled_table_templet_temp[1,"pval_Q"]<-NA
  pooled_table<-rbind(pooled_table,pooled_table_templet_temp)
}

#### 02_04_02_02 Subtotal excision; PFS; dichotomous CD8 ####
# 02_04_02 Subtotal excision
meta_table<-subset(sum_table,Complete_resection=="Subtotal excision")
# 02_04_02_02 Subtotal excision; PFS
meta_table<-subset(meta_table,Outcome=="PFS")
# 02_04_02_02 Subtotal excision; PFS; dichotomous CD8
meta_table<-subset(meta_table,PatientNumInCD8PositiveGroup!="Cox continue CD8")
# exclude results without convergence
meta_table<-meta_table[(meta_table$PMID!="PMID29910795"),]
# meta-analysis
# no study left
# meta_result<-metagen(logHR, selogHR, sm = "HR",studlab = ResearchName,data=meta_table)
# # forest plot
# pdf(paste0(output_dir,"//ForestPlot_Subgroup_Complete_resection_Subtotal excision_PFS_dichotomousCD8.pdf"),width=10, height=4)
# p_forest<-forest(meta_result,print.Q =T,digits=4,digits.pval = 3,digits.pval.Q = 3,
#                  test.overall.common = TRUE,
#                  test.overall.random = TRUE)
# print(p_forest)
# dev.off()
# # funnel plot
# pdf(paste0(output_dir,"//FunnelPlot_Subgroup_Complete_resection_Subtotal excision_PFS_dichotomousCD8.pdf"),width=6, height=6)
# p_funnel<-funnel(meta_result)
# print(p_funnel)
# dev.off()

# add results to pooled table
pooled_table_templet_temp<-pooled_table_templet
pooled_table_templet_temp[1,"Subgroup"]<-"Subtotal excision"
pooled_table_templet_temp[1,"Outcome"]<-"PFS"
pooled_table_templet_temp[1,"Study_number"]<-nrow(meta_table)
if(nrow(meta_table)>=1){
  pooled_table_templet_temp[1,"Patient_numbers"]<-sum(as.numeric(meta_table$PatientNumInCD8PositiveGroup),as.numeric(meta_table$PatientNumInCD8NegativeGroup))
  pooled_table_templet_temp[1,"HR_Random_model"]<-round(exp(meta_result$TE.random),digits = 4)
  pooled_table_templet_temp[1,"CI_lower_Random_model"]<-round(exp(meta_result$lower.random),digits = 4)
  pooled_table_templet_temp[1,"CI_upper_Random_model"]<-round(exp(meta_result$upper.random),digits = 4)
  pooled_table_templet_temp[1,"pval_Random_model"]<-round(meta_result$pval.random,digits = 4)
  pooled_table_templet_temp[1,"HR_Common_model"]<-round(exp(meta_result$TE.common),digits = 4)
  pooled_table_templet_temp[1,"CI_lower_Common_model"]<-round(exp(meta_result$lower.common),digits = 4)
  pooled_table_templet_temp[1,"CI_upper_Common_model"]<-round(exp(meta_result$upper.common),digits = 4)
  pooled_table_templet_temp[1,"pval_Common_model"]<-round(meta_result$pval.common,digits = 4)
  pooled_table_templet_temp[1,"tau"]<-round(meta_result$tau,digits = 4)
  pooled_table_templet_temp[1,"I2"]<-paste0(as.character(round(100*meta_result$I2,digits = 1)),"%")
  pooled_table_templet_temp[1,"pval_Q"]<-round(meta_result$pval.Q,digits = 4)
  pooled_table<-rbind(pooled_table,pooled_table_templet_temp)
} else {
  pooled_table_templet_temp[1,"Patient_numbers"]<-0
  pooled_table_templet_temp[1,"HR_Random_model"]<-NA
  pooled_table_templet_temp[1,"CI_lower_Random_model"]<-NA
  pooled_table_templet_temp[1,"CI_upper_Random_model"]<-NA
  pooled_table_templet_temp[1,"pval_Random_model"]<-NA
  pooled_table_templet_temp[1,"HR_Common_model"]<-NA
  pooled_table_templet_temp[1,"CI_lower_Common_model"]<-NA
  pooled_table_templet_temp[1,"CI_upper_Common_model"]<-NA
  pooled_table_templet_temp[1,"pval_Common_model"]<-NA
  pooled_table_templet_temp[1,"tau"]<-NA
  pooled_table_templet_temp[1,"I2"]<-NA
  pooled_table_templet_temp[1,"pval_Q"]<-NA
  pooled_table<-rbind(pooled_table,pooled_table_templet_temp)
}

#### 02_04_02_03 Subtotal excision; Survival after immunotherapy; dichotomous CD8 ####
# 02_04_02 Subtotal excision
meta_table<-subset(sum_table,Complete_resection=="Subtotal excision")
# 02_04_02_03 Subtotal excision; Survival after immunotherapy
meta_table<-subset(meta_table,Outcome=="Survival after immunotherapy")
# 02_04_02_03 Subtotal excision; Survival after immunotherapy; dichotomous CD8
meta_table<-subset(meta_table,PatientNumInCD8PositiveGroup!="Cox continue CD8")
# exclude results without convergence
# no exclusion
# meta-analysis
# no study left 
# meta_result<-metagen(logHR, selogHR, sm = "HR",studlab = ResearchName,data=meta_table)
# # forest plot
# pdf(paste0(output_dir,"//ForestPlot_Subgroup_Complete_resection_Subtotal excision_SurvivalAfterImmunotherapy_dichotomousCD8.pdf"),width=10, height=4)
# p_forest<-forest(meta_result,print.Q =T,digits=4,digits.pval = 3,digits.pval.Q = 3,
#                  test.overall.common = TRUE,
#                  test.overall.random = TRUE)
# print(p_forest)
# dev.off()
# # funnel plot
# pdf(paste0(output_dir,"//FunnelPlot_Subgroup_Complete_resection_Subtotal excision_SurvivalAfterImmunotherapy_dichotomousCD8.pdf"),width=6, height=6)
# p_funnel<-funnel(meta_result)
# print(p_funnel)
# dev.off()

# add results to pooled table
pooled_table_templet_temp<-pooled_table_templet
pooled_table_templet_temp[1,"Subgroup"]<-"Subtotal excision"
pooled_table_templet_temp[1,"Outcome"]<-"Survival after immunotherapy"
pooled_table_templet_temp[1,"Study_number"]<-nrow(meta_table)
if(nrow(meta_table)>=1){
  pooled_table_templet_temp[1,"Patient_numbers"]<-sum(as.numeric(meta_table$PatientNumInCD8PositiveGroup),as.numeric(meta_table$PatientNumInCD8NegativeGroup))
  pooled_table_templet_temp[1,"HR_Random_model"]<-round(exp(meta_result$TE.random),digits = 4)
  pooled_table_templet_temp[1,"CI_lower_Random_model"]<-round(exp(meta_result$lower.random),digits = 4)
  pooled_table_templet_temp[1,"CI_upper_Random_model"]<-round(exp(meta_result$upper.random),digits = 4)
  pooled_table_templet_temp[1,"pval_Random_model"]<-round(meta_result$pval.random,digits = 4)
  pooled_table_templet_temp[1,"HR_Common_model"]<-round(exp(meta_result$TE.common),digits = 4)
  pooled_table_templet_temp[1,"CI_lower_Common_model"]<-round(exp(meta_result$lower.common),digits = 4)
  pooled_table_templet_temp[1,"CI_upper_Common_model"]<-round(exp(meta_result$upper.common),digits = 4)
  pooled_table_templet_temp[1,"pval_Common_model"]<-round(meta_result$pval.common,digits = 4)
  pooled_table_templet_temp[1,"tau"]<-round(meta_result$tau,digits = 4)
  pooled_table_templet_temp[1,"I2"]<-paste0(as.character(round(100*meta_result$I2,digits = 1)),"%")
  pooled_table_templet_temp[1,"pval_Q"]<-round(meta_result$pval.Q,digits = 4)
  pooled_table<-rbind(pooled_table,pooled_table_templet_temp)
} else {
  pooled_table_templet_temp[1,"Patient_numbers"]<-0
  pooled_table_templet_temp[1,"HR_Random_model"]<-NA
  pooled_table_templet_temp[1,"CI_lower_Random_model"]<-NA
  pooled_table_templet_temp[1,"CI_upper_Random_model"]<-NA
  pooled_table_templet_temp[1,"pval_Random_model"]<-NA
  pooled_table_templet_temp[1,"HR_Common_model"]<-NA
  pooled_table_templet_temp[1,"CI_lower_Common_model"]<-NA
  pooled_table_templet_temp[1,"CI_upper_Common_model"]<-NA
  pooled_table_templet_temp[1,"pval_Common_model"]<-NA
  pooled_table_templet_temp[1,"tau"]<-NA
  pooled_table_templet_temp[1,"I2"]<-NA
  pooled_table_templet_temp[1,"pval_Q"]<-NA
  pooled_table<-rbind(pooled_table,pooled_table_templet_temp)
}

#### 02_04_03_01 Partial excision; OS; dichotomous CD8 ####
# 02_04_03 Partial excision
meta_table<-subset(sum_table,Complete_resection=="Partial excision")
# 02_04_03_01 Partial excision; OS
meta_table<-subset(meta_table,Outcome=="OS")
# 02_04_03_01 Partial excision; OS; dichotomous CD8
meta_table<-subset(meta_table,PatientNumInCD8PositiveGroup!="Cox continue CD8")
# exclude results without convergence
meta_table<-meta_table[(meta_table$PMID!="PMID29910795"),]
meta_table<-meta_table[(meta_table$PMID!="PMID33130898"),]
# meta-analysis
# no study left
# meta_result<-metagen(logHR, selogHR, sm = "HR",studlab = ResearchName,data=meta_table)
# # forest plot
# pdf(paste0(output_dir,"//ForestPlot_Subgroup_Complete_resection_Partial excision_OS_dichotomousCD8.pdf"),width=10, height=4)
# p_forest<-forest(meta_result,print.Q =T,digits=4,digits.pval = 3,digits.pval.Q = 3,
#                  test.overall.common = TRUE,
#                  test.overall.random = TRUE)
# print(p_forest)
# dev.off()
# # funnel plot
# pdf(paste0(output_dir,"//FunnelPlot_Subgroup_Complete_resection_Partial excision_OS_dichotomousCD8.pdf"),width=6, height=6)
# p_funnel<-funnel(meta_result)
# print(p_funnel)
# dev.off()

# add results to pooled table
pooled_table_templet_temp<-pooled_table_templet
pooled_table_templet_temp[1,"Subgroup"]<-"Partial excision"
pooled_table_templet_temp[1,"Outcome"]<-"OS"
pooled_table_templet_temp[1,"Study_number"]<-nrow(meta_table)
if(nrow(meta_table)>=1){
  pooled_table_templet_temp[1,"Patient_numbers"]<-sum(as.numeric(meta_table$PatientNumInCD8PositiveGroup),as.numeric(meta_table$PatientNumInCD8NegativeGroup))
  pooled_table_templet_temp[1,"HR_Random_model"]<-round(exp(meta_result$TE.random),digits = 4)
  pooled_table_templet_temp[1,"CI_lower_Random_model"]<-round(exp(meta_result$lower.random),digits = 4)
  pooled_table_templet_temp[1,"CI_upper_Random_model"]<-round(exp(meta_result$upper.random),digits = 4)
  pooled_table_templet_temp[1,"pval_Random_model"]<-round(meta_result$pval.random,digits = 4)
  pooled_table_templet_temp[1,"HR_Common_model"]<-round(exp(meta_result$TE.common),digits = 4)
  pooled_table_templet_temp[1,"CI_lower_Common_model"]<-round(exp(meta_result$lower.common),digits = 4)
  pooled_table_templet_temp[1,"CI_upper_Common_model"]<-round(exp(meta_result$upper.common),digits = 4)
  pooled_table_templet_temp[1,"pval_Common_model"]<-round(meta_result$pval.common,digits = 4)
  pooled_table_templet_temp[1,"tau"]<-round(meta_result$tau,digits = 4)
  pooled_table_templet_temp[1,"I2"]<-paste0(as.character(round(100*meta_result$I2,digits = 1)),"%")
  pooled_table_templet_temp[1,"pval_Q"]<-round(meta_result$pval.Q,digits = 4)
  pooled_table<-rbind(pooled_table,pooled_table_templet_temp)
} else {
  pooled_table_templet_temp[1,"Patient_numbers"]<-0
  pooled_table_templet_temp[1,"HR_Random_model"]<-NA
  pooled_table_templet_temp[1,"CI_lower_Random_model"]<-NA
  pooled_table_templet_temp[1,"CI_upper_Random_model"]<-NA
  pooled_table_templet_temp[1,"pval_Random_model"]<-NA
  pooled_table_templet_temp[1,"HR_Common_model"]<-NA
  pooled_table_templet_temp[1,"CI_lower_Common_model"]<-NA
  pooled_table_templet_temp[1,"CI_upper_Common_model"]<-NA
  pooled_table_templet_temp[1,"pval_Common_model"]<-NA
  pooled_table_templet_temp[1,"tau"]<-NA
  pooled_table_templet_temp[1,"I2"]<-NA
  pooled_table_templet_temp[1,"pval_Q"]<-NA
  pooled_table<-rbind(pooled_table,pooled_table_templet_temp)
}

#### 02_04_03_02 Partial excision; PFS; dichotomous CD8 ####
# 02_04_03 Partial excision
meta_table<-subset(sum_table,Complete_resection=="Partial excision")
# 02_04_03_02 Partial excision; PFS
meta_table<-subset(meta_table,Outcome=="PFS")
# 02_04_03_02 Partial excision; PFS; dichotomous CD8
meta_table<-subset(meta_table,PatientNumInCD8PositiveGroup!="Cox continue CD8")
# exclude results without convergence
meta_table<-meta_table[(meta_table$PMID!="PMID29910795"),]
# meta-analysis
# no study left
# meta_result<-metagen(logHR, selogHR, sm = "HR",studlab = ResearchName,data=meta_table)
# # forest plot
# pdf(paste0(output_dir,"//ForestPlot_Subgroup_Complete_resection_Partial excision_PFS_dichotomousCD8.pdf"),width=10, height=4)
# p_forest<-forest(meta_result,print.Q =T,digits=4,digits.pval = 3,digits.pval.Q = 3,
#                  test.overall.common = TRUE,
#                  test.overall.random = TRUE)
# print(p_forest)
# dev.off()
# # funnel plot
# pdf(paste0(output_dir,"//FunnelPlot_Subgroup_Complete_resection_Partial excision_PFS_dichotomousCD8.pdf"),width=6, height=6)
# p_funnel<-funnel(meta_result)
# print(p_funnel)
# dev.off()

# add results to pooled table
pooled_table_templet_temp<-pooled_table_templet
pooled_table_templet_temp[1,"Subgroup"]<-"Partial excision"
pooled_table_templet_temp[1,"Outcome"]<-"PFS"
pooled_table_templet_temp[1,"Study_number"]<-nrow(meta_table)
if(nrow(meta_table)>=1){
  pooled_table_templet_temp[1,"Patient_numbers"]<-sum(as.numeric(meta_table$PatientNumInCD8PositiveGroup),as.numeric(meta_table$PatientNumInCD8NegativeGroup))
  pooled_table_templet_temp[1,"HR_Random_model"]<-round(exp(meta_result$TE.random),digits = 4)
  pooled_table_templet_temp[1,"CI_lower_Random_model"]<-round(exp(meta_result$lower.random),digits = 4)
  pooled_table_templet_temp[1,"CI_upper_Random_model"]<-round(exp(meta_result$upper.random),digits = 4)
  pooled_table_templet_temp[1,"pval_Random_model"]<-round(meta_result$pval.random,digits = 4)
  pooled_table_templet_temp[1,"HR_Common_model"]<-round(exp(meta_result$TE.common),digits = 4)
  pooled_table_templet_temp[1,"CI_lower_Common_model"]<-round(exp(meta_result$lower.common),digits = 4)
  pooled_table_templet_temp[1,"CI_upper_Common_model"]<-round(exp(meta_result$upper.common),digits = 4)
  pooled_table_templet_temp[1,"pval_Common_model"]<-round(meta_result$pval.common,digits = 4)
  pooled_table_templet_temp[1,"tau"]<-round(meta_result$tau,digits = 4)
  pooled_table_templet_temp[1,"I2"]<-paste0(as.character(round(100*meta_result$I2,digits = 1)),"%")
  pooled_table_templet_temp[1,"pval_Q"]<-round(meta_result$pval.Q,digits = 4)
  pooled_table<-rbind(pooled_table,pooled_table_templet_temp)
} else {
  pooled_table_templet_temp[1,"Patient_numbers"]<-0
  pooled_table_templet_temp[1,"HR_Random_model"]<-NA
  pooled_table_templet_temp[1,"CI_lower_Random_model"]<-NA
  pooled_table_templet_temp[1,"CI_upper_Random_model"]<-NA
  pooled_table_templet_temp[1,"pval_Random_model"]<-NA
  pooled_table_templet_temp[1,"HR_Common_model"]<-NA
  pooled_table_templet_temp[1,"CI_lower_Common_model"]<-NA
  pooled_table_templet_temp[1,"CI_upper_Common_model"]<-NA
  pooled_table_templet_temp[1,"pval_Common_model"]<-NA
  pooled_table_templet_temp[1,"tau"]<-NA
  pooled_table_templet_temp[1,"I2"]<-NA
  pooled_table_templet_temp[1,"pval_Q"]<-NA
  pooled_table<-rbind(pooled_table,pooled_table_templet_temp)
}

#### 02_04_03_03 Partial excision; Survival after immunotherapy; dichotomous CD8 ####
# 02_04_03 Partial excision
meta_table<-subset(sum_table,Complete_resection=="Partial excision")
# 02_04_03_03 Partial excision; Survival after immunotherapy
meta_table<-subset(meta_table,Outcome=="Survival after immunotherapy")
# 02_04_03_03 Partial excision; Survival after immunotherapy; dichotomous CD8
meta_table<-subset(meta_table,PatientNumInCD8PositiveGroup!="Cox continue CD8")
# exclude results without convergence
# no exclusion
# meta-analysis
# no study left 
# meta_result<-metagen(logHR, selogHR, sm = "HR",studlab = ResearchName,data=meta_table)
# # forest plot
# pdf(paste0(output_dir,"//ForestPlot_Subgroup_Complete_resection_Partial excision_SurvivalAfterImmunotherapy_dichotomousCD8.pdf"),width=10, height=4)
# p_forest<-forest(meta_result,print.Q =T,digits=4,digits.pval = 3,digits.pval.Q = 3,
#                  test.overall.common = TRUE,
#                  test.overall.random = TRUE)
# print(p_forest)
# dev.off()
# # funnel plot
# pdf(paste0(output_dir,"//FunnelPlot_Subgroup_Complete_resection_Partial excision_SurvivalAfterImmunotherapy_dichotomousCD8.pdf"),width=6, height=6)
# p_funnel<-funnel(meta_result)
# print(p_funnel)
# dev.off()

# add results to pooled table
pooled_table_templet_temp<-pooled_table_templet
pooled_table_templet_temp[1,"Subgroup"]<-"Partial excision"
pooled_table_templet_temp[1,"Outcome"]<-"Survival after immunotherapy"
pooled_table_templet_temp[1,"Study_number"]<-nrow(meta_table)
if(nrow(meta_table)>=1){
  pooled_table_templet_temp[1,"Patient_numbers"]<-sum(as.numeric(meta_table$PatientNumInCD8PositiveGroup),as.numeric(meta_table$PatientNumInCD8NegativeGroup))
  pooled_table_templet_temp[1,"HR_Random_model"]<-round(exp(meta_result$TE.random),digits = 4)
  pooled_table_templet_temp[1,"CI_lower_Random_model"]<-round(exp(meta_result$lower.random),digits = 4)
  pooled_table_templet_temp[1,"CI_upper_Random_model"]<-round(exp(meta_result$upper.random),digits = 4)
  pooled_table_templet_temp[1,"pval_Random_model"]<-round(meta_result$pval.random,digits = 4)
  pooled_table_templet_temp[1,"HR_Common_model"]<-round(exp(meta_result$TE.common),digits = 4)
  pooled_table_templet_temp[1,"CI_lower_Common_model"]<-round(exp(meta_result$lower.common),digits = 4)
  pooled_table_templet_temp[1,"CI_upper_Common_model"]<-round(exp(meta_result$upper.common),digits = 4)
  pooled_table_templet_temp[1,"pval_Common_model"]<-round(meta_result$pval.common,digits = 4)
  pooled_table_templet_temp[1,"tau"]<-round(meta_result$tau,digits = 4)
  pooled_table_templet_temp[1,"I2"]<-paste0(as.character(round(100*meta_result$I2,digits = 1)),"%")
  pooled_table_templet_temp[1,"pval_Q"]<-round(meta_result$pval.Q,digits = 4)
  pooled_table<-rbind(pooled_table,pooled_table_templet_temp)
} else {
  pooled_table_templet_temp[1,"Patient_numbers"]<-0
  pooled_table_templet_temp[1,"HR_Random_model"]<-NA
  pooled_table_templet_temp[1,"CI_lower_Random_model"]<-NA
  pooled_table_templet_temp[1,"CI_upper_Random_model"]<-NA
  pooled_table_templet_temp[1,"pval_Random_model"]<-NA
  pooled_table_templet_temp[1,"HR_Common_model"]<-NA
  pooled_table_templet_temp[1,"CI_lower_Common_model"]<-NA
  pooled_table_templet_temp[1,"CI_upper_Common_model"]<-NA
  pooled_table_templet_temp[1,"pval_Common_model"]<-NA
  pooled_table_templet_temp[1,"tau"]<-NA
  pooled_table_templet_temp[1,"I2"]<-NA
  pooled_table_templet_temp[1,"pval_Q"]<-NA
  pooled_table<-rbind(pooled_table,pooled_table_templet_temp)
}

#### 02_04 Subgroup_Complete_resection end ####


output_dir<-paste0(output_dir_root,"//03_meta_analysis//02_05 Subgroup_PR_Type")
dir.create(output_dir)
#### 02_05 Subgroup_PR_Type start ####
#### 02_05_01_01 Primary; OS; dichotomous CD8 ####
meta_table<-sum_table[(c(1:25)),]
# 02_05_01 Primary
meta_table<-subset(meta_table,PR_Type=="Primary")
# 02_05_01_01 Primary; OS
meta_table<-subset(meta_table,Outcome=="OS")
# 02_05_01_01 Primary; OS; dichotomous CD8
meta_table<-subset(meta_table,PatientNumInCD8PositiveGroup!="Cox continue CD8")
# exclude results without convergence
# no exclusion
# meta-analysis
meta_result<-metagen(logHR, selogHR, sm = "HR",studlab = ResearchName,data=meta_table)
# forest plot
pdf(paste0(output_dir,"//ForestPlot_Subgroup_PR_Type_Primary_OS_dichotomousCD8.pdf"),width=10, height=4)
p_forest<-forest(meta_result,print.Q =T,digits=4,digits.pval = 3,digits.pval.Q = 3,
                 test.overall.common = TRUE,
                 test.overall.random = TRUE)
print(p_forest)
dev.off()
# funnel plot
pdf(paste0(output_dir,"//FunnelPlot_Subgroup_PR_Type_Primary_OS_dichotomousCD8.pdf"),width=6, height=6)
p_funnel<-funnel(meta_result)
print(p_funnel)
dev.off()

# add results to pooled table
pooled_table_templet_temp<-pooled_table_templet
pooled_table_templet_temp[1,"Subgroup"]<-"Primary"
pooled_table_templet_temp[1,"Outcome"]<-"OS"
pooled_table_templet_temp[1,"Study_number"]<-nrow(meta_table)
if(nrow(meta_table)>=1){
  pooled_table_templet_temp[1,"Patient_numbers"]<-sum(as.numeric(meta_table$PatientNumInCD8PositiveGroup),as.numeric(meta_table$PatientNumInCD8NegativeGroup))
  pooled_table_templet_temp[1,"HR_Random_model"]<-round(exp(meta_result$TE.random),digits = 4)
  pooled_table_templet_temp[1,"CI_lower_Random_model"]<-round(exp(meta_result$lower.random),digits = 4)
  pooled_table_templet_temp[1,"CI_upper_Random_model"]<-round(exp(meta_result$upper.random),digits = 4)
  pooled_table_templet_temp[1,"pval_Random_model"]<-round(meta_result$pval.random,digits = 4)
  pooled_table_templet_temp[1,"HR_Common_model"]<-round(exp(meta_result$TE.common),digits = 4)
  pooled_table_templet_temp[1,"CI_lower_Common_model"]<-round(exp(meta_result$lower.common),digits = 4)
  pooled_table_templet_temp[1,"CI_upper_Common_model"]<-round(exp(meta_result$upper.common),digits = 4)
  pooled_table_templet_temp[1,"pval_Common_model"]<-round(meta_result$pval.common,digits = 4)
  pooled_table_templet_temp[1,"tau"]<-round(meta_result$tau,digits = 4)
  pooled_table_templet_temp[1,"I2"]<-paste0(as.character(round(100*meta_result$I2,digits = 1)),"%")
  pooled_table_templet_temp[1,"pval_Q"]<-round(meta_result$pval.Q,digits = 4)
  pooled_table<-rbind(pooled_table,pooled_table_templet_temp)
} else {
  pooled_table_templet_temp[1,"Patient_numbers"]<-0
  pooled_table_templet_temp[1,"HR_Random_model"]<-NA
  pooled_table_templet_temp[1,"CI_lower_Random_model"]<-NA
  pooled_table_templet_temp[1,"CI_upper_Random_model"]<-NA
  pooled_table_templet_temp[1,"pval_Random_model"]<-NA
  pooled_table_templet_temp[1,"HR_Common_model"]<-NA
  pooled_table_templet_temp[1,"CI_lower_Common_model"]<-NA
  pooled_table_templet_temp[1,"CI_upper_Common_model"]<-NA
  pooled_table_templet_temp[1,"pval_Common_model"]<-NA
  pooled_table_templet_temp[1,"tau"]<-NA
  pooled_table_templet_temp[1,"I2"]<-NA
  pooled_table_templet_temp[1,"pval_Q"]<-NA
  pooled_table<-rbind(pooled_table,pooled_table_templet_temp)
}
#### 02_05_01_02 Primary; PFS; dichotomous CD8 ####
meta_table<-sum_table[(c(1:25)),]
# 02_05_01 Primary
meta_table<-subset(meta_table,PR_Type=="Primary")
# 02_05_01_02 Primary; PFS
meta_table<-subset(meta_table,Outcome=="PFS")
# 02_05_01_02 Primary; PFS; dichotomous CD8
meta_table<-subset(meta_table,PatientNumInCD8PositiveGroup!="Cox continue CD8")
# exclude results without convergence
# no exclusion
# meta-analysis
meta_result<-metagen(logHR, selogHR, sm = "HR",studlab = ResearchName,data=meta_table)
# forest plot
pdf(paste0(output_dir,"//ForestPlot_Subgroup_PR_Type_Primary_PFS_dichotomousCD8.pdf"),width=10, height=4)
p_forest<-forest(meta_result,print.Q =T,digits=4,digits.pval = 3,digits.pval.Q = 3,
                 test.overall.common = TRUE,
                 test.overall.random = TRUE)
print(p_forest)
dev.off()
# funnel plot
pdf(paste0(output_dir,"//FunnelPlot_Subgroup_PR_Type_Primary_PFS_dichotomousCD8.pdf"),width=6, height=6)
p_funnel<-funnel(meta_result)
print(p_funnel)
dev.off()

# add results to pooled table
pooled_table_templet_temp<-pooled_table_templet
pooled_table_templet_temp[1,"Subgroup"]<-"Primary"
pooled_table_templet_temp[1,"Outcome"]<-"PFS"
pooled_table_templet_temp[1,"Study_number"]<-nrow(meta_table)
if(nrow(meta_table)>=1){
  pooled_table_templet_temp[1,"Patient_numbers"]<-sum(as.numeric(meta_table$PatientNumInCD8PositiveGroup),as.numeric(meta_table$PatientNumInCD8NegativeGroup))
  pooled_table_templet_temp[1,"HR_Random_model"]<-round(exp(meta_result$TE.random),digits = 4)
  pooled_table_templet_temp[1,"CI_lower_Random_model"]<-round(exp(meta_result$lower.random),digits = 4)
  pooled_table_templet_temp[1,"CI_upper_Random_model"]<-round(exp(meta_result$upper.random),digits = 4)
  pooled_table_templet_temp[1,"pval_Random_model"]<-round(meta_result$pval.random,digits = 4)
  pooled_table_templet_temp[1,"HR_Common_model"]<-round(exp(meta_result$TE.common),digits = 4)
  pooled_table_templet_temp[1,"CI_lower_Common_model"]<-round(exp(meta_result$lower.common),digits = 4)
  pooled_table_templet_temp[1,"CI_upper_Common_model"]<-round(exp(meta_result$upper.common),digits = 4)
  pooled_table_templet_temp[1,"pval_Common_model"]<-round(meta_result$pval.common,digits = 4)
  pooled_table_templet_temp[1,"tau"]<-round(meta_result$tau,digits = 4)
  pooled_table_templet_temp[1,"I2"]<-paste0(as.character(round(100*meta_result$I2,digits = 1)),"%")
  pooled_table_templet_temp[1,"pval_Q"]<-round(meta_result$pval.Q,digits = 4)
  pooled_table<-rbind(pooled_table,pooled_table_templet_temp)
} else {
  pooled_table_templet_temp[1,"Patient_numbers"]<-0
  pooled_table_templet_temp[1,"HR_Random_model"]<-NA
  pooled_table_templet_temp[1,"CI_lower_Random_model"]<-NA
  pooled_table_templet_temp[1,"CI_upper_Random_model"]<-NA
  pooled_table_templet_temp[1,"pval_Random_model"]<-NA
  pooled_table_templet_temp[1,"HR_Common_model"]<-NA
  pooled_table_templet_temp[1,"CI_lower_Common_model"]<-NA
  pooled_table_templet_temp[1,"CI_upper_Common_model"]<-NA
  pooled_table_templet_temp[1,"pval_Common_model"]<-NA
  pooled_table_templet_temp[1,"tau"]<-NA
  pooled_table_templet_temp[1,"I2"]<-NA
  pooled_table_templet_temp[1,"pval_Q"]<-NA
  pooled_table<-rbind(pooled_table,pooled_table_templet_temp)
}
#### 02_05_01_03 Primary; Survival after immunotherapy; dichotomous CD8 ####
meta_table<-sum_table[(c(1:25)),]
# 02_05_01 Primary
meta_table<-subset(meta_table,PR_Type=="Primary")
# 02_05_01_03 Primary; Survival after immunotherapy
meta_table<-subset(meta_table,Outcome=="Survival after immunotherapy")
# 02_05_01_03 Primary; Survival after immunotherapy; dichotomous CD8
meta_table<-subset(meta_table,PatientNumInCD8PositiveGroup!="Cox continue CD8")
# exclude results without convergence
# no exclusion
# meta-analysis
meta_result<-metagen(logHR, selogHR, sm = "HR",studlab = ResearchName,data=meta_table)
# forest plot
pdf(paste0(output_dir,"//ForestPlot_Subgroup_PR_Type_Primary_SurvivalAfterImmunotherapy_dichotomousCD8.pdf"),width=10, height=4)
p_forest<-forest(meta_result,print.Q =T,digits=4,digits.pval = 3,digits.pval.Q = 3,
                 test.overall.common = TRUE,
                 test.overall.random = TRUE)
print(p_forest)
dev.off()
# funnel plot
pdf(paste0(output_dir,"//FunnelPlot_Subgroup_PR_Type_Primary_SurvivalAfterImmunotherapy_dichotomousCD8.pdf"),width=6, height=6)
p_funnel<-funnel(meta_result)
print(p_funnel)
dev.off()

# add results to pooled table
pooled_table_templet_temp<-pooled_table_templet
pooled_table_templet_temp[1,"Subgroup"]<-"Primary"
pooled_table_templet_temp[1,"Outcome"]<-"Survival after immunotherapy"
pooled_table_templet_temp[1,"Study_number"]<-nrow(meta_table)
if(nrow(meta_table)>=1){
  pooled_table_templet_temp[1,"Patient_numbers"]<-sum(as.numeric(meta_table$PatientNumInCD8PositiveGroup),as.numeric(meta_table$PatientNumInCD8NegativeGroup))
  pooled_table_templet_temp[1,"HR_Random_model"]<-round(exp(meta_result$TE.random),digits = 4)
  pooled_table_templet_temp[1,"CI_lower_Random_model"]<-round(exp(meta_result$lower.random),digits = 4)
  pooled_table_templet_temp[1,"CI_upper_Random_model"]<-round(exp(meta_result$upper.random),digits = 4)
  pooled_table_templet_temp[1,"pval_Random_model"]<-round(meta_result$pval.random,digits = 4)
  pooled_table_templet_temp[1,"HR_Common_model"]<-round(exp(meta_result$TE.common),digits = 4)
  pooled_table_templet_temp[1,"CI_lower_Common_model"]<-round(exp(meta_result$lower.common),digits = 4)
  pooled_table_templet_temp[1,"CI_upper_Common_model"]<-round(exp(meta_result$upper.common),digits = 4)
  pooled_table_templet_temp[1,"pval_Common_model"]<-round(meta_result$pval.common,digits = 4)
  pooled_table_templet_temp[1,"tau"]<-round(meta_result$tau,digits = 4)
  pooled_table_templet_temp[1,"I2"]<-paste0(as.character(round(100*meta_result$I2,digits = 1)),"%")
  pooled_table_templet_temp[1,"pval_Q"]<-round(meta_result$pval.Q,digits = 4)
  pooled_table<-rbind(pooled_table,pooled_table_templet_temp)
} else {
  pooled_table_templet_temp[1,"Patient_numbers"]<-0
  pooled_table_templet_temp[1,"HR_Random_model"]<-NA
  pooled_table_templet_temp[1,"CI_lower_Random_model"]<-NA
  pooled_table_templet_temp[1,"CI_upper_Random_model"]<-NA
  pooled_table_templet_temp[1,"pval_Random_model"]<-NA
  pooled_table_templet_temp[1,"HR_Common_model"]<-NA
  pooled_table_templet_temp[1,"CI_lower_Common_model"]<-NA
  pooled_table_templet_temp[1,"CI_upper_Common_model"]<-NA
  pooled_table_templet_temp[1,"pval_Common_model"]<-NA
  pooled_table_templet_temp[1,"tau"]<-NA
  pooled_table_templet_temp[1,"I2"]<-NA
  pooled_table_templet_temp[1,"pval_Q"]<-NA
  pooled_table<-rbind(pooled_table,pooled_table_templet_temp)
}


#### 02_05_02_01 Recurrence; OS; dichotomous CD8 ####
meta_table<-sum_table[(c(1:25)),]
# 02_05_02 Recurrence
meta_table<-subset(meta_table,PR_Type=="Recurrence")
# 02_05_02_01 Recurrence; OS
meta_table<-subset(meta_table,Outcome=="OS")
# 02_05_02_01 Recurrence; OS; dichotomous CD8
meta_table<-subset(meta_table,PatientNumInCD8PositiveGroup!="Cox continue CD8")
# exclude results without convergence
# no exclusion
# meta-analysis
# no study left
# meta_result<-metagen(logHR, selogHR, sm = "HR",studlab = ResearchName,data=meta_table)
# # forest plot
# pdf(paste0(output_dir,"//ForestPlot_Subgroup_PR_Type_Recurrence_OS_dichotomousCD8.pdf"),width=10, height=4)
# p_forest<-forest(meta_result,print.Q =T,digits=4,digits.pval = 3,digits.pval.Q = 3,
#                  test.overall.common = TRUE,
#                  test.overall.random = TRUE)
# print(p_forest)
# dev.off()
# # funnel plot
# pdf(paste0(output_dir,"//FunnelPlot_Subgroup_PR_Type_Recurrence_OS_dichotomousCD8.pdf"),width=6, height=6)
# p_funnel<-funnel(meta_result)
# print(p_funnel)
# dev.off()

# add results to pooled table
pooled_table_templet_temp<-pooled_table_templet
pooled_table_templet_temp[1,"Subgroup"]<-"Recurrence"
pooled_table_templet_temp[1,"Outcome"]<-"OS"
pooled_table_templet_temp[1,"Study_number"]<-nrow(meta_table)
if(nrow(meta_table)>=1){
  pooled_table_templet_temp[1,"Patient_numbers"]<-sum(as.numeric(meta_table$PatientNumInCD8PositiveGroup),as.numeric(meta_table$PatientNumInCD8NegativeGroup))
  pooled_table_templet_temp[1,"HR_Random_model"]<-round(exp(meta_result$TE.random),digits = 4)
  pooled_table_templet_temp[1,"CI_lower_Random_model"]<-round(exp(meta_result$lower.random),digits = 4)
  pooled_table_templet_temp[1,"CI_upper_Random_model"]<-round(exp(meta_result$upper.random),digits = 4)
  pooled_table_templet_temp[1,"pval_Random_model"]<-round(meta_result$pval.random,digits = 4)
  pooled_table_templet_temp[1,"HR_Common_model"]<-round(exp(meta_result$TE.common),digits = 4)
  pooled_table_templet_temp[1,"CI_lower_Common_model"]<-round(exp(meta_result$lower.common),digits = 4)
  pooled_table_templet_temp[1,"CI_upper_Common_model"]<-round(exp(meta_result$upper.common),digits = 4)
  pooled_table_templet_temp[1,"pval_Common_model"]<-round(meta_result$pval.common,digits = 4)
  pooled_table_templet_temp[1,"tau"]<-round(meta_result$tau,digits = 4)
  pooled_table_templet_temp[1,"I2"]<-paste0(as.character(round(100*meta_result$I2,digits = 1)),"%")
  pooled_table_templet_temp[1,"pval_Q"]<-round(meta_result$pval.Q,digits = 4)
  pooled_table<-rbind(pooled_table,pooled_table_templet_temp)
} else {
  pooled_table_templet_temp[1,"Patient_numbers"]<-0
  pooled_table_templet_temp[1,"HR_Random_model"]<-NA
  pooled_table_templet_temp[1,"CI_lower_Random_model"]<-NA
  pooled_table_templet_temp[1,"CI_upper_Random_model"]<-NA
  pooled_table_templet_temp[1,"pval_Random_model"]<-NA
  pooled_table_templet_temp[1,"HR_Common_model"]<-NA
  pooled_table_templet_temp[1,"CI_lower_Common_model"]<-NA
  pooled_table_templet_temp[1,"CI_upper_Common_model"]<-NA
  pooled_table_templet_temp[1,"pval_Common_model"]<-NA
  pooled_table_templet_temp[1,"tau"]<-NA
  pooled_table_templet_temp[1,"I2"]<-NA
  pooled_table_templet_temp[1,"pval_Q"]<-NA
  pooled_table<-rbind(pooled_table,pooled_table_templet_temp)
}

#### 02_05_02_02 Recurrence; PFS; dichotomous CD8 ####
meta_table<-sum_table[(c(1:25)),]
# 02_05_02 Recurrence
meta_table<-subset(meta_table,PR_Type=="Recurrence")
# 02_05_02_02 Recurrence; PFS
meta_table<-subset(meta_table,Outcome=="PFS")
# 02_05_02_02 Recurrence; PFS; dichotomous CD8
meta_table<-subset(meta_table,PatientNumInCD8PositiveGroup!="Cox continue CD8")
# exclude results without convergence
# no exclusion
# meta-analysis
meta_result<-metagen(logHR, selogHR, sm = "HR",studlab = ResearchName,data=meta_table)
# forest plot
pdf(paste0(output_dir,"//ForestPlot_Subgroup_PR_Type_Recurrence_PFS_dichotomousCD8.pdf"),width=10, height=4)
p_forest<-forest(meta_result,print.Q =T,digits=4,digits.pval = 3,digits.pval.Q = 3,
                 test.overall.common = TRUE,
                 test.overall.random = TRUE)
print(p_forest)
dev.off()
# funnel plot
pdf(paste0(output_dir,"//FunnelPlot_Subgroup_PR_Type_Recurrence_PFS_dichotomousCD8.pdf"),width=6, height=6)
p_funnel<-funnel(meta_result)
print(p_funnel)
dev.off()

# add results to pooled table
pooled_table_templet_temp<-pooled_table_templet
pooled_table_templet_temp[1,"Subgroup"]<-"Recurrence"
pooled_table_templet_temp[1,"Outcome"]<-"PFS"
pooled_table_templet_temp[1,"Study_number"]<-nrow(meta_table)
if(nrow(meta_table)>=1){
  pooled_table_templet_temp[1,"Patient_numbers"]<-sum(as.numeric(meta_table$PatientNumInCD8PositiveGroup),as.numeric(meta_table$PatientNumInCD8NegativeGroup))
  pooled_table_templet_temp[1,"HR_Random_model"]<-round(exp(meta_result$TE.random),digits = 4)
  pooled_table_templet_temp[1,"CI_lower_Random_model"]<-round(exp(meta_result$lower.random),digits = 4)
  pooled_table_templet_temp[1,"CI_upper_Random_model"]<-round(exp(meta_result$upper.random),digits = 4)
  pooled_table_templet_temp[1,"pval_Random_model"]<-round(meta_result$pval.random,digits = 4)
  pooled_table_templet_temp[1,"HR_Common_model"]<-round(exp(meta_result$TE.common),digits = 4)
  pooled_table_templet_temp[1,"CI_lower_Common_model"]<-round(exp(meta_result$lower.common),digits = 4)
  pooled_table_templet_temp[1,"CI_upper_Common_model"]<-round(exp(meta_result$upper.common),digits = 4)
  pooled_table_templet_temp[1,"pval_Common_model"]<-round(meta_result$pval.common,digits = 4)
  pooled_table_templet_temp[1,"tau"]<-round(meta_result$tau,digits = 4)
  pooled_table_templet_temp[1,"I2"]<-paste0(as.character(round(100*meta_result$I2,digits = 1)),"%")
  pooled_table_templet_temp[1,"pval_Q"]<-round(meta_result$pval.Q,digits = 4)
  pooled_table<-rbind(pooled_table,pooled_table_templet_temp)
} else {
  pooled_table_templet_temp[1,"Patient_numbers"]<-0
  pooled_table_templet_temp[1,"HR_Random_model"]<-NA
  pooled_table_templet_temp[1,"CI_lower_Random_model"]<-NA
  pooled_table_templet_temp[1,"CI_upper_Random_model"]<-NA
  pooled_table_templet_temp[1,"pval_Random_model"]<-NA
  pooled_table_templet_temp[1,"HR_Common_model"]<-NA
  pooled_table_templet_temp[1,"CI_lower_Common_model"]<-NA
  pooled_table_templet_temp[1,"CI_upper_Common_model"]<-NA
  pooled_table_templet_temp[1,"pval_Common_model"]<-NA
  pooled_table_templet_temp[1,"tau"]<-NA
  pooled_table_templet_temp[1,"I2"]<-NA
  pooled_table_templet_temp[1,"pval_Q"]<-NA
  pooled_table<-rbind(pooled_table,pooled_table_templet_temp)
}

#### 02_05_02_03 Recurrence; Survival after immunotherapy; dichotomous CD8 ####
meta_table<-sum_table[(c(1:25)),]
# 02_05_02 Recurrence
meta_table<-subset(meta_table,PR_Type=="Recurrence")
# 02_05_02_03 Recurrence; Survival after immunotherapy
meta_table<-subset(meta_table,Outcome=="Survival after immunotherapy")
# 02_05_02_03 Recurrence; Survival after immunotherapy; dichotomous CD8
meta_table<-subset(meta_table,PatientNumInCD8PositiveGroup!="Cox continue CD8")
# exclude results without convergence
meta_table<-meta_table[(meta_table$PMID!="PMID37148198"),]
# meta-analysis
meta_result<-metagen(logHR, selogHR, sm = "HR",studlab = ResearchName,data=meta_table)
# forest plot
pdf(paste0(output_dir,"//ForestPlot_Subgroup_PR_Type_Recurrence_SurvivalAfterImmunotherapy_dichotomousCD8.pdf"),width=10, height=4)
p_forest<-forest(meta_result,print.Q =T,digits=4,digits.pval = 3,digits.pval.Q = 3,
                 test.overall.common = TRUE,
                 test.overall.random = TRUE)
print(p_forest)
dev.off()
# funnel plot
pdf(paste0(output_dir,"//FunnelPlot_Subgroup_PR_Type_Recurrence_SurvivalAfterImmunotherapy_dichotomousCD8.pdf"),width=6, height=6)
p_funnel<-funnel(meta_result)
print(p_funnel)
dev.off()

# add results to pooled table
pooled_table_templet_temp<-pooled_table_templet
pooled_table_templet_temp[1,"Subgroup"]<-"Recurrence"
pooled_table_templet_temp[1,"Outcome"]<-"Survival after immunotherapy"
pooled_table_templet_temp[1,"Study_number"]<-nrow(meta_table)
if(nrow(meta_table)>=1){
  pooled_table_templet_temp[1,"Patient_numbers"]<-sum(as.numeric(meta_table$PatientNumInCD8PositiveGroup),as.numeric(meta_table$PatientNumInCD8NegativeGroup))
  pooled_table_templet_temp[1,"HR_Random_model"]<-round(exp(meta_result$TE.random),digits = 4)
  pooled_table_templet_temp[1,"CI_lower_Random_model"]<-round(exp(meta_result$lower.random),digits = 4)
  pooled_table_templet_temp[1,"CI_upper_Random_model"]<-round(exp(meta_result$upper.random),digits = 4)
  pooled_table_templet_temp[1,"pval_Random_model"]<-round(meta_result$pval.random,digits = 4)
  pooled_table_templet_temp[1,"HR_Common_model"]<-round(exp(meta_result$TE.common),digits = 4)
  pooled_table_templet_temp[1,"CI_lower_Common_model"]<-round(exp(meta_result$lower.common),digits = 4)
  pooled_table_templet_temp[1,"CI_upper_Common_model"]<-round(exp(meta_result$upper.common),digits = 4)
  pooled_table_templet_temp[1,"pval_Common_model"]<-round(meta_result$pval.common,digits = 4)
  pooled_table_templet_temp[1,"tau"]<-round(meta_result$tau,digits = 4)
  pooled_table_templet_temp[1,"I2"]<-paste0(as.character(round(100*meta_result$I2,digits = 1)),"%")
  pooled_table_templet_temp[1,"pval_Q"]<-round(meta_result$pval.Q,digits = 4)
  pooled_table<-rbind(pooled_table,pooled_table_templet_temp)
} else {
  pooled_table_templet_temp[1,"Patient_numbers"]<-0
  pooled_table_templet_temp[1,"HR_Random_model"]<-NA
  pooled_table_templet_temp[1,"CI_lower_Random_model"]<-NA
  pooled_table_templet_temp[1,"CI_upper_Random_model"]<-NA
  pooled_table_templet_temp[1,"pval_Random_model"]<-NA
  pooled_table_templet_temp[1,"HR_Common_model"]<-NA
  pooled_table_templet_temp[1,"CI_lower_Common_model"]<-NA
  pooled_table_templet_temp[1,"CI_upper_Common_model"]<-NA
  pooled_table_templet_temp[1,"pval_Common_model"]<-NA
  pooled_table_templet_temp[1,"tau"]<-NA
  pooled_table_templet_temp[1,"I2"]<-NA
  pooled_table_templet_temp[1,"pval_Q"]<-NA
  pooled_table<-rbind(pooled_table,pooled_table_templet_temp)
}
#### 02_05 Subgroup_PR_Type end ####


output_dir<-paste0(output_dir_root,"//03_meta_analysis//02_06 Subgroup_Immunotherapy")
dir.create(output_dir)
#### 02_06 Subgroup_Immunotherapy start ####
#### 02_06_01_01 DC Vaccination; OS; dichotomous CD8 ####
meta_table<-sum_table[(c(1:25)),]
# 02_06_01 DC Vaccination
meta_table<-subset(meta_table,Immunotherapy=="DC Vaccination")
# 02_06_01_01 DC Vaccination; OS
meta_table<-subset(meta_table,Outcome=="OS")
# 02_06_01_01 DC Vaccination; OS; dichotomous CD8
meta_table<-subset(meta_table,PatientNumInCD8PositiveGroup!="Cox continue CD8")
# exclude results without convergence
# no exclusion
# meta-analysis
meta_result<-metagen(logHR, selogHR, sm = "HR",studlab = ResearchName,data=meta_table)
# forest plot
pdf(paste0(output_dir,"//ForestPlot_Subgroup_Immunotherapy_DC Vaccination_OS_dichotomousCD8.pdf"),width=10, height=4)
p_forest<-forest(meta_result,print.Q =T,digits=4,digits.pval = 3,digits.pval.Q = 3,
                 test.overall.common = TRUE,
                 test.overall.random = TRUE)
print(p_forest)
dev.off()
# funnel plot
pdf(paste0(output_dir,"//FunnelPlot_Subgroup_Immunotherapy_DC Vaccination_OS_dichotomousCD8.pdf"),width=6, height=6)
p_funnel<-funnel(meta_result)
print(p_funnel)
dev.off()

# add results to pooled table
pooled_table_templet_temp<-pooled_table_templet
pooled_table_templet_temp[1,"Subgroup"]<-"DC Vaccination"
pooled_table_templet_temp[1,"Outcome"]<-"OS"
pooled_table_templet_temp[1,"Study_number"]<-nrow(meta_table)
if(nrow(meta_table)>=1){
  pooled_table_templet_temp[1,"Patient_numbers"]<-sum(as.numeric(meta_table$PatientNumInCD8PositiveGroup),as.numeric(meta_table$PatientNumInCD8NegativeGroup))
  pooled_table_templet_temp[1,"HR_Random_model"]<-round(exp(meta_result$TE.random),digits = 4)
  pooled_table_templet_temp[1,"CI_lower_Random_model"]<-round(exp(meta_result$lower.random),digits = 4)
  pooled_table_templet_temp[1,"CI_upper_Random_model"]<-round(exp(meta_result$upper.random),digits = 4)
  pooled_table_templet_temp[1,"pval_Random_model"]<-round(meta_result$pval.random,digits = 4)
  pooled_table_templet_temp[1,"HR_Common_model"]<-round(exp(meta_result$TE.common),digits = 4)
  pooled_table_templet_temp[1,"CI_lower_Common_model"]<-round(exp(meta_result$lower.common),digits = 4)
  pooled_table_templet_temp[1,"CI_upper_Common_model"]<-round(exp(meta_result$upper.common),digits = 4)
  pooled_table_templet_temp[1,"pval_Common_model"]<-round(meta_result$pval.common,digits = 4)
  pooled_table_templet_temp[1,"tau"]<-round(meta_result$tau,digits = 4)
  pooled_table_templet_temp[1,"I2"]<-paste0(as.character(round(100*meta_result$I2,digits = 1)),"%")
  pooled_table_templet_temp[1,"pval_Q"]<-round(meta_result$pval.Q,digits = 4)
  pooled_table<-rbind(pooled_table,pooled_table_templet_temp)
} else {
  pooled_table_templet_temp[1,"Patient_numbers"]<-0
  pooled_table_templet_temp[1,"HR_Random_model"]<-NA
  pooled_table_templet_temp[1,"CI_lower_Random_model"]<-NA
  pooled_table_templet_temp[1,"CI_upper_Random_model"]<-NA
  pooled_table_templet_temp[1,"pval_Random_model"]<-NA
  pooled_table_templet_temp[1,"HR_Common_model"]<-NA
  pooled_table_templet_temp[1,"CI_lower_Common_model"]<-NA
  pooled_table_templet_temp[1,"CI_upper_Common_model"]<-NA
  pooled_table_templet_temp[1,"pval_Common_model"]<-NA
  pooled_table_templet_temp[1,"tau"]<-NA
  pooled_table_templet_temp[1,"I2"]<-NA
  pooled_table_templet_temp[1,"pval_Q"]<-NA
  pooled_table<-rbind(pooled_table,pooled_table_templet_temp)
}
#### 02_06_01_02 DC Vaccination; PFS; dichotomous CD8 ####
meta_table<-sum_table[(c(1:25)),]
# 02_06_01 DC Vaccination
meta_table<-subset(meta_table,Immunotherapy=="DC Vaccination")
# 02_06_01_02 DC Vaccination; PFS
meta_table<-subset(meta_table,Outcome=="PFS")
# 02_06_01_02 DC Vaccination; PFS; dichotomous CD8
meta_table<-subset(meta_table,PatientNumInCD8PositiveGroup!="Cox continue CD8")
# exclude results without convergence
# no exclusion
# meta-analysis
meta_result<-metagen(logHR, selogHR, sm = "HR",studlab = ResearchName,data=meta_table)
# forest plot
pdf(paste0(output_dir,"//ForestPlot_Subgroup_Immunotherapy_DC Vaccination_PFS_dichotomousCD8.pdf"),width=10, height=4)
p_forest<-forest(meta_result,print.Q =T,digits=4,digits.pval = 3,digits.pval.Q = 3,
                 test.overall.common = TRUE,
                 test.overall.random = TRUE)
print(p_forest)
dev.off()
# funnel plot
pdf(paste0(output_dir,"//FunnelPlot_Subgroup_Immunotherapy_DC Vaccination_PFS_dichotomousCD8.pdf"),width=6, height=6)
p_funnel<-funnel(meta_result)
print(p_funnel)
dev.off()

# add results to pooled table
pooled_table_templet_temp<-pooled_table_templet
pooled_table_templet_temp[1,"Subgroup"]<-"DC Vaccination"
pooled_table_templet_temp[1,"Outcome"]<-"PFS"
pooled_table_templet_temp[1,"Study_number"]<-nrow(meta_table)
if(nrow(meta_table)>=1){
  pooled_table_templet_temp[1,"Patient_numbers"]<-sum(as.numeric(meta_table$PatientNumInCD8PositiveGroup),as.numeric(meta_table$PatientNumInCD8NegativeGroup))
  pooled_table_templet_temp[1,"HR_Random_model"]<-round(exp(meta_result$TE.random),digits = 4)
  pooled_table_templet_temp[1,"CI_lower_Random_model"]<-round(exp(meta_result$lower.random),digits = 4)
  pooled_table_templet_temp[1,"CI_upper_Random_model"]<-round(exp(meta_result$upper.random),digits = 4)
  pooled_table_templet_temp[1,"pval_Random_model"]<-round(meta_result$pval.random,digits = 4)
  pooled_table_templet_temp[1,"HR_Common_model"]<-round(exp(meta_result$TE.common),digits = 4)
  pooled_table_templet_temp[1,"CI_lower_Common_model"]<-round(exp(meta_result$lower.common),digits = 4)
  pooled_table_templet_temp[1,"CI_upper_Common_model"]<-round(exp(meta_result$upper.common),digits = 4)
  pooled_table_templet_temp[1,"pval_Common_model"]<-round(meta_result$pval.common,digits = 4)
  pooled_table_templet_temp[1,"tau"]<-round(meta_result$tau,digits = 4)
  pooled_table_templet_temp[1,"I2"]<-paste0(as.character(round(100*meta_result$I2,digits = 1)),"%")
  pooled_table_templet_temp[1,"pval_Q"]<-round(meta_result$pval.Q,digits = 4)
  pooled_table<-rbind(pooled_table,pooled_table_templet_temp)
} else {
  pooled_table_templet_temp[1,"Patient_numbers"]<-0
  pooled_table_templet_temp[1,"HR_Random_model"]<-NA
  pooled_table_templet_temp[1,"CI_lower_Random_model"]<-NA
  pooled_table_templet_temp[1,"CI_upper_Random_model"]<-NA
  pooled_table_templet_temp[1,"pval_Random_model"]<-NA
  pooled_table_templet_temp[1,"HR_Common_model"]<-NA
  pooled_table_templet_temp[1,"CI_lower_Common_model"]<-NA
  pooled_table_templet_temp[1,"CI_upper_Common_model"]<-NA
  pooled_table_templet_temp[1,"pval_Common_model"]<-NA
  pooled_table_templet_temp[1,"tau"]<-NA
  pooled_table_templet_temp[1,"I2"]<-NA
  pooled_table_templet_temp[1,"pval_Q"]<-NA
  pooled_table<-rbind(pooled_table,pooled_table_templet_temp)
}
#### 02_06_01_03 DC Vaccination; Survival after immunotherapy; dichotomous CD8 ####
meta_table<-sum_table[(c(1:25)),]
# 02_06_01 DC Vaccination
meta_table<-subset(meta_table,Immunotherapy=="DC Vaccination")
# 02_06_01_03 DC Vaccination; Survival after immunotherapy
meta_table<-subset(meta_table,Outcome=="Survival after immunotherapy")
# 02_06_01_03 DC Vaccination; Survival after immunotherapy; dichotomous CD8
meta_table<-subset(meta_table,PatientNumInCD8PositiveGroup!="Cox continue CD8")
# exclude results without convergence
# no exclusion
# meta-analysis
# no study left
# meta_result<-metagen(logHR, selogHR, sm = "HR",studlab = ResearchName,data=meta_table)
# # forest plot
# pdf(paste0(output_dir,"//ForestPlot_Subgroup_Immunotherapy_DC Vaccination_SurvivalAfterImmunotherapy_dichotomousCD8.pdf"),width=10, height=4)
# p_forest<-forest(meta_result,print.Q =T,digits=4,digits.pval = 3,digits.pval.Q = 3,
#                  test.overall.common = TRUE,
#                  test.overall.random = TRUE)
# print(p_forest)
# dev.off()
# # funnel plot
# pdf(paste0(output_dir,"//FunnelPlot_Subgroup_Immunotherapy_DC Vaccination_SurvivalAfterImmunotherapy_dichotomousCD8.pdf"),width=6, height=6)
# p_funnel<-funnel(meta_result)
# print(p_funnel)
# dev.off()

# add results to pooled table
pooled_table_templet_temp<-pooled_table_templet
pooled_table_templet_temp[1,"Subgroup"]<-"DC Vaccination"
pooled_table_templet_temp[1,"Outcome"]<-"Survival after immunotherapy"
pooled_table_templet_temp[1,"Study_number"]<-nrow(meta_table)
if(nrow(meta_table)>=1){
  pooled_table_templet_temp[1,"Patient_numbers"]<-sum(as.numeric(meta_table$PatientNumInCD8PositiveGroup),as.numeric(meta_table$PatientNumInCD8NegativeGroup))
  pooled_table_templet_temp[1,"HR_Random_model"]<-round(exp(meta_result$TE.random),digits = 4)
  pooled_table_templet_temp[1,"CI_lower_Random_model"]<-round(exp(meta_result$lower.random),digits = 4)
  pooled_table_templet_temp[1,"CI_upper_Random_model"]<-round(exp(meta_result$upper.random),digits = 4)
  pooled_table_templet_temp[1,"pval_Random_model"]<-round(meta_result$pval.random,digits = 4)
  pooled_table_templet_temp[1,"HR_Common_model"]<-round(exp(meta_result$TE.common),digits = 4)
  pooled_table_templet_temp[1,"CI_lower_Common_model"]<-round(exp(meta_result$lower.common),digits = 4)
  pooled_table_templet_temp[1,"CI_upper_Common_model"]<-round(exp(meta_result$upper.common),digits = 4)
  pooled_table_templet_temp[1,"pval_Common_model"]<-round(meta_result$pval.common,digits = 4)
  pooled_table_templet_temp[1,"tau"]<-round(meta_result$tau,digits = 4)
  pooled_table_templet_temp[1,"I2"]<-paste0(as.character(round(100*meta_result$I2,digits = 1)),"%")
  pooled_table_templet_temp[1,"pval_Q"]<-round(meta_result$pval.Q,digits = 4)
  pooled_table<-rbind(pooled_table,pooled_table_templet_temp)
} else {
  pooled_table_templet_temp[1,"Patient_numbers"]<-0
  pooled_table_templet_temp[1,"HR_Random_model"]<-NA
  pooled_table_templet_temp[1,"CI_lower_Random_model"]<-NA
  pooled_table_templet_temp[1,"CI_upper_Random_model"]<-NA
  pooled_table_templet_temp[1,"pval_Random_model"]<-NA
  pooled_table_templet_temp[1,"HR_Common_model"]<-NA
  pooled_table_templet_temp[1,"CI_lower_Common_model"]<-NA
  pooled_table_templet_temp[1,"CI_upper_Common_model"]<-NA
  pooled_table_templet_temp[1,"pval_Common_model"]<-NA
  pooled_table_templet_temp[1,"tau"]<-NA
  pooled_table_templet_temp[1,"I2"]<-NA
  pooled_table_templet_temp[1,"pval_Q"]<-NA
  pooled_table<-rbind(pooled_table,pooled_table_templet_temp)
}


#### 02_06_02_01 Peptide Vaccination; OS; dichotomous CD8 ####
meta_table<-sum_table[(c(1:25)),]
# 02_06_02 Peptide Vaccination
meta_table<-subset(meta_table,Immunotherapy=="Peptide Vaccination")
# 02_06_02_01 Peptide Vaccination; OS
meta_table<-subset(meta_table,Outcome=="OS")
# 02_06_02_01 Peptide Vaccination; OS; dichotomous CD8
meta_table<-subset(meta_table,PatientNumInCD8PositiveGroup!="Cox continue CD8")
# exclude results without convergence
# no exclusion
# meta-analysis
meta_result<-metagen(logHR, selogHR, sm = "HR",studlab = ResearchName,data=meta_table)
# forest plot
pdf(paste0(output_dir,"//ForestPlot_Subgroup_Immunotherapy_Peptide Vaccination_OS_dichotomousCD8.pdf"),width=10, height=4)
p_forest<-forest(meta_result,print.Q =T,digits=4,digits.pval = 3,digits.pval.Q = 3,
                 test.overall.common = TRUE,
                 test.overall.random = TRUE)
print(p_forest)
dev.off()
# funnel plot
pdf(paste0(output_dir,"//FunnelPlot_Subgroup_Immunotherapy_Peptide Vaccination_OS_dichotomousCD8.pdf"),width=6, height=6)
p_funnel<-funnel(meta_result)
print(p_funnel)
dev.off()

# add results to pooled table
pooled_table_templet_temp<-pooled_table_templet
pooled_table_templet_temp[1,"Subgroup"]<-"Peptide Vaccination"
pooled_table_templet_temp[1,"Outcome"]<-"OS"
pooled_table_templet_temp[1,"Study_number"]<-nrow(meta_table)
if(nrow(meta_table)>=1){
  pooled_table_templet_temp[1,"Patient_numbers"]<-sum(as.numeric(meta_table$PatientNumInCD8PositiveGroup),as.numeric(meta_table$PatientNumInCD8NegativeGroup))
  pooled_table_templet_temp[1,"HR_Random_model"]<-round(exp(meta_result$TE.random),digits = 4)
  pooled_table_templet_temp[1,"CI_lower_Random_model"]<-round(exp(meta_result$lower.random),digits = 4)
  pooled_table_templet_temp[1,"CI_upper_Random_model"]<-round(exp(meta_result$upper.random),digits = 4)
  pooled_table_templet_temp[1,"pval_Random_model"]<-round(meta_result$pval.random,digits = 4)
  pooled_table_templet_temp[1,"HR_Common_model"]<-round(exp(meta_result$TE.common),digits = 4)
  pooled_table_templet_temp[1,"CI_lower_Common_model"]<-round(exp(meta_result$lower.common),digits = 4)
  pooled_table_templet_temp[1,"CI_upper_Common_model"]<-round(exp(meta_result$upper.common),digits = 4)
  pooled_table_templet_temp[1,"pval_Common_model"]<-round(meta_result$pval.common,digits = 4)
  pooled_table_templet_temp[1,"tau"]<-round(meta_result$tau,digits = 4)
  pooled_table_templet_temp[1,"I2"]<-paste0(as.character(round(100*meta_result$I2,digits = 1)),"%")
  pooled_table_templet_temp[1,"pval_Q"]<-round(meta_result$pval.Q,digits = 4)
  pooled_table<-rbind(pooled_table,pooled_table_templet_temp)
} else {
  pooled_table_templet_temp[1,"Patient_numbers"]<-0
  pooled_table_templet_temp[1,"HR_Random_model"]<-NA
  pooled_table_templet_temp[1,"CI_lower_Random_model"]<-NA
  pooled_table_templet_temp[1,"CI_upper_Random_model"]<-NA
  pooled_table_templet_temp[1,"pval_Random_model"]<-NA
  pooled_table_templet_temp[1,"HR_Common_model"]<-NA
  pooled_table_templet_temp[1,"CI_lower_Common_model"]<-NA
  pooled_table_templet_temp[1,"CI_upper_Common_model"]<-NA
  pooled_table_templet_temp[1,"pval_Common_model"]<-NA
  pooled_table_templet_temp[1,"tau"]<-NA
  pooled_table_templet_temp[1,"I2"]<-NA
  pooled_table_templet_temp[1,"pval_Q"]<-NA
  pooled_table<-rbind(pooled_table,pooled_table_templet_temp)
}
#### 02_06_02_02 Peptide Vaccination; PFS; dichotomous CD8 ####
meta_table<-sum_table[(c(1:25)),]
# 02_06_02 Peptide Vaccination
meta_table<-subset(meta_table,Immunotherapy=="Peptide Vaccination")
# 02_06_02_02 Peptide Vaccination; PFS
meta_table<-subset(meta_table,Outcome=="PFS")
# 02_06_02_02 Peptide Vaccination; PFS; dichotomous CD8
meta_table<-subset(meta_table,PatientNumInCD8PositiveGroup!="Cox continue CD8")
# exclude results without convergence
# no exclusion
# meta-analysis
meta_result<-metagen(logHR, selogHR, sm = "HR",studlab = ResearchName,data=meta_table)
# forest plot
pdf(paste0(output_dir,"//ForestPlot_Subgroup_Immunotherapy_Peptide Vaccination_PFS_dichotomousCD8.pdf"),width=10, height=4)
p_forest<-forest(meta_result,print.Q =T,digits=4,digits.pval = 3,digits.pval.Q = 3,
                 test.overall.common = TRUE,
                 test.overall.random = TRUE)
print(p_forest)
dev.off()
# funnel plot
pdf(paste0(output_dir,"//FunnelPlot_Subgroup_Immunotherapy_Peptide Vaccination_PFS_dichotomousCD8.pdf"),width=6, height=6)
p_funnel<-funnel(meta_result)
print(p_funnel)
dev.off()

# add results to pooled table
pooled_table_templet_temp<-pooled_table_templet
pooled_table_templet_temp[1,"Subgroup"]<-"Peptide Vaccination"
pooled_table_templet_temp[1,"Outcome"]<-"PFS"
pooled_table_templet_temp[1,"Study_number"]<-nrow(meta_table)
if(nrow(meta_table)>=1){
  pooled_table_templet_temp[1,"Patient_numbers"]<-sum(as.numeric(meta_table$PatientNumInCD8PositiveGroup),as.numeric(meta_table$PatientNumInCD8NegativeGroup))
  pooled_table_templet_temp[1,"HR_Random_model"]<-round(exp(meta_result$TE.random),digits = 4)
  pooled_table_templet_temp[1,"CI_lower_Random_model"]<-round(exp(meta_result$lower.random),digits = 4)
  pooled_table_templet_temp[1,"CI_upper_Random_model"]<-round(exp(meta_result$upper.random),digits = 4)
  pooled_table_templet_temp[1,"pval_Random_model"]<-round(meta_result$pval.random,digits = 4)
  pooled_table_templet_temp[1,"HR_Common_model"]<-round(exp(meta_result$TE.common),digits = 4)
  pooled_table_templet_temp[1,"CI_lower_Common_model"]<-round(exp(meta_result$lower.common),digits = 4)
  pooled_table_templet_temp[1,"CI_upper_Common_model"]<-round(exp(meta_result$upper.common),digits = 4)
  pooled_table_templet_temp[1,"pval_Common_model"]<-round(meta_result$pval.common,digits = 4)
  pooled_table_templet_temp[1,"tau"]<-round(meta_result$tau,digits = 4)
  pooled_table_templet_temp[1,"I2"]<-paste0(as.character(round(100*meta_result$I2,digits = 1)),"%")
  pooled_table_templet_temp[1,"pval_Q"]<-round(meta_result$pval.Q,digits = 4)
  pooled_table<-rbind(pooled_table,pooled_table_templet_temp)
} else {
  pooled_table_templet_temp[1,"Patient_numbers"]<-0
  pooled_table_templet_temp[1,"HR_Random_model"]<-NA
  pooled_table_templet_temp[1,"CI_lower_Random_model"]<-NA
  pooled_table_templet_temp[1,"CI_upper_Random_model"]<-NA
  pooled_table_templet_temp[1,"pval_Random_model"]<-NA
  pooled_table_templet_temp[1,"HR_Common_model"]<-NA
  pooled_table_templet_temp[1,"CI_lower_Common_model"]<-NA
  pooled_table_templet_temp[1,"CI_upper_Common_model"]<-NA
  pooled_table_templet_temp[1,"pval_Common_model"]<-NA
  pooled_table_templet_temp[1,"tau"]<-NA
  pooled_table_templet_temp[1,"I2"]<-NA
  pooled_table_templet_temp[1,"pval_Q"]<-NA
  pooled_table<-rbind(pooled_table,pooled_table_templet_temp)
}

#### 02_06_02_03 Peptide Vaccination; Survival after immunotherapy; dichotomous CD8 ####
meta_table<-sum_table[(c(1:25)),]
# 02_06_02 Peptide Vaccination
meta_table<-subset(meta_table,Immunotherapy=="Peptide Vaccination")
# 02_06_02_03 Peptide Vaccination; Survival after immunotherapy
meta_table<-subset(meta_table,Outcome=="Survival after immunotherapy")
# 02_06_02_03 Peptide Vaccination; Survival after immunotherapy; dichotomous CD8
meta_table<-subset(meta_table,PatientNumInCD8PositiveGroup!="Cox continue CD8")
# exclude results without convergence
# no exclusion
# meta-analysis
# no study left
# meta_result<-metagen(logHR, selogHR, sm = "HR",studlab = ResearchName,data=meta_table)
# # forest plot
# pdf(paste0(output_dir,"//ForestPlot_Subgroup_Immunotherapy_Peptide Vaccination_SurvivalAfterImmunotherapy_dichotomousCD8.pdf"),width=10, height=4)
# p_forest<-forest(meta_result,print.Q =T,digits=4,digits.pval = 3,digits.pval.Q = 3,
#                  test.overall.common = TRUE,
#                  test.overall.random = TRUE)
# print(p_forest)
# dev.off()
# # funnel plot
# pdf(paste0(output_dir,"//FunnelPlot_Subgroup_Immunotherapy_Peptide Vaccination_SurvivalAfterImmunotherapy_dichotomousCD8.pdf"),width=6, height=6)
# p_funnel<-funnel(meta_result)
# print(p_funnel)
# dev.off()

# add results to pooled table
pooled_table_templet_temp<-pooled_table_templet
pooled_table_templet_temp[1,"Subgroup"]<-"Peptide Vaccination"
pooled_table_templet_temp[1,"Outcome"]<-"Survival after immunotherapy"
pooled_table_templet_temp[1,"Study_number"]<-nrow(meta_table)
if(nrow(meta_table)>=1){
  pooled_table_templet_temp[1,"Patient_numbers"]<-sum(as.numeric(meta_table$PatientNumInCD8PositiveGroup),as.numeric(meta_table$PatientNumInCD8NegativeGroup))
  pooled_table_templet_temp[1,"HR_Random_model"]<-round(exp(meta_result$TE.random),digits = 4)
  pooled_table_templet_temp[1,"CI_lower_Random_model"]<-round(exp(meta_result$lower.random),digits = 4)
  pooled_table_templet_temp[1,"CI_upper_Random_model"]<-round(exp(meta_result$upper.random),digits = 4)
  pooled_table_templet_temp[1,"pval_Random_model"]<-round(meta_result$pval.random,digits = 4)
  pooled_table_templet_temp[1,"HR_Common_model"]<-round(exp(meta_result$TE.common),digits = 4)
  pooled_table_templet_temp[1,"CI_lower_Common_model"]<-round(exp(meta_result$lower.common),digits = 4)
  pooled_table_templet_temp[1,"CI_upper_Common_model"]<-round(exp(meta_result$upper.common),digits = 4)
  pooled_table_templet_temp[1,"pval_Common_model"]<-round(meta_result$pval.common,digits = 4)
  pooled_table_templet_temp[1,"tau"]<-round(meta_result$tau,digits = 4)
  pooled_table_templet_temp[1,"I2"]<-paste0(as.character(round(100*meta_result$I2,digits = 1)),"%")
  pooled_table_templet_temp[1,"pval_Q"]<-round(meta_result$pval.Q,digits = 4)
  pooled_table<-rbind(pooled_table,pooled_table_templet_temp)
} else {
  pooled_table_templet_temp[1,"Patient_numbers"]<-0
  pooled_table_templet_temp[1,"HR_Random_model"]<-NA
  pooled_table_templet_temp[1,"CI_lower_Random_model"]<-NA
  pooled_table_templet_temp[1,"CI_upper_Random_model"]<-NA
  pooled_table_templet_temp[1,"pval_Random_model"]<-NA
  pooled_table_templet_temp[1,"HR_Common_model"]<-NA
  pooled_table_templet_temp[1,"CI_lower_Common_model"]<-NA
  pooled_table_templet_temp[1,"CI_upper_Common_model"]<-NA
  pooled_table_templet_temp[1,"pval_Common_model"]<-NA
  pooled_table_templet_temp[1,"tau"]<-NA
  pooled_table_templet_temp[1,"I2"]<-NA
  pooled_table_templet_temp[1,"pval_Q"]<-NA
  pooled_table<-rbind(pooled_table,pooled_table_templet_temp)
}


#### 02_06_03_01 Oncolytic viral vectors therapy; OS; dichotomous CD8 ####
meta_table<-sum_table[(c(1:25)),]
# 02_06_03 Oncolytic viral vectors therapy
meta_table<-subset(meta_table,Immunotherapy=="Oncolytic viral vectors therapy")
# 02_06_03_01 Oncolytic viral vectors therapy; OS
meta_table<-subset(meta_table,Outcome=="OS")
# 02_06_03_01 Oncolytic viral vectors therapy; OS; dichotomous CD8
meta_table<-subset(meta_table,PatientNumInCD8PositiveGroup!="Cox continue CD8")
# exclude results without convergence
# no exclusion
# meta-analysis
# no study left
# meta_result<-metagen(logHR, selogHR, sm = "HR",studlab = ResearchName,data=meta_table)
# # forest plot
# pdf(paste0(output_dir,"//ForestPlot_Subgroup_Immunotherapy_Oncolytic viral vectors therapy_OS_dichotomousCD8.pdf"),width=10, height=4)
# p_forest<-forest(meta_result,print.Q =T,digits=4,digits.pval = 3,digits.pval.Q = 3,
#                  test.overall.common = TRUE,
#                  test.overall.random = TRUE)
# print(p_forest)
# dev.off()
# # funnel plot
# pdf(paste0(output_dir,"//FunnelPlot_Subgroup_Immunotherapy_Oncolytic viral vectors therapy_OS_dichotomousCD8.pdf"),width=6, height=6)
# p_funnel<-funnel(meta_result)
# print(p_funnel)
# dev.off()

# add results to pooled table
pooled_table_templet_temp<-pooled_table_templet
pooled_table_templet_temp[1,"Subgroup"]<-"Oncolytic viral vectors therapy"
pooled_table_templet_temp[1,"Outcome"]<-"OS"
pooled_table_templet_temp[1,"Study_number"]<-nrow(meta_table)
if(nrow(meta_table)>=1){
  pooled_table_templet_temp[1,"Patient_numbers"]<-sum(as.numeric(meta_table$PatientNumInCD8PositiveGroup),as.numeric(meta_table$PatientNumInCD8NegativeGroup))
  pooled_table_templet_temp[1,"HR_Random_model"]<-round(exp(meta_result$TE.random),digits = 4)
  pooled_table_templet_temp[1,"CI_lower_Random_model"]<-round(exp(meta_result$lower.random),digits = 4)
  pooled_table_templet_temp[1,"CI_upper_Random_model"]<-round(exp(meta_result$upper.random),digits = 4)
  pooled_table_templet_temp[1,"pval_Random_model"]<-round(meta_result$pval.random,digits = 4)
  pooled_table_templet_temp[1,"HR_Common_model"]<-round(exp(meta_result$TE.common),digits = 4)
  pooled_table_templet_temp[1,"CI_lower_Common_model"]<-round(exp(meta_result$lower.common),digits = 4)
  pooled_table_templet_temp[1,"CI_upper_Common_model"]<-round(exp(meta_result$upper.common),digits = 4)
  pooled_table_templet_temp[1,"pval_Common_model"]<-round(meta_result$pval.common,digits = 4)
  pooled_table_templet_temp[1,"tau"]<-round(meta_result$tau,digits = 4)
  pooled_table_templet_temp[1,"I2"]<-paste0(as.character(round(100*meta_result$I2,digits = 1)),"%")
  pooled_table_templet_temp[1,"pval_Q"]<-round(meta_result$pval.Q,digits = 4)
  pooled_table<-rbind(pooled_table,pooled_table_templet_temp)
} else {
  pooled_table_templet_temp[1,"Patient_numbers"]<-0
  pooled_table_templet_temp[1,"HR_Random_model"]<-NA
  pooled_table_templet_temp[1,"CI_lower_Random_model"]<-NA
  pooled_table_templet_temp[1,"CI_upper_Random_model"]<-NA
  pooled_table_templet_temp[1,"pval_Random_model"]<-NA
  pooled_table_templet_temp[1,"HR_Common_model"]<-NA
  pooled_table_templet_temp[1,"CI_lower_Common_model"]<-NA
  pooled_table_templet_temp[1,"CI_upper_Common_model"]<-NA
  pooled_table_templet_temp[1,"pval_Common_model"]<-NA
  pooled_table_templet_temp[1,"tau"]<-NA
  pooled_table_templet_temp[1,"I2"]<-NA
  pooled_table_templet_temp[1,"pval_Q"]<-NA
  pooled_table<-rbind(pooled_table,pooled_table_templet_temp)
}

#### 02_06_03_02 Oncolytic viral vectors therapy; PFS; dichotomous CD8 ####
meta_table<-sum_table[(c(1:25)),]
# 02_06_03 Oncolytic viral vectors therapy
meta_table<-subset(meta_table,Immunotherapy=="Oncolytic viral vectors therapy")
# 02_06_03_02 Oncolytic viral vectors therapy; PFS
meta_table<-subset(meta_table,Outcome=="PFS")
# 02_06_03_02 Oncolytic viral vectors therapy; PFS; dichotomous CD8
meta_table<-subset(meta_table,PatientNumInCD8PositiveGroup!="Cox continue CD8")
# exclude results without convergence
# no exclusion
# meta-analysis
# no study left
# meta_result<-metagen(logHR, selogHR, sm = "HR",studlab = ResearchName,data=meta_table)
# # forest plot
# pdf(paste0(output_dir,"//ForestPlot_Subgroup_Immunotherapy_Oncolytic viral vectors therapy_PFS_dichotomousCD8.pdf"),width=10, height=4)
# p_forest<-forest(meta_result,print.Q =T,digits=4,digits.pval = 3,digits.pval.Q = 3,
#                  test.overall.common = TRUE,
#                  test.overall.random = TRUE)
# print(p_forest)
# dev.off()
# # funnel plot
# pdf(paste0(output_dir,"//FunnelPlot_Subgroup_Immunotherapy_Oncolytic viral vectors therapy_PFS_dichotomousCD8.pdf"),width=6, height=6)
# p_funnel<-funnel(meta_result)
# print(p_funnel)
# dev.off()

# add results to pooled table
pooled_table_templet_temp<-pooled_table_templet
pooled_table_templet_temp[1,"Subgroup"]<-"Oncolytic viral vectors therapy"
pooled_table_templet_temp[1,"Outcome"]<-"PFS"
pooled_table_templet_temp[1,"Study_number"]<-nrow(meta_table)
if(nrow(meta_table)>=1){
  pooled_table_templet_temp[1,"Patient_numbers"]<-sum(as.numeric(meta_table$PatientNumInCD8PositiveGroup),as.numeric(meta_table$PatientNumInCD8NegativeGroup))
  pooled_table_templet_temp[1,"HR_Random_model"]<-round(exp(meta_result$TE.random),digits = 4)
  pooled_table_templet_temp[1,"CI_lower_Random_model"]<-round(exp(meta_result$lower.random),digits = 4)
  pooled_table_templet_temp[1,"CI_upper_Random_model"]<-round(exp(meta_result$upper.random),digits = 4)
  pooled_table_templet_temp[1,"pval_Random_model"]<-round(meta_result$pval.random,digits = 4)
  pooled_table_templet_temp[1,"HR_Common_model"]<-round(exp(meta_result$TE.common),digits = 4)
  pooled_table_templet_temp[1,"CI_lower_Common_model"]<-round(exp(meta_result$lower.common),digits = 4)
  pooled_table_templet_temp[1,"CI_upper_Common_model"]<-round(exp(meta_result$upper.common),digits = 4)
  pooled_table_templet_temp[1,"pval_Common_model"]<-round(meta_result$pval.common,digits = 4)
  pooled_table_templet_temp[1,"tau"]<-round(meta_result$tau,digits = 4)
  pooled_table_templet_temp[1,"I2"]<-paste0(as.character(round(100*meta_result$I2,digits = 1)),"%")
  pooled_table_templet_temp[1,"pval_Q"]<-round(meta_result$pval.Q,digits = 4)
  pooled_table<-rbind(pooled_table,pooled_table_templet_temp)
} else {
  pooled_table_templet_temp[1,"Patient_numbers"]<-0
  pooled_table_templet_temp[1,"HR_Random_model"]<-NA
  pooled_table_templet_temp[1,"CI_lower_Random_model"]<-NA
  pooled_table_templet_temp[1,"CI_upper_Random_model"]<-NA
  pooled_table_templet_temp[1,"pval_Random_model"]<-NA
  pooled_table_templet_temp[1,"HR_Common_model"]<-NA
  pooled_table_templet_temp[1,"CI_lower_Common_model"]<-NA
  pooled_table_templet_temp[1,"CI_upper_Common_model"]<-NA
  pooled_table_templet_temp[1,"pval_Common_model"]<-NA
  pooled_table_templet_temp[1,"tau"]<-NA
  pooled_table_templet_temp[1,"I2"]<-NA
  pooled_table_templet_temp[1,"pval_Q"]<-NA
  pooled_table<-rbind(pooled_table,pooled_table_templet_temp)
}

#### 02_06_03_03 Oncolytic viral vectors therapy; Survival after immunotherapy; dichotomous CD8 ####
meta_table<-sum_table[(c(1:25)),]
# 02_06_03 Oncolytic viral vectors therapy
meta_table<-subset(meta_table,Immunotherapy=="Oncolytic viral vectors therapy")
# 02_06_03_03 Oncolytic viral vectors therapy; Survival after immunotherapy
meta_table<-subset(meta_table,Outcome=="Survival after immunotherapy")
# 02_06_03_03 Oncolytic viral vectors therapy; Survival after immunotherapy; dichotomous CD8
meta_table<-subset(meta_table,PatientNumInCD8PositiveGroup!="Cox continue CD8")
# exclude results without convergence
# no exclusion
# meta-analysis
meta_result<-metagen(logHR, selogHR, sm = "HR",studlab = ResearchName,data=meta_table)
# forest plot
pdf(paste0(output_dir,"//ForestPlot_Subgroup_Immunotherapy_Oncolytic viral vectors therapy_SurvivalAfterImmunotherapy_dichotomousCD8.pdf"),width=10, height=4)
p_forest<-forest(meta_result,print.Q =T,digits=4,digits.pval = 3,digits.pval.Q = 3,
                 test.overall.common = TRUE,
                 test.overall.random = TRUE)
print(p_forest)
dev.off()
# funnel plot
pdf(paste0(output_dir,"//FunnelPlot_Subgroup_Immunotherapy_Oncolytic viral vectors therapy_SurvivalAfterImmunotherapy_dichotomousCD8.pdf"),width=6, height=6)
p_funnel<-funnel(meta_result)
print(p_funnel)
dev.off()

# add results to pooled table
pooled_table_templet_temp<-pooled_table_templet
pooled_table_templet_temp[1,"Subgroup"]<-"Oncolytic viral vectors therapy"
pooled_table_templet_temp[1,"Outcome"]<-"Survival after immunotherapy"
pooled_table_templet_temp[1,"Study_number"]<-nrow(meta_table)
if(nrow(meta_table)>=1){
  pooled_table_templet_temp[1,"Patient_numbers"]<-sum(as.numeric(meta_table$PatientNumInCD8PositiveGroup),as.numeric(meta_table$PatientNumInCD8NegativeGroup))
  pooled_table_templet_temp[1,"HR_Random_model"]<-round(exp(meta_result$TE.random),digits = 4)
  pooled_table_templet_temp[1,"CI_lower_Random_model"]<-round(exp(meta_result$lower.random),digits = 4)
  pooled_table_templet_temp[1,"CI_upper_Random_model"]<-round(exp(meta_result$upper.random),digits = 4)
  pooled_table_templet_temp[1,"pval_Random_model"]<-round(meta_result$pval.random,digits = 4)
  pooled_table_templet_temp[1,"HR_Common_model"]<-round(exp(meta_result$TE.common),digits = 4)
  pooled_table_templet_temp[1,"CI_lower_Common_model"]<-round(exp(meta_result$lower.common),digits = 4)
  pooled_table_templet_temp[1,"CI_upper_Common_model"]<-round(exp(meta_result$upper.common),digits = 4)
  pooled_table_templet_temp[1,"pval_Common_model"]<-round(meta_result$pval.common,digits = 4)
  pooled_table_templet_temp[1,"tau"]<-round(meta_result$tau,digits = 4)
  pooled_table_templet_temp[1,"I2"]<-paste0(as.character(round(100*meta_result$I2,digits = 1)),"%")
  pooled_table_templet_temp[1,"pval_Q"]<-round(meta_result$pval.Q,digits = 4)
  pooled_table<-rbind(pooled_table,pooled_table_templet_temp)
} else {
  pooled_table_templet_temp[1,"Patient_numbers"]<-0
  pooled_table_templet_temp[1,"HR_Random_model"]<-NA
  pooled_table_templet_temp[1,"CI_lower_Random_model"]<-NA
  pooled_table_templet_temp[1,"CI_upper_Random_model"]<-NA
  pooled_table_templet_temp[1,"pval_Random_model"]<-NA
  pooled_table_templet_temp[1,"HR_Common_model"]<-NA
  pooled_table_templet_temp[1,"CI_lower_Common_model"]<-NA
  pooled_table_templet_temp[1,"CI_upper_Common_model"]<-NA
  pooled_table_templet_temp[1,"pval_Common_model"]<-NA
  pooled_table_templet_temp[1,"tau"]<-NA
  pooled_table_templet_temp[1,"I2"]<-NA
  pooled_table_templet_temp[1,"pval_Q"]<-NA
  pooled_table<-rbind(pooled_table,pooled_table_templet_temp)
}

#### 02_06_04_01 Immune Checkpoint Inhibitors; OS; dichotomous CD8 ####
meta_table<-sum_table[(c(1:25)),]
# 02_06_04 Immune Checkpoint Inhibitors
meta_table<-subset(meta_table,Immunotherapy=="Immune Checkpoint Inhibitors")
# 02_06_04_01 Immune Checkpoint Inhibitors; OS
meta_table<-subset(meta_table,Outcome=="OS")
# 02_06_04_01 Immune Checkpoint Inhibitors; OS; dichotomous CD8
meta_table<-subset(meta_table,PatientNumInCD8PositiveGroup!="Cox continue CD8")
# exclude results without convergence
# no exclusion
# meta-analysis
# no study left
# meta_result<-metagen(logHR, selogHR, sm = "HR",studlab = ResearchName,data=meta_table)
# # forest plot
# pdf(paste0(output_dir,"//ForestPlot_Subgroup_Immunotherapy_Immune Checkpoint Inhibitors_OS_dichotomousCD8.pdf"),width=10, height=4)
# p_forest<-forest(meta_result,print.Q =T,digits=4,digits.pval = 3,digits.pval.Q = 3,
#                  test.overall.common = TRUE,
#                  test.overall.random = TRUE)
# print(p_forest)
# dev.off()
# # funnel plot
# pdf(paste0(output_dir,"//FunnelPlot_Subgroup_Immunotherapy_Immune Checkpoint Inhibitors_OS_dichotomousCD8.pdf"),width=6, height=6)
# p_funnel<-funnel(meta_result)
# print(p_funnel)
# dev.off()

# add results to pooled table
pooled_table_templet_temp<-pooled_table_templet
pooled_table_templet_temp[1,"Subgroup"]<-"Immune Checkpoint Inhibitors"
pooled_table_templet_temp[1,"Outcome"]<-"OS"
pooled_table_templet_temp[1,"Study_number"]<-nrow(meta_table)
if(nrow(meta_table)>=1){
  pooled_table_templet_temp[1,"Patient_numbers"]<-sum(as.numeric(meta_table$PatientNumInCD8PositiveGroup),as.numeric(meta_table$PatientNumInCD8NegativeGroup))
  pooled_table_templet_temp[1,"HR_Random_model"]<-round(exp(meta_result$TE.random),digits = 4)
  pooled_table_templet_temp[1,"CI_lower_Random_model"]<-round(exp(meta_result$lower.random),digits = 4)
  pooled_table_templet_temp[1,"CI_upper_Random_model"]<-round(exp(meta_result$upper.random),digits = 4)
  pooled_table_templet_temp[1,"pval_Random_model"]<-round(meta_result$pval.random,digits = 4)
  pooled_table_templet_temp[1,"HR_Common_model"]<-round(exp(meta_result$TE.common),digits = 4)
  pooled_table_templet_temp[1,"CI_lower_Common_model"]<-round(exp(meta_result$lower.common),digits = 4)
  pooled_table_templet_temp[1,"CI_upper_Common_model"]<-round(exp(meta_result$upper.common),digits = 4)
  pooled_table_templet_temp[1,"pval_Common_model"]<-round(meta_result$pval.common,digits = 4)
  pooled_table_templet_temp[1,"tau"]<-round(meta_result$tau,digits = 4)
  pooled_table_templet_temp[1,"I2"]<-paste0(as.character(round(100*meta_result$I2,digits = 1)),"%")
  pooled_table_templet_temp[1,"pval_Q"]<-round(meta_result$pval.Q,digits = 4)
  pooled_table<-rbind(pooled_table,pooled_table_templet_temp)
} else {
  pooled_table_templet_temp[1,"Patient_numbers"]<-0
  pooled_table_templet_temp[1,"HR_Random_model"]<-NA
  pooled_table_templet_temp[1,"CI_lower_Random_model"]<-NA
  pooled_table_templet_temp[1,"CI_upper_Random_model"]<-NA
  pooled_table_templet_temp[1,"pval_Random_model"]<-NA
  pooled_table_templet_temp[1,"HR_Common_model"]<-NA
  pooled_table_templet_temp[1,"CI_lower_Common_model"]<-NA
  pooled_table_templet_temp[1,"CI_upper_Common_model"]<-NA
  pooled_table_templet_temp[1,"pval_Common_model"]<-NA
  pooled_table_templet_temp[1,"tau"]<-NA
  pooled_table_templet_temp[1,"I2"]<-NA
  pooled_table_templet_temp[1,"pval_Q"]<-NA
  pooled_table<-rbind(pooled_table,pooled_table_templet_temp)
}
#### 02_06_04_02 Immune Checkpoint Inhibitors; PFS; dichotomous CD8 ####
meta_table<-sum_table[(c(1:25)),]
# 02_06_04 Immune Checkpoint Inhibitors
meta_table<-subset(meta_table,Immunotherapy=="Immune Checkpoint Inhibitors")
# 02_06_04_02 Immune Checkpoint Inhibitors; PFS
meta_table<-subset(meta_table,Outcome=="PFS")
# 02_06_04_02 Immune Checkpoint Inhibitors; PFS; dichotomous CD8
meta_table<-subset(meta_table,PatientNumInCD8PositiveGroup!="Cox continue CD8")
# exclude results without convergence
# no exclusion
# meta-analysis
# no study left
# meta_result<-metagen(logHR, selogHR, sm = "HR",studlab = ResearchName,data=meta_table)
# # forest plot
# pdf(paste0(output_dir,"//ForestPlot_Subgroup_Immunotherapy_Immune Checkpoint Inhibitors_PFS_dichotomousCD8.pdf"),width=10, height=4)
# p_forest<-forest(meta_result,print.Q =T,digits=4,digits.pval = 3,digits.pval.Q = 3,
#                  test.overall.common = TRUE,
#                  test.overall.random = TRUE)
# print(p_forest)
# dev.off()
# # funnel plot
# pdf(paste0(output_dir,"//FunnelPlot_Subgroup_Immunotherapy_Immune Checkpoint Inhibitors_PFS_dichotomousCD8.pdf"),width=6, height=6)
# p_funnel<-funnel(meta_result)
# print(p_funnel)
# dev.off()

# add results to pooled table
pooled_table_templet_temp<-pooled_table_templet
pooled_table_templet_temp[1,"Subgroup"]<-"Immune Checkpoint Inhibitors"
pooled_table_templet_temp[1,"Outcome"]<-"PFS"
pooled_table_templet_temp[1,"Study_number"]<-nrow(meta_table)
if(nrow(meta_table)>=1){
  pooled_table_templet_temp[1,"Patient_numbers"]<-sum(as.numeric(meta_table$PatientNumInCD8PositiveGroup),as.numeric(meta_table$PatientNumInCD8NegativeGroup))
  pooled_table_templet_temp[1,"HR_Random_model"]<-round(exp(meta_result$TE.random),digits = 4)
  pooled_table_templet_temp[1,"CI_lower_Random_model"]<-round(exp(meta_result$lower.random),digits = 4)
  pooled_table_templet_temp[1,"CI_upper_Random_model"]<-round(exp(meta_result$upper.random),digits = 4)
  pooled_table_templet_temp[1,"pval_Random_model"]<-round(meta_result$pval.random,digits = 4)
  pooled_table_templet_temp[1,"HR_Common_model"]<-round(exp(meta_result$TE.common),digits = 4)
  pooled_table_templet_temp[1,"CI_lower_Common_model"]<-round(exp(meta_result$lower.common),digits = 4)
  pooled_table_templet_temp[1,"CI_upper_Common_model"]<-round(exp(meta_result$upper.common),digits = 4)
  pooled_table_templet_temp[1,"pval_Common_model"]<-round(meta_result$pval.common,digits = 4)
  pooled_table_templet_temp[1,"tau"]<-round(meta_result$tau,digits = 4)
  pooled_table_templet_temp[1,"I2"]<-paste0(as.character(round(100*meta_result$I2,digits = 1)),"%")
  pooled_table_templet_temp[1,"pval_Q"]<-round(meta_result$pval.Q,digits = 4)
  pooled_table<-rbind(pooled_table,pooled_table_templet_temp)
} else {
  pooled_table_templet_temp[1,"Patient_numbers"]<-0
  pooled_table_templet_temp[1,"HR_Random_model"]<-NA
  pooled_table_templet_temp[1,"CI_lower_Random_model"]<-NA
  pooled_table_templet_temp[1,"CI_upper_Random_model"]<-NA
  pooled_table_templet_temp[1,"pval_Random_model"]<-NA
  pooled_table_templet_temp[1,"HR_Common_model"]<-NA
  pooled_table_templet_temp[1,"CI_lower_Common_model"]<-NA
  pooled_table_templet_temp[1,"CI_upper_Common_model"]<-NA
  pooled_table_templet_temp[1,"pval_Common_model"]<-NA
  pooled_table_templet_temp[1,"tau"]<-NA
  pooled_table_templet_temp[1,"I2"]<-NA
  pooled_table_templet_temp[1,"pval_Q"]<-NA
  pooled_table<-rbind(pooled_table,pooled_table_templet_temp)
}

#### 02_06_04_03 Immune Checkpoint Inhibitors; Survival after immunotherapy; dichotomous CD8 ####
meta_table<-sum_table[(c(1:25)),]
# 02_06_04 Immune Checkpoint Inhibitors
meta_table<-subset(meta_table,Immunotherapy=="Immune Checkpoint Inhibitors")
# 02_06_04_03 Immune Checkpoint Inhibitors; Survival after immunotherapy
meta_table<-subset(meta_table,Outcome=="Survival after immunotherapy")
# 02_06_04_03 Immune Checkpoint Inhibitors; Survival after immunotherapy; dichotomous CD8
meta_table<-subset(meta_table,PatientNumInCD8PositiveGroup!="Cox continue CD8")
# exclude results without convergence
# no exclusion
# meta-analysis

meta_result<-metagen(logHR, selogHR, sm = "HR",studlab = ResearchName,data=meta_table)

# forest plot
pdf(paste0(output_dir,"//ForestPlot_Subgroup_Immunotherapy_Immune Checkpoint Inhibitors_SurvivalAfterImmunotherapy_dichotomousCD8.pdf"),width=10, height=4)
p_forest<-forest(meta_result,print.Q =T,digits=4,digits.pval = 3,digits.pval.Q = 3,
                 test.overall.common = TRUE,
                 test.overall.random = TRUE)
print(p_forest)
dev.off()
# funnel plot
pdf(paste0(output_dir,"//FunnelPlot_Subgroup_Immunotherapy_Immune Checkpoint Inhibitors_SurvivalAfterImmunotherapy_dichotomousCD8.pdf"),width=6, height=6)
p_funnel<-funnel(meta_result)
print(p_funnel)
dev.off()

# add results to pooled table
pooled_table_templet_temp<-pooled_table_templet
pooled_table_templet_temp[1,"Subgroup"]<-"Immune Checkpoint Inhibitors"
pooled_table_templet_temp[1,"Outcome"]<-"Survival after immunotherapy"
pooled_table_templet_temp[1,"Study_number"]<-nrow(meta_table)
if(nrow(meta_table)>=1){
  pooled_table_templet_temp[1,"Patient_numbers"]<-sum(as.numeric(meta_table$PatientNumInCD8PositiveGroup),as.numeric(meta_table$PatientNumInCD8NegativeGroup))
  pooled_table_templet_temp[1,"HR_Random_model"]<-round(exp(meta_result$TE.random),digits = 4)
  pooled_table_templet_temp[1,"CI_lower_Random_model"]<-round(exp(meta_result$lower.random),digits = 4)
  pooled_table_templet_temp[1,"CI_upper_Random_model"]<-round(exp(meta_result$upper.random),digits = 4)
  pooled_table_templet_temp[1,"pval_Random_model"]<-round(meta_result$pval.random,digits = 4)
  pooled_table_templet_temp[1,"HR_Common_model"]<-round(exp(meta_result$TE.common),digits = 4)
  pooled_table_templet_temp[1,"CI_lower_Common_model"]<-round(exp(meta_result$lower.common),digits = 4)
  pooled_table_templet_temp[1,"CI_upper_Common_model"]<-round(exp(meta_result$upper.common),digits = 4)
  pooled_table_templet_temp[1,"pval_Common_model"]<-round(meta_result$pval.common,digits = 4)
  pooled_table_templet_temp[1,"tau"]<-round(meta_result$tau,digits = 4)
  pooled_table_templet_temp[1,"I2"]<-paste0(as.character(round(100*meta_result$I2,digits = 1)),"%")
  pooled_table_templet_temp[1,"pval_Q"]<-round(meta_result$pval.Q,digits = 4)
  pooled_table<-rbind(pooled_table,pooled_table_templet_temp)
} else {
  pooled_table_templet_temp[1,"Patient_numbers"]<-0
  pooled_table_templet_temp[1,"HR_Random_model"]<-NA
  pooled_table_templet_temp[1,"CI_lower_Random_model"]<-NA
  pooled_table_templet_temp[1,"CI_upper_Random_model"]<-NA
  pooled_table_templet_temp[1,"pval_Random_model"]<-NA
  pooled_table_templet_temp[1,"HR_Common_model"]<-NA
  pooled_table_templet_temp[1,"CI_lower_Common_model"]<-NA
  pooled_table_templet_temp[1,"CI_upper_Common_model"]<-NA
  pooled_table_templet_temp[1,"pval_Common_model"]<-NA
  pooled_table_templet_temp[1,"tau"]<-NA
  pooled_table_templet_temp[1,"I2"]<-NA
  pooled_table_templet_temp[1,"pval_Q"]<-NA
  pooled_table<-rbind(pooled_table,pooled_table_templet_temp)
}

#### 02_06_05_01 CAR-NK; OS; dichotomous CD8 ####
meta_table<-sum_table[(c(1:25)),]
# 02_06_05 CAR-NK
meta_table<-subset(meta_table,Immunotherapy=="CAR-NK")
# 02_06_05_01 CAR-NK; OS
meta_table<-subset(meta_table,Outcome=="OS")
# 02_06_05_01 CAR-NK; OS; dichotomous CD8
meta_table<-subset(meta_table,PatientNumInCD8PositiveGroup!="Cox continue CD8")
# exclude results without convergence
# no exclusion
# meta-analysis
# no study left
# meta_result<-metagen(logHR, selogHR, sm = "HR",studlab = ResearchName,data=meta_table)
# # forest plot
# pdf(paste0(output_dir,"//ForestPlot_Subgroup_Immunotherapy_CAR-NK_OS_dichotomousCD8.pdf"),width=10, height=4)
# p_forest<-forest(meta_result,print.Q =T,digits=4,digits.pval = 3,digits.pval.Q = 3,
#                  test.overall.common = TRUE,
#                  test.overall.random = TRUE)
# print(p_forest)
# dev.off()
# # funnel plot
# pdf(paste0(output_dir,"//FunnelPlot_Subgroup_Immunotherapy_CAR-NK_OS_dichotomousCD8.pdf"),width=6, height=6)
# p_funnel<-funnel(meta_result)
# print(p_funnel)
# dev.off()

# add results to pooled table
pooled_table_templet_temp<-pooled_table_templet
pooled_table_templet_temp[1,"Subgroup"]<-"CAR-NK"
pooled_table_templet_temp[1,"Outcome"]<-"OS"
pooled_table_templet_temp[1,"Study_number"]<-nrow(meta_table)
if(nrow(meta_table)>=1){
  pooled_table_templet_temp[1,"Patient_numbers"]<-sum(as.numeric(meta_table$PatientNumInCD8PositiveGroup),as.numeric(meta_table$PatientNumInCD8NegativeGroup))
  pooled_table_templet_temp[1,"HR_Random_model"]<-round(exp(meta_result$TE.random),digits = 4)
  pooled_table_templet_temp[1,"CI_lower_Random_model"]<-round(exp(meta_result$lower.random),digits = 4)
  pooled_table_templet_temp[1,"CI_upper_Random_model"]<-round(exp(meta_result$upper.random),digits = 4)
  pooled_table_templet_temp[1,"pval_Random_model"]<-round(meta_result$pval.random,digits = 4)
  pooled_table_templet_temp[1,"HR_Common_model"]<-round(exp(meta_result$TE.common),digits = 4)
  pooled_table_templet_temp[1,"CI_lower_Common_model"]<-round(exp(meta_result$lower.common),digits = 4)
  pooled_table_templet_temp[1,"CI_upper_Common_model"]<-round(exp(meta_result$upper.common),digits = 4)
  pooled_table_templet_temp[1,"pval_Common_model"]<-round(meta_result$pval.common,digits = 4)
  pooled_table_templet_temp[1,"tau"]<-round(meta_result$tau,digits = 4)
  pooled_table_templet_temp[1,"I2"]<-paste0(as.character(round(100*meta_result$I2,digits = 1)),"%")
  pooled_table_templet_temp[1,"pval_Q"]<-round(meta_result$pval.Q,digits = 4)
  pooled_table<-rbind(pooled_table,pooled_table_templet_temp)
} else {
  pooled_table_templet_temp[1,"Patient_numbers"]<-0
  pooled_table_templet_temp[1,"HR_Random_model"]<-NA
  pooled_table_templet_temp[1,"CI_lower_Random_model"]<-NA
  pooled_table_templet_temp[1,"CI_upper_Random_model"]<-NA
  pooled_table_templet_temp[1,"pval_Random_model"]<-NA
  pooled_table_templet_temp[1,"HR_Common_model"]<-NA
  pooled_table_templet_temp[1,"CI_lower_Common_model"]<-NA
  pooled_table_templet_temp[1,"CI_upper_Common_model"]<-NA
  pooled_table_templet_temp[1,"pval_Common_model"]<-NA
  pooled_table_templet_temp[1,"tau"]<-NA
  pooled_table_templet_temp[1,"I2"]<-NA
  pooled_table_templet_temp[1,"pval_Q"]<-NA
  pooled_table<-rbind(pooled_table,pooled_table_templet_temp)
}
#### 02_06_05_02 CAR-NK; PFS; dichotomous CD8 ####
meta_table<-sum_table[(c(1:25)),]
# 02_06_05 CAR-NK
meta_table<-subset(meta_table,Immunotherapy=="CAR-NK")
# 02_06_05_02 CAR-NK; PFS
meta_table<-subset(meta_table,Outcome=="PFS")
# 02_06_05_02 CAR-NK; PFS; dichotomous CD8
meta_table<-subset(meta_table,PatientNumInCD8PositiveGroup!="Cox continue CD8")
# exclude results without convergence
# no exclusion
# meta-analysis
# no study left
# meta_result<-metagen(logHR, selogHR, sm = "HR",studlab = ResearchName,data=meta_table)
# # forest plot
# pdf(paste0(output_dir,"//ForestPlot_Subgroup_Immunotherapy_CAR-NK_PFS_dichotomousCD8.pdf"),width=10, height=4)
# p_forest<-forest(meta_result,print.Q =T,digits=4,digits.pval = 3,digits.pval.Q = 3,
#                  test.overall.common = TRUE,
#                  test.overall.random = TRUE)
# print(p_forest)
# dev.off()
# # funnel plot
# pdf(paste0(output_dir,"//FunnelPlot_Subgroup_Immunotherapy_CAR-NK_PFS_dichotomousCD8.pdf"),width=6, height=6)
# p_funnel<-funnel(meta_result)
# print(p_funnel)
# dev.off()

# add results to pooled table
pooled_table_templet_temp<-pooled_table_templet
pooled_table_templet_temp[1,"Subgroup"]<-"CAR-NK"
pooled_table_templet_temp[1,"Outcome"]<-"PFS"
pooled_table_templet_temp[1,"Study_number"]<-nrow(meta_table)
if(nrow(meta_table)>=1){
  pooled_table_templet_temp[1,"Patient_numbers"]<-sum(as.numeric(meta_table$PatientNumInCD8PositiveGroup),as.numeric(meta_table$PatientNumInCD8NegativeGroup))
  pooled_table_templet_temp[1,"HR_Random_model"]<-round(exp(meta_result$TE.random),digits = 4)
  pooled_table_templet_temp[1,"CI_lower_Random_model"]<-round(exp(meta_result$lower.random),digits = 4)
  pooled_table_templet_temp[1,"CI_upper_Random_model"]<-round(exp(meta_result$upper.random),digits = 4)
  pooled_table_templet_temp[1,"pval_Random_model"]<-round(meta_result$pval.random,digits = 4)
  pooled_table_templet_temp[1,"HR_Common_model"]<-round(exp(meta_result$TE.common),digits = 4)
  pooled_table_templet_temp[1,"CI_lower_Common_model"]<-round(exp(meta_result$lower.common),digits = 4)
  pooled_table_templet_temp[1,"CI_upper_Common_model"]<-round(exp(meta_result$upper.common),digits = 4)
  pooled_table_templet_temp[1,"pval_Common_model"]<-round(meta_result$pval.common,digits = 4)
  pooled_table_templet_temp[1,"tau"]<-round(meta_result$tau,digits = 4)
  pooled_table_templet_temp[1,"I2"]<-paste0(as.character(round(100*meta_result$I2,digits = 1)),"%")
  pooled_table_templet_temp[1,"pval_Q"]<-round(meta_result$pval.Q,digits = 4)
  pooled_table<-rbind(pooled_table,pooled_table_templet_temp)
} else {
  pooled_table_templet_temp[1,"Patient_numbers"]<-0
  pooled_table_templet_temp[1,"HR_Random_model"]<-NA
  pooled_table_templet_temp[1,"CI_lower_Random_model"]<-NA
  pooled_table_templet_temp[1,"CI_upper_Random_model"]<-NA
  pooled_table_templet_temp[1,"pval_Random_model"]<-NA
  pooled_table_templet_temp[1,"HR_Common_model"]<-NA
  pooled_table_templet_temp[1,"CI_lower_Common_model"]<-NA
  pooled_table_templet_temp[1,"CI_upper_Common_model"]<-NA
  pooled_table_templet_temp[1,"pval_Common_model"]<-NA
  pooled_table_templet_temp[1,"tau"]<-NA
  pooled_table_templet_temp[1,"I2"]<-NA
  pooled_table_templet_temp[1,"pval_Q"]<-NA
  pooled_table<-rbind(pooled_table,pooled_table_templet_temp)
}

#### 02_06_05_03 CAR-NK; Survival after immunotherapy; dichotomous CD8 ####
meta_table<-sum_table[(c(1:25)),]
# 02_06_05 CAR-NK
meta_table<-subset(meta_table,Immunotherapy=="CAR-NK")
# 02_06_05_03 CAR-NK; Survival after immunotherapy
meta_table<-subset(meta_table,Outcome=="Survival after immunotherapy")
# 02_06_05_03 CAR-NK; Survival after immunotherapy; dichotomous CD8
meta_table<-subset(meta_table,PatientNumInCD8PositiveGroup!="Cox continue CD8")
# exclude results without convergence
meta_table<-meta_table[(meta_table$PMID!="PMID37148198"),]
# meta-analysis
# no study left
# meta_result<-metagen(logHR, selogHR, sm = "HR",studlab = ResearchName,data=meta_table)
# 
# # forest plot
# pdf(paste0(output_dir,"//ForestPlot_Subgroup_Immunotherapy_CAR-NK_SurvivalAfterImmunotherapy_dichotomousCD8.pdf"),width=10, height=4)
# p_forest<-forest(meta_result,print.Q =T,digits=4,digits.pval = 3,digits.pval.Q = 3,
#                  test.overall.common = TRUE,
#                  test.overall.random = TRUE)
# print(p_forest)
# dev.off()
# # funnel plot
# pdf(paste0(output_dir,"//FunnelPlot_Subgroup_Immunotherapy_CAR-NK_SurvivalAfterImmunotherapy_dichotomousCD8.pdf"),width=6, height=6)
# p_funnel<-funnel(meta_result)
# print(p_funnel)
# dev.off()

# add results to pooled table
pooled_table_templet_temp<-pooled_table_templet
pooled_table_templet_temp[1,"Subgroup"]<-"CAR-NK"
pooled_table_templet_temp[1,"Outcome"]<-"Survival after immunotherapy"
pooled_table_templet_temp[1,"Study_number"]<-nrow(meta_table)
if(nrow(meta_table)>=1){
  pooled_table_templet_temp[1,"Patient_numbers"]<-sum(as.numeric(meta_table$PatientNumInCD8PositiveGroup),as.numeric(meta_table$PatientNumInCD8NegativeGroup))
  pooled_table_templet_temp[1,"HR_Random_model"]<-round(exp(meta_result$TE.random),digits = 4)
  pooled_table_templet_temp[1,"CI_lower_Random_model"]<-round(exp(meta_result$lower.random),digits = 4)
  pooled_table_templet_temp[1,"CI_upper_Random_model"]<-round(exp(meta_result$upper.random),digits = 4)
  pooled_table_templet_temp[1,"pval_Random_model"]<-round(meta_result$pval.random,digits = 4)
  pooled_table_templet_temp[1,"HR_Common_model"]<-round(exp(meta_result$TE.common),digits = 4)
  pooled_table_templet_temp[1,"CI_lower_Common_model"]<-round(exp(meta_result$lower.common),digits = 4)
  pooled_table_templet_temp[1,"CI_upper_Common_model"]<-round(exp(meta_result$upper.common),digits = 4)
  pooled_table_templet_temp[1,"pval_Common_model"]<-round(meta_result$pval.common,digits = 4)
  pooled_table_templet_temp[1,"tau"]<-round(meta_result$tau,digits = 4)
  pooled_table_templet_temp[1,"I2"]<-paste0(as.character(round(100*meta_result$I2,digits = 1)),"%")
  pooled_table_templet_temp[1,"pval_Q"]<-round(meta_result$pval.Q,digits = 4)
  pooled_table<-rbind(pooled_table,pooled_table_templet_temp)
} else {
  pooled_table_templet_temp[1,"Patient_numbers"]<-0
  pooled_table_templet_temp[1,"HR_Random_model"]<-NA
  pooled_table_templet_temp[1,"CI_lower_Random_model"]<-NA
  pooled_table_templet_temp[1,"CI_upper_Random_model"]<-NA
  pooled_table_templet_temp[1,"pval_Random_model"]<-NA
  pooled_table_templet_temp[1,"HR_Common_model"]<-NA
  pooled_table_templet_temp[1,"CI_lower_Common_model"]<-NA
  pooled_table_templet_temp[1,"CI_upper_Common_model"]<-NA
  pooled_table_templet_temp[1,"pval_Common_model"]<-NA
  pooled_table_templet_temp[1,"tau"]<-NA
  pooled_table_templet_temp[1,"I2"]<-NA
  pooled_table_templet_temp[1,"pval_Q"]<-NA
  pooled_table<-rbind(pooled_table,pooled_table_templet_temp)
}


#### 02_06 Subgroup_Immunotherapy end ####

output_dir<-output_dir_root
write.table(pooled_table,file = paste0(output_dir, "\\", "pooled_table.txt"), sep = "\t")
saveRDS(pooled_table,file = paste0(output_dir,"//pooled_table.rds"))

