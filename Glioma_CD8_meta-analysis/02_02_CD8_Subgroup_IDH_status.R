
library(survival)
library(survminer)
library(rms)
library(stringr)
####00 input data####
output_dir_root <- choose.dir(default = "D:\\output_dir", caption = "choose the output folder")
output_dir<-output_dir_root
# dir.create(paste0(output_dir_root,"//02_Subgroup"))
dir.create(paste0(output_dir_root,"//02_Subgroup//02_IDH status"))


data_dir <- choose.dir(default = "D:\\data_dir", caption = "choose the folder storing input_data.txt file")

input_dataset.rds_path <- choose.files(default = data_dir, caption = "choose the input_dataset.rds file",
                                       multi = TRUE, filters = Filters,
                                       index = nrow(Filters))
input_dataset<- readRDS(file = input_dataset.rds_path)
sum_table.rds_path <- choose.files(default = data_dir, caption = "choose the sum_table.rds file",
                                   multi = TRUE, filters = Filters,
                                   index = nrow(Filters))
sum_table<-readRDS(file = sum_table.rds_path)

sum_table_templet<- data.frame(matrix(ncol = 18, nrow = 0))
colnames(sum_table_templet) <- c('PMID', 'ResearchName', 'Age',"Gender","PR_Type","IDH_status","MGMT_status","Immunotherapy",
                                 "Complete_resection","Outcome","PatientNumInCD8PositiveGroup","PatientNumInCD8NegativeGroup",
                                 "logHR","selogHR","HR","HR_CI_down","HR_CI_up","P_value")


output_dir<-paste0(output_dir_root,"//02_Subgroup//02_IDH status//MUT")
dir.create(output_dir)
input_dataset<- readRDS(file = input_dataset.rds_path)
input_dataset<-subset(input_dataset,IDH1_status=="MUT")
####01_06 PMID33130898 start####
output_dir<-paste0(output_dir_root,"//02_Subgroup//02_IDH status//MUT//PMID33130898")
dir.create(output_dir)
#data preprocess
temp_dataset_id<-"PMID33130898"
temp_dataset<-input_dataset[input_dataset$PMID==temp_dataset_id,]
temp_dataset$CD8_value
temp_dataset$CD8_value<-as.numeric(temp_dataset$CD8_value)
#density plot
pdf(paste0(output_dir,"//",temp_dataset_id,"_density_CD8_value.pdf"),width=6, height=6)
p_density<-plot(density(temp_dataset$CD8_value,na.rm = T))
print(p_density)
dev.off()
#histogram
pdf(paste0(output_dir,"//",temp_dataset_id,"_hist_CD8_value.pdf"),width=6, height=6)
p_hist<-ggplot(temp_dataset, aes(x=CD8_value)) + 
  geom_histogram(color="black", fill="white",bins =100)
print(p_hist)
dev.off()
#add CD8_group info
# threshold<-100
# for (index in 1:nrow(input_dataset)) {
#   if(input_dataset$PMID[index] == temp_dataset_id){
#     if(!is.na(input_dataset[index,"CD8_value"])){
#       if(as.numeric(input_dataset[index,"CD8_value"])<threshold){
#         input_dataset[index,"CD8_group"]<-"Negative"
#       }else{
#         input_dataset[index,"CD8_group"]<-"Positive"
#       }
#     }
#   }
# }
# temp_dataset<-input_dataset[input_dataset$PMID==temp_dataset_id,]
#do prognostic ananlysis
#OS KM curve
fit <- survfit(Surv(OS_months,OS_Status) ~ CD8_group,  
               data = temp_dataset)
print(fit)
summary(fit)
pdf(paste0(output_dir,"//",temp_dataset_id,"_OS_survplot_CD8_group.pdf"),width=6, height=6)
os_plot<-ggsurvplot(fit, data = temp_dataset,
                    conf.int = TRUE,
                    risk.table = TRUE,
                    pval = TRUE)
print(os_plot)
dev.off()

#Cox dichotomous CD8
f_coxph<-coxph(Surv(OS_months,OS_Status)~CD8_group,data=temp_dataset)
# summary(f_coxph)

sum_table_templet_temp<-sum_table_templet
sum_table_templet_temp[1,"PMID"]<-temp_dataset_id
sum_table_templet_temp[1,"ResearchName"]<-"Dejaegher_2021"
sum_table_templet_temp[1,"Age"]<-"Adult"
sum_table_templet_temp[1,"Gender"]<-"Mixed"
sum_table_templet_temp[1,"PR_Type"]<-"Primary"
sum_table_templet_temp[1,"IDH_status"]<-"MUT"
sum_table_templet_temp[1,"MGMT_status"]<-"Mixed"
sum_table_templet_temp[1,"Immunotherapy"]<-"DC Vaccination"
sum_table_templet_temp[1,"Complete_resection"]<-"Mixed"
sum_table_templet_temp[1,"Outcome"]<-"OS"
sum_table_templet_temp[1,"PatientNumInCD8PositiveGroup"]<-sum(str_count(temp_dataset$CD8_group, pattern = "Positive"),na.rm = T)
sum_table_templet_temp[1,"PatientNumInCD8NegativeGroup"]<-sum(str_count(temp_dataset$CD8_group, pattern = "Negative"),na.rm = T)
sum_table_templet_temp[1,"logHR"]<-NA #coef
sum_table_templet_temp[1,"selogHR"]<-NA #se(coef)
sum_table_templet_temp[1,"HR"]<-NA #exp(coef)
sum_table_templet_temp[1,"HR_CI_down"]<-NA #exp(coef) lower .95
sum_table_templet_temp[1,"HR_CI_up"]<-NA #exp(coef) upper .95
sum_table_templet_temp[1,"P_value"]<-NA #Pr(>|z|)
sum_table<-rbind(sum_table,sum_table_templet_temp)

#Cox continue CD8
temp_dataset$CD8_value<-as.numeric(temp_dataset$CD8_value)
f_coxph<-coxph(Surv(OS_months,OS_Status)~CD8_value,data=temp_dataset)
summary(f_coxph)

sum_table_templet_temp<-sum_table_templet
sum_table_templet_temp[1,"PMID"]<-temp_dataset_id
sum_table_templet_temp[1,"ResearchName"]<-"Dejaegher_2021"
sum_table_templet_temp[1,"Age"]<-"Adult"
sum_table_templet_temp[1,"Gender"]<-"Mixed"
sum_table_templet_temp[1,"PR_Type"]<-"Primary"
sum_table_templet_temp[1,"IDH_status"]<-"MUT"
sum_table_templet_temp[1,"MGMT_status"]<-"Mixed"
sum_table_templet_temp[1,"Immunotherapy"]<-"DC Vaccination"
sum_table_templet_temp[1,"Complete_resection"]<-"Mixed"
sum_table_templet_temp[1,"Outcome"]<-"OS"
sum_table_templet_temp[1,"PatientNumInCD8PositiveGroup"]<-"Cox continue CD8"
sum_table_templet_temp[1,"PatientNumInCD8NegativeGroup"]<-sum(!is.na(temp_dataset$CD8_group))
sum_table_templet_temp[1,"logHR"]<-summary(f_coxph)$coefficients[1] #coef
sum_table_templet_temp[1,"selogHR"]<-summary(f_coxph)$coefficients[3] #se(coef)
sum_table_templet_temp[1,"HR"]<-summary(f_coxph)$coefficients[2] #exp(coef)
sum_table_templet_temp[1,"HR_CI_down"]<-summary(f_coxph)$conf.int[3] #exp(coef) lower .95
sum_table_templet_temp[1,"HR_CI_up"]<-summary(f_coxph)$conf.int[4] #exp(coef) upper .95
sum_table_templet_temp[1,"P_value"]<-summary(f_coxph)$coefficients[5] #Pr(>|z|)
sum_table<-rbind(sum_table,sum_table_templet_temp)

#PFS KM curve
#no PFS data
# fit <- survfit(Surv(PFS_months,PFS_Status) ~ CD8_group,  
#                data = temp_dataset)
# print(fit)
# summary(fit)
# pdf(paste0(output_dir,"//",temp_dataset_id,"_PFS_survplot_CD8_group.pdf"),width=6, height=6)
# pfs_plot<-ggsurvplot(fit, data = temp_dataset,
#                      conf.int = TRUE,
#                      risk.table = TRUE,
#                      pval = TRUE)
# print(pfs_plot)
# dev.off()
# 
# f_coxph<-coxph(Surv(PFS_months,PFS_Status)~CD8_group,data=temp_dataset)
# summary(f_coxph)
####01_06 PMID33130898 end####

output_dir<-paste0(output_dir_root,"//02_Subgroup//02_IDH status//WT")
dir.create(output_dir)
input_dataset<- readRDS(file = input_dataset.rds_path)
input_dataset<-subset(input_dataset,IDH1_status=="WT")

####01_04 PMID29910795 start####
output_dir<-paste0(output_dir_root,"//02_Subgroup//02_IDH status//WT//PMID29910795")
dir.create(output_dir)
#data preprocess
temp_dataset_id<-"PMID29910795"
temp_dataset<-input_dataset[input_dataset$PMID==temp_dataset_id,]
temp_dataset$CD8_value
temp_dataset$CD8_value<-as.numeric(temp_dataset$CD8_value)
#density plot
pdf(paste0(output_dir,"//",temp_dataset_id,"_density_CD8_value.pdf"),width=6, height=6)
p_density<-plot(density(temp_dataset$CD8_value,na.rm = T))
print(p_density)
dev.off()
#histogram
pdf(paste0(output_dir,"//",temp_dataset_id,"_hist_CD8_value.pdf"),width=6, height=6)
p_hist<-ggplot(temp_dataset, aes(x=CD8_value)) + 
  geom_histogram(color="black", fill="white",bins =100)
print(p_hist)
dev.off()
#add CD8_group info
# threshold<-500
# for (index in 1:nrow(input_dataset)) {
#   if(input_dataset$PMID[index] == temp_dataset_id){
#     if(!is.na(input_dataset[index,"CD8_value"])){
#       if(as.numeric(input_dataset[index,"CD8_value"])<threshold){
#         input_dataset[index,"CD8_group"]<-"Negative"
#       }else{
#         input_dataset[index,"CD8_group"]<-"Positive"
#       }
#     }
#   }
# }
# temp_dataset<-input_dataset[input_dataset$PMID==temp_dataset_id,]
#do prognostic ananlysis
#OS KM curve
fit <- survfit(Surv(OS_months,OS_Status) ~ CD8_group,  
               data = temp_dataset)
print(fit)
summary(fit)
pdf(paste0(output_dir,"//",temp_dataset_id,"_OS_survplot_CD8_group.pdf"),width=6, height=6)
os_plot<-ggsurvplot(fit, data = temp_dataset,
                    conf.int = TRUE,
                    risk.table = TRUE,
                    pval = TRUE)
print(os_plot)
dev.off()

#Cox dichotomous CD8
f_coxph<-coxph(Surv(OS_months,OS_Status)~CD8_group,data=temp_dataset)
summary(f_coxph)

sum_table_templet_temp<-sum_table_templet
sum_table_templet_temp[1,"PMID"]<-temp_dataset_id
sum_table_templet_temp[1,"ResearchName"]<-"Jan_2018"
sum_table_templet_temp[1,"Age"]<-"Adult"
sum_table_templet_temp[1,"Gender"]<-"Mixed"
sum_table_templet_temp[1,"PR_Type"]<-"Primary"
sum_table_templet_temp[1,"IDH_status"]<-"WT"
sum_table_templet_temp[1,"MGMT_status"]<-"Mixed"
sum_table_templet_temp[1,"Immunotherapy"]<-"DC Vaccination"
sum_table_templet_temp[1,"Complete_resection"]<-"Mixed"
sum_table_templet_temp[1,"Outcome"]<-"OS"
sum_table_templet_temp[1,"PatientNumInCD8PositiveGroup"]<-sum(str_count(temp_dataset$CD8_group, pattern = "Positive"),na.rm = T)
sum_table_templet_temp[1,"PatientNumInCD8NegativeGroup"]<-sum(str_count(temp_dataset$CD8_group, pattern = "Negative"),na.rm = T)
sum_table_templet_temp[1,"logHR"]<-summary(f_coxph)$coefficients[1] #coef
sum_table_templet_temp[1,"selogHR"]<-summary(f_coxph)$coefficients[3] #se(coef)
sum_table_templet_temp[1,"HR"]<-summary(f_coxph)$coefficients[2] #exp(coef)
sum_table_templet_temp[1,"HR_CI_down"]<-summary(f_coxph)$conf.int[3] #exp(coef) lower .95
sum_table_templet_temp[1,"HR_CI_up"]<-summary(f_coxph)$conf.int[4] #exp(coef) upper .95
sum_table_templet_temp[1,"P_value"]<-summary(f_coxph)$coefficients[5] #Pr(>|z|)
sum_table<-rbind(sum_table,sum_table_templet_temp)

#Cox continue CD8
temp_dataset$CD8_value<-as.numeric(temp_dataset$CD8_value)
f_coxph<-coxph(Surv(OS_months,OS_Status)~CD8_value,data=temp_dataset)
summary(f_coxph)

sum_table_templet_temp<-sum_table_templet
sum_table_templet_temp[1,"PMID"]<-temp_dataset_id
sum_table_templet_temp[1,"ResearchName"]<-"Jan_2018"
sum_table_templet_temp[1,"Age"]<-"Adult"
sum_table_templet_temp[1,"Gender"]<-"Mixed"
sum_table_templet_temp[1,"PR_Type"]<-"Primary"
sum_table_templet_temp[1,"IDH_status"]<-"WT"
sum_table_templet_temp[1,"MGMT_status"]<-"Mixed"
sum_table_templet_temp[1,"Immunotherapy"]<-"DC Vaccination"
sum_table_templet_temp[1,"Complete_resection"]<-"Mixed"
sum_table_templet_temp[1,"Outcome"]<-"OS"
sum_table_templet_temp[1,"PatientNumInCD8PositiveGroup"]<-"Cox continue CD8"
sum_table_templet_temp[1,"PatientNumInCD8NegativeGroup"]<-sum(!is.na(temp_dataset$CD8_group))
sum_table_templet_temp[1,"logHR"]<-summary(f_coxph)$coefficients[1] #coef
sum_table_templet_temp[1,"selogHR"]<-summary(f_coxph)$coefficients[3] #se(coef)
sum_table_templet_temp[1,"HR"]<-summary(f_coxph)$coefficients[2] #exp(coef)
sum_table_templet_temp[1,"HR_CI_down"]<-summary(f_coxph)$conf.int[3] #exp(coef) lower .95
sum_table_templet_temp[1,"HR_CI_up"]<-summary(f_coxph)$conf.int[4] #exp(coef) upper .95
sum_table_templet_temp[1,"P_value"]<-summary(f_coxph)$coefficients[5] #Pr(>|z|)
sum_table<-rbind(sum_table,sum_table_templet_temp)

#PFS KM curve
fit <- survfit(Surv(PFS_months,PFS_Status) ~ CD8_group,  
               data = temp_dataset)
print(fit)
summary(fit)
pdf(paste0(output_dir,"//",temp_dataset_id,"_PFS_survplot_CD8_group.pdf"),width=6, height=6)
pfs_plot<-ggsurvplot(fit, data = temp_dataset,
                     conf.int = TRUE,
                     risk.table = TRUE,
                     pval = TRUE)
print(pfs_plot)
dev.off()

#Cox dichotomous CD8
f_coxph<-coxph(Surv(PFS_months,PFS_Status)~CD8_group,data=temp_dataset)
summary(f_coxph)

sum_table_templet_temp<-sum_table_templet
sum_table_templet_temp[1,"PMID"]<-temp_dataset_id
sum_table_templet_temp[1,"ResearchName"]<-"Jan_2018"
sum_table_templet_temp[1,"Age"]<-"Adult"
sum_table_templet_temp[1,"Gender"]<-"Mixed"
sum_table_templet_temp[1,"PR_Type"]<-"Primary"
sum_table_templet_temp[1,"IDH_status"]<-"WT"
sum_table_templet_temp[1,"MGMT_status"]<-"Mixed"
sum_table_templet_temp[1,"Immunotherapy"]<-"DC Vaccination"
sum_table_templet_temp[1,"Complete_resection"]<-"Mixed"
sum_table_templet_temp[1,"Outcome"]<-"PFS"
sum_table_templet_temp[1,"PatientNumInCD8PositiveGroup"]<-sum(str_count(temp_dataset$CD8_group, pattern = "Positive"),na.rm = T)
sum_table_templet_temp[1,"PatientNumInCD8NegativeGroup"]<-sum(str_count(temp_dataset$CD8_group, pattern = "Negative"),na.rm = T)
sum_table_templet_temp[1,"logHR"]<-summary(f_coxph)$coefficients[1] #coef
sum_table_templet_temp[1,"selogHR"]<-summary(f_coxph)$coefficients[3] #se(coef)
sum_table_templet_temp[1,"HR"]<-summary(f_coxph)$coefficients[2] #exp(coef)
sum_table_templet_temp[1,"HR_CI_down"]<-summary(f_coxph)$conf.int[3] #exp(coef) lower .95
sum_table_templet_temp[1,"HR_CI_up"]<-summary(f_coxph)$conf.int[4] #exp(coef) upper .95
sum_table_templet_temp[1,"P_value"]<-summary(f_coxph)$coefficients[5] #Pr(>|z|)
sum_table<-rbind(sum_table,sum_table_templet_temp)

#Cox continue CD8
temp_dataset$CD8_value<-as.numeric(temp_dataset$CD8_value)
f_coxph<-coxph(Surv(PFS_months,PFS_Status)~CD8_value,data=temp_dataset)
summary(f_coxph)

sum_table_templet_temp<-sum_table_templet
sum_table_templet_temp[1,"PMID"]<-temp_dataset_id
sum_table_templet_temp[1,"ResearchName"]<-"Jan_2018"
sum_table_templet_temp[1,"Age"]<-"Adult"
sum_table_templet_temp[1,"Gender"]<-"Mixed"
sum_table_templet_temp[1,"PR_Type"]<-"Primary"
sum_table_templet_temp[1,"IDH_status"]<-"WT"
sum_table_templet_temp[1,"MGMT_status"]<-"Mixed"
sum_table_templet_temp[1,"Immunotherapy"]<-"DC Vaccination"
sum_table_templet_temp[1,"Complete_resection"]<-"Mixed"
sum_table_templet_temp[1,"Outcome"]<-"PFS"
sum_table_templet_temp[1,"PatientNumInCD8PositiveGroup"]<-"Cox continue CD8"
sum_table_templet_temp[1,"PatientNumInCD8NegativeGroup"]<-sum(!is.na(temp_dataset$CD8_group))
sum_table_templet_temp[1,"logHR"]<-summary(f_coxph)$coefficients[1] #coef
sum_table_templet_temp[1,"selogHR"]<-summary(f_coxph)$coefficients[3] #se(coef)
sum_table_templet_temp[1,"HR"]<-summary(f_coxph)$coefficients[2] #exp(coef)
sum_table_templet_temp[1,"HR_CI_down"]<-summary(f_coxph)$conf.int[3] #exp(coef) lower .95
sum_table_templet_temp[1,"HR_CI_up"]<-summary(f_coxph)$conf.int[4] #exp(coef) upper .95
sum_table_templet_temp[1,"P_value"]<-summary(f_coxph)$coefficients[5] #Pr(>|z|)
sum_table<-rbind(sum_table,sum_table_templet_temp)

####01_04 PMID29910795 end####
####01_06 PMID33130898 start####
output_dir<-paste0(output_dir_root,"//02_Subgroup//02_IDH status//WT//PMID33130898")
dir.create(output_dir)
#data preprocess
temp_dataset_id<-"PMID33130898"
temp_dataset<-input_dataset[input_dataset$PMID==temp_dataset_id,]
temp_dataset$CD8_value
temp_dataset$CD8_value<-as.numeric(temp_dataset$CD8_value)
#density plot
pdf(paste0(output_dir,"//",temp_dataset_id,"_density_CD8_value.pdf"),width=6, height=6)
p_density<-plot(density(temp_dataset$CD8_value,na.rm = T))
print(p_density)
dev.off()
#histogram
pdf(paste0(output_dir,"//",temp_dataset_id,"_hist_CD8_value.pdf"),width=6, height=6)
p_hist<-ggplot(temp_dataset, aes(x=CD8_value)) + 
  geom_histogram(color="black", fill="white",bins =100)
print(p_hist)
dev.off()
#add CD8_group info
# threshold<-100
# for (index in 1:nrow(input_dataset)) {
#   if(input_dataset$PMID[index] == temp_dataset_id){
#     if(!is.na(input_dataset[index,"CD8_value"])){
#       if(as.numeric(input_dataset[index,"CD8_value"])<threshold){
#         input_dataset[index,"CD8_group"]<-"Negative"
#       }else{
#         input_dataset[index,"CD8_group"]<-"Positive"
#       }
#     }
#   }
# }
# temp_dataset<-input_dataset[input_dataset$PMID==temp_dataset_id,]
#do prognostic ananlysis
#OS KM curve
fit <- survfit(Surv(OS_months,OS_Status) ~ CD8_group,  
               data = temp_dataset)
print(fit)
summary(fit)
pdf(paste0(output_dir,"//",temp_dataset_id,"_OS_survplot_CD8_group.pdf"),width=6, height=6)
os_plot<-ggsurvplot(fit, data = temp_dataset,
                    conf.int = TRUE,
                    risk.table = TRUE,
                    pval = TRUE)
print(os_plot)
dev.off()

#Cox dichotomous CD8
f_coxph<-coxph(Surv(OS_months,OS_Status)~CD8_group,data=temp_dataset)
summary(f_coxph)

sum_table_templet_temp<-sum_table_templet
sum_table_templet_temp[1,"PMID"]<-temp_dataset_id
sum_table_templet_temp[1,"ResearchName"]<-"Dejaegher_2021"
sum_table_templet_temp[1,"Age"]<-"Adult"
sum_table_templet_temp[1,"Gender"]<-"Mixed"
sum_table_templet_temp[1,"PR_Type"]<-"Primary"
sum_table_templet_temp[1,"IDH_status"]<-"WT"
sum_table_templet_temp[1,"MGMT_status"]<-"Mixed"
sum_table_templet_temp[1,"Immunotherapy"]<-"DC Vaccination"
sum_table_templet_temp[1,"Complete_resection"]<-"Mixed"
sum_table_templet_temp[1,"Outcome"]<-"OS"
sum_table_templet_temp[1,"PatientNumInCD8PositiveGroup"]<-sum(str_count(temp_dataset$CD8_group, pattern = "Positive"),na.rm = T)
sum_table_templet_temp[1,"PatientNumInCD8NegativeGroup"]<-sum(str_count(temp_dataset$CD8_group, pattern = "Negative"),na.rm = T)
sum_table_templet_temp[1,"logHR"]<-summary(f_coxph)$coefficients[1] #coef
sum_table_templet_temp[1,"selogHR"]<-summary(f_coxph)$coefficients[3] #se(coef)
sum_table_templet_temp[1,"HR"]<-summary(f_coxph)$coefficients[2] #exp(coef)
sum_table_templet_temp[1,"HR_CI_down"]<-summary(f_coxph)$conf.int[3] #exp(coef) lower .95
sum_table_templet_temp[1,"HR_CI_up"]<-summary(f_coxph)$conf.int[4] #exp(coef) upper .95
sum_table_templet_temp[1,"P_value"]<-summary(f_coxph)$coefficients[5] #Pr(>|z|)
sum_table<-rbind(sum_table,sum_table_templet_temp)

#Cox continue CD8
temp_dataset$CD8_value<-as.numeric(temp_dataset$CD8_value)
f_coxph<-coxph(Surv(OS_months,OS_Status)~CD8_value,data=temp_dataset)
summary(f_coxph)

sum_table_templet_temp<-sum_table_templet
sum_table_templet_temp[1,"PMID"]<-temp_dataset_id
sum_table_templet_temp[1,"ResearchName"]<-"Dejaegher_2021"
sum_table_templet_temp[1,"Age"]<-"Adult"
sum_table_templet_temp[1,"Gender"]<-"Mixed"
sum_table_templet_temp[1,"PR_Type"]<-"Primary"
sum_table_templet_temp[1,"IDH_status"]<-"WT"
sum_table_templet_temp[1,"MGMT_status"]<-"Mixed"
sum_table_templet_temp[1,"Immunotherapy"]<-"DC Vaccination"
sum_table_templet_temp[1,"Complete_resection"]<-"Mixed"
sum_table_templet_temp[1,"Outcome"]<-"OS"
sum_table_templet_temp[1,"PatientNumInCD8PositiveGroup"]<-"Cox continue CD8"
sum_table_templet_temp[1,"PatientNumInCD8NegativeGroup"]<-sum(!is.na(temp_dataset$CD8_group))
sum_table_templet_temp[1,"logHR"]<-summary(f_coxph)$coefficients[1] #coef
sum_table_templet_temp[1,"selogHR"]<-summary(f_coxph)$coefficients[3] #se(coef)
sum_table_templet_temp[1,"HR"]<-summary(f_coxph)$coefficients[2] #exp(coef)
sum_table_templet_temp[1,"HR_CI_down"]<-summary(f_coxph)$conf.int[3] #exp(coef) lower .95
sum_table_templet_temp[1,"HR_CI_up"]<-summary(f_coxph)$conf.int[4] #exp(coef) upper .95
sum_table_templet_temp[1,"P_value"]<-summary(f_coxph)$coefficients[5] #Pr(>|z|)
sum_table<-rbind(sum_table,sum_table_templet_temp)


#PFS KM curve
#no PFS data
# fit <- survfit(Surv(PFS_months,PFS_Status) ~ CD8_group,  
#                data = temp_dataset)
# print(fit)
# summary(fit)
# pdf(paste0(output_dir,"//",temp_dataset_id,"_PFS_survplot_CD8_group.pdf"),width=6, height=6)
# pfs_plot<-ggsurvplot(fit, data = temp_dataset,
#                      conf.int = TRUE,
#                      risk.table = TRUE,
#                      pval = TRUE)
# print(pfs_plot)
# dev.off()
# 
# f_coxph<-coxph(Surv(PFS_months,PFS_Status)~CD8_group,data=temp_dataset)
# summary(f_coxph)
####01_06 PMID33130898 end####

output_dir<-output_dir_root
saveRDS(sum_table,file = paste0(output_dir,"//sum_table.rds"))
