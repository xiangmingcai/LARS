
library(survival)
library(survminer)
library(rms)
library(stringr)
####00 input data####
output_dir_root <- choose.dir(default = "D:\\output_dir", caption = "choose the output folder")
output_dir<-output_dir_root
dir.create(paste0(output_dir_root,"//01_All_population"))

data_dir <- choose.dir(default = "D:\\data_dir", caption = "choose the folder storing input_data.txt file")

input_dataset_path <- choose.files(default = data_dir, caption = "choose the input_data.txt file",
                                      multi = TRUE, filters = Filters,
                                      index = nrow(Filters))
input_dataset<- read.csv(input_dataset_path, header = TRUE,sep="\t", stringsAsFactors=FALSE)
for (index in 1:nrow(input_dataset)) {
  if(!is.na(input_dataset$CD8_group[index])){
    if(input_dataset$CD8_group[index]=="tbd"){
      input_dataset$CD8_group[index]<-NA
    }
  }
}
####01 data process####
names(table(input_dataset$PMID))
sum_table_templet<- data.frame(matrix(ncol = 18, nrow = 0))
colnames(sum_table_templet) <- c('PMID', 'ResearchName', 'Age',"Gender","PR_Type","IDH_status","MGMT_status","Immunotherapy",
                         "Complete_resection","Outcome","PatientNumInCD8PositiveGroup","PatientNumInCD8NegativeGroup",
                         "logHR","selogHR","HR","HR_CI_down","HR_CI_up","P_value")
sum_table<-sum_table_templet

####01_01 PMID26968205 start####
###fail to deduce OS_Status and PFS_Status
#
{
# PMID26968205_dataset<-input_dataset[input_dataset$PMID=="PMID26968205",]
# PMID26968205_dataset$CD8_value<-as.numeric(PMID26968205_dataset$CD8_value)
# PMID26968205_lower_subset <- subset(PMID26968205_dataset, CD8_value <= quantile(PMID26968205_dataset$CD8_value, 0.75))
# PMID26968205_dataset$CD8_group<-"high"
# PMID26968205_dataset$CD8_group[PMID26968205_dataset$Patient_ID %in% PMID26968205_lower_subset$Patient_ID]<-"low"
# rm(PMID26968205_lower_subset)
# PMID26968205_dataset$OS_Status=1
# PMID26968205_dataset$PFS_Status=1
# 
# fit <- survfit(Surv(OS_months,OS_Status) ~ CD8_group,  
#                data = PMID26968205_dataset)
# print(fit)
# summary(fit)
# ggsurvplot(fit, data = PMID26968205_dataset)
# dev.off()
# colnames(PMID26968205_dataset)
# ddist <- datadist(PMID26968205_dataset[,c("Patient_ID","CD8_group","OS_months","OS_Status","PFS_months","PFS_Status")])
# options(datadist='ddist')
# for (index in 1:nrow(PMID26968205_dataset)) {
#   temp_PMID26968205_dataset<-PMID26968205_dataset
#   temp_PMID26968205_dataset$PFS_Status[index]<-0
#   for (index_2 in 1:nrow(PMID26968205_dataset)) {
#     temp_PMID26968205_dataset$PFS_Status[index_2]<-0
#     for (index_3 in 1:nrow(PMID26968205_dataset)) {
#       temp_PMID26968205_dataset$PFS_Status[index_3]<-0
#       for (index_4 in 1:nrow(PMID26968205_dataset)){
#         temp_PMID26968205_dataset$PFS_Status[index_4]<-0
#         f_cph <- try(cph(Surv(OS_months,OS_Status) ~ CD8_group,
#                          x=T, y=T, surv=T,
#                          data=temp_PMID26968205_dataset),silent=T)
#         if(!('try-error' %in% class(f_cph)))
#         {
#           a <- try((abs((f_cph$stats[[5]]-0.046))<0.001),silent=T)
#           if(!('try-error' %in% class(a)))
#           {
#             if(length(a)>0){
#               if(abs((f_cph$stats[[5]]-0.046))<0.001){
#                 print(index)
#                 print(index_2)
#                 print(index_3)
#                 print(index_4)
#                 print(f_cph$stats[[5]])
#               }
#             }
#           }
#         }
#       }
#     }
#   }
# }
}
#
# input_dataset<-input_dataset[(!(input_dataset$PMID %in% "PMID26968205")),]
# OS
HR_CI_down<-0.078
HR_CI_up<-0.978
HR<-0.277
selogHR<-(log(HR_CI_up)-log(HR_CI_down))/(2*1.96)
logHR<-log(HR)
sum_table_templet_temp<-sum_table_templet
sum_table_templet_temp[1,"PMID"]<-"PMID26968205"
sum_table_templet_temp[1,"ResearchName"]<-"Hsu_2016"
sum_table_templet_temp[1,"Age"]<-"Mixed"
sum_table_templet_temp[1,"Gender"]<-"Mixed"
sum_table_templet_temp[1,"PR_Type"]<-"Mixed"
sum_table_templet_temp[1,"IDH_status"]<-"WT"
sum_table_templet_temp[1,"MGMT_status"]<-"Mixed"
sum_table_templet_temp[1,"Immunotherapy"]<-"DC Vaccination"
sum_table_templet_temp[1,"Complete_resection"]<-"Mixed"
sum_table_templet_temp[1,"Outcome"]<-"OS"
sum_table_templet_temp[1,"PatientNumInCD8PositiveGroup"]<-4 #From KM curve
sum_table_templet_temp[1,"PatientNumInCD8NegativeGroup"]<-11 #From KM curve
sum_table_templet_temp[1,"logHR"]<-logHR
sum_table_templet_temp[1,"selogHR"]<-selogHR
sum_table_templet_temp[1,"HR"]<-HR
sum_table_templet_temp[1,"HR_CI_down"]<-HR_CI_down
sum_table_templet_temp[1,"HR_CI_up"]<-HR_CI_up
sum_table_templet_temp[1,"P_value"]<-0.0461
sum_table<-rbind(sum_table,sum_table_templet_temp)
# PFS
HR_CI_down<-0.066
HR_CI_up<-0.965
HR<-0.252
selogHR<-(log(HR_CI_up)-log(HR_CI_down))/(2*1.96)
logHR<-log(HR)
sum_table_templet_temp<-sum_table_templet
sum_table_templet_temp[1,"PMID"]<-"PMID26968205"
sum_table_templet_temp[1,"ResearchName"]<-"Hsu_2016"
sum_table_templet_temp[1,"Age"]<-"Mixed"
sum_table_templet_temp[1,"Gender"]<-"Mixed"
sum_table_templet_temp[1,"PR_Type"]<-"Mixed"
sum_table_templet_temp[1,"IDH_status"]<-"WT"
sum_table_templet_temp[1,"MGMT_status"]<-"Mixed"
sum_table_templet_temp[1,"Immunotherapy"]<-"DC Vaccination"
sum_table_templet_temp[1,"Complete_resection"]<-"Mixed"
sum_table_templet_temp[1,"Outcome"]<-"PFS"
sum_table_templet_temp[1,"PatientNumInCD8PositiveGroup"]<-4 #From KM curve
sum_table_templet_temp[1,"PatientNumInCD8NegativeGroup"]<-11 #From KM curve
sum_table_templet_temp[1,"logHR"]<-logHR
sum_table_templet_temp[1,"selogHR"]<-selogHR
sum_table_templet_temp[1,"HR"]<-HR
sum_table_templet_temp[1,"HR_CI_down"]<-HR_CI_down
sum_table_templet_temp[1,"HR_CI_up"]<-HR_CI_up
sum_table_templet_temp[1,"P_value"]<-0.0442
sum_table<-rbind(sum_table,sum_table_templet_temp)

####01_01 PMID26968205 end####
####01_02 PMID15452186 start####
output_dir<-paste0(output_dir_root,"//01_All_population//PMID15452186")
dir.create(output_dir)
#data preprocess
temp_dataset_id<-"PMID15452186"
temp_dataset<-input_dataset[input_dataset$PMID==temp_dataset_id,]
temp_dataset$CD8_value
temp_dataset$CD8_value<-as.numeric(temp_dataset$CD8_value)
#density plot
pdf(paste0(output_dir,"//",temp_dataset_id,"_density_CD8_value.pdf"),width=6, height=6)
p_density<-plot(density(temp_dataset$CD8_value))
print(p_density)
dev.off()
#histogram
pdf(paste0(output_dir,"//",temp_dataset_id,"_hist_CD8_value.pdf"),width=6, height=6)
p_hist<-ggplot(temp_dataset, aes(x=CD8_value)) + 
  geom_histogram(color="black", fill="white",bins =50)
print(p_hist)
dev.off()
#add CD8_group info
threshold<-0.5
for (index in 1:nrow(input_dataset)) {
  if(input_dataset$PMID[index] == temp_dataset_id){
    if(!is.na(input_dataset[index,"CD8_value"])){
      if(as.numeric(input_dataset[index,"CD8_value"])<threshold){
        input_dataset[index,"CD8_group"]<-"Negative"
      }else{
        input_dataset[index,"CD8_group"]<-"Positive"
      }
    }
  }
}
temp_dataset<-input_dataset[input_dataset$PMID==temp_dataset_id,]
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
sum_table_templet_temp[1,"ResearchName"]<-"Steiner_2004"
sum_table_templet_temp[1,"Age"]<-"Adult"
sum_table_templet_temp[1,"Gender"]<-"Mixed"
sum_table_templet_temp[1,"PR_Type"]<-"Primary"
sum_table_templet_temp[1,"IDH_status"]<-"Mixed"
sum_table_templet_temp[1,"MGMT_status"]<-"Mixed"
sum_table_templet_temp[1,"Immunotherapy"]<-"Peptide Vaccination"
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
sum_table_templet_temp[1,"ResearchName"]<-"Steiner_2004"
sum_table_templet_temp[1,"Age"]<-"Adult"
sum_table_templet_temp[1,"Gender"]<-"Mixed"
sum_table_templet_temp[1,"PR_Type"]<-"Primary"
sum_table_templet_temp[1,"IDH_status"]<-"Mixed"
sum_table_templet_temp[1,"MGMT_status"]<-"Mixed"
sum_table_templet_temp[1,"Immunotherapy"]<-"Peptide Vaccination"
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
sum_table_templet_temp[1,"ResearchName"]<-"Steiner_2004"
sum_table_templet_temp[1,"Age"]<-"Adult"
sum_table_templet_temp[1,"Gender"]<-"Mixed"
sum_table_templet_temp[1,"PR_Type"]<-"Primary"
sum_table_templet_temp[1,"IDH_status"]<-"Mixed"
sum_table_templet_temp[1,"MGMT_status"]<-"Mixed"
sum_table_templet_temp[1,"Immunotherapy"]<-"Peptide Vaccination"
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
sum_table_templet_temp[1,"ResearchName"]<-"Steiner_2004"
sum_table_templet_temp[1,"Age"]<-"Adult"
sum_table_templet_temp[1,"Gender"]<-"Mixed"
sum_table_templet_temp[1,"PR_Type"]<-"Primary"
sum_table_templet_temp[1,"IDH_status"]<-"Mixed"
sum_table_templet_temp[1,"MGMT_status"]<-"Mixed"
sum_table_templet_temp[1,"Immunotherapy"]<-"Peptide Vaccination"
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

####01_02 PMID15452186 end####
####01_03 PMID18957964 start####
output_dir<-paste0(output_dir_root,"//01_All_population//PMID18957964")
dir.create(output_dir)
#data preprocess
temp_dataset_id<-"PMID18957964"
temp_dataset<-input_dataset[input_dataset$PMID==temp_dataset_id,]
temp_dataset$CD8_value
temp_dataset$Patient_ID
temp_dataset$CD8_value<-c(10,18,10,10,10,10)
#density plot
pdf(paste0(output_dir,"//",temp_dataset_id,"_density_CD8_value.pdf"),width=6, height=6)
p_density<-plot(density(temp_dataset$CD8_value))
print(p_density)
dev.off()
#histogram
pdf(paste0(output_dir,"//",temp_dataset_id,"_hist_CD8_value.pdf"),width=6, height=6)
p_hist<-ggplot(temp_dataset, aes(x=CD8_value)) + 
  geom_histogram(color="black", fill="white",bins =50)
print(p_hist)
dev.off()
#add CD8_group info
threshold<-14
temp_dataset$Patient_ID[temp_dataset$CD8_value<threshold]
for (index in 1:nrow(input_dataset)) {
  if(input_dataset$PMID[index] == temp_dataset_id){
    if(input_dataset$Patient_ID[index] %in% c(temp_dataset$Patient_ID[temp_dataset$CD8_value<threshold])){
      input_dataset[index,"CD8_group"]<-"Negative"
    }else if(input_dataset$Patient_ID[index] %in% c(temp_dataset$Patient_ID[temp_dataset$CD8_value >= threshold])){
      input_dataset[index,"CD8_group"]<-"Positive"
    }
  }
}
temp_dataset<-input_dataset[input_dataset$PMID==temp_dataset_id,]
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
sum_table_templet_temp[1,"ResearchName"]<-"Markert_2009"
sum_table_templet_temp[1,"Age"]<-"Adult"
sum_table_templet_temp[1,"Gender"]<-"Mixed"
sum_table_templet_temp[1,"PR_Type"]<-"Recurrence"
sum_table_templet_temp[1,"IDH_status"]<-"WT"
sum_table_templet_temp[1,"MGMT_status"]<-"Mixed"
sum_table_templet_temp[1,"Immunotherapy"]<-"Oncolytic viral vectors therapy"
sum_table_templet_temp[1,"Complete_resection"]<-"Mixed"
sum_table_templet_temp[1,"Outcome"]<-"Survival after immunotherapy"
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
temp_dataset$CD8_value<-c(10,18,10,10,10,10)
f_coxph<-coxph(Surv(OS_months,OS_Status)~CD8_value,data=temp_dataset)
summary(f_coxph)

sum_table_templet_temp<-sum_table_templet
sum_table_templet_temp[1,"PMID"]<-temp_dataset_id
sum_table_templet_temp[1,"ResearchName"]<-"Markert_2009"
sum_table_templet_temp[1,"Age"]<-"Adult"
sum_table_templet_temp[1,"Gender"]<-"Mixed"
sum_table_templet_temp[1,"PR_Type"]<-"Recurrence"
sum_table_templet_temp[1,"IDH_status"]<-"WT"
sum_table_templet_temp[1,"MGMT_status"]<-"Mixed"
sum_table_templet_temp[1,"Immunotherapy"]<-"Oncolytic viral vectors therapy"
sum_table_templet_temp[1,"Complete_resection"]<-"Mixed"
sum_table_templet_temp[1,"Outcome"]<-"Survival after immunotherapy"
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

####01_03 PMID18957964 end####
####01_04 PMID29910795 start####
output_dir<-paste0(output_dir_root,"//01_All_population//PMID29910795")
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
threshold<-500
for (index in 1:nrow(input_dataset)) {
  if(input_dataset$PMID[index] == temp_dataset_id){
    if(!is.na(input_dataset[index,"CD8_value"])){
      if(as.numeric(input_dataset[index,"CD8_value"])<threshold){
        input_dataset[index,"CD8_group"]<-"Negative"
      }else{
        input_dataset[index,"CD8_group"]<-"Positive"
      }
    }
  }
}
temp_dataset<-input_dataset[input_dataset$PMID==temp_dataset_id,]
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
sum_table_templet_temp[1,"IDH_status"]<-"Mixed"
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
sum_table_templet_temp[1,"IDH_status"]<-"Mixed"
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
sum_table_templet_temp[1,"IDH_status"]<-"Mixed"
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
sum_table_templet_temp[1,"IDH_status"]<-"Mixed"
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
####01_05 PMID30568305 start####
output_dir<-paste0(output_dir_root,"//01_All_population//PMID30568305")
dir.create(output_dir)
#data preprocess
temp_dataset_id<-"PMID30568305"
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
  geom_histogram(color="black", fill="white",bins =50)
print(p_hist)
dev.off()
#add CD8_group info
threshold<-100
for (index in 1:nrow(input_dataset)) {
  if(input_dataset$PMID[index] == temp_dataset_id){
    if(!is.na(input_dataset[index,"CD8_value"])){
      if(as.numeric(input_dataset[index,"CD8_value"])<threshold){
        input_dataset[index,"CD8_group"]<-"Negative"
      }else{
        input_dataset[index,"CD8_group"]<-"Positive"
      }
    }
  }
}
temp_dataset<-input_dataset[input_dataset$PMID==temp_dataset_id,]
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
sum_table_templet_temp[1,"ResearchName"]<-"Keskin_2019"
sum_table_templet_temp[1,"Age"]<-"Adult"
sum_table_templet_temp[1,"Gender"]<-"Mixed"
sum_table_templet_temp[1,"PR_Type"]<-"Primary"
sum_table_templet_temp[1,"IDH_status"]<-"WT"
sum_table_templet_temp[1,"MGMT_status"]<-"unmethylated"
sum_table_templet_temp[1,"Immunotherapy"]<-"Peptide Vaccination"
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
sum_table_templet_temp[1,"ResearchName"]<-"Keskin_2019"
sum_table_templet_temp[1,"Age"]<-"Adult"
sum_table_templet_temp[1,"Gender"]<-"Mixed"
sum_table_templet_temp[1,"PR_Type"]<-"Primary"
sum_table_templet_temp[1,"IDH_status"]<-"WT"
sum_table_templet_temp[1,"MGMT_status"]<-"unmethylated"
sum_table_templet_temp[1,"Immunotherapy"]<-"Peptide Vaccination"
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
sum_table_templet_temp[1,"ResearchName"]<-"Keskin_2019"
sum_table_templet_temp[1,"Age"]<-"Adult"
sum_table_templet_temp[1,"Gender"]<-"Mixed"
sum_table_templet_temp[1,"PR_Type"]<-"Primary"
sum_table_templet_temp[1,"IDH_status"]<-"WT"
sum_table_templet_temp[1,"MGMT_status"]<-"unmethylated"
sum_table_templet_temp[1,"Immunotherapy"]<-"Peptide Vaccination"
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
sum_table_templet_temp[1,"ResearchName"]<-"Keskin_2019"
sum_table_templet_temp[1,"Age"]<-"Adult"
sum_table_templet_temp[1,"Gender"]<-"Mixed"
sum_table_templet_temp[1,"PR_Type"]<-"Primary"
sum_table_templet_temp[1,"IDH_status"]<-"WT"
sum_table_templet_temp[1,"MGMT_status"]<-"unmethylated"
sum_table_templet_temp[1,"Immunotherapy"]<-"Peptide Vaccination"
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

####01_05 PMID30568305 end####
####01_06 PMID33130898 start####
output_dir<-paste0(output_dir_root,"//01_All_population//PMID33130898")
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
threshold<-100
for (index in 1:nrow(input_dataset)) {
  if(input_dataset$PMID[index] == temp_dataset_id){
    if(!is.na(input_dataset[index,"CD8_value"])){
      if(as.numeric(input_dataset[index,"CD8_value"])<threshold){
        input_dataset[index,"CD8_group"]<-"Negative"
      }else{
        input_dataset[index,"CD8_group"]<-"Positive"
      }
    }
  }
}
temp_dataset<-input_dataset[input_dataset$PMID==temp_dataset_id,]
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
sum_table_templet_temp[1,"IDH_status"]<-"Mixed"
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
sum_table_templet_temp[1,"IDH_status"]<-"Mixed"
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
####01_07 PMID33838625 start####
output_dir<-paste0(output_dir_root,"//01_All_population//PMID33838625")
dir.create(output_dir)
#data preprocess
temp_dataset_id<-"PMID33838625"
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
  geom_histogram(color="black", fill="white",bins =50)
print(p_hist)
dev.off()
#add CD8_group info
threshold<-3
for (index in 1:nrow(input_dataset)) {
  if(input_dataset$PMID[index] == temp_dataset_id){
    if(!is.na(input_dataset[index,"CD8_value"])){
      if(as.numeric(input_dataset[index,"CD8_value"])<threshold){
        input_dataset[index,"CD8_group"]<-"Negative"
      }else{
        input_dataset[index,"CD8_group"]<-"Positive"
      }
    }
  }
}
temp_dataset<-input_dataset[input_dataset$PMID==temp_dataset_id,]
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
sum_table_templet_temp[1,"ResearchName"]<-"Friedman_2021"
sum_table_templet_temp[1,"Age"]<-"Children"
sum_table_templet_temp[1,"Gender"]<-"Mixed"
sum_table_templet_temp[1,"PR_Type"]<-"Recurrence"
sum_table_templet_temp[1,"IDH_status"]<-"WT"
sum_table_templet_temp[1,"MGMT_status"]<-"Mixed"
sum_table_templet_temp[1,"Immunotherapy"]<-"Oncolytic viral vectors therapy"
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
sum_table_templet_temp[1,"ResearchName"]<-"Friedman_2021"
sum_table_templet_temp[1,"Age"]<-"Children"
sum_table_templet_temp[1,"Gender"]<-"Mixed"
sum_table_templet_temp[1,"PR_Type"]<-"Recurrence"
sum_table_templet_temp[1,"IDH_status"]<-"WT"
sum_table_templet_temp[1,"MGMT_status"]<-"Mixed"
sum_table_templet_temp[1,"Immunotherapy"]<-"Oncolytic viral vectors therapy"
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
sum_table_templet_temp[1,"ResearchName"]<-"Friedman_2021"
sum_table_templet_temp[1,"Age"]<-"Children"
sum_table_templet_temp[1,"Gender"]<-"Mixed"
sum_table_templet_temp[1,"PR_Type"]<-"Recurrence"
sum_table_templet_temp[1,"IDH_status"]<-"WT"
sum_table_templet_temp[1,"MGMT_status"]<-"Mixed"
sum_table_templet_temp[1,"Immunotherapy"]<-"Oncolytic viral vectors therapy"
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
sum_table_templet_temp[1,"ResearchName"]<-"Friedman_2021"
sum_table_templet_temp[1,"Age"]<-"Children"
sum_table_templet_temp[1,"Gender"]<-"Mixed"
sum_table_templet_temp[1,"PR_Type"]<-"Recurrence"
sum_table_templet_temp[1,"IDH_status"]<-"WT"
sum_table_templet_temp[1,"MGMT_status"]<-"Mixed"
sum_table_templet_temp[1,"Immunotherapy"]<-"Oncolytic viral vectors therapy"
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


####01_07 PMID33838625 end####
####01_08 PMID35377001 start####
output_dir<-paste0(output_dir_root,"//01_All_population//PMID35377001")
dir.create(output_dir)
#data preprocess
temp_dataset_id<-"PMID35377001"
temp_dataset<-input_dataset[input_dataset$PMID==temp_dataset_id,]
temp_dataset$CD8_value
# temp_dataset$CD8_value<-as.numeric(temp_dataset$CD8_value)
#density plot
# pdf(paste0(output_dir,"//",temp_dataset_id,"_density_CD8_value.pdf"),width=6, height=6)
# p_density<-plot(density(temp_dataset$CD8_value,na.rm = T))
# print(p_density)
# dev.off()
# #histogram
# pdf(paste0(output_dir,"//",temp_dataset_id,"_hist_CD8_value.pdf"),width=6, height=6)
# p_hist<-ggplot(temp_dataset, aes(x=CD8_value)) + 
#   geom_histogram(color="black", fill="white",bins =50)
# print(p_hist)
# dev.off()
# #add CD8_group info
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
#no OS data
# fit <- survfit(Surv(OS_months,OS_Status) ~ CD8_group,  
#                data = temp_dataset)
# print(fit)
# summary(fit)
# pdf(paste0(output_dir,"//",temp_dataset_id,"_OS_survplot_CD8_group.pdf"),width=6, height=6)
# os_plot<-ggsurvplot(fit, data = temp_dataset,
#                     conf.int = TRUE,
#                     risk.table = TRUE,
#                     pval = TRUE)
# print(os_plot)
# dev.off()
# 
# f_coxph<-coxph(Surv(OS_months,OS_Status)~CD8_group,data=temp_dataset)
# summary(f_coxph)

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
sum_table_templet_temp[1,"ResearchName"]<-"Narita_2022"
sum_table_templet_temp[1,"Age"]<-"Adult"
sum_table_templet_temp[1,"Gender"]<-"Mixed"
sum_table_templet_temp[1,"PR_Type"]<-"Recurrence"
sum_table_templet_temp[1,"IDH_status"]<-"Mixed"
sum_table_templet_temp[1,"MGMT_status"]<-"Mixed"
sum_table_templet_temp[1,"Immunotherapy"]<-"Peptide Vaccination"
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

####01_08 PMID35377001 end####
####01_09 PMID36707424 start####
####01_09_01 PMID36707424 Primary start####
output_dir<-paste0(output_dir_root,"//01_All_population//PMID36707424_Primary")
dir.create(output_dir)
#data preprocess
temp_dataset_id<-"PMID36707424"
PR_Type<-"Primary"
temp_dataset<-input_dataset[c((input_dataset$PMID==temp_dataset_id)&(input_dataset$PR_Type==PR_Type)),]
temp_dataset$CD8_value
temp_dataset$Patient_ID
temp_dataset$CD8_value<-c(1,1,2.5,2,5,2,3)
#density plot
pdf(paste0(output_dir,"//",temp_dataset_id,"_",PR_Type,"_density_CD8_value.pdf"),width=6, height=6)
p_density<-plot(density(temp_dataset$CD8_value))
print(p_density)
dev.off()
#histogram
pdf(paste0(output_dir,"//",temp_dataset_id,"_",PR_Type,"_hist_CD8_value.pdf"),width=6, height=6)
p_hist<-ggplot(temp_dataset, aes(x=CD8_value)) + 
  geom_histogram(color="black", fill="white",bins =100)
print(p_hist)
dev.off()
#add CD8_group info
threshold<-4
# temp_dataset$Patient_ID[temp_dataset$CD8_value<threshold]
for (index in 1:nrow(input_dataset)) {
  if(input_dataset$PMID[index] == temp_dataset_id){
    if(input_dataset$PR_Type[index] == PR_Type){
      if(input_dataset$Patient_ID[index] %in% c(temp_dataset$Patient_ID[temp_dataset$CD8_value<threshold])){
        input_dataset[index,"CD8_group"]<-"Negative"
      }else if(input_dataset$Patient_ID[index] %in% c(temp_dataset$Patient_ID[temp_dataset$CD8_value >= threshold])){
        input_dataset[index,"CD8_group"]<-"Positive"
      }
    }
  }
}
temp_dataset<-input_dataset[c((input_dataset$PMID==temp_dataset_id)&(input_dataset$PR_Type==PR_Type)),]
#do prognostic ananlysis
#OS KM curve
fit <- survfit(Surv(OS_months,OS_Status) ~ CD8_group,  
               data = temp_dataset)
print(fit)
summary(fit)
pdf(paste0(output_dir,"//",temp_dataset_id,"_",PR_Type,"_OS_survplot_CD8_group.pdf"),width=6, height=6)
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
sum_table_templet_temp[1,"ResearchName"]<-"Friedman_2023"
sum_table_templet_temp[1,"Age"]<-"Adult"
sum_table_templet_temp[1,"Gender"]<-"Mixed"
sum_table_templet_temp[1,"PR_Type"]<-"Primary"
sum_table_templet_temp[1,"IDH_status"]<-"WT"
sum_table_templet_temp[1,"MGMT_status"]<-"Mixed"
sum_table_templet_temp[1,"Immunotherapy"]<-"Immune Checkpoint Inhibitors"
sum_table_templet_temp[1,"Complete_resection"]<-"Mixed"
sum_table_templet_temp[1,"Outcome"]<-"Survival after immunotherapy"
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
temp_dataset$CD8_value<-c(1,1,2.5,2,5,2,3)
f_coxph<-coxph(Surv(OS_months,OS_Status)~CD8_value,data=temp_dataset)
summary(f_coxph)

sum_table_templet_temp<-sum_table_templet
sum_table_templet_temp[1,"PMID"]<-temp_dataset_id
sum_table_templet_temp[1,"ResearchName"]<-"Friedman_2023"
sum_table_templet_temp[1,"Age"]<-"Adult"
sum_table_templet_temp[1,"Gender"]<-"Mixed"
sum_table_templet_temp[1,"PR_Type"]<-"Primary"
sum_table_templet_temp[1,"IDH_status"]<-"WT"
sum_table_templet_temp[1,"MGMT_status"]<-"Mixed"
sum_table_templet_temp[1,"Immunotherapy"]<-"Immune Checkpoint Inhibitors"
sum_table_templet_temp[1,"Complete_resection"]<-"Mixed"
sum_table_templet_temp[1,"Outcome"]<-"Survival after immunotherapy"
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
####01_09_01 PMID36707424 Primary end####
####01_09_02 PMID36707424 Recurrence start####
output_dir<-paste0(output_dir_root,"//01_All_population//PMID36707424_Recurrence")
dir.create(output_dir)
#data preprocess
temp_dataset_id<-"PMID36707424"
PR_Type<-"Recurrence"
temp_dataset<-input_dataset[c((input_dataset$PMID==temp_dataset_id)&(input_dataset$PR_Type==PR_Type)),]
temp_dataset$CD8_value
temp_dataset$Patient_ID
temp_dataset$CD8_value<-c(7.5,2.5,4,2.5,3)
#density plot
pdf(paste0(output_dir,"//",temp_dataset_id,"_",PR_Type,"_density_CD8_value.pdf"),width=6, height=6)
p_density<-plot(density(temp_dataset$CD8_value))
print(p_density)
dev.off()
#histogram
pdf(paste0(output_dir,"//",temp_dataset_id,"_",PR_Type,"_hist_CD8_value.pdf"),width=6, height=6)
p_hist<-ggplot(temp_dataset, aes(x=CD8_value)) + 
  geom_histogram(color="black", fill="white",bins =100)
print(p_hist)
dev.off()
#add CD8_group info
threshold<-5
# temp_dataset$Patient_ID[temp_dataset$CD8_value<threshold]
for (index in 1:nrow(input_dataset)) {
  if(input_dataset$PMID[index] == temp_dataset_id){
    if(input_dataset$PR_Type[index] == PR_Type){
      if(input_dataset$Patient_ID[index] %in% c(temp_dataset$Patient_ID[temp_dataset$CD8_value<threshold])){
        input_dataset[index,"CD8_group"]<-"Negative"
      }else if(input_dataset$Patient_ID[index] %in% c(temp_dataset$Patient_ID[temp_dataset$CD8_value >= threshold])){
        input_dataset[index,"CD8_group"]<-"Positive"
      }
    }
  }
}
temp_dataset<-input_dataset[c((input_dataset$PMID==temp_dataset_id)&(input_dataset$PR_Type==PR_Type)),]
#do prognostic ananlysis
#OS KM curve
fit <- survfit(Surv(OS_months,OS_Status) ~ CD8_group,  
               data = temp_dataset)
print(fit)
summary(fit)
pdf(paste0(output_dir,"//",temp_dataset_id,"_",PR_Type,"_OS_survplot_CD8_group.pdf"),width=6, height=6)
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
sum_table_templet_temp[1,"ResearchName"]<-"Friedman_2023"
sum_table_templet_temp[1,"Age"]<-"Adult"
sum_table_templet_temp[1,"Gender"]<-"Mixed"
sum_table_templet_temp[1,"PR_Type"]<-"Recurrence"
sum_table_templet_temp[1,"IDH_status"]<-"WT"
sum_table_templet_temp[1,"MGMT_status"]<-"Mixed"
sum_table_templet_temp[1,"Immunotherapy"]<-"Immune Checkpoint Inhibitors"
sum_table_templet_temp[1,"Complete_resection"]<-"Mixed"
sum_table_templet_temp[1,"Outcome"]<-"Survival after immunotherapy"
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
temp_dataset$CD8_value<-c(7.5,2.5,4,2.5,3)
f_coxph<-coxph(Surv(OS_months,OS_Status)~CD8_value,data=temp_dataset)
summary(f_coxph)

sum_table_templet_temp<-sum_table_templet
sum_table_templet_temp[1,"PMID"]<-temp_dataset_id
sum_table_templet_temp[1,"ResearchName"]<-"Friedman_2023"
sum_table_templet_temp[1,"Age"]<-"Adult"
sum_table_templet_temp[1,"Gender"]<-"Mixed"
sum_table_templet_temp[1,"PR_Type"]<-"Recurrence"
sum_table_templet_temp[1,"IDH_status"]<-"WT"
sum_table_templet_temp[1,"MGMT_status"]<-"Mixed"
sum_table_templet_temp[1,"Immunotherapy"]<-"Immune Checkpoint Inhibitors"
sum_table_templet_temp[1,"Complete_resection"]<-"Mixed"
sum_table_templet_temp[1,"Outcome"]<-"Survival after immunotherapy"
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
####01_09_02 PMID36707424 Recurrence end####
####01_09 PMID36707424 end####
####01_10 PMID37148198 start####
output_dir<-paste0(output_dir_root,"//01_All_population//PMID37148198")
dir.create(output_dir)
#data preprocess
temp_dataset_id<-"PMID37148198"
temp_dataset<-input_dataset[input_dataset$PMID==temp_dataset_id,]
temp_dataset$CD8_value
temp_dataset$CD8_value<-as.numeric(temp_dataset$CD8_value)
#density plot
pdf(paste0(output_dir,"//",temp_dataset_id,"_density_CD8_value.pdf"),width=6, height=6)
p_density<-plot(density(temp_dataset$CD8_value))
print(p_density)
dev.off()
#histogram
pdf(paste0(output_dir,"//",temp_dataset_id,"_hist_CD8_value.pdf"),width=6, height=6)
p_hist<-ggplot(temp_dataset, aes(x=CD8_value)) + 
  geom_histogram(color="black", fill="white",bins =50)
print(p_hist)
dev.off()
#add CD8_group info
threshold<-4
for (index in 1:nrow(input_dataset)) {
  if(input_dataset$PMID[index] == temp_dataset_id){
    if(!is.na(input_dataset[index,"CD8_value"])){
      if(as.numeric(input_dataset[index,"CD8_value"])<threshold){
        input_dataset[index,"CD8_group"]<-"Negative"
      }else{
        input_dataset[index,"CD8_group"]<-"Positive"
      }
    }
  }
}
temp_dataset<-input_dataset[input_dataset$PMID==temp_dataset_id,]
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
sum_table_templet_temp[1,"ResearchName"]<-"Michael_2023"
sum_table_templet_temp[1,"Age"]<-"Adult"
sum_table_templet_temp[1,"Gender"]<-"Mixed"
sum_table_templet_temp[1,"PR_Type"]<-"Recurrence"
sum_table_templet_temp[1,"IDH_status"]<-"WT"
sum_table_templet_temp[1,"MGMT_status"]<-"Mixed"
sum_table_templet_temp[1,"Immunotherapy"]<-"CAR-NK"
sum_table_templet_temp[1,"Complete_resection"]<-"Mixed"
sum_table_templet_temp[1,"Outcome"]<-"Survival after immunotherapy"
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
sum_table_templet_temp[1,"ResearchName"]<-"Michael_2023"
sum_table_templet_temp[1,"Age"]<-"Adult"
sum_table_templet_temp[1,"Gender"]<-"Mixed"
sum_table_templet_temp[1,"PR_Type"]<-"Recurrence"
sum_table_templet_temp[1,"IDH_status"]<-"WT"
sum_table_templet_temp[1,"MGMT_status"]<-"Mixed"
sum_table_templet_temp[1,"Immunotherapy"]<-"CAR-NK"
sum_table_templet_temp[1,"Complete_resection"]<-"Mixed"
sum_table_templet_temp[1,"Outcome"]<-"Survival after immunotherapy"
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
#since the PFS is after immunotherapy, we exclude this PFS analysis

####01_10 PMID37148198 end####

output_dir<-output_dir_root
saveRDS(input_dataset,file = paste0(output_dir,"//input_dataset.rds"))
saveRDS(sum_table,file = paste0(output_dir,"//sum_table.rds"))
