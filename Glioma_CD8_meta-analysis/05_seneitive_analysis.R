

####--------------------------------- all population ---------------------------------####
library(survival)
library(survminer)
library(rms)
library(stringr)
####00 input data####
output_dir_root <- choose.dir(default = "D:\\output_dir", caption = "choose the output folder")
output_dir<-output_dir_root
dir.create(paste0(output_dir_root,"//05_sensitive_analysis"))
dir.create(paste0(output_dir_root,"//05_sensitive_analysis//01_All_population"))

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
output_dir<-paste0(output_dir_root,"//05_sensitive_analysis//01_All_population//PMID15452186")
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
threshold<-median(temp_dataset$CD8_value)+0.00000001
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
output_dir<-paste0(output_dir_root,"//05_sensitive_analysis//01_All_population//PMID18957964")
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
output_dir<-paste0(output_dir_root,"//05_sensitive_analysis//01_All_population//PMID29910795")
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
threshold<-median(temp_dataset$CD8_value,na.rm = T)+0.00000001
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
output_dir<-paste0(output_dir_root,"//05_sensitive_analysis//01_All_population//PMID30568305")
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
threshold<-median(temp_dataset$CD8_value,na.rm = T)+0.00000001

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
output_dir<-paste0(output_dir_root,"//05_sensitive_analysis//01_All_population//PMID33130898")
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
threshold<-median(temp_dataset$CD8_value,na.rm = T)+0.00000001
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
output_dir<-paste0(output_dir_root,"//05_sensitive_analysis//01_All_population//PMID33838625")
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
threshold<-threshold<-median(temp_dataset$CD8_value,na.rm = T)+0.00000001
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
output_dir<-paste0(output_dir_root,"//05_sensitive_analysis//01_All_population//PMID35377001")
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
output_dir<-paste0(output_dir_root,"//05_sensitive_analysis//01_All_population//PMID36707424_Primary")
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
threshold<-threshold<-median(temp_dataset$CD8_value,na.rm = T)+0.00000001
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
output_dir<-paste0(output_dir_root,"//05_sensitive_analysis//01_All_population//PMID36707424_Recurrence")
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
threshold<-median(temp_dataset$CD8_value,na.rm = T)+0.00000001
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
output_dir<-paste0(output_dir_root,"//05_sensitive_analysis//01_All_population//PMID37148198")
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
threshold<-median(temp_dataset$CD8_value,na.rm = T)+0.00000001
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
saveRDS(input_dataset,file = paste0(output_dir,"//input_dataset_sensitive_analysis.rds"))
saveRDS(sum_table,file = paste0(output_dir,"//sum_table_sensitive_analysis.rds"))

####--------------------------------- 02_01_CD8_Subgroup_Gender ---------------------------------####

library(survival)
library(survminer)
library(rms)
library(stringr)

####00 input data####
output_dir_root <- choose.dir(default = "D:\\output_dir", caption = "choose the output folder")
output_dir<-output_dir_root
dir.create(paste0(output_dir_root,"//05_sensitive_analysis//02_Subgroup"))
dir.create(paste0(output_dir_root,"//05_sensitive_analysis//02_Subgroup//01_Gender"))

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

output_dir<-paste0(output_dir_root,"//05_sensitive_analysis//02_Subgroup//01_Gender//Male")
dir.create(output_dir)
input_dataset<- readRDS(file = input_dataset.rds_path)
input_dataset<-subset(input_dataset,Gender=="Male")
####01_02 PMID15452186 start####
output_dir<-paste0(output_dir_root,"//05_sensitive_analysis//02_Subgroup//01_Gender//Male//PMID15452186")
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
# #add CD8_group info
# threshold<-0.5
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
sum_table_templet_temp[1,"ResearchName"]<-"Steiner_2004"
sum_table_templet_temp[1,"Age"]<-"Adult"
sum_table_templet_temp[1,"Gender"]<-"Male"
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
sum_table_templet_temp[1,"Gender"]<-"Male"
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
sum_table_templet_temp[1,"Gender"]<-"Male"
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
sum_table_templet_temp[1,"Gender"]<-"Male"
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
output_dir<-paste0(output_dir_root,"//05_sensitive_analysis//02_Subgroup//01_Gender//Male//PMID18957964")
dir.create(output_dir)
#data preprocess
temp_dataset_id<-"PMID18957964"
temp_dataset<-input_dataset[input_dataset$PMID==temp_dataset_id,]
temp_dataset$CD8_value
temp_dataset$Patient_ID
temp_dataset$CD8_value<-c(10,10)
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
# threshold<-14
# temp_dataset$Patient_ID[temp_dataset$CD8_value<threshold]
# for (index in 1:nrow(input_dataset)) {
#   if(input_dataset$PMID[index] == temp_dataset_id){
#     if(input_dataset$Patient_ID[index] %in% c(temp_dataset$Patient_ID[temp_dataset$CD8_value<threshold])){
#       input_dataset[index,"CD8_group"]<-"Negative"
#     }else if(input_dataset$Patient_ID[index] %in% c(temp_dataset$Patient_ID[temp_dataset$CD8_value >= threshold])){
#       input_dataset[index,"CD8_group"]<-"Positive"
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
sum_table_templet_temp[1,"ResearchName"]<-"Markert_2009"
sum_table_templet_temp[1,"Age"]<-"Adult"
sum_table_templet_temp[1,"Gender"]<-"Male"
sum_table_templet_temp[1,"PR_Type"]<-"Recurrence"
sum_table_templet_temp[1,"IDH_status"]<-"WT"
sum_table_templet_temp[1,"MGMT_status"]<-"Mixed"
sum_table_templet_temp[1,"Immunotherapy"]<-"Oncolytic viral vectors therapy"
sum_table_templet_temp[1,"Complete_resection"]<-"Mixed"
sum_table_templet_temp[1,"Outcome"]<-"Survival after immunotherapy"
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
temp_dataset$CD8_value<-c(10,10)
f_coxph<-coxph(Surv(OS_months,OS_Status)~CD8_value,data=temp_dataset)
summary(f_coxph)

sum_table_templet_temp<-sum_table_templet
sum_table_templet_temp[1,"PMID"]<-temp_dataset_id
sum_table_templet_temp[1,"ResearchName"]<-"Markert_2009"
sum_table_templet_temp[1,"Age"]<-"Adult"
sum_table_templet_temp[1,"Gender"]<-"Male"
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
output_dir<-paste0(output_dir_root,"//05_sensitive_analysis//02_Subgroup//01_Gender//Male//PMID29910795")
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
sum_table_templet_temp[1,"Gender"]<-"Male"
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
sum_table_templet_temp[1,"Gender"]<-"Male"
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
sum_table_templet_temp[1,"Gender"]<-"Male"
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
sum_table_templet_temp[1,"Gender"]<-"Male"
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
####01_06 PMID33130898 start####
output_dir<-paste0(output_dir_root,"//05_sensitive_analysis//02_Subgroup//01_Gender//Male//PMID33130898")
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
sum_table_templet_temp[1,"Gender"]<-"Male"
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
sum_table_templet_temp[1,"Gender"]<-"Male"
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
output_dir<-paste0(output_dir_root,"//05_sensitive_analysis//02_Subgroup//01_Gender//Male//PMID33838625")
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
# threshold<-3
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
sum_table_templet_temp[1,"ResearchName"]<-"Friedman_2021"
sum_table_templet_temp[1,"Age"]<-"Children"
sum_table_templet_temp[1,"Gender"]<-"Male"
sum_table_templet_temp[1,"PR_Type"]<-"Recurrence"
sum_table_templet_temp[1,"IDH_status"]<-"WT"
sum_table_templet_temp[1,"MGMT_status"]<-"Mixed"
sum_table_templet_temp[1,"Immunotherapy"]<-"Oncolytic viral vectors therapy"
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
sum_table_templet_temp[1,"ResearchName"]<-"Friedman_2021"
sum_table_templet_temp[1,"Age"]<-"Children"
sum_table_templet_temp[1,"Gender"]<-"Male"
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
# summary(f_coxph)

sum_table_templet_temp<-sum_table_templet
sum_table_templet_temp[1,"PMID"]<-temp_dataset_id
sum_table_templet_temp[1,"ResearchName"]<-"Friedman_2021"
sum_table_templet_temp[1,"Age"]<-"Children"
sum_table_templet_temp[1,"Gender"]<-"Male"
sum_table_templet_temp[1,"PR_Type"]<-"Recurrence"
sum_table_templet_temp[1,"IDH_status"]<-"WT"
sum_table_templet_temp[1,"MGMT_status"]<-"Mixed"
sum_table_templet_temp[1,"Immunotherapy"]<-"Oncolytic viral vectors therapy"
sum_table_templet_temp[1,"Complete_resection"]<-"Mixed"
sum_table_templet_temp[1,"Outcome"]<-"PFS"
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
f_coxph<-coxph(Surv(PFS_months,PFS_Status)~CD8_value,data=temp_dataset)
summary(f_coxph)

sum_table_templet_temp<-sum_table_templet
sum_table_templet_temp[1,"PMID"]<-temp_dataset_id
sum_table_templet_temp[1,"ResearchName"]<-"Friedman_2021"
sum_table_templet_temp[1,"Age"]<-"Children"
sum_table_templet_temp[1,"Gender"]<-"Male"
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
####01_10 PMID37148198 start####
output_dir<-paste0(output_dir_root,"//05_sensitive_analysis//02_Subgroup//01_Gender//Male//PMID37148198")
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
# threshold<-4
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
sum_table_templet_temp[1,"ResearchName"]<-"Michael_2023"
sum_table_templet_temp[1,"Age"]<-"Adult"
sum_table_templet_temp[1,"Gender"]<-"Male"
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
sum_table_templet_temp[1,"Gender"]<-"Male"
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

output_dir<-paste0(output_dir_root,"//05_sensitive_analysis//02_Subgroup//01_Gender//Female")
dir.create(output_dir)
input_dataset<- readRDS(file = input_dataset.rds_path)
input_dataset<-subset(input_dataset,Gender=="Female")
####01_02 PMID15452186 start####
output_dir<-paste0(output_dir_root,"//05_sensitive_analysis//02_Subgroup//01_Gender//Female//PMID15452186")
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
# #add CD8_group info
# threshold<-0.5
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
sum_table_templet_temp[1,"ResearchName"]<-"Steiner_2004"
sum_table_templet_temp[1,"Age"]<-"Adult"
sum_table_templet_temp[1,"Gender"]<-"Female"
sum_table_templet_temp[1,"PR_Type"]<-"Primary"
sum_table_templet_temp[1,"IDH_status"]<-"Mixed"
sum_table_templet_temp[1,"MGMT_status"]<-"Mixed"
sum_table_templet_temp[1,"Immunotherapy"]<-"Peptide Vaccination"
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
sum_table_templet_temp[1,"ResearchName"]<-"Steiner_2004"
sum_table_templet_temp[1,"Age"]<-"Adult"
sum_table_templet_temp[1,"Gender"]<-"Female"
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
# summary(f_coxph)

sum_table_templet_temp<-sum_table_templet
sum_table_templet_temp[1,"PMID"]<-temp_dataset_id
sum_table_templet_temp[1,"ResearchName"]<-"Steiner_2004"
sum_table_templet_temp[1,"Age"]<-"Adult"
sum_table_templet_temp[1,"Gender"]<-"Female"
sum_table_templet_temp[1,"PR_Type"]<-"Primary"
sum_table_templet_temp[1,"IDH_status"]<-"Mixed"
sum_table_templet_temp[1,"MGMT_status"]<-"Mixed"
sum_table_templet_temp[1,"Immunotherapy"]<-"Peptide Vaccination"
sum_table_templet_temp[1,"Complete_resection"]<-"Mixed"
sum_table_templet_temp[1,"Outcome"]<-"PFS"
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
f_coxph<-coxph(Surv(PFS_months,PFS_Status)~CD8_value,data=temp_dataset)
summary(f_coxph)

sum_table_templet_temp<-sum_table_templet
sum_table_templet_temp[1,"PMID"]<-temp_dataset_id
sum_table_templet_temp[1,"ResearchName"]<-"Steiner_2004"
sum_table_templet_temp[1,"Age"]<-"Adult"
sum_table_templet_temp[1,"Gender"]<-"Female"
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
output_dir<-paste0(output_dir_root,"//05_sensitive_analysis//02_Subgroup//01_Gender//Female//PMID18957964")
dir.create(output_dir)
#data preprocess
temp_dataset_id<-"PMID18957964"
temp_dataset<-input_dataset[input_dataset$PMID==temp_dataset_id,]
temp_dataset$CD8_value
temp_dataset$Patient_ID
temp_dataset$CD8_value<-c(18,10,10,10)
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
# threshold<-14
# temp_dataset$Patient_ID[temp_dataset$CD8_value<threshold]
# for (index in 1:nrow(input_dataset)) {
#   if(input_dataset$PMID[index] == temp_dataset_id){
#     if(input_dataset$Patient_ID[index] %in% c(temp_dataset$Patient_ID[temp_dataset$CD8_value<threshold])){
#       input_dataset[index,"CD8_group"]<-"Negative"
#     }else if(input_dataset$Patient_ID[index] %in% c(temp_dataset$Patient_ID[temp_dataset$CD8_value >= threshold])){
#       input_dataset[index,"CD8_group"]<-"Positive"
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
sum_table_templet_temp[1,"ResearchName"]<-"Markert_2009"
sum_table_templet_temp[1,"Age"]<-"Adult"
sum_table_templet_temp[1,"Gender"]<-"Female"
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
temp_dataset$CD8_value<-c(18,10,10,10)
f_coxph<-coxph(Surv(OS_months,OS_Status)~CD8_value,data=temp_dataset)
summary(f_coxph)

sum_table_templet_temp<-sum_table_templet
sum_table_templet_temp[1,"PMID"]<-temp_dataset_id
sum_table_templet_temp[1,"ResearchName"]<-"Markert_2009"
sum_table_templet_temp[1,"Age"]<-"Adult"
sum_table_templet_temp[1,"Gender"]<-"Female"
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
output_dir<-paste0(output_dir_root,"//05_sensitive_analysis//02_Subgroup//01_Gender//Female//PMID29910795")
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
sum_table_templet_temp[1,"Gender"]<-"Female"
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
sum_table_templet_temp[1,"Gender"]<-"Female"
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
sum_table_templet_temp[1,"Gender"]<-"Female"
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
sum_table_templet_temp[1,"Gender"]<-"Female"
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
####01_06 PMID33130898 start####
output_dir<-paste0(output_dir_root,"//05_sensitive_analysis//02_Subgroup//01_Gender//Female//PMID33130898")
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
sum_table_templet_temp[1,"Gender"]<-"Female"
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
sum_table_templet_temp[1,"Gender"]<-"Female"
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
output_dir<-paste0(output_dir_root,"//05_sensitive_analysis//02_Subgroup//01_Gender//Female//PMID33838625")
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
# threshold<-3
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
sum_table_templet_temp[1,"ResearchName"]<-"Friedman_2021"
sum_table_templet_temp[1,"Age"]<-"Children"
sum_table_templet_temp[1,"Gender"]<-"Female"
sum_table_templet_temp[1,"PR_Type"]<-"Recurrence"
sum_table_templet_temp[1,"IDH_status"]<-"WT"
sum_table_templet_temp[1,"MGMT_status"]<-"Mixed"
sum_table_templet_temp[1,"Immunotherapy"]<-"Oncolytic viral vectors therapy"
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
sum_table_templet_temp[1,"ResearchName"]<-"Friedman_2021"
sum_table_templet_temp[1,"Age"]<-"Children"
sum_table_templet_temp[1,"Gender"]<-"Female"
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
# summary(f_coxph)

sum_table_templet_temp<-sum_table_templet
sum_table_templet_temp[1,"PMID"]<-temp_dataset_id
sum_table_templet_temp[1,"ResearchName"]<-"Friedman_2021"
sum_table_templet_temp[1,"Age"]<-"Children"
sum_table_templet_temp[1,"Gender"]<-"Female"
sum_table_templet_temp[1,"PR_Type"]<-"Recurrence"
sum_table_templet_temp[1,"IDH_status"]<-"WT"
sum_table_templet_temp[1,"MGMT_status"]<-"Mixed"
sum_table_templet_temp[1,"Immunotherapy"]<-"Oncolytic viral vectors therapy"
sum_table_templet_temp[1,"Complete_resection"]<-"Mixed"
sum_table_templet_temp[1,"Outcome"]<-"PFS"
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
f_coxph<-coxph(Surv(PFS_months,PFS_Status)~CD8_value,data=temp_dataset)
summary(f_coxph)

sum_table_templet_temp<-sum_table_templet
sum_table_templet_temp[1,"PMID"]<-temp_dataset_id
sum_table_templet_temp[1,"ResearchName"]<-"Friedman_2021"
sum_table_templet_temp[1,"Age"]<-"Children"
sum_table_templet_temp[1,"Gender"]<-"Female"
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
####01_10 PMID37148198 start####
output_dir<-paste0(output_dir_root,"//05_sensitive_analysis//02_Subgroup//01_Gender//Female//PMID37148198")
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
# threshold<-4
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
sum_table_templet_temp[1,"ResearchName"]<-"Michael_2023"
sum_table_templet_temp[1,"Age"]<-"Adult"
sum_table_templet_temp[1,"Gender"]<-"Female"
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
sum_table_templet_temp[1,"Gender"]<-"Female"
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
saveRDS(sum_table,file = paste0(output_dir,"//sum_table_sensitive_analysis.rds"))

####--------------------------------- 02_02_CD8_Subgroup_IDH_status ---------------------------------####

library(survival)
library(survminer)
library(rms)
library(stringr)
####00 input data####
output_dir_root <- choose.dir(default = "D:\\output_dir", caption = "choose the output folder")
output_dir<-output_dir_root
# dir.create(paste0(output_dir_root,"//05_sensitive_analysis//02_Subgroup"))
dir.create(paste0(output_dir_root,"//05_sensitive_analysis//02_Subgroup//02_IDH status"))

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

output_dir<-paste0(output_dir_root,"//05_sensitive_analysis//02_Subgroup//02_IDH status//MUT")
dir.create(output_dir)
input_dataset<- readRDS(file = input_dataset.rds_path)
input_dataset<-subset(input_dataset,IDH1_status=="MUT")
####01_06 PMID33130898 start####
output_dir<-paste0(output_dir_root,"//05_sensitive_analysis//02_Subgroup//02_IDH status//MUT//PMID33130898")
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

output_dir<-paste0(output_dir_root,"//05_sensitive_analysis//02_Subgroup//02_IDH status//WT")
dir.create(output_dir)
input_dataset<- readRDS(file = input_dataset.rds_path)
input_dataset<-subset(input_dataset,IDH1_status=="WT")

####01_04 PMID29910795 start####
output_dir<-paste0(output_dir_root,"//05_sensitive_analysis//02_Subgroup//02_IDH status//WT//PMID29910795")
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
output_dir<-paste0(output_dir_root,"//05_sensitive_analysis//02_Subgroup//02_IDH status//WT//PMID33130898")
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
saveRDS(sum_table,file = paste0(output_dir,"//sum_table_sensitive_analysis.rds"))

####--------------------------------- 02_03_CD8_Subgroup_MGMT_status ---------------------------------####

library(survival)
library(survminer)
library(rms)
library(stringr)
####00 input data####
output_dir_root <- choose.dir(default = "D:\\output_dir", caption = "choose the output folder")
output_dir<-output_dir_root
# dir.create(paste0(output_dir_root,"//05_sensitive_analysis//02_Subgroup"))
dir.create(paste0(output_dir_root,"//05_sensitive_analysis//02_Subgroup//03_MGMT status"))

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

output_dir<-paste0(output_dir_root,"//05_sensitive_analysis//02_Subgroup//03_MGMT status//methylated")
dir.create(output_dir)
input_dataset<- readRDS(file = input_dataset.rds_path)
input_dataset<-subset(input_dataset,MGMT_status=="methylated")

####01_04 PMID29910795 start####
output_dir<-paste0(output_dir_root,"//05_sensitive_analysis//02_Subgroup//03_MGMT status//methylated//PMID29910795")
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
sum_table_templet_temp[1,"IDH_status"]<-"Mixed"
sum_table_templet_temp[1,"MGMT_status"]<-"methylated"
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
sum_table_templet_temp[1,"MGMT_status"]<-"methylated"
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
sum_table_templet_temp[1,"MGMT_status"]<-"methylated"
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
sum_table_templet_temp[1,"MGMT_status"]<-"methylated"
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
output_dir<-paste0(output_dir_root,"//05_sensitive_analysis//02_Subgroup//03_MGMT status//methylated//PMID33130898")
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
sum_table_templet_temp[1,"IDH_status"]<-"Mixed"
sum_table_templet_temp[1,"MGMT_status"]<-"methylated"
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
sum_table_templet_temp[1,"MGMT_status"]<-"methylated"
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
####01_10 PMID37148198 start####
output_dir<-paste0(output_dir_root,"//05_sensitive_analysis//02_Subgroup//03_MGMT status//methylated//PMID37148198")
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
# threshold<-4
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
sum_table_templet_temp[1,"ResearchName"]<-"Michael_2023"
sum_table_templet_temp[1,"Age"]<-"Adult"
sum_table_templet_temp[1,"Gender"]<-"Mixed"
sum_table_templet_temp[1,"PR_Type"]<-"Recurrence"
sum_table_templet_temp[1,"IDH_status"]<-"WT"
sum_table_templet_temp[1,"MGMT_status"]<-"methylated"
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
sum_table_templet_temp[1,"MGMT_status"]<-"methylated"
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

output_dir<-paste0(output_dir_root,"//05_sensitive_analysis//02_Subgroup//03_MGMT status//unmethylated")
dir.create(output_dir)
input_dataset<- readRDS(file = input_dataset.rds_path)
input_dataset<-subset(input_dataset,MGMT_status=="unmethylated")
####01_04 PMID29910795 start####
output_dir<-paste0(output_dir_root,"//05_sensitive_analysis//02_Subgroup//03_MGMT status//unmethylated//PMID29910795")
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
sum_table_templet_temp[1,"IDH_status"]<-"Mixed"
sum_table_templet_temp[1,"MGMT_status"]<-"unmethylated"
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
sum_table_templet_temp[1,"MGMT_status"]<-"unmethylated"
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
sum_table_templet_temp[1,"MGMT_status"]<-"unmethylated"
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
sum_table_templet_temp[1,"MGMT_status"]<-"unmethylated"
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
output_dir<-paste0(output_dir_root,"//05_sensitive_analysis//02_Subgroup//03_MGMT status//unmethylated//PMID33130898")
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
sum_table_templet_temp[1,"IDH_status"]<-"Mixed"
sum_table_templet_temp[1,"MGMT_status"]<-"unmethylated"
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
sum_table_templet_temp[1,"MGMT_status"]<-"unmethylated"
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
output_dir<-paste0(output_dir_root,"//05_sensitive_analysis//02_Subgroup//03_MGMT status//unmethylated//PMID33838625")
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
# threshold<-3
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
sum_table_templet_temp[1,"ResearchName"]<-"Friedman_2021"
sum_table_templet_temp[1,"Age"]<-"Children"
sum_table_templet_temp[1,"Gender"]<-"Mixed"
sum_table_templet_temp[1,"PR_Type"]<-"Recurrence"
sum_table_templet_temp[1,"IDH_status"]<-"WT"
sum_table_templet_temp[1,"MGMT_status"]<-"unmethylated"
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
sum_table_templet_temp[1,"MGMT_status"]<-"unmethylated"
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
sum_table_templet_temp[1,"MGMT_status"]<-"unmethylated"
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
sum_table_templet_temp[1,"MGMT_status"]<-"unmethylated"
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
####01_10 PMID37148198 start####
output_dir<-paste0(output_dir_root,"//05_sensitive_analysis//02_Subgroup//03_MGMT status//unmethylated//PMID37148198")
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
# threshold<-4
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
sum_table_templet_temp[1,"ResearchName"]<-"Michael_2023"
sum_table_templet_temp[1,"Age"]<-"Adult"
sum_table_templet_temp[1,"Gender"]<-"Mixed"
sum_table_templet_temp[1,"PR_Type"]<-"Recurrence"
sum_table_templet_temp[1,"IDH_status"]<-"WT"
sum_table_templet_temp[1,"MGMT_status"]<-"unmethylated"
sum_table_templet_temp[1,"Immunotherapy"]<-"CAR-NK"
sum_table_templet_temp[1,"Complete_resection"]<-"Mixed"
sum_table_templet_temp[1,"Outcome"]<-"Survival after immunotherapy"
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
sum_table_templet_temp[1,"ResearchName"]<-"Michael_2023"
sum_table_templet_temp[1,"Age"]<-"Adult"
sum_table_templet_temp[1,"Gender"]<-"Mixed"
sum_table_templet_temp[1,"PR_Type"]<-"Recurrence"
sum_table_templet_temp[1,"IDH_status"]<-"WT"
sum_table_templet_temp[1,"MGMT_status"]<-"unmethylated"
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
saveRDS(sum_table,file = paste0(output_dir,"//sum_table_sensitive_analysis.rds"))

####--------------------------------- 02_04_CD8_Subgroup_Complete resection ---------------------------------####


library(survival)
library(survminer)
library(rms)
library(stringr)

####00 input data####
output_dir_root <- choose.dir(default = "D:\\output_dir", caption = "choose the output folder")
output_dir<-output_dir_root
# dir.create(paste0(output_dir_root,"//05_sensitive_analysis//02_Subgroup"))
dir.create(paste0(output_dir_root,"//05_sensitive_analysis//02_Subgroup//04_Complete resection"))

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

colnames(input_dataset)
table(input_dataset$Complete_resection)

output_dir<-paste0(output_dir_root,"//05_sensitive_analysis//02_Subgroup//04_Complete resection//Total excision")
dir.create(output_dir)
input_dataset<- readRDS(file = input_dataset.rds_path)
input_dataset<-input_dataset[(input_dataset$Complete_resection %in% c("Total excision", "Yes")),]
####01_02 PMID15452186 start####
output_dir<-paste0(output_dir_root,"//05_sensitive_analysis//02_Subgroup//04_Complete resection//Total excision//PMID15452186")
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
# threshold<-0.5
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
sum_table_templet_temp[1,"ResearchName"]<-"Steiner_2004"
sum_table_templet_temp[1,"Age"]<-"Adult"
sum_table_templet_temp[1,"Gender"]<-"Mixed"
sum_table_templet_temp[1,"PR_Type"]<-"Primary"
sum_table_templet_temp[1,"IDH_status"]<-"Mixed"
sum_table_templet_temp[1,"MGMT_status"]<-"Mixed"
sum_table_templet_temp[1,"Immunotherapy"]<-"Peptide Vaccination"
sum_table_templet_temp[1,"Complete_resection"]<-"Total excision"
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
sum_table_templet_temp[1,"Complete_resection"]<-"Total excision"
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
sum_table_templet_temp[1,"Complete_resection"]<-"Total excision"
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
sum_table_templet_temp[1,"Complete_resection"]<-"Total excision"
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
####01_04 PMID29910795 start####
output_dir<-paste0(output_dir_root,"//05_sensitive_analysis//02_Subgroup//04_Complete resection//Total excision//PMID29910795")
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
sum_table_templet_temp[1,"IDH_status"]<-"Mixed"
sum_table_templet_temp[1,"MGMT_status"]<-"Mixed"
sum_table_templet_temp[1,"Immunotherapy"]<-"DC Vaccination"
sum_table_templet_temp[1,"Complete_resection"]<-"Total excision"
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
sum_table_templet_temp[1,"Complete_resection"]<-"Total excision"
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
sum_table_templet_temp[1,"Complete_resection"]<-"Total excision"
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
sum_table_templet_temp[1,"Complete_resection"]<-"Total excision"
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
output_dir<-paste0(output_dir_root,"//05_sensitive_analysis//02_Subgroup//04_Complete resection//Total excision//PMID33130898")
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
sum_table_templet_temp[1,"IDH_status"]<-"Mixed"
sum_table_templet_temp[1,"MGMT_status"]<-"Mixed"
sum_table_templet_temp[1,"Immunotherapy"]<-"DC Vaccination"
sum_table_templet_temp[1,"Complete_resection"]<-"Total excision"
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
sum_table_templet_temp[1,"Complete_resection"]<-"Total excision"
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

output_dir<-paste0(output_dir_root,"//05_sensitive_analysis//02_Subgroup//04_Complete resection//Subtotal excision")
dir.create(output_dir)
input_dataset<- readRDS(file = input_dataset.rds_path)
input_dataset<-subset(input_dataset,Complete_resection=="Subtotal excision")
####01_04 PMID29910795 start####
output_dir<-paste0(output_dir_root,"//05_sensitive_analysis//02_Subgroup//04_Complete resection//Subtotal excision//PMID29910795")
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
# summary(f_coxph)

sum_table_templet_temp<-sum_table_templet
sum_table_templet_temp[1,"PMID"]<-temp_dataset_id
sum_table_templet_temp[1,"ResearchName"]<-"Jan_2018"
sum_table_templet_temp[1,"Age"]<-"Adult"
sum_table_templet_temp[1,"Gender"]<-"Mixed"
sum_table_templet_temp[1,"PR_Type"]<-"Primary"
sum_table_templet_temp[1,"IDH_status"]<-"Mixed"
sum_table_templet_temp[1,"MGMT_status"]<-"Mixed"
sum_table_templet_temp[1,"Immunotherapy"]<-"DC Vaccination"
sum_table_templet_temp[1,"Complete_resection"]<-"Subtotal excision"
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
sum_table_templet_temp[1,"ResearchName"]<-"Jan_2018"
sum_table_templet_temp[1,"Age"]<-"Adult"
sum_table_templet_temp[1,"Gender"]<-"Mixed"
sum_table_templet_temp[1,"PR_Type"]<-"Primary"
sum_table_templet_temp[1,"IDH_status"]<-"Mixed"
sum_table_templet_temp[1,"MGMT_status"]<-"Mixed"
sum_table_templet_temp[1,"Immunotherapy"]<-"DC Vaccination"
sum_table_templet_temp[1,"Complete_resection"]<-"Subtotal excision"
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
# summary(f_coxph)

sum_table_templet_temp<-sum_table_templet
sum_table_templet_temp[1,"PMID"]<-temp_dataset_id
sum_table_templet_temp[1,"ResearchName"]<-"Jan_2018"
sum_table_templet_temp[1,"Age"]<-"Adult"
sum_table_templet_temp[1,"Gender"]<-"Mixed"
sum_table_templet_temp[1,"PR_Type"]<-"Primary"
sum_table_templet_temp[1,"IDH_status"]<-"Mixed"
sum_table_templet_temp[1,"MGMT_status"]<-"Mixed"
sum_table_templet_temp[1,"Immunotherapy"]<-"DC Vaccination"
sum_table_templet_temp[1,"Complete_resection"]<-"Subtotal excision"
sum_table_templet_temp[1,"Outcome"]<-"PFS"
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
sum_table_templet_temp[1,"Complete_resection"]<-"Subtotal excision"
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
output_dir<-paste0(output_dir_root,"//05_sensitive_analysis//02_Subgroup//04_Complete resection//Subtotal excision//PMID33130898")
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
sum_table_templet_temp[1,"IDH_status"]<-"Mixed"
sum_table_templet_temp[1,"MGMT_status"]<-"Mixed"
sum_table_templet_temp[1,"Immunotherapy"]<-"DC Vaccination"
sum_table_templet_temp[1,"Complete_resection"]<-"Subtotal excision"
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
sum_table_templet_temp[1,"Complete_resection"]<-"Subtotal excision"
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

output_dir<-paste0(output_dir_root,"//05_sensitive_analysis//02_Subgroup//04_Complete resection//Partial excision")
dir.create(output_dir)
input_dataset<- readRDS(file = input_dataset.rds_path)
input_dataset<-subset(input_dataset,Complete_resection=="Partial excision")
####01_04 PMID29910795 start####
output_dir<-paste0(output_dir_root,"//05_sensitive_analysis//02_Subgroup//04_Complete resection//Partial excision//PMID29910795")
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
# summary(f_coxph)

sum_table_templet_temp<-sum_table_templet
sum_table_templet_temp[1,"PMID"]<-temp_dataset_id
sum_table_templet_temp[1,"ResearchName"]<-"Jan_2018"
sum_table_templet_temp[1,"Age"]<-"Adult"
sum_table_templet_temp[1,"Gender"]<-"Mixed"
sum_table_templet_temp[1,"PR_Type"]<-"Primary"
sum_table_templet_temp[1,"IDH_status"]<-"Mixed"
sum_table_templet_temp[1,"MGMT_status"]<-"Mixed"
sum_table_templet_temp[1,"Immunotherapy"]<-"DC Vaccination"
sum_table_templet_temp[1,"Complete_resection"]<-"Partial excision"
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
sum_table_templet_temp[1,"ResearchName"]<-"Jan_2018"
sum_table_templet_temp[1,"Age"]<-"Adult"
sum_table_templet_temp[1,"Gender"]<-"Mixed"
sum_table_templet_temp[1,"PR_Type"]<-"Primary"
sum_table_templet_temp[1,"IDH_status"]<-"Mixed"
sum_table_templet_temp[1,"MGMT_status"]<-"Mixed"
sum_table_templet_temp[1,"Immunotherapy"]<-"DC Vaccination"
sum_table_templet_temp[1,"Complete_resection"]<-"Partial excision"
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
# summary(f_coxph)

sum_table_templet_temp<-sum_table_templet
sum_table_templet_temp[1,"PMID"]<-temp_dataset_id
sum_table_templet_temp[1,"ResearchName"]<-"Jan_2018"
sum_table_templet_temp[1,"Age"]<-"Adult"
sum_table_templet_temp[1,"Gender"]<-"Mixed"
sum_table_templet_temp[1,"PR_Type"]<-"Primary"
sum_table_templet_temp[1,"IDH_status"]<-"Mixed"
sum_table_templet_temp[1,"MGMT_status"]<-"Mixed"
sum_table_templet_temp[1,"Immunotherapy"]<-"DC Vaccination"
sum_table_templet_temp[1,"Complete_resection"]<-"Partial excision"
sum_table_templet_temp[1,"Outcome"]<-"PFS"
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
sum_table_templet_temp[1,"Complete_resection"]<-"Partial excision"
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
output_dir<-paste0(output_dir_root,"//05_sensitive_analysis//02_Subgroup//04_Complete resection//Partial excision//PMID33130898")
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
sum_table_templet_temp[1,"IDH_status"]<-"Mixed"
sum_table_templet_temp[1,"MGMT_status"]<-"Mixed"
sum_table_templet_temp[1,"Immunotherapy"]<-"DC Vaccination"
sum_table_templet_temp[1,"Complete_resection"]<-"Partial excision"
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
sum_table_templet_temp[1,"Complete_resection"]<-"Partial excision"
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
saveRDS(sum_table,file = paste0(output_dir,"//sum_table_sensitive_analysis.rds"))

####--------------------------------- 03_meta_analysis ---------------------------------####

library(meta)
####00 input data####
output_dir_root <- choose.dir(default = "D:\\output_dir", caption = "choose the output folder")
output_dir<-output_dir_root
dir.create(paste0(output_dir_root,"//05_sensitive_analysis//03_meta_analysis"))

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

output_dir<-paste0(output_dir_root,"//05_sensitive_analysis//03_meta_analysis//01 All_population")
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
#no exclusion
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

output_dir<-paste0(output_dir_root,"//05_sensitive_analysis//03_meta_analysis//02_01 Subgroup_Gender")
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

output_dir<-paste0(output_dir_root,"//05_sensitive_analysis//03_meta_analysis//02_02 Subgroup_IDH_status")
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
# no exclusion
# exclude duplicate
meta_table<-subset(meta_table,Gender=="Mixed")
meta_table<-subset(meta_table,MGMT_status=="Mixed")
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

output_dir<-paste0(output_dir_root,"//05_sensitive_analysis//03_meta_analysis//02_03 Subgroup_MGMT_status")
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

output_dir<-paste0(output_dir_root,"//05_sensitive_analysis//03_meta_analysis//02_04 Subgroup_Complete_resection")
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
# meta-analysis
# no study left
meta_result<-metagen(logHR, selogHR, sm = "HR",studlab = ResearchName,data=meta_table)
# forest plot
pdf(paste0(output_dir,"//ForestPlot_Subgroup_Complete_resection_Partial excision_OS_dichotomousCD8.pdf"),width=10, height=4)
p_forest<-forest(meta_result,print.Q =T,digits=4,digits.pval = 3,digits.pval.Q = 3,
                 test.overall.common = TRUE,
                 test.overall.random = TRUE)
print(p_forest)
dev.off()
# funnel plot
pdf(paste0(output_dir,"//FunnelPlot_Subgroup_Complete_resection_Partial excision_OS_dichotomousCD8.pdf"),width=6, height=6)
p_funnel<-funnel(meta_result)
print(p_funnel)
dev.off()

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

output_dir<-paste0(output_dir_root,"//05_sensitive_analysis//03_meta_analysis//02_05 Subgroup_PR_Type")
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
# no exclusion
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

output_dir<-paste0(output_dir_root,"//05_sensitive_analysis//03_meta_analysis//02_06 Subgroup_Immunotherapy")
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
# no exclusion
# meta-analysis
meta_result<-metagen(logHR, selogHR, sm = "HR",studlab = ResearchName,data=meta_table)

# forest plot
pdf(paste0(output_dir,"//ForestPlot_Subgroup_Immunotherapy_CAR-NK_SurvivalAfterImmunotherapy_dichotomousCD8.pdf"),width=10, height=4)
p_forest<-forest(meta_result,print.Q =T,digits=4,digits.pval = 3,digits.pval.Q = 3,
                 test.overall.common = TRUE,
                 test.overall.random = TRUE)
print(p_forest)
dev.off()
# funnel plot
pdf(paste0(output_dir,"//FunnelPlot_Subgroup_Immunotherapy_CAR-NK_SurvivalAfterImmunotherapy_dichotomousCD8.pdf"),width=6, height=6)
p_funnel<-funnel(meta_result)
print(p_funnel)
dev.off()

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
write.table(pooled_table,file = paste0(output_dir, "\\", "pooled_table_sensitive_analysis.txt"), sep = "\t")
saveRDS(pooled_table,file = paste0(output_dir,"//pooled_table_sensitive_analysis.rds"))











