
library(stringr)
library(dbplyr)
library(ggplot2)
#DIY functions

{
  cal_contingency_tb<-function(data,colume1,colume2,na.ignor=TRUE){
    if(na.ignor){
      data<-data[!is.na(data[,colume1]),]
      data<-data[!is.na(data[,colume2]),]
    }else{
      for (index in 1:nrow(data)) {
        if(is.na(data[index,colume1])){
          data[index,colume1]<-"NA"
        }
        if(is.na(data[index,colume2])){
          data[index,colume2]<-"NA"
        }
      }
    }
    
    col_types<-names(table(data[,colume1]))
    row_types<-names(table(data[,colume2]))
    result_table<-as.data.frame(matrix(data = 0,nrow=length(row_types),ncol=length(col_types)))
    colnames(result_table)<-col_types
    rownames(result_table)<-row_types
    
    for (index in 1:nrow(data)) {
      value_colume1<-data[index,colume1]
      value_colume2<-data[index,colume2]
      position_col<-match(x = value_colume1,table = col_types)
      position_row<-match(x = value_colume2,table = row_types)
      result_table[position_row,position_col]<-result_table[position_row,position_col]+1
    }
    colnames(result_table)<-paste0(colume1,"_",colnames(result_table))
    rownames(result_table)<-paste0(colume2,"_",rownames(result_table))
    
    return(result_table)
    
  }
  yield_combine <- function(vec){
    vec <- sort(vec)
    out_list <- list()
    index <- 1
    for(i in 1:length(vec)){
      sub_com <- combn(vec, i)
      for(j in 1:ncol(sub_com)){
        out_list[[index]] <- sub_com[, j]
        index = index +1
      }
    }
    return(out_list)
  }
  summary_combine_calculator<-function(data,columes){
    temp_data<-data[,columes]
    combines_list<-yield_combine(1:length(columes))
    #add combined columes into temp_data
    for (index in 1:length(combines_list)) {
      new_colname<-str_c(c("combined",colnames(temp_data)[combines_list[index][[1]]]),collapse = "_")
      temp_data<-cbind(temp_data,temp_data[,1])
      colnames(temp_data)[length(colnames(temp_data))]<-new_colname
      temp_data[,new_colname]<-NA
      for (index_row in 1:nrow(temp_data)) {
        temp_judged_items<-temp_data[index_row,colnames(temp_data)[combines_list[index][[1]]]]
        if(any(temp_judged_items=="N")){
          temp_data[index_row,new_colname]<-"No"
        }else{
          temp_data[index_row,new_colname]<-"Yes"
        }
      }
    }
    #move combined columes from temp_data to data
    temp_data<-temp_data[,c(!(colnames(temp_data) %in% columes))]
    data<-cbind(data,temp_data)
    return(data)
  }
  cal_contingency_tb_Yes_No_restrict<-function(data,colume1,colume2){
    if(any(c(c(!(names(table(data[,colume1])) %in% c("No","Yes"))),
             c(!(names(table(data[,colume2])) %in% c("No","Yes")))))){
      print(paste0("these columes: ",colume1," and ",colume2," have value that is not No or Yes!"))
    }
    col_types<-c("No","Yes")# this row modifed
    row_types<-c("No","Yes")# this row modifed
    result_table<-as.data.frame(matrix(data = 0,nrow=length(row_types),ncol=length(col_types)))
    colnames(result_table)<-col_types
    rownames(result_table)<-row_types
    
    for (index in 1:nrow(data)) {
      value_colume1<-data[index,colume1]
      value_colume2<-data[index,colume2]
      position_col<-match(x = value_colume1,table = col_types)#missed match may happen here, if value other than "No","Yes" exist
      position_row<-match(x = value_colume2,table = row_types)
      result_table[position_row,position_col]<-result_table[position_row,position_col]+1
    }
    colnames(result_table)<-paste0(colume1,"_",colnames(result_table))
    rownames(result_table)<-paste0(colume2,"_",rownames(result_table))
    
    return(result_table)
    
  }
  metrics_calculator<-function(data,comp_columes,manual_colume,Question){
    #out_table initiation
    out_table_templet<-data.frame(matrix(ncol = 10, nrow = 0))
    colnames(out_table_templet) <- c('Question', 'Approach', 'Precision',"Recall","F1","Work_reduced",
                                     "TN_Manual_No_Comp_No","FN_Manual_Yes_Comp_No",
                                     "FP_Manual_No_Comp_Yes","TP_Manual_Yes_Comp_Yes")
    out_table<-out_table_templet
    #
    for (index_colume in 1:length(comp_columes)) {
      temp_comp_colume<-comp_columes[index_colume]
      temp_cal_results<-cal_contingency_tb_Yes_No_restrict(data=data,colume1=manual_colume,colume2=temp_comp_colume)
      print(paste0(manual_colume," vs. ",temp_comp_colume))
      print(temp_cal_results)
      print(" ")
      #add temp_out_table
      temp_out_table<-out_table_templet
      temp_out_table[1,"Question"]<-Question
      temp_out_table[1,"Approach"]<-temp_comp_colume
      temp_out_table[1,"TN_Manual_No_Comp_No"]<-temp_cal_results[(paste0(temp_comp_colume,"_No")),(paste0(manual_colume,"_No"))]
      temp_out_table[1,"FN_Manual_Yes_Comp_No"]<-temp_cal_results[(paste0(temp_comp_colume,"_No")),(paste0(manual_colume,"_Yes"))]
      temp_out_table[1,"FP_Manual_No_Comp_Yes"]<-temp_cal_results[(paste0(temp_comp_colume,"_Yes")),(paste0(manual_colume,"_No"))]
      temp_out_table[1,"TP_Manual_Yes_Comp_Yes"]<-temp_cal_results[(paste0(temp_comp_colume,"_Yes")),(paste0(manual_colume,"_Yes"))]
      temp_out_table[1,"Precision"]<-temp_out_table[1,"TP_Manual_Yes_Comp_Yes"]/(temp_out_table[1,"TP_Manual_Yes_Comp_Yes"]+temp_out_table[1,"FP_Manual_No_Comp_Yes"])
      temp_out_table[1,"Recall"]<-temp_out_table[1,"TP_Manual_Yes_Comp_Yes"]/(temp_out_table[1,"TP_Manual_Yes_Comp_Yes"]+temp_out_table[1,"FN_Manual_Yes_Comp_No"])
      temp_out_table[1,"F1"]<-2*temp_out_table[1,"Precision"]*temp_out_table[1,"Recall"]/(temp_out_table[1,"Precision"]+temp_out_table[1,"Recall"])
      temp_out_table[1,"Work_reduced"]<-(sum(temp_out_table[1,"TN_Manual_No_Comp_No"]+temp_out_table[1,"FN_Manual_Yes_Comp_No"]))/(sum(temp_out_table[1,"TN_Manual_No_Comp_No"]+temp_out_table[1,"FN_Manual_Yes_Comp_No"]+temp_out_table[1,"FP_Manual_No_Comp_Yes"]+temp_out_table[1,"TP_Manual_Yes_Comp_Yes"]))
      #merge out_table
      out_table<-rbind(out_table,temp_out_table)
    }
    return(out_table)
  }
}


####00 input data####
output_dir <- choose.dir(default = "D:\\output_dir", caption = "choose the output folder")
data_dir <- choose.dir(default = "D:\\data_dir", caption = "choose the folder storing input_data.txt file")

input_dataset_path <- choose.files(default = data_dir, caption = "choose the input_data.txt file",
                                   multi = TRUE, filters = Filters,
                                   index = nrow(Filters))
input_dataset<- read.csv(input_dataset_path, header = TRUE,sep="\t", stringsAsFactors=FALSE)
#combine "NS" into "Yes"
input_dataset[(input_dataset == "NS")&(!is.na(input_dataset))] = "Yes" 
colnames(input_dataset)

#### CD8_GBM ####
set.seed(1234)
metrics<-function(table,label="label"){
  table_temp<-table
  Real_No_name<-colnames(table_temp)[grepl("_No",colnames(table_temp))]
  Real_Yes_name<-colnames(table_temp)[grepl("_Yes",colnames(table_temp))]
  Model_No_name<-rownames(table_temp)[grepl("_No",rownames(table_temp))]
  Model_Yes_name<-rownames(table_temp)[grepl("_Yes",rownames(table_temp))]
  
  TP<-table_temp[Model_Yes_name,Real_Yes_name]
  FP<-table_temp[Model_Yes_name,Real_No_name]
  TN<-table_temp[Model_No_name,Real_No_name]
  FN<-table_temp[Model_No_name,Real_Yes_name]
  
  precision <-(TP/(TP+FP))
  recall<-(TP/(TP+FN))
  F1 <- (2 * precision * recall) / (precision + recall)
  Work_reduced<-(TN+FN)/(TP+FP+TN+FN)
  
  # print(paste0("TP: ",TP,"; FP: ",FP,"; TN: ",TN,"; FN: ",FN))
  # print(paste0("precision: ",precision))
  # print(paste0("recall: ",recall))
  # print(paste0("F1: ",F1))
  return(data.frame(TP=TP,FP=FP,TN=TN,FN=FN,precision=precision,recall=recall,F1=F1,Work_reduced=Work_reduced,label=label))
}
output_CD8_GBM<-data.frame(TP="TP",FP="FP",TN="TN",FN="FN",precision="precision",recall="recall",F1="F1",Work_reduced="Work_reduced",label="label")
#### CD8_GBM_species ####
#GPT35
species_contingency_tb<-cal_contingency_tb(data=input_dataset,colume1="ResercherLabeled_species",colume2="GPT35_species",na.ignor=TRUE)
species_contingency_tb
output_CD8_GBM<-rbind(output_CD8_GBM,metrics(table = species_contingency_tb,label = "species_GPT35"))
#GPT4
species_contingency_tb<-cal_contingency_tb(data=input_dataset,colume1="ResercherLabeled_species",colume2="GPT4_species",na.ignor=TRUE)
species_contingency_tb
output_CD8_GBM<-rbind(output_CD8_GBM,metrics(table = species_contingency_tb,label = "species_GPT4"))

#Random classifier
temp_Random_output_CD8_GBM<-data.frame(TP="TP",FP="FP",TN="TN",FN="FN",precision="precision",recall="recall",F1="F1",Work_reduced="Work_reduced",label="label")
for (index in 1:100) {
  input_dataset$random_col<-sample(c("Yes", "No"), nrow(input_dataset), replace=TRUE)
  contingency_tb<-cal_contingency_tb(data=input_dataset,colume1="ResercherLabeled_species",colume2="random_col",na.ignor=TRUE)
  temp_Random_output_CD8_GBM<-rbind(temp_Random_output_CD8_GBM,metrics(table = contingency_tb,label = index))
}
temp_Random_output_CD8_GBM<-temp_Random_output_CD8_GBM[-1,]
temp_Random_output_CD8_GBM<-apply(temp_Random_output_CD8_GBM, 2, as.numeric)
temp_Random_output_CD8_GBM<-apply(temp_Random_output_CD8_GBM, 2, function(x){mean(x,na.rm=TRUE)})
temp_Random_output_CD8_GBM<-as.data.frame(t(temp_Random_output_CD8_GBM))
temp_Random_output_CD8_GBM$label<-"species_Random_classifier"
output_CD8_GBM<-rbind(output_CD8_GBM,temp_Random_output_CD8_GBM)

#### CD8_GBM_disease ####
#GPT35
disease_contingency_tb<-cal_contingency_tb(data=input_dataset,colume1="ResercherLabeled_disease",colume2="GPT35_disease",na.ignor=TRUE)
disease_contingency_tb
output_CD8_GBM<-rbind(output_CD8_GBM,metrics(table = disease_contingency_tb,label = "disease_GPT35"))
#GPT4
disease_contingency_tb<-cal_contingency_tb(data=input_dataset,colume1="ResercherLabeled_disease",colume2="GPT4_disease",na.ignor=TRUE)
disease_contingency_tb
output_CD8_GBM<-rbind(output_CD8_GBM,metrics(table = disease_contingency_tb,label = "disease_GPT4"))

#Random classifier
temp_Random_output_CD8_GBM<-data.frame(TP="TP",FP="FP",TN="TN",FN="FN",precision="precision",recall="recall",F1="F1",Work_reduced="Work_reduced",label="label")
for (index in 1:100) {
  input_dataset$random_col<-sample(c("Yes", "No"), nrow(input_dataset), replace=TRUE)
  contingency_tb<-cal_contingency_tb(data=input_dataset,colume1="ResercherLabeled_disease",colume2="random_col",na.ignor=TRUE)
  temp_Random_output_CD8_GBM<-rbind(temp_Random_output_CD8_GBM,metrics(table = contingency_tb,label = index))
}
temp_Random_output_CD8_GBM<-temp_Random_output_CD8_GBM[-1,]
temp_Random_output_CD8_GBM<-apply(temp_Random_output_CD8_GBM, 2, as.numeric)
temp_Random_output_CD8_GBM<-apply(temp_Random_output_CD8_GBM, 2, function(x){mean(x,na.rm=TRUE)})
temp_Random_output_CD8_GBM<-as.data.frame(t(temp_Random_output_CD8_GBM))
temp_Random_output_CD8_GBM$label<-"disease_Random_classifier"
output_CD8_GBM<-rbind(output_CD8_GBM,temp_Random_output_CD8_GBM)


#### CD8_GBM_treatment ####
#GPT35
treatment_contingency_tb<-cal_contingency_tb(data=input_dataset,colume1="ResercherLabeled_treatment",colume2="GPT35_treatment",na.ignor=TRUE)
treatment_contingency_tb
output_CD8_GBM<-rbind(output_CD8_GBM,metrics(table = treatment_contingency_tb,label = "treatment_GPT35"))
#GPT4
treatment_contingency_tb<-cal_contingency_tb(data=input_dataset,colume1="ResercherLabeled_treatment",colume2="GPT4_treatment",na.ignor=TRUE)
treatment_contingency_tb
output_CD8_GBM<-rbind(output_CD8_GBM,metrics(table = treatment_contingency_tb,label = "treatment_GPT4"))

#Random classifier
temp_Random_output_CD8_GBM<-data.frame(TP="TP",FP="FP",TN="TN",FN="FN",precision="precision",recall="recall",F1="F1",Work_reduced="Work_reduced",label="label")
for (index in 1:100) {
  input_dataset$random_col<-sample(c("Yes", "No"), nrow(input_dataset), replace=TRUE)
  contingency_tb<-cal_contingency_tb(data=input_dataset,colume1="ResercherLabeled_treatment",colume2="random_col",na.ignor=TRUE)
  temp_Random_output_CD8_GBM<-rbind(temp_Random_output_CD8_GBM,metrics(table = contingency_tb,label = index))
}
temp_Random_output_CD8_GBM<-temp_Random_output_CD8_GBM[-1,]
temp_Random_output_CD8_GBM<-apply(temp_Random_output_CD8_GBM, 2, as.numeric)
temp_Random_output_CD8_GBM<-apply(temp_Random_output_CD8_GBM, 2, function(x){mean(x,na.rm=TRUE)})
temp_Random_output_CD8_GBM<-as.data.frame(t(temp_Random_output_CD8_GBM))
temp_Random_output_CD8_GBM$label<-"treatment_Random_classifier"
output_CD8_GBM<-rbind(output_CD8_GBM,temp_Random_output_CD8_GBM)

#### CD8_GBM_research_type ####
#GPT35
research_type_contingency_tb<-cal_contingency_tb(data=input_dataset,colume1="ResercherLabeled_research_type",colume2="GPT35_research_type",na.ignor=TRUE)
research_type_contingency_tb
output_CD8_GBM<-rbind(output_CD8_GBM,metrics(table = research_type_contingency_tb,label = "research_type_GPT35"))
#GPT4
research_type_contingency_tb<-cal_contingency_tb(data=input_dataset,colume1="ResercherLabeled_research_type",colume2="GPT4_research_type",na.ignor=TRUE)
research_type_contingency_tb
output_CD8_GBM<-rbind(output_CD8_GBM,metrics(table = research_type_contingency_tb,label = "research_type_GPT4"))

#Random classifier
temp_Random_output_CD8_GBM<-data.frame(TP="TP",FP="FP",TN="TN",FN="FN",precision="precision",recall="recall",F1="F1",Work_reduced="Work_reduced",label="label")
for (index in 1:100) {
  input_dataset$random_col<-sample(c("Yes", "No"), nrow(input_dataset), replace=TRUE)
  contingency_tb<-cal_contingency_tb(data=input_dataset,colume1="ResercherLabeled_research_type",colume2="random_col",na.ignor=TRUE)
  temp_Random_output_CD8_GBM<-rbind(temp_Random_output_CD8_GBM,metrics(table = contingency_tb,label = index))
}
temp_Random_output_CD8_GBM<-temp_Random_output_CD8_GBM[-1,]
temp_Random_output_CD8_GBM<-apply(temp_Random_output_CD8_GBM, 2, as.numeric)
temp_Random_output_CD8_GBM<-apply(temp_Random_output_CD8_GBM, 2, function(x){mean(x,na.rm=TRUE)})
temp_Random_output_CD8_GBM<-as.data.frame(t(temp_Random_output_CD8_GBM))
temp_Random_output_CD8_GBM$label<-"research_type_Random_classifier"
output_CD8_GBM<-rbind(output_CD8_GBM,temp_Random_output_CD8_GBM)




output_CD8_GBM<-output_CD8_GBM[-1,]
write.table(output_CD8_GBM,file = paste0(output_dir, "\\", "CD8_GBM_single_question_evaluation.txt"), sep = "\t")


#### IBD ####
set.seed(1234)
metrics<-function(table,label="label"){
  table_temp<-table
  Real_No_name<-colnames(table_temp)[grepl("_No",colnames(table_temp))]
  Real_Yes_name<-colnames(table_temp)[grepl("_Yes",colnames(table_temp))]
  Model_No_name<-rownames(table_temp)[grepl("_No",rownames(table_temp))]
  Model_Yes_name<-rownames(table_temp)[grepl("_Yes",rownames(table_temp))]
  
  TP<-table_temp[Model_Yes_name,Real_Yes_name]
  FP<-table_temp[Model_Yes_name,Real_No_name]
  TN<-table_temp[Model_No_name,Real_No_name]
  FN<-table_temp[Model_No_name,Real_Yes_name]
  
  precision <-(TP/(TP+FP))
  recall<-(TP/(TP+FN))
  F1 <- (2 * precision * recall) / (precision + recall)
  Work_reduced<-(TN+FN)/(TP+FP+TN+FN)
  # print(paste0("TP: ",TP,"; FP: ",FP,"; TN: ",TN,"; FN: ",FN))
  # print(paste0("precision: ",precision))
  # print(paste0("recall: ",recall))
  # print(paste0("F1: ",F1))
  return(data.frame(TP=TP,FP=FP,TN=TN,FN=FN,precision=precision,recall=recall,F1=F1,Work_reduced=Work_reduced,label=label))
}
output_IBD<-data.frame(TP="TP",FP="FP",TN="TN",FN="FN",precision="precision",recall="recall",F1="F1",Work_reduced="Work_reduced",label="label")
#### IBD_species ####
#GPT35
species_contingency_tb<-cal_contingency_tb(data=input_dataset,colume1="ResercherLabeled_Species",colume2="GPT35_species",na.ignor=TRUE)
species_contingency_tb
output_IBD<-rbind(output_IBD,metrics(table = species_contingency_tb,label = "species_GPT35"))
#GPT4
species_contingency_tb<-cal_contingency_tb(data=input_dataset,colume1="ResercherLabeled_Species",colume2="GPT4_species",na.ignor=TRUE)
species_contingency_tb
output_IBD<-rbind(output_IBD,metrics(table = species_contingency_tb,label = "species_GPT4"))
#Baichuan
# species_contingency_tb<-cal_contingency_tb(data=input_dataset,colume1="ResercherLabeled_Species",colume2="Baichuan_Action1",na.ignor=TRUE)
# species_contingency_tb
# output_IBD<-rbind(output_IBD,metrics(table = species_contingency_tb,label = "species_Baichuan"))
#Random classifier
temp_Random_output_IBD<-data.frame(TP="TP",FP="FP",TN="TN",FN="FN",precision="precision",recall="recall",F1="F1",Work_reduced="Work_reduced",label="label")
for (index in 1:100) {
  input_dataset$random_col<-sample(c("Yes", "No"), nrow(input_dataset), replace=TRUE)
  contingency_tb<-cal_contingency_tb(data=input_dataset,colume1="ResercherLabeled_Species",colume2="random_col",na.ignor=TRUE)
  temp_Random_output_IBD<-rbind(temp_Random_output_IBD,metrics(table = contingency_tb,label = index))
}
temp_Random_output_IBD<-temp_Random_output_IBD[-1,]
temp_Random_output_IBD<-apply(temp_Random_output_IBD, 2, as.numeric)
temp_Random_output_IBD<-apply(temp_Random_output_IBD, 2, function(x){mean(x,na.rm=TRUE)})
temp_Random_output_IBD<-as.data.frame(t(temp_Random_output_IBD))
temp_Random_output_IBD$label<-"species_Random_classifier"
output_IBD<-rbind(output_IBD,temp_Random_output_IBD)

#### IBD_disease ####
#GPT35
disease_contingency_tb<-cal_contingency_tb(data=input_dataset,colume1="ResercherLabeled_disease",colume2="GPT35_disease",na.ignor=TRUE)
disease_contingency_tb
output_IBD<-rbind(output_IBD,metrics(table = disease_contingency_tb,label = "disease_GPT35"))
#GPT4
disease_contingency_tb<-cal_contingency_tb(data=input_dataset,colume1="ResercherLabeled_disease",colume2="GPT4_disease",na.ignor=TRUE)
disease_contingency_tb
output_IBD<-rbind(output_IBD,metrics(table = disease_contingency_tb,label = "disease_GPT4"))
#Baichuan
# disease_contingency_tb<-cal_contingency_tb(data=input_dataset,colume1="ResercherLabeled_disease",colume2="Baichuan_Action2",na.ignor=TRUE)
# disease_contingency_tb
# output_IBD<-rbind(output_IBD,metrics(table = disease_contingency_tb,label = "disease_Baichuan"))
#Random classifier
temp_Random_output_IBD<-data.frame(TP="TP",FP="FP",TN="TN",FN="FN",precision="precision",recall="recall",F1="F1",Work_reduced="Work_reduced",label="label")
for (index in 1:100) {
  input_dataset$random_col<-sample(c("Yes", "No"), nrow(input_dataset), replace=TRUE)
  contingency_tb<-cal_contingency_tb(data=input_dataset,colume1="ResercherLabeled_disease",colume2="random_col",na.ignor=TRUE)
  temp_Random_output_IBD<-rbind(temp_Random_output_IBD,metrics(table = contingency_tb,label = index))
}
temp_Random_output_IBD<-temp_Random_output_IBD[-1,]
temp_Random_output_IBD<-apply(temp_Random_output_IBD, 2, as.numeric)
temp_Random_output_IBD<-apply(temp_Random_output_IBD, 2, function(x){mean(x,na.rm=TRUE)})
temp_Random_output_IBD<-as.data.frame(t(temp_Random_output_IBD))
temp_Random_output_IBD$label<-"disease_Random_classifier"
output_IBD<-rbind(output_IBD,temp_Random_output_IBD)


#### IBD_research_type ####
#GPT35
research_type_contingency_tb<-cal_contingency_tb(data=input_dataset,colume1="ResercherLabeled_research_type",colume2="GPT35_research_type",na.ignor=TRUE)
research_type_contingency_tb
output_IBD<-rbind(output_IBD,metrics(table = research_type_contingency_tb,label = "research_type_GPT35"))
#GPT4
research_type_contingency_tb<-cal_contingency_tb(data=input_dataset,colume1="ResercherLabeled_research_type",colume2="GPT4_research_type",na.ignor=TRUE)
research_type_contingency_tb
output_IBD<-rbind(output_IBD,metrics(table = research_type_contingency_tb,label = "research_type_GPT4"))
#Baichuan
# research_type_contingency_tb<-cal_contingency_tb(data=input_dataset,colume1="ResercherLabeled_research_type",colume2="Baichuan_Action3",na.ignor=TRUE)
# research_type_contingency_tb
# output_IBD<-rbind(output_IBD,metrics(table = research_type_contingency_tb,label = "research_type_Baichuan"))
#Random classifier
temp_Random_output_IBD<-data.frame(TP="TP",FP="FP",TN="TN",FN="FN",precision="precision",recall="recall",F1="F1",Work_reduced="Work_reduced",label="label")
for (index in 1:100) {
  input_dataset$random_col<-sample(c("Yes", "No"), nrow(input_dataset), replace=TRUE)
  contingency_tb<-cal_contingency_tb(data=input_dataset,colume1="ResercherLabeled_research_type",colume2="random_col",na.ignor=TRUE)
  temp_Random_output_IBD<-rbind(temp_Random_output_IBD,metrics(table = contingency_tb,label = index))
}
temp_Random_output_IBD<-temp_Random_output_IBD[-1,]
temp_Random_output_IBD<-apply(temp_Random_output_IBD, 2, as.numeric)
temp_Random_output_IBD<-apply(temp_Random_output_IBD, 2, function(x){mean(x,na.rm=TRUE)})
temp_Random_output_IBD<-as.data.frame(t(temp_Random_output_IBD))
temp_Random_output_IBD$label<-"research_type_Random_classifier"
output_IBD<-rbind(output_IBD,temp_Random_output_IBD)


#### IBD_age ####
#GPT35
age_contingency_tb<-cal_contingency_tb(data=input_dataset,colume1="ResercherLabeled_age",colume2="GPT35_age",na.ignor=TRUE)
age_contingency_tb
output_IBD<-rbind(output_IBD,metrics(table = age_contingency_tb,label = "age_GPT35"))
#GPT4
age_contingency_tb<-cal_contingency_tb(data=input_dataset,colume1="ResercherLabeled_age",colume2="GPT4_age",na.ignor=TRUE)
age_contingency_tb
output_IBD<-rbind(output_IBD,metrics(table = age_contingency_tb,label = "age_GPT4"))
#Baichuan
# age_contingency_tb<-cal_contingency_tb(data=input_dataset,colume1="ResercherLabeled_age",colume2="Baichuan_Action4",na.ignor=TRUE)
# age_contingency_tb
# output_IBD<-rbind(output_IBD,metrics(table = age_contingency_tb,label = "age_Baichuan"))
#Random classifier
temp_Random_output_IBD<-data.frame(TP="TP",FP="FP",TN="TN",FN="FN",precision="precision",recall="recall",F1="F1",Work_reduced="Work_reduced",label="label")
for (index in 1:100) {
  input_dataset$random_col<-sample(c("Yes", "No"), nrow(input_dataset), replace=TRUE)
  contingency_tb<-cal_contingency_tb(data=input_dataset,colume1="ResercherLabeled_age",colume2="random_col",na.ignor=TRUE)
  temp_Random_output_IBD<-rbind(temp_Random_output_IBD,metrics(table = contingency_tb,label = index))
}
temp_Random_output_IBD<-temp_Random_output_IBD[-1,]
temp_Random_output_IBD<-apply(temp_Random_output_IBD, 2, as.numeric)
temp_Random_output_IBD<-apply(temp_Random_output_IBD, 2, function(x){mean(x,na.rm=TRUE)})
temp_Random_output_IBD<-as.data.frame(t(temp_Random_output_IBD))
temp_Random_output_IBD$label<-"age_Random_classifier"
output_IBD<-rbind(output_IBD,temp_Random_output_IBD)

#### IBD_Protein_related ####
#GPT35
Protein_related_contingency_tb<-cal_contingency_tb(data=input_dataset,colume1="ResercherLabeled_Protein_related",colume2="GPT35_Protein_related",na.ignor=TRUE)
Protein_related_contingency_tb
output_IBD<-rbind(output_IBD,metrics(table = Protein_related_contingency_tb,label = "Protein_related_GPT35"))
#GPT4
Protein_related_contingency_tb<-cal_contingency_tb(data=input_dataset,colume1="ResercherLabeled_Protein_related",colume2="GPT4_Protein_related",na.ignor=TRUE)
Protein_related_contingency_tb
output_IBD<-rbind(output_IBD,metrics(table = Protein_related_contingency_tb,label = "Protein_related_GPT4"))
#Baichuan
# Protein_related_contingency_tb<-cal_contingency_tb(data=input_dataset,colume1="ResercherLabeled_Protein_related",colume2="Baichuan_Action5",na.ignor=TRUE)
# Protein_related_contingency_tb
# output_IBD<-rbind(output_IBD,metrics(table = Protein_related_contingency_tb,label = "Protein_related_Baichuan"))
#Random classifier
temp_Random_output_IBD<-data.frame(TP="TP",FP="FP",TN="TN",FN="FN",precision="precision",recall="recall",F1="F1",Work_reduced="Work_reduced",label="label")
for (index in 1:100) {
  input_dataset$random_col<-sample(c("Yes", "No"), nrow(input_dataset), replace=TRUE)
  contingency_tb<-cal_contingency_tb(data=input_dataset,colume1="ResercherLabeled_Protein_related",colume2="random_col",na.ignor=TRUE)
  temp_Random_output_IBD<-rbind(temp_Random_output_IBD,metrics(table = contingency_tb,label = index))
}
temp_Random_output_IBD<-temp_Random_output_IBD[-1,]
temp_Random_output_IBD<-apply(temp_Random_output_IBD, 2, as.numeric)
temp_Random_output_IBD<-apply(temp_Random_output_IBD, 2, function(x){mean(x,na.rm=TRUE)})
temp_Random_output_IBD<-as.data.frame(t(temp_Random_output_IBD))
temp_Random_output_IBD$label<-"Protein_related_Random_classifier"
output_IBD<-rbind(output_IBD,temp_Random_output_IBD)



output_IBD<-output_IBD[-1,]
write.table(output_IBD,file = paste0(output_dir, "\\", "IBD_single_question_evaluation.txt"), sep = "\t")


#### DM ####
set.seed(1234)
metrics<-function(table,label="label"){
  table_temp<-table
  Real_No_name<-colnames(table_temp)[grepl("_No",colnames(table_temp))]
  Real_Yes_name<-colnames(table_temp)[grepl("_Yes",colnames(table_temp))]
  Model_No_name<-rownames(table_temp)[grepl("_No",rownames(table_temp))]
  Model_Yes_name<-rownames(table_temp)[grepl("_Yes",rownames(table_temp))]
  
  TP<-table_temp[Model_Yes_name,Real_Yes_name]
  FP<-table_temp[Model_Yes_name,Real_No_name]
  TN<-table_temp[Model_No_name,Real_No_name]
  FN<-table_temp[Model_No_name,Real_Yes_name]
  
  precision <-(TP/(TP+FP))
  recall<-(TP/(TP+FN))
  F1 <- (2 * precision * recall) / (precision + recall)
  Work_reduced<-(TN+FN)/(TP+FP+TN+FN)
  
  # print(paste0("TP: ",TP,"; FP: ",FP,"; TN: ",TN,"; FN: ",FN))
  # print(paste0("precision: ",precision))
  # print(paste0("recall: ",recall))
  # print(paste0("F1: ",F1))
  return(data.frame(TP=TP,FP=FP,TN=TN,FN=FN,precision=precision,recall=recall,F1=F1,Work_reduced=Work_reduced,label=label))
}
output_DM<-data.frame(TP="TP",FP="FP",TN="TN",FN="FN",precision="precision",recall="recall",F1="F1",Work_reduced="Work_reduced",label="label")
#### DM_species ####
#GPT35
species_contingency_tb<-cal_contingency_tb(data=input_dataset,colume1="ResercherLabeled_Species",colume2="GPT35_species",na.ignor=TRUE)
species_contingency_tb
output_DM<-rbind(output_DM,metrics(table = species_contingency_tb,label = "species_GPT35"))
#GPT4
species_contingency_tb<-cal_contingency_tb(data=input_dataset,colume1="ResercherLabeled_Species",colume2="GPT4_species",na.ignor=TRUE)
species_contingency_tb
output_DM<-rbind(output_DM,metrics(table = species_contingency_tb,label = "species_GPT4"))

#Random classifier
temp_Random_output_DM<-data.frame(TP="TP",FP="FP",TN="TN",FN="FN",precision="precision",recall="recall",F1="F1",Work_reduced="Work_reduced",label="label")
for (index in 1:100) {
  input_dataset$random_col<-sample(c("Yes", "No"), nrow(input_dataset), replace=TRUE)
  contingency_tb<-cal_contingency_tb(data=input_dataset,colume1="ResercherLabeled_Species",colume2="random_col",na.ignor=TRUE)
  temp_Random_output_DM<-rbind(temp_Random_output_DM,metrics(table = contingency_tb,label = index))
}
temp_Random_output_DM<-temp_Random_output_DM[-1,]
temp_Random_output_DM<-apply(temp_Random_output_DM, 2, as.numeric)
temp_Random_output_DM<-apply(temp_Random_output_DM, 2, function(x){mean(x,na.rm=TRUE)})
temp_Random_output_DM<-as.data.frame(t(temp_Random_output_DM))
temp_Random_output_DM$label<-"species_Random_classifier"
output_DM<-rbind(output_DM,temp_Random_output_DM)

#### DM_research_design ####
#GPT35
disease_contingency_tb<-cal_contingency_tb(data=input_dataset,colume1="ResercherLabeled_research_design",colume2="GPT35_research_type",na.ignor=TRUE)
disease_contingency_tb
output_DM<-rbind(output_DM,metrics(table = disease_contingency_tb,label = "research_type_GPT35"))
#GPT4
disease_contingency_tb<-cal_contingency_tb(data=input_dataset,colume1="ResercherLabeled_research_design",colume2="GPT4_research_design",na.ignor=TRUE)
disease_contingency_tb
output_DM<-rbind(output_DM,metrics(table = disease_contingency_tb,label = "research_type_GPT4"))

#Random classifier
temp_Random_output_DM<-data.frame(TP="TP",FP="FP",TN="TN",FN="FN",precision="precision",recall="recall",F1="F1",Work_reduced="Work_reduced",label="label")
for (index in 1:100) {
  input_dataset$random_col<-sample(c("Yes", "No"), nrow(input_dataset), replace=TRUE)
  contingency_tb<-cal_contingency_tb(data=input_dataset,colume1="ResercherLabeled_research_design",colume2="random_col",na.ignor=TRUE)
  temp_Random_output_DM<-rbind(temp_Random_output_DM,metrics(table = contingency_tb,label = index))
}
temp_Random_output_DM<-temp_Random_output_DM[-1,]
temp_Random_output_DM<-apply(temp_Random_output_DM, 2, as.numeric)
temp_Random_output_DM<-apply(temp_Random_output_DM, 2, function(x){mean(x,na.rm=TRUE)})
temp_Random_output_DM<-as.data.frame(t(temp_Random_output_DM))
temp_Random_output_DM$label<-"research_design_Random_classifier"
output_DM<-rbind(output_DM,temp_Random_output_DM)


#### DM_disease_dm ####
#GPT35
treatment_contingency_tb<-cal_contingency_tb(data=input_dataset,colume1="ResercherLabeled_disease_md",colume2="GPT35_disease_dm",na.ignor=TRUE)
treatment_contingency_tb
output_DM<-rbind(output_DM,metrics(table = treatment_contingency_tb,label = "disease_md_GPT35"))
#GPT4
treatment_contingency_tb<-cal_contingency_tb(data=input_dataset,colume1="ResercherLabeled_disease_md",colume2="GPT4_disease_m",na.ignor=TRUE)
treatment_contingency_tb
output_DM<-rbind(output_DM,metrics(table = treatment_contingency_tb,label = "disease_md_GPT4"))

#Random classifier
temp_Random_output_DM<-data.frame(TP="TP",FP="FP",TN="TN",FN="FN",precision="precision",recall="recall",F1="F1",Work_reduced="Work_reduced",label="label")
for (index in 1:100) {
  input_dataset$random_col<-sample(c("Yes", "No"), nrow(input_dataset), replace=TRUE)
  contingency_tb<-cal_contingency_tb(data=input_dataset,colume1="ResercherLabeled_disease_md",colume2="random_col",na.ignor=TRUE)
  temp_Random_output_DM<-rbind(temp_Random_output_DM,metrics(table = contingency_tb,label = index))
}
temp_Random_output_DM<-temp_Random_output_DM[-1,]
temp_Random_output_DM<-apply(temp_Random_output_DM, 2, as.numeric)
temp_Random_output_DM<-apply(temp_Random_output_DM, 2, function(x){mean(x,na.rm=TRUE)})
temp_Random_output_DM<-as.data.frame(t(temp_Random_output_DM))
temp_Random_output_DM$label<-"disease_dm_Random_classifier"
output_DM<-rbind(output_DM,temp_Random_output_DM)

#### DM_disease_p ####
#GPT35
research_type_contingency_tb<-cal_contingency_tb(data=input_dataset,colume1="ResercherLabeled_disease_p",colume2="GPT35_disease_p",na.ignor=TRUE)
research_type_contingency_tb
output_DM<-rbind(output_DM,metrics(table = research_type_contingency_tb,label = "disease_p_GPT35"))
#GPT4
research_type_contingency_tb<-cal_contingency_tb(data=input_dataset,colume1="ResercherLabeled_disease_p",colume2="GPT4_disease_p",na.ignor=TRUE)
research_type_contingency_tb
output_DM<-rbind(output_DM,metrics(table = research_type_contingency_tb,label = "disease_p_GPT4"))

#Random classifier
temp_Random_output_DM<-data.frame(TP="TP",FP="FP",TN="TN",FN="FN",precision="precision",recall="recall",F1="F1",Work_reduced="Work_reduced",label="label")
for (index in 1:100) {
  input_dataset$random_col<-sample(c("Yes", "No"), nrow(input_dataset), replace=TRUE)
  contingency_tb<-cal_contingency_tb(data=input_dataset,colume1="ResercherLabeled_disease_p",colume2="random_col",na.ignor=TRUE)
  temp_Random_output_DM<-rbind(temp_Random_output_DM,metrics(table = contingency_tb,label = index))
}
temp_Random_output_DM<-temp_Random_output_DM[-1,]
temp_Random_output_DM<-apply(temp_Random_output_DM, 2, as.numeric)
temp_Random_output_DM<-apply(temp_Random_output_DM, 2, function(x){mean(x,na.rm=TRUE)})
temp_Random_output_DM<-as.data.frame(t(temp_Random_output_DM))
temp_Random_output_DM$label<-"disease_p_Random_classifier"
output_DM<-rbind(output_DM,temp_Random_output_DM)




output_DM<-output_DM[-1,]
write.table(output_DM,file = paste0(output_dir, "\\", "DM_single_question_evaluation.txt"), sep = "\t")



#### Sarcopenia ####
set.seed(1234)
metrics<-function(table,label="label"){
  table_temp<-table
  Real_No_name<-colnames(table_temp)[grepl("_No",colnames(table_temp))]
  Real_Yes_name<-colnames(table_temp)[grepl("_Yes",colnames(table_temp))]
  Model_No_name<-rownames(table_temp)[grepl("_No",rownames(table_temp))]
  Model_Yes_name<-rownames(table_temp)[grepl("_Yes",rownames(table_temp))]
  
  TP<-table_temp[Model_Yes_name,Real_Yes_name]
  FP<-table_temp[Model_Yes_name,Real_No_name]
  TN<-table_temp[Model_No_name,Real_No_name]
  FN<-table_temp[Model_No_name,Real_Yes_name]
  
  precision <-(TP/(TP+FP))
  recall<-(TP/(TP+FN))
  F1 <- (2 * precision * recall) / (precision + recall)
  Work_reduced<-(TN+FN)/(TP+FP+TN+FN)
  
  # print(paste0("TP: ",TP,"; FP: ",FP,"; TN: ",TN,"; FN: ",FN))
  # print(paste0("precision: ",precision))
  # print(paste0("recall: ",recall))
  # print(paste0("F1: ",F1))
  return(data.frame(TP=TP,FP=FP,TN=TN,FN=FN,precision=precision,recall=recall,F1=F1,Work_reduced=Work_reduced,label=label))
}
output_Sarcopenia<-data.frame(TP="TP",FP="FP",TN="TN",FN="FN",precision="precision",recall="recall",F1="F1",Work_reduced="Work_reduced",label="label")
#### Sarcopenia_species ####
#GPT35
species_contingency_tb<-cal_contingency_tb(data=input_dataset,colume1="ResercherLabeled_Species",colume2="GPT35_species",na.ignor=TRUE)
species_contingency_tb
output_Sarcopenia<-rbind(output_Sarcopenia,metrics(table = species_contingency_tb,label = "species_GPT35"))
#GPT4
species_contingency_tb<-cal_contingency_tb(data=input_dataset,colume1="ResercherLabeled_Species",colume2="GPT4_species",na.ignor=TRUE)
species_contingency_tb
output_Sarcopenia<-rbind(output_Sarcopenia,metrics(table = species_contingency_tb,label = "species_GPT4"))

#Random classifier
temp_Random_output_Sarcopenia<-data.frame(TP="TP",FP="FP",TN="TN",FN="FN",precision="precision",recall="recall",F1="F1",Work_reduced="Work_reduced",label="label")
for (index in 1:100) {
  input_dataset$random_col<-sample(c("Yes", "No"), nrow(input_dataset), replace=TRUE)
  contingency_tb<-cal_contingency_tb(data=input_dataset,colume1="ResercherLabeled_Species",colume2="random_col",na.ignor=TRUE)
  temp_Random_output_Sarcopenia<-rbind(temp_Random_output_Sarcopenia,metrics(table = contingency_tb,label = index))
}
temp_Random_output_Sarcopenia<-temp_Random_output_Sarcopenia[-1,]
temp_Random_output_Sarcopenia<-apply(temp_Random_output_Sarcopenia, 2, as.numeric)
temp_Random_output_Sarcopenia<-apply(temp_Random_output_Sarcopenia, 2, function(x){mean(x,na.rm=TRUE)})
temp_Random_output_Sarcopenia<-as.data.frame(t(temp_Random_output_Sarcopenia))
temp_Random_output_Sarcopenia$label<-"species_Random_classifier"
output_Sarcopenia<-rbind(output_Sarcopenia,temp_Random_output_Sarcopenia)

#### Sarcopenia_disease ####
#GPT35
disease_contingency_tb<-cal_contingency_tb(data=input_dataset,colume1="ResercherLabeled_disease",colume2="GPT35_disease",na.ignor=TRUE)
disease_contingency_tb
output_Sarcopenia<-rbind(output_Sarcopenia,metrics(table = disease_contingency_tb,label = "disease_GPT35"))
#GPT4
disease_contingency_tb<-cal_contingency_tb(data=input_dataset,colume1="ResercherLabeled_disease",colume2="GPT4_disease",na.ignor=TRUE)
disease_contingency_tb
output_Sarcopenia<-rbind(output_Sarcopenia,metrics(table = disease_contingency_tb,label = "disease_GPT4"))

#Random classifier
temp_Random_output_Sarcopenia<-data.frame(TP="TP",FP="FP",TN="TN",FN="FN",precision="precision",recall="recall",F1="F1",Work_reduced="Work_reduced",label="label")
for (index in 1:100) {
  input_dataset$random_col<-sample(c("Yes", "No"), nrow(input_dataset), replace=TRUE)
  contingency_tb<-cal_contingency_tb(data=input_dataset,colume1="ResercherLabeled_disease",colume2="random_col",na.ignor=TRUE)
  temp_Random_output_Sarcopenia<-rbind(temp_Random_output_Sarcopenia,metrics(table = contingency_tb,label = index))
}
temp_Random_output_Sarcopenia<-temp_Random_output_Sarcopenia[-1,]
temp_Random_output_Sarcopenia<-apply(temp_Random_output_Sarcopenia, 2, as.numeric)
temp_Random_output_Sarcopenia<-apply(temp_Random_output_Sarcopenia, 2, function(x){mean(x,na.rm=TRUE)})
temp_Random_output_Sarcopenia<-as.data.frame(t(temp_Random_output_Sarcopenia))
temp_Random_output_Sarcopenia$label<-"disease_Random_classifier"
output_Sarcopenia<-rbind(output_Sarcopenia,temp_Random_output_Sarcopenia)

#### Sarcopenia_control ####
#GPT35
control_contingency_tb<-cal_contingency_tb(data=input_dataset,colume1="ResercherLabeled_control",colume2="GPT35_control",na.ignor=TRUE)
control_contingency_tb
output_Sarcopenia<-rbind(output_Sarcopenia,metrics(table = control_contingency_tb,label = "control_GPT35"))
#GPT4
control_contingency_tb<-cal_contingency_tb(data=input_dataset,colume1="ResercherLabeled_control",colume2="GPT4_control",na.ignor=TRUE)
control_contingency_tb
output_Sarcopenia<-rbind(output_Sarcopenia,metrics(table = control_contingency_tb,label = "control_GPT4"))

#Random classifier
temp_Random_output_Sarcopenia<-data.frame(TP="TP",FP="FP",TN="TN",FN="FN",precision="precision",recall="recall",F1="F1",Work_reduced="Work_reduced",label="label")
for (index in 1:100) {
  input_dataset$random_col<-sample(c("Yes", "No"), nrow(input_dataset), replace=TRUE)
  contingency_tb<-cal_contingency_tb(data=input_dataset,colume1="ResercherLabeled_control",colume2="random_col",na.ignor=TRUE)
  temp_Random_output_Sarcopenia<-rbind(temp_Random_output_Sarcopenia,metrics(table = contingency_tb,label = index))
}
temp_Random_output_Sarcopenia<-temp_Random_output_Sarcopenia[-1,]
temp_Random_output_Sarcopenia<-apply(temp_Random_output_Sarcopenia, 2, as.numeric)
temp_Random_output_Sarcopenia<-apply(temp_Random_output_Sarcopenia, 2, function(x){mean(x,na.rm=TRUE)})
temp_Random_output_Sarcopenia<-as.data.frame(t(temp_Random_output_Sarcopenia))
temp_Random_output_Sarcopenia$label<-"control_Random_classifier"
output_Sarcopenia<-rbind(output_Sarcopenia,temp_Random_output_Sarcopenia)


#### Sarcopenia_research_type ####
#GPT35
research_type_contingency_tb<-cal_contingency_tb(data=input_dataset,colume1="ResercherLabeled_research_design",colume2="GPT35_research_type",na.ignor=TRUE)
research_type_contingency_tb
output_Sarcopenia<-rbind(output_Sarcopenia,metrics(table = research_type_contingency_tb,label = "research_type_GPT35"))
#GPT4
research_type_contingency_tb<-cal_contingency_tb(data=input_dataset,colume1="ResercherLabeled_research_design",colume2="GPT4_research_type",na.ignor=TRUE)
research_type_contingency_tb
output_Sarcopenia<-rbind(output_Sarcopenia,metrics(table = research_type_contingency_tb,label = "research_type_GPT4"))

#Random classifier
temp_Random_output_Sarcopenia<-data.frame(TP="TP",FP="FP",TN="TN",FN="FN",precision="precision",recall="recall",F1="F1",Work_reduced="Work_reduced",label="label")
for (index in 1:100) {
  input_dataset$random_col<-sample(c("Yes", "No"), nrow(input_dataset), replace=TRUE)
  contingency_tb<-cal_contingency_tb(data=input_dataset,colume1="ResercherLabeled_research_design",colume2="random_col",na.ignor=TRUE)
  temp_Random_output_Sarcopenia<-rbind(temp_Random_output_Sarcopenia,metrics(table = contingency_tb,label = index))
}
temp_Random_output_Sarcopenia<-temp_Random_output_Sarcopenia[-1,]
temp_Random_output_Sarcopenia<-apply(temp_Random_output_Sarcopenia, 2, as.numeric)
temp_Random_output_Sarcopenia<-apply(temp_Random_output_Sarcopenia, 2, function(x){mean(x,na.rm=TRUE)})
temp_Random_output_Sarcopenia<-as.data.frame(t(temp_Random_output_Sarcopenia))
temp_Random_output_Sarcopenia$label<-"research_type_Random_classifier"
output_Sarcopenia<-rbind(output_Sarcopenia,temp_Random_output_Sarcopenia)

#### Sarcopenia_outcome ####
#GPT35
outcome_contingency_tb<-cal_contingency_tb(data=input_dataset,colume1="ResercherLabeled_outcome",colume2="GPT35_outcome",na.ignor=TRUE)
outcome_contingency_tb
output_Sarcopenia<-rbind(output_Sarcopenia,metrics(table = outcome_contingency_tb,label = "outcome_GPT35"))
#GPT4
outcome_contingency_tb<-cal_contingency_tb(data=input_dataset,colume1="ResercherLabeled_outcome",colume2="GPT4_outcome",na.ignor=TRUE)
outcome_contingency_tb
output_Sarcopenia<-rbind(output_Sarcopenia,metrics(table = outcome_contingency_tb,label = "outcome_GPT4"))

#Random classifier
temp_Random_output_Sarcopenia<-data.frame(TP="TP",FP="FP",TN="TN",FN="FN",precision="precision",recall="recall",F1="F1",Work_reduced="Work_reduced",label="label")
for (index in 1:100) {
  input_dataset$random_col<-sample(c("Yes", "No"), nrow(input_dataset), replace=TRUE)
  contingency_tb<-cal_contingency_tb(data=input_dataset,colume1="ResercherLabeled_outcome",colume2="random_col",na.ignor=TRUE)
  temp_Random_output_Sarcopenia<-rbind(temp_Random_output_Sarcopenia,metrics(table = contingency_tb,label = index))
}
temp_Random_output_Sarcopenia<-temp_Random_output_Sarcopenia[-1,]
temp_Random_output_Sarcopenia<-apply(temp_Random_output_Sarcopenia, 2, as.numeric)
temp_Random_output_Sarcopenia<-apply(temp_Random_output_Sarcopenia, 2, function(x){mean(x,na.rm=TRUE)})
temp_Random_output_Sarcopenia<-as.data.frame(t(temp_Random_output_Sarcopenia))
temp_Random_output_Sarcopenia$label<-"outcome_Random_classifier"
output_Sarcopenia<-rbind(output_Sarcopenia,temp_Random_output_Sarcopenia)


output_Sarcopenia<-output_Sarcopenia[-1,]
write.table(output_Sarcopenia,file = paste0(output_dir, "\\", "Sarcopenia_single_question_evaluation.txt"), sep = "\t")
