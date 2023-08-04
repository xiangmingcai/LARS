
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

#### GBM_CD8 ####
#### GBM_CD8 GPT35 ####
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
output_GBM_CD8<-data.frame(TP="TP",FP="FP",TN="TN",FN="FN",precision="precision",recall="recall",F1="F1",Work_reduced="Work_reduced",label="label")

#GPT35 GPT35_single_question_combined_strategy_Full_combination
GPT35_single_question_combined_strategy_Full_combination_contingency_tb<-cal_contingency_tb(data=input_dataset,colume1="Manual_AllYES_summary",colume2="GPT35_single_question_combined_strategy_Full_combination",na.ignor=TRUE)
GPT35_single_question_combined_strategy_Full_combination_contingency_tb
output_GBM_CD8<-rbind(output_GBM_CD8,metrics(table = GPT35_single_question_combined_strategy_Full_combination_contingency_tb,label = "GPT35_single_question_combined_strategy_Full_combination"))
#GPT35 GPT35_well_structured_combined_questions_strategy_Full_combination
GPT35_well_structured_combined_questions_strategy_Full_combination_contingency_tb<-cal_contingency_tb(data=input_dataset,colume1="Manual_AllYES_summary",colume2="GPT35_well_structured_combined_questions_strategy_Full_combination",na.ignor=TRUE)
GPT35_well_structured_combined_questions_strategy_Full_combination_contingency_tb
output_GBM_CD8<-rbind(output_GBM_CD8,metrics(table = GPT35_well_structured_combined_questions_strategy_Full_combination_contingency_tb,label = "GPT35_well_structured_combined_questions_strategy_Full_combination"))
#GPT35 GPT35_simple_combined_questions_strategy_Full_combination
GPT35_simple_combined_questions_strategy_Full_combination_contingency_tb<-cal_contingency_tb(data=input_dataset,colume1="Manual_AllYES_summary",colume2="GPT35_simple_combined_questions_strategy_Full_combination",na.ignor=TRUE)
GPT35_simple_combined_questions_strategy_Full_combination_contingency_tb
output_GBM_CD8<-rbind(output_GBM_CD8,metrics(table = GPT35_simple_combined_questions_strategy_Full_combination_contingency_tb,label = "GPT35_simple_combined_questions_strategy_Full_combination"))

#GPT35 GPT35_single_question_combined_strategy_Best_combination
GPT35_single_question_combined_strategy_Best_combination_contingency_tb<-cal_contingency_tb(data=input_dataset,colume1="Manual_AllYES_summary",colume2="GPT35_single_question_combined_strategy_Best_combination",na.ignor=TRUE)
GPT35_single_question_combined_strategy_Best_combination_contingency_tb
output_GBM_CD8<-rbind(output_GBM_CD8,metrics(table = GPT35_single_question_combined_strategy_Best_combination_contingency_tb,label = "GPT35_single_question_combined_strategy_Best_combination"))
#GPT35 GPT35_well_structured_combined_questions_strategy_Best_combination
GPT35_well_structured_combined_questions_strategy_Best_combination_contingency_tb<-cal_contingency_tb(data=input_dataset,colume1="Manual_AllYES_summary",colume2="GPT35_well_structured_combined_questions_strategy_Best_combination",na.ignor=TRUE)
GPT35_well_structured_combined_questions_strategy_Best_combination_contingency_tb
output_GBM_CD8<-rbind(output_GBM_CD8,metrics(table = GPT35_well_structured_combined_questions_strategy_Best_combination_contingency_tb,label = "GPT35_well_structured_combined_questions_strategy_Best_combination"))
#GPT35 GPT35_simple_combined_questions_strategy_Best_combination
GPT35_simple_combined_questions_strategy_Best_combination_contingency_tb<-cal_contingency_tb(data=input_dataset,colume1="Manual_AllYES_summary",colume2="GPT35_simple_combined_questions_strategy_Best_combination",na.ignor=TRUE)
GPT35_simple_combined_questions_strategy_Best_combination_contingency_tb
output_GBM_CD8<-rbind(output_GBM_CD8,metrics(table = GPT35_simple_combined_questions_strategy_Best_combination_contingency_tb,label = "GPT35_simple_combined_questions_strategy_Best_combination"))

#Random classifier
temp_Random_output_GBM_CD8<-data.frame(TP="TP",FP="FP",TN="TN",FN="FN",precision="precision",recall="recall",F1="F1",Work_reduced="Work_reduced",label="label")
for (index in 1:100) {
  input_dataset$random_col<-sample(c("Yes", "No"), nrow(input_dataset), replace=TRUE)
  contingency_tb<-cal_contingency_tb(data=input_dataset,colume1="Manual_AllYES_summary",colume2="random_col",na.ignor=TRUE)
  temp_Random_output_GBM_CD8<-rbind(temp_Random_output_GBM_CD8,metrics(table = contingency_tb,label = index))
}
temp_Random_output_GBM_CD8<-temp_Random_output_GBM_CD8[-1,]
temp_Random_output_GBM_CD8<-apply(temp_Random_output_GBM_CD8, 2, as.numeric)
temp_Random_output_GBM_CD8<-apply(temp_Random_output_GBM_CD8, 2, function(x){mean(x,na.rm=TRUE)})
temp_Random_output_GBM_CD8<-as.data.frame(t(temp_Random_output_GBM_CD8))
temp_Random_output_GBM_CD8$label<-"Random_classifier"
output_GBM_CD8<-rbind(output_GBM_CD8,temp_Random_output_GBM_CD8)

output_GBM_CD8<-output_GBM_CD8[-1,]
write.table(output_GBM_CD8,file = paste0(output_dir, "\\", "GBM_CD8_Prompt_strategy_evaluation.txt"), sep = "\t")

#### GBM_CD8 GPT4 ####
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
output_GBM_CD8<-data.frame(TP="TP",FP="FP",TN="TN",FN="FN",precision="precision",recall="recall",F1="F1",Work_reduced="Work_reduced",label="label")

#GPT4 GPT4_single_question_combined_strategy_Full_combination
GPT4_single_question_combined_strategy_Full_combination_contingency_tb<-cal_contingency_tb(data=input_dataset,colume1="Manual_AllYES_summary",colume2="GPT4_single_question_combined_strategy_Full_combination",na.ignor=TRUE)
GPT4_single_question_combined_strategy_Full_combination_contingency_tb
output_GBM_CD8<-rbind(output_GBM_CD8,metrics(table = GPT4_single_question_combined_strategy_Full_combination_contingency_tb,label = "GPT4_single_question_combined_strategy_Full_combination"))
#GPT4 GPT4_well_structured_combined_questions_strategy_Full_combination
GPT4_well_structured_combined_questions_strategy_Full_combination_contingency_tb<-cal_contingency_tb(data=input_dataset,colume1="Manual_AllYES_summary",colume2="GPT4_well_structured_combined_questions_strategy_Full_combination",na.ignor=TRUE)
GPT4_well_structured_combined_questions_strategy_Full_combination_contingency_tb
output_GBM_CD8<-rbind(output_GBM_CD8,metrics(table = GPT4_well_structured_combined_questions_strategy_Full_combination_contingency_tb,label = "GPT4_well_structured_combined_questions_strategy_Full_combination"))
#GPT4 GPT4_simple_combined_questions_strategy_Full_combination
GPT4_simple_combined_questions_strategy_Full_combination_contingency_tb<-cal_contingency_tb(data=input_dataset,colume1="Manual_AllYES_summary",colume2="GPT4_simple_combined_questions_strategy_Full_combination",na.ignor=TRUE)
GPT4_simple_combined_questions_strategy_Full_combination_contingency_tb
output_GBM_CD8<-rbind(output_GBM_CD8,metrics(table = GPT4_simple_combined_questions_strategy_Full_combination_contingency_tb,label = "GPT4_simple_combined_questions_strategy_Full_combination"))

#GPT4 GPT4_single_question_combined_strategy_Best_combination
GPT4_single_question_combined_strategy_Best_combination_contingency_tb<-cal_contingency_tb(data=input_dataset,colume1="Manual_AllYES_summary",colume2="GPT4_single_question_combined_strategy_Best_combination",na.ignor=TRUE)
GPT4_single_question_combined_strategy_Best_combination_contingency_tb
output_GBM_CD8<-rbind(output_GBM_CD8,metrics(table = GPT4_single_question_combined_strategy_Best_combination_contingency_tb,label = "GPT4_single_question_combined_strategy_Best_combination"))
#GPT4 GPT4_well_structured_combined_questions_strategy_Best_combination
GPT4_well_structured_combined_questions_strategy_Best_combination_contingency_tb<-cal_contingency_tb(data=input_dataset,colume1="Manual_AllYES_summary",colume2="GPT4_well_structured_combined_questions_strategy_Best_combination",na.ignor=TRUE)
GPT4_well_structured_combined_questions_strategy_Best_combination_contingency_tb
output_GBM_CD8<-rbind(output_GBM_CD8,metrics(table = GPT4_well_structured_combined_questions_strategy_Best_combination_contingency_tb,label = "GPT4_well_structured_combined_questions_strategy_Best_combination"))
#GPT4 GPT4_simple_combined_questions_strategy_Best_combination
GPT4_simple_combined_questions_strategy_Best_combination_contingency_tb<-cal_contingency_tb(data=input_dataset,colume1="Manual_AllYES_summary",colume2="GPT4_simple_combined_questions_strategy_Best_combination",na.ignor=TRUE)
GPT4_simple_combined_questions_strategy_Best_combination_contingency_tb
output_GBM_CD8<-rbind(output_GBM_CD8,metrics(table = GPT4_simple_combined_questions_strategy_Best_combination_contingency_tb,label = "GPT4_simple_combined_questions_strategy_Best_combination"))

#Random classifier
temp_Random_output_GBM_CD8<-data.frame(TP="TP",FP="FP",TN="TN",FN="FN",precision="precision",recall="recall",F1="F1",Work_reduced="Work_reduced",label="label")
for (index in 1:100) {
  input_dataset$random_col<-sample(c("Yes", "No"), nrow(input_dataset), replace=TRUE)
  contingency_tb<-cal_contingency_tb(data=input_dataset,colume1="Manual_AllYES_summary",colume2="random_col",na.ignor=TRUE)
  temp_Random_output_GBM_CD8<-rbind(temp_Random_output_GBM_CD8,metrics(table = contingency_tb,label = index))
}
temp_Random_output_GBM_CD8<-temp_Random_output_GBM_CD8[-1,]
temp_Random_output_GBM_CD8<-apply(temp_Random_output_GBM_CD8, 2, as.numeric)
temp_Random_output_GBM_CD8<-apply(temp_Random_output_GBM_CD8, 2, function(x){mean(x,na.rm=TRUE)})
temp_Random_output_GBM_CD8<-as.data.frame(t(temp_Random_output_GBM_CD8))
temp_Random_output_GBM_CD8$label<-"Random_classifier"
output_GBM_CD8<-rbind(output_GBM_CD8,temp_Random_output_GBM_CD8)

output_GBM_CD8<-output_GBM_CD8[-1,]
write.table(output_GBM_CD8,file = paste0(output_dir, "\\", "GBM_CD8_Prompt_strategy_evaluation.txt"), sep = "\t")

#### IBD ####
#### IBD GPT35 ####
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

#GPT35 GPT35_single_question_combined_strategy_Full_combination
GPT35_single_question_combined_strategy_Full_combination_contingency_tb<-cal_contingency_tb(data=input_dataset,colume1="Manual_AllYES_summary",colume2="GPT35_single_question_combined_strategy_Full_combination",na.ignor=TRUE)
GPT35_single_question_combined_strategy_Full_combination_contingency_tb
output_IBD<-rbind(output_IBD,metrics(table = GPT35_single_question_combined_strategy_Full_combination_contingency_tb,label = "GPT35_single_question_combined_strategy_Full_combination"))
#GPT35 GPT35_well_structured_combined_questions_strategy_Full_combination
GPT35_well_structured_combined_questions_strategy_Full_combination_contingency_tb<-cal_contingency_tb(data=input_dataset,colume1="Manual_AllYES_summary",colume2="GPT35_well_structured_combined_questions_strategy_Full_combination",na.ignor=TRUE)
GPT35_well_structured_combined_questions_strategy_Full_combination_contingency_tb
output_IBD<-rbind(output_IBD,metrics(table = GPT35_well_structured_combined_questions_strategy_Full_combination_contingency_tb,label = "GPT35_well_structured_combined_questions_strategy_Full_combination"))
#GPT35 GPT35_simple_combined_questions_strategy_Full_combination
GPT35_simple_combined_questions_strategy_Full_combination_contingency_tb<-cal_contingency_tb(data=input_dataset,colume1="Manual_AllYES_summary",colume2="GPT35_simple_combined_questions_strategy_Full_combination",na.ignor=TRUE)
GPT35_simple_combined_questions_strategy_Full_combination_contingency_tb
output_IBD<-rbind(output_IBD,metrics(table = GPT35_simple_combined_questions_strategy_Full_combination_contingency_tb,label = "GPT35_simple_combined_questions_strategy_Full_combination"))

#GPT35 GPT35_single_question_combined_strategy_Best_combination
GPT35_single_question_combined_strategy_Best_combination_contingency_tb<-cal_contingency_tb(data=input_dataset,colume1="Manual_AllYES_summary",colume2="GPT35_single_question_combined_strategy_Best_combination",na.ignor=TRUE)
GPT35_single_question_combined_strategy_Best_combination_contingency_tb
output_IBD<-rbind(output_IBD,metrics(table = GPT35_single_question_combined_strategy_Best_combination_contingency_tb,label = "GPT35_single_question_combined_strategy_Best_combination"))
#GPT35 GPT35_well_structured_combined_questions_strategy_Best_combination
GPT35_well_structured_combined_questions_strategy_Best_combination_contingency_tb<-cal_contingency_tb(data=input_dataset,colume1="Manual_AllYES_summary",colume2="GPT35_well_structured_combined_questions_strategy_Best_combination",na.ignor=TRUE)
GPT35_well_structured_combined_questions_strategy_Best_combination_contingency_tb
output_IBD<-rbind(output_IBD,metrics(table = GPT35_well_structured_combined_questions_strategy_Best_combination_contingency_tb,label = "GPT35_well_structured_combined_questions_strategy_Best_combination"))
#GPT35 GPT35_simple_combined_questions_strategy_Best_combination
GPT35_simple_combined_questions_strategy_Best_combination_contingency_tb<-cal_contingency_tb(data=input_dataset,colume1="Manual_AllYES_summary",colume2="GPT35_simple_combined_questions_strategy_Best_combination",na.ignor=TRUE)
GPT35_simple_combined_questions_strategy_Best_combination_contingency_tb
output_IBD<-rbind(output_IBD,metrics(table = GPT35_simple_combined_questions_strategy_Best_combination_contingency_tb,label = "GPT35_simple_combined_questions_strategy_Best_combination"))

#Random classifier
temp_Random_output_IBD<-data.frame(TP="TP",FP="FP",TN="TN",FN="FN",precision="precision",recall="recall",F1="F1",Work_reduced="Work_reduced",label="label")
for (index in 1:100) {
  input_dataset$random_col<-sample(c("Yes", "No"), nrow(input_dataset), replace=TRUE)
  contingency_tb<-cal_contingency_tb(data=input_dataset,colume1="Manual_AllYES_summary",colume2="random_col",na.ignor=TRUE)
  temp_Random_output_IBD<-rbind(temp_Random_output_IBD,metrics(table = contingency_tb,label = index))
}
temp_Random_output_IBD<-temp_Random_output_IBD[-1,]
temp_Random_output_IBD<-apply(temp_Random_output_IBD, 2, as.numeric)
temp_Random_output_IBD<-apply(temp_Random_output_IBD, 2, function(x){mean(x,na.rm=TRUE)})
temp_Random_output_IBD<-as.data.frame(t(temp_Random_output_IBD))
temp_Random_output_IBD$label<-"Random_classifier"
output_IBD<-rbind(output_IBD,temp_Random_output_IBD)

output_IBD<-output_IBD[-1,]
write.table(output_IBD,file = paste0(output_dir, "\\", "IBD_Prompt_strategy_evaluation.txt"), sep = "\t")

#### IBD GPT4 ####
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

#GPT4 GPT4_single_question_combined_strategy_Full_combination
GPT4_single_question_combined_strategy_Full_combination_contingency_tb<-cal_contingency_tb(data=input_dataset,colume1="Manual_AllYES_summary",colume2="GPT4_single_question_combined_strategy_Full_combination",na.ignor=TRUE)
GPT4_single_question_combined_strategy_Full_combination_contingency_tb
output_IBD<-rbind(output_IBD,metrics(table = GPT4_single_question_combined_strategy_Full_combination_contingency_tb,label = "GPT4_single_question_combined_strategy_Full_combination"))
#GPT4 GPT4_well_structured_combined_questions_strategy_Full_combination
GPT4_well_structured_combined_questions_strategy_Full_combination_contingency_tb<-cal_contingency_tb(data=input_dataset,colume1="Manual_AllYES_summary",colume2="GPT4_well_structured_combined_questions_strategy_Full_combination",na.ignor=TRUE)
GPT4_well_structured_combined_questions_strategy_Full_combination_contingency_tb
output_IBD<-rbind(output_IBD,metrics(table = GPT4_well_structured_combined_questions_strategy_Full_combination_contingency_tb,label = "GPT4_well_structured_combined_questions_strategy_Full_combination"))
#GPT4 GPT4_simple_combined_questions_strategy_Full_combination
GPT4_simple_combined_questions_strategy_Full_combination_contingency_tb<-cal_contingency_tb(data=input_dataset,colume1="Manual_AllYES_summary",colume2="GPT4_simple_combined_questions_strategy_Full_combination",na.ignor=TRUE)
GPT4_simple_combined_questions_strategy_Full_combination_contingency_tb
output_IBD<-rbind(output_IBD,metrics(table = GPT4_simple_combined_questions_strategy_Full_combination_contingency_tb,label = "GPT4_simple_combined_questions_strategy_Full_combination"))

#GPT4 GPT4_single_question_combined_strategy_Best_combination
GPT4_single_question_combined_strategy_Best_combination_contingency_tb<-cal_contingency_tb(data=input_dataset,colume1="Manual_AllYES_summary",colume2="GPT4_single_question_combined_strategy_Best_combination",na.ignor=TRUE)
GPT4_single_question_combined_strategy_Best_combination_contingency_tb
output_IBD<-rbind(output_IBD,metrics(table = GPT4_single_question_combined_strategy_Best_combination_contingency_tb,label = "GPT4_single_question_combined_strategy_Best_combination"))
#GPT4 GPT4_well_structured_combined_questions_strategy_Best_combination
GPT4_well_structured_combined_questions_strategy_Best_combination_contingency_tb<-cal_contingency_tb(data=input_dataset,colume1="Manual_AllYES_summary",colume2="GPT4_well_structured_combined_questions_strategy_Best_combination",na.ignor=TRUE)
GPT4_well_structured_combined_questions_strategy_Best_combination_contingency_tb
output_IBD<-rbind(output_IBD,metrics(table = GPT4_well_structured_combined_questions_strategy_Best_combination_contingency_tb,label = "GPT4_well_structured_combined_questions_strategy_Best_combination"))
#GPT4 GPT4_simple_combined_questions_strategy_Best_combination
GPT4_simple_combined_questions_strategy_Best_combination_contingency_tb<-cal_contingency_tb(data=input_dataset,colume1="Manual_AllYES_summary",colume2="GPT4_simple_combined_questions_strategy_Best_combination",na.ignor=TRUE)
GPT4_simple_combined_questions_strategy_Best_combination_contingency_tb
output_IBD<-rbind(output_IBD,metrics(table = GPT4_simple_combined_questions_strategy_Best_combination_contingency_tb,label = "GPT4_simple_combined_questions_strategy_Best_combination"))

#Random classifier
temp_Random_output_IBD<-data.frame(TP="TP",FP="FP",TN="TN",FN="FN",precision="precision",recall="recall",F1="F1",Work_reduced="Work_reduced",label="label")
for (index in 1:100) {
  input_dataset$random_col<-sample(c("Yes", "No"), nrow(input_dataset), replace=TRUE)
  contingency_tb<-cal_contingency_tb(data=input_dataset,colume1="Manual_AllYES_summary",colume2="random_col",na.ignor=TRUE)
  temp_Random_output_IBD<-rbind(temp_Random_output_IBD,metrics(table = contingency_tb,label = index))
}
temp_Random_output_IBD<-temp_Random_output_IBD[-1,]
temp_Random_output_IBD<-apply(temp_Random_output_IBD, 2, as.numeric)
temp_Random_output_IBD<-apply(temp_Random_output_IBD, 2, function(x){mean(x,na.rm=TRUE)})
temp_Random_output_IBD<-as.data.frame(t(temp_Random_output_IBD))
temp_Random_output_IBD$label<-"Random_classifier"
output_IBD<-rbind(output_IBD,temp_Random_output_IBD)

output_IBD<-output_IBD[-1,]
write.table(output_IBD,file = paste0(output_dir, "\\", "IBD_Prompt_strategy_evaluation.txt"), sep = "\t")


#### DM GPT35 ####
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

#GPT35 GPT35_single_question_combined_strategy_Full_combination
GPT35_single_question_combined_strategy_Full_combination_contingency_tb<-cal_contingency_tb(data=input_dataset,colume1="Manual_AllYES_summary",colume2="GPT35_single_question_combined_strategy_Full_combination",na.ignor=TRUE)
GPT35_single_question_combined_strategy_Full_combination_contingency_tb
output_DM<-rbind(output_DM,metrics(table = GPT35_single_question_combined_strategy_Full_combination_contingency_tb,label = "GPT35_single_question_combined_strategy_Full_combination"))
#GPT35 GPT35_well_structured_combined_questions_strategy_Full_combination
GPT35_well_structured_combined_questions_strategy_Full_combination_contingency_tb<-cal_contingency_tb(data=input_dataset,colume1="Manual_AllYES_summary",colume2="GPT35_well_structured_combined_questions_strategy_Full_combination",na.ignor=TRUE)
GPT35_well_structured_combined_questions_strategy_Full_combination_contingency_tb
output_DM<-rbind(output_DM,metrics(table = GPT35_well_structured_combined_questions_strategy_Full_combination_contingency_tb,label = "GPT35_well_structured_combined_questions_strategy_Full_combination"))
#GPT35 GPT35_simple_combined_questions_strategy_Full_combination
GPT35_simple_combined_questions_strategy_Full_combination_contingency_tb<-cal_contingency_tb(data=input_dataset,colume1="Manual_AllYES_summary",colume2="GPT35_simple_combined_questions_strategy_Full_combination",na.ignor=TRUE)
GPT35_simple_combined_questions_strategy_Full_combination_contingency_tb
output_DM<-rbind(output_DM,metrics(table = GPT35_simple_combined_questions_strategy_Full_combination_contingency_tb,label = "GPT35_simple_combined_questions_strategy_Full_combination"))

#GPT35 GPT35_single_question_combined_strategy_Best_combination
GPT35_single_question_combined_strategy_Best_combination_contingency_tb<-cal_contingency_tb(data=input_dataset,colume1="Manual_AllYES_summary",colume2="GPT35_single_question_combined_strategy_Best_combination",na.ignor=TRUE)
GPT35_single_question_combined_strategy_Best_combination_contingency_tb
output_DM<-rbind(output_DM,metrics(table = GPT35_single_question_combined_strategy_Best_combination_contingency_tb,label = "GPT35_single_question_combined_strategy_Best_combination"))
#GPT35 GPT35_well_structured_combined_questions_strategy_Best_combination
GPT35_well_structured_combined_questions_strategy_Best_combination_contingency_tb<-cal_contingency_tb(data=input_dataset,colume1="Manual_AllYES_summary",colume2="GPT35_well_structured_combined_questions_strategy_Best_combination",na.ignor=TRUE)
GPT35_well_structured_combined_questions_strategy_Best_combination_contingency_tb
output_DM<-rbind(output_DM,metrics(table = GPT35_well_structured_combined_questions_strategy_Best_combination_contingency_tb,label = "GPT35_well_structured_combined_questions_strategy_Best_combination"))
#GPT35 GPT35_simple_combined_questions_strategy_Best_combination
GPT35_simple_combined_questions_strategy_Best_combination_contingency_tb<-cal_contingency_tb(data=input_dataset,colume1="Manual_AllYES_summary",colume2="GPT35_simple_combined_questions_strategy_Best_combination",na.ignor=TRUE)
GPT35_simple_combined_questions_strategy_Best_combination_contingency_tb
output_DM<-rbind(output_DM,metrics(table = GPT35_simple_combined_questions_strategy_Best_combination_contingency_tb,label = "GPT35_simple_combined_questions_strategy_Best_combination"))

#Random classifier
temp_Random_output_DM<-data.frame(TP="TP",FP="FP",TN="TN",FN="FN",precision="precision",recall="recall",F1="F1",Work_reduced="Work_reduced",label="label")
for (index in 1:100) {
  input_dataset$random_col<-sample(c("Yes", "No"), nrow(input_dataset), replace=TRUE)
  contingency_tb<-cal_contingency_tb(data=input_dataset,colume1="Manual_AllYES_summary",colume2="random_col",na.ignor=TRUE)
  temp_Random_output_DM<-rbind(temp_Random_output_DM,metrics(table = contingency_tb,label = index))
}
temp_Random_output_DM<-temp_Random_output_DM[-1,]
temp_Random_output_DM<-apply(temp_Random_output_DM, 2, as.numeric)
temp_Random_output_DM<-apply(temp_Random_output_DM, 2, function(x){mean(x,na.rm=TRUE)})
temp_Random_output_DM<-as.data.frame(t(temp_Random_output_DM))
temp_Random_output_DM$label<-"Random_classifier"
output_DM<-rbind(output_DM,temp_Random_output_DM)

output_DM<-output_DM[-1,]
write.table(output_DM,file = paste0(output_dir, "\\", "DM_Prompt_strategy_evaluation.txt"), sep = "\t")

#### DM GPT4 ####
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

#GPT4 GPT4_single_question_combined_strategy_Full_combination
GPT4_single_question_combined_strategy_Full_combination_contingency_tb<-cal_contingency_tb(data=input_dataset,colume1="Manual_AllYES_summary",colume2="GPT4_single_question_combined_strategy_Full_combination",na.ignor=TRUE)
GPT4_single_question_combined_strategy_Full_combination_contingency_tb
output_DM<-rbind(output_DM,metrics(table = GPT4_single_question_combined_strategy_Full_combination_contingency_tb,label = "GPT4_single_question_combined_strategy_Full_combination"))
#GPT4 GPT4_well_structured_combined_questions_strategy_Full_combination
GPT4_well_structured_combined_questions_strategy_Full_combination_contingency_tb<-cal_contingency_tb(data=input_dataset,colume1="Manual_AllYES_summary",colume2="GPT4_well_structured_combined_questions_strategy_Full_combination",na.ignor=TRUE)
GPT4_well_structured_combined_questions_strategy_Full_combination_contingency_tb
output_DM<-rbind(output_DM,metrics(table = GPT4_well_structured_combined_questions_strategy_Full_combination_contingency_tb,label = "GPT4_well_structured_combined_questions_strategy_Full_combination"))
#GPT4 GPT4_simple_combined_questions_strategy_Full_combination
GPT4_simple_combined_questions_strategy_Full_combination_contingency_tb<-cal_contingency_tb(data=input_dataset,colume1="Manual_AllYES_summary",colume2="GPT4_simple_combined_questions_strategy_Full_combination",na.ignor=TRUE)
GPT4_simple_combined_questions_strategy_Full_combination_contingency_tb
output_DM<-rbind(output_DM,metrics(table = GPT4_simple_combined_questions_strategy_Full_combination_contingency_tb,label = "GPT4_simple_combined_questions_strategy_Full_combination"))

#GPT4 GPT4_single_question_combined_strategy_Best_combination
GPT4_single_question_combined_strategy_Best_combination_contingency_tb<-cal_contingency_tb(data=input_dataset,colume1="Manual_AllYES_summary",colume2="GPT4_single_question_combined_strategy_Best_combination",na.ignor=TRUE)
GPT4_single_question_combined_strategy_Best_combination_contingency_tb
output_DM<-rbind(output_DM,metrics(table = GPT4_single_question_combined_strategy_Best_combination_contingency_tb,label = "GPT4_single_question_combined_strategy_Best_combination"))
#GPT4 GPT4_well_structured_combined_questions_strategy_Best_combination
GPT4_well_structured_combined_questions_strategy_Best_combination_contingency_tb<-cal_contingency_tb(data=input_dataset,colume1="Manual_AllYES_summary",colume2="GPT4_well_structured_combined_questions_strategy_Best_combination",na.ignor=TRUE)
GPT4_well_structured_combined_questions_strategy_Best_combination_contingency_tb
output_DM<-rbind(output_DM,metrics(table = GPT4_well_structured_combined_questions_strategy_Best_combination_contingency_tb,label = "GPT4_well_structured_combined_questions_strategy_Best_combination"))
#GPT4 GPT4_simple_combined_questions_strategy_Best_combination
GPT4_simple_combined_questions_strategy_Best_combination_contingency_tb<-cal_contingency_tb(data=input_dataset,colume1="Manual_AllYES_summary",colume2="GPT4_simple_combined_questions_strategy_Best_combination",na.ignor=TRUE)
GPT4_simple_combined_questions_strategy_Best_combination_contingency_tb
output_DM<-rbind(output_DM,metrics(table = GPT4_simple_combined_questions_strategy_Best_combination_contingency_tb,label = "GPT4_simple_combined_questions_strategy_Best_combination"))

#Random classifier
temp_Random_output_DM<-data.frame(TP="TP",FP="FP",TN="TN",FN="FN",precision="precision",recall="recall",F1="F1",Work_reduced="Work_reduced",label="label")
for (index in 1:100) {
  input_dataset$random_col<-sample(c("Yes", "No"), nrow(input_dataset), replace=TRUE)
  contingency_tb<-cal_contingency_tb(data=input_dataset,colume1="Manual_AllYES_summary",colume2="random_col",na.ignor=TRUE)
  temp_Random_output_DM<-rbind(temp_Random_output_DM,metrics(table = contingency_tb,label = index))
}
temp_Random_output_DM<-temp_Random_output_DM[-1,]
temp_Random_output_DM<-apply(temp_Random_output_DM, 2, as.numeric)
temp_Random_output_DM<-apply(temp_Random_output_DM, 2, function(x){mean(x,na.rm=TRUE)})
temp_Random_output_DM<-as.data.frame(t(temp_Random_output_DM))
temp_Random_output_DM$label<-"Random_classifier"
output_DM<-rbind(output_DM,temp_Random_output_DM)

output_DM<-output_DM[-1,]
write.table(output_DM,file = paste0(output_dir, "\\", "DM_Prompt_strategy_evaluation.txt"), sep = "\t")


#### Sarcopenia GPT35 ####
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

#GPT35 GPT35_single_question_combined_strategy_Full_combination
GPT35_single_question_combined_strategy_Full_combination_contingency_tb<-cal_contingency_tb(data=input_dataset,colume1="Manual_AllYES_summary",colume2="GPT35_single_question_combined_strategy_Full_combination",na.ignor=TRUE)
GPT35_single_question_combined_strategy_Full_combination_contingency_tb
output_Sarcopenia<-rbind(output_Sarcopenia,metrics(table = GPT35_single_question_combined_strategy_Full_combination_contingency_tb,label = "GPT35_single_question_combined_strategy_Full_combination"))
#GPT35 GPT35_well_structured_combined_questions_strategy_Full_combination
GPT35_well_structured_combined_questions_strategy_Full_combination_contingency_tb<-cal_contingency_tb(data=input_dataset,colume1="Manual_AllYES_summary",colume2="GPT35_well_structured_combined_questions_strategy_Full_combination",na.ignor=TRUE)
GPT35_well_structured_combined_questions_strategy_Full_combination_contingency_tb
output_Sarcopenia<-rbind(output_Sarcopenia,metrics(table = GPT35_well_structured_combined_questions_strategy_Full_combination_contingency_tb,label = "GPT35_well_structured_combined_questions_strategy_Full_combination"))
#GPT35 GPT35_simple_combined_questions_strategy_Full_combination
GPT35_simple_combined_questions_strategy_Full_combination_contingency_tb<-cal_contingency_tb(data=input_dataset,colume1="Manual_AllYES_summary",colume2="GPT35_simple_combined_questions_strategy_Full_combination",na.ignor=TRUE)
GPT35_simple_combined_questions_strategy_Full_combination_contingency_tb
output_Sarcopenia<-rbind(output_Sarcopenia,metrics(table = GPT35_simple_combined_questions_strategy_Full_combination_contingency_tb,label = "GPT35_simple_combined_questions_strategy_Full_combination"))

#GPT35 GPT35_single_question_combined_strategy_Best_combination
GPT35_single_question_combined_strategy_Best_combination_contingency_tb<-cal_contingency_tb(data=input_dataset,colume1="Manual_AllYES_summary",colume2="GPT35_single_question_combined_strategy_Best_combination",na.ignor=TRUE)
GPT35_single_question_combined_strategy_Best_combination_contingency_tb
output_Sarcopenia<-rbind(output_Sarcopenia,metrics(table = GPT35_single_question_combined_strategy_Best_combination_contingency_tb,label = "GPT35_single_question_combined_strategy_Best_combination"))
#GPT35 GPT35_well_structured_combined_questions_strategy_Best_combination
GPT35_well_structured_combined_questions_strategy_Best_combination_contingency_tb<-cal_contingency_tb(data=input_dataset,colume1="Manual_AllYES_summary",colume2="GPT35_well_structured_combined_questions_strategy_Best_combination",na.ignor=TRUE)
GPT35_well_structured_combined_questions_strategy_Best_combination_contingency_tb
output_Sarcopenia<-rbind(output_Sarcopenia,metrics(table = GPT35_well_structured_combined_questions_strategy_Best_combination_contingency_tb,label = "GPT35_well_structured_combined_questions_strategy_Best_combination"))
#GPT35 GPT35_simple_combined_questions_strategy_Best_combination
GPT35_simple_combined_questions_strategy_Best_combination_contingency_tb<-cal_contingency_tb(data=input_dataset,colume1="Manual_AllYES_summary",colume2="GPT35_simple_combined_questions_strategy_Best_combination",na.ignor=TRUE)
GPT35_simple_combined_questions_strategy_Best_combination_contingency_tb
output_Sarcopenia<-rbind(output_Sarcopenia,metrics(table = GPT35_simple_combined_questions_strategy_Best_combination_contingency_tb,label = "GPT35_simple_combined_questions_strategy_Best_combination"))

#Random classifier
temp_Random_output_Sarcopenia<-data.frame(TP="TP",FP="FP",TN="TN",FN="FN",precision="precision",recall="recall",F1="F1",Work_reduced="Work_reduced",label="label")
for (index in 1:100) {
  input_dataset$random_col<-sample(c("Yes", "No"), nrow(input_dataset), replace=TRUE)
  contingency_tb<-cal_contingency_tb(data=input_dataset,colume1="Manual_AllYES_summary",colume2="random_col",na.ignor=TRUE)
  temp_Random_output_Sarcopenia<-rbind(temp_Random_output_Sarcopenia,metrics(table = contingency_tb,label = index))
}
temp_Random_output_Sarcopenia<-temp_Random_output_Sarcopenia[-1,]
temp_Random_output_Sarcopenia<-apply(temp_Random_output_Sarcopenia, 2, as.numeric)
temp_Random_output_Sarcopenia<-apply(temp_Random_output_Sarcopenia, 2, function(x){mean(x,na.rm=TRUE)})
temp_Random_output_Sarcopenia<-as.data.frame(t(temp_Random_output_Sarcopenia))
temp_Random_output_Sarcopenia$label<-"Random_classifier"
output_Sarcopenia<-rbind(output_Sarcopenia,temp_Random_output_Sarcopenia)

output_Sarcopenia<-output_Sarcopenia[-1,]
write.table(output_Sarcopenia,file = paste0(output_dir, "\\", "Sarcopenia_Prompt_strategy_evaluation.txt"), sep = "\t")

#### Sarcopenia GPT4 ####
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

#GPT4 GPT4_single_question_combined_strategy_Full_combination
GPT4_single_question_combined_strategy_Full_combination_contingency_tb<-cal_contingency_tb(data=input_dataset,colume1="Manual_AllYES_summary",colume2="GPT4_single_question_combined_strategy_Full_combination",na.ignor=TRUE)
GPT4_single_question_combined_strategy_Full_combination_contingency_tb
output_Sarcopenia<-rbind(output_Sarcopenia,metrics(table = GPT4_single_question_combined_strategy_Full_combination_contingency_tb,label = "GPT4_single_question_combined_strategy_Full_combination"))
#GPT4 GPT4_well_structured_combined_questions_strategy_Full_combination
GPT4_well_structured_combined_questions_strategy_Full_combination_contingency_tb<-cal_contingency_tb(data=input_dataset,colume1="Manual_AllYES_summary",colume2="GPT4_well_structured_combined_questions_strategy_Full_combination",na.ignor=TRUE)
GPT4_well_structured_combined_questions_strategy_Full_combination_contingency_tb
output_Sarcopenia<-rbind(output_Sarcopenia,metrics(table = GPT4_well_structured_combined_questions_strategy_Full_combination_contingency_tb,label = "GPT4_well_structured_combined_questions_strategy_Full_combination"))
#GPT4 GPT4_simple_combined_questions_strategy_Full_combination
GPT4_simple_combined_questions_strategy_Full_combination_contingency_tb<-cal_contingency_tb(data=input_dataset,colume1="Manual_AllYES_summary",colume2="GPT4_simple_combined_questions_strategy_Full_combination",na.ignor=TRUE)
GPT4_simple_combined_questions_strategy_Full_combination_contingency_tb
output_Sarcopenia<-rbind(output_Sarcopenia,metrics(table = GPT4_simple_combined_questions_strategy_Full_combination_contingency_tb,label = "GPT4_simple_combined_questions_strategy_Full_combination"))

#GPT4 GPT4_single_question_combined_strategy_Best_combination
GPT4_single_question_combined_strategy_Best_combination_contingency_tb<-cal_contingency_tb(data=input_dataset,colume1="Manual_AllYES_summary",colume2="GPT4_single_question_combined_strategy_Best_combination",na.ignor=TRUE)
GPT4_single_question_combined_strategy_Best_combination_contingency_tb
output_Sarcopenia<-rbind(output_Sarcopenia,metrics(table = GPT4_single_question_combined_strategy_Best_combination_contingency_tb,label = "GPT4_single_question_combined_strategy_Best_combination"))
#GPT4 GPT4_well_structured_combined_questions_strategy_Best_combination
GPT4_well_structured_combined_questions_strategy_Best_combination_contingency_tb<-cal_contingency_tb(data=input_dataset,colume1="Manual_AllYES_summary",colume2="GPT4_well_structured_combined_questions_strategy_Best_combination",na.ignor=TRUE)
GPT4_well_structured_combined_questions_strategy_Best_combination_contingency_tb
output_Sarcopenia<-rbind(output_Sarcopenia,metrics(table = GPT4_well_structured_combined_questions_strategy_Best_combination_contingency_tb,label = "GPT4_well_structured_combined_questions_strategy_Best_combination"))
#GPT4 GPT4_simple_combined_questions_strategy_Best_combination
GPT4_simple_combined_questions_strategy_Best_combination_contingency_tb<-cal_contingency_tb(data=input_dataset,colume1="Manual_AllYES_summary",colume2="GPT4_simple_combined_questions_strategy_Best_combination",na.ignor=TRUE)
GPT4_simple_combined_questions_strategy_Best_combination_contingency_tb
output_Sarcopenia<-rbind(output_Sarcopenia,metrics(table = GPT4_simple_combined_questions_strategy_Best_combination_contingency_tb,label = "GPT4_simple_combined_questions_strategy_Best_combination"))

#Random classifier
temp_Random_output_Sarcopenia<-data.frame(TP="TP",FP="FP",TN="TN",FN="FN",precision="precision",recall="recall",F1="F1",Work_reduced="Work_reduced",label="label")
for (index in 1:100) {
  input_dataset$random_col<-sample(c("Yes", "No"), nrow(input_dataset), replace=TRUE)
  contingency_tb<-cal_contingency_tb(data=input_dataset,colume1="Manual_AllYES_summary",colume2="random_col",na.ignor=TRUE)
  temp_Random_output_Sarcopenia<-rbind(temp_Random_output_Sarcopenia,metrics(table = contingency_tb,label = index))
}
temp_Random_output_Sarcopenia<-temp_Random_output_Sarcopenia[-1,]
temp_Random_output_Sarcopenia<-apply(temp_Random_output_Sarcopenia, 2, as.numeric)
temp_Random_output_Sarcopenia<-apply(temp_Random_output_Sarcopenia, 2, function(x){mean(x,na.rm=TRUE)})
temp_Random_output_Sarcopenia<-as.data.frame(t(temp_Random_output_Sarcopenia))
temp_Random_output_Sarcopenia$label<-"Random_classifier"
output_Sarcopenia<-rbind(output_Sarcopenia,temp_Random_output_Sarcopenia)

output_Sarcopenia<-output_Sarcopenia[-1,]
write.table(output_Sarcopenia,file = paste0(output_dir, "\\", "Sarcopenia_Prompt_strategy_evaluation.txt"), sep = "\t")



#### comparison between three strategies ####
library(rstatix)
library(ggplot2)

output_dir <- choose.dir(default = "D:\\output_dir", caption = "choose the output folder")
data_dir <- choose.dir(default = "D:\\data_dir", caption = "choose the folder storing input_data.txt file")

input_dataset_path <- choose.files(default = data_dir, caption = "choose the prompt_strategies.txt file",
                                   multi = TRUE, filters = Filters,
                                   index = nrow(Filters))
input_dataset<- read.csv(input_dataset_path, header = TRUE,sep="\t", stringsAsFactors=FALSE)
colnames(input_dataset)

#Recall
shapiro.test(input_dataset$Recall)
leveneTest(input_dataset$Recall~input_dataset$Prompt.strategy, center=median) 
friedman.test(y = input_dataset$Recall, groups = input_dataset$Prompt.strategy, blocks = input_dataset$Sample)
pdf(paste0(output_dir,"//Prompt_strategy_Recall.pdf"),width=4, height=6)
p<-ggplot(input_dataset, aes(x = Prompt.strategy, y = Recall)) + 
  labs(x="Prompt strategy", title="Friedman.test p-value = 0.8338") +
  # scale_fill_manual(values = c("Prompt strategy 1" = '#1685a9',
  #                              "Prompt strategy 2" = '#d9b611',
  #                              "Prompt strategy 3" = "#8c4356")) +
  geom_boxplot(width=0.5, lwd = 0.5) +
  geom_line(aes(group=Sample), color="#3d3b4f" ,position = position_dodge(0.2)) +
  geom_point(aes(fill=Prompt.strategy, group=Sample), 
             size = 2, 
             position = position_dodge(0.2)) +
  theme_classic() +
  theme(axis.title = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        axis.text.x = element_text(size = 15,angle = 45,hjust = 1),
        axis.line = element_line(size = 1),
        axis.ticks = element_line(size = 1),
        title = element_text(size = 10),
        legend.position="none")
print(p)
dev.off()

#Work_reduced
shapiro.test(input_dataset$Work_reduced)
leveneTest(input_dataset$Work_reduced~input_dataset$Prompt.strategy, center=median) 
anova_test(data = input_dataset, dv = Work_reduced, wid = Sample, within = Prompt.strategy)

pdf(paste0(output_dir,"//Prompt_strategy_Work_reduced.pdf"),width=4, height=6)
p<-ggplot(input_dataset, aes(x = Prompt.strategy, y = Work_reduced)) + 
  labs(x="Prompt strategy", title="Anova.test p-value = 0.349") +
  # scale_fill_manual(values = c("Prompt strategy 1" = '#1685a9',
  #                              "Prompt strategy 2" = '#d9b611',
  #                              "Prompt strategy 3" = "#8c4356")) +
  geom_boxplot(width=0.5, lwd = 0.5) +
  geom_line(aes(group=Sample), color="#3d3b4f" ,position = position_dodge(0.2)) +
  geom_point(aes(fill=Prompt.strategy, group=Sample), 
             size = 2, 
             position = position_dodge(0.2)) +
  theme_classic() +
  theme(axis.title = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        axis.text.x = element_text(size = 15,angle = 45,hjust = 1),
        axis.line = element_line(size = 1),
        axis.ticks = element_line(size = 1),
        title = element_text(size = 10),
        legend.position="none")
print(p)
dev.off()

#Precision
shapiro.test(input_dataset$Precision)
leveneTest(input_dataset$Precision~input_dataset$Prompt.strategy, center=median) 
friedman.test(y = input_dataset$Precision, groups = input_dataset$Prompt.strategy, blocks = input_dataset$Sample)
pdf(paste0(output_dir,"//Prompt_strategy_Precision.pdf"),width=4, height=6)
p<-ggplot(input_dataset, aes(x = Prompt.strategy, y = Precision)) + 
  labs(x="Prompt strategy", title="Friedman.test p-value = 0.5079") +
  # scale_fill_manual(values = c("Prompt strategy 1" = '#1685a9',
  #                              "Prompt strategy 2" = '#d9b611',
  #                              "Prompt strategy 3" = "#8c4356")) +
  geom_boxplot(width=0.5, lwd = 0.5) +
  geom_line(aes(group=Sample), color="#3d3b4f" ,position = position_dodge(0.2)) +
  geom_point(aes(fill=Prompt.strategy, group=Sample), 
             size = 2, 
             position = position_dodge(0.2)) +
  theme_classic() +
  theme(axis.title = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        axis.text.x = element_text(size = 15,angle = 45,hjust = 1),
        axis.line = element_line(size = 1),
        axis.ticks = element_line(size = 1),
        title = element_text(size = 10),
        legend.position="none")
print(p)
dev.off()

#F1
shapiro.test(input_dataset$F1)
leveneTest(input_dataset$F1~input_dataset$Prompt.strategy, center=median) 
friedman.test(y = input_dataset$F1, groups = input_dataset$Prompt.strategy, blocks = input_dataset$Sample)
pdf(paste0(output_dir,"//Prompt_strategy_F1.pdf"),width=4, height=6)
p<-ggplot(input_dataset, aes(x = Prompt.strategy, y = F1)) + 
  labs(x="Prompt strategy", title="Friedman.test p-value = 0.3558") +
  # scale_fill_manual(values = c("Prompt strategy 1" = '#1685a9',
  #                              "Prompt strategy 2" = '#d9b611',
  #                              "Prompt strategy 3" = "#8c4356")) +
  geom_boxplot(width=0.5, lwd = 0.5) +
  geom_line(aes(group=Sample), color="#3d3b4f" ,position = position_dodge(0.2)) +
  geom_point(aes(fill=Prompt.strategy, group=Sample), 
             size = 2, 
             position = position_dodge(0.2)) +
  theme_classic() +
  theme(axis.title = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        axis.text.x = element_text(size = 15,angle = 45,hjust = 1),
        axis.line = element_line(size = 1),
        axis.ticks = element_line(size = 1),
        title = element_text(size = 10),
        legend.position="none")
print(p)
dev.off()


#### comparison between two GPT versions ####
library(rstatix)
library(ggplot2)
library(car)

output_dir <- choose.dir(default = "D:\\output_dir", caption = "choose the output folder")
data_dir <- choose.dir(default = "D:\\data_dir", caption = "choose the folder storing input_data.txt file")

input_dataset_path <- choose.files(default = data_dir, caption = "choose the prompt_strategies.txt file",
                                   multi = TRUE, filters = Filters,
                                   index = nrow(Filters))
input_dataset<- read.csv(input_dataset_path, header = TRUE,sep="\t", stringsAsFactors=FALSE)
colnames(input_dataset)
input_dataset$Model<-as.factor(input_dataset$Model)
input_dataset$Sample<-as.factor(input_dataset$Sample)
#Recall
shapiro.test(input_dataset$Recall)
leveneTest(input_dataset$Recall~input_dataset$Model, center=median) 
friedman.test(y = input_dataset$Recall, groups = input_dataset$Model, blocks = input_dataset$Sample2)
pdf(paste0(output_dir,"//Model_Recall.pdf"),width=4, height=6)
p<-ggplot(input_dataset, aes(x = Model, y = Recall)) + 
  labs(x="Prompt strategy", title="Friedman.test p-value = 0.5637") +
  # scale_fill_manual(values = c("Prompt strategy 1" = '#1685a9',
  #                              "Prompt strategy 2" = '#d9b611',
  #                              "Prompt strategy 3" = "#8c4356")) +
  geom_boxplot(width=0.5, lwd = 0.5) +
  geom_line(aes(group=Sample2), color="#3d3b4f" ,position = position_dodge(0.2)) +
  geom_point(aes(fill=Model, group=Sample2), 
             size = 2, 
             position = position_dodge(0.2)) +
  theme_classic() +
  theme(axis.title = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        axis.text.x = element_text(size = 15,angle = 45,hjust = 1),
        axis.line = element_line(size = 1),
        axis.ticks = element_line(size = 1),
        title = element_text(size = 10),
        legend.position="none")
print(p)
dev.off()

#Work_reduced
shapiro.test(input_dataset$Work_reduced)
leveneTest(input_dataset$Work_reduced~input_dataset$Model, center=median) 
anova_test(data = input_dataset, dv = Work_reduced, wid = Sample2, within = Model)

pdf(paste0(output_dir,"//Model_Work_reduced.pdf"),width=4, height=6)
p<-ggplot(input_dataset, aes(x = Model, y = Work_reduced)) + 
  labs(x="Prompt strategy", title="Anova.test p-value = 0.037") +
  # scale_fill_manual(values = c("Prompt strategy 1" = '#1685a9',
  #                              "Prompt strategy 2" = '#d9b611',
  #                              "Prompt strategy 3" = "#8c4356")) +
  geom_boxplot(width=0.5, lwd = 0.5) +
  geom_line(aes(group=Sample2), color="#3d3b4f" ,position = position_dodge(0.2)) +
  geom_point(aes(fill=Model, group=Sample2), 
             size = 2, 
             position = position_dodge(0.2)) +
  theme_classic() +
  theme(axis.title = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        axis.text.x = element_text(size = 15,angle = 45,hjust = 1),
        axis.line = element_line(size = 1),
        axis.ticks = element_line(size = 1),
        title = element_text(size = 10),
        legend.position="none")
print(p)
dev.off()
#Precision
shapiro.test(input_dataset$Precision)
leveneTest(input_dataset$Precision~input_dataset$Model, center=median) 
friedman.test(y = input_dataset$Precision, groups = input_dataset$Model, blocks = input_dataset$Sample2)
pdf(paste0(output_dir,"//Model_Precision.pdf"),width=4, height=6)
p<-ggplot(input_dataset, aes(x = Model, y = Precision)) + 
  labs(x="Prompt strategy", title="Friedman.test p-value = 0.5637") +
  # scale_fill_manual(values = c("Prompt strategy 1" = '#1685a9',
  #                              "Prompt strategy 2" = '#d9b611',
  #                              "Prompt strategy 3" = "#8c4356")) +
  geom_boxplot(width=0.5, lwd = 0.5) +
  geom_line(aes(group=Sample2), color="#3d3b4f" ,position = position_dodge(0.2)) +
  geom_point(aes(fill=Model, group=Sample2), 
             size = 2, 
             position = position_dodge(0.2)) +
  theme_classic() +
  theme(axis.title = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        axis.text.x = element_text(size = 15,angle = 45,hjust = 1),
        axis.line = element_line(size = 1),
        axis.ticks = element_line(size = 1),
        title = element_text(size = 10),
        legend.position="none")
print(p)
dev.off()

#F1
shapiro.test(input_dataset$F1)
leveneTest(input_dataset$F1~input_dataset$Model, center=median) 
friedman.test(y = input_dataset$F1, groups = input_dataset$Model, blocks = input_dataset$Sample2)
pdf(paste0(output_dir,"//Model_F1.pdf"),width=4, height=6)
p<-ggplot(input_dataset, aes(x = Model, y = F1)) + 
  labs(x="Prompt strategy", title="Friedman.test p-value = 0.5637") +
  # scale_fill_manual(values = c("Prompt strategy 1" = '#1685a9',
  #                              "Prompt strategy 2" = '#d9b611',
  #                              "Prompt strategy 3" = "#8c4356")) +
  geom_boxplot(width=0.5, lwd = 0.5) +
  geom_line(aes(group=Sample2), color="#3d3b4f" ,position = position_dodge(0.2)) +
  geom_point(aes(fill=Model, group=Sample2), 
             size = 2, 
             position = position_dodge(0.2)) +
  theme_classic() +
  theme(axis.title = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        axis.text.x = element_text(size = 15,angle = 45,hjust = 1),
        axis.line = element_line(size = 1),
        axis.ticks = element_line(size = 1),
        title = element_text(size = 10),
        legend.position="none")
print(p)
dev.off()
