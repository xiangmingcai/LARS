
library(stringr)
library(dbplyr)
library(ggplot2)
library(ComplexHeatmap)
library(circlize)
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
# dim(input_dataset)
input_dataset<-input_dataset[complete.cases(input_dataset),]
colnames(input_dataset)

#### CD8_GBM ####
#### CD8_GBM GPT35 ####
set.seed(1234)
colnames(input_dataset)
#01 all records
#calculate combined classification results
columes<-c("GPT35_species","GPT35_disease","GPT35_treatment","GPT35_research_type")
input_dataset<-summary_combine_calculator(data=input_dataset,columes=columes)

#calculate metrics
comp_columes<-colnames(input_dataset)[c(12:26)]
result_metrics<-metrics_calculator(data=input_dataset,comp_columes=comp_columes,manual_colume="Manual_AllYES_summary",Question="Combined")
#calculate random classifier
random_input_dataset<-input_dataset[,c("id","PMID","Manual_AllYES_summary")]
for (index in 1:100) {
  random_input_dataset[,c(paste0("random_col_",index))]<-sample(c("Yes", "No"), nrow(random_input_dataset), replace=TRUE)
}
random_comp_columes<-colnames(random_input_dataset)[c(4:103)]
random_result_metrics<-metrics_calculator(data=random_input_dataset,comp_columes=random_comp_columes,manual_colume="Manual_AllYES_summary",Question="Random")
temp_random_result_metrics<-random_result_metrics[1,]
temp_random_result_metrics$Approach<-"Random classifier"
temp_random_result_metrics[1,c("Precision","Recall","F1","Work_reduced",
                               "TN_Manual_No_Comp_No","FN_Manual_Yes_Comp_No",
                               "FP_Manual_No_Comp_Yes","TP_Manual_Yes_Comp_Yes")]<-apply(random_result_metrics[,c("Precision","Recall","F1","Work_reduced",
                                                                                                                  "TN_Manual_No_Comp_No","FN_Manual_Yes_Comp_No",
                                                                                                                  "FP_Manual_No_Comp_Yes","TP_Manual_Yes_Comp_Yes")], 2, function(x){mean(x,na.rm=TRUE)})
result_metrics<-rbind(result_metrics,temp_random_result_metrics)

write.table(result_metrics,file = paste0(output_dir, "\\", "CD8_GBM_result_metrics.txt"), sep = "\t")

#create mat matrix for upset plot
lt = list("GPT35_species" = columes,
          "GPT35_disease" = columes,
          "GPT35_treatment" = columes,
          "GPT35_research_type" = columes
)
m = make_comb_mat(lt, mode = "intersect")

#create corresponding df for upset plot
corresponding_df<-as.data.frame(m)
seq_colume_names<-rownames(as.data.frame(m))

result_metrics[,c(seq_colume_names)]<-0
for (question_index in 1:length(seq_colume_names)) {
  temp_question_name<-seq_colume_names[question_index]
  for (index_row in 1:nrow(result_metrics)) {
    if(length(grep(temp_question_name,result_metrics[index_row,"Approach"]))>0){
      result_metrics[index_row,temp_question_name]<-1
    }
  }
}

corresponding_df[c("Precision","Recall","F1","Work_reduced"),]<-0
for (index_col in 1:ncol(corresponding_df)) {
  for (index_row in 1:nrow(result_metrics)) {
    if (all(result_metrics[index_row,seq_colume_names]==corresponding_df[seq_colume_names,index_col])) {
      corresponding_df[c("Precision"),index_col]<-result_metrics[index_row,c("Precision")]
      corresponding_df[c("Recall"),index_col]<-result_metrics[index_row,c("Recall")]
      corresponding_df[c("F1"),index_col]<-result_metrics[index_row,c("F1")]
      corresponding_df[c("Work_reduced"),index_col]<-result_metrics[index_row,c("Work_reduced")]
    }
  }
}


#Upset plots Precision
col_fun = colorRamp2(c(0, 5), c("#e3f9fd", "#065279"))
pdf(paste0(output_dir,"//geom_bar_summary_CD8_GBM_Precision.pdf"),width=10, height=4)
p1<-UpSet(m,right_annotation = NULL, 
          top_annotation = HeatmapAnnotation("Question number" = as.numeric(comb_degree(m)), col = list("Question number" = col_fun),
                                             Precision = anno_barplot(as.numeric(corresponding_df["Precision",]), 
                                                                      border = FALSE, 
                                                                      gp = gpar(fill = "black"), 
                                                                      height = unit(6, "cm")
                                             ), 
                                             annotation_name_side = "left", 
                                             annotation_name_rot = 0), 
          set_order = NULL, 
          comb_order = order(corresponding_df["Precision",],decreasing = TRUE)
)
print(p1)
dev.off()

#Upset plots Recall
col_fun = colorRamp2(c(0, 5), c("#e3f9fd", "#065279"))
pdf(paste0(output_dir,"//geom_bar_summary_CD8_GBM_Recall.pdf"),width=10, height=4)
p1<-UpSet(m,right_annotation = NULL, 
          top_annotation = HeatmapAnnotation("Question number" = as.numeric(comb_degree(m)), col = list("Question number" = col_fun),
                                             Recall = anno_barplot(as.numeric(corresponding_df["Recall",]), 
                                                                   border = FALSE, 
                                                                   gp = gpar(fill = "black"), 
                                                                   height = unit(6, "cm")
                                             ), 
                                             annotation_name_side = "left", 
                                             annotation_name_rot = 0), 
          set_order = NULL, 
          comb_order = order(corresponding_df["Recall",],decreasing = TRUE)
)
print(p1)
dev.off()

#Upset plots F1
col_fun = colorRamp2(c(0, 5), c("#e3f9fd", "#065279"))
pdf(paste0(output_dir,"//geom_bar_summary_CD8_GBM_F1.pdf"),width=10, height=4)
p1<-UpSet(m,right_annotation = NULL, 
          top_annotation = HeatmapAnnotation("Question number" = as.numeric(comb_degree(m)), col = list("Question number" = col_fun),
                                             F1 = anno_barplot(as.numeric(corresponding_df["F1",]), 
                                                               border = FALSE, 
                                                               gp = gpar(fill = "black"), 
                                                               height = unit(6, "cm")
                                             ), 
                                             annotation_name_side = "left", 
                                             annotation_name_rot = 0), 
          set_order = NULL, 
          comb_order = order(corresponding_df["F1",],decreasing = TRUE)
)
print(p1)
dev.off()


#Upset plots Work_reduced
col_fun = colorRamp2(c(0, 5), c("#e3f9fd", "#065279"))
pdf(paste0(output_dir,"//geom_bar_summary_CD8_GBM_Work_reduced.pdf"),width=10, height=4)
p1<-UpSet(m,right_annotation = NULL, 
          top_annotation = HeatmapAnnotation("Question number" = as.numeric(comb_degree(m)), col = list("Question number" = col_fun),
                                             Work_reduced = anno_barplot(as.numeric(corresponding_df["Work_reduced",]), 
                                                                         border = FALSE, 
                                                                         gp = gpar(fill = "black"), 
                                                                         height = unit(6, "cm")
                                             ), 
                                             annotation_name_side = "left", 
                                             annotation_name_rot = 0), 
          set_order = NULL, 
          comb_order = order(corresponding_df["Work_reduced",],decreasing = TRUE)
)
print(p1)
dev.off()


#### CD8_GBM GPT4 ####
set.seed(1234)
colnames(input_dataset)
#01 all records
#calculate combined classification results
columes<-c("GPT4_species","GPT4_disease","GPT4_treatment","GPT4_research_type")
input_dataset<-summary_combine_calculator(data=input_dataset,columes=columes)

#calculate metrics
comp_columes<-colnames(input_dataset)[c(12:26)]
result_metrics<-metrics_calculator(data=input_dataset,comp_columes=comp_columes,manual_colume="Manual_AllYES_summary",Question="Combined")
#calculate random classifier
random_input_dataset<-input_dataset[,c("id","PMID","Manual_AllYES_summary")]
for (index in 1:100) {
  random_input_dataset[,c(paste0("random_col_",index))]<-sample(c("Yes", "No"), nrow(random_input_dataset), replace=TRUE)
}
random_comp_columes<-colnames(random_input_dataset)[c(4:103)]
random_result_metrics<-metrics_calculator(data=random_input_dataset,comp_columes=random_comp_columes,manual_colume="Manual_AllYES_summary",Question="Random")
temp_random_result_metrics<-random_result_metrics[1,]
temp_random_result_metrics$Approach<-"Random classifier"
temp_random_result_metrics[1,c("Precision","Recall","F1","Work_reduced",
                               "TN_Manual_No_Comp_No","FN_Manual_Yes_Comp_No",
                               "FP_Manual_No_Comp_Yes","TP_Manual_Yes_Comp_Yes")]<-apply(random_result_metrics[,c("Precision","Recall","F1","Work_reduced",
                                                                                                                  "TN_Manual_No_Comp_No","FN_Manual_Yes_Comp_No",
                                                                                                                  "FP_Manual_No_Comp_Yes","TP_Manual_Yes_Comp_Yes")], 2, function(x){mean(x,na.rm=TRUE)})
result_metrics<-rbind(result_metrics,temp_random_result_metrics)

write.table(result_metrics,file = paste0(output_dir, "\\", "CD8_GBM_result_metrics.txt"), sep = "\t")

#create mat matrix for upset plot
lt = list("GPT4_species" = columes,
          "GPT4_disease" = columes,
          "GPT4_treatment" = columes,
          "GPT4_research_type" = columes
)
m = make_comb_mat(lt, mode = "intersect")

#create corresponding df for upset plot
corresponding_df<-as.data.frame(m)
seq_colume_names<-rownames(as.data.frame(m))

result_metrics[,c(seq_colume_names)]<-0
for (question_index in 1:length(seq_colume_names)) {
  temp_question_name<-seq_colume_names[question_index]
  for (index_row in 1:nrow(result_metrics)) {
    if(length(grep(temp_question_name,result_metrics[index_row,"Approach"]))>0){
      result_metrics[index_row,temp_question_name]<-1
    }
  }
}

corresponding_df[c("Precision","Recall","F1","Work_reduced"),]<-0
for (index_col in 1:ncol(corresponding_df)) {
  for (index_row in 1:nrow(result_metrics)) {
    if (all(result_metrics[index_row,seq_colume_names]==corresponding_df[seq_colume_names,index_col])) {
      corresponding_df[c("Precision"),index_col]<-result_metrics[index_row,c("Precision")]
      corresponding_df[c("Recall"),index_col]<-result_metrics[index_row,c("Recall")]
      corresponding_df[c("F1"),index_col]<-result_metrics[index_row,c("F1")]
      corresponding_df[c("Work_reduced"),index_col]<-result_metrics[index_row,c("Work_reduced")]
    }
  }
}


#Upset plots Precision
col_fun = colorRamp2(c(0, 5), c("#e3f9fd", "#065279"))
pdf(paste0(output_dir,"//geom_bar_summary_CD8_GBM_Precision.pdf"),width=10, height=4)
p1<-UpSet(m,right_annotation = NULL, 
          top_annotation = HeatmapAnnotation("Question number" = as.numeric(comb_degree(m)), col = list("Question number" = col_fun),
                                             Precision = anno_barplot(as.numeric(corresponding_df["Precision",]), 
                                                                      border = FALSE, 
                                                                      gp = gpar(fill = "black"), 
                                                                      height = unit(6, "cm")
                                             ), 
                                             annotation_name_side = "left", 
                                             annotation_name_rot = 0), 
          set_order = NULL, 
          comb_order = order(corresponding_df["Precision",],decreasing = TRUE)
)
print(p1)
dev.off()

#Upset plots Recall
col_fun = colorRamp2(c(0, 5), c("#e3f9fd", "#065279"))
pdf(paste0(output_dir,"//geom_bar_summary_CD8_GBM_Recall.pdf"),width=10, height=4)
p1<-UpSet(m,right_annotation = NULL, 
          top_annotation = HeatmapAnnotation("Question number" = as.numeric(comb_degree(m)), col = list("Question number" = col_fun),
                                             Recall = anno_barplot(as.numeric(corresponding_df["Recall",]), 
                                                                   border = FALSE, 
                                                                   gp = gpar(fill = "black"), 
                                                                   height = unit(6, "cm")
                                             ), 
                                             annotation_name_side = "left", 
                                             annotation_name_rot = 0), 
          set_order = NULL, 
          comb_order = order(corresponding_df["Recall",],decreasing = TRUE)
)
print(p1)
dev.off()

#Upset plots F1
col_fun = colorRamp2(c(0, 5), c("#e3f9fd", "#065279"))
pdf(paste0(output_dir,"//geom_bar_summary_CD8_GBM_F1.pdf"),width=10, height=4)
p1<-UpSet(m,right_annotation = NULL, 
          top_annotation = HeatmapAnnotation("Question number" = as.numeric(comb_degree(m)), col = list("Question number" = col_fun),
                                             F1 = anno_barplot(as.numeric(corresponding_df["F1",]), 
                                                               border = FALSE, 
                                                               gp = gpar(fill = "black"), 
                                                               height = unit(6, "cm")
                                             ), 
                                             annotation_name_side = "left", 
                                             annotation_name_rot = 0), 
          set_order = NULL, 
          comb_order = order(corresponding_df["F1",],decreasing = TRUE)
)
print(p1)
dev.off()


#Upset plots Work_reduced
col_fun = colorRamp2(c(0, 5), c("#e3f9fd", "#065279"))
pdf(paste0(output_dir,"//geom_bar_summary_CD8_GBM_Work_reduced.pdf"),width=10, height=4)
p1<-UpSet(m,right_annotation = NULL, 
          top_annotation = HeatmapAnnotation("Question number" = as.numeric(comb_degree(m)), col = list("Question number" = col_fun),
                                             Work_reduced = anno_barplot(as.numeric(corresponding_df["Work_reduced",]), 
                                                                         border = FALSE, 
                                                                         gp = gpar(fill = "black"), 
                                                                         height = unit(6, "cm")
                                             ), 
                                             annotation_name_side = "left", 
                                             annotation_name_rot = 0), 
          set_order = NULL, 
          comb_order = order(corresponding_df["Work_reduced",],decreasing = TRUE)
)
print(p1)
dev.off()


####inflammatory bowel diseases####
####inflammatory bowel diseases GPT35 ####
set.seed(1234)
colnames(input_dataset)
#01 all records
#calculate combined classification results
columes<-c("GPT35_species","GPT35_disease","GPT35_age","GPT35_research_type","GPT35_Protein_related")
input_dataset<-summary_combine_calculator(data=input_dataset,columes=columes)

#calculate metrics
comp_columes<-colnames(input_dataset)[c(17:47)]
result_metrics<-metrics_calculator(data=input_dataset,comp_columes=comp_columes,manual_colume="Manual_AllYES_summary",Question="Combined")
#calculate random classifier
random_input_dataset<-input_dataset[,c("id","PMID","Manual_AllYES_summary")]
for (index in 1:100) {
  random_input_dataset[,c(paste0("random_col_",index))]<-sample(c("Yes", "No"), nrow(random_input_dataset), replace=TRUE)
}
random_comp_columes<-colnames(random_input_dataset)[c(4:103)]
random_result_metrics<-metrics_calculator(data=random_input_dataset,comp_columes=random_comp_columes,manual_colume="Manual_AllYES_summary",Question="Random")
temp_random_result_metrics<-random_result_metrics[1,]
temp_random_result_metrics$Approach<-"Random classifier"
temp_random_result_metrics[1,c("Precision","Recall","F1","Work_reduced",
                               "TN_Manual_No_Comp_No","FN_Manual_Yes_Comp_No",
                               "FP_Manual_No_Comp_Yes","TP_Manual_Yes_Comp_Yes")]<-apply(random_result_metrics[,c("Precision","Recall","F1","Work_reduced",
                                                                                                                  "TN_Manual_No_Comp_No","FN_Manual_Yes_Comp_No",
                                                                                                                  "FP_Manual_No_Comp_Yes","TP_Manual_Yes_Comp_Yes")], 2, function(x){mean(x,na.rm=TRUE)})
result_metrics<-rbind(result_metrics,temp_random_result_metrics)

write.table(result_metrics,file = paste0(output_dir, "\\", "IBD_result_metrics.txt"), sep = "\t")

#create mat matrix for upset plot
lt = list("GPT35_species" = columes,
          "GPT35_disease" = columes,
          "GPT35_age" = columes,
          "GPT35_research_type" = columes,
          "GPT35_Protein_related" = columes
          )
m = make_comb_mat(lt, mode = "intersect")

#create corresponding df for upset plot
corresponding_df<-as.data.frame(m)
seq_colume_names<-rownames(as.data.frame(m))

result_metrics[,c(seq_colume_names)]<-0
for (question_index in 1:length(seq_colume_names)) {
  temp_question_name<-seq_colume_names[question_index]
  for (index_row in 1:nrow(result_metrics)) {
    if(length(grep(temp_question_name,result_metrics[index_row,"Approach"]))>0){
      result_metrics[index_row,temp_question_name]<-1
    }
  }
}

corresponding_df[c("Precision","Recall","F1","Work_reduced"),]<-0
for (index_col in 1:ncol(corresponding_df)) {
  for (index_row in 1:nrow(result_metrics)) {
    if (all(result_metrics[index_row,seq_colume_names]==corresponding_df[seq_colume_names,index_col])) {
      corresponding_df[c("Precision"),index_col]<-result_metrics[index_row,c("Precision")]
      corresponding_df[c("Recall"),index_col]<-result_metrics[index_row,c("Recall")]
      corresponding_df[c("F1"),index_col]<-result_metrics[index_row,c("F1")]
      corresponding_df[c("Work_reduced"),index_col]<-result_metrics[index_row,c("Work_reduced")]
    }
  }
}


#Upset plots Precision
col_fun = colorRamp2(c(0, 5), c("#e3f9fd", "#065279"))
pdf(paste0(output_dir,"//geom_bar_summary_IBD_Precision.pdf"),width=10, height=4)
p1<-UpSet(m,right_annotation = NULL, 
      top_annotation = HeatmapAnnotation("Question number" = as.numeric(comb_degree(m)), col = list("Question number" = col_fun),
                                         Precision = anno_barplot(as.numeric(corresponding_df["Precision",]), 
                                                                             border = FALSE, 
                                                                             gp = gpar(fill = "black"), 
                                                                             height = unit(6, "cm")
                                         ), 
                                         annotation_name_side = "left", 
                                         annotation_name_rot = 0), 
      set_order = NULL, 
      comb_order = order(corresponding_df["Precision",],decreasing = TRUE)
)
print(p1)
dev.off()

#Upset plots Recall
col_fun = colorRamp2(c(0, 5), c("#e3f9fd", "#065279"))
pdf(paste0(output_dir,"//geom_bar_summary_IBD_Recall.pdf"),width=10, height=4)
p1<-UpSet(m,right_annotation = NULL, 
          top_annotation = HeatmapAnnotation("Question number" = as.numeric(comb_degree(m)), col = list("Question number" = col_fun),
                                             Recall = anno_barplot(as.numeric(corresponding_df["Recall",]), 
                                                                      border = FALSE, 
                                                                      gp = gpar(fill = "black"), 
                                                                      height = unit(6, "cm")
                                             ), 
                                             annotation_name_side = "left", 
                                             annotation_name_rot = 0), 
          set_order = NULL, 
          comb_order = order(corresponding_df["Recall",],decreasing = TRUE)
)
print(p1)
dev.off()

#Upset plots F1
col_fun = colorRamp2(c(0, 5), c("#e3f9fd", "#065279"))
pdf(paste0(output_dir,"//geom_bar_summary_IBD_F1.pdf"),width=10, height=4)
p1<-UpSet(m,right_annotation = NULL, 
          top_annotation = HeatmapAnnotation("Question number" = as.numeric(comb_degree(m)), col = list("Question number" = col_fun),
                                             F1 = anno_barplot(as.numeric(corresponding_df["F1",]), 
                                                                   border = FALSE, 
                                                                   gp = gpar(fill = "black"), 
                                                                   height = unit(6, "cm")
                                             ), 
                                             annotation_name_side = "left", 
                                             annotation_name_rot = 0), 
          set_order = NULL, 
          comb_order = order(corresponding_df["F1",],decreasing = TRUE)
)
print(p1)
dev.off()


#Upset plots Work_reduced
col_fun = colorRamp2(c(0, 5), c("#e3f9fd", "#065279"))
pdf(paste0(output_dir,"//geom_bar_summary_IBD_Work_reduced.pdf"),width=10, height=4)
p1<-UpSet(m,right_annotation = NULL, 
          top_annotation = HeatmapAnnotation("Question number" = as.numeric(comb_degree(m)), col = list("Question number" = col_fun),
                                             Work_reduced = anno_barplot(as.numeric(corresponding_df["Work_reduced",]), 
                                                               border = FALSE, 
                                                               gp = gpar(fill = "black"), 
                                                               height = unit(6, "cm")
                                             ), 
                                             annotation_name_side = "left", 
                                             annotation_name_rot = 0), 
          set_order = NULL, 
          comb_order = order(corresponding_df["Work_reduced",],decreasing = TRUE)
)
print(p1)
dev.off()

####inflammatory bowel diseases GPT4 ####
set.seed(1234)
colnames(input_dataset)
#01 all records
#calculate combined classification results
columes<-c("GPT4_species","GPT4_disease","GPT4_age","GPT4_research_type","GPT4_Protein_related")
input_dataset<-summary_combine_calculator(data=input_dataset,columes=columes)

#calculate metrics
comp_columes<-colnames(input_dataset)[c(9:39)]
result_metrics<-metrics_calculator(data=input_dataset,comp_columes=comp_columes,manual_colume="Manual_AllYES_summary",Question="Combined")
#calculate random classifier
random_input_dataset<-input_dataset[,c("id","PMID","Manual_AllYES_summary")]
for (index in 1:100) {
  random_input_dataset[,c(paste0("random_col_",index))]<-sample(c("Yes", "No"), nrow(random_input_dataset), replace=TRUE)
}
random_comp_columes<-colnames(random_input_dataset)[c(4:103)]
random_result_metrics<-metrics_calculator(data=random_input_dataset,comp_columes=random_comp_columes,manual_colume="Manual_AllYES_summary",Question="Random")
temp_random_result_metrics<-random_result_metrics[1,]
temp_random_result_metrics$Approach<-"Random classifier"
temp_random_result_metrics[1,c("Precision","Recall","F1","Work_reduced",
                               "TN_Manual_No_Comp_No","FN_Manual_Yes_Comp_No",
                               "FP_Manual_No_Comp_Yes","TP_Manual_Yes_Comp_Yes")]<-apply(random_result_metrics[,c("Precision","Recall","F1","Work_reduced",
                                                                                                                  "TN_Manual_No_Comp_No","FN_Manual_Yes_Comp_No",
                                                                                                                  "FP_Manual_No_Comp_Yes","TP_Manual_Yes_Comp_Yes")], 2, function(x){mean(x,na.rm=TRUE)})
result_metrics<-rbind(result_metrics,temp_random_result_metrics)

write.table(result_metrics,file = paste0(output_dir, "\\", "IBD_result_metrics.txt"), sep = "\t")

#create mat matrix for upset plot
lt = list("GPT4_species" = columes,
          "GPT4_disease" = columes,
          "GPT4_age" = columes,
          "GPT4_research_type" = columes,
          "GPT4_Protein_related" = columes
)
m = make_comb_mat(lt, mode = "intersect")

#create corresponding df for upset plot
corresponding_df<-as.data.frame(m)
seq_colume_names<-rownames(as.data.frame(m))

result_metrics[,c(seq_colume_names)]<-0
for (question_index in 1:length(seq_colume_names)) {
  temp_question_name<-seq_colume_names[question_index]
  for (index_row in 1:nrow(result_metrics)) {
    if(length(grep(temp_question_name,result_metrics[index_row,"Approach"]))>0){
      result_metrics[index_row,temp_question_name]<-1
    }
  }
}

corresponding_df[c("Precision","Recall","F1","Work_reduced"),]<-0
for (index_col in 1:ncol(corresponding_df)) {
  for (index_row in 1:nrow(result_metrics)) {
    if (all(result_metrics[index_row,seq_colume_names]==corresponding_df[seq_colume_names,index_col])) {
      corresponding_df[c("Precision"),index_col]<-result_metrics[index_row,c("Precision")]
      corresponding_df[c("Recall"),index_col]<-result_metrics[index_row,c("Recall")]
      corresponding_df[c("F1"),index_col]<-result_metrics[index_row,c("F1")]
      corresponding_df[c("Work_reduced"),index_col]<-result_metrics[index_row,c("Work_reduced")]
    }
  }
}


#Upset plots Precision
col_fun = colorRamp2(c(0, 5), c("#e3f9fd", "#065279"))
pdf(paste0(output_dir,"//geom_bar_summary_IBD_Precision.pdf"),width=10, height=4)
p1<-UpSet(m,right_annotation = NULL, 
          top_annotation = HeatmapAnnotation("Question number" = as.numeric(comb_degree(m)), col = list("Question number" = col_fun),
                                             Precision = anno_barplot(as.numeric(corresponding_df["Precision",]), 
                                                                      border = FALSE, 
                                                                      gp = gpar(fill = "black"), 
                                                                      height = unit(6, "cm")
                                             ), 
                                             annotation_name_side = "left", 
                                             annotation_name_rot = 0), 
          set_order = NULL, 
          comb_order = order(corresponding_df["Precision",],decreasing = TRUE)
)
print(p1)
dev.off()

#Upset plots Recall
col_fun = colorRamp2(c(0, 5), c("#e3f9fd", "#065279"))
pdf(paste0(output_dir,"//geom_bar_summary_IBD_Recall.pdf"),width=10, height=4)
p1<-UpSet(m,right_annotation = NULL, 
          top_annotation = HeatmapAnnotation("Question number" = as.numeric(comb_degree(m)), col = list("Question number" = col_fun),
                                             Recall = anno_barplot(as.numeric(corresponding_df["Recall",]), 
                                                                   border = FALSE, 
                                                                   gp = gpar(fill = "black"), 
                                                                   height = unit(6, "cm")
                                             ), 
                                             annotation_name_side = "left", 
                                             annotation_name_rot = 0), 
          set_order = NULL, 
          comb_order = order(corresponding_df["Recall",],decreasing = TRUE)
)
print(p1)
dev.off()

#Upset plots F1
col_fun = colorRamp2(c(0, 5), c("#e3f9fd", "#065279"))
pdf(paste0(output_dir,"//geom_bar_summary_IBD_F1.pdf"),width=10, height=4)
p1<-UpSet(m,right_annotation = NULL, 
          top_annotation = HeatmapAnnotation("Question number" = as.numeric(comb_degree(m)), col = list("Question number" = col_fun),
                                             F1 = anno_barplot(as.numeric(corresponding_df["F1",]), 
                                                               border = FALSE, 
                                                               gp = gpar(fill = "black"), 
                                                               height = unit(6, "cm")
                                             ), 
                                             annotation_name_side = "left", 
                                             annotation_name_rot = 0), 
          set_order = NULL, 
          comb_order = order(corresponding_df["F1",],decreasing = TRUE)
)
print(p1)
dev.off()


#Upset plots Work_reduced
col_fun = colorRamp2(c(0, 5), c("#e3f9fd", "#065279"))
pdf(paste0(output_dir,"//geom_bar_summary_IBD_Work_reduced.pdf"),width=10, height=4)
p1<-UpSet(m,right_annotation = NULL, 
          top_annotation = HeatmapAnnotation("Question number" = as.numeric(comb_degree(m)), col = list("Question number" = col_fun),
                                             Work_reduced = anno_barplot(as.numeric(corresponding_df["Work_reduced",]), 
                                                                         border = FALSE, 
                                                                         gp = gpar(fill = "black"), 
                                                                         height = unit(6, "cm")
                                             ), 
                                             annotation_name_side = "left", 
                                             annotation_name_rot = 0), 
          set_order = NULL, 
          comb_order = order(corresponding_df["Work_reduced",],decreasing = TRUE)
)
print(p1)
dev.off()

#### Diabetes mellitus####
#### Diabetes mellitus GPT35####
set.seed(1234)
colnames(input_dataset)
#01 all records
#calculate combined classification results
columes<-c("GPT35_species","GPT35_research_type","GPT35_disease_dm","GPT35_disease_p")
input_dataset<-summary_combine_calculator(data=input_dataset,columes=columes)

#calculate metrics
comp_columes<-colnames(input_dataset)[c(12:26)]
result_metrics<-metrics_calculator(data=input_dataset,comp_columes=comp_columes,manual_colume="Manual_AllYES_summary",Question="Combined")
#calculate random classifier
random_input_dataset<-input_dataset[,c("id","PMID","Manual_AllYES_summary")]
for (index in 1:100) {
  random_input_dataset[,c(paste0("random_col_",index))]<-sample(c("Yes", "No"), nrow(random_input_dataset), replace=TRUE)
}
random_comp_columes<-colnames(random_input_dataset)[c(4:103)]
random_result_metrics<-metrics_calculator(data=random_input_dataset,comp_columes=random_comp_columes,manual_colume="Manual_AllYES_summary",Question="Random")
temp_random_result_metrics<-random_result_metrics[1,]
temp_random_result_metrics$Approach<-"Random classifier"
temp_random_result_metrics[1,c("Precision","Recall","F1","Work_reduced",
                               "TN_Manual_No_Comp_No","FN_Manual_Yes_Comp_No",
                               "FP_Manual_No_Comp_Yes","TP_Manual_Yes_Comp_Yes")]<-apply(random_result_metrics[,c("Precision","Recall","F1","Work_reduced",
                                                                                                                  "TN_Manual_No_Comp_No","FN_Manual_Yes_Comp_No",
                                                                                                                  "FP_Manual_No_Comp_Yes","TP_Manual_Yes_Comp_Yes")], 2, function(x){mean(x,na.rm=TRUE)})
result_metrics<-rbind(result_metrics,temp_random_result_metrics)

write.table(result_metrics,file = paste0(output_dir, "\\", "DM_result_metrics.txt"), sep = "\t")

#create mat matrix for upset plot
lt = list("GPT35_species" = columes,
          "GPT35_research_type" = columes,
          "GPT35_disease_dm" = columes,
          "GPT35_disease_p" = columes
)
m = make_comb_mat(lt, mode = "intersect")

#create corresponding df for upset plot
corresponding_df<-as.data.frame(m)
seq_colume_names<-rownames(as.data.frame(m))

result_metrics[,c(seq_colume_names)]<-0
for (question_index in 1:length(seq_colume_names)) {
  temp_question_name<-seq_colume_names[question_index]
  for (index_row in 1:nrow(result_metrics)) {
    if(length(grep(temp_question_name,result_metrics[index_row,"Approach"]))>0){
      result_metrics[index_row,temp_question_name]<-1
    }
  }
}

corresponding_df[c("Precision","Recall","F1","Work_reduced"),]<-0
for (index_col in 1:ncol(corresponding_df)) {
  for (index_row in 1:nrow(result_metrics)) {
    if (all(result_metrics[index_row,seq_colume_names]==corresponding_df[seq_colume_names,index_col])) {
      corresponding_df[c("Precision"),index_col]<-result_metrics[index_row,c("Precision")]
      corresponding_df[c("Recall"),index_col]<-result_metrics[index_row,c("Recall")]
      corresponding_df[c("F1"),index_col]<-result_metrics[index_row,c("F1")]
      corresponding_df[c("Work_reduced"),index_col]<-result_metrics[index_row,c("Work_reduced")]
    }
  }
}


#Upset plots Precision
col_fun = colorRamp2(c(0, 5), c("#e3f9fd", "#065279"))
pdf(paste0(output_dir,"//geom_bar_summary_DM_Precision.pdf"),width=10, height=4)
p1<-UpSet(m,right_annotation = NULL, 
          top_annotation = HeatmapAnnotation("Question number" = as.numeric(comb_degree(m)), col = list("Question number" = col_fun),
                                             Precision = anno_barplot(as.numeric(corresponding_df["Precision",]), 
                                                                      border = FALSE, 
                                                                      gp = gpar(fill = "black"), 
                                                                      height = unit(6, "cm")
                                             ), 
                                             annotation_name_side = "left", 
                                             annotation_name_rot = 0), 
          set_order = NULL, 
          comb_order = order(corresponding_df["Precision",],decreasing = TRUE)
)
print(p1)
dev.off()

#Upset plots Recall
col_fun = colorRamp2(c(0, 5), c("#e3f9fd", "#065279"))
pdf(paste0(output_dir,"//geom_bar_summary_DM_Recall.pdf"),width=10, height=4)
p1<-UpSet(m,right_annotation = NULL, 
          top_annotation = HeatmapAnnotation("Question number" = as.numeric(comb_degree(m)), col = list("Question number" = col_fun),
                                             Recall = anno_barplot(as.numeric(corresponding_df["Recall",]), 
                                                                   border = FALSE, 
                                                                   gp = gpar(fill = "black"), 
                                                                   height = unit(6, "cm")
                                             ), 
                                             annotation_name_side = "left", 
                                             annotation_name_rot = 0), 
          set_order = NULL, 
          comb_order = order(corresponding_df["Recall",],decreasing = TRUE)
)
print(p1)
dev.off()

#Upset plots F1
col_fun = colorRamp2(c(0, 5), c("#e3f9fd", "#065279"))
pdf(paste0(output_dir,"//geom_bar_summary_DM_F1.pdf"),width=10, height=4)
p1<-UpSet(m,right_annotation = NULL, 
          top_annotation = HeatmapAnnotation("Question number" = as.numeric(comb_degree(m)), col = list("Question number" = col_fun),
                                             F1 = anno_barplot(as.numeric(corresponding_df["F1",]), 
                                                               border = FALSE, 
                                                               gp = gpar(fill = "black"), 
                                                               height = unit(6, "cm")
                                             ), 
                                             annotation_name_side = "left", 
                                             annotation_name_rot = 0), 
          set_order = NULL, 
          comb_order = order(corresponding_df["F1",],decreasing = TRUE)
)
print(p1)
dev.off()


#Upset plots Work_reduced
col_fun = colorRamp2(c(0, 5), c("#e3f9fd", "#065279"))
pdf(paste0(output_dir,"//geom_bar_summary_DM_Work_reduced.pdf"),width=10, height=4)
p1<-UpSet(m,right_annotation = NULL, 
          top_annotation = HeatmapAnnotation("Question number" = as.numeric(comb_degree(m)), col = list("Question number" = col_fun),
                                             Work_reduced = anno_barplot(as.numeric(corresponding_df["Work_reduced",]), 
                                                                         border = FALSE, 
                                                                         gp = gpar(fill = "black"), 
                                                                         height = unit(6, "cm")
                                             ), 
                                             annotation_name_side = "left", 
                                             annotation_name_rot = 0), 
          set_order = NULL, 
          comb_order = order(corresponding_df["Work_reduced",],decreasing = TRUE)
)
print(p1)
dev.off()

#### Diabetes mellitus GPT4####
set.seed(1234)
colnames(input_dataset)
#01 all records
#calculate combined classification results
columes<-c("GPT4_species","GPT4_research_type","GPT4_disease_dm","GPT4_disease_p")
input_dataset<-summary_combine_calculator(data=input_dataset,columes=columes)

#calculate metrics
comp_columes<-colnames(input_dataset)[c(11:25)]
result_metrics<-metrics_calculator(data=input_dataset,comp_columes=comp_columes,manual_colume="Manual_AllYES_summary",Question="Combined")
#calculate random classifier
random_input_dataset<-input_dataset[,c("id","PMID","Manual_AllYES_summary")]
for (index in 1:100) {
  random_input_dataset[,c(paste0("random_col_",index))]<-sample(c("Yes", "No"), nrow(random_input_dataset), replace=TRUE)
}
random_comp_columes<-colnames(random_input_dataset)[c(4:103)]
random_result_metrics<-metrics_calculator(data=random_input_dataset,comp_columes=random_comp_columes,manual_colume="Manual_AllYES_summary",Question="Random")
temp_random_result_metrics<-random_result_metrics[1,]
temp_random_result_metrics$Approach<-"Random classifier"
temp_random_result_metrics[1,c("Precision","Recall","F1","Work_reduced",
                               "TN_Manual_No_Comp_No","FN_Manual_Yes_Comp_No",
                               "FP_Manual_No_Comp_Yes","TP_Manual_Yes_Comp_Yes")]<-apply(random_result_metrics[,c("Precision","Recall","F1","Work_reduced",
                                                                                                                  "TN_Manual_No_Comp_No","FN_Manual_Yes_Comp_No",
                                                                                                                  "FP_Manual_No_Comp_Yes","TP_Manual_Yes_Comp_Yes")], 2, function(x){mean(x,na.rm=TRUE)})
result_metrics<-rbind(result_metrics,temp_random_result_metrics)

write.table(result_metrics,file = paste0(output_dir, "\\", "DM_result_metrics.txt"), sep = "\t")

#create mat matrix for upset plot
lt = list("GPT4_species" = columes,
          "GPT4_research_type" = columes,
          "GPT4_disease_dm" = columes,
          "GPT4_disease_p" = columes
)
m = make_comb_mat(lt, mode = "intersect")

#create corresponding df for upset plot
corresponding_df<-as.data.frame(m)
seq_colume_names<-rownames(as.data.frame(m))

result_metrics[,c(seq_colume_names)]<-0
for (question_index in 1:length(seq_colume_names)) {
  temp_question_name<-seq_colume_names[question_index]
  for (index_row in 1:nrow(result_metrics)) {
    if(length(grep(temp_question_name,result_metrics[index_row,"Approach"]))>0){
      result_metrics[index_row,temp_question_name]<-1
    }
  }
}

corresponding_df[c("Precision","Recall","F1","Work_reduced"),]<-0
for (index_col in 1:ncol(corresponding_df)) {
  for (index_row in 1:nrow(result_metrics)) {
    if (all(result_metrics[index_row,seq_colume_names]==corresponding_df[seq_colume_names,index_col])) {
      corresponding_df[c("Precision"),index_col]<-result_metrics[index_row,c("Precision")]
      corresponding_df[c("Recall"),index_col]<-result_metrics[index_row,c("Recall")]
      corresponding_df[c("F1"),index_col]<-result_metrics[index_row,c("F1")]
      corresponding_df[c("Work_reduced"),index_col]<-result_metrics[index_row,c("Work_reduced")]
    }
  }
}


#Upset plots Precision
col_fun = colorRamp2(c(0, 5), c("#e3f9fd", "#065279"))
pdf(paste0(output_dir,"//geom_bar_summary_DM_Precision.pdf"),width=10, height=4)
p1<-UpSet(m,right_annotation = NULL, 
          top_annotation = HeatmapAnnotation("Question number" = as.numeric(comb_degree(m)), col = list("Question number" = col_fun),
                                             Precision = anno_barplot(as.numeric(corresponding_df["Precision",]), 
                                                                      border = FALSE, 
                                                                      gp = gpar(fill = "black"), 
                                                                      height = unit(6, "cm")
                                             ), 
                                             annotation_name_side = "left", 
                                             annotation_name_rot = 0), 
          set_order = NULL, 
          comb_order = order(corresponding_df["Precision",],decreasing = TRUE)
)
print(p1)
dev.off()

#Upset plots Recall
col_fun = colorRamp2(c(0, 5), c("#e3f9fd", "#065279"))
pdf(paste0(output_dir,"//geom_bar_summary_DM_Recall.pdf"),width=10, height=4)
p1<-UpSet(m,right_annotation = NULL, 
          top_annotation = HeatmapAnnotation("Question number" = as.numeric(comb_degree(m)), col = list("Question number" = col_fun),
                                             Recall = anno_barplot(as.numeric(corresponding_df["Recall",]), 
                                                                   border = FALSE, 
                                                                   gp = gpar(fill = "black"), 
                                                                   height = unit(6, "cm")
                                             ), 
                                             annotation_name_side = "left", 
                                             annotation_name_rot = 0), 
          set_order = NULL, 
          comb_order = order(corresponding_df["Recall",],decreasing = TRUE)
)
print(p1)
dev.off()

#Upset plots F1
col_fun = colorRamp2(c(0, 5), c("#e3f9fd", "#065279"))
pdf(paste0(output_dir,"//geom_bar_summary_DM_F1.pdf"),width=10, height=4)
p1<-UpSet(m,right_annotation = NULL, 
          top_annotation = HeatmapAnnotation("Question number" = as.numeric(comb_degree(m)), col = list("Question number" = col_fun),
                                             F1 = anno_barplot(as.numeric(corresponding_df["F1",]), 
                                                               border = FALSE, 
                                                               gp = gpar(fill = "black"), 
                                                               height = unit(6, "cm")
                                             ), 
                                             annotation_name_side = "left", 
                                             annotation_name_rot = 0), 
          set_order = NULL, 
          comb_order = order(corresponding_df["F1",],decreasing = TRUE)
)
print(p1)
dev.off()


#Upset plots Work_reduced
col_fun = colorRamp2(c(0, 5), c("#e3f9fd", "#065279"))
pdf(paste0(output_dir,"//geom_bar_summary_DM_Work_reduced.pdf"),width=10, height=4)
p1<-UpSet(m,right_annotation = NULL, 
          top_annotation = HeatmapAnnotation("Question number" = as.numeric(comb_degree(m)), col = list("Question number" = col_fun),
                                             Work_reduced = anno_barplot(as.numeric(corresponding_df["Work_reduced",]), 
                                                                         border = FALSE, 
                                                                         gp = gpar(fill = "black"), 
                                                                         height = unit(6, "cm")
                                             ), 
                                             annotation_name_side = "left", 
                                             annotation_name_rot = 0), 
          set_order = NULL, 
          comb_order = order(corresponding_df["Work_reduced",],decreasing = TRUE)
)
print(p1)
dev.off()


#### Sarcopenia####
#### Sarcopenia GPT35####
set.seed(1234)
colnames(input_dataset)
#01 all records
#calculate combined classification results
columes<-c("GPT35_species","GPT35_disease","GPT35_control","GPT35_research_type","GPT35_outcome")
input_dataset<-summary_combine_calculator(data=input_dataset,columes=columes)

#calculate metrics
comp_columes<-colnames(input_dataset)[c(13:43)]
result_metrics<-metrics_calculator(data=input_dataset,comp_columes=comp_columes,manual_colume="Manual_AllYES_summary",Question="Combined")
#calculate random classifier
random_input_dataset<-input_dataset[,c("id","PMID","Manual_AllYES_summary")]
for (index in 1:100) {
  random_input_dataset[,c(paste0("random_col_",index))]<-sample(c("Yes", "No"), nrow(random_input_dataset), replace=TRUE)
}
random_comp_columes<-colnames(random_input_dataset)[c(4:103)]
random_result_metrics<-metrics_calculator(data=random_input_dataset,comp_columes=random_comp_columes,manual_colume="Manual_AllYES_summary",Question="Random")
temp_random_result_metrics<-random_result_metrics[1,]
temp_random_result_metrics$Approach<-"Random classifier"
temp_random_result_metrics[1,c("Precision","Recall","F1","Work_reduced",
                               "TN_Manual_No_Comp_No","FN_Manual_Yes_Comp_No",
                               "FP_Manual_No_Comp_Yes","TP_Manual_Yes_Comp_Yes")]<-apply(random_result_metrics[,c("Precision","Recall","F1","Work_reduced",
                                                                                                                  "TN_Manual_No_Comp_No","FN_Manual_Yes_Comp_No",
                                                                                                                  "FP_Manual_No_Comp_Yes","TP_Manual_Yes_Comp_Yes")], 2, function(x){mean(x,na.rm=TRUE)})
result_metrics<-rbind(result_metrics,temp_random_result_metrics)

write.table(result_metrics,file = paste0(output_dir, "\\", "Sarcopenia_result_metrics.txt"), sep = "\t")

#create mat matrix for upset plot
lt = list("GPT35_species" = columes,
          "GPT35_disease" = columes,
          "GPT35_control" = columes,
          "GPT35_research_type" = columes,
          "GPT35_outcome" = columes
)
m = make_comb_mat(lt, mode = "intersect")

#create corresponding df for upset plot
corresponding_df<-as.data.frame(m)
seq_colume_names<-rownames(as.data.frame(m))

result_metrics[,c(seq_colume_names)]<-0
for (question_index in 1:length(seq_colume_names)) {
  temp_question_name<-seq_colume_names[question_index]
  for (index_row in 1:nrow(result_metrics)) {
    if(length(grep(temp_question_name,result_metrics[index_row,"Approach"]))>0){
      result_metrics[index_row,temp_question_name]<-1
    }
  }
}

corresponding_df[c("Precision","Recall","F1","Work_reduced"),]<-0
for (index_col in 1:ncol(corresponding_df)) {
  for (index_row in 1:nrow(result_metrics)) {
    if (all(result_metrics[index_row,seq_colume_names]==corresponding_df[seq_colume_names,index_col])) {
      corresponding_df[c("Precision"),index_col]<-result_metrics[index_row,c("Precision")]
      corresponding_df[c("Recall"),index_col]<-result_metrics[index_row,c("Recall")]
      corresponding_df[c("F1"),index_col]<-result_metrics[index_row,c("F1")]
      corresponding_df[c("Work_reduced"),index_col]<-result_metrics[index_row,c("Work_reduced")]
    }
  }
}


#Upset plots Precision
col_fun = colorRamp2(c(0, 5), c("#e3f9fd", "#065279"))
pdf(paste0(output_dir,"//geom_bar_summary_Sarcopenia_Precision.pdf"),width=10, height=4)
p1<-UpSet(m,right_annotation = NULL, 
          top_annotation = HeatmapAnnotation("Question number" = as.numeric(comb_degree(m)), col = list("Question number" = col_fun),
                                             Precision = anno_barplot(as.numeric(corresponding_df["Precision",]), 
                                                                      border = FALSE, 
                                                                      gp = gpar(fill = "black"), 
                                                                      height = unit(6, "cm")
                                             ), 
                                             annotation_name_side = "left", 
                                             annotation_name_rot = 0), 
          set_order = NULL, 
          comb_order = order(corresponding_df["Precision",],decreasing = TRUE)
)
print(p1)
dev.off()

#Upset plots Recall
col_fun = colorRamp2(c(0, 5), c("#e3f9fd", "#065279"))
pdf(paste0(output_dir,"//geom_bar_summary_Sarcopenia_Recall.pdf"),width=10, height=4)
p1<-UpSet(m,right_annotation = NULL, 
          top_annotation = HeatmapAnnotation("Question number" = as.numeric(comb_degree(m)), col = list("Question number" = col_fun),
                                             Recall = anno_barplot(as.numeric(corresponding_df["Recall",]), 
                                                                   border = FALSE, 
                                                                   gp = gpar(fill = "black"), 
                                                                   height = unit(6, "cm")
                                             ), 
                                             annotation_name_side = "left", 
                                             annotation_name_rot = 0), 
          set_order = NULL, 
          comb_order = order(corresponding_df["Recall",],decreasing = TRUE)
)
print(p1)
dev.off()

#Upset plots F1
col_fun = colorRamp2(c(0, 5), c("#e3f9fd", "#065279"))
pdf(paste0(output_dir,"//geom_bar_summary_Sarcopenia_F1.pdf"),width=10, height=4)
p1<-UpSet(m,right_annotation = NULL, 
          top_annotation = HeatmapAnnotation("Question number" = as.numeric(comb_degree(m)), col = list("Question number" = col_fun),
                                             F1 = anno_barplot(as.numeric(corresponding_df["F1",]), 
                                                               border = FALSE, 
                                                               gp = gpar(fill = "black"), 
                                                               height = unit(6, "cm")
                                             ), 
                                             annotation_name_side = "left", 
                                             annotation_name_rot = 0), 
          set_order = NULL, 
          comb_order = order(corresponding_df["F1",],decreasing = TRUE)
)
print(p1)
dev.off()


#Upset plots Work_reduced
col_fun = colorRamp2(c(0, 5), c("#e3f9fd", "#065279"))
pdf(paste0(output_dir,"//geom_bar_summary_Sarcopenia_Work_reduced.pdf"),width=10, height=4)
p1<-UpSet(m,right_annotation = NULL, 
          top_annotation = HeatmapAnnotation("Question number" = as.numeric(comb_degree(m)), col = list("Question number" = col_fun),
                                             Work_reduced = anno_barplot(as.numeric(corresponding_df["Work_reduced",]), 
                                                                         border = FALSE, 
                                                                         gp = gpar(fill = "black"), 
                                                                         height = unit(6, "cm")
                                             ), 
                                             annotation_name_side = "left", 
                                             annotation_name_rot = 0), 
          set_order = NULL, 
          comb_order = order(corresponding_df["Work_reduced",],decreasing = TRUE)
)
print(p1)
dev.off()

#### Sarcopenia GPT4####
set.seed(1234)
colnames(input_dataset)
#01 all records
#calculate combined classification results
columes<-c("GPT4_species","GPT4_disease","GPT4_control","GPT4_research_type","GPT4_outcome")
input_dataset<-summary_combine_calculator(data=input_dataset,columes=columes)

#calculate metrics
comp_columes<-colnames(input_dataset)[c(12:42)]
result_metrics<-metrics_calculator(data=input_dataset,comp_columes=comp_columes,manual_colume="Manual_AllYES_summary",Question="Combined")
#calculate random classifier
random_input_dataset<-input_dataset[,c("id","PMID","Manual_AllYES_summary")]
for (index in 1:100) {
  random_input_dataset[,c(paste0("random_col_",index))]<-sample(c("Yes", "No"), nrow(random_input_dataset), replace=TRUE)
}
random_comp_columes<-colnames(random_input_dataset)[c(4:103)]
random_result_metrics<-metrics_calculator(data=random_input_dataset,comp_columes=random_comp_columes,manual_colume="Manual_AllYES_summary",Question="Random")
temp_random_result_metrics<-random_result_metrics[1,]
temp_random_result_metrics$Approach<-"Random classifier"
temp_random_result_metrics[1,c("Precision","Recall","F1","Work_reduced",
                               "TN_Manual_No_Comp_No","FN_Manual_Yes_Comp_No",
                               "FP_Manual_No_Comp_Yes","TP_Manual_Yes_Comp_Yes")]<-apply(random_result_metrics[,c("Precision","Recall","F1","Work_reduced",
                                                                                                                  "TN_Manual_No_Comp_No","FN_Manual_Yes_Comp_No",
                                                                                                                  "FP_Manual_No_Comp_Yes","TP_Manual_Yes_Comp_Yes")], 2, function(x){mean(x,na.rm=TRUE)})
result_metrics<-rbind(result_metrics,temp_random_result_metrics)

write.table(result_metrics,file = paste0(output_dir, "\\", "Sarcopenia_result_metrics.txt"), sep = "\t")

#create mat matrix for upset plot
lt = list("GPT4_species" = columes,
          "GPT4_disease" = columes,
          "GPT4_control" = columes,
          "GPT4_research_type" = columes,
          "GPT4_outcome" = columes
)
m = make_comb_mat(lt, mode = "intersect")

#create corresponding df for upset plot
corresponding_df<-as.data.frame(m)
seq_colume_names<-rownames(as.data.frame(m))

result_metrics[,c(seq_colume_names)]<-0
for (question_index in 1:length(seq_colume_names)) {
  temp_question_name<-seq_colume_names[question_index]
  for (index_row in 1:nrow(result_metrics)) {
    if(length(grep(temp_question_name,result_metrics[index_row,"Approach"]))>0){
      result_metrics[index_row,temp_question_name]<-1
    }
  }
}

corresponding_df[c("Precision","Recall","F1","Work_reduced"),]<-0
for (index_col in 1:ncol(corresponding_df)) {
  for (index_row in 1:nrow(result_metrics)) {
    if (all(result_metrics[index_row,seq_colume_names]==corresponding_df[seq_colume_names,index_col])) {
      corresponding_df[c("Precision"),index_col]<-result_metrics[index_row,c("Precision")]
      corresponding_df[c("Recall"),index_col]<-result_metrics[index_row,c("Recall")]
      corresponding_df[c("F1"),index_col]<-result_metrics[index_row,c("F1")]
      corresponding_df[c("Work_reduced"),index_col]<-result_metrics[index_row,c("Work_reduced")]
    }
  }
}


#Upset plots Precision
col_fun = colorRamp2(c(0, 5), c("#e3f9fd", "#065279"))
pdf(paste0(output_dir,"//geom_bar_summary_Sarcopenia_Precision.pdf"),width=10, height=4)
p1<-UpSet(m,right_annotation = NULL, 
          top_annotation = HeatmapAnnotation("Question number" = as.numeric(comb_degree(m)), col = list("Question number" = col_fun),
                                             Precision = anno_barplot(as.numeric(corresponding_df["Precision",]), 
                                                                      border = FALSE, 
                                                                      gp = gpar(fill = "black"), 
                                                                      height = unit(6, "cm")
                                             ), 
                                             annotation_name_side = "left", 
                                             annotation_name_rot = 0), 
          set_order = NULL, 
          comb_order = order(corresponding_df["Precision",],decreasing = TRUE)
)
print(p1)
dev.off()

#Upset plots Recall
col_fun = colorRamp2(c(0, 5), c("#e3f9fd", "#065279"))
pdf(paste0(output_dir,"//geom_bar_summary_Sarcopenia_Recall.pdf"),width=10, height=4)
p1<-UpSet(m,right_annotation = NULL, 
          top_annotation = HeatmapAnnotation("Question number" = as.numeric(comb_degree(m)), col = list("Question number" = col_fun),
                                             Recall = anno_barplot(as.numeric(corresponding_df["Recall",]), 
                                                                   border = FALSE, 
                                                                   gp = gpar(fill = "black"), 
                                                                   height = unit(6, "cm")
                                             ), 
                                             annotation_name_side = "left", 
                                             annotation_name_rot = 0), 
          set_order = NULL, 
          comb_order = order(corresponding_df["Recall",],decreasing = TRUE)
)
print(p1)
dev.off()

#Upset plots F1
col_fun = colorRamp2(c(0, 5), c("#e3f9fd", "#065279"))
pdf(paste0(output_dir,"//geom_bar_summary_Sarcopenia_F1.pdf"),width=10, height=4)
p1<-UpSet(m,right_annotation = NULL, 
          top_annotation = HeatmapAnnotation("Question number" = as.numeric(comb_degree(m)), col = list("Question number" = col_fun),
                                             F1 = anno_barplot(as.numeric(corresponding_df["F1",]), 
                                                               border = FALSE, 
                                                               gp = gpar(fill = "black"), 
                                                               height = unit(6, "cm")
                                             ), 
                                             annotation_name_side = "left", 
                                             annotation_name_rot = 0), 
          set_order = NULL, 
          comb_order = order(corresponding_df["F1",],decreasing = TRUE)
)
print(p1)
dev.off()


#Upset plots Work_reduced
col_fun = colorRamp2(c(0, 5), c("#e3f9fd", "#065279"))
pdf(paste0(output_dir,"//geom_bar_summary_Sarcopenia_Work_reduced.pdf"),width=10, height=4)
p1<-UpSet(m,right_annotation = NULL, 
          top_annotation = HeatmapAnnotation("Question number" = as.numeric(comb_degree(m)), col = list("Question number" = col_fun),
                                             Work_reduced = anno_barplot(as.numeric(corresponding_df["Work_reduced",]), 
                                                                         border = FALSE, 
                                                                         gp = gpar(fill = "black"), 
                                                                         height = unit(6, "cm")
                                             ), 
                                             annotation_name_side = "left", 
                                             annotation_name_rot = 0), 
          set_order = NULL, 
          comb_order = order(corresponding_df["Work_reduced",],decreasing = TRUE)
)
print(p1)
dev.off()


##############################
####Baichuan
input_dataset[(input_dataset == "N")&(!is.na(input_dataset))] = "No" 
input_dataset[(input_dataset == "Y")&(!is.na(input_dataset))] = "Yes"
metrics<-function(table){
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
  
  print(paste0("TP: ",TP,"; FP: ",FP,"; TN: ",TN,"; FN: ",FN))
  print(paste0("precision: ",precision))
  print(paste0("recall: ",recall))
  print(paste0("F1: ",F1))
}
Manullay_contingency_tb<-cal_contingency_tb(data=input_dataset,colume1="Outcome",colume2="Manullay_action6",na.ignor=TRUE)
Manullay_contingency_tb
metrics(table = Manullay_contingency_tb)
GPT35_contingency_tb<-cal_contingency_tb(data=input_dataset,colume1="Outcome",colume2="GPT35_AllYES_summary",na.ignor=TRUE)
GPT35_contingency_tb
metrics(table = GPT35_contingency_tb)
GPT4_contingency_tb<-cal_contingency_tb(data=input_dataset,colume1="Outcome",colume2="GPT4_AllYES_summary",na.ignor=TRUE)
GPT4_contingency_tb
metrics(table = GPT4_contingency_tb)
Random_contingency_tb<-cal_contingency_tb(data=input_dataset,colume1="Outcome",colume2="Random_Direct",na.ignor=TRUE)
Random_contingency_tb
metrics(table = Random_contingency_tb)
strategy_1_contingency_tb<-cal_contingency_tb(data=input_dataset,colume1="Outcome",colume2="strategy_1",na.ignor=TRUE)
strategy_1_contingency_tb
metrics(table = strategy_1_contingency_tb)


