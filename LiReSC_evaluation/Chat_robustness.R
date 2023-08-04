
library(ggplot2)
# library(ggprism)
library(tidyverse)

####00 input data####
output_dir <- choose.dir(default = "D:\\output_dir", caption = "choose the output folder")
data_dir <- choose.dir(default = "D:\\data_dir", caption = "choose the folder storing input_data.txt file")

input_dataset_path <- choose.files(default = data_dir, caption = "choose the input_data.txt file",
                                   multi = TRUE, filters = Filters,
                                   index = nrow(Filters))
input_dataset<- read.csv(input_dataset_path, header = TRUE,sep=",", stringsAsFactors=FALSE)
colnames(input_dataset)
input_dataset$id<-factor(input_dataset$id,levels = sort(unique(input_dataset$id), na.last = TRUE))

#### ibd ####
#plot
input_dataset<-input_dataset[,c("id","Chat_species","Chat_disease","Chat_age","Chat_research_type","Chat_Protein_related")]

colnames(input_dataset)[1]<-"Publication_ID"
data_long <- input_dataset %>% gather(key = "variable", value = "value", -Publication_ID)
pdf(paste0(output_dir,"//geom_bar_fill_ibd.pdf"),width=9, height=6)
p1<-ggplot(data_long, aes(x = Publication_ID, fill = value)) +
  geom_bar(position = "fill") +
  facet_wrap(~ variable, ncol = 1) + 
  scale_fill_manual(values = c("#FFC200", "#560EAD", "#0C5DA5"))+ 
  theme(strip.background = element_rect(
    color="black", fill=NA, size=1.5, linetype="blank"
  ),
  panel.background =element_rect(fill=NA),
  axis.line = element_line(colour = "black"))
print(p1)
dev.off()

#calculate the proportion of max subclass
percentage_table_templet <- data.frame(matrix(ncol = 5, nrow = 0))
colnames(percentage_table_templet) <- c('Question', 'publication_num', 
                                    'max_percentage',"min_percentage",
                                    "mean_percentage")
percentage_table<-percentage_table_templet

for (question_index in 2:ncol(input_dataset)) {
  temp_question<-colnames(input_dataset)[question_index]
  temp_table<-input_dataset[,c(1,question_index)]
  colnames(temp_table)[2]<-"question"
  result <- temp_table %>%
    group_by(Publication_ID) %>%
    summarize(max_percent = max(table(question)) / sum(table(question)) * 100)
  
  temp_percentage_table<-percentage_table_templet
  temp_percentage_table[1,"Question"]<-temp_question
  temp_percentage_table[1,"publication_num"]<-nrow(result)
  temp_percentage_table[1,"max_percentage"]<-max(result$max_percent)
  temp_percentage_table[1,"min_percentage"]<-min(result$max_percent)
  temp_percentage_table[1,"mean_percentage"]<-mean(result$max_percent)
  
  percentage_table<-rbind(percentage_table,temp_percentage_table)
}

write.table(percentage_table,file = paste0(output_dir, "\\", "percentage_table_ibd.txt"), sep = "\t")


#### Sarcopenia ####
#plot
input_dataset<-input_dataset[,c("id","Chat_species","Chat_disease","Chat_control","Chat_research_type","Chat_outcome")]

colnames(input_dataset)[1]<-"Publication_ID"
data_long <- input_dataset %>% gather(key = "variable", value = "value", -Publication_ID)
pdf(paste0(output_dir,"//geom_bar_fill_Sarcopenia.pdf"),width=9, height=6)
p1<-ggplot(data_long, aes(x = Publication_ID, fill = value)) +
  geom_bar(position = "fill") +
  facet_wrap(~ variable, ncol = 1) + 
  scale_fill_manual(values = c("#FFC200", "#560EAD", "#0C5DA5"))+ 
  theme(strip.background = element_rect(
    color="black", fill=NA, size=1.5, linetype="blank"
  ),
  panel.background =element_rect(fill=NA),
  axis.line = element_line(colour = "black"))
print(p1)
dev.off()

#calculate the proportion of max subclass
percentage_table_templet <- data.frame(matrix(ncol = 5, nrow = 0))
colnames(percentage_table_templet) <- c('Question', 'publication_num', 
                                        'max_percentage',"min_percentage",
                                        "mean_percentage")
percentage_table<-percentage_table_templet

for (question_index in 2:ncol(input_dataset)) {
  temp_question<-colnames(input_dataset)[question_index]
  temp_table<-input_dataset[,c(1,question_index)]
  colnames(temp_table)[2]<-"question"
  result <- temp_table %>%
    group_by(Publication_ID) %>%
    summarize(max_percent = max(table(question)) / sum(table(question)) * 100)
  
  temp_percentage_table<-percentage_table_templet
  temp_percentage_table[1,"Question"]<-temp_question
  temp_percentage_table[1,"publication_num"]<-nrow(result)
  temp_percentage_table[1,"max_percentage"]<-max(result$max_percent)
  temp_percentage_table[1,"min_percentage"]<-min(result$max_percent)
  temp_percentage_table[1,"mean_percentage"]<-mean(result$max_percent)
  
  percentage_table<-rbind(percentage_table,temp_percentage_table)
}

write.table(percentage_table,file = paste0(output_dir, "\\", "percentage_table_Sarcopenia.txt"), sep = "\t")


#### dm ####
#plot

input_dataset<-input_dataset[,c("id","Chat_species","Chat_disease_dm","Chat_research_type","Chat_disease_p")]

colnames(input_dataset)[1]<-"Publication_ID"
data_long <- input_dataset %>% gather(key = "variable", value = "value", -Publication_ID)
pdf(paste0(output_dir,"//geom_bar_fill_dm.pdf"),width=9, height=6)
p1<-ggplot(data_long, aes(x = Publication_ID, fill = value)) +
  geom_bar(position = "fill") +
  facet_wrap(~ variable, ncol = 1) + 
  scale_fill_manual(values = c("#FFC200", "#560EAD", "#0C5DA5"))+ 
  theme(strip.background = element_rect(
    color="black", fill=NA, size=1.5, linetype="blank"
  ),
  panel.background =element_rect(fill=NA),
  axis.line = element_line(colour = "black"))
print(p1)
dev.off()

#calculate the proportion of max subclass
percentage_table_templet <- data.frame(matrix(ncol = 5, nrow = 0))
colnames(percentage_table_templet) <- c('Question', 'publication_num', 
                                        'max_percentage',"min_percentage",
                                        "mean_percentage")
percentage_table<-percentage_table_templet

for (question_index in 2:ncol(input_dataset)) {
  temp_question<-colnames(input_dataset)[question_index]
  temp_table<-input_dataset[,c(1,question_index)]
  colnames(temp_table)[2]<-"question"
  result <- temp_table %>%
    group_by(Publication_ID) %>%
    summarize(max_percent = max(table(question)) / sum(table(question)) * 100)
  
  temp_percentage_table<-percentage_table_templet
  temp_percentage_table[1,"Question"]<-temp_question
  temp_percentage_table[1,"publication_num"]<-nrow(result)
  temp_percentage_table[1,"max_percentage"]<-max(result$max_percent)
  temp_percentage_table[1,"min_percentage"]<-min(result$max_percent)
  temp_percentage_table[1,"mean_percentage"]<-mean(result$max_percent)
  
  percentage_table<-rbind(percentage_table,temp_percentage_table)
}

write.table(percentage_table,file = paste0(output_dir, "\\", "percentage_table_dm.txt"), sep = "\t")

#### GBM_CD8 ####
#plot

input_dataset<-input_dataset[,c("id","Chat_species","Chat_disease","Chat_treatment","Chat_research_type")]
input_dataset <- input_dataset[complete.cases(input_dataset), ]

colnames(input_dataset)[1]<-"Publication_ID"
data_long <- input_dataset %>% gather(key = "variable", value = "value", -Publication_ID)
pdf(paste0(output_dir,"//geom_bar_fill_GBM_CD8.pdf"),width=9, height=6)
p1<-ggplot(data_long, aes(x = Publication_ID, fill = value)) +
  geom_bar(position = "fill") +
  facet_wrap(~ variable, ncol = 1) + 
  scale_fill_manual(values = c("#FFC200", "#560EAD", "#0C5DA5"))+ 
  theme(strip.background = element_rect(
    color="black", fill=NA, size=1.5, linetype="blank"
  ),
  panel.background =element_rect(fill=NA),
  axis.line = element_line(colour = "black"))
print(p1)
dev.off()

#calculate the proportion of max subclass
percentage_table_templet <- data.frame(matrix(ncol = 5, nrow = 0))
colnames(percentage_table_templet) <- c('Question', 'publication_num', 
                                        'max_percentage',"min_percentage",
                                        "mean_percentage")
percentage_table<-percentage_table_templet

for (question_index in 2:ncol(input_dataset)) {
  temp_question<-colnames(input_dataset)[question_index]
  temp_table<-input_dataset[,c(1,question_index)]
  colnames(temp_table)[2]<-"question"
  result <- temp_table %>%
    group_by(Publication_ID) %>%
    summarize(max_percent = max(table(question)) / sum(table(question)) * 100)
  
  temp_percentage_table<-percentage_table_templet
  temp_percentage_table[1,"Question"]<-temp_question
  temp_percentage_table[1,"publication_num"]<-nrow(result)
  temp_percentage_table[1,"max_percentage"]<-max(result$max_percent)
  temp_percentage_table[1,"min_percentage"]<-min(result$max_percent)
  temp_percentage_table[1,"mean_percentage"]<-mean(result$max_percent)
  
  percentage_table<-rbind(percentage_table,temp_percentage_table)
}

write.table(percentage_table,file = paste0(output_dir, "\\", "percentage_table_GBM_CD8.txt"), sep = "\t")



#### summary ####
output_dir <- choose.dir(default = "D:\\output_dir", caption = "choose the output folder")
data_dir <- choose.dir(default = "D:\\data_dir", caption = "choose the folder storing input_data.txt file")

input_dataset_path <- choose.files(default = data_dir, caption = "choose the input_data.txt file",
                                   multi = TRUE, filters = Filters,
                                   index = nrow(Filters))
input_dataset<- read.csv(input_dataset_path, header = TRUE,sep="\t", stringsAsFactors=FALSE)
colnames(input_dataset)

#Plot
input_dataset$Question<-factor(input_dataset$Question,levels = c(input_dataset$Question))
input_dataset$Robustness_score<-input_dataset$Robustness_score/100
pdf(paste0(output_dir,"//summary Robustness_score.pdf"),width=8, height=5)
ggplot(input_dataset,aes(Question,Robustness_score))+
  geom_bar(stat="identity",position="dodge",width = 0.6) +
  labs(x=NULL,y="Robustness score")+
  theme_prism(
              base_fontface = "plain",
              base_family = "sans",
              base_size = 10,
              axis_text_angle = 90)
dev.off()



