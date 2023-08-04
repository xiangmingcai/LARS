


#### PMID35377001 start ####
output_dir <- choose.dir(default = "D:\\output_dir", caption = "choose the output folder")
data_dir <- choose.dir(default = "D:\\data_dir", caption = "choose the folder storing input_data.txt file")

# 00 input data #
input_dataset_path <- choose.files(default = data_dir, caption = "choose the PMID35377001.txt file",
                                                        multi = TRUE, filters = Filters,
                                                        index = nrow(Filters))
input_dataset<- read.csv(input_dataset_path, header = TRUE,sep="\t", stringsAsFactors=FALSE)
colnames(input_dataset)

input_dataset<-input_dataset[(input_dataset$Treatment.response %in% c("PD","SD")),]
input_dataset<-input_dataset[(!is.na(input_dataset$CD8.TIL.value)),]
input_dataset$CD8.TIL.value
input_dataset$CD8.TIL.value[input_dataset$CD8.TIL.value=="<87"]<-"low_CD8_TIL"
input_dataset$CD8.TIL.value[input_dataset$CD8.TIL.value=="¡Ý87"]<-"high_CD8_TIL"
library(gmodels)
CrossTable(input_dataset$CD8.TIL.value,input_dataset$Treatment.response,chisq=T,format =c("SPSS"))
#### PMID35377001 end ####

#### PMID37148198 start ####
output_dir <- choose.dir(default = "D:\\output_dir", caption = "choose the output folder")
data_dir <- choose.dir(default = "D:\\data_dir", caption = "choose the folder storing input_data.txt file")

input_dataset_path <- choose.files(default = data_dir, caption = "choose the PMID35377001.txt file",
                                   multi = TRUE, filters = Filters,
                                   index = nrow(Filters))
input_dataset<- read.csv(input_dataset_path, header = TRUE,sep="\t", stringsAsFactors=FALSE)
colnames(input_dataset)

library(ggplot2)
library(ggsignif)

pdf(paste0(output_dir,"//PMID37148198_treatment_response.pdf"),width=3, height=4)
ggplot(input_dataset,aes(Treatment.response,CD8.TIL.value))+
  geom_point(data=input_dataset,aes(Treatment.response,CD8.TIL.value),size=2,pch=20,color="black") +
  stat_summary(geom = "errorbar", width = 0.3)+
  geom_signif(comparisons = list(c("PD","SD")),# 设置需要比较的组
              map_signif_level = T,
              test = "t.test", 
              tip_length = c(c(0.01,0.01)),#横线下方的竖线设置
              size=0.8,color="black")+
  theme_bw()
dev.off()

#### PMID37148198 end ####