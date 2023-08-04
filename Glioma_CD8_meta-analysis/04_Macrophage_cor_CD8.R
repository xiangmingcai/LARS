
library(survival)
library(survminer)
library(rms)
library(stringr)
library(ggplot2)
library(ggpubr)

output_dir <- choose.dir(default = "D:\\output_dir", caption = "choose the output folder")
data_dir <- choose.dir(default = "D:\\data_dir", caption = "choose the folder storing input_data.txt file")
#### PMID29910795_with_MF start ####
# 00 input data #
input_dataset_path_PMID29910795_with_MF <- choose.files(default = data_dir, caption = "choose the input_dataset_path_PMID29910795_with_MF.txt file",
                                   multi = TRUE, filters = Filters,
                                   index = nrow(Filters))
input_dataset<- read.csv(input_dataset_path_PMID29910795_with_MF, header = TRUE,sep="\t", stringsAsFactors=FALSE)
colnames(input_dataset)
#### All population start ####
temp_table<-input_dataset
temp_table$CD8_count<-as.numeric(temp_table$CD8_count)
temp_table$CD8_count_log10<-log10(temp_table$CD8_count)

#boxplot
pdf(paste0(output_dir,"//PMID29910795_boxplot_PDL1_CD8_count.pdf"),width=4, height=5)
p <- ggboxplot(temp_table, x = "PDL1", y = "CD8_count",
               color = "PDL1", 
               # palette = "aaas",
               add = "jitter")+ 
  stat_compare_means(method = "t.test")
print(p)
dev.off()
#OS KM curve
fit <- survfit(Surv(OS_months,OS_Status) ~ PDL1,  
               data = temp_table)
print(fit)
summary(fit)
pdf(paste0(output_dir,"//PMID29910795_boxplot_OS_survplot_PDL1.pdf"),width=6, height=6)
os_plot<-ggsurvplot(fit, data = temp_table,
                    conf.int = TRUE,
                    risk.table = TRUE,
                    pval = TRUE)
print(os_plot)
dev.off()
#PFS KM curve
fit <- survfit(Surv(PFS_months,PFS_Status) ~ PDL1,  
               data = temp_table)
print(fit)
summary(fit)
pdf(paste0(output_dir,"//PMID29910795_boxplot_PFS_survplot_PDL1.pdf"),width=6, height=6)
pfs_plot<-ggsurvplot(fit, data = temp_table,
                    conf.int = TRUE,
                    risk.table = TRUE,
                    pval = TRUE)
print(pfs_plot)
dev.off()

#uni cox
fit<-coxph(Surv(OS_months,OS_Status) ~ PDL1,  
           data = temp_table)
summary(fit)
fit<-coxph(Surv(PFS_months,PFS_Status) ~ PDL1,  
           data = temp_table)
summary(fit)

#multi cox
fit<-coxph(Surv(OS_months,OS_Status) ~ Age+Gender+MGMT.status+CD8_count_log10+PDL1,  
           data = temp_table)
summary(fit)
fit<-coxph(Surv(PFS_months,PFS_Status) ~ Age+Gender+MGMT.status+CD8_count_log10+PDL1,  
           data = temp_table)
summary(fit)

# age cor PDL1/CD8
#boxplot
pdf(paste0(output_dir,"//PMID29910795_boxplot_PDL1_Age.pdf"),width=4, height=5)
p <- ggboxplot(temp_table, x = "PDL1", y = "Age",
               color = "PDL1", 
               # palette = "aaas",
               add = "jitter")+ 
  stat_compare_means(method = "t.test")
print(p)
dev.off()

pdf(paste0(output_dir,"//PMID29910795_boxplot_PDL1_CD8_count_log10.pdf"),width=4, height=5)
p <- ggboxplot(temp_table, x = "PDL1", y = "CD8_count_log10",
               color = "PDL1", 
               # palette = "aaas",
               add = "jitter")+ 
  stat_compare_means(method = "t.test")
print(p)
dev.off()

pdf(paste0(output_dir,"//PMID29910795_geom_point_CD8_count_log10_Age.pdf"),width=5, height=5)
gg <- ggplot(temp_table, aes(x=CD8_count_log10, y=Age)) + 
  geom_point(aes(col=PDL1)) + 
  geom_smooth(method="lm", se=T) + 
  stat_cor(data=temp_table, method = "pearson")+ theme_light()


print(gg)
dev.off()

#### All population end ####

#### Total excision start ####

subgroup_label<-"Total excision"
temp_table<-input_dataset[(input_dataset$Complete.resection==subgroup_label),]
#boxplot
pdf(paste0(output_dir,"//PMID29910795_",subgroup_label,"_boxplot_PDL1_CD8_count.pdf"),width=4, height=5)
p <- ggboxplot(temp_table, x = "PDL1", y = "CD8_count",
               color = "PDL1", 
               # palette = "aaas",
               add = "jitter")+ 
  stat_compare_means(method = "t.test")
print(p)
dev.off()
#OS KM curve
fit <- survfit(Surv(OS_months,OS_Status) ~ PDL1,  
               data = temp_table)
print(fit)
summary(fit)
pdf(paste0(output_dir,"//PMID29910795_",subgroup_label,"_boxplot_OS_survplot_PDL1.pdf"),width=6, height=6)
os_plot<-ggsurvplot(fit, data = temp_table,
                    conf.int = TRUE,
                    risk.table = TRUE,
                    pval = TRUE)
print(os_plot)
dev.off()
#PFS KM curve
fit <- survfit(Surv(PFS_months,PFS_Status) ~ PDL1,  
               data = temp_table)
print(fit)
summary(fit)
pdf(paste0(output_dir,"//PMID29910795_",subgroup_label,"_boxplot_PFS_survplot_PDL1.pdf"),width=6, height=6)
pfs_plot<-ggsurvplot(fit, data = temp_table,
                     conf.int = TRUE,
                     risk.table = TRUE,
                     pval = TRUE)
print(pfs_plot)
dev.off()


#### Total excision end ####
#### Subtotal excision start ####

subgroup_label<-"Subtotal excision"
temp_table<-input_dataset[(input_dataset$Complete.resection==subgroup_label),]
#boxplot
pdf(paste0(output_dir,"//PMID29910795_",subgroup_label,"_boxplot_PDL1_CD8_count.pdf"),width=4, height=5)
p <- ggboxplot(temp_table, x = "PDL1", y = "CD8_count",
               color = "PDL1", 
               # palette = "aaas",
               add = "jitter")+ 
  stat_compare_means(method = "t.test")
print(p)
dev.off()
#OS KM curve
fit <- survfit(Surv(OS_months,OS_Status) ~ PDL1,  
               data = temp_table)
print(fit)
summary(fit)
pdf(paste0(output_dir,"//PMID29910795_",subgroup_label,"_boxplot_OS_survplot_PDL1.pdf"),width=6, height=6)
os_plot<-ggsurvplot(fit, data = temp_table,
                    conf.int = TRUE,
                    risk.table = TRUE,
                    pval = TRUE)
print(os_plot)
dev.off()
#PFS KM curve
fit <- survfit(Surv(PFS_months,PFS_Status) ~ PDL1,  
               data = temp_table)
print(fit)
summary(fit)
pdf(paste0(output_dir,"//PMID29910795_",subgroup_label,"_boxplot_PFS_survplot_PDL1.pdf"),width=6, height=6)
pfs_plot<-ggsurvplot(fit, data = temp_table,
                     conf.int = TRUE,
                     risk.table = TRUE,
                     pval = TRUE)
print(pfs_plot)
dev.off()


#### Subtotal excision end ####
#### Partial excision start ####

subgroup_label<-"Partial excision"
temp_table<-input_dataset[(input_dataset$Complete.resection==subgroup_label),]
#boxplot
pdf(paste0(output_dir,"//PMID29910795_",subgroup_label,"_boxplot_PDL1_CD8_count.pdf"),width=4, height=5)
p <- ggboxplot(temp_table, x = "PDL1", y = "CD8_count",
               color = "PDL1", 
               # palette = "aaas",
               add = "jitter")+ 
  stat_compare_means(method = "t.test")
print(p)
dev.off()
#OS KM curve
fit <- survfit(Surv(OS_months,OS_Status) ~ PDL1,  
               data = temp_table)
print(fit)
summary(fit)
pdf(paste0(output_dir,"//PMID29910795_",subgroup_label,"_boxplot_OS_survplot_PDL1.pdf"),width=6, height=6)
os_plot<-ggsurvplot(fit, data = temp_table,
                    conf.int = TRUE,
                    risk.table = TRUE,
                    pval = TRUE)
print(os_plot)
dev.off()
#PFS KM curve
fit <- survfit(Surv(PFS_months,PFS_Status) ~ PDL1,  
               data = temp_table)
print(fit)
summary(fit)
pdf(paste0(output_dir,"//PMID29910795_",subgroup_label,"_boxplot_PFS_survplot_PDL1.pdf"),width=6, height=6)
pfs_plot<-ggsurvplot(fit, data = temp_table,
                     conf.int = TRUE,
                     risk.table = TRUE,
                     pval = TRUE)
print(pfs_plot)
dev.off()


#### Partial excision end ####
#### methylated start ####

subgroup_label<-"methylated"
temp_table<-input_dataset[(input_dataset$MGMT.status==subgroup_label),]
#boxplot
pdf(paste0(output_dir,"//PMID29910795_",subgroup_label,"_boxplot_PDL1_CD8_count.pdf"),width=4, height=5)
p <- ggboxplot(temp_table, x = "PDL1", y = "CD8_count",
               color = "PDL1", 
               # palette = "aaas",
               add = "jitter")+ 
  stat_compare_means(method = "t.test")
print(p)
dev.off()
#OS KM curve
fit <- survfit(Surv(OS_months,OS_Status) ~ PDL1,  
               data = temp_table)
print(fit)
summary(fit)
pdf(paste0(output_dir,"//PMID29910795_",subgroup_label,"_boxplot_OS_survplot_PDL1.pdf"),width=6, height=6)
os_plot<-ggsurvplot(fit, data = temp_table,
                    conf.int = TRUE,
                    risk.table = TRUE,
                    pval = TRUE)
print(os_plot)
dev.off()
#PFS KM curve
fit <- survfit(Surv(PFS_months,PFS_Status) ~ PDL1,  
               data = temp_table)
print(fit)
summary(fit)
pdf(paste0(output_dir,"//PMID29910795_",subgroup_label,"_boxplot_PFS_survplot_PDL1.pdf"),width=6, height=6)
pfs_plot<-ggsurvplot(fit, data = temp_table,
                     conf.int = TRUE,
                     risk.table = TRUE,
                     pval = TRUE)
print(pfs_plot)
dev.off()


#### methylated end ####
#### unmethylated start ####

subgroup_label<-"unmethylated"
temp_table<-input_dataset[(input_dataset$MGMT.status==subgroup_label),]
#boxplot
pdf(paste0(output_dir,"//PMID29910795_",subgroup_label,"_boxplot_PDL1_CD8_count.pdf"),width=4, height=5)
p <- ggboxplot(temp_table, x = "PDL1", y = "CD8_count",
               color = "PDL1", 
               # palette = "aaas",
               add = "jitter")+ 
  stat_compare_means(method = "t.test")
print(p)
dev.off()
#OS KM curve
fit <- survfit(Surv(OS_months,OS_Status) ~ PDL1,  
               data = temp_table)
print(fit)
summary(fit)
pdf(paste0(output_dir,"//PMID29910795_",subgroup_label,"_boxplot_OS_survplot_PDL1.pdf"),width=6, height=6)
os_plot<-ggsurvplot(fit, data = temp_table,
                    conf.int = TRUE,
                    risk.table = TRUE,
                    pval = TRUE)
print(os_plot)
dev.off()
#PFS KM curve
fit <- survfit(Surv(PFS_months,PFS_Status) ~ PDL1,  
               data = temp_table)
print(fit)
summary(fit)
pdf(paste0(output_dir,"//PMID29910795_",subgroup_label,"_boxplot_PFS_survplot_PDL1.pdf"),width=6, height=6)
pfs_plot<-ggsurvplot(fit, data = temp_table,
                     conf.int = TRUE,
                     risk.table = TRUE,
                     pval = TRUE)
print(pfs_plot)
dev.off()


#### unmethylated end ####

#### PMID29910795_with_MF end ####



#### PMID33130898_with_MF start ####
# 00 input data #
input_dataset_path_PMID33130898_with_MF <- choose.files(default = data_dir, caption = "choose the input_dataset_path_PMID33130898_with_MF.txt file",
                                                        multi = TRUE, filters = Filters,
                                                        index = nrow(Filters))
input_dataset<- read.csv(input_dataset_path_PMID33130898_with_MF, header = TRUE,sep="\t", stringsAsFactors=FALSE)
colnames(input_dataset)
#### All population start ####
temp_table<-input_dataset
temp_table$CD8_count<-as.numeric(temp_table$CD8_count)
temp_table$CD8_count_log10<-log10(temp_table$CD8_count)
temp_table$CD68_grade<-as.factor(temp_table$CD68_grade)
temp_table$MGMT.status[temp_table$MGMT.status=="unsure"]<-NA
# temp_table$MGMT.status<-as.factor(temp_table$MGMT.status)
#boxplot
pdf(paste0(output_dir,"//PMID33130898_boxplot_CD68_grade_CD8_count.pdf"),width=5, height=5)
p <- ggboxplot(subset(temp_table, !is.na(CD68_grade)), x = "CD68_grade", y = "CD8_count",
               color = "CD68_grade", 
               # palette = "aaas",
               add = "jitter")+ 
  stat_compare_means(comparisons = list( c("0", "1"), 
                                         c("1", "2"), 
                                         c("0", "2") ))+ 
  stat_compare_means(label.y = 250)+
  stat_compare_means(method = "anova",label.y = 270)
print(p)
dev.off()

pdf(paste0(output_dir,"//PMID33130898_boxplot_CD68_grade_CD8_count_log10.pdf"),width=5, height=5)
p <- ggboxplot(subset(temp_table, !is.na(CD68_grade)), x = "CD68_grade", y = "CD8_count_log10",
               color = "CD68_grade", 
               # palette = "aaas",
               add = "jitter")+ 
  stat_compare_means(comparisons = list( c("0", "1"), 
                                         c("1", "2"), 
                                         c("0", "2") ))+ 
  stat_compare_means(label.y = 3.5)+
  stat_compare_means(method = "anova",label.y = 3.7)
print(p)
dev.off()


#OS KM curve
fit <- survfit(Surv(OS_months,OS_Status) ~ CD68_grade,  
               data = temp_table)
print(fit)
summary(fit)
pdf(paste0(output_dir,"//PMID33130898_boxplot_OS_survplot_CD68_grade.pdf"),width=6, height=6)
os_plot<-ggsurvplot(fit, data = temp_table,
                    conf.int = TRUE,
                    risk.table = TRUE,
                    pval = TRUE)
print(os_plot)
dev.off()

#uni cox
fit<-coxph(Surv(OS_months,OS_Status) ~ CD68_grade,  
           data = temp_table)
summary(fit)

#multi cox
fit<-coxph(Surv(OS_months,OS_Status) ~ Age+Gender+IDH1.status+MGMT.status+CD8_count_log10+CD68_grade,  
           data = temp_table)
summary(fit)

# age cor CD68_grade/CD8
#boxplot
pdf(paste0(output_dir,"//PMID33130898_boxplot_CD68_grade_Age.pdf"),width=4, height=5)
p <- ggboxplot(subset(temp_table, !is.na(CD68_grade)), x = "CD68_grade", y = "Age",
               color = "CD68_grade", 
               # palette = "aaas",
               add = "jitter")+ 
  stat_compare_means(method = "t.test",comparisons = list( c("0", "1"), 
                                                           c("1", "2"), 
                                                           c("0", "2")))+ 
  stat_compare_means(label.y = 85)+
  stat_compare_means(method = "anova",label.y = 90)
print(p)
dev.off()


pdf(paste0(output_dir,"//PMID33130898_geom_point_CD8_count_log10_Age.pdf"),width=5, height=5)
gg <- ggplot(temp_table, aes(x=CD8_count_log10, y=Age)) + 
  geom_point(aes(col=CD68_grade)) + 
  geom_smooth(method="lm", se=T) + 
  stat_cor(data=temp_table, method = "pearson")+ theme_light()


print(gg)
dev.off()

pdf(paste0(output_dir,"//PMID33130898_geom_point_CD8_count_Age.pdf"),width=5, height=5)
gg <- ggplot(temp_table, aes(x=CD8_count, y=Age)) + 
  geom_point(aes(col=CD68_grade)) + 
  geom_smooth(method="lm", se=T) + 
  stat_cor(data=temp_table, method = "pearson")+ theme_light()


print(gg)
dev.off()

#### All population end ####

#### PMID33130898_with_MF end ####


#### PMID36707424_with_MF start ####
# 00 input data #
input_dataset_path_PMID36707424_with_MF <- choose.files(default = data_dir, caption = "choose the input_dataset_path_PMID36707424_with_MF.txt file",
                                                        multi = TRUE, filters = Filters,
                                                        index = nrow(Filters))
input_dataset<- read.csv(input_dataset_path_PMID36707424_with_MF, header = TRUE,sep="\t", stringsAsFactors=FALSE)
colnames(input_dataset)
#### Primary start ####
temp_table<-input_dataset
temp_table<-temp_table[(temp_table$PR_Type=="Primary"),]
temp_table$CD8_percentage<-as.numeric(temp_table$CD8_percentage)
temp_table$PDL1<-as.numeric(temp_table$PDL1)
temp_table$CD163<-as.numeric(temp_table$CD163)
# CD8_percentage cor PDL1
pdf(paste0(output_dir,"//PMID36707424_Primary_geom_point_CD8_percentage_PDL1.pdf"),width=5, height=5)
gg <- ggplot(temp_table, aes(x=CD8_percentage, y=PDL1)) + 
  geom_point() +
  geom_smooth(method="lm", se=T) + 
  stat_cor(data=temp_table, method = "pearson")+ theme_light()
print(gg)
dev.off()
# CD8_percentage cor CD163
pdf(paste0(output_dir,"//PMID36707424_Primary_geom_point_CD8_percentage_CD163.pdf"),width=5, height=5)
gg <- ggplot(temp_table, aes(x=CD8_percentage, y=CD163)) + 
  geom_point() +
  geom_smooth(method="lm", se=T) + 
  stat_cor(data=temp_table, method = "pearson")+ theme_light()
print(gg)
dev.off()

#uni cox
fit<-coxph(Surv(OS_months,OS_Status) ~ CD163,  
           data = temp_table)
summary(fit)

#multi cox
fit<-coxph(Surv(OS_months,OS_Status) ~ CD8_percentage+PDL1+CD163,  
           data = temp_table)
summary(fit)
#### Primary end ####
#### Recurrence start ####
temp_table<-input_dataset
temp_table<-temp_table[(temp_table$PR_Type=="Recurrence"),]
temp_table$CD8_percentage<-as.numeric(temp_table$CD8_percentage)
temp_table$PDL1<-as.numeric(temp_table$PDL1)
temp_table$CD163<-as.numeric(temp_table$CD163)
# CD8_percentage cor PDL1
pdf(paste0(output_dir,"//PMID36707424_Recurrence_geom_point_CD8_percentage_PDL1.pdf"),width=5, height=5)
gg <- ggplot(temp_table, aes(x=CD8_percentage, y=PDL1)) + 
  geom_point() +
  geom_smooth(method="lm", se=T) + 
  stat_cor(data=temp_table, method = "pearson")+ theme_light()
print(gg)
dev.off()
# CD8_percentage cor CD163
pdf(paste0(output_dir,"//PMID36707424_Recurrence_geom_point_CD8_percentage_CD163.pdf"),width=5, height=5)
gg <- ggplot(temp_table, aes(x=CD8_percentage, y=CD163)) + 
  geom_point() +
  geom_smooth(method="lm", se=T) + 
  stat_cor(data=temp_table, method = "pearson")+ theme_light()
print(gg)
dev.off()

#uni cox
fit<-coxph(Surv(OS_months,OS_Status) ~ CD163,  
           data = temp_table)
summary(fit)

#multi cox
fit<-coxph(Surv(OS_months,OS_Status) ~ CD8_percentage+PDL1+CD163,  
           data = temp_table)
summary(fit)
#### Recurrence end ####


#### PMID36707424_with_MF end ####