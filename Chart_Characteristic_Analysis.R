library(tidyverse)
library(readxl)
library(lmerTest)
library(stringr)

setwd("Emory_Projects/Anthony_Mendel/")

##########################################
### Prepare Data                       ###
##########################################

# Load Analytic Data
times = readRDS("Data/Final_Data/adjusted_times.RDS")
human = readRDS("Data/Final_Data/human_analytic.RDS")
aug = readRDS("Data/Final_Data/aug_analytic.RDS")

# Chart Level Data
human_charts = human %>% group_by(`Patient ID`) %>% summarize(num_events = n(), accuracy = 100*sum(accurate)/n())
human_charts$arm = "human_alone"
aug_charts = aug %>% group_by(`Patient ID`) %>% summarize(num_events = n(), accuracy = 100*sum(accurate)/n())
aug_charts$arm = "augmented"
full_charts = rbind(human_charts, aug_charts) %>% rename(patient_id = `Patient ID`)

# Combine and Dichotomoize Chart Characteristics
fulldata = full_charts %>% left_join(times, by = c("patient_id", "arm")) 
fulldata$arm = factor(fulldata$arm, levels = c("human_alone", "augmented"))
fulldata$large_docs = factor(ifelse(fulldata$num_docs > median(fulldata$num_docs), 1,0))
fulldata$large_events = factor(ifelse(fulldata$num_events > median(fulldata$num_events), 1,0))
fulldata$later_batch = factor(ifelse(fulldata$order_num > median(fulldata$order_num), 1,0))
fulldata$NSCLC = factor(ifelse(fulldata$cancer_type == "NSCLC",1,0))

##########################################
### Stratify by Chart Characteristics  ###
##########################################
fulldata %>% group_by(arm, later_batch) %>% summarize(avg_accuracy = mean(accuracy))
fulldata %>% group_by(arm, large_docs) %>% summarize(avg_accuracy = mean(accuracy))
fulldata %>% group_by(arm, large_events) %>% summarize(avg_accuracy = mean(accuracy))
fulldata %>% group_by(arm, cancer_type) %>% summarize(avg_accuracy = mean(accuracy))

fulldata %>% group_by(arm, later_batch) %>% summarize(avg_minutes = mean(minutes))
fulldata %>% group_by(arm, large_docs) %>% summarize(avg_minutes = mean(minutes))
fulldata %>% group_by(arm, large_events) %>% summarize(avg_minutes = mean(minutes))
fulldata %>% group_by(arm, cancer_type) %>% summarize(avg_accuracy = mean(minutes))

##########################################
###         Wilcoxon Rank Tests        ###
##########################################

return_outcome = function(df, outcome, bin_var){

  group0_human = df %>% arrange(patient_id) %>% filter({{ bin_var }} == 0 & arm == "human_alone") %>% pull({{ outcome }})
  group1_human = df %>% arrange(patient_id) %>% filter({{ bin_var }} == 1 & arm == "human_alone") %>% pull({{ outcome }})
  group0_aug = df %>% arrange(patient_id) %>% filter({{ bin_var }} == 0 & arm == "augmented") %>% pull({{ outcome }})
  group1_aug = df %>% arrange(patient_id) %>% filter({{ bin_var }} == 1 & arm == "augmented") %>% pull({{ outcome }})
  
  return(list(group0_human = group0_human, group1_human = group1_human,
              group0_aug = group0_aug, group1_aug = group1_aug))
}
  
docs_test = return_outcome(fulldata, accuracy, large_docs)
wilcox.test(docs_test$group0_human, docs_test$group1_human)
wilcox.test(docs_test$group0_aug, docs_test$group1_aug)

events_test = return_outcome(fulldata, accuracy, large_events)
wilcox.test(events_test$group0_human, events_test$group1_human)
wilcox.test(events_test$group0_aug, events_test$group1_aug)

batch_test = return_outcome(fulldata, accuracy, later_batch)
wilcox.test(batch_test$group0_human, batch_test$group1_human)
wilcox.test(batch_test$group0_aug, batch_test$group1_aug)

cancer_test = return_outcome(fulldata, accuracy, NSCLC)
wilcox.test(cancer_test$group0_human, cancer_test$group1_human)
wilcox.test(cancer_test$group0_aug, cancer_test$group1_aug)

# Timeliness

docs_test = return_outcome(fulldata, minutes, large_docs)
wilcox.test(docs_test$group0_human, docs_test$group1_human)
wilcox.test(docs_test$group0_aug, docs_test$group1_aug)

events_test = return_outcome(fulldata, minutes, large_events)
wilcox.test(events_test$group0_human, events_test$group1_human)
wilcox.test(events_test$group0_aug, events_test$group1_aug)

batch_test = return_outcome(fulldata, minutes, later_batch)
wilcox.test(batch_test$group0_human, batch_test$group1_human)
wilcox.test(batch_test$group0_aug, batch_test$group1_aug)

cancer_test = return_outcome(fulldata, minutes, NSCLC)
wilcox.test(cancer_test$group0_human, cancer_test$group1_human)
wilcox.test(cancer_test$group0_aug, cancer_test$group1_aug)
