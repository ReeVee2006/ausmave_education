
##MAVE Education project - 2023 workshop analysis
##input data downloaded from redcap 02.04.24
##base annotation datasets in SUPP folder
##Author Rehan Villani - created v4 23.10.24

#Clear existing data and graphics
rm(list=ls())
graphics.off()

#set up environment
library(Hmisc)
library(tidyverse)
library(GenomicRanges)
library(cowplot)
library(RColorBrewer)
library(wesanderson)
library(ggpubr)
library(grid)
library(forcats)
library(tm)
library(SnowballC)
library(wordcloud)
library(tidytext)

#colour palette RdYlBl
#scale_fill_brewer(palette = "Blues")
theme_set(theme_gray(base_size = 14))

#set up files--------------------------------------------
#setwd 
setwd("D:/project_functionalevidence/manuscript_workshops/SUPP")
date = strftime(Sys.Date(),"%y%m%d")
#workshop activity data from 17.11.2023
workshop23_data=read.csv('20241023_23FEworkshop_results_cleaned.csv',header = TRUE)
workshop23_data = workshop23_data[which(!is.na(workshop23_data$participantID)),]

workshop_23_barriers=read.table('D:/project_functionalevidence/manuscript_workshops/workshop23/20250212_fe_barrier_analysis_3.txt',sep = "\t", header = TRUE)

#######################WORKSHOP 1######################
## workshop 1 scully and findlay assay evaluation results

#Figure 2 - first half
#looking at the decision making variation in a workshop 2023
summary(workshop23_data)
colnames(workshop23_data)

#summarise the information
colnames(workshop23_data)
cols = c("finding_FE",
         "assay_1_appropriate",
         "assay_1_appropriate_new",
         "assay_1_valid",
         "assay_1_valid_new",
         "assay_1_FE",
         "assay_1_direction_FE_new","assay_1_strength_FE_new","assay_1_FE_comments","assay_2_FE",
         "assay_2_FE_strength_new", "oddspath_possible")

#converting relevant columns to factors
workshop23_data[,cols] = lapply(workshop23_data[,cols] , factor)

summary(workshop23_data)
summary(workshop23_data$assay_1_appropriate_new)

levels = c("Yes","No","Unsure")
levels_st = c("Unsure","Supporting","Moderate","Strong")

######scully assay
w23_a1_app = ggplot(workshop23_data, aes(x = fct_infreq(assay_1_appropriate_new), fill = assay_1_appropriate_new))+ 
  geom_bar(color = "#999999") + 
  labs(title = "Assay 1 Appropriate",x = "", y = "Number of participants")+
  theme(legend.position="none", axis.text.x = element_text(angle = 45, hjust = 1))+
  scale_fill_manual(values=c("skyblue2","gray76", "dodgerblue4"), na.value="#999999")
w23_a1_app


w23_a1_valid = workshop23_data %>%
  mutate(assay_1_valid_new = factor(assay_1_valid_new, levels = unique(assay_1_valid_new))) %>%
  ggplot(aes(x = assay_1_valid_new, fill = assay_1_valid_new))+
  labs(title = "Assay 1 Valid",x = "", y = "Number of participants")+
  geom_bar(color = "#999999")+
  theme(legend.position="none", axis.text.x = element_text(angle = 45, hjust = 1))+
  scale_fill_manual(values=c("dodgerblue4","gray76","skyblue2"), na.value="#999999")
w23_a1_valid

w23_a1_dir = workshop23_data %>%
  mutate(assay_1_direction_FE_new = factor(assay_1_direction_FE_new, levels = unique(assay_1_direction_FE_new))) %>%
  ggplot(aes(x = assay_1_direction_FE_new, fill = assay_1_direction_FE_new))+
  geom_bar(color = "#999999")+ 
  labs(title = "Assay 1 Direction",x = "", y = "Number of participants")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),legend.position="none")+
  scale_fill_manual(values=c("dodgerblue4","gray76","skyblue2"), na.value="#999999")
w23_a1_dir

w23_a1_str = workshop23_data %>%
  ggplot(aes(x = assay_1_strength_FE_new, fill = assay_1_strength_FE_new))+
  geom_bar(color = "#999999")+ 
  labs(title = "Assay 1  Strength",x = "", y = "Number of participants")+ 
  theme(legend.position="none", axis.text.x = element_text(angle = 45, hjust = 1))+
  scale_fill_manual(values=c("dodgerblue2","skyblue2", "gray76"), na.value="#999999")
w23_a1_str

#check
summary(workshop23_data$assay_1_strength_FE_new)

####findlay assay-------------------
colnames(workshop23_data)

summary(workshop23_data$oddspath_possible)

w23_a2_op = ggplot(workshop23_data, aes(x = fct_infreq(oddspath_possible), fill = oddspath_possible))+ 
  geom_bar(color = "#999999") + 
  labs(title = "Assay 2 OddsPath",x = "", y = "Number of participants")+
  theme(legend.position="none",axis.text.x = element_text(angle = 45, hjust = 1))+
  scale_fill_manual(values=c("skyblue2","gray76","dodgerblue4"), na.value="#999999")
w23_a2_op

w23_a2_str = workshop23_data %>%
  ggplot(aes(x = fct_relevel(assay_2_FE_strength_new, "Strong","Moderate","Supporting","Unsure"), fill = assay_2_FE_strength_new))+
  geom_bar(color = "#999999")+
  labs(x = "", y = "Number of participants", title = "Assay 2 Strength")+
  theme(legend.position="none",axis.text.x = element_text(angle = 45, hjust = 1))+
  scale_fill_manual(values=c("dodgerblue2","dodgerblue4","skyblue2", "gray76"), na.value="#999999")
w23_a2_str

####figure second half - free text barrier analysis--------------------------

###look at the data
head(workshop_23_barriers)
workshop_23_barriers[,3:15] <- lapply(workshop_23_barriers[,3:15] , factor)
str(workshop_23_barriers)

#####looking at the manual translation of categorical translated barriers######################
workshop_23_barriers$interpretation_2 = as.factor(workshop_23_barriers$interpretation_2)
summary(workshop_23_barriers$interpretation_2)

workshop_23_barriers$interpretation_3 = as.factor(workshop_23_barriers$interpretation_3)
summary(workshop_23_barriers$interpretation_3)

plot_barrier_txt = workshop_23_barriers %>% 
  filter(interpretation_3 != "") %>%
  count(interpretation_3) %>%
  ggplot(., aes(x = reorder(interpretation_3, n), y = n, fill = n))+
  geom_bar(stat = 'identity')+
  coord_flip()+
  labs(x = "Barrier", y = "Number of participants")+
  scale_y_continuous("Number of participants", breaks = c(0,2,4,6,8,10))+
  scale_fill_gradient(low = "gray76", high = "dodgerblue4")
plot_barrier_txt

####looking at the words listed in the barriers
summary(workshop_23_barriers)
colnames(workshop_23_barriers)

barriers = tibble(line = 1:(nrow(workshop_23_barriers)), fe_barrier_adj = workshop_23_barriers$fe_barriers_adj)

type_sum(barriers)
colnames(barriers)

#create a word list as 'tokens'
tidy_barriers = barriers %>% unnest_tokens(word, fe_barrier_adj) 
tidy_barriers$activity_q = "fe_barrier_adj"

#remove stop words

tidy_barriers_2 <- tidy_barriers %>%
  anti_join(stop_words) #went from 412 to 212

#find the most common words
tidy_barriers_2 %>%
  count(word, sort = TRUE) 


####FIGURE 2 - Villani et al. 2025 ## variation in evidence application------------------------------

#figure 1 all
plot_grid(plot_grid(w23_a1_app, w23_a1_valid,NULL,
                    w23_a1_dir,w23_a1_str,NULL,
                    w23_a2_op, w23_a2_str,NULL,
                    align = "h", axis = "b", rel_widths = c(1, 1, 0.5),
                    labels = c("A","B", "","C", "D","","E","F",""),
                    ncol = 3, nrow = 3), plot_barrier_txt,
          align = "h", axis = "b", 
          rel_heights = c(2.5,1),
          labels = c("","G"),
          ncol = 1, nrow = 2)

ggsave(path = "D:/project_functionalevidence/manuscript_workshops/figures", 
       filename = paste(gsub(":", "-", Sys.Date()),"_fig2_wkshp1.png",sep=""), 
       width = 10, height = 16, device='png', dpi=600)

####SUPP TABLES - Villani et al. 2025 ####determine the counts of # participants for each 'opinion'
summary(workshop23_data)
colnames(workshop23_data)

#select the columns to count
w23_col_count = c("assay_1_appropriate_new","assay_1_valid_new",
                  "assay_1_direction_FE_new","assay_1_strength_FE_new",
                  "oddspath_possible" , "assay_2_FE_strength_new")

#summary table of participant number in figures 
workshop23_data %>% select(all_of(w23_col_count)) %>% summary()

table(workshop23_data$assay_1_appropriate_new)
table(workshop23_data$assay_1_valid_new)
table(workshop23_data$assay_1_direction_FE_new)
table(workshop23_data$assay_1_strength_FE_new)
table(workshop23_data$oddspath_possible)
table(workshop23_data$assay_2_FE_strength_new)

