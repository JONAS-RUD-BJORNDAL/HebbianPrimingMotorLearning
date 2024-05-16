#### Manuscript: Hebbian Priming of Human motor learning ####
#### Figure 3, Experiment 3: within design - PCMS+, PCMS-, PCMScoupled-control #### 
# Script by Jonas Rud Bjørndal, 08/12-2023
# R version 4.1.3 (2022-03-10)

#### Load packages ####
# If not already installed, then install before loading the library: install.packages("package-name")

library(readxl) # used to import data from excel document
library(tidyverse)  # incl ggplot2 package used to visualize data
library(gridExtra) # used to combine subplots into a single Figure
library(ggsignif)
library(png)


#### import SourceData excel file, with individual sheets for each sub figure - insert file location below ####
excel_file <- "H:/Project - STDPage_manus/4_NatureCommunications - REVIEW/Rejection_Revision2/SourceData.xlsx"

#### Choose color scheme ####
my_colors <- c("purple", "blue", "orange")


#### Figure 3a - space for graphic ####
fig3a <- ggplot() +
  labs(tag="a")+
  theme_classic()+ 
  guides(x = "none", y = "none")+
  theme(text = element_text(size=7))+
  theme(plot.tag=element_text(size = 12, face="bold"))


#### Figure 3b - Learning curve ####
# Making different geom_lines for B1, B2 and B3
data_Figure_3b <- read_excel(excel_file, "Figure_3b")
data_Figure_3b$PROTOCOL <- as.factor(data_Figure_3b$PROTOCOL)
data_Figure_3b$PROTOCOL <- factor(data_Figure_3b$PROTOCOL, levels=c("PCMS+", "PCMS-", "PCMScoupled-control"))

names(my_colors) <- levels(factor(c(levels(data_Figure_3b$PROTOCOL))))
my_scale_fill_ballistic <- scale_fill_manual(name="PROTOCOL", values = my_colors, limits = c("PCMS+", "PCMS-", "PCMScoupled-control"))
my_scale_color_ballistic <- scale_color_manual(name="PROTOCOL", values = my_colors, limits = c("PCMS+", "PCMS-", "PCMScoupled-control"))

block1_line <- data_Figure_3b %>%  filter(TIME_blocked == "B1")
block2_line <- data_Figure_3b %>%  filter(TIME_blocked == "B2")
block3_line <- data_Figure_3b %>%  filter(TIME_blocked == "B3")


pa <- ggplot(data = data_Figure_3b,
       aes(x=TIME_binned, y=peak_acc_baseline_mean, group = PROTOCOL, colour=PROTOCOL, fill=PROTOCOL))+
  geom_errorbar(aes(ymin=peak_acc_baseline_mean, ymax=peak_acc_baseline_mean+peak_acc_baseline_sd), position = position_dodge(0.2), width = 0.25, show.legend = FALSE) +
  geom_point(aes(color=PROTOCOL))+
  geom_line(data =   block1_line, aes(y = peak_acc_baseline_mean, x = TIME_binned))+
  geom_line(data =   block2_line, aes(y = peak_acc_baseline_mean, x = TIME_binned))+
  geom_line(data =   block3_line, aes(y = peak_acc_baseline_mean, x = TIME_binned))+
  geom_hline(yintercept=100, linetype="dashed")+
  coord_cartesian(ylim=c(90,160))+
  scale_y_continuous(breaks=seq(90,160,10))+
  theme_classic()+
  labs(y="Peak acceleration (%baseline)", tag="b")+
  scale_x_discrete(" ", labels = c("0" = "Baseline", "1" = "Post Stimulation", "2" = "", "3" = "", "4" = "B1", "5" = "", "6"="", "7" = "", "8" = "", "9" = "B2", "10" = "", "11"="", "12" = "", "13" = "", "14" = "B3", "15" = "", "16"="", "19"="","20"="","21"="7 day relearning", "22"="", "23"=""))+
  theme(legend.position = c(0.2, 0.9))+
  theme(legend.title = element_blank())+
  my_scale_color_ballistic+
  my_scale_fill_ballistic+
  theme(text = element_text(size=7))+
  theme(plot.tag=element_text(size = 12, face="bold"))

# Figure 3b - boxplots inserted in Learning Curve ####

# Subset data 
ballistic_practice = data_Figure_3b %>% 
  filter(TIME_binned != "0") %>% 
  filter(TIME_binned != "1")%>% 
  filter(TIME_binned != "17") %>% 
  filter(TIME_binned != "18") 
# Barplot - practice blocks
ballistic_practice_summary <- ballistic_practice %>%
  group_by(PROTOCOL, TIME_blocked) %>%  
  summarize(
    n_samples = n(),
    peak_baseline_mean= mean(peak_acc_baseline_mean, na.rm=T),
    peak_baseline_sd = sd(peak_acc_baseline_mean, na.rm=T)
  ) %>%
  ungroup() 



paa <- ggplot(ballistic_practice_individual, aes(x = block_original, y = peak_baseline_mean, fill = PCMS)) +   
  geom_boxplot(position = position_dodge(width = 0.9), alpha = 0.8, width = 0.7, outlier.shape = NA) +
  geom_point(data =   ballistic_practice_individual, aes(y = peak_baseline_mean, x = block_original), position = position_jitterdodge(), size=0.1)+
  labs(title="Pooled blocks", x="post PCMS", y="")+
  theme_classic() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(legend.position = "none")+
  #coord_cartesian(ylim=c(100,170))+
  #scale_y_continuous(breaks=seq(100,170,20))+
  geom_hline(yintercept=100, linetype="dashed", color = "black")+
  scale_x_discrete("", labels = c("3" = "B1", "4" = "B2", "5"="B3"))+
  my_scale_color_ballistic+
  my_scale_fill_ballistic+
  geom_signif(
    y_position = c(150, 155, 170, 175, 190, 195), xmin = c(0.6, 0.9, 1.6, 1.6, 2.6, 2.6), xmax = c(0.9, 1.3, 1.9, 2.3, 2.9, 3.3),
    annotation = c("#"), tip_length = 0)+
  theme(plot.title = element_text(size=7))+
  theme(text = element_text(size=7))+
  theme(axis.title.y = element_blank())


fig3b <- pa+annotation_custom(ggplotGrob(paa),xmin=8, xmax=16.8, ymin=82, ymax=113.5)

#### Figure 3c - individuel data - start of B1to end of B3 ####

data_Figure_3c <- read_excel(excel_file, "Figure_3c")
data_Figure_3c$PROTOCOL <- as.factor(data_Figure_3c$PROTOCOL)
data_Figure_3c$PROTOCOL <- factor(data_Figure_3c$PROTOCOL, levels=c("PCMS+", "PCMS-", "PCMScoupled-control"))
data_Figure_3c$peak_acc_baseline_mean <- as.numeric(data_Figure_3c$peak_acc_baseline_mean)
data_Figure_3c$TIME <- as.factor(data_Figure_3c$TIME)
data_Figure_3c$TIME <- relevel (data_Figure_3c$TIME, "Start_B1")
data_Figure_3c$TIME <- factor(data_Figure_3c$TIME, levels=c("Start_B1", "End_B3", "Day7"))

names(my_colors) <- levels(factor(c(levels(data_Figure_3c$PROTOCOL))))
my_scale_color_ballistic <- scale_color_manual(name="PROTOCOL", values = my_colors)


fig3c <- ggplot(data = data_Figure_3c,
       aes(x=TIME, y=peak_acc_baseline_mean, group = PARTICIPANT, colour=PROTOCOL))+
  geom_line()+
  geom_point()+
  geom_hline(yintercept = 100, linetype="dashed")+
  theme_classic()+
  scale_x_discrete(" ", labels = c("Start_B1" = "Start of B1", "End_B3"="End of B3"))+
   labs(tag="c")+
  labs(x="blocks", y="Peak Acceleration (% of baseline)") +
  theme(legend.position = "none")+
  my_scale_color_ballistic+
  scale_x_discrete("", labels = c("2" = "Start of B1", "16" = "End of B3"))+
  facet_wrap(~factor(PROTOCOL, levels=c('PCMS+', 'PCMS-', 'PCMScoupled-control')), ncol=1)+
  theme(strip.background = element_blank())+
  coord_cartesian(ylim=c(70,210))+
  stat_summary(fun.y=mean,geom="line",color="black", lwd=1,aes(group=1))+
  theme(text = element_text(size=7))+
  theme(plot.tag=element_text(size = 12, face="bold"))

#### Figure 3d - space for graphic ####
fig3d <- ggplot() +
  labs(tag="d")+
  theme_classic()+ 
  guides(x = "none", y = "none")+
  theme(text = element_text(size=7))+
  theme(plot.tag=element_text(size = 12, face="bold"))


####  Figure 3e - MEP norm Mmax ####
data_Figure_3e <- read_excel(excel_file, "Figure_3e")
data_Figure_3e$PROTOCOL <- as.factor(data_Figure_3e$PROTOCOL)
data_Figure_3e$PROTOCOL <- factor(data_Figure_3e$PROTOCOL, levels=c("PCMS+", "PCMS-", "PCMScoupled-control"))
data_Figure_3e$TIME <- as.factor(data_Figure_3e$TIME)
data_Figure_3e$TIME <- factor(data_Figure_3e$TIME, levels=c("Baseline", "Post Stimulation", "Post Practice"))



names(my_colors) <- levels(factor(c(levels(data_Figure_3e$PROTOCOL))))
my_scale_color_mep <- scale_color_manual(name="PROTOCOL", values = my_colors, limits = c("PCMS+", "PCMS-", "PCMScoupled-control"))
my_scale_fill_mep <- scale_fill_manual(name="PROTOCOL", values = my_colors, limits = c("PCMS+", "PCMS-", "PCMScoupled-control"))

# MEP mmax - boxplots
fig3e <- ggplot(data= mep_means,
                aes(x=block, y=MEP_mmax_mean, fill=PCMS))+
  geom_errorbar(aes(ymin=MEP_mmax_mean, ymax=MEP_mmax_mean+MEP_mmax_sd), position = position_dodge(0.9), width = 0.15, show.legend = FALSE) +
  geom_bar(stat = "identity", position = "dodge")  +
  geom_point(data =   mep_means_individual, aes(y = MEP_mmax_mean, x = block), position = position_jitterdodge(), size=1)+
  theme_classic()+
  theme(legend.position = c(0.3, 0.95))+
  theme(legend.title = element_blank())+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  labs(tag="e")+
  labs(x="blocks", y="MEP amplitude (%Mmax)") +
  scale_x_discrete("", labels = c("0" = "Baseline", "1" = "Post Stimulation", "17" = "Post Practice"))+
  my_scale_color_mep+
  my_scale_fill_mep+
  geom_signif(
    y_position = c(20,30, 25, 25, 20, 25, 20), xmin = c(1,0.7,0.7,1.7,2,2.7, 3), xmax = c(1.3,1.3,1,2,2.3,3,3.3),
    annotation = c("#","#","#","#","#","#","#"), tip_length = 0)+
  theme(text = element_text(size=7))+
  theme(plot.tag=element_text(size = 12, face="bold"))+ 
  guides(fill = guide_legend(override.aes = list(shape = NA)))+
  theme(legend.key.size = unit(0.2, "cm"))


####  Figure 3f - individual data MEP norm Mmax ####
data_Figure_3f <- read_excel(excel_file, "Figure_3f")
data_Figure_3f$PARTICIPANT <- as.factor(data_Figure_2f$PARTICIPANT)
data_Figure_3f$PROTOCOL <- as.factor(data_Figure_3f$PROTOCOL)
data_Figure_3f$TIME <- as.factor(data_Figure_3f$TIME)
data_Figure_3f$TIME <- factor(data_Figure_3f$TIME, levels=c("Baseline", "Post Stimulation", "Post Practice"))

names(my_colors) <- levels(factor(c(levels(data_Figure_3f$PROTOCOL))))
my_scale_color_mep <- scale_color_manual(name="PROTOCOL", values = my_colors)
my_scale_fill_mep <- scale_fill_manual(name="PROTOCOL", values = my_colors)

fig3F <- ggplot(data = data_Figure_3f,
                aes(x=TIME, y=MEP_mmax_baseline_mean, group = PARTICIPANT, colour=PROTOCOL))+
  geom_point()+
  geom_line()+
  geom_hline(yintercept=100, linetype="dashed")+
  theme_classic()+
  theme(legend.position = "none")+
  scale_x_discrete("", labels = c("0" = "Baseline", "1" = "Post Stimulation", "17" = "  Post Practice"))+
  labs(x="", y="MEP amplitude (% of baseline)", tag="f")+
  my_scale_color_mep+
  stat_summary(fun.y=mean,geom="line", color="black", lwd=1,aes(group=1))+
  coord_cartesian(ylim=c(0,450))+
  theme(strip.background = element_blank())+
  facet_wrap(~factor(PROTOCOL, levels=c('PCMS+', 'PCMS-', 'PCMScoupled-control')), ncol=1)+
  theme(text = element_text(size=6))+
  theme(plot.tag=element_text(size = 12, face="bold"))

#### Combine complete graph ####

# Figure 3.
lay <- rbind(c(1,1,2,2,2,2,2,2,3,3,3),
             c(1,1,2,2,2,2,2,2,3,3,3),
             c(1,1,2,2,2,2,2,2,3,3,3),
             c(1,1,2,2,2,2,2,2,3,3,3),
             c(1,1,2,2,2,2,2,2,3,3,3),
             c(4,4,4,4,5,5,5,5,6,6,6),
             c(4,4,4,4,5,5,5,5,6,6,6),
             c(4,4,4,4,5,5,5,5,6,6,6),
             c(4,4,4,4,5,5,5,5,6,6,6))
fig3 <- grid.arrange(fig3a, fig3b, fig3c, fig3d, fig3e, fig3F, layout_matrix = lay)

#### Save figure with correct parameters #### 
ggsave(file="path to save final figure/Figure3.pdf", fig3, width = 7.04, height=7, dpi=300)

# Note: Graphics for part "a" and "d" was inserted afterwards in software allowing vector graphics




