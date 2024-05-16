#### Manuscript: Hebbian Priming of Human motor learning ####
#### Figure 1: Experiment 1: PCMS+ vs Rest ####
# Script by Jonas Rud Bjørndal, 08/12-2023
# R version 4.1.3 (2022-03-10)



#### Load packages ####
# If not already installed, then install before loading the library: install.packages("package-name")

library(readxl) # used to import data from excel document
library(tidyverse)  # incl ggplot2 package used to visualize data
library(gridExtra) # used to combine subplots into a single Figure
library(ggsignif)
library(tidytext)
library(grid)
library(lattice)
library(ggpubr)
library(ggeasy)

#### import SourceData excel file, with individual sheets for each sub figure - insert file location below ####
excel_file <- "C:/insert your file location/SourceData.xlsx"

#### Choose color scheme ####
my_colors <- c("blue", "darkgrey")


#### Figure 1a - space for graphic ####
fig1a <- ggplot() +
  labs(tag="a")+
  theme_classic()+ 
  guides(x = "none", y = "none")+
  theme(text = element_text(size=7))+
  theme(plot.tag=element_text(size = 12, face="bold"))


#### Figure 1b - Learning curve ####
data_Figure_1b <- read_excel(excel_file, "Figure_1b")
data_Figure_1b$GROUP <- as.factor(data_Figure_1b$GROUP)

names(my_colors) <- levels(factor(c(levels(data_Figure_1b$GROUP))))
my_scale_fill_ballistic <- scale_fill_manual(name="GROUP", values = my_colors)
my_scale_color_ballistic <- scale_color_manual(name="GROUP", values = my_colors)

block1_line <- data_Figure_1b %>%  filter(TIME_blocked == "B1")
block2_line <- data_Figure_1b %>%  filter(TIME_blocked == "B2")
block3_line <- data_Figure_1b %>%  filter(TIME_blocked == "B3")

pa <- ggplot(data = data_Figure_1b,
             aes(x=TIME_binned, y=peak_acc_baseline_mean, group = GROUP, colour=GROUP, fill=GROUP))+
  geom_errorbar(aes(ymin=peak_acc_baseline_mean, ymax=peak_acc_baseline_mean+peak_acc_baseline_sd), position = position_dodge(0.2), width = 0.25, show.legend = FALSE) +
  geom_point(aes(color=GROUP))+
  geom_line(data =   block1_line, aes(y = peak_acc_baseline_mean, x = TIME_binned))+
  geom_line(data =   block2_line, aes(y = peak_acc_baseline_mean, x = TIME_binned))+
  geom_line(data =   block3_line, aes(y = peak_acc_baseline_mean, x = TIME_binned))+
  geom_hline(yintercept=100, linetype="dashed")+
  coord_cartesian(ylim=c(90,180))+
  scale_y_continuous(breaks=seq(90,180,10))+
  theme_classic()+
  labs(y="Peak acceleration (%baseline)", tag="b")+
  scale_x_discrete("", labels = c("0" = "Baseline", "1" = "", "2" = "", "3" = "B1", "4" = "", "5" = "", "6"="", "7" = "", "8" = "B2", "9" = "", "10" = "", "11"="", "12" = "", "13" = "B3", "14" = "", "15" = "", "16"="", "19"="","20"="","21"="7 day relearning", "22"="", "23"=""))+
  theme(legend.position = c(0.3, 0.9))+
  theme(legend.title = element_blank())+
  my_scale_color_ballistic+
  my_scale_fill_ballistic+
  theme(text = element_text(size=7))+
  theme(plot.tag=element_text(size = 12, face="bold"))
pa

# Figure 1B - boxplots inserted in Learning Curve 
# Subset data 
ballistic_practice = data_Figure_1b %>% 
  filter(TIME_binned != "0") %>% 
  filter(TIME_binned != "1")%>% 
  filter(TIME_binned != "17") %>% 
  filter(TIME_binned != "18") 
# Barplot - practice blocks
ballistic_practice_summary <- ballistic_practice %>%
  group_by(GROUP, TIME_blocked) %>%  
  summarize(
    n_samples = n(),
    peak_baseline_mean= mean(peak_acc_baseline_mean, na.rm=T),
    peak_baseline_sd = sd(peak_acc_baseline_mean, na.rm=T)
  ) %>%
  ungroup() 

paa <- ggplot(ballistic_practice_individual, aes(x = block_original, y = peak_baseline_mean, fill = PCMS)) +
  geom_boxplot(position = position_dodge(width = 0.9), alpha = 0.8, width = 0.7, outlier.shape = NA, size=0.3) +
  geom_point(data = ballistic_practice_individual, aes(y = peak_baseline_mean, x = block_original), position = position_jitterdodge(), size = 0.01) +
  labs(x = "post PCMS") +
  theme_classic() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(legend.position = "none") +
  #coord_cartesian(ylim = c(100, 185)) +
  #scale_y_continuous(breaks = seq(100, 185, 20)) +
  geom_hline(yintercept = 100, linetype = "dashed", color = "black", linewidth= 0.3) +
  scale_x_discrete("", labels = c("2" = "B1", "3" = "B2", "4" = "B3", "7" = "7Day relearning")) +
  my_scale_color_ballistic +
  my_scale_fill_ballistic +
  theme(plot.title = element_text(size = 5)) +
  geom_signif(
    y_position = c(170, 190), xmin = c(1.7, 2.7), xmax = c(2.3, 3.3),
    annotation = c("#"), tip_length = 0, textsize = 2, size=0.2 ) +
  annotate("text", x = 1.6, y = 200, label = "Pooled Blocks", size=2)+
  theme(text = element_text(size = 5)) +
  theme(axis.title.y = element_blank()) +
  theme(plot.tag = element_text(size = 12, face = "bold")) +
  theme(axis.line = element_line(size = 0.4))

fig1b <- pa+annotation_custom(ggplotGrob(paa),xmin=8.5, xmax=17.5, ymin=81, ymax=117.5)


#### Figure 1c - individual data - start B1 til slut B3 ####

data_Figure_1c <- read_excel(excel_file, "Figure_1c")
data_Figure_1c$GROUP <- as.factor(data_Figure_1c$GROUP)
data_Figure_1c$TIME <- as.factor(data_Figure_1c$TIME)
data_Figure_1c$TIME <- relevel (data_Figure_1c$TIME, "Start_B1")

names(my_colors) <- levels(factor(c(levels(data_Figure_1c$GROUP))))
my_scale_color_ballistic <- scale_color_manual(name="GROUP", values = my_colors)


fig1c <- ggplot(data = data_Figure_1c,
       aes(x=TIME, y=peak_acc_baseline_mean, group = PARTICIPANT, colour=GROUP))+
  geom_line()+
  geom_point()+
  geom_hline(yintercept = 100, linetype="dashed")+
  theme_classic()+
     labs(tag="c")+
  labs(x="blocks", y="Peak Acceleration (% of baseline)") +
  theme(legend.position = "none")+
  my_scale_color_ballistic+
  scale_x_discrete(" ", labels = c("Start_B1" = "Start of B1", "End_B3"="End of B3"))+
  facet_wrap(~GROUP, ncol=1)+
  theme(strip.background = element_blank())+
  stat_summary(fun.y=mean,geom="line", color="black", lwd=1.5,aes(group=1))+
  theme(text = element_text(size=6))+
  theme(plot.tag=element_text(size = 12, face="bold"))
fig1c

#### Figure1d - space for graphic ####

fig1d <- ggplot() +
  labs(tag="d")+
theme_classic()+ 
  guides(x = "none", y = "none")+
  theme(text = element_text(size=7))+
  theme(plot.tag=element_text(size = 12, face="bold"))


####  Figure 1e - MEP norm Mmax ####

data_Figure_1e <- read_excel(excel_file, "Figure_1e")
data_Figure_1e$GROUP <- as.factor(data_Figure_1e$GROUP)
data_Figure_1e$TIME <- as.factor(data_Figure_1e$TIME)
data_Figure_1e$TIME <- factor(data_Figure_1e$TIME, levels=c("Baseline", "PostStimulation", "PostPractice"))

names(my_colors) <- levels(factor(c(levels(data_Figure_1e$GROUP))))
my_scale_color_mep <- scale_color_manual(name="GROUP", values = my_colors)
my_scale_fill_mep <- scale_fill_manual(name="GROUP", values = my_colors)


# MEP mmax - boxplots
fig1e <- ggplot(data= mep_means_individual,
                aes(x=block, y=MEP_mmax_mean, fill=PCMS))+
  geom_boxplot(position = position_dodge(width = 0.9), alpha = 0.8, width = 0.7, outlier.shape = NA) +
  geom_point(data =   mep_means_individual, aes(y = MEP_mmax_mean, x = block), position = position_jitterdodge(), size=1)+
  theme_classic()+
  theme(legend.position = c(0.2, 0.9))+
  theme(legend.title = element_blank())+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  labs(tag="e")+
  labs(x="blocks", y="MEP amplitude (%Mmax)") +
  scale_x_discrete("", labels = c("0" = "Baseline", "1" = "Post Stimulation", "17" = "Post Practice"))+
  my_scale_color_mep+
  my_scale_fill_mep+  
  geom_signif(
    y_position = c(21), xmin = c(1.7), xmax = c(2.3),
    annotation = c("#"), tip_length = 0)+
  theme(text = element_text(size=7))+
  theme(plot.tag=element_text(size = 12, face="bold"))+ 
  guides(fill = guide_legend(override.aes = list(shape = NA)))+
  theme(legend.key.size = unit(0.2, "cm"))

####  Figure 1f - individual data MEP norm Mmax ####

data_Figure_1f <- read_excel(excel_file, "Figure_1f")
data_Figure_1f$PARTICIPANT <- as.factor(data_Figure_1f$PARTICIPANT)
data_Figure_1f$GROUP <- as.factor(data_Figure_1f$GROUP)
data_Figure_1f$TIME <- as.factor(data_Figure_1f$TIME)
data_Figure_1f$TIME <- factor(data_Figure_1f$TIME, levels=c("Baseline", "Post Stimulation", "Post Practice"))

names(my_colors) <- levels(factor(c(levels(data_Figure_1f$GROUP))))
my_scale_color_mep <- scale_color_manual(name="GROUP", values = my_colors)
my_scale_fill_mep <- scale_fill_manual(name="GROUP", values = my_colors)

fig1F <- ggplot(data = data_Figure_1f,
       aes(x=TIME, y=MEP_mmax_baseline_mean, group = PARTICIPANT, colour=GROUP))+
  geom_point()+
  geom_line()+
  geom_hline(yintercept=100, linetype="dashed")+
  theme_classic()+
  theme(legend.position = "none")+
  scale_x_discrete("", labels = c("Baseline" = "Baseline", "Post Stimulation" = "Post Stimulation", "Post Practice" = "Post Practice"))+
  labs(x="", y="MEP amplitude (% of baseline)", tag="f")+
  my_scale_color_mep+
  stat_summary(fun.y=mean,geom="line", color="black", lwd=1,aes(group=1))+
  facet_wrap(~GROUP, ncol=1)+
  theme(strip.background = element_blank())+
  theme(text = element_text(size=5))+
  theme(plot.tag=element_text(size = 12, face="bold"))
fig1F

#### Combine complete graph ####
 
# Figure 1
lay <- rbind(c(1,1,1,1,1,2,2,2,2,2,3,3,3),
             c(1,1,1,1,1,2,2,2,2,2,3,3,3),
             c(1,1,1,1,1,2,2,2,2,2,3,3,3),
             c(1,1,1,1,1,2,2,2,2,2,3,3,3),
             c(1,1,1,1,1,2,2,2,2,2,3,3,3),
             c(1,1,1,1,1,2,2,2,2,2,3,3,3),
             c(4,4,4,4,5,5,5,5,5,6,6,6,6),
             c(4,4,4,4,5,5,5,5,5,6,6,6,6),
             c(4,4,4,4,5,5,5,5,5,6,6,6,6),
             c(4,4,4,4,5,5,5,5,5,6,6,6,6))
fig1 <- grid.arrange(fig1a, fig1b, fig1c, fig1d, fig1e, fig1F, layout_matrix = lay)

#### Save figure with correct parameters #### 
ggsave(file="path to save final figure/Figure1.pdf", fig1, width = 7.04, height=7, dpi=300)


# Note: Graphics for part "a" and "d" was inserted afterwards in software allowing vector graphics
