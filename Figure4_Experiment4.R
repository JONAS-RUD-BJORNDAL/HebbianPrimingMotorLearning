#### Manuscript: Hebbian Priming of Human motor learning ####
#### Figure 4, Control Experiment: MEP time course #### 
# Script by Jonas Rud Bjørndal, 08/12-2023####  
# R version 4.1.3 (2022-03-10)

#### Load packages ####
# If not already installed, then install before loading the library: install.packages("package-name")

library(readxl) # used to import data from excel document
library(tidyverse) # incl ggplot2 package used to visualize data
library(gridExtra) # used to combine subplots into a single Figure

#### import Source Data excel file, with individual sheets for each sub figure - insert file location below ####
excel_file <- "H:/Project - STDPage_manus/4_NatureCommunications - REVIEW/Rejection_Revision2/SourceData.xlsx"

#### Figure 4 a -  rPNS and rTMS ####

my_colors <-  c("#5D3FD3", "#CBC3E3")

data_Figure_4a <- read_excel(excel_file, "Figure_4a")
data_Figure_4a$PROTOCOL <- as.factor(data_Figure_4a$PROTOCOL)
data_Figure_4a$TIME <- as.factor(data_Figure_4a$TIME)
data_Figure_4a$TIME <- factor(data_Figure_4a$TIME, levels=c("Baseline", "PostStimulation", "Post15", "Post30", "Post45"))

names(my_colors) <- levels(factor(c(levels(data_Figure_4a$PROTOCOL))))
my_scale_fill_3 <- scale_fill_manual(name="PROTOCOL", values = my_colors)

names(my_colors) <- levels(factor(c(levels(data_Figure_4a$PROTOCOL))))
my_scale_color_3 <- scale_color_manual(name="PROTOCOL", values = my_colors)


Fig4a <- ggplot(data = data_Figure_4a,
                aes(x=TIME, y=MEP_mmax_baseline_mean, group=PROTOCOL, color=PROTOCOL))+
  theme_classic()+
  labs(x="Time", y="MEP amplitude (% of baseline)", tag="a")+
  geom_point(size=1, na.rm=T)+
  geom_line(linewidth=0.5)+
  geom_hline(yintercept = 100, linetype="dashed")+
  geom_errorbar(aes(ymin=MEP_mmax_baseline_mean, ymax=MEP_mmax_baseline_mean+MEP_mmax_baseline_sd), position = position_dodge(0.1), width = 0.15, show.legend = FALSE) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(legend.position = c(0.1, 0.9))+
  theme(legend.title = element_blank())+
  my_scale_color_3+
  my_scale_fill_3+
  coord_cartesian(ylim=c(50,500))+
  scale_y_continuous(breaks=seq(50,500,50))+
  scale_x_discrete("", labels = c("Baseline" = "Baseline", "PostStimulation"="Post Stimulation", "Post15"="Post15", "Post30" = "Post30", "Post45"="Post45"))+
  annotate("rect", xmin = 1.3, xmax = 1.7, ymin = 10, ymax = 250,
           alpha = .3,fill = "grey")+
  theme(text = element_text(size=6))+
  theme(plot.tag=element_text(size = 12, face="bold"))

#### Figure 4 b -  PCMS time course ####

data_Figure_4b <- read_excel(excel_file, "Figure_4b")

data_Figure_4b$PROTOCOL <- as.factor(data_Figure_4b$PROTOCOL)
data_Figure_4b$TIME <- as.factor(data_Figure_4b$TIME)
data_Figure_4b$TIME <- factor(data_Figure_4b$TIME, levels=c("Baseline", "PostStimulation", "Post15", "Post30", "Post45", "Post60"))

my_colors <- c("purple", "blue", "orange")
names(my_colors) <- levels(factor(c(levels(data_Figure_4b$PROTOCOL))))
my_scale_fill_mep <- scale_fill_manual(name="PROTOCOL", values = my_colors)

names(my_colors) <- levels(factor(c(levels(data_Figure_4b$PROTOCOL))))
my_scale_color_mep <- scale_color_manual(name="PROTOCOL", values = my_colors)


Fig4b <- ggplot(data = data_Figure_4b,
                aes(x=TIME, y=MEP_mmax_baseline_mean, group=PROTOCOL, color=PROTOCOL))+
  theme_classic()+
  labs(x="Time", y="MEP amplitude (% of baseline)", tag="b")+
  geom_point(size=1, na.rm=T)+
  geom_line(size=0.5)+
  geom_hline(yintercept = 100, linetype="dashed")+
  geom_errorbar(aes(ymin=MEP_mmax_baseline_mean, ymax=MEP_mmax_baseline_mean+MEP_mmax_baseline_sd), position = position_dodge(0.1), width = 0.15, show.legend = FALSE) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(legend.position = c(0.15, 0.85))+
  theme(legend.title = element_blank())+
  my_scale_color_mep+
  my_scale_fill_mep+
  coord_cartesian(ylim=c(50,500))+
  scale_y_continuous(breaks=seq(50,500,50))+
  scale_x_discrete("", labels = c("Baseline" = "Baseline", "PostStimulation"="Post Stimulation", "Post15"="Post15", "Post30" = "Post30", "Post45"="Post45", "Post60"="Post60"))+
  annotate("rect", xmin = 1.3, xmax = 1.7, ymin = 10, ymax = 250,
           alpha = .3,fill = "grey")+
  theme(text = element_text(size=6))+
  theme(plot.tag=element_text(size = 12, face="bold"))


#### Figure 4 c -  Motor practice vs Rest  ####

data_Figure_4c <- read_excel(excel_file, "Figure_4c")

data_Figure_4c$PROTOCOL <- as.factor(data_Figure_4c$PROTOCOL)
data_Figure_4c$TIME <- as.factor(data_Figure_4c$TIME)
data_Figure_4c$TIME <- factor(data_Figure_4c$TIME, levels=c("Baseline", "PostStimulation", "Post15", "Post30", "Post45", "Post60"))

my_colors <- c("red", "grey")
names(my_colors) <- levels(factor(c(levels(data_Figure_4c$PROTOCOL))))
my_scale_fill_mep <- scale_fill_manual(name="PROTOCOL", values = my_colors)

names(my_colors) <- levels(factor(c(levels(data_Figure_4c$PROTOCOL))))
my_scale_color_mep <- scale_color_manual(name="PROTOCOL", values = my_colors)



Fig4c <- ggplot(data = data_Figure_4c,
                aes(x=TIME, y=MEP_mmax_baseline_mean, group=PROTOCOL, color=PROTOCOL))+
  theme_classic()+
  labs(x="Time", y="MEP amplitude (% of baseline)", tag="c")+
  geom_point(size=1, na.rm=T)+
  geom_line(size=0.5)+
  geom_hline(yintercept = 100, linetype="dashed")+
  geom_errorbar(aes(ymin=MEP_mmax_baseline_mean, ymax=MEP_mmax_baseline_mean+MEP_mmax_baseline_sd), position = position_dodge(0.1), width = 0.15, show.legend = FALSE) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(legend.position = c(0.15, 0.85))+
  theme(legend.title = element_blank())+
  my_scale_fill_mep+
  my_scale_color_mep+
  coord_cartesian(ylim=c(50,500))+
  scale_y_continuous(breaks=seq(50,500,50))+
  scale_x_discrete("", labels = c("Baseline" = "Baseline", "PostStimulation"="Post Stimulation", "Post15"="Post15", "Post30" = "Post30", "Post45"="Post45", "Post60"="Post60"))+
  annotate("rect", xmin = 1.3, xmax = 1.7, ymin = -10, ymax = 12,
           alpha = .3,fill = "grey")+
  annotate("rect", xmin = 1.3, xmax = 1.7, ymin = -10, ymax = 250,
           alpha = .1,fill = "red")+
  theme(text = element_text(size=6))+
  theme(plot.tag=element_text(size = 12, face="bold"))
Fig4c


#### Combine complete graph ####

# Figure 4. Control experiments: MEP % baseline
lay <- rbind(c(1,2,3))
fig4 <- grid.arrange(Fig4a, Fig4b, Fig4c, layout_matrix = lay)

#### Save figure with correct parameters #### 
ggsave(file="path to save final figure/Figure4.pdf", fig4, width = 7.04, height=3, dpi=300)

