library(tidyr)
library(ggplot2)
library(patchwork)
library(ggpattern)
library(cowplot)
library(grid)
library(gridExtra)

Att_Bio <- read.csv("C:/Users/ecor9/Google Drive/Grad School/Research/Data Sets/Ontario Enrollment Data Sets/Att_Bio.csv")
Att_Phys <- read.csv("C:/Users/ecor9/Google Drive/Grad School/Research/Data Sets/Ontario Enrollment Data Sets/Att_Phys.csv")
Att_Chem <- read.csv("C:/Users/ecor9/Google Drive/Grad School/Research/Data Sets/Ontario Enrollment Data Sets/Att_Chem.csv")
Att_Math <- read.csv("C:/Users/ecor9/Google Drive/Grad School/Research/Data Sets/Ontario Enrollment Data Sets/Att_Math.csv")

Arrows_df <- data.frame(x_pos = c(1.85, 2.3, 2.85, 3.3), y_start = c(100, 100, 100, 100), y_B = c(48, 71, 29, 45), y_P = c(53, 36, 34, 19), y_C = c(59, 62, 40, 41), y_M = c(85, 77, 58, 48))
Arrows_df <- cbind(Arrows_df, data.frame(B_loss = paste(Arrows_df$y_B - rep(100,4), rep("%",4), sep = ''), P_loss = paste(Arrows_df$y_P - rep(100,4), rep("%",4), sep = ''), C_loss = paste(Arrows_df$y_C - rep(100,4), rep("%",4), sep = ''), M_loss = paste(Arrows_df$y_M - rep(100,4), rep("%",4), sep = '')))
Arrows_df <- cbind(Arrows_df, data.frame(B_text = (Arrows_df$y_B + rep(100,4))/2, P_text = (Arrows_df$y_P + rep(100,4))/2, C_text = (Arrows_df$y_C + rep(100,4))/2, M_text = (Arrows_df$y_M + rep(100,4))/2))

#### Plotting ####

##Bio Plot##
##Reverse the data so it says Male then Female##
Att_Bio$Gender <- factor(Att_Bio$Gender, levels = c("Male", "Female"))

##Plot the Bio Attrition bargraph##
Bio_bar <- ggplot(data = Att_Bio) +
  geom_bar_pattern(aes(x = Course,y = Percent, fill = Gender, pattern = Gender),
                   stat = "identity",
                   position = position_dodge(),
                   pattern_size = 0,
                   pattern_colour = "#006658",
                   pattern_fill = "#006658",
                   pattern_spacing = 0.02) +
  geom_segment(data = Arrows_df, 
               aes(x = x_pos, y = y_start, xend = x_pos, yend = y_B), 
               arrow = arrow(length = unit(0.3, "cm"), type = "closed")) +
  geom_segment(data = Arrows_df, 
               aes(x = x_pos, y = y_B, xend = x_pos, yend = y_start), 
               arrow = arrow(length = unit(0.2, "cm"), angle = 90)) +
  geom_text(data = Arrows_df,
            aes(label = B_loss,x = x_pos, hjust = rep(1.1,4), y = B_text), 
            size = 6) +
  ggtitle('Biology') +
  scale_fill_manual(values = c("#00b39b", "#99ffce")) + 
  scale_pattern_manual( values = c('stripe','none'), ) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        legend.position="none",
        axis.title = element_blank(), 
        axis.text = element_text(size = 18), 
        legend.text = element_text(size = 16), 
        axis.ticks = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 20, face = 'bold'))

##Chem Plot##
##Reverse the data so it says Male then Female##
Att_Chem$Gender <- factor(Att_Chem$Gender, levels = c("Male", "Female"))

##Plot the Chem Attrition bargraph##
Chem_bar <- ggplot(data = Att_Chem) +
  geom_bar_pattern(aes(x = Course,y = Percent, fill = Gender, pattern = Gender),
                   stat = "identity",
                   position = position_dodge(),
                   pattern_size = 0,
                   pattern_colour = "#7e0204",
                   pattern_fill = "#7e0204",
                   pattern_spacing = 0.02) +
  geom_segment(data = Arrows_df, aes(x = x_pos, y = y_start, xend = x_pos, yend = y_C), arrow = arrow(length = unit(0.3, "cm"), type = "closed")) +
  geom_segment(data = Arrows_df, aes(x = x_pos, y = y_C, xend = x_pos, yend = y_start), arrow = arrow(length = unit(0.2, "cm"), angle = 90)) +
  geom_text(data = Arrows_df, 
            aes(label = C_loss, x = x_pos, hjust = rep(1.1,4), y = C_text),
            size = 6) +  
  ##delete the line above if you want to get rid of the arrow
  scale_fill_manual(values = c("#FD6467", "#F2CFE1")) + ##First is female, second male##
  scale_pattern_manual( values = c('stripe','none'), ) +
  ggtitle('Chemistry') +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        legend.position="none",
        axis.title = element_blank(), 
        axis.text = element_text(size = 18), 
        legend.text = element_text(size = 16), 
        axis.ticks = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 20, face = 'bold'))

##Phys Plot##
##Reverse the data so it says Male then Female##
Att_Phys$Gender <- factor(Att_Phys$Gender, levels = c("Male", "Female"))

##Plot the Phys Attrition bargraph##
Phys_bar <- ggplot(data = Att_Phys) +
  geom_bar_pattern(aes(x = Course,y = Percent, fill = Gender, pattern = Gender),
                   stat = "identity",
                   position = position_dodge(),
                   pattern_size = 0,
                   pattern_colour = "#30569c",
                   pattern_fill = "#30569c",
                   pattern_spacing = 0.02) +
  geom_segment(data = Arrows_df, aes(x = x_pos, y = y_start, xend = x_pos, yend = y_P), arrow = arrow(length = unit(0.3, "cm"), type = "closed")) +
  geom_segment(data = Arrows_df, aes(x = x_pos, y = y_P, xend = x_pos, yend = y_start), arrow = arrow(length = unit(0.2, "cm"), angle = 90)) +
  geom_text(data = Arrows_df, 
            aes(label = P_loss, x = x_pos, hjust = rep(1.1,4), y = P_text),
            size = 6) +  
  ##delete the bar above if you want to get rid of the arrow
  scale_fill_manual(values = c("#7294D4", "#C6CDF7")) + ##colours##
  scale_pattern_manual( values = c('stripe','none'), ) +
  ggtitle('Physics') +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        legend.position="none",
        axis.title = element_blank(), 
        axis.text = element_text(size = 18), 
        legend.text = element_text(size = 16), 
        axis.ticks = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 20, face = 'bold'))

##Math Plot##
##Reverse the data so it says Male then Female##
Att_Math$Gender <- factor(Att_Math$Gender, levels = c("Male", "Female"))
Att_Math$Course <- factor(Att_Math$Course, levels = c("11U Functions", "12U Functions", "12U Calculus"))

##Plot the Math Attrition bargraph##
Math_bar <- ggplot(data = Att_Math) +
  geom_bar_pattern(aes(x = Course, y = Percent, fill = Gender, pattern = Gender),
                   stat = "identity",
                   position = position_dodge(),
                   pattern_size = 0,
                   pattern_colour = "#C16D00",
                   pattern_fill = "#C16D00",
                   pattern_spacing = 0.02) +
  geom_segment(data = Arrows_df, aes(x = x_pos, y = y_start, xend = x_pos, yend = y_M), arrow = arrow(length = unit(0.3, "cm"), type = "closed")) +
  geom_segment(data = Arrows_df, aes(x = x_pos, y = y_M, xend = x_pos, yend = y_start), arrow = arrow(length = unit(0.2, "cm"), angle = 90)) +
  geom_text(data = Arrows_df, 
            aes(label = M_loss, x = x_pos, hjust = rep(1.1,4), y = M_text),
            size = 6) +
  ##delete the line above if you want to get rid of the arrow
  scale_fill_manual(values = c("#F7BC25", "#feedc2")) + ##colours##
  scale_pattern_manual( values = c('stripe','none'), ) +
  ggtitle('Mathematics') +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        legend.position="none",
        axis.title = element_blank(), 
        axis.text = element_text(size = 18), 
        legend.text = element_text(size = 16), 
        axis.ticks = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 20, face = 'bold'))

## Make dummy legend ##
Leg_bar <- ggplot(data = Att_Phys) +
  geom_bar_pattern(aes(x = Course,y = Percent, fill = Gender, pattern = Gender),
                   stat = "identity",
                   position = position_dodge(),
                   pattern_size = 0,
                   pattern_colour = "black",
                   pattern_fill = "black",
                   pattern_spacing = 0.02) +
  scale_fill_manual('Sex', values = c("grey50", "grey80")) + 
  scale_pattern_manual('Sex', values = c('stripe','none'), ) +
  theme(legend.key.size = unit(1.3,"cm"), 
        legend.position = "bottom", 
        legend.direction = "vertical",
        legend.title = element_text(size = 22, face = "bold"),
        legend.text = element_text(size = 20))

Comb_bar <- plot_grid(Phys_bar, Bio_bar, Chem_bar, Math_bar, ncol = 2, labels = c('a.', 'b.', 'c.', 'd.'), label_size = 22)

y.grob <- textGrob("Student Continuation Rate (%)", 
                   gp=gpar(fontface="bold", fontsize=20), rot=90)

x.grob <- textGrob("Course", 
                   gp=gpar(fontface="bold", fontsize=20))

Comb_bar <- grid.arrange(arrangeGrob(Comb_bar, left = y.grob, bottom = x.grob))

AttritionPlot <- plot_grid(Comb_bar,
          get_legend(Leg_bar),
          ncol = 2,
          rel_widths = c(1.5, 0.2))


ggsave2(filename = 'Figure 4 - AttritionPlot.pdf',
        plot = AttritionPlot,
        device = 'pdf',
        path = 'C:/Users/ecor9/Google Drive/Grad School/Research/Data Sets/School Enrollment Analysis/ON Enrolment Trends Paper Graphics',
        scale = 2.05,
        width = 175,
        height = 130,
        units = "mm",
        dpi = 500,
        limitsize = TRUE
)

