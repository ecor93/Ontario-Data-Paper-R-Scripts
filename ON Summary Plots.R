library(tidyr)
library(plotrix)
library(ggplot2)
library(dplyr)
library(ggthemes)
library(patchwork)
library(ggpattern)
library(lme4) #Mixed effects models with lmer
library(lmerTest) #Adds p-values to lmer
library(ggforce) #Adds geom_link for gradient fill geom_segments
library(cowplot) #Adds guide_area for patchwork

#######################################################
##### Proportion of Female Students vs. Year Plot #####
#######################################################
#### Load Dataset & Calculate M/F Prop ####
ON_MFProp <- read.csv("C:/Users/ecor9/Google Drive/Grad School/Research/Data Sets/Ontario Enrollment Data Sets/ON_Combined.csv", na.strings = "<10") # Load csv while setting <10  suppressed cells to NA

ON_MFProp <- ON_MFProp  %>% #Transmute creates new columns based on the specified calculations and then deletes all others
  transmute(School.Number = School.Number, #Need to keep school number otherwise values aren't uniquely identified and the code won't work
            Course.Code = Course.Code, 
            Prop2018 = F18/(F18 + M18),  
            Prop2017 = F17/(F17 + M17),
            Prop2016 = F16/(F16 + M16),
            Prop2015 = F15/(F15 + M15),
            Prop2014 = F14/(F14 + M14),
            Prop2013 = F13/(F13 + M13),
            Prop2012 = F12/(F12 + M12),
            Prop2011 = F11/(F11 + M11),
            Prop2010 = F10/(F10 + M10),
            Prop2009 = F09/(F09 + M09),
            Prop2008 = F08/(F08 + M08)
            ) %>%
  pivot_longer(names_to = 'Year', values_to = 'Proportion', cols = c(3:13)) %>% #Transforms years from being spread horizontally to vertically
  na.omit() %>% #Delete years of missing/suppressed data
  pivot_wider(names_from = 'Course.Code', values_from = 'Proportion') %>% #transforms course code from being sread vertically to horizontally
  separate(col = 'Year', into = c('Prop', 'Year'), sep = 4) %>% #Splits the values in Year column after 4 characters, i.e. splits Prop18 into Prop and 18
  select(-c('Prop', 'School.Number')) #Drops the filler column "Prop" and School.Number as these will interfere with aggregate

#### Create DF of means with SE ####
ag <- aggregate(. ~ Year, data=ON_MFProp, FUN=function(x) c(median = median(x, na.rm = TRUE)*100, 
                                                            SE = std.error(x, na.rm = TRUE)*100, 
                                                            SD = sd(x, na.rm = TRUE)*100, 
                                                            Q = quantile(x, probs = 0.25, na.rm = TRUE)*100, 
                                                            Q = quantile(x, probs = 0.75, na.rm = TRUE)*100), na.action = na.pass)

ag_flat <- do.call("data.frame", ag) #turn ag into a proper DF

ag_flat_Median <- subset(ag_flat, select = c(1, seq(from = 2, to = 56, by = 5))) #split median/SE columns to avoid the data doubling up
ag_flat_SE <- subset(ag_flat, select = c(1, seq(from = 3, to = 56, by = 5))) #the seq function creates a vector of equally spaced numbers
ag_flat_SD <- subset(ag_flat, select = c(1, seq(from = 4, to = 56, by = 5)))
ag_flat_25 <- subset(ag_flat, select = c(1, seq(from = 5, to = 56, by = 5)))
ag_flat_75 <- subset(ag_flat, select = c(1, seq(from = 6, to = 56, by = 5)))


ag_flat_Median <- ag_flat_Median %>% gather(Course, Median, 2:12) %>% separate(Course, c("Course", "w")) #switch from long to tall, with fixing column names
ag_flat_SE <- ag_flat_SE %>% gather(Course, SE,2:12) %>% separate(Course, c("Course", "w"))
ag_flat_SD <- ag_flat_SD %>% gather(Course, SD,2:12) %>% separate(Course, c("Course", "w"))
ag_flat_25 <- ag_flat_25 %>% gather(Course, Q25,2:12) %>% separate(Course, c("Course", "w"), sep = 5)
ag_flat_75 <- ag_flat_75 %>% gather(Course, Q75,2:12) %>% separate(Course, c("Course", "w"), sep = 5)

ag_flat <- merge(merge(ag_flat_Median, ag_flat_SE, by = c("Course", "Year")), ag_flat_SD, by = c("Course", "Year"))
ag_flat <- subset(ag_flat, select = -c(3, 5, 7)) #delete filler columns "w"
ag_flat <- merge(merge(ag_flat, ag_flat_25, by = c("Course", "Year")), ag_flat_75, by = c("Course", "Year"))
ag_flat <- subset(ag_flat, select = -c(6, 8)) #delete filler columns "w"

ag_flat <- ag_flat %>% 
              mutate(Course2 = Course) %>%
              mutate(Course2 = recode(Course2,
                     "MHF4U" = "MHF3U")) %>% #Want 12U functions to plot looking like the 3U science courses
              separate(Course2, c("Stream", "Level"), 3) %>% #Makes two variables of the Stream, e.g. bio, phys and level, e.g., 3U, 4U for adjusting plotting aesthetics in ggplot2
              mutate(Stream = recode(Stream, 
                     "MCR" = "Math",
                     "MHF" = "Math",
                     "MCV" = "Math")) %>%
              mutate(Course = recode(Course,    #Rename variables to be clearer
                     "SBI3U" = "Bio 11",
                     "SBI4U" = "Bio 12",
                     "SCH3U" = "Chem 11",
                     "SCH4U" = "Chem 12",
                     "SPH3U" = "Phys 11",
                     "SPH4U" = "Phys 12",
                     "SNC2D" = "Sci 10",
                     "MPM2D" = "Math 10",
                     "MCR3U" = "Math 11",
                     "MHF4U" = "Math 12",
                     "MCV4U" = "Calc 12")) %>%
              mutate(Course = ordered(Course,  #Change order of factor variables in Course
                     levels = c(
                     "Bio 11", 
                     "Bio 12", 
                     "Chem 11", 
                     "Chem 12", 
                     "Sci 10",
                     "Math 11",
                     "Math 12",
                     "Calc 12",
                     "Phys 11", 
                     "Phys 12",                      
                     "Math 10")))

ag_flat_no10 <- ag_flat[which(ag_flat$Course != "Sci 10" & ag_flat$Course != "Math 10" & ag_flat$Course != "Math 11"),]

write.csv(ag_flat, "C:/Users/ecor9/Google Drive/Grad School/Research/Data Sets/Ontario Enrollment Data Sets/R Results/Median_FProp_vs_Year.csv", row.names = F)


#### Plot ####
Prop_Plot <- 
  ggplot(ag_flat_no10, aes(x = Year, y = Median, group = Course, color= Course)) +
  geom_line(aes(color = Course), size = 0.75, show.legend = F) +
  geom_errorbar(aes(ymin = Median-SE, ymax = Median+SE), width = 0.15, size = 0.75, show.legend = F)+
  geom_point(aes(fill = Course, shape = Level, size = Level), stroke = 1) +
  scale_fill_manual(values = c("#99ffce", "#00b39b", "#E6A0C4", "#FD6467", "#feedc2", "#f7bc25", "#C6CDF7", "#7294D4" )) + #, "grey70"
  scale_color_manual( values = c("#00b386", "#006658", "#cf4a8e", "#7e0204", "#F2AD00", "#A06609", "#798aec", "#30569c")) + #, "grey40"
  scale_shape_manual(values = c(21, 24)) +
  scale_size_manual(values = c(3, 2.5), guide = 'none') +
  scale_y_continuous(breaks = c(35, 40, 45, 50, 55, 60, 65),  labels = c('35%', '', '45%','', '55%', '', '65%')) +
  guides(fill=guide_legend(override.aes=list(shape=c(21, 24, 21, 24, 21, 24, 21, 24), size = rep(c(3, 2.5),4))), shape = 'none') + #, 22
  ylab("Median Proportion of Female Students")+
  #ggtitle("Median Proportion of Female Students in Science Courses 2008-2018") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        legend.position=c(.3,.93), legend.direction = "horizontal", legend.key.width = unit(.6,"cm"), legend.title = element_blank(),
        axis.title = element_text(size = 16, face = "bold"), axis.text = element_text(size = 14), legend.text = element_text(size = 14) )

Prop_Plot

ggsave2(filename = 'Figure 3 - Median Female Enrolment vs. Time.pdf',
        plot = Prop_Plot,
        device = 'pdf',
        path = 'C:/Users/ecor9/Google Drive/Grad School/Research/Data Sets/School Enrollment Analysis/ON Enrolment Trends Paper Graphics',
        scale = 2.5,
        width = 85,
        height = 75,
        units = "mm",
        dpi = 500,
        limitsize = TRUE
)

#### Regression of Year vs. Median Enrolment ####
ag_flat_Phys11 <-ag_flat[which(ag_flat$Course == "Phys 11"),]
ag_flat_Phys12 <-ag_flat[which(ag_flat$Course == "Phys 12"),]
ag_flat_Chem11 <-ag_flat[which(ag_flat$Course == "Chem 11"),]
ag_flat_Chem12 <-ag_flat[which(ag_flat$Course == "Chem 12"),]
ag_flat_Bio11 <-ag_flat[which(ag_flat$Course == "Bio 11"),]
ag_flat_Bio12 <-ag_flat[which(ag_flat$Course == "Bio 12"),]
ag_flat_Sci10 <-ag_flat[which(ag_flat$Course == "Sci 10"),]
ag_flat_Math11 <-ag_flat[which(ag_flat$Course == "Math 11"),]
ag_flat_Math12 <-ag_flat[which(ag_flat$Course == "Math 12"),]
ag_flat_Calc12 <-ag_flat[which(ag_flat$Course == "Calc 12"),]

Median_vs_Time_lm <- round(rbind(summary(lm(Median ~ as.numeric(Year), data = ag_flat_Sci10, weights = 1/(SD^2)))$coefficients[2, ],
                                 summary(lm(Median ~ as.numeric(Year), data = ag_flat_Bio11, weights = 1/(SD^2)))$coefficients[2, ],
                                 summary(lm(Median ~ as.numeric(Year), data = ag_flat_Bio12, weights = 1/(SD^2)))$coefficients[2, ],
                                 summary(lm(Median ~ as.numeric(Year), data = ag_flat_Chem11, weights = 1/(SD^2)))$coefficients[2, ],
                                 summary(lm(Median ~ as.numeric(Year), data = ag_flat_Chem12, weights = 1/(SD^2)))$coefficients[2, ],
                                 summary(lm(Median ~ as.numeric(Year), data = ag_flat_Phys11, weights = 1/(SD^2)))$coefficients[2, ],
                                 summary(lm(Median ~ as.numeric(Year), data = ag_flat_Phys12, weights = 1/(SD^2)))$coefficients[2, ],
                                 summary(lm(Median ~ as.numeric(Year), data = ag_flat_Math11, weights = 1/(SD^2)))$coefficients[2, ],
                                 summary(lm(Median ~ as.numeric(Year), data = ag_flat_Math12, weights = 1/(SD^2)))$coefficients[2, ],
                                 summary(lm(Median ~ as.numeric(Year), data = ag_flat_Calc12, weights = 1/(SD^2)))$coefficients[2, ]), 5)

Median_vs_Time_lm <- data.frame("Course" = c('Sci10', 'Bio11', 'Bio12', 'Chem11', 'Chem12', 'Phys11', 'Phys12', 'Math11', 'Math12', 'Calc12'), Median_vs_Time_lm)

write.csv(Median_vs_Time_lm, "C:/Users/ecor9/Google Drive/Grad School/Research/Data Sets/Ontario Enrollment Data Sets/R Results/ON_FProp_vs_Time_Regression_Results.csv", row.names = F)



#######################################
#### Total Enrolment vs. Time Plot ####
#######################################
#### Load datasets ####

## With <10 = 9 ##
ON_Enrol_Time <- read.csv("C:/Users/ecor9/Google Drive/Grad School/Research/Data Sets/Ontario Enrollment Data Sets/ON_Sup=9.csv", na.strings=c("<10","NA"))

ON_Enrol_Time <- subset(ON_Enrol_Time, select = -c(1:22))

ON_Enrol_Time <- aggregate(. ~ Course.Code, data=ON_Enrol_Time, FUN=sum)

ON_Enrol_Time <- ON_Enrol_Time %>% pivot_longer(cols=c(2:23), names_to='sex/year', values_to='Total_Enrolment')%>%
  separate(col = `sex/year`, into = c("Sex", "Year"), sep = 1)

## Adjust Factor Levels ##
ON_Enrol_Time$Sex <- factor(ON_Enrol_Time$Sex, levels = c('M', 'F'))
ON_Enrol_Time <- ON_Enrol_Time %>% mutate(Course.Code = recode(Course.Code,                                 
                                                               "MPM2D" = "10D Math",
                                                               "MCR3U" = "11U Functions",
                                                               "MHF4U" = "12U Functions",
                                                               "MCV4U" = "12U Calculus",
                                                               "SBI3U" = "11U Biology",
                                                               "SBI4U" = "12U Biology",
                                                               "SCH3U" = "11U Chemistry",
                                                               "SCH4U" = "12U Chemistry",
                                                               "SPH3U" = "11U Physics",
                                                               "SPH4U" = "12U Physics",
                                                               "SNC2D" = "10D Science")) %>%
                                    mutate(Sex = recode(Sex,
                                                         "M" = "Male",
                                                         "F" = "Female"))

ON_Enrol_Time$Course.Code <- factor(ON_Enrol_Time$Course.Code,levels = c("10D Math", "10D Science", "11U Functions", "11U Biology", "11U Chemistry", "11U Physics", "12U Functions", "12U Calculus", "12U Biology", "12U Chemistry", "12U Physics"))

ON_Enrol_Time_12U <- ON_Enrol_Time[(ON_Enrol_Time$Course.Code == "12U Biology" | ON_Enrol_Time$Course.Code =="12U Chemistry" | ON_Enrol_Time$Course.Code =="12U Physics" | ON_Enrol_Time$Course.Code == "12U Functions" | ON_Enrol_Time$Course.Code == "12U Calculus"),]
ON_Enrol_Time_11U <- ON_Enrol_Time[(ON_Enrol_Time$Course.Code == "10D Science" | ON_Enrol_Time$Course.Code == "11U Biology" | ON_Enrol_Time$Course.Code =="11U Chemistry" | ON_Enrol_Time$Course.Code =="11U Physics" | ON_Enrol_Time$Course.Code == "11U Functions"),]

ON_Enrol_Time_12U_Phys <- ON_Enrol_Time_12U[(ON_Enrol_Time_12U$Course.Code == '12U Physics'),]
write.csv(ON_Enrol_Time_12U_Phys, "C:/Users/ecor9/Google Drive/Grad School/Research/Data Sets/Ontario Enrollment Data Sets/R Results/Total_MF_Enrol_12U_Phys_vs_Time.csv", row.names = F)
#### Regression of Total Enrol vs. Time ####
ON_Enrol_Time <- ON_Enrol_Time %>% pivot_wider(names_from = "Sex", values_from = "Total_Enrolment")%>%
                                   mutate(Total_Enrolment = Male + Female,
                                          Year = as.numeric(Year)-8)

ON_Enrol_Time_Phys11 <-ON_Enrol_Time[which(ON_Enrol_Time$Course.Code == "11U Physics"),]
ON_Enrol_Time_Phys12 <-ON_Enrol_Time[which(ON_Enrol_Time$Course.Code == "12U Physics"),]
ON_Enrol_Time_Chem11 <-ON_Enrol_Time[which(ON_Enrol_Time$Course.Code == "11U Chemistry"),]
ON_Enrol_Time_Chem12 <-ON_Enrol_Time[which(ON_Enrol_Time$Course.Code == "12U Chemistry"),]
ON_Enrol_Time_Bio11 <-ON_Enrol_Time[which(ON_Enrol_Time$Course.Code == "11U Biology"),]
ON_Enrol_Time_Bio12 <-ON_Enrol_Time[which(ON_Enrol_Time$Course.Code == "12U Biology"),]
ON_Enrol_Time_Sci10 <-ON_Enrol_Time[which(ON_Enrol_Time$Course.Code == "10D Science"),]
ON_Enrol_Time_Math11 <-ON_Enrol_Time[which(ON_Enrol_Time$Course.Code == "11U Functions"),]
ON_Enrol_Time_Math12 <-ON_Enrol_Time[which(ON_Enrol_Time$Course.Code == "12U Functions"),]
ON_Enrol_Time_Calc12 <-ON_Enrol_Time[which(ON_Enrol_Time$Course.Code == "12U Calculus"),]



#### Calculate Regression Models #####
Enrol_vs_Time_lm <- rbind(summary(lm( Total_Enrolment ~ as.numeric(Year), data = ON_Enrol_Time_Sci10))$coefficients[c(1,2), ],
                          summary(lm( Total_Enrolment ~ as.numeric(Year), data = ON_Enrol_Time_Bio11))$coefficients[c(1,2), ],
                          summary(lm( Total_Enrolment ~ as.numeric(Year), data = ON_Enrol_Time_Chem11))$coefficients[c(1,2), ],
                          summary(lm( Total_Enrolment ~ as.numeric(Year), data = ON_Enrol_Time_Phys11))$coefficients[c(1,2), ],
                          summary(lm( Total_Enrolment ~ as.numeric(Year), data = ON_Enrol_Time_Bio12))$coefficients[c(1,2), ],
                          summary(lm( Total_Enrolment ~ as.numeric(Year), data = ON_Enrol_Time_Chem12))$coefficients[c(1,2), ],
                          summary(lm( Total_Enrolment ~ as.numeric(Year), data = ON_Enrol_Time_Phys12))$coefficients[c(1,2), ],
                          summary(lm( Total_Enrolment ~ as.numeric(Year), data = ON_Enrol_Time_Math11))$coefficients[c(1,2), ],
                          summary(lm( Total_Enrolment ~ as.numeric(Year), data = ON_Enrol_Time_Math12))$coefficients[c(1,2), ],
                          summary(lm( Total_Enrolment ~ as.numeric(Year), data = ON_Enrol_Time_Calc12))$coefficients[c(1,2), ])


Enrol_vs_Time_lm <- data.frame("Course.Code" = c('10D Science','10D Science', '11U Biology','11U Biology', '11U Chemistry','11U Chemistry', '11U Physics', '11U Physics','12U Biology', '12U Biology','12U Chemistry', '12U Chemistry','12U Physics', '12U Physics','11U Functions', '11U Functions','12U Functions', '12U Functions', '12U Calculus','12U Calculus'), Enrol_vs_Time_lm)
Enrol_vs_Time_lm <- data.frame("Type" = rep(c("Intercept", "Slope"), 10), Enrol_vs_Time_lm)

write.csv(Enrol_vs_Time_lm, "C:/Users/ecor9/Google Drive/Grad School/Research/Data Sets/Ontario Enrollment Data Sets/R Results/ON_Total_Enrol_vs_Time_Regression_Results.csv")

Enrol_vs_Time_Est <- subset(Enrol_vs_Time_lm, select = c(1:3)) %>%
  pivot_wider(values_from = Estimate, names_from = Type)

Enrol_vs_Time_lm <- Enrol_vs_Time_lm[(Enrol_vs_Time_lm$Type != "Intercept"),]

Enrol_vs_Time_lm <- Enrol_vs_Time_lm %>%
  mutate(Sig = if_else(Pr...t..< 0.05, 'p < 0.05', 'p > 0.05'),
         Year = rep("13", 10)) %>%
  mutate(Sig = as.factor(Sig))

Enrol_vs_Time_lm <- merge(Enrol_vs_Time_lm, Enrol_vs_Time_Est, by = 'Course.Code')

Enrol_vs_Time_lm$Course.Code <- factor(Enrol_vs_Time_lm$Course.Code,levels = c("10D Math", "10D Science", "11U Functions", "11U Biology", "11U Chemistry", "11U Physics", "12U Functions", "12U Calculus", "12U Biology", "12U Chemistry", "12U Physics"))

## round significant digits and concatenate ##
Enrol_vs_Time_lm$Std..Error <- ceiling(Enrol_vs_Time_lm$Std..Error) 
#Enrol_vs_Time_lm <- Enrol_vs_Time_lm %>% mutate(Digits = if_else(Std..Error < 100 & Estimate > 100, 3, 2))
Enrol_vs_Time_lm$Estimate <- ceiling(Enrol_vs_Time_lm$Estimate)
Enrol_vs_Time_lm <- Enrol_vs_Time_lm %>% mutate(Change = paste(if_else(Enrol_vs_Time_lm$Estimate > 0, "+",""),Estimate,"±",Std..Error,' Students/Year', sep = ""))

Enrol_vs_Time_lm_12U <- Enrol_vs_Time_lm[(Enrol_vs_Time_lm$Course.Code == "12U Biology" | Enrol_vs_Time_lm$Course.Code =="12U Chemistry" | Enrol_vs_Time_lm$Course.Code =="12U Physics" | Enrol_vs_Time_lm$Course.Code == "12U Functions" | Enrol_vs_Time_lm$Course.Code == "12U Calculus"),]
Enrol_vs_Time_lm_11U <- Enrol_vs_Time_lm[(Enrol_vs_Time_lm$Course.Code == "10D Science" | Enrol_vs_Time_lm$Course.Code == "11U Biology" | Enrol_vs_Time_lm$Course.Code =="11U Chemistry" | Enrol_vs_Time_lm$Course.Code =="11U Physics" | Enrol_vs_Time_lm$Course.Code == "11U Functions"),]

Enrol_vs_Time_lm_11U <- Enrol_vs_Time_lm_11U %>% mutate(Total_Enrolment = c(115000,
                                                                            75000,
                                                                            75000,
                                                                            83000,
                                                                            55000))

Enrol_vs_Time_lm_12U <- Enrol_vs_Time_lm_12U %>% mutate(Total_Enrolment = c(47000,
                                                                            47000,
                                                                            47000,
                                                                            59000,
                                                                            32000))


#### Plot ####
Enrol_vs_Time_11 <- ggplot(ON_Enrol_Time_11U) +
  geom_col_pattern(aes(fill = Sex, colour = Sex, y= Total_Enrolment, x = Year, pattern = Sex),
                   pattern_density = 0.4,
                   pattern_colour = "#006658",
                   pattern_fill = "#006658",
                   width = 1,
                   size = 0.8) +
  facet_grid(.~Course.Code) +
  ylab("Province Wide Enrolment") +
  scale_fill_manual(values = c("#00b39b", "#FD6467")) +
  scale_color_manual(values = c("#006658", "#7e0204")) +
  scale_pattern_manual( values = c('circle','none'), ) +
  scale_y_continuous(limits = c(0, 120000), 
                     breaks = c(12500, 25000, 37500, 50000, 62500, 75000, 87500, 100000, 112500),
                     labels = c('', '25,000', '', '50,000', '', '75,000', '', '100,000', '')) +
  scale_x_discrete(labels = c('2008', '', '2010', '', '2012', '', '2014', '', '2016', '', '2018')) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "grey95"), 
        axis.line = element_line(colour = "black"), 
        axis.title = element_text(size = 22, face = "bold"), 
        axis.text.y = element_text(size = 18),
        axis.text.x = element_text(angle=55, vjust=1, hjust=1, size = 18),
        strip.text.x = element_text(size = 24, face = "bold"),
        strip.background = element_rect(fill="White"))

Enrol_vs_Time_11 <- Enrol_vs_Time_11 +  
  geom_abline(data = Enrol_vs_Time_lm_11U, 
              aes(slope = Slope, intercept = Intercept + 5000, linetype = Sig),
              size = 1.5) +
  geom_text(data = Enrol_vs_Time_lm_11U, 
             aes(x = Year, y = Total_Enrolment, label = Change),
             hjust = 0.5,
             size = 6) + 
  guides(shape = guide_legend(override.aes = list(shape = c(21, 23), 
                                                  size = c(4.5, 3.5), 
                                                  linetype = 0, 
                                                  fill = c("#FD6467", "#00b39b"),
                                                  colour = c("#7e0204", "#006658"),
                                                  stroke = 1.2))) +
  theme(legend.position = "none")



Enrol_vs_Time_12 <- ggplot(ON_Enrol_Time_12U) +
  geom_col_pattern(aes(fill = Sex, color = Sex, y= Total_Enrolment, x = Year, pattern = Sex),
                   pattern_density = 0.4,
                   pattern_colour = "#006658",
                   pattern_fill = "#006658",
                   width = 1,
                   size = 0.8) + 
  facet_grid(.~Course.Code) +
  ylab("Province Wide Enrolment") +
  scale_fill_manual(values = c("#00b39b", "#FD6467")) +
  scale_color_manual(values = c("#006658", "#7e0204")) +
  scale_pattern_manual( values = c('circle','none'), ) +
  scale_y_continuous(limits = c(0, 60000), 
                     breaks = c(12500, 25000, 37500, 50000),
                     labels = c('', '25,000', '', '50,000')) +
  scale_x_discrete(labels = c('2008', '', '2010', '', '2012', '', '2014', '', '2016', '', '2018')) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "grey95"), 
        axis.line = element_line(colour = "black"), 
        axis.title = element_text(size = 22, face = "bold"), 
        axis.text.y = element_text(size = 18),
        axis.text.x = element_text(angle=55, vjust=1, hjust=1, size = 18),
        strip.text.x = element_text(size = 24, face = "bold"),
        strip.background = element_rect(fill="White"))

Enrol_vs_Time_12 <- Enrol_vs_Time_12 +  
  geom_abline(data = Enrol_vs_Time_lm_12U, 
              aes(slope = Slope, intercept = Intercept + c(rep(5000,3), 3500, 5000), linetype = Sig),
              size = 1.5) +
  geom_text(data = Enrol_vs_Time_lm_12U, 
                   aes(x = Year, y = Total_Enrolment, label = Change),
                   hjust = 0.5,
                   size = 6)+ 
  labs(linetype = "Regression Estimates")+
  guides(linetype = guide_legend(order = 2),
         color = guide_legend(order = 1),
         pattern = guide_legend(order = 1),
         fill = guide_legend(order = 1)) +
  theme(legend.direction = "horizontal",
        legend.position = "bottom",
        legend.key.size = unit(1,"cm"),
        legend.text = element_text(size = 18),
        legend.title = element_text(size = 22, face = "bold"))

Enrol_vs_Time <- Enrol_vs_Time_11 + Enrol_vs_Time_12 + plot_layout(ncol = 1,
                                                                 guides = 'keep',
                                                                 heights = c(1.5, 1.5)) +
                                                    plot_annotation(tag_levels = ('a'),
                                                                    tag_suffix = '.') & 
                                                    theme(plot.tag = element_text(size = 26))
ggsave2(filename = 'Figure 2 - Total Enrolment vs. Time.pdf',
        plot = Enrol_vs_Time,
        device = 'pdf',
        path = 'C:/Users/ecor9/Google Drive/Grad School/Research/Data Sets/School Enrollment Analysis/ON Enrolment Trends Paper Graphics',
        scale = 2.5,
        width = 175,
        height = 120,
        units = "mm",
        dpi = 500,
        limitsize = TRUE
        )


##############################################
#### Regression of Year vs. Individual School M/F Attrition Rate ####
##############################################
#### Loading Data ####
ON_Att <-  read.csv("C:/Users/ecor9/Google Drive/Grad School/Research/Data Sets/Ontario Enrollment Data Sets/ON_Combined.csv", na.strings = "<10")
ON_Att <- ON_Att %>% select(c(School.Number, 23:45))

## rearrange math data set to be same format as science ##
ON_Att_Math_3U <- ON_Att[which(ON_Att$Course.Code == "MCR3U"),]
ON_Att_Math_10 <- ON_Att[which(ON_Att$Course.Code == "MPM2D"),]
ON_Att_Math <- ON_Att[which(ON_Att$Course.Code == "MHF4U" | ON_Att$Course.Code == "MCV4U"),]
ON_Att_Math <- merge(ON_Att_Math, ON_Att_Math_3U, by = "School.Number")
ON_Att_Math_3U <- merge(ON_Att_Math_3U, ON_Att_Math_10, by = "School.Number")

## Separate Science Courses ##
ON_Att10 <- ON_Att[ON_Att$Course.Code == 'SNC2D',]
ON_Att <- ON_Att[!(ON_Att$Course.Code == 'SNC2D' | ON_Att$Course.Code == 'MPM2D' | ON_Att$Course.Code == 'MCR3U' | ON_Att$Course.Code == 'MHF4U' | ON_Att$Course.Code == 'MCV4U'),]

ON_Att <- merge(ON_Att, ON_Att10, by = "School.Number")

#### Calculate Attrition Rates ####
ON_Att_3U <- ON_Att[!(ON_Att$Course.Code.x == "SBI4U" | ON_Att$Course.Code.x =="SCH4U" | ON_Att$Course.Code.x =="SPH4U"),]
ON_Att_4U <- ON_Att[!(ON_Att$Course.Code.x == "SBI3U" | ON_Att$Course.Code.x =="SCH3U" | ON_Att$Course.Code.x =="SPH3U"),]

## Calculate 11U Courses Attrition ##
ON_Att_3U$MProp18 <- ON_Att_3U$M18.x/ON_Att_3U$M17.y
ON_Att_3U$MProp17 <- ON_Att_3U$M17.x/ON_Att_3U$M16.y
ON_Att_3U$MProp16 <- ON_Att_3U$M16.x/ON_Att_3U$M15.y
ON_Att_3U$MProp15 <- ON_Att_3U$M15.x/ON_Att_3U$M14.y
ON_Att_3U$MProp14 <- ON_Att_3U$M14.x/ON_Att_3U$M13.y
ON_Att_3U$MProp13 <- ON_Att_3U$M13.x/ON_Att_3U$M12.y
ON_Att_3U$MProp12 <- ON_Att_3U$M12.x/ON_Att_3U$M11.y
ON_Att_3U$MProp11 <- ON_Att_3U$M11.x/ON_Att_3U$M10.y
ON_Att_3U$MProp10 <- ON_Att_3U$M10.x/ON_Att_3U$M09.y
ON_Att_3U$MProp9 <- ON_Att_3U$M09.x/ON_Att_3U$M08.y

ON_Att_3U$FProp18 <- ON_Att_3U$F18.x/ON_Att_3U$F17.y
ON_Att_3U$FProp17 <- ON_Att_3U$F17.x/ON_Att_3U$F16.y
ON_Att_3U$FProp16 <- ON_Att_3U$F16.x/ON_Att_3U$F15.y
ON_Att_3U$FProp15 <- ON_Att_3U$F15.x/ON_Att_3U$F14.y
ON_Att_3U$FProp14 <- ON_Att_3U$F14.x/ON_Att_3U$F13.y
ON_Att_3U$FProp13 <- ON_Att_3U$F13.x/ON_Att_3U$F12.y
ON_Att_3U$FProp12 <- ON_Att_3U$F12.x/ON_Att_3U$F11.y
ON_Att_3U$FProp11 <- ON_Att_3U$F11.x/ON_Att_3U$F10.y
ON_Att_3U$FProp10 <- ON_Att_3U$F10.x/ON_Att_3U$F09.y
ON_Att_3U$FProp9 <- ON_Att_3U$F09.x/ON_Att_3U$F08.y

## Calculate 12U Courses Attrition ##
ON_Att_4U$MProp18 <- ON_Att_4U$M18.x/ON_Att_4U$M16.y
ON_Att_4U$MProp17 <- ON_Att_4U$M17.x/ON_Att_4U$M15.y
ON_Att_4U$MProp16 <- ON_Att_4U$M16.x/ON_Att_4U$M14.y
ON_Att_4U$MProp15 <- ON_Att_4U$M15.x/ON_Att_4U$M13.y
ON_Att_4U$MProp14 <- ON_Att_4U$M14.x/ON_Att_4U$M12.y
ON_Att_4U$MProp13 <- ON_Att_4U$M13.x/ON_Att_4U$M11.y
ON_Att_4U$MProp12 <- ON_Att_4U$M12.x/ON_Att_4U$M10.y
ON_Att_4U$MProp11 <- ON_Att_4U$M11.x/ON_Att_4U$M09.y
ON_Att_4U$MProp10 <- ON_Att_4U$M10.x/ON_Att_4U$M08.y
ON_Att_4U$MProp9 <- NA

ON_Att_4U$FProp18 <- ON_Att_4U$F18.x/ON_Att_4U$F16.y
ON_Att_4U$FProp17 <- ON_Att_4U$F17.x/ON_Att_4U$F15.y
ON_Att_4U$FProp16 <- ON_Att_4U$F16.x/ON_Att_4U$F14.y
ON_Att_4U$FProp15 <- ON_Att_4U$F15.x/ON_Att_4U$F13.y
ON_Att_4U$FProp14 <- ON_Att_4U$F14.x/ON_Att_4U$F12.y
ON_Att_4U$FProp13 <- ON_Att_4U$F13.x/ON_Att_4U$F11.y
ON_Att_4U$FProp12 <- ON_Att_4U$F12.x/ON_Att_4U$F10.y
ON_Att_4U$FProp11 <- ON_Att_4U$F11.x/ON_Att_4U$F09.y
ON_Att_4U$FProp10 <- ON_Att_4U$F10.x/ON_Att_4U$F08.y
ON_Att_4U$FProp9 <- NA

## Calculate Math Courses Attrition ##
## 12U Courses ##
ON_Att_Math$MProp18 <- ON_Att_Math$M18.x/ON_Att_Math$M17.y
ON_Att_Math$MProp17 <- ON_Att_Math$M17.x/ON_Att_Math$M16.y
ON_Att_Math$MProp16 <- ON_Att_Math$M16.x/ON_Att_Math$M15.y
ON_Att_Math$MProp15 <- ON_Att_Math$M15.x/ON_Att_Math$M14.y
ON_Att_Math$MProp14 <- ON_Att_Math$M14.x/ON_Att_Math$M13.y
ON_Att_Math$MProp13 <- ON_Att_Math$M13.x/ON_Att_Math$M12.y
ON_Att_Math$MProp12 <- ON_Att_Math$M12.x/ON_Att_Math$M11.y
ON_Att_Math$MProp11 <- ON_Att_Math$M11.x/ON_Att_Math$M10.y
ON_Att_Math$MProp10 <- ON_Att_Math$M10.x/ON_Att_Math$M09.y
ON_Att_Math$MProp9 <- ON_Att_Math$M09.x/ON_Att_Math$M08.y

ON_Att_Math$FProp18 <- ON_Att_Math$F18.x/ON_Att_Math$F17.y
ON_Att_Math$FProp17 <- ON_Att_Math$F17.x/ON_Att_Math$F16.y
ON_Att_Math$FProp16 <- ON_Att_Math$F16.x/ON_Att_Math$F15.y
ON_Att_Math$FProp15 <- ON_Att_Math$F15.x/ON_Att_Math$F14.y
ON_Att_Math$FProp14 <- ON_Att_Math$F14.x/ON_Att_Math$F13.y
ON_Att_Math$FProp13 <- ON_Att_Math$F13.x/ON_Att_Math$F12.y
ON_Att_Math$FProp12 <- ON_Att_Math$F12.x/ON_Att_Math$F11.y
ON_Att_Math$FProp11 <- ON_Att_Math$F11.x/ON_Att_Math$F10.y
ON_Att_Math$FProp10 <- ON_Att_Math$F10.x/ON_Att_Math$F09.y
ON_Att_Math$FProp9 <- ON_Att_Math$F09.x/ON_Att_Math$F08.y

## 11U Math ##
ON_Att_Math_3U$MProp18 <- ON_Att_Math_3U$M18.x/ON_Att_Math_3U$M17.y
ON_Att_Math_3U$MProp17 <- ON_Att_Math_3U$M17.x/ON_Att_Math_3U$M16.y
ON_Att_Math_3U$MProp16 <- ON_Att_Math_3U$M16.x/ON_Att_Math_3U$M15.y
ON_Att_Math_3U$MProp15 <- ON_Att_Math_3U$M15.x/ON_Att_Math_3U$M14.y
ON_Att_Math_3U$MProp14 <- ON_Att_Math_3U$M14.x/ON_Att_Math_3U$M13.y
ON_Att_Math_3U$MProp13 <- ON_Att_Math_3U$M13.x/ON_Att_Math_3U$M12.y
ON_Att_Math_3U$MProp12 <- ON_Att_Math_3U$M12.x/ON_Att_Math_3U$M11.y
ON_Att_Math_3U$MProp11 <- ON_Att_Math_3U$M11.x/ON_Att_Math_3U$M10.y
ON_Att_Math_3U$MProp10 <- ON_Att_Math_3U$M10.x/ON_Att_Math_3U$M09.y
ON_Att_Math_3U$MProp9 <- ON_Att_Math_3U$M09.x/ON_Att_Math_3U$M08.y

ON_Att_Math_3U$FProp18 <- ON_Att_Math_3U$F18.x/ON_Att_Math_3U$F17.y
ON_Att_Math_3U$FProp17 <- ON_Att_Math_3U$F17.x/ON_Att_Math_3U$F16.y
ON_Att_Math_3U$FProp16 <- ON_Att_Math_3U$F16.x/ON_Att_Math_3U$F15.y
ON_Att_Math_3U$FProp15 <- ON_Att_Math_3U$F15.x/ON_Att_Math_3U$F14.y
ON_Att_Math_3U$FProp14 <- ON_Att_Math_3U$F14.x/ON_Att_Math_3U$F13.y
ON_Att_Math_3U$FProp13 <- ON_Att_Math_3U$F13.x/ON_Att_Math_3U$F12.y
ON_Att_Math_3U$FProp12 <- ON_Att_Math_3U$F12.x/ON_Att_Math_3U$F11.y
ON_Att_Math_3U$FProp11 <- ON_Att_Math_3U$F11.x/ON_Att_Math_3U$F10.y
ON_Att_Math_3U$FProp10 <- ON_Att_Math_3U$F10.x/ON_Att_Math_3U$F09.y
ON_Att_Math_3U$FProp9 <- ON_Att_Math_3U$F09.x/ON_Att_Math_3U$F08.y

ON_Att_Math <- subset(ON_Att_Math, select = -c(3:47) )
ON_Att_Math_3U <- subset(ON_Att_Math_3U, select = -c(3:47) )
ON_Att_3U <- subset(ON_Att_3U, select = -c(3:47) ) 
ON_Att_4U <- subset(ON_Att_4U, select = -c(3:47) ) 

ON_Att <- rbind(ON_Att_3U, ON_Att_4U, ON_Att_Math, ON_Att_Math_3U)

#### Calculate Regression Models #####
ON_Att_Long <- ON_Att %>% pivot_longer(cols=c(3:22),
                                       names_to='Year/Sex',
                                       values_to='Attrition_Prop') %>%
                          separate(col = 'Year/Sex',
                                   sep = c(1,5),
                                   into = c("Sex", NA, "Year")) %>%
                          pivot_wider(names_from = 'Course.Code.x',
                                      values_from = 'Attrition_Prop')

ON_Att_Long$School.Number <- as.factor(ON_Att_Long$School.Number)
ON_Att_Long$Year <- as.numeric(ON_Att_Long$Year)
ON_Att_Long[4:12] <- ON_Att_Long[4:12]*100


# calculate male regression coefficients, with M as the reference category
ON_Att_Long$Sex <- factor(ON_Att_Long$Sex, levels = c('M', 'F'))

Att_vs_Time_lm_M <- rbind(summary(lmer( SBI3U ~ Year*Sex + (0 + Year|School.Number), data = ON_Att_Long))$coefficients[c(2), ],
                          summary(lmer( SCH3U ~ Year*Sex + (0 + Year|School.Number), data = ON_Att_Long))$coefficients[c(2), ],
                          summary(lmer( SPH3U ~ Year*Sex + (0 + Year|School.Number), data = ON_Att_Long))$coefficients[c(2), ],
                          summary(lmer( SBI4U ~ Year*Sex + (0 + Year|School.Number), data = ON_Att_Long))$coefficients[c(2), ],
                          summary(lmer( SCH4U ~ Year*Sex + (0 + Year|School.Number), data = ON_Att_Long))$coefficients[c(2), ],
                          summary(lmer( SPH4U ~ Year*Sex + (0 + Year|School.Number), data = ON_Att_Long))$coefficients[c(2), ],
                          #summary(lmer( MCR3U ~ Year*Sex + (0 + Year|School.Number), data = ON_Att_Long))$coefficients[c(2), ],
                          summary(lmer( MHF4U ~ Year*Sex + (0 + Year|School.Number), data = ON_Att_Long))$coefficients[c(2), ],
                          summary(lmer( MCV4U ~ Year*Sex + (0 + Year|School.Number), data = ON_Att_Long))$coefficients[c(2), ])

#calculate female regression coefficients
ON_Att_Long$Sex <- factor(ON_Att_Long$Sex, levels = c('F', 'M'))

Att_vs_Time_lm_F  <-  rbind(summary(lmer( SBI3U ~ Year*Sex + (0 + Year|School.Number), data = ON_Att_Long))$coefficients[c(2), ],
                            summary(lmer( SCH3U ~ Year*Sex + (0 + Year|School.Number), data = ON_Att_Long))$coefficients[c(2), ],
                            summary(lmer( SPH3U ~ Year*Sex + (0 + Year|School.Number), data = ON_Att_Long))$coefficients[c(2), ],
                            summary(lmer( SBI4U ~ Year*Sex + (0 + Year|School.Number), data = ON_Att_Long))$coefficients[c(2), ],
                            summary(lmer( SCH4U ~ Year*Sex + (0 + Year|School.Number), data = ON_Att_Long))$coefficients[c(2), ],
                            summary(lmer( SPH4U ~ Year*Sex + (0 + Year|School.Number), data = ON_Att_Long))$coefficients[c(2), ],
                            #summary(lmer( MCR3U ~ Year*Sex + (0 + Year|School.Number), data = ON_Att_Long))$coefficients[c(2), ],
                            summary(lmer( MHF4U ~ Year*Sex + (0 + Year|School.Number), data = ON_Att_Long))$coefficients[c(2), ],
                            summary(lmer( MCV4U ~ Year*Sex + (0 + Year|School.Number), data = ON_Att_Long))$coefficients[c(2), ])

#calculate interaction terms for M/F difference to calculate if the difference is stat sig
Att_vs_Time_lm_diff  <-  rbind(summary(lmer( SBI3U ~ Year*Sex + (0 + Year|School.Number), data = ON_Att_Long))$coefficients[c(4), ],
                            summary(lmer( SCH3U ~ Year*Sex + (0 + Year|School.Number), data = ON_Att_Long))$coefficients[c(4), ],
                            summary(lmer( SPH3U ~ Year*Sex + (0 + Year|School.Number), data = ON_Att_Long))$coefficients[c(4), ],
                            summary(lmer( SBI4U ~ Year*Sex + (0 + Year|School.Number), data = ON_Att_Long))$coefficients[c(4), ],
                            summary(lmer( SCH4U ~ Year*Sex + (0 + Year|School.Number), data = ON_Att_Long))$coefficients[c(4), ],
                            summary(lmer( SPH4U ~ Year*Sex + (0 + Year|School.Number), data = ON_Att_Long))$coefficients[c(4), ],
                            #summary(lmer( MCR3U ~ Year*Sex + (0 + Year|School.Number), data = ON_Att_Long))$coefficients[c(4), ],
                            summary(lmer( MHF4U ~ Year*Sex + (0 + Year|School.Number), data = ON_Att_Long))$coefficients[c(4), ],
                            summary(lmer( MCV4U ~ Year*Sex + (0 + Year|School.Number), data = ON_Att_Long))$coefficients[c(4), ])

## attach course names
Att_vs_Time_lm_M <- cbind("Sex" = rep('Male', 8), #switch to 9 if including 11U Math 
                          data.frame("Course" = c('11U Biology', 
                                                  '11U Chemistry', 
                                                  '11U Physics', 
                                                  '12U Biology', 
                                                  '12U Chemistry', 
                                                  '12U Physics', 
                                                  #'11U Math', 
                                                  '12U Functions', 
                                                  '12U Calculus')),
                          Att_vs_Time_lm_M)

Att_vs_Time_lm_F <- cbind("Sex" = rep('Female', 8), 
                          data.frame("Course" = c('11U Biology', 
                                                  '11U Chemistry', 
                                                  '11U Physics', 
                                                  '12U Biology', 
                                                  '12U Chemistry', 
                                                  '12U Physics', 
                                                  #'11U Math', 
                                                  '12U Functions', 
                                                  '12U Calculus')),
                          Att_vs_Time_lm_F)

Att_vs_Time_lm_diff <- cbind("Sex" = rep('Diff', 8), 
                          data.frame("Course" = c('11U Biology', 
                                                  '11U Chemistry', 
                                                  '11U Physics', 
                                                  '12U Biology', 
                                                  '12U Chemistry', 
                                                  '12U Physics', 
                                                  #'11U Math', 
                                                  '12U Functions', 
                                                  '12U Calculus')),
                          Att_vs_Time_lm_diff)

#### Save regression outputs to .csv to easily write tables ####
write.csv(Att_vs_Time_lm_M, "C:/Users/ecor9/Google Drive/Grad School/Research/Data Sets/Ontario Enrollment Data Sets/R Results/Att_Regression_M.csv")
write.csv(Att_vs_Time_lm_F, "C:/Users/ecor9/Google Drive/Grad School/Research/Data Sets/Ontario Enrollment Data Sets/R Results/Att_Regression_F.csv")
write.csv(Att_vs_Time_lm_diff, "C:/Users/ecor9/Google Drive/Grad School/Research/Data Sets/Ontario Enrollment Data Sets/R Results/Att_Regression_Diff.csv")


#### spread diff values and combine both sexes with this ####
Att_vs_Time_lm_M <- Att_vs_Time_lm_M %>% pivot_wider(names_from = 'Sex', values_from = c('Estimate', 'Pr(>|t|)', 'Std. Error')) %>%
                      rename(p_value = 'Pr(>|t|)_Male',
                             SE = 'Std. Error_Male') %>%
                      transmute(Course = Course,
                                Male.Est = Estimate_Male,
                                Male.p = p_value,
                                Male.SE = SE) #commnet out to do first plot style

Att_vs_Time_lm_F <- Att_vs_Time_lm_F %>% pivot_wider(names_from = 'Sex', values_from = c('Estimate', 'Pr(>|t|)', 'Std. Error')) %>%
                      rename(p_value = 'Pr(>|t|)_Female',
                             SE = 'Std. Error_Female') %>%
                      transmute(Course = Course,
                                Female.Est = Estimate_Female,
                                Female.p = p_value,
                                Female.SE = SE) #commnet out to do first plot style

Att_vs_Time_lm_diff <- Att_vs_Time_lm_diff %>% pivot_wider(names_from = 'Sex', values_from = c('Estimate', 'Pr(>|t|)', 'Std. Error')) %>%
  rename(p_value = 'Pr(>|t|)_Diff',
         SE = 'Std. Error_Diff') %>%
  transmute(Course = Course,
            Diff.Est = round(Estimate_Diff, 3),
            Diff.p = p_value,
            Diff.SE = SE)

Att_vs_Time_lm_M <- merge(Att_vs_Time_lm_M, Att_vs_Time_lm_F) #need to change to use first plotting style
Att_vs_Time_lm <- merge(Att_vs_Time_lm_M, Att_vs_Time_lm_diff)
  
Att_vs_Time_lm <- Att_vs_Time_lm %>%
  mutate(M.Sig = if_else(Male.p < 0.05, 'M.Sig', 'M.Not.Sig')) %>%
  mutate(F.Sig = if_else(Female.p < 0.05, 'F.Sig', 'F.Not.Sig')) %>%
  mutate(Diff.Sig = if_else(Diff.p > 0.05, Course, NULL)) %>%
  mutate(Text.Pos = (Male.Est + Female.Est)/2) %>%
  mutate(Dummy = rep(c("Male", "Female"), 4))

Att_vs_Time_lm$Course <- factor(Att_vs_Time_lm$Course, levels = c('11U Biology', 
                                                                  '11U Chemistry', 
                                                                  '11U Physics', 
                                                                  '12U Biology', 
                                                                  '12U Chemistry', 
                                                                  '12U Physics', 
                                                                  #'11U Math', 
                                                                  '12U Functions', 
                                                                  '12U Calculus'))
#### Plot Change in M/F Enrolment vs Year ####
DeltaSCR<- ggplot(Att_vs_Time_lm, aes(y = Course)) +
  geom_vline(xintercept = 0,
             linetype = 'dashed',
             colour = "grey30",
             size = 1) +
  geom_link(aes(x = Male.Est, xend = Female.Est, yend = Course, colour = stat(index)), #Makes gradient fill conencting
            size = 4) +
  scale_color_gradient(low = "#c9ede7",
                       high = "#fed2d2",
                       guide = "none") +
  geom_segment(aes(x = Male.Est, xend = Female.Est, y = Diff.Sig, yend = Diff.Sig), #Makes white bar to cover gradient for not stat sig differences
            colour = "white",
            size = 4) +
  geom_segment(aes(x = Male.Est, xend = Female.Est, y = Diff.Sig, yend = Diff.Sig), #add dotted black line to not stat sig diff
               colour = "black",
               size = 1,
               linetype = "blank") +
  geom_point(aes(x = Male.Est, shape = Dummy)) + #makes dummy legend labels
  geom_errorbar(aes(xmin = Male.Est - Male.SE, xmax = Male.Est + Male.SE), #Plots error bars on male
                width = 0.15, 
                size = 0.75,
                colour = "#006658")+
  geom_errorbar(aes(xmin = Female.Est - Female.SE, xmax = Female.Est + Female.SE), #Error bars for female
                width = 0.15, 
                size = 0.75,
                colour = "#7e0204")+
  geom_point(aes(x = Male.Est, fill = M.Sig), #Male points
             shape = 23,
             colour = "#006658",
             size = 3.5,
             stroke = 1.2) +
  geom_point(aes(x = Female.Est, fill = F.Sig), #female points
             shape = 21,
             colour = "#7e0204",
             size = 4.5,
             stroke = 1.2) +
  scale_fill_manual(values = c( "white", "#FD6467",  "#00b39b"), guide = 'none') +
  scale_y_discrete(limits = rev(c('11U Biology', '11U Chemistry', '11U Physics', '12U Biology', '12U Chemistry', '12U Physics', #'11U Math', 
                                  '12U Functions', '12U Calculus' ))) +
  xlab('Regression Estimates For Yearly \nChange in Student Continuation Rates') +
  guides(shape = guide_legend(override.aes = list(shape = c(21, 23), 
                                                  size = c(4.5, 3.5), 
                                                  linetype = 0, 
                                                  fill = c("#FD6467", "#00b39b"),
                                                  colour = c("#7e0204", "#006658"),
                                                  stroke = 1.2))) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        panel.border = element_blank(),
        axis.ticks.y = element_blank(),
        axis.line = element_line(colour = "black"), 
        legend.position=c(.8,.93), 
        legend.direction = "vertical", 
        legend.key.width = unit(.6,"cm"), 
        legend.title = element_blank(),
        axis.title = element_text(size = 22, face = "bold"), 
        axis.text = element_text(size = 18),
        legend.text = element_text(size = 18))
  
ggsave2(filename = 'Figure 5 - Yearly Change in SCR for Male and Female Students.pdf',
        plot = DeltaSCR,
        device = 'pdf',
        path = 'C:/Users/ecor9/Google Drive/Grad School/Research/Data Sets/School Enrollment Analysis/ON Enrolment Trends Paper Graphics',
        scale = 1.75,
        width = 175,
        height = 130,
        units = "mm",
        dpi = 500,
        limitsize = TRUE
)
