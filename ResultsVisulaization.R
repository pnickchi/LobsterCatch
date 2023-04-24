library(ggplot2)
library(ggpubr)
library(tidyverse)
#In case we need to read an RDS or bunch them


#--Reading in  RDS files and combining w/o saturation datasets

NOSat_densityimapctdf <- list.files( path =
'C:/Users/pourfarajv/Desktop/Kumu_R_Visulization/AgentbasedModeling/lobsterCatchV2023/resultsVP/NewResults/', pattern = "*.rds", full.names = TRUE ) %>%
  map_dfr(readRDS) %>%
  mutate(Trap_saturation= "No trap saturation")

Sat5_densityimapctdf <- list.files( path ='C:/Users/pourfarajv/Desktop/Kumu_R_Visulization/AgentbasedModeling/lobsterCatchV2023/resultsVP/NewResults/Varieddensity_Varieddstep_Saturation5/', pattern = "*.rds", full.names = TRUE ) %>%
  map_dfr(readRDS) %>%
  mutate(Trap_saturation= "5")

SatimpactDF<- bind_rows(Sat5_densityimapctdf,NOSat_densityimapctdf)
#selecteddensity<- filter(SatimpactDF, densitylambda == 0.1 | densitylambda== 0.5 | densitylambda== 1 | densitylambda== 1.6)

# Plotting (Fig 4)
ggplot(SatimpactDF, aes(x=factor(densitylambda), y=MaxCatch_Trap1)) +
  stat_summary(fun.y=mean,  geom="line", aes(group = Trap_saturation, color=Trap_saturation)) +
  stat_summary(fun.y=mean,  geom="point", aes(group = Trap_saturation, color=Trap_saturation)) +
  xlab("Lobster density") +
  theme(panel.border = element_blank()) +
  scale_y_continuous(name = "Mean cath (no lobster per trap)", limits = c(0,40))  +
  ggtitle("Fig 4 of Addison & Bell")


# (Fig 6)

localdepDF <- list.files( path ='C:/Users/pourfarajv/Desktop/Kumu_R_Visulization/AgentbasedModeling/lobsterCatchV2023/resultsVP/NewResults/', pattern = "*.rds", full.names = TRUE ) %>%
  map_dfr(readRDS)
localdepDF$dstepmov <- factor(localdepDF$dstepmov)
ggplot(localdepDF, aes(x=densitylambda, y=MaxCatch_Trap1)) +
  stat_summary(fun.y=mean,  geom="line", aes(group = dstepmov, color=dstepmov, linetype=dstepmov)) +
  stat_summary(fun.y=mean,  geom="point", aes(group = dstepmov, color=dstepmov)) +
  xlab("Lobster density") +
  theme(panel.border = element_blank()) +
  scale_y_continuous(name = "Mean cath (no lobster per trap)")  +
  ggtitle("Fig 6 of Addison & Bell paper replicated")

# (Fig 7)

variedsat <- list.files( path ='C:/Users/pourfarajv/Desktop/Kumu_R_Visulization/AgentbasedModeling/lobsterCatch/resultsVP/variedSaturation/', pattern = "*.rds", full.names = TRUE ) %>%
  map_dfr(readRDS)
variedsat$saturationThreshold <- factor(variedsat$saturationThreshold)
ggplot(variedsat, aes(x=densitylambda, y=maxcatchno)) +
  stat_summary(fun.y=mean,  geom="line", aes(group = saturationThreshold, color=saturationThreshold, linetype=saturationThreshold)) +
  stat_summary(fun.y=mean,  geom="point", aes(group = saturationThreshold, color=saturationThreshold)) +
  xlab("Lobster density") +
  theme(panel.border = element_blank()) +
  scale_y_continuous(name = "Mean cath (no lobster per trap)")  +
  ggtitle("Fig 7 of Addison & Bell paper replicated")


# to get summary stats
summarydf<- localdepDF %>%
  group_by( densitylambda, dstepmov) %>%
  summarise(mean = mean(maxcatchno))


#### Adam's CPUE results
cpue<- readRDS ("tempCatchability.rds")

a<- ggplot(cpue, aes(x=temp, y=pred)) +
  geom_point(size=1, alpha=0.05) + xlab("Temperature") + ylab ("Catch rate") + ylim(0,1.5)

b<- ggplot(cpue, aes(x=temp, y=res)) +
  geom_point(size=1, alpha=0.05) + xlab("Temperature") + ylab ("Corrected catch rate") + ylim(0,1.5)

CPUE_plots<- ggarrange(a, b,
          labels = c("A", "B"),
          ncol = 2, nrow = 1)

ggsave(filename = "desktop/CPUE.png", plot = CPUE_plots, width = 12, height = 10, dpi = 300, units = "cm")








