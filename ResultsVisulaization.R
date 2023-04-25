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

# Plotting (Fig 4)
fig4<- ggplot(SatimpactDF, aes(x=factor(densitylambda), y=MaxCatch_Trap1)) +
  stat_summary(fun.y=mean,  geom="line", aes(group = Trap_saturation, color=Trap_saturation)) +
  stat_summary(fun.y=mean,  geom="point", aes(group = Trap_saturation, color=Trap_saturation)) +
  xlab("Lobster density") +
  theme(panel.border = element_blank()) +
  scale_y_continuous(name = "Mean cath (no lobster per trap)", limits = c(0,40))  +
  ggtitle("Fig 4; number of simulation=50, soaktime=50")


# (Fig 6)

localdepDF <- list.files( path ='C:/Users/pourfarajv/Desktop/Kumu_R_Visulization/AgentbasedModeling/lobsterCatchV2023/resultsVP/NewResults/', pattern = "*.rds", full.names = TRUE ) %>%
  map_dfr(readRDS)
selecteddstep<- filter(localdepDF, dstepmov == 1 | dstepmov== 2 | dstepmov== 5 | dstepmov== 10)

selecteddstep$dstepmov <- factor(selecteddstep$dstepmov)

 ggplot(selecteddstep, aes(x=densitylambda, y=MaxCatch_Trap1)) +
  stat_summary(fun.y=mean,  geom="line", aes(group = dstepmov, color=dstepmov, linetype=dstepmov)) +
  stat_summary(fun.y=mean,  geom="point", aes(group = dstepmov, color=dstepmov)) +
  xlab("Lobster density") +
  theme(panel.border = element_blank()) +
  scale_y_continuous(name = "Mean cath (no lobster per trap)")  +
  ggtitle("Fig 6 of Addison & Bell simulation=50, soaktime=50")

# (Fig 7)

variedsat <- list.files( path ='C:/Users/pourfarajv/Desktop/Kumu_R_Visulization/AgentbasedModeling/lobsterCatchV2023/resultsVP/NewResults/', pattern = "*.rds", full.names = TRUE ) %>%
  map_dfr(readRDS)
variedsat$saturationThreshold <- factor(variedsat$saturationThreshold)
ggplot(variedsat, aes(x=densitylambda, y=MaxCatch_Trap1)) +
  stat_summary(fun.y=mean,  geom="line", aes(group = saturationThreshold, color=saturationThreshold, linetype=saturationThreshold)) +
  stat_summary(fun.y=mean,  geom="point", aes(group = saturationThreshold, color=saturationThreshold)) +
  xlab("Lobster density") +
  theme(panel.border = element_blank()) +
  scale_y_continuous(name = "Mean cath (no lobster per trap)")  +
  ggtitle("Fig 7 of Addison & Bell paper replicated")

# fig 9
variedShrinkage <- list.files( path ='C:/Users/pourfarajv/Desktop/Kumu_R_Visulization/AgentbasedModeling/lobsterCatchV2023/resultsVP/NewResults/shrinkageImpact/', pattern = "*.rds", full.names = TRUE ) %>%
  map_dfr(readRDS)
variedShrinkage$baitShrinkage <- factor(variedShrinkage$baitShrinkage)


fig9<- ggplot(variedShrinkage, aes(x=baitShrinkage, y=MaxCatch_Trap1, group=baitShrinkage)) +
  stat_summary(fun.y=mean,  geom="line", aes(group=1)) +
  stat_summary(fun.y=mean,  geom="point", aes(group=baitShrinkage)) +
  xlab("Bait shrinkage factor") +
  theme(panel.border = element_blank()) +
  scale_y_continuous(name = "Mean cath (no lobster per trap)")  +
  ggtitle("Fig 9; density=0.5 dstep=5")

# to get summary stats
summarydf<- variedShrinkage %>%
  group_by( baitShrinkage) %>%
  summarise(mean = mean(MaxCatch_Trap1))


#### Adam's CPUE results
cpue<- readRDS ("tempCatchability.rds")

a<- ggplot(cpue, aes(x=temp, y=pred)) +
  geom_point(size=1, alpha=0.05) + xlab("Temperature") + ylab ("Catch rate") + ylim(0,1.5)

b<- ggplot(cpue, aes(x=temp, y=res)) +
  geom_point(size=1, alpha=0.05) + xlab("Temperature") + ylab ("Corrected catch rate") + ylim(0,1.5)

CPUE_plots<- ggarrange(a, b,
          labels = c("A", "B"),
          ncol = 2, nrow = 1)

ggsave(filename = "fig9.png", plot = fig9, width = 12, height = 10, dpi = 300, units = "cm")








