library(dplyr)

library(tidyverse)

MechaCar_df <- read.csv(file='MechaCar_mpg.csv',check.names=F,stringsAsFactors = F)
lm(mpg ~ vehicle_length + vehicle_weight + spoiler_angle + ground_clearance + AWD ,data=MechaCar_df)

summary(lm(mpg ~ vehicle_length + vehicle_weight + spoiler_angle + ground_clearance + AWD ,data=MechaCar_df))


Suspension_Coil_df <- read.csv(file='Suspension_Coil.csv',check.names=F,stringsAsFactors = F)
total_summary <- Suspension_Coil_df %>% summarize(Mean = mean(PSI), Median = median(PSI), Variance = var(PSI), SD = sd(PSI), .groups = 'keep')
lot_summary <- Suspension_Coil_df %>% group_by(Manufacturing_Lot) %>% summarize(Mean = mean(PSI),Median = median(PSI), Variance = var(PSI), SD = sd(PSI) , .groups = 'keep') 

t.test(Suspension_Coil_df$PSI,mu=mean(Suspension_Coil_df$PSI))
t.test(subset(Suspension_Coil_df$PSI,Suspension_Coil_df$Manufacturing_Lot == "Lot1"),mu=mean(Suspension_Coil_df$PSI))
t.test(subset(Suspension_Coil_df$PSI,Suspension_Coil_df$Manufacturing_Lot == "Lot2"),mu=mean(Suspension_Coil_df$PSI))
t.test(subset(Suspension_Coil_df$PSI,Suspension_Coil_df$Manufacturing_Lot == "Lot3"),mu=mean(Suspension_Coil_df$PSI))
