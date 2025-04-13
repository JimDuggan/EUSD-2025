library(tidyr)
library(dplyr)
library(purrr)
library(ggplot2)

sim_l <- readRDS("data/SimSave.RDS")

sim_w <- sim_l %>%
  pivot_wider(names_from = Variable,
              values_from = Value) %>%
  mutate(`Activate B1 Overtime`=as.logical(`Activate B1 Overtime`),
         `Activate R1 Burnout`=as.logical(`Activate R1 Burnout`),
         `Activate B1.1 Duty of Care`=as.logical(`Activate B1.1 Duty of Care`),
         `Activate B2 Digital Health`=as.logical(`Activate B2 Digital Health`),
         `Activate R2 Telemedicine Rework`=as.logical(`Activate R2 Telemedicine Rework`),
         `Activate B3 Surge Resources`=as.logical(`Activate B3 Surge Resources`),
         `Activate R3 Sub-Optimal Consultations`=as.logical(`Activate R3 Sub-Optimal Consultations`),
         `SEIR.Activate B1.2 Aware Citizens`=as.logical(`SEIR.Activate B1.2 Aware Citizens`),
         `SEIR.Activate B1.1 Vaccination`=as.logical(`SEIR.Activate B1.1 Vaccination`))


# (1) Trace plots
ggplot(sim_w,aes(x=Days,y=`Regular Care Backlog`,group=RunID,colour=RunID))+
  geom_line()+
  theme(legend.position = "none")+
  scale_color_gradientn(colors=rainbow(14))+
  labs(title="Sensitivity Runs for Regular Care Backlog")


# (2) Quantiles
q_data <- sim_l %>%
            filter(Variable=="Regular Care Backlog") %>%
            group_by(Days) %>%
            summarise(Mean=mean(Value),
                      Q95=quantile(Value,0.95),
                      Q5=quantile(Value,0.05))

ggplot(q_data,aes(x=Days,y=Mean))+
  geom_line(colour="red")+
  geom_ribbon(aes(x=Days,ymin=Q5,ymax=Q95),alpha=0.4,fill="steelblue2")



# (3) Final value explorations
f_sum <- sim_w %>%
  filter(Days==max(Days))

f_sum_sorted <-f_sum %>% arrange(RMSE) 

three_rows <- f_sum_sorted[c(1,5000,10000),]

write_csv(three_rows,"data/ThreeRows.csv")

f_sum %>% arrange(RMSE) %>% slice(1) %>% glimpse()
f_sum %>% arrange(RMSE) %>% slice(5000) %>% glimpse()
f_sum %>% arrange(desc(RMSE)) %>% slice(1) %>% glimpse()



# (3) Final value per run exploring loops that are activated
f_data1 <- sim_w %>%
           filter(Days==max(Days)) %>%
           select(RunID,`Regular Care Backlog`,-RMSE,contains("Activate")) %>%
           pivot_longer(`Activate B1 Overtime`:`SEIR.Activate B1.1 Vaccination`,
                        names_to = "Loop",
                        values_to = "IsActive") %>%
           mutate(Info=paste0(Loop,"-",IsActive))

f_data2 <- sim_w %>%
  filter(Days==max(Days)) %>%
  select(RunID,-`Regular Care Backlog`,RMSE,contains("Activate")) %>%
  pivot_longer(`Activate B1 Overtime`:`SEIR.Activate B1.1 Vaccination`,
               names_to = "Loop",
               values_to = "IsActive") %>%
  mutate(Info=paste0(Loop,"-",IsActive))


ggplot(f_data1,aes(x=Info,y=`Regular Care Backlog`))+
  geom_boxplot()+
  theme(axis.text.x=element_text(angle=90))

ggplot(filter(f_data1,`Regular Care Backlog` < 250),
              aes(x=Info,y=`Regular Care Backlog`))+
  geom_boxplot()+
  theme(axis.text.x=element_text(angle=90))

ggplot(f_data2,aes(x=Info,y=RMSE))+
  geom_boxplot()+
  theme(axis.text.x=element_text(angle=90))

ggplot(f_data2,aes(x=RMSE,colour=Info))+
  geom_freqpoly()

# (4) Final value per run exploring parameters

p_data1 <- sim_w %>%
  filter(Days==max(Days)) %>%
  select(RunID,RMSE,!contains("Activate")) %>%
  select(RMSE,`SEIR.PAR ALPHA`:`AF Preparedness`)

r_mod <- lm(RMSE~.,data=p_data1)
summary(r_mod)


           


