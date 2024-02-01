#Para estrtategia
inputd<- BM.strategy3

inputd$Day <- factor(inputd$Day, levels = c("Day 1","Day 2","Day 3","Day 4","Day 5","Day 7"))
level.check.fun(inputd,"ATF")
inputd$ATF<-ordered.fun(inputd,"ATF")
inputd$Symbol <- as.factor(inputd$Symbol) 


BM.development <-
  ggplot((inputd), aes(x=Day, y=error, color=ATF))+
  stat_summary(data = subset(inputd, ATF == 'HTT-NI'),  fun =mean, geom = "point", shape=21, colour="black", fill= '#0C7BDC', size=4) + 
  stat_summary(data = subset(inputd, ATF == 'HTT-NI'), aes(group=ATF),  fun =mean, geom = "line") + 
  stat_summary(data = subset(inputd, ATF == 'HTT-NI'), fun.data = mean_se, geom = "errorbar", width = .05)+
  
  stat_summary(data = subset(inputd, ATF == 'HTT+NI'),  fun =mean, geom = "point", shape=21, colour="black", fill= "#994F00", size=4) + 
  stat_summary(data = subset(inputd, ATF == 'HTT+NI'), aes(group=ATF),  fun =mean, geom = "line") + 
  stat_summary(data = subset(inputd, ATF == 'HTT+NI'), fun.data = mean_se, geom = "errorbar", width = .05)+
  
  stat_summary(data = subset(inputd, ATF == 'PSD95-6ZF-NoED'),  fun =mean, geom = "point", shape=21, colour="black", fill= '#E69F00', size=4) + 
  stat_summary(data = subset(inputd, ATF == 'PSD95-6ZF-NoED'), aes(group=ATF),  fun =mean, geom = "line") + 
  stat_summary(data = subset(inputd, ATF == 'PSD95-6ZF-NoED'), fun.data = mean_se, geom = "errorbar", width = .05)+
  
  stat_summary(data = subset(inputd, ATF == 'PSD95-6ZF-VP64'),  fun =mean, geom = "point", shape=21, colour="black", fill= '#009E73', size=4) + 
  stat_summary(data = subset(inputd, ATF == 'PSD95-6ZF-VP64'), aes(group=ATF),  fun =mean, geom = "line") + 
  stat_summary(data = subset(inputd, ATF == 'PSD95-6ZF-VP64'), fun.data = mean_se, geom = "errorbar", width = .05)+
  
  scale_color_manual(values=c("HTT-NI"='#0C7BDC',"HTT+NI"="#994F00", 'PSD95-6ZF-NoED'='#E69F00','PSD95-6ZF-VP64'='#009E73')) +
  geom_rangeframe() +
  theme_classic() +
  theme(axis.line.x = element_line(colour = 'black', size=0.5, linetype='solid'),
        axis.line.y = element_line(colour = 'black', size=0.5, linetype='solid'),
        axis.title.x = element_blank(),
        axis.title.y  = element_text(size = 16),
        axis.text = element_text(size = 12),
        legend.title = element_blank(),
        legend.text = element_blank()
  ) + ylim(0,70)+
  guides(colour = guide_legend(override.aes = list(size=4)))

BM.development

#----
summary_df <- inputd %>%
  group_by(ATF, Day, SpaceSTR) %>%
  summarise(Count = n()) %>%
  group_by(ATF, Day) %>%
  mutate(Percentage = Count / sum(Count) * 100)

# Imprimir el DataFrame resumido
print(summary_df)

# Crear el gráfico de barras
ggplot(summary_df, aes(x = as.factor(Day), y = Percentage, fill = SpaceSTR)) +
  geom_bar(stat = "identity", position = "stack") +
  facet_wrap(~ATF, scales = "free_y") +
  labs(title = "Distribución de Estrategias por Día y Tratamiento",
       x = "Día", y = "Porcentaje") +
  theme_minimal()

BM.strategy3 <- BM.strategy3 %>% mutate(SpaceSTR = if_else(Strategy %in% c("Mixed", "Serial"), "No espacial", Strategy))
#----
inputd<- left_join(filter(BM.Data, ColorCode!="error", ATF=="HTT-NI"), 
                   filter(Main, ExpCode_6=="BHBM"), 
                   by=c("Short_ID","ATF","ColorCode"))

inputd <- filter(inputd, Day %in% c("Day 1", "Day 5"))
inputd$Day <- factor(inputd$Day, levels = c("Day 1","Day 2","Day 3","Day 4","Day 5","Day 7"))
level.check.fun(inputd,"ATF")
inputd$ATF<-ordered.fun(inputd,"ATF")

BM.WT.15.sts<- two_sample_test(na.omit(inputd), Day, error, paired = TRUE)
BM.WT.15.sts<- BM.WT.15.sts %>% dplyr::mutate(asterisk_label = starmaker(BM.WT.15.sts$p.value, p.levels=c(.001, .01, .05, .1), symbols=c("***", "**", "*", "")))
BM.WT.15.sts

BM.WT.15.des.sts <- cbind(sts.BM.calc.OLT("Day 1", "Day 5"), BM.WT.15.sts[6], BM.WT.15.sts[3], BM.WT.15.sts[5], BM.WT.15.sts[17], 
                          BM.WT.15.sts[8:12])
BM.WT.15.des.sts

BM.WT.15.g <- grapher.BM(c("Day 1", "Day 5"), BM.WT.15.sts$asterisk_label) +
  theme_classic() +
  theme(axis.line.x = element_line(colour = 'black', size=0.5, linetype='solid'),
        axis.line.y = element_line(colour = 'black', size=0.5, linetype='solid'),
        axis.title.x = element_blank())

BM.WT.15.g.anexo <- grapher.BM.anexo(c("Day 1", "Day 5"), BM.WT.15.sts$asterisk_label) +
  theme_classic() +
  theme(axis.line.x = element_line(colour = 'black', size=0.5, linetype='solid'),
        axis.line.y = element_line(colour = 'black', size=0.5, linetype='solid'),
        axis.title.x = element_blank())
BM.WT.15.g
BM.WT.15.g.anexo