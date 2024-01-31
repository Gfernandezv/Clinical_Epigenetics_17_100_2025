install.packages("ggthemes") # Install 
library(ggthemes) # Load

# Development -------------------------------------------------------------

 
inputd<- left_join(filter(BM.Data, ColorCode!="error"), 
                   filter(Main, ExpCode_6=="BHBM"), 
                   by=c("Short_ID","ATF","ColorCode"))

  inputd$Day <- factor(inputd$Day, levels = c("Day 1","Day 2","Day 3","Day 4","Day 5","Day 7"))
  level.check.fun(inputd,"ATF")
  inputd$ATF<-ordered.fun(inputd,"ATF")
  inputd$Symbol <- as.factor(inputd$Symbol) 

BM.development <-
  ggplot(inputd, aes(x=Day, y=Latency, color=ATF))+
  geom_point(    alpha    = 0.3, 
                 size     = 3,
                 position = position_jitter(width = 0.1, seed = 1))+
  stat_summary(data = subset(inputd, ATF == 'HTT-NI'),  fun =mean, geom = "point", size=3, alpha=.7) + 
  stat_summary(data = subset(inputd, ATF == 'HTT-NI'), aes(group=ATF),  fun =mean, geom = "line") + 
  stat_summary(data = subset(inputd, ATF == 'HTT-NI'), fun.data = mean_se, geom = "errorbar", width = .05)+
  
  stat_summary(data = subset(inputd, ATF == 'HTT+NI'),  fun =mean, geom = "point", size=3, alpha=.7) + 
  stat_summary(data = subset(inputd, ATF == 'HTT+NI'), aes(group=ATF),  fun =mean, geom = "line") + 
  stat_summary(data = subset(inputd, ATF == 'HTT+NI'), fun.data = mean_se, geom = "errorbar", width = .05)+
  
  stat_summary(data = subset(inputd, ATF == 'PSD95-6ZF-NoED'),  fun =mean, geom = "point", size=3, alpha=.7) + 
  stat_summary(data = subset(inputd, ATF == 'PSD95-6ZF-NoED'), aes(group=ATF),  fun =mean, geom = "line") + 
  stat_summary(data = subset(inputd, ATF == 'PSD95-6ZF-NoED'), fun.data = mean_se, geom = "errorbar", width = .05)+
  
  stat_summary(data = subset(inputd, ATF == 'PSD95-6ZF-VP64'),  fun =mean, geom = "point", size=3, alpha=.7) + 
  stat_summary(data = subset(inputd, ATF == 'PSD95-6ZF-VP64'), aes(group=ATF),  fun =mean, geom = "line") + 
  stat_summary(data = subset(inputd, ATF == 'PSD95-6ZF-VP64'), fun.data = mean_se, geom = "errorbar", width = .05)+
  
  scale_color_manual(values=c("HTT-NI"='#0C7BDC',"HTT+NI"="#994F00", 'PSD95-6ZF-NoED'='#E69F00','PSD95-6ZF-VP64'='#009E73')) +
  geom_rangeframe() +
  theme_classic() +
  theme(axis.line.x = element_line(colour = 'black', size=0.5, linetype='solid'),
        axis.line.y = element_line(colour = 'black', size=0.5, linetype='solid'),
        axis.title.x = element_blank(),
        legend.title = element_blank(),
        legend.text = element_blank(),
        legend.position = c(0.8,0.95),
        legend.spacing.y = unit(0, 'cm')) +
  guides(colour = guide_legend(override.aes = list(size=4)))

#ggsave("graphs/BM.days.png", width = 2000, height = 1000, units = "px", dpi = 300, bg=NULL)


# WT.lat.1.5 ----------------------------------------------------------------
inputd<- left_join(filter(BM.Data, ColorCode!="error", ATF=="HTT-NI"), 
                   filter(Main, ExpCode_6=="BHBM"), 
                   by=c("Short_ID","ATF","ColorCode"))

inputd <- filter(inputd, Day %in% c("Day 1", "Day 5"))
inputd$Day <- factor(inputd$Day, levels = c("Day 1","Day 2","Day 3","Day 4","Day 5","Day 7"))
level.check.fun(inputd,"ATF")
inputd$ATF<-ordered.fun(inputd,"ATF")

BM.WT.15.sts<- two_sample_test(inputd, Day, Latency, paired = TRUE)
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

# HTT.lat.1.5 ----------------------------------------------------------------
inputd<- left_join(filter(BM.Data, ColorCode!="error", ATF=="HTT+NI"), 
                   filter(Main, ExpCode_6=="BHBM"), 
                   by=c("Short_ID","ATF","ColorCode"))

inputd <- filter(inputd, Day %in% c("Day 1", "Day 5"))
inputd$Day <- factor(inputd$Day, levels = c("Day 1","Day 2","Day 3","Day 4","Day 5","Day 7"))
level.check.fun(inputd,"ATF")
inputd$ATF<-ordered.fun(inputd,"ATF")

BM.HTT.15.sts<- two_sample_test(inputd, Day, Latency, paired = TRUE)
BM.HTT.15.sts<- BM.HTT.15.sts %>% dplyr::mutate(asterisk_label = starmaker(BM.HTT.15.sts$p.value, p.levels=c(.001, .01, .05, .1), symbols=c("***", "**", "*", "")))
BM.HTT.15.sts

BM.HTT.15.des.sts <- cbind(sts.BM.calc.OLT("Day 1", "Day 5"), BM.HTT.15.sts[6], BM.HTT.15.sts[3], BM.HTT.15.sts[5], BM.HTT.15.sts[17], 
                        BM.HTT.15.sts[8:12])
BM.HTT.15.des.sts

BM.HTT.15.g <- grapher.BM(c("Day 1", "Day 5"), BM.HTT.15.sts$asterisk_label) 
BM.HTT.15.anexo.g <- grapher.BM.anexo(c("Day 1", "Day 5"), BM.HTT.15.sts$asterisk_label)

# NoED.lat.1.5 ----------------------------------------------------------------
inputd<- left_join(filter(BM.Data, ColorCode!="error", ATF=="PSD95-6ZF-NoED"), 
                   filter(Main, ExpCode_6=="BHBM"), 
                   by=c("Short_ID","ATF","ColorCode"))

inputd <- filter(inputd, Day %in% c("Day 1", "Day 5"))
inputd$Day <- factor(inputd$Day, levels = c("Day 1","Day 2","Day 3","Day 4","Day 5","Day 7"))
level.check.fun(inputd,"ATF")
inputd$ATF<-ordered.fun(inputd,"ATF") 

BM.NoED.15.sts<- two_sample_test(inputd, Day, Latency, paired = TRUE)
BM.NoED.15.sts<-BM.NoED.15.sts %>% dplyr::mutate(asterisk_label = starmaker(BM.NoED.15.sts$p.value, p.levels=c(.001, .01, .05, .1), symbols=c("***", "**", "*", "")))
BM.NoED.15.sts

BM.NoED.15.des.sts <- cbind(sts.BM.calc.OLT("Day 1", "Day 5"), BM.NoED.15.sts[6], BM.NoED.15.sts[3], BM.NoED.15.sts[5], BM.NoED.15.sts[17], 
                         BM.NoED.15.sts[8:12])
BM.NoED.15.des.sts

BM.NoED.15.g <- grapher.BM(c("Day 1", "Day 5"), BM.NoED.15.sts$asterisk_label) 
BM.NoED.15.anexo.g <- grapher.BM.anexo(c("Day 1", "Day 5"), BM.NoED.15.sts$asterisk_label)

# VP64.lat.1.5 ----------------------------------------------------------------
inputd<- left_join(filter(BM.Data, ColorCode!="error", ATF=="PSD95-6ZF-VP64"), 
                   filter(Main, ExpCode_6=="BHBM"), 
                   by=c("Short_ID","ATF","ColorCode"))

inputd <- filter(inputd, Day %in% c("Day 1", "Day 5"))
inputd$Day <- factor(inputd$Day, levels = c("Day 1","Day 2","Day 3","Day 4","Day 5","Day 7"))
level.check.fun(inputd,"ATF")
inputd$ATF<-ordered.fun(inputd,"ATF")

BM.VP64.15.sts<- two_sample_test(inputd, Day, Latency, paired = TRUE)
BM.VP64.15.sts<-BM.VP64.15.sts %>% dplyr::mutate(asterisk_label = starmaker(BM.VP64.15.sts$p.value, p.levels=c(.001, .01, .05, .1), symbols=c("***", "**", "*", "")))
BM.VP64.15.sts

BM.VP64.15.des.sts <- cbind(sts.BM.calc.OLT("Day 1", "Day 5"), BM.VP64.15.sts[6], BM.VP64.15.sts[3], BM.VP64.15.sts[5], BM.VP64.15.sts[17], 
                         BM.VP64.15.sts[8:12])
BM.VP64.15.des.sts

BM.VP64.15.g <- grapher.BM(c("Day 1", "Day 5"), BM.VP64.15.sts$asterisk_label) 
BM.VP64.15.anexo.g <- grapher.BM.anexo(c("Day 1", "Day 5"), BM.VP64.15.sts$asterisk_label)

# Resumen sección 15 ------------------------------------------------------


##Graficos -------------------------------------------------------------


BM.15.plot <- plot_grid(
                          BM.WT.15.g,
                          BM.HTT.15.g,
                          BM.VP64.15.g,
                          BM.NoED.15.g,
                          ncol = 4, align = "h", rel_widths = c(1.5, 1,1,1))


BM.15.plot.anexo <- plot_grid(
  BM.WT.15.g.anexo,
  BM.HTT.15.anexo.g,
  BM.VP64.15.anexo.g,
  BM.NoED.15.anexo.g,
  ncol = 4, align = "h", rel_widths = c(1.5, 1,1,1))

BM.15.plot.cmp.anexo <- grph.layers(BM.15.plot.anexo)
BM.15.plot.cmp <- grph.layers(BM.15.plot)
## Tabla -------------------------------------------------------------------

BM.15.table.t1 <- list(BM.WT.15.des.sts, BM.HTT.15.des.sts,BM.VP64.15.des.sts,BM.NoED.15.des.sts) %>% bind_rows()
BM.15.table.t2<-cbind(c("Wt","Transgenic","Transgenic","Transgenic"),c("-","-","+","-"),c("-","-","-","+"),BM.15.table.t1)
row.names(BM.15.table.t2) <- NULL
BM.15.table.t2$p.value2 <- BM.15.table.t2$p.value
BM.15.table.t2$p.value <- as.character(formatC(BM.15.table.t2$p.value, digits = 2, format = 'g')) 
BM.15.table.t2$p.value = cell_spec(BM.15.table.t2$p.value, bold = ifelse(BM.15.table.t2$p.value2 <= 0.05, T, F))

BM.15.table <- kbl (BM.15.table.t2[1:20], align = "c",digits = 2, col.names = c(" ","PSD95-6ZF-\nVP64",
                                                                               "PSD95-6ZF-\nNoED", 
                                                                               "n","Mean Latency (s)","SD","SE",
                                                                               "n","Mean Latency (s)","SD","SE",
                                                                               "Method","Statistic","p.value"," ","Effect size",
                                                                               "Estimate","conf.level","conf.low","conf.high"),
                       booktabs =TRUE, escape = F) %>% 
  collapse_rows(columns = 1, valign = "middle") %>% 
  kable_classic("striped", full_width = T, font_size = 20) %>%
  add_header_above(c(" "=3, "Day 1"=4,"Day 5"=4, " "=9))%>%  
  as_image(file = "graphs/BM.15.table.png", width = 3 )

# WT.lat.5.7 ----------------------------------------------------------------
inputd<- left_join(filter(BM.Data, ColorCode!="error", ATF=="HTT-NI"), 
                   filter(Main, ExpCode_6=="BHBM"), 
                   by=c("Short_ID","ATF","ColorCode"))

inputd <- filter(inputd, Day %in% c("Day 5", "Day 7"))
inputd$Day <- factor(inputd$Day, levels = c("Day 1","Day 2","Day 3","Day 4","Day 5","Day 7"))
level.check.fun(inputd,"ATF")
inputd$ATF<-ordered.fun(inputd,"ATF")

BM.WT.57.sts<- two_sample_test(inputd, Day, Latency, paired = TRUE)
BM.WT.57.sts<- BM.WT.57.sts %>% dplyr::mutate(asterisk_label = starmaker(BM.WT.57.sts$p.value, p.levels=c(.001, .01, .05, .1), symbols=c("***", "**", "*", "")))
BM.WT.57.sts

BM.WT.57.des.sts <- cbind(sts.BM.calc.OLT("Day 5", "Day 7"), BM.WT.57.sts[6], BM.WT.57.sts[3], BM.WT.57.sts[5], BM.WT.57.sts[17], 
                          BM.WT.57.sts[8:12])
BM.WT.57.des.sts

BM.WT.57.g <- grapher.BM(c("Day 5", "Day 7"), BM.WT.57.sts$asterisk_label) +
  theme_classic() +
  theme(axis.line.x = element_line(colour = 'black', size=0.5, linetype='solid'),
        axis.line.y = element_line(colour = 'black', size=0.5, linetype='solid'),
        axis.title.x = element_blank())

BM.WT.57.g.anexo <- grapher.BM.anexo(c("Day 5", "Day 7"), BM.WT.57.sts$asterisk_label) +
  theme_classic() +
  theme(axis.line.x = element_line(colour = 'black', size=0.5, linetype='solid'),
        axis.line.y = element_line(colour = 'black', size=0.5, linetype='solid'),
        axis.title.x = element_blank())
BM.WT.57.g
BM.WT.57.g.anexo

# HTT.lat.5.7 ----------------------------------------------------------------
inputd<- left_join(filter(BM.Data, ColorCode!="error", ATF=="HTT+NI"), 
                   filter(Main, ExpCode_6=="BHBM"), 
                   by=c("Short_ID","ATF","ColorCode"))

inputd <- filter(inputd, Day %in% c("Day 5", "Day 7"))
inputd$Day <- factor(inputd$Day, levels = c("Day 1","Day 2","Day 3","Day 4","Day 5","Day 7"))
level.check.fun(inputd,"ATF")
inputd$ATF<-ordered.fun(inputd,"ATF")

BM.HTT.57.sts<- two_sample_test(inputd, Day, Latency, paired = TRUE)
BM.HTT.57.sts<- BM.HTT.57.sts %>% dplyr::mutate(asterisk_label = starmaker(BM.HTT.57.sts$p.value, p.levels=c(.001, .01, .05, .1), symbols=c("***", "**", "*", "")))
BM.HTT.57.sts

BM.HTT.57.des.sts <- cbind(sts.BM.calc.OLT("Day 5", "Day 7"), BM.HTT.57.sts[6], BM.HTT.57.sts[3], BM.HTT.57.sts[5], BM.HTT.57.sts[17], 
                           BM.HTT.57.sts[8:12])
BM.HTT.57.des.sts

BM.HTT.57.g <- grapher.BM(c("Day 5", "Day 7"), BM.HTT.57.sts$asterisk_label) 
BM.HTT.57.anexo.g <- grapher.BM.anexo(c("Day 5", "Day 7"), BM.HTT.57.sts$asterisk_label)

# NoED.lat.1.7 ----------------------------------------------------------------
inputd<- left_join(filter(BM.Data, ColorCode!="error", ATF=="PSD95-6ZF-NoED"), 
                   filter(Main, ExpCode_6=="BHBM"), 
                   by=c("Short_ID","ATF","ColorCode"))

inputd <- filter(inputd, Day %in% c("Day 5", "Day 7"))
inputd$Day <- factor(inputd$Day, levels = c("Day 1","Day 2","Day 3","Day 4","Day 5","Day 7"))
level.check.fun(inputd,"ATF")
inputd$ATF<-ordered.fun(inputd,"ATF") 

BM.NoED.57.sts<- two_sample_test(inputd, Day, Latency, paired = TRUE)
BM.NoED.57.sts<-BM.NoED.57.sts %>% dplyr::mutate(asterisk_label = starmaker(BM.NoED.57.sts$p.value, p.levels=c(.001, .01, .05, .1), symbols=c("***", "**", "*", "")))
BM.NoED.57.sts

BM.NoED.57.des.sts <- cbind(sts.BM.calc.OLT("Day 5", "Day 7"), BM.NoED.57.sts[6], BM.NoED.57.sts[3], BM.NoED.57.sts[5], BM.NoED.57.sts[17], 
                            BM.NoED.57.sts[8:12])
BM.NoED.57.des.sts

BM.NoED.57.g <- grapher.BM(c("Day 5", "Day 7"), BM.NoED.57.sts$asterisk_label) 
BM.NoED.57.anexo.g <- grapher.BM.anexo(c("Day 5", "Day 7"), BM.NoED.57.sts$asterisk_label)

# VP64.lat.5.7 ----------------------------------------------------------------
inputd<- left_join(filter(BM.Data, ColorCode!="error", ATF=="PSD95-6ZF-VP64"), 
                   filter(Main, ExpCode_6=="BHBM"), 
                   by=c("Short_ID","ATF","ColorCode"))

inputd <- filter(inputd, Day %in% c("Day 5", "Day 7"))
inputd$Day <- factor(inputd$Day, levels = c("Day 1","Day 2","Day 3","Day 4","Day 5","Day 7"))
level.check.fun(inputd,"ATF")
inputd$ATF<-ordered.fun(inputd,"ATF")

BM.VP64.57.sts<- two_sample_test(inputd, Day, Latency, paired = TRUE)
BM.VP64.57.sts<-BM.VP64.57.sts %>% dplyr::mutate(asterisk_label = starmaker(BM.VP64.57.sts$p.value, p.levels=c(.001, .01, .05, .1), symbols=c("***", "**", "*", "")))
BM.VP64.57.sts

BM.VP64.57.des.sts <- cbind(sts.BM.calc.OLT("Day 5", "Day 7"), BM.VP64.57.sts[6], BM.VP64.57.sts[3], BM.VP64.57.sts[5], BM.VP64.57.sts[17], 
                            BM.VP64.57.sts[8:12])
BM.VP64.57.des.sts

BM.VP64.57.g <- grapher.BM(c("Day 5", "Day 7"), BM.VP64.57.sts$asterisk_label) 
BM.VP64.57.anexo.g <- grapher.BM.anexo(c("Day 5", "Day 7"), BM.VP64.57.sts$asterisk_label)

# Resumen sección 57 ------------------------------------------------------


##Graficos -------------------------------------------------------------
BM.57.plot <- plot_grid(
                          BM.WT.57.g + scale_y_continuous(expand = c(0, 0), limits = c(0, 200)),
                          BM.HTT.57.g+ scale_y_continuous(expand = c(0, 0), limits = c(0, 200)),
                          BM.VP64.57.g+ scale_y_continuous(expand = c(0, 0), limits = c(0, 200)),
                          BM.NoED.57.g+ scale_y_continuous(expand = c(0, 0), limits = c(0, 200)),
                          ncol = 4, align = "h", rel_widths = c(1.5, 1,1,1))

BM.57.plot.anexo <- plot_grid(
  BM.WT.57.g.anexo + scale_y_continuous(expand = c(0, 0), limits = c(0, 200)),
  BM.HTT.57.anexo.g+ scale_y_continuous(expand = c(0, 0), limits = c(0, 200)),
  BM.NoED.57.anexo.g+ scale_y_continuous(expand = c(0, 0), limits = c(0, 200)),
  BM.VP64.57.anexo.g+ scale_y_continuous(expand = c(0, 0), limits = c(0, 200)),
                          ncol = 4, align = "h", rel_widths = c(1.5, 1,1,1))

BM.57.plot.cmp <- grph.layers(BM.57.plot)
BM.57.plot.cmp.anexo <- grph.layers(BM.57.plot.anexo)

## Tabla -------------------------------------------------------------------

BM.57.table.t1 <- list(BM.WT.57.des.sts, BM.HTT.57.des.sts,BM.VP64.57.des.sts,BM.NoED.57.des.sts) %>% bind_rows()
BM.57.table.t2<-cbind(c("Wt","Transgenic","Transgenic","Transgenic"),c("-","-","+","-"),c("-","-","-","+"),BM.57.table.t1)
row.names(BM.57.table.t2) <- NULL
BM.57.table.t2$p.value2 <- BM.57.table.t2$p.value
BM.57.table.t2$p.value <- as.character(formatC(BM.57.table.t2$p.value, digits = 2, format = 'g')) 
BM.57.table.t2$p.value = cell_spec(BM.57.table.t2$p.value, bold = ifelse(BM.57.table.t2$p.value2 <= 0.05, T, F))

BM.57.table <- kbl (BM.57.table.t2[1:20], align = "c",digits = 2, col.names = c(" ","PSD95-6ZF-\nVP64",
                                                                                "PSD95-6ZF-\nNoED", 
                                                                                "n","Mean Latency (s)","SD","SE",
                                                                                "n","Mean Latency (s)","SD","SE",
                                                                                "Method","Statistic","p.value"," ","Effect size",
                                                                                "Estimate","conf.level","conf.low","conf.high"),
                    booktabs =TRUE, escape = F) %>% 
  collapse_rows(columns = 1, valign = "middle") %>% 
  kable_classic("striped", full_width = T, font_size = 20) %>%
  add_header_above(c(" "=3, "Day 5"=4,"Day 7"=4, " "=9))%>%  
as_image(file = "graphs/BM.57.table.png", width = 3 )

# Velocidad Wt ----------------------------------------------------------------
inputd<- left_join(filter(BM.Data, ColorCode!="error"), 
                   filter(Main, ExpCode_6=="BHBM"), 
                   by=c("Short_ID","ATF","ColorCode"))


inputd$Day <- factor(inputd$Day, levels = c("Day 1","Day 2","Day 3","Day 4","Day 5","Day 7"))
level.check.fun(inputd,"ATF")
inputd$ATF<-ordered.fun(inputd,"ATF")
inputd$Symbol <- as.factor(inputd$Symbol) 
inputd <- filter(inputd, Day %in% c("Day 1", "Day 5", "Day 7"))

BM.Speed.graph<-
  ggplot(inputd, aes(x=Day, y=Velocity, color=ATF))+
  geom_point(    alpha    = 0.3, 
                 size     = 3,
                 position = position_jitter(width = 0.1, seed = 1))+
  stat_summary(data = subset(inputd, ATF == 'HTT-NI'),  fun =mean, geom = "point", size=2, alpha=.8) + 
  stat_summary(data = subset(inputd, ATF == 'HTT-NI'), aes(group=ATF),  fun =mean, geom = "line") + 
  stat_summary(data = subset(inputd, ATF == 'HTT-NI'), fun.data = mean_se, geom = "errorbar", width = .05)+
  
  stat_summary(data = subset(inputd, ATF == 'HTT+NI'),  fun =mean, geom = "point", size=2, alpha=.8) + 
  stat_summary(data = subset(inputd, ATF == 'HTT+NI'), aes(group=ATF),  fun =mean, geom = "line") + 
  stat_summary(data = subset(inputd, ATF == 'HTT+NI'), fun.data = mean_se, geom = "errorbar", width = .05)+
  
  stat_summary(data = subset(inputd, ATF == 'PSD95-6ZF-NoED'),  fun =mean, geom = "point", size=2, alpha=.8) + 
  stat_summary(data = subset(inputd, ATF == 'PSD95-6ZF-NoED'), aes(group=ATF),  fun =mean, geom = "line") + 
  stat_summary(data = subset(inputd, ATF == 'PSD95-6ZF-NoED'), fun.data = mean_se, geom = "errorbar", width = .05)+
  
  stat_summary(data = subset(inputd, ATF == 'PSD95-6ZF-VP64'),  fun =mean, geom = "point", size=2, alpha=.8) + 
  stat_summary(data = subset(inputd, ATF == 'PSD95-6ZF-VP64'), aes(group=ATF),  fun =mean, geom = "line") + 
  stat_summary(data = subset(inputd, ATF == 'PSD95-6ZF-VP64'), fun.data = mean_se, geom = "errorbar", width = .05)+
  
  scale_color_manual(values=c("HTT-NI"='#0C7BDC',"HTT+NI"="#994F00", 'PSD95-6ZF-NoED'='#E69F00','PSD95-6ZF-VP64'='#009E73')) +
  theme_classic() +  ylab("Mean speed (cm/s)") +
  theme(axis.line.x = element_line(colour = 'black', size=0.5, linetype='solid'),
        axis.line.y = element_line(colour = 'black', size=0.5, linetype='solid'),
        legend.position = 'none', axis.title.x = element_blank())

#ggsave("graphs/BM.days.png", width = 2000, height = 1000, units = "px", dpi = 300, bg=NULL)

# distance ----------------------------------------------------------------

inputd<- left_join(filter(BM.Data, Distance!="NA"),
                   filter(Main, ExpCode_6=="BHBM"), 
                   by=c("Short_ID","ATF","ColorCode"))

level.check.fun(inputd,"ATF")
inputd$ATF<-ordered.fun(inputd,"ATF")

## Estadisticas descriptivas
group_by(inputd, ATF) %>%
  summarise(
    count = n(),
    mean  = mean(d_ddCq, na.rm = TRUE), #dif
    sd    = sd(d_ddCq, na.rm = TRUE) #dif
  )
## normalidad
ggqqplot(inputd$d_ddCq)
shapiro.test(inputd$d_ddCq)
## lavene test
car::leveneTest(d_ddCq ~ (as.factor(ATF)), data = inputd) #<- no parametricos
bartlett.test(d_ddCq ~ (as.factor(ATF)), data = inputd) # en ambos, si el p>0.05 entonces las varianzas son homogeneas

BM.dis.sts <-
  pairwise_comparisons(inputd, ATF, Distance) 
BM.dis.sts <- BM.dis.sts %>%
  dplyr::mutate(groups = purrr::pmap(.l = list(group1, group2), .f = c)) %>%
  dplyr::arrange(group1) %>%
  dplyr::mutate(asterisk_label = starmaker(BM.dis.sts$p.value, p.levels=c(.001, .01, .05, .1), symbols=c("***", "**", "*", "")))
BM.dis.sts
BM.dis.sts<-filter(BM.dis.sts, p.value<=.05)


BM.dis.g <-
  ggplot(inputd,aes(ATF,Distance))+
  geom_boxplot(width=0.3, fill=NA,linewidth = 0.1, size=0, varwidth = FALSE, outlier.alpha = 0)+
  geom_point(inherit.aes = TRUE,
             colour   = inputd$ColorCode,
             fill     = inputd$ColorCode,
             shape    = inputd$Symbol,
             alpha    = 0.5, 
             size     = 3, 
             position = position_jitter(width = 0.1, seed = 1)) +
  geom_point(
    colour   = inputd$ColorCode,
    shape    = inputd$Symbol,
    fill     = inputd$ColorCode,
    position = position_jitter(width = 0.1, seed = 1)) +
 
  xlab("")+ ylab("Total distance (cm)") +
  theme_classic () + plot.theme.box + 
  theme (axis.text.x= element_blank(),
         axis.line = element_line(colour = "black",size = .5, linetype = "solid"),
         axis.text.y = element_text(size = 10),
         axis.title.y = element_text(size = 10),
         panel.background = element_blank())
  

BM.dis.g
BM.dis.f <-
  ggarrange(g.a,
            BM.dis.g,
            g.d+ theme(axis.text.y = element_text(size = 8)),
            ncol = 1, nrow = 4,  
            align = "v",
            heights = c(.3,1.7,0.5))

# integrador --------------------------------------------------------------


ggdraw(xlim = c(0, 1), ylim = c(0, 1)) +
  draw_plot(BM.development,   0,  .7,  1,  .27) + 
  draw_plot(BM.dis.f,       0,  0.3,  .5,  .38)+
  draw_plot(BM.Speed.graph,  .65,  .37,  .3,  .3) +
  draw_plot(BM.15.plot.cmp,   0,   0,  .5,  .3)+
  draw_plot(BM.57.plot.cmp,  .5,   0,  .5,  .3)+
draw_label("Wt", x = 0.76, y = 0.99, hjust = 0, vjust = 0, size = 8, fontface = "plain")+
draw_label("Transgenic", x = 0.71, y = .93, hjust = 0, vjust = 0, size = 8, fontface = "plain")+
draw_label("non injected", x = 0.82, y = .96, hjust = 0, vjust = 0, size = 8, fontface = "plain")+
draw_label("+ AAV-PSD95-6ZF-VP64", x = 0.82, y = .93, hjust = 0, vjust = 0, size = 8, fontface = "plain")+
draw_label("+ AAV-PSD95-6ZF-NoED", x = 0.82, y = .9, hjust = 0, vjust = 0, size = 8, fontface = "plain")+
draw_line(x = c(0.783, 0.783), y = c(0.973, 0.891),color = "black", size = .5)+
  
draw_label("Wt", x = .10275, y = .31, hjust = 0, vjust = 0, size = 8, fontface = "plain")+
draw_line(x = c(0.092, 0.132), y = c(0.30, 0.30),color = "black", size = .5)+
    
draw_label("Transgenic", x = .3, y = .31, hjust = 0, vjust = 0, size = 8, fontface = "plain")+    
draw_line(x = c(0.2, 0.473), y = c(0.30, 0.30),color = "black", size = .5)+
    
draw_label("Wt", x = .605, y = .31, hjust = 0, vjust = 0, size = 8, fontface = "plain")+    
draw_line(x = c(0.59, 0.635), y = c(0.30, 0.30),color = "black", size = .5)+
    
draw_label("Transgenic", x = .8, y = .31, hjust = 0, vjust = 0, size = 8, fontface = "plain")+  
draw_line(x = c(0.7, .97), y = c(0.30, 0.30),color = "black", size = .5) +
  draw_plot_label(label = c("A", "B", "C","D","E"), size = 15,
                  x = c(0, 0, 0.65, 0, 0.5), y = c(1, 0.7, 0.7, 0.34, 0.34))

ggsave("graphs/BM.Graph.png", width = 20, height = 20, units = "cm", dpi = 300, bg=NULL)

# anexo -------------------------------------------------------------------

  
ggdraw(xlim = c(0, 1), ylim = c(0, 1)) +
  draw_plot(BM.cmp.157.anexo,       0,  .5,  1,  .5)+
  draw_plot(BM.15.plot.cmp.anexo,   0,   0,  .5,  .45)+
  draw_plot(BM.57.plot.cmp.anexo,  .5,   0,  .5,  .45)+
  
  draw_label("Wt", x = .10275, y = .46, hjust = 0, vjust = 0, size = 8, fontface = "plain")+
  draw_line(x = c(0.092, 0.132), y = c(0.45, 0.45),color = "black", size = .5)+
  
  draw_label("Transgenic", x = .3, y = .46, hjust = 0, vjust = 0, size = 8, fontface = "plain")+    
  draw_line(x = c(0.2, 0.473), y = c(0.45, 0.45),color = "black", size = .5)+
  
  draw_label("Wt", x = .605, y = .46, hjust = 0, vjust = 0, size = 8, fontface = "plain")+    
  draw_line(x = c(0.59, 0.635), y = c(0.45, 0.45),color = "black", size = .5)+
  
  draw_label("Transgenic", x = .8, y = .46, hjust = 0, vjust = 0, size = 8, fontface = "plain")+  
  draw_line(x = c(0.7, .97), y = c(0.45, 0.45),color = "black", size = .5) +
  draw_plot_label(label = c("A", "B", "C"), size = 15,
                  x = c(0, 0, 0.5), y = c(1, 0.5, 0.5))

ggsave("graphs/BM.Graph.anexo.png", width = 16, height = 22, units = "cm", dpi = 300, bg=NULL)
# funs --------------------------------------------------------------------

grapher.BM <- function (level.i, label.i) {
  print(level.i)
  print(label.i)
g <-  ggplot(inputd,aes(x=Day, y=Latency))+
  geom_boxplot(width=0.3, fill=NA,linewidth = 0.1, size=0, varwidth = FALSE, outlier.alpha = 0)+
  stat_summary(aes(group=Day), data = inputd,  fun =median, color = "black", geom = "line", size=0.5, alpha=0.2) +
  geom_point(
    colour   = inputd$ColorCode,
    shape    = inputd$Symbol,
    fill     = inputd$ColorCode,
    alpha    = 0.5, 
    size     = 2, 
    position = position_jitter(width = 0.1, seed = 1)) +
  geom_line(aes(group = Short_ID),
    colour   = "grey",
    alpha    = 0.5, 
    position = position_jitter(width = 0.1, seed = 1),
    linetype = "dashed")+
  theme_classic () + labs(y="Latency (s)") + theme (axis.line.y = element_blank(),
                                                            axis.title.y = element_blank(),
                                                            axis.text.y = element_blank(),
                                                            axis.ticks.y = element_blank(),
                                                            axis.title.x = element_blank(),
                                                            plot.margin = margin(0,0,0,0,"cm"))+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 350))
  
  if((label.i)!="") {
    g <- g + geom_signif(
      comparisons = list(level.i),
      map_signif_level = TRUE,
      annotations = label.i,
      textsize    = 3,
      step_increase = 0.17)

    return(g)
  } else {

    return (g)
  }
  
}

grapher.BM.anexo <- function (level.i, label.i) {
  
  g <-  ggplot(inputd,aes(x=Day, y=Latency))+
  geom_boxplot(width=0.3, fill=NA,linewidth = 0.1, size=0, varwidth = FALSE, outlier.alpha = 0)+
  stat_summary(aes(group=Day), data = inputd,  fun =median, color = "black", geom = "line", size=0.5, alpha=0.2) +
  geom_point(
    colour   = inputd$ColorCode,
    shape    = inputd$Symbol,
    fill     = inputd$ColorCode,
    alpha    = 0.5, 
    size     = 2, 
    position = position_jitter(width = 0.1, seed = 1)) +
  geom_line(aes(group = Short_ID),
    colour   = "grey",
    alpha    = 0.5, 
    position = position_jitter(width = 0.1, seed = 1),
    linetype = "dashed")+
  geom_text_repel(
    label=inputd$Short_ID,
    colour=inputd$ColorCode,
    point.padding     = 0.5,
    max.overlaps      = Inf,
    force             = 1,
    direction         = "both",
    size              = 2,
    segment.size      = 0.3,
    segment.curvature = -0.1,
    position = position_jitter(width = 0.1, seed = 1))+
  theme_classic () + labs(y="Latency (s)") + theme (axis.line.y = element_blank(),
                                                      axis.title.y = element_blank(),
                                                      axis.text.y = element_blank(),
                                                      axis.ticks.y = element_blank(),
                                                      axis.title.x = element_blank(),
                                                      plot.margin = margin(0,0,0,0,"cm"))+
    scale_y_continuous(limits = c(0, 350))
  
  if((label.i)!="") {
    g <- g + geom_signif(
      comparisons = list(level.i),
      map_signif_level = TRUE,
      annotations = label.i,
      textsize    = 3,
      step_increase = 0.17)
    
    return(g)
  } else {
    
    return (g)
  }
  
}

grapher.BM.speed <- function () {
  
  g <-  ggplot(inputd,aes(x=ATF, y=Velocity))+
    geom_boxplot(width=0.3, fill=NA,linewidth = 0.1, size=0, varwidth = FALSE, outlier.alpha = 0)+
    stat_summary(aes(group=ATF), data = inputd,  fun =median, color = "black", geom = "line", size=0.5, alpha=0.2) +
    geom_point(
      colour   = inputd$ColorCode,
      shape    = inputd$Symbol,
      fill     = inputd$ColorCode,
      alpha    = 0.5, 
      size     = 3, 
      position = position_jitter(width = 0.1, seed = 1)) +
    geom_point(
      colour   = inputd$ColorCode,
      shape    = inputd$Symbol,
      fill     = inputd$ColorCode,
      alpha    = 0.8, 
      position = position_jitter(width = 0.1, seed = 1)) +
    geom_line(aes(group = Short_ID),
              colour   = "grey",
              alpha    = 0.5, 
              position = position_jitter(width = 0.1, seed = 1),
              linetype = "dashed")+
    theme_classic () + labs(y="Speed (cm/s)") + theme (axis.line.y = element_blank(),
                                                      axis.title.y = element_blank(),
                                                      axis.text.y = element_blank(),
                                                      axis.ticks.y = element_blank(),
                                                      axis.title.x = element_blank(),
                                                      plot.margin = margin(0,0,0,0,"cm"))+
    scale_y_continuous(expand = c(0, 0), limits = c(0, 10))
  
  if((label.i)!="") {
    g <- g + geom_signif(
      comparisons = graph.sts$groups,
      map_signif_level = TRUE,
      annotations = graph.sts$asterisk_label,
      textsize    = 5,
      step_increase = 0.17)
    return(g)
  } else {
    return (g)
  }
  
}

