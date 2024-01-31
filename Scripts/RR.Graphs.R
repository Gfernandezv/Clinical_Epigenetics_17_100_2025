# RR.6wks.graph -------------------------------------------------------
inputd<- left_join(filter(RR.Data, Week == "s6"), 
                   filter(Main, ExpCode_3=="BHRR6"), 
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


g <-
  ggplot(inputd,aes(x= Day, y=time))+
  facet_wrap(~ATF, ncol=2)+
  geom_boxplot(width=0.3, fill=NA,linewidth = 0.1, size=0, varwidth = FALSE, outlier.alpha = 0)+
  stat_summary(aes(group=ATF), data = inputd,  fun =median, color = "black", geom = "line", size=0.5, alpha=0.2)+
  geom_point(inherit.aes = TRUE,
             colour   = inputd$ColorCode,
             fill     = inputd$ColorCode,
             shape    = inputd$Symbol,
             alpha    = 0.5, 
             size     = 3, 
             position = position_jitter(width = 0.1, seed = 1)) +
  geom_line(aes(group = Short_ID),
            colour   = "grey",
            alpha    = 0.5, 
            position = position_jitter(width = 0.1, seed = 1),
            linetype = "dashed"
  )+
  geom_point(
    colour   = inputd$ColorCode,
    shape    = inputd$Symbol,
    fill     = inputd$ColorCode,
    position = position_jitter(width = 0.1, seed = 1)) +
  geom_text_repel(aes(label=inputd$Short_ID),
                  colour=inputd$ColorCode,
                  point.padding     = 0.5,
                  max.overlaps      = Inf,
                  force             = 1,
                  direction         = "both",
                  size              = 2,
                  segment.size      = 0.3,
                  segment.curvature = -0.1,
                  position = position_jitter(width = 0.1, seed = 1))+
  theme_classic () + plot.theme.box + labs(y="Latency to fall (s)") + scale_y_continuous(expand = c(0, 0), limits = c(0, 300))
  
g
ggsave("graphs/RRs6.graph.png", width = 2000, height = 2000, units = "px", dpi = 300, bg=NULL)

# RR.6Wks.Table.stats -------------------------------------------------------------

RR.data.list <- list((inputd %>%  group_by(ATF) %>%  
                        summarise(count.1=sum(Day == "Day 1"), 
                                  Mean.1=mean(time[Day == "Day 1"]), 
                                  SD.1=sd(time[Day == "Day 1"]),
                                  SE.1= (sd(time[Day == "Day 1"]))/(sqrt(sum(Day == "Day 1"))))
                      ), 
                      (inputd %>%  group_by(ATF) %>%  
                         summarise(count.1=sum(Day == "Day 2"), 
                                   Mean.1=mean(time[Day == "Day 2"]), 
                                   SD.1=sd(time[Day == "Day 2"]),
                                   SE.1= (sd(time[Day == "Day 2"]))/(sqrt(sum(Day == "Day 2"))))
                      ),
                      (inputd %>%  group_by(ATF) %>%  
                         summarise(count.1=sum(Day == "Day 3"), 
                                   Mean.1=mean(time[Day == "Day 3"]), 
                                   SD.1=sd(time[Day == "Day 3"]),
                                   SE.1= (sd(time[Day == "Day 3"]))/(sqrt(sum(Day == "Day 3"))))
                      )
) 

kbl((RR.data.list %>% reduce(inner_join, by='ATF')), align = "c", digits = 2, 
    col.names = c("ATF" ,"n","Mean (s)","SD","SE","n","Mean (s)","SD","SE","n","Mean (s)","SD","SE"),booktabs =TRUE) %>%
  kable_classic(full_width = T, font_size = 26) %>% 
  add_header_above(c(" "=1, "Day 1"=4,"Day 2"=4,"Day 3"=4)) %>% 
  as_image(height = 12, file = "graphs/wks6.table.stats.png")  #<--

# RR.6wks.PairWisettest.table --------------------------------------------------


# B2 -----------------------------------------------------------------------

    
ptt.list <- list(pptest.fun2(inputd,'HTT-NI'),
                 pptest.fun2(inputd,'HTT+NI'),
                 pptest.fun2(inputd,'PSD95-6ZF-VP64'),
                 pptest.fun2(inputd,'PSD95-6ZF-NoED'))

pp.Day <- ptt.list %>% 
  reduce(inner_join, by='Day') %>% 
  mutate(
    across(2:9, 
           ~ if_else((.x)<0.05,(paste((format(.x, digits=3,scientific=TRUE)),starmaker(.x))),
                     format(.x, digits=3,scientific=TRUE),
                     ""))
  )

kbl(pp.ATF, align = "c", col.names = c("" ,"Day 1","Day 2","Day 1","Day 2","Day 1","Day 2","Day 1","Day 2"),booktabs =TRUE) %>%
  kable_classic(full_width = T, font_size = 26) %>% 
  add_header_above(c(" "=1, "HTT-NI"=2,"HTT+NI"=2,"PSD95-6ZF-VP64"=2,"PSD95-6ZF-NoED"=2)) %>%
  as_image(height = 12, file = "graphs/wks6.table.byDays.png")

# C1 ----------------------------------------------------------------------

ptt.list.ATF <- list(pptest.ATF.fun2(inputd,'Day 1'),
                     pptest.ATF.fun2(inputd,'Day 2'),
                     pptest.ATF.fun2(inputd,'Day 3')
)

pp.test <- bind_rows(ptt.list.ATF) 
row.names(pp.test) <- NULL

pp.ATF <- pp.test %>% 
  mutate(
    across(2:4, 
           ~ if_else((.x)<0.05,(paste((format(.x, digits=3,scientific=TRUE)),starmaker(.x))),
                     format(.x, digits=3,scientific=TRUE),
                     ""))
  ) 

kbl (pp.ATF, align = "l", col.names = c("" ,"HTT-NI","HTT+NI","PSD95-6ZF-NoED"),booktabs =TRUE) %>% 
  collapse_rows(columns = 1, valign = "middle") %>%
  kable_classic(full_width = T, font_size = 26) %>%
  pack_rows("Day 1", 1, 3) %>%
  pack_rows("Day 2", 4, 6) %>%
  pack_rows("Day 3", 7, 9) %>%
  as_image(height = 12, file = "graphs/wks6.table.byATF.png")

# RR.9wks.graph -------------------------------------------------------
inputd<- left_join(filter(RR.Data, Week == "s9"), 
                   filter(Main, ExpCode_4=="BHRR9"), 
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


g <-
  ggplot(inputd,aes(x= Day, y=time))+
  facet_wrap(~ATF, ncol=2)+
  geom_boxplot(width=0.3, fill=NA,linewidth = 0.1, size=0, varwidth = FALSE, outlier.alpha = 0)+
  stat_summary(aes(group=ATF), data = inputd,  fun =median, color = "black", geom = "line", size=0.5, alpha=0.2)+
  geom_point(inherit.aes = TRUE,
             colour   = inputd$ColorCode,
             fill     = inputd$ColorCode,
             shape    = inputd$Symbol,
             alpha    = 0.5, 
             size     = 3, 
             position = position_jitter(width = 0.1, seed = 1)) +
  geom_line(aes(group = Short_ID),
            colour   = "grey",
            alpha    = 0.5, 
            position = position_jitter(width = 0.1, seed = 1),
            linetype = "dashed"
  )+
  geom_point(
    colour   = inputd$ColorCode,
    shape    = inputd$Symbol,
    fill     = inputd$ColorCode,
    position = position_jitter(width = 0.1, seed = 1)) +
  geom_text_repel(aes(label=inputd$Short_ID),
                  colour=inputd$ColorCode,
                  point.padding     = 0.5,
                  max.overlaps      = Inf,
                  force             = 1,
                  direction         = "both",
                  size              = 2,
                  segment.size      = 0.3,
                  segment.curvature = -0.1,
                  position = position_jitter(width = 0.1, seed = 1))+
  theme_classic () + plot.theme.box + labs(y="Latency to fall (s)") + scale_y_continuous(expand = c(0, 0), limits = c(0, 300))

g
ggsave("graphs/RRs9.graph.png", width = 2000, height = 2000, units = "px", dpi = 300, bg=NULL)

# RR.9Wks.Table.stats -------------------------------------------------------------

RR.data.list <- list((inputd %>%  group_by(ATF) %>%  
                        summarise(count.1=sum(Day == "Day 1"), 
                                  Mean.1=mean(time[Day == "Day 1"]), 
                                  SD.1=sd(time[Day == "Day 1"]),
                                  SE.1= (sd(time[Day == "Day 1"]))/(sqrt(sum(Day == "Day 1"))))
                      ), 
                      (inputd %>%  group_by(ATF) %>%  
                         summarise(count.1=sum(Day == "Day 2"), 
                                   Mean.1=mean(time[Day == "Day 2"]), 
                                   SD.1=sd(time[Day == "Day 2"]),
                                   SE.1= (sd(time[Day == "Day 2"]))/(sqrt(sum(Day == "Day 2"))))
                      ),
                      (inputd %>%  group_by(ATF) %>%  
                         summarise(count.1=sum(Day == "Day 3"), 
                                   Mean.1=mean(time[Day == "Day 3"]), 
                                   SD.1=sd(time[Day == "Day 3"]),
                                   SE.1= (sd(time[Day == "Day 3"]))/(sqrt(sum(Day == "Day 3"))))
                      )
                      ) 

kbl((RR.data.list %>% reduce(inner_join, by='ATF')), align = "c", digits = 2, 
    col.names = c("ATF" ,"n","Mean (s)","SD","SE","n","Mean (s)","SD","SE","n","Mean (s)","SD","SE"),booktabs =TRUE) %>%
  kable_classic(full_width = T, font_size = 26) %>% 
  add_header_above(c(" "=1, "Day 1"=4,"Day 2"=4,"Day 3"=4)) %>%
  as_image(height = 12, file = "graphs/wks9.table.stats.png")  #<--

# RR.9wks.PairWisettest.table --------------------------------------------------


# B2 -----------------------------------------------------------------------


ptt.list <- list(pptest.fun2(inputd,'HTT-NI'),
                 pptest.fun2(inputd,'HTT+NI'),
                 pptest.fun2(inputd,'PSD95-6ZF-VP64'),
                 pptest.fun2(inputd,'PSD95-6ZF-NoED'))

pp.Day <- ptt.list %>% 
  reduce(inner_join, by='Day') %>% 
  mutate(
    across(2:9, 
           ~ if_else((.x)<0.05,(paste((format(.x, digits=3,scientific=TRUE)),starmaker(.x))),
                     format(.x, digits=3,scientific=TRUE),
                     ""))
  )

kbl(pp.Day, align = "c", col.names = c("" ,"Day 1","Day 2","Day 1","Day 2","Day 1","Day 2","Day 1","Day 2"),booktabs =TRUE) %>%
  kable_classic(full_width = T, font_size = 26) %>% 
  add_header_above(c(" "=1, "HTT-NI"=2,"HTT+NI"=2,"PSD95-6ZF-VP64"=2,"PSD95-6ZF-NoED"=2)) %>%
  as_image(height = 12, file = "graphs/wks9.table.byDays.png")

# C1 ----------------------------------------------------------------------

ptt.list.ATF <- list(pptest.ATF.fun2(inputd,'Day 1'),
                     pptest.ATF.fun2(inputd,'Day 2'),
                     pptest.ATF.fun2(inputd,'Day 3')
)

pp.test <- bind_rows(ptt.list.ATF) 
row.names(pp.test) <- NULL

pp.ATF <- pp.test %>% 
  mutate(
    across(2:4, 
           ~ if_else((.x)<0.05,(paste((format(.x, digits=3,scientific=TRUE)),starmaker(.x))),
                     format(.x, digits=3,scientific=TRUE),
                     ""))
  ) 

kbl (pp.ATF, align = "l", col.names = c("" ,"HTT-NI","HTT+NI","PSD95-6ZF-NoED"),booktabs =TRUE) %>% 
  collapse_rows(columns = 1, valign = "middle") %>%
  kable_classic(full_width = T, font_size = 26) %>%
  pack_rows("Day 1", 1, 3) %>%
  pack_rows("Day 2", 4, 6) %>%
  pack_rows("Day 3", 7, 9) %>%
  as_image(height = 12, file = "graphs/wks9.table.byATF.png")
