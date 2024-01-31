# A1.9wks.graph -------------------------------------------------------
inputd<- left_join(filter(RR.Data, Week == "s9"), 
                   filter(Main, ExpCode_4=="BHRR9"), 
                   by=c("Short_ID","ATF","ColorCode"))

inputd <- filter(inputd, ATF=="HTT-NI")
level.check.fun(inputd,"ATF")
inputd$ATF<-ordered.fun(inputd,"ATF")
inputd$Day <- as.factor(inputd$Day) 

df <-
  pairwise_comparisons(inputd, Day, time) %>%
  dplyr::mutate(groups = purrr::pmap(.l = list(group1, group2), .f = c)) %>%
  dplyr::arrange(group1) %>%
  dplyr::mutate(asterisk_label = starmaker(df$p.value, p.levels=c(.001, .01, .05, .1), symbols=c("***", "**", "*", "")))

graph.sts<-filter(df, p.value<=.05)

A1 <-
  ggplot(inputd,aes(x= Day, y=time))+
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
            linetype = "dashed")+
  geom_point(
    colour   = inputd$ColorCode,
    shape    = inputd$Symbol,
    fill     = inputd$ColorCode,
    position = position_jitter(width = 0.1, seed = 1)) +
  theme_classic () + labs(y="Latency to fall (s)") + theme (
    axis.title.x = element_blank())+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 300))
A1




# A2 ----------------------------------------------------------------------

inputd<- left_join(filter(RR.Data, Week == "s9", ATF=="HTT+NI"), 
                   filter(Main, ExpCode_4=="BHRR9"), 
                   by=c("Short_ID","ATF","ColorCode"))

level.check.fun(inputd,"ATF")
inputd$ATF<-ordered.fun(inputd,"ATF")
inputd$Day <- as.factor(inputd$Day) 

A2 <- grapher.RR()
A2
# A3 ----------------------------------------------------------------------

inputd<- left_join(filter(RR.Data, Week == "s9", ATF=="PSD95-6ZF-VP64"), 
                   filter(Main, ExpCode_4=="BHRR9"), 
                   by=c("Short_ID","ATF","ColorCode"))

level.check.fun(inputd,"ATF")
inputd$ATF<-ordered.fun(inputd,"ATF")
inputd$Day <- as.factor(inputd$Day) 

A3 <- grapher.RR()

A3
# A4 ----------------------------------------------------------------------

inputd<- left_join(filter(RR.Data, Week == "s9", ATF=="PSD95-6ZF-NoED"), 
                   filter(Main, ExpCode_4=="BHRR9"), 
                   by=c("Short_ID","ATF","ColorCode"))

level.check.fun(inputd,"ATF")
inputd$ATF<-ordered.fun(inputd,"ATF")
inputd$Day <- as.factor(inputd$Day) 

A4 <- grapher.RR()

A4

# integrator --------------------------------------------------------------

B2<- ggarrange(g.a,
  (ggarrange(A1,A2,A3,A4, ncol=4, nrow=1,align = "hv", widths =c(2,2,2,2))),
  ncol = 1, nrow = 2, align = "v", heights = c(.3,1)
)
B2
annotate_figure(B2,top = text_grob("Week 9", face = "bold", size = 14))

ggsave("graphs/RR.Weeks9.png", width = 3000, height = 1000, units = "px", dpi = 300, bg=NULL)

# A1.6wks.graph -------------------------------------------------------
inputd<- left_join(filter(RR.Data, Week == "s6"), 
                   filter(Main, ExpCode_3=="BHRR6"), 
                   by=c("Short_ID","ATF","ColorCode"))

inputd <- filter(inputd, ATF=="HTT-NI")
level.check.fun(inputd,"ATF")
inputd$ATF<-ordered.fun(inputd,"ATF")
inputd$Day <- as.factor(inputd$Day) 

df <-
  pairwise_comparisons(inputd, Day, time) %>%
  dplyr::mutate(groups = purrr::pmap(.l = list(group1, group2), .f = c)) %>%
  dplyr::arrange(group1) %>%
  dplyr::mutate(asterisk_label = starmaker(df$p.value, p.levels=c(.001, .01, .05, .1), symbols=c("***", "**", "*", "")))

graph.sts<-filter(df, p.value<=.05)

A1 <-
  ggplot(inputd,aes(x= Day, y=time))+
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
            linetype = "dashed")+
  geom_point(
    colour   = inputd$ColorCode,
    shape    = inputd$Symbol,
    fill     = inputd$ColorCode,
    position = position_jitter(width = 0.1, seed = 1)) +
  theme_classic () + labs(y="Latency to fall (s)") + theme (
    axis.title.x = element_blank())+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 300)) +
  ggsignif::geom_signif(
    comparisons = graph.sts$groups,
    map_signif_level = TRUE,
    annotations = graph.sts$asterisk_label,
    textsize    = 5,
    y_position  = 230,
    step_increase = 0.2)
A1




# A2 ----------------------------------------------------------------------

inputd<- left_join(filter(RR.Data, Week == "s6", ATF=="HTT+NI"), 
                   filter(Main, ExpCode_3=="BHRR6"), 
                   by=c("Short_ID","ATF","ColorCode"))

level.check.fun(inputd,"ATF")
inputd$ATF<-ordered.fun(inputd,"ATF")
inputd$Day <- as.factor(inputd$Day) 

A2 <- grapher.RR()
A2
# A3 ----------------------------------------------------------------------

inputd<- left_join(filter(RR.Data, Week == "s6", ATF=="PSD95-6ZF-VP64"), 
                   filter(Main, ExpCode_3=="BHRR6"), 
                   by=c("Short_ID","ATF","ColorCode"))

level.check.fun(inputd,"ATF")
inputd$ATF<-ordered.fun(inputd,"ATF")
inputd$Day <- as.factor(inputd$Day) 

A3 <- grapher.RR()

A3
# A4 ----------------------------------------------------------------------

inputd<- left_join(filter(RR.Data, Week == "s6", ATF=="PSD95-6ZF-NoED"), 
                   filter(Main, ExpCode_3=="BHRR6"), 
                   by=c("Short_ID","ATF","ColorCode"))

level.check.fun(inputd,"ATF")
inputd$ATF<-ordered.fun(inputd,"ATF")
inputd$Day <- as.factor(inputd$Day) 

A4 <- grapher.RR()

A4

# integrator --------------------------------------------------------------

B2<- ggarrange(g.a,
               (ggarrange(A1,A2,A3,A4, ncol=4, nrow=1,align = "hv", widths =c(2,2,2,2))),
               ncol = 1, nrow = 2, align = "v", heights = c(.3,1)
)

annotate_figure(B2,top = text_grob("Week 6", face = "bold", size = 14))

ggsave("graphs/RR.Weeks6.png", width = 3000, height = 1000, units = "px", dpi = 300, bg=NULL)

# Funs ----------------------------------------------------------------------

grapher.RR <- function () {
df <-
    pairwise_comparisons(inputd, Day, time) %>%
    dplyr::mutate(groups = purrr::pmap(.l = list(group1, group2), .f = c)) %>%
    dplyr::arrange(group1) %>%
    dplyr::mutate(asterisk_label = starmaker(df$p.value, p.levels=c(.001, .01, .05, .1), symbols=c("***", "**", "*", "")))
  
graph.sts<-filter(df, p.value<=.05)
  
g<- ggplot(inputd,aes(x= Day, y=time))+
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
      linetype = "dashed")+
    geom_point(
      colour   = inputd$ColorCode,
      shape    = inputd$Symbol,
      fill     = inputd$ColorCode,
      position = position_jitter(width = 0.1, seed = 1)) +
    theme_classic () + labs(y="Latency to fall (s)") + theme (axis.line.y = element_blank(),
                                                              axis.title.y = element_blank(),
                                                              axis.text.y = element_blank(),
                                                              axis.ticks.y = element_blank(),
                                                              axis.title.x = element_blank())+
    scale_y_continuous(expand = c(0, 0), limits = c(0, 300))

if(length(graph.sts$groups)!=0) {
 g <- g + geom_signif(
    comparisons = graph.sts$groups,
    map_signif_level = TRUE,
    annotations = graph.sts$asterisk_label,
    textsize    = 5,
    y_position  = 1.5,
    step_increase = 0.2)
  return(g)
} else {
  return (g)
}

}


# RR.Descriptive.Table.stats -------------------------------------------------------------

RR.table.sts.desc <- 
  list(
  (filter(RR.Data, Week=="s6") %>%  group_by(ATF) %>%  
     summarise(count.1=sum(Day == "Day 1"), 
               Mean.1=mean(time[Day == "Day 1"]), 
               SD.1=sd(time[Day == "Day 1"]),
               SE.1= (sd(time[Day == "Day 1"]))/(sqrt(sum(Day == "Day 1"))),
               count.2=sum(Day == "Day 2"), 
               Mean.2=mean(time[Day == "Day 2"]), 
               SD.2=sd(time[Day == "Day 2"]),
               SE.2= (sd(time[Day == "Day 2"]))/(sqrt(sum(Day == "Day 2"))),
               count.3=sum(Day == "Day 3"), 
               Mean.3=mean(time[Day == "Day 3"]), 
               SD.3=sd(time[Day == "Day 3"]),
               SE.3= (sd(time[Day == "Day 3"]))/(sqrt(sum(Day == "Day 3"))))
  ),
  (filter(RR.Data, Week=="s9") %>%  group_by(ATF) %>%  
     summarise(count.1=sum(Day == "Day 1"), 
               Mean.1=mean(time[Day == "Day 1"]), 
               SD.1=sd(time[Day == "Day 1"]),
               SE.1= (sd(time[Day == "Day 1"]))/(sqrt(sum(Day == "Day 1"))),
               count.2=sum(Day == "Day 2"), 
               Mean.2=mean(time[Day == "Day 2"]), 
               SD.2=sd(time[Day == "Day 2"]),
               SE.2= (sd(time[Day == "Day 2"]))/(sqrt(sum(Day == "Day 2"))),
               count.3=sum(Day == "Day 3"), 
               Mean.3=mean(time[Day == "Day 3"]), 
               SD.3=sd(time[Day == "Day 3"]),
               SE.3= (sd(time[Day == "Day 3"]))/(sqrt(sum(Day == "Day 3"))))
  )
) #%>% bind_rows()

row.names(RR.table.sts.desc) <- NULL

RR.table.sts.desc.graph <- kbl (RR.table.sts.desc, align = "c",digits = 2, col.names = c("ATF" ,"n","Mean (s)","SD","SE","n","Mean (s)","SD","SE","n","Mean (s)","SD","SE"),booktabs =TRUE) %>% 
  collapse_rows(columns = 1, valign = "middle") %>%
  kable_classic(full_width = T, font_size = 26) %>%
  add_header_above(c(" "=1, "Day 1"=4,"Day 2"=4, "Day 3"=4)) %>%
  pack_rows("Week 6", 1, 4) %>%
  pack_rows("Week 9", 5, 8) 

# RR.6wks.PairWisettest.table --------------------------------------------------


# B2 -----------------------------------------------------------------------
inputd<- filter(RR.Data, Week=="s6") 

ptt.list <-list(data.frame(
                 pptest.fun2(filter(RR.Data, Week=="s6"),'HTT-NI'), ##test pareados.
                 pptest.fun2(filter(RR.Data, Week=="s6"),'HTT+NI'),
                 pptest.fun2(filter(RR.Data, Week=="s6"),'PSD95-6ZF-VP64'),
                 pptest.fun2(filter(RR.Data, Week=="s6"),'PSD95-6ZF-NoED')),
                data.frame(
                 pptest.fun2(filter(RR.Data, Week=="s9"),'HTT-NI'), ##test pareados.
                 pptest.fun2(filter(RR.Data, Week=="s9"),'HTT+NI'),
                 pptest.fun2(filter(RR.Data, Week=="s9"),'PSD95-6ZF-VP64'),
                 pptest.fun2(filter(RR.Data, Week=="s9"),'PSD95-6ZF-NoED'))
                ) %>% bind_rows()


row.names(ptt.list) <- NULL

pp.Day <- ptt.list %>% 
  mutate(
    across(1:8, 
           ~ if_else((.x)<0.05,(paste((format(.x, digits=3,scientific=TRUE)),starmaker(.x))),
                     format(.x, digits=3,scientific=TRUE),
                     ""))
  )
a <- c("Day 2","Day 3","Day 2","Day 3")
pp.Day2 <- cbind(a, pp.Day)

RR.table.sts.cmpbyday.graph <- kbl(pp.Day2, align = "c", col.names = c(" ", "Day 1","Day 2","Day 1","Day 2","Day 1","Day 2","Day 1","Day 2"),booktabs =FALSE) %>%
  kable_classic(full_width = T, font_size = 26) %>% 
  add_header_above(c(" "=1,"Wild type"=2,"Transgenic"=6), align = "l") %>%
  add_header_above(c(" "=5,"PSD95-6ZF-VP64"=2,"PSD95-6ZF-NoED"=2), align = "l") %>%
  pack_rows("Week 6", 1, 2) %>%
  pack_rows("Week 9", 3, 4)

  as_image(table, file = "graphs/test2.png", width = 3 )
  dims(as_image)

  ggtexttable(pp.Day2)
