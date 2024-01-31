

# A1 ----------------------------------------------------------------------
inputd<- left_join(filter(RR.Data, Week == "s6", Day == "Day 1"), 
                   filter(Main, ExpCode_3=="BHRR6"), 
                   by=c("Short_ID","ATF","ColorCode"))

inputd$Day <- as.factor(inputd$Day)
inputd$ATF <- as.factor(inputd$ATF)
inputd$ATF<-ordered.fun(inputd,"ATF")

df <-
  pairwise_comparisons(inputd, ATF, time) %>%
  dplyr::mutate(groups = purrr::pmap(.l = list(group1, group2), .f = c)) %>%
  dplyr::arrange(group1) %>%
  dplyr::mutate(asterisk_label = starmaker(df$p.value, p.levels=c(.001, .01, .05, .1), symbols=c("***", "**", "*", "")))

graph.sts<-filter(df, p.value<=.05)

A1 <-
  ggplot(inputd,aes(x= ATF, y=time))+
  geom_boxplot(width=0.3, fill=NA,linewidth = 0.1, size=0, varwidth = FALSE, outlier.alpha = 0)+
  stat_summary(aes(group=Day), data = inputd,  fun =median, color = "black", geom = "line", size=0.5, alpha=0.2)+
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
  theme_classic () + labs(y="Latency to fall (s)") + theme (
                                                            axis.text.x = element_blank(),
                                                            axis.title.x = element_blank())+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 330)) +
  ggsignif::geom_signif(
    comparisons = graph.sts$groups,
    map_signif_level = TRUE,
    annotations = graph.sts$asterisk_label,
    textsize    = 5,
    y_position  = 165,
    step_increase = 0.15)
A1
# A2 ----------------------------------------------------------------------

inputd<- left_join(filter(RR.Data, Week == "s6", Day == "Day 2"), 
                   filter(Main, ExpCode_3=="BHRR6"), 
                   by=c("Short_ID","ATF","ColorCode"))

level.check.fun(inputd,"ATF")
inputd$ATF<-ordered.fun(inputd,"ATF")
inputd$Day <- as.factor(inputd$Day) 

A2 <- grapher2.RR()
A2

# A3 ----------------------------------------------------------------------

inputd<- left_join(filter(RR.Data, Week == "s6", Day == "Day 3"), 
                   filter(Main, ExpCode_3=="BHRR6"), 
                   by=c("Short_ID","ATF","ColorCode"))

level.check.fun(inputd,"ATF")
inputd$ATF<-ordered.fun(inputd,"ATF")
inputd$Day <- as.factor(inputd$Day) 

A3 <- grapher2.RR()
A3

# integrator --------------------------------------------------------------

B1 <- ggarrange((g.a+ggtitle("Day 1")),A1,g.c,g.d,ncol = 1, nrow = 4,align = "v",heights = c(.7,2,.15,0.7))

B2 <- ggarrange((g.a+ggtitle("Day 2")),A2,g.c,g.d,ncol = 1, nrow = 4,align = "v",heights = c(.7,2,.15,0.7))

B3 <- ggarrange((g.a+ggtitle("Day 3")),A3,g.c,g.d,ncol = 1, nrow = 4,align = "v",heights = c(.7,2,.15,0.7))

C1 <- ggarrange(B1,B2,B3, ncol=3, nrow = 1)
annotate_figure(C1,top = text_grob("Week 6", face = "bold", size = 14))

ggsave("graphs/RR.DaysW6.png", width = 3000, height = 1000, units = "px", dpi = 300, bg=NULL)
# A1 ----------------------------------------------------------------------
inputd<- left_join(filter(RR.Data, Week == "s9", Day == "Day 1"), 
                   filter(Main, ExpCode_4=="BHRR9"), 
                   by=c("Short_ID","ATF","ColorCode"))

inputd$Day <- as.factor(inputd$Day)
inputd$ATF <- as.factor(inputd$ATF)
inputd$ATF<-ordered.fun(inputd,"ATF")

df <-
  pairwise_comparisons(inputd, ATF, time) %>%
  dplyr::mutate(groups = purrr::pmap(.l = list(group1, group2), .f = c)) %>%
  dplyr::arrange(group1) %>%
  dplyr::mutate(asterisk_label = starmaker(df$p.value, p.levels=c(.001, .01, .05, .1), symbols=c("***", "**", "*", "")))

graph.sts<-filter(df, p.value<=.05)

A1 <-
  ggplot(inputd,aes(x= ATF, y=time))+
  geom_boxplot(width=0.3, fill=NA,linewidth = 0.1, size=0, varwidth = FALSE, outlier.alpha = 0)+
  stat_summary(aes(group=Day), data = inputd,  fun =median, color = "black", geom = "line", size=0.5, alpha=0.2)+
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
  theme_classic () + labs(y="Latency to fall (s)") + theme (
    axis.text.x = element_blank(),
    axis.title.x = element_blank())+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 400)) +
  ggsignif::geom_signif(
    comparisons = graph.sts$groups,
    map_signif_level = TRUE,
    annotations = graph.sts$asterisk_label,
    textsize    = 5,
    y_position  = 220,
    step_increase = 0.15)
A1
# A2 ----------------------------------------------------------------------

inputd<- left_join(filter(RR.Data, Week == "s9", Day == "Day 2"), 
                   filter(Main, ExpCode_4=="BHRR9"), 
                   by=c("Short_ID","ATF","ColorCode"))

level.check.fun(inputd,"ATF")
inputd$ATF<-ordered.fun(inputd,"ATF")
inputd$Day <- as.factor(inputd$Day) 

df <-
  pairwise_comparisons(inputd, ATF, time) %>%
  dplyr::mutate(groups = purrr::pmap(.l = list(group1, group2), .f = c)) %>%
  dplyr::arrange(group1) %>%
  dplyr::mutate(asterisk_label = starmaker(df$p.value, p.levels=c(.001, .01, .05, .1), symbols=c("***", "**", "*", "")))

graph.sts<-filter(df, p.value<=.05)

A2 <- grapher2.RR()
A2

# A3 ----------------------------------------------------------------------

inputd<- left_join(filter(RR.Data, Week == "s9", Day == "Day 3"), 
                   filter(Main, ExpCode_4=="BHRR9"), 
                   by=c("Short_ID","ATF","ColorCode"))

level.check.fun(inputd,"ATF")
inputd$ATF<-ordered.fun(inputd,"ATF")
inputd$Day <- as.factor(inputd$Day) 

df <-
  pairwise_comparisons(inputd, ATF, time) %>%
  dplyr::mutate(groups = purrr::pmap(.l = list(group1, group2), .f = c)) %>%
  dplyr::arrange(group1) %>%
  dplyr::mutate(asterisk_label = starmaker(df$p.value, p.levels=c(.001, .01, .05, .1), symbols=c("***", "**", "*", "")))

graph.sts<-filter(df, p.value<=.05)
A3 <- grapher2.RR()
A3

# integrator --------------------------------------------------------------

B1 <- ggarrange((g.a+ggtitle("Day 1")),A1,g.c,g.d,ncol = 1, nrow = 4,align = "v",heights = c(.7,2,.15,0.7))

B2 <- ggarrange((g.a+ggtitle("Day 2")),A2,g.c,g.d,ncol = 1, nrow = 4,align = "v",heights = c(.7,2,.15,0.7))

B3 <- ggarrange((g.a+ggtitle("Day 3")),A3,g.c,g.d,ncol = 1, nrow = 4,align = "v",heights = c(.7,2,.15,0.7))

C1 <- ggarrange(B1,B2,B3, ncol=3, nrow = 1)
annotate_figure(C1,top = text_grob("Week 9", face = "bold", size = 14))

ggsave("graphs/RR.DaysW9.png", width = 3000, height = 1500, units = "px", dpi = 300, bg=NULL)
# Funs ----------------------------------------------------------------------

grapher2.RR <- function () {
  
  
  g<- ggplot(inputd,aes(x= ATF, y=time))+
    geom_boxplot(width=0.3, fill=NA,linewidth = 0.1, size=0, varwidth = FALSE, outlier.alpha = 0)+
    stat_summary(aes(group=Day), data = inputd,  fun =median, color = "black", geom = "line", size=0.5, alpha=0.2)+
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
    theme_classic () + labs(y="Latency to fall (s)") + theme (axis.line.y = element_blank(),
                                                              axis.title.y = element_blank(),
                                                              axis.text.y = element_blank(),
                                                              axis.ticks.y = element_blank(),
                                                              axis.text.x = element_blank(),
                                                              axis.title.x = element_blank())+
    scale_y_continuous(expand = c(0, 0), limits = c(0, 400))
  
  if(length(graph.sts$groups)!=0) {
    g <- g + geom_signif(
      comparisons = graph.sts$groups,
      map_signif_level = TRUE,
      annotations = graph.sts$asterisk_label,
      textsize    = 5,
      y_position  = 233,
      step_increase = 0.17)
    return(g)
  } else {
    return (g)
  }
  
}
##


