
# BM.Day1 ------------------------------------------------------------------
inputd<- left_join(filter(BM.Data, ColorCode!="error", Day =="Day 1"), 
                   filter(Main, ExpCode_6=="BHBM"), 
                   by=c("Short_ID","ATF","ColorCode"))

level.check.fun(inputd,"ATF")
inputd$ATF<-ordered.fun(inputd,"ATF")

BM.graph.day1.sts <-
  pairwise_comparisons(inputd, ATF, Latency) 

BM.graph.day1.sts<- BM.graph.day1.sts %>%
  dplyr::mutate(groups = purrr::pmap(.l = list(group1, group2), .f = c)) %>%
  dplyr::arrange(group1) %>%
  dplyr::mutate(asterisk_label = starmaker(BM.graph.day1.sts$p.value, p.levels=c(.001, .01, .05, .1), symbols=c("***", "**", "*", "")))

BM.graph.day1.sts<-filter(BM.graph.day1.sts, p.value<=.05)
BM.graph.day1.sts

BM.day1.des.sts<- ggbetweenstats(inputd, ATF, Latency)

BM.graph.day1.anexo <- grapher.V2.BMDAY.anexo((BM.graph.day1.sts$groups),BM.graph.day1.sts$expression) +
  theme(axis.line.y = element_line(size = 0.5, colour = "black", linetype=1), 
        axis.title.y = element_text(size = 10, angle = 90),
        axis.text.y = element_text(size=7, colour = "black"),
        axis.ticks.y = element_line(size = 0.5, colour = "black", linetype=1),
        axis.text.x = element_blank()) +
  labs(y="Latency (s)")

BM.graph.day1 <- grapher.V2.BMDAY((BM.graph.day1.sts$groups), BM.graph.day1.sts$asterisk_label) +
  theme(axis.line.y = element_line(size = 0.5, colour = "black", linetype=1), 
        axis.title.y = element_text(size = 10, angle = 90),
        axis.text.y = element_text(size=7, colour = "black"),
        axis.ticks.y = element_line(size = 0.5, colour = "black", linetype=1),
        axis.text.x = element_blank()) +
  labs(y="Latency (s)")

# BM.Day5 ------------------------------------------------------------------
inputd<- left_join(filter(BM.Data, ColorCode!="error", Day =="Day 5"), 
                   filter(Main, ExpCode_6=="BHBM"), 
                   by=c("Short_ID","ATF","ColorCode"))

level.check.fun(inputd,"ATF")
inputd$ATF<-ordered.fun(inputd,"ATF")

BM.graph.day5.sts <-
  pairwise_comparisons(inputd, ATF, Latency) 

BM.graph.day5.sts<- BM.graph.day5.sts %>%
  dplyr::mutate(groups = purrr::pmap(.l = list(group1, group2), .f = c)) %>%
  dplyr::arrange(group1) %>%
  dplyr::mutate(asterisk_label = starmaker(BM.graph.day5.sts$p.value, p.levels=c(.001, .01, .05, .1), symbols=c("***", "**", "*", "")))

BM.graph.day5.sts<-filter(BM.graph.day5.sts, p.value<=.05)
BM.day5.des.sts<- ggbetweenstats(inputd, ATF, Latency)

BM.graph.day5.anexo <- grapher.V2.BMDAY.anexo((BM.graph.day5.sts$groups), (BM.graph.day5.sts$expression)) +
                                                theme(axis.text.x = element_blank())
BM.graph.day5 <- grapher.V2.BMDAY((BM.graph.day5.sts$groups), BM.graph.day5.sts$asterisk_label) + 
                                                theme(axis.text.x = element_blank())

# BM.Day7 ------------------------------------------------------------------
inputd<- left_join(filter(BM.Data, ColorCode!="error", Day =="Day 7"), 
                   filter(Main, ExpCode_6=="BHBM"), 
                   by=c("Short_ID","ATF","ColorCode"))

level.check.fun(inputd,"ATF")
inputd$ATF<-ordered.fun(inputd,"ATF")

BM.graph.day7.sts <-
  pairwise_comparisons(inputd, ATF, Latency) 

BM.graph.day7.sts<- BM.graph.day7.sts %>%
  dplyr::mutate(groups = purrr::pmap(.l = list(group1, group2), .f = c)) %>%
  dplyr::arrange(group1) %>%
  dplyr::mutate(asterisk_label = starmaker(BM.graph.day7.sts$p.value, p.levels=c(.001, .01, .05, .1), symbols=c("***", "**", "*", "")))

BM.graph.day7.sts<-filter(BM.graph.day7.sts, p.value<=.05)
BM.day7.des.sts<- ggbetweenstats(inputd, ATF, Latency)

BM.graph.day7.anexo <- grapher.V2.BMDAY.anexo((BM.graph.day7.sts$groups), (BM.graph.day7.sts$expression)) + 
                                                theme(axis.text.x = element_blank())
                                              
BM.graph.day7 <- grapher.V2.BMDAY((BM.graph.day7.sts$groups), BM.graph.day7.sts$asterisk_label) +
            theme(axis.text.x = element_blank())

# Integrador --------------------------------------------------------------
library(cowplot)
BM.cmp.157 <-
  ggdraw(xlim = c(0, 1), ylim = c(0, 1)) +
  draw_plot(
    ggarrange(g.a,BM.graph.day1,g.d2,ncol = 1, nrow = 3,align = "hv",hjust=-15, heights = c(.35,1.5,0.5)) %>%
      annotate_figure(top = text_grob("Day 1", size = 10, hjust = 1, x = 0.7, y=-0.5)),
             .11,  0,  .33,  1) + 
  draw_plot(
    ggarrange(g.a,BM.graph.day5,g.d + theme(axis.text.y = element_blank()),
              ncol = 1, nrow = 3,align = "hv",hjust=-15, heights = c(.35,1.5,0.5)) %>%
      annotate_figure(top = text_grob("Day 5", size = 10, hjust = 1, x = 0.7, y=-0.5)),
            .43,  0,  .23,  1) +
  draw_plot(
    ggarrange(g.a,BM.graph.day7,g.d + theme(axis.text.y = element_blank()),
              ncol = 1, nrow = 3,align = "hv",hjust=-15, heights = c(.35,1.5,0.5)) %>%
      annotate_figure(top = text_grob("Day 7", size = 10, hjust = 1, x = 0.7, y=-0.5)),
            .66,   0,  .23,  1)

BM.cmp.157.anexo <-
  ggdraw(xlim = c(0, 1), ylim = c(0, 1)) +
  draw_plot(
    ggarrange(g.a,BM.graph.day1.anexo,g.d,ncol = 1, nrow = 3,align = "hv",hjust=-15, heights = c(.35,1.5,0.5)) %>%
      annotate_figure(top = text_grob(extract_subtitle(BM.day1.des.sts), face = "plain", size = 5), 
                      fig.lab = "Day 1", fig.lab.pos ="top",
                      bottom = text_grob(extract_caption(BM.day1.des.sts), face = "plain", size = 5, x=.6)),
    .11,  0,  .33,  1) + 
  draw_plot(
    ggarrange(g.a,BM.graph.day5.anexo,g.d + theme(axis.text.y = element_blank()),
              ncol = 1, nrow = 3,align = "hv",hjust=-15, heights = c(.35,1.5,0.5)) %>%
      annotate_figure(top = text_grob("Day 5", size = 10, hjust = 1, x = 0.7, y=-0.5)),
    .43,  0,  .23,  1) +
  draw_plot(
    ggarrange(g.a,BM.graph.day7.anexo,g.d + theme(axis.text.y = element_blank()),
              ncol = 1, nrow = 3,align = "hv",hjust=-15, heights = c(.35,1.5,0.5)) %>%
      annotate_figure(top = text_grob("Day 7", size = 10, hjust = 1, x = 0.7, y=-0.5)),
    .66,   0,  .23,  1)

BM.cmp.157.anexo <- plot_grid(
  plot_grid(g.a,
            BM.graph.day1.anexo,
            g.d, 
            ncol = 1, align = "v", rel_heights = c(.15, 1,.2)) %>% 
    annotate_figure(top = text_grob(extract_subtitle(BM.day1.des.sts), face = "plain", size = 5, x=.6, y=-.1), 
                    fig.lab = "Day 1", fig.lab.pos ="top",
                    bottom = text_grob(extract_caption(BM.day1.des.sts), face = "plain", size = 5, x=.6)),
  plot_grid(g.a,
            BM.graph.day5.anexo,
            (g.d + theme(axis.text.y = element_blank())),
            ncol = 1, align = "v", rel_heights = c(.15, 1,.2)) %>% 
    annotate_figure(top = text_grob(extract_subtitle(BM.day5.des.sts), face = "plain", size = 5, y=-.1), 
                    fig.lab = "Day 5", fig.lab.pos ="top",
                    bottom = text_grob(extract_caption(BM.day5.des.sts), face = "plain", size = 5, x=.6)),
  plot_grid(g.a,
            BM.graph.day7.anexo,
            (g.d + theme(axis.text.y = element_blank())),
            ncol = 1, align = "v", rel_heights = c(.15, 1,.2)) %>% 
    annotate_figure(top = text_grob(extract_subtitle(BM.day7.des.sts), face = "plain", size = 5, y=-.1), 
                    fig.lab = "Day 7", fig.lab.pos ="top",
                    bottom = text_grob(extract_caption(BM.day7.des.sts), face = "plain", size = 5, x=.6)),
  ncol = 3, align = "hv", rel_widths = c(1.3,1,1))

BM.cmp.157.anexo

# funs --------------------------------------------------------------------


grapher.V2.BMDAY <- function (level.i, label.i) {
  
  g <-  ggplot(inputd,aes(x=ATF, y=Latency))+
    geom_boxplot(width=0.3, fill=NA,linewidth = 0.1, size=0, varwidth = FALSE, outlier.alpha = 0)+
    stat_summary(aes(group=ATF), data = inputd,  fun =median, color = "black", geom = "line", size=0.5, alpha=0.2) +
    geom_point(inherit.aes = TRUE,
               colour   = inputd$ColorCode,
               fill     = inputd$ColorCode,
               shape    = inputd$Symbol,
               alpha    = 0.5, 
               size     = 2, 
               position = position_jitter(width = 0.1, seed = 1)) +
    geom_line(aes(group = Short_ID),
              colour   = "grey",
              alpha    = 0.5, 
              position = position_jitter(width = 0.1, seed = 1),
              linetype = "dashed")+
    geom_signif(
      comparisons = level.i,
      map_signif_level = TRUE,
      annotations = label.i,
      textsize    = 2,
      size = 0.1,
      step_increase = 0.1)+
    theme_classic () + labs(y="Latency (s)") + theme (axis.line.y = element_blank(),
                                                            axis.title.y = element_blank(),
                                                            axis.text.y = element_blank(),
                                                            axis.ticks.y = element_blank(),
                                                            axis.title.x = element_blank(),
                                                            plot.margin = margin(.5,0,0,0, "cm"))+
    scale_y_continuous(limits = c(0, 450))
  
}

grapher.V2.BMDAY.anexo <- function (level.i, label.i) {
  
  g <-  
    ggplot(inputd,aes(x=ATF, y=Latency))+
    geom_boxplot(width=0.3, fill=NA,linewidth = 0.1, size=0, varwidth = FALSE, outlier.alpha = 0)+
    stat_summary(aes(group=ATF), data = inputd,  fun =median, color = "black", geom = "line", size=0.5, alpha=0.2) +
    geom_point(inherit.aes = TRUE,
               colour   = inputd$ColorCode,
               fill     = inputd$ColorCode,
               shape    = inputd$Symbol,
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
    geom_signif(
      comparisons = (level.i),
      map_signif_level = TRUE,
      annotations = as.character(label.i),
      parse =TRUE,
      textsize    = 2,
      size = .1,
      step_increase = 0.2)+
    theme_classic () + labs(y="Recognition index") + theme (axis.line.y = element_blank(),
                                                            axis.title.y = element_blank(),
                                                            axis.text.y = element_blank(),
                                                            axis.ticks.y = element_blank(),
                                                            axis.title.x = element_blank(),
                                                            text = element_text(size = 7),
                                                            plot.margin = margin(.5,0,0,0, "cm"))+
    scale_y_continuous(limits = c(0, 450)) 
  
}
