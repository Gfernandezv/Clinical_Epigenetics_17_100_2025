

# By days -----------------------------------------------------------------


# 6 Weeks -----------------------------------------------------------------


# RR.W6D1 ----------------------------------------------------------------------
inputd<- left_join(filter(RR.Data, Week == "s6", Day == "Day 1"), 
                   filter(Main, ExpCode_3=="BHRR6"), 
                   by=c("Short_ID","ATF","ColorCode"))

level.check.fun(inputd,"ATF")
inputd$ATF<-ordered.fun(inputd,"ATF")
inputd$Day <- as.factor(inputd$Day) 

RR.W6D1.des.sts <-
  pairwise_comparisons(inputd, ATF, time)

RR.W6D1.des.sts <- RR.W6D1.des.sts %>%
  dplyr::mutate(groups = purrr::pmap(.l = list(group1, group2), .f = c)) %>%
  dplyr::arrange(group1) %>%
  dplyr::mutate(asterisk_label = starmaker(RR.W6D1.des.sts$p.value, p.levels=c(.001, .01, .05, .1), symbols=c("***", "**", "*", "")))
RR.W6D1.des.sts

extract_stats(RR.W6D1.des.sts2)
RR.W6D1.sts<-filter(RR.W6D1.des.sts, p.value<=.05)

RR.W6D1.det.sts<- ggbetweenstats(inputd, ATF, time)

RR.W6D1.sts <- graph.sts
RR.w6.D1 <- grapher2.RR()  + theme_classic () + labs(y="Latency to fall (s)") + theme (
  axis.text.x = element_blank(),
  axis.title.x = element_blank(),
  text = element_text(size = 7))

RR.w6.D1.anexo <- grapher6.RR()  + theme_classic () + labs(y="Latency to fall (s)") + theme (
  axis.text.x = element_blank(),
  axis.title.x = element_blank(),
  text = element_text(size = 7))

# RR.W6D2 ----------------------------------------------------------------------

inputd<- left_join(filter(RR.Data, Week == "s6", Day == "Day 2"), 
                   filter(Main, ExpCode_3=="BHRR6"), 
                   by=c("Short_ID","ATF","ColorCode"))

level.check.fun(inputd,"ATF")
inputd$ATF<-ordered.fun(inputd,"ATF")
inputd$Day <- as.factor(inputd$Day) 

df <-
  pairwise_comparisons(inputd, ATF, time)
df <- df %>%
  dplyr::mutate(groups = purrr::pmap(.l = list(group1, group2), .f = c)) %>%
  dplyr::arrange(group1) %>%
  dplyr::mutate(asterisk_label = starmaker(df$p.value, p.levels=c(.001, .01, .05, .1), symbols=c("***", "**", "*", "")))

graph.sts<-filter(df, p.value<=.05)
RR.W6D2.det.sts<- ggbetweenstats(inputd, ATF, time)

RR.W6D2.sts <- graph.sts
RR.w6.D2 <- grapher2.RR()
RR.w6.D2.anexo <- grapher6.RR()

# RR.W6D3 ----------------------------------------------------------------------

inputd<- left_join(filter(RR.Data, Week == "s6", Day == "Day 3"), 
                   filter(Main, ExpCode_3=="BHRR6"), 
                   by=c("Short_ID","ATF","ColorCode"))

level.check.fun(inputd,"ATF")
inputd$ATF<-ordered.fun(inputd,"ATF")
inputd$Day <- as.factor(inputd$Day) 

df <-
  pairwise_comparisons(inputd, ATF, time)
df <- df %>%
  dplyr::mutate(groups = purrr::pmap(.l = list(group1, group2), .f = c)) %>%
  dplyr::arrange(group1) %>%
  dplyr::mutate(asterisk_label = starmaker(df$p.value, p.levels=c(.001, .01, .05, .1), symbols=c("***", "**", "*", "")))

graph.sts<-filter(df, p.value<=.05)
RR.W6D3.det.sts<- ggbetweenstats(inputd, ATF, time)

RR.w6.D3 <- grapher2.RR()
RR.w6.D3.anexo <- grapher6.RR()
# integrator --------------------------------------------------------------
library(cowplot)

RR.W6 <- plot_grid(
  plot_grid(g.a,
            RR.w6.D1,
            g.d2, 
            ncol = 1, align = "v", rel_heights = c(.15, 1,.2)) %>% 
    annotate_figure(top = text_grob("Day 1", face = "plain", size = 9, x=.7)),
  plot_grid(g.a,
            RR.w6.D2,
            (g.d2 + theme(axis.text.y = element_blank())),
            ncol = 1, align = "v", rel_heights = c(.15, 1,.2)) %>% 
    annotate_figure(top = text_grob("Day 2", face = "plain", size = 9,)),
  plot_grid(g.a,
            RR.w6.D3,
            (g.d2 + theme(axis.text.y = element_blank())),
            ncol = 1, align = "v", rel_heights = c(.15, 1,.2)) %>% 
    annotate_figure(top = text_grob("Day 3", face = "plain", size = 9)),
                   ncol = 3, align = "hv", rel_widths = c(1.5,1,1)) %>%
  annotate_figure(top = text_grob("6 Weeks", face = "plain", size = 9, x=.5))

RR.W6

RR.W6.anexo <- plot_grid(
  plot_grid(g.a,
            RR.w6.D1.anexo,
            g.d, 
            ncol = 1, align = "v", rel_heights = c(.15, 1,.2)) %>% 
    annotate_figure(top = text_grob(extract_subtitle(RR.W6D1.det.sts), face = "plain", size = 5, x=.6, y=-.1), 
                    fig.lab = "Day 1", fig.lab.pos ="top",
                    bottom = text_grob(extract_caption(RR.W6D1.det.sts), face = "plain", size = 5, x=.6)),
  plot_grid(g.a,
            RR.w6.D2.anexo,
            (g.d + theme(axis.text.y = element_blank())),
            ncol = 1, align = "v", rel_heights = c(.15, 1,.2)) %>% 
    annotate_figure(top = text_grob(extract_subtitle(RR.W6D2.det.sts), face = "plain", size = 5, y=-.1), 
                    fig.lab = "Day 2", fig.lab.pos ="top",
                    bottom = text_grob(extract_caption(RR.W6D2.det.sts), face = "plain", size = 5, x=.6)),
  plot_grid(g.a,
            RR.w6.D3.anexo,
            (g.d + theme(axis.text.y = element_blank())),
            ncol = 1, align = "v", rel_heights = c(.15, 1,.2)) %>% 
    annotate_figure(top = text_grob(extract_subtitle(RR.W6D3.det.sts), face = "plain", size = 5, y=-.1), 
                    fig.lab = "Day 3", fig.lab.pos ="top",
                    bottom = text_grob(extract_caption(RR.W6D3.det.sts), face = "plain", size = 5, x=.6)),
  ncol = 3, align = "hv", rel_widths = c(1.3,1,1))

RR.W6.anexo

ggsave("graphs/test.png", width = 20, units = "cm", dpi = 300, bg=NULL)

test <- paste(c("Day 1",extract_subtitle(RR.W6D1.det.sts)))
test

plot_grid(g.a,
          RR.w6.D1.anexo,
          g.d, 
          ncol = 1, align = "v", rel_heights = c(.15, 1,.2))
               
ggdraw()+
  draw_plot(
    plot_grid(g.a,
            RR.w6.D1.anexo,
            g.d, 
            ncol = 1, align = "v", rel_heights = c(.15, 1,.2)), x=0, y=0) +
  draw_text(as.character(extract_caption(RR.W6D1.det.sts)), x = 0, y = 1)

a<-as.character(extract_caption(RR.W6D1.det.sts))


# 9 Weeks -----------------------------------------------------------------


# D1 ----------------------------------------------------------------------
inputd<- left_join(filter(RR.Data, Week == "s9", Day == "Day 1"), 
                   filter(Main, ExpCode_4=="BHRR9"), 
                   by=c("Short_ID","ATF","ColorCode"))

level.check.fun(inputd,"ATF")
inputd$ATF<-ordered.fun(inputd,"ATF")
inputd$Day <- as.factor(inputd$Day) 

df <-
  pairwise_comparisons(inputd, ATF, time, p.adjust.method = "none")
df <- df %>%
  dplyr::mutate(groups = purrr::pmap(.l = list(group1, group2), .f = c)) %>%
  dplyr::arrange(group1) %>%
  dplyr::mutate(asterisk_label = starmaker(df$p.value, p.levels=c(.001, .01, .05, .1), symbols=c("***", "**", "*", "")))

graph.sts<-filter(df, p.value<=.05)
RR.W9D1.det.sts<- ggbetweenstats(inputd, ATF, time)

RR.w9.D1 <- grapher2.RR()  + theme_classic () + labs(y="Latency to fall (s)") + theme (
  axis.text.x = element_blank(),
  axis.title.x = element_blank(),
  text = element_text(size = 7))

RR.w9.D1.anexo <- grapher6.RR()  + theme_classic () + labs(y="Latency to fall (s)") + theme (
  axis.text.x = element_blank(),
  axis.title.x = element_blank(),
  text = element_text(size = 7))
# D2 ----------------------------------------------------------------------

inputd<- left_join(filter(RR.Data, Week == "s9", Day == "Day 2"), 
                   filter(Main, ExpCode_4=="BHRR9"), 
                   by=c("Short_ID","ATF","ColorCode"))

level.check.fun(inputd,"ATF")
inputd$ATF<-ordered.fun(inputd,"ATF")
inputd$Day <- as.factor(inputd$Day) 

df <-
  pairwise_comparisons(inputd, ATF, time, p.adjust.method = "none") 
df <- df %>%
  dplyr::mutate(groups = purrr::pmap(.l = list(group1, group2), .f = c)) %>%
  dplyr::arrange(group1) %>%
  dplyr::mutate(asterisk_label = starmaker(df$p.value, p.levels=c(.001, .01, .05, .1), symbols=c("***", "**", "*", "")))

graph.sts<-filter(df, p.value<=.05)
RR.W9D2.det.sts<- ggbetweenstats(inputd, ATF, time)

RR.w9.D2 <- grapher2.RR()
RR.w9.D2.anexo <- grapher6.RR()

# D3 ----------------------------------------------------------------------

inputd<- left_join(filter(RR.Data, Week == "s9", Day == "Day 3"), 
                   filter(Main, ExpCode_4=="BHRR9"), 
                   by=c("Short_ID","ATF","ColorCode"))

level.check.fun(inputd,"ATF")
inputd$ATF<-ordered.fun(inputd,"ATF")
inputd$Day <- as.factor(inputd$Day) 

df <-
  pairwise_comparisons(inputd, ATF, time) 
df <- df %>%
  dplyr::mutate(groups = purrr::pmap(.l = list(group1, group2), .f = c)) %>%
  dplyr::arrange(group1) %>%
  dplyr::mutate(asterisk_label = starmaker(df$p.value, p.levels=c(.001, .01, .05, .1), symbols=c("***", "**", "*", "")))

graph.sts<-filter(df, p.value<=.05)
RR.W9D3.det.sts<- ggbetweenstats(inputd, ATF, time)

RR.w9.D3 <- grapher2.RR()
RR.w9.D3.anexo <- grapher6.RR()

# integrator --------------------------------------------------------------
RR.W9 <- plot_grid(
  plot_grid(g.a,
            RR.w9.D1,
            g.d2, 
            ncol = 1, align = "v", rel_heights = c(.15, 1,.2)) %>% 
    annotate_figure(top = text_grob("Day 1", face = "plain", size = 9, x=.7)),
  plot_grid(g.a,
            RR.w9.D2,
            (g.d2 + theme(axis.text.y = element_blank())),
            ncol = 1, align = "v", rel_heights = c(.15, 1,.2)) %>% 
    annotate_figure(top = text_grob("Day 2", face = "plain", size = 9,)),
  plot_grid(g.a,
            RR.w9.D3,
            (g.d2 + theme(axis.text.y = element_blank())),
            ncol = 1, align = "v", rel_heights = c(.15, 1,.2)) %>% 
    annotate_figure(top = text_grob("Day 3", face = "plain", size = 9)),
  ncol = 3, align = "hv", rel_widths = c(1.5,1,1))%>%
  annotate_figure(top = text_grob("9 Weeks", face = "plain", size = 10, x=.5))

RR.W9

RR.W9.anexo <- plot_grid(
  plot_grid(g.a,
            RR.w9.D1.anexo,
            g.d, 
            ncol = 1, align = "v", rel_heights = c(.15, 1,.2)) %>% 
    annotate_figure(top = text_grob(extract_subtitle(RR.W9D1.det.sts), face = "plain", size = 5, x=.6, y=-.1), 
                    fig.lab = "Day 1", fig.lab.pos ="top",
                    bottom = text_grob(extract_caption(RR.W9D1.det.sts), face = "plain", size = 5, x=.6)),
  plot_grid(g.a,
            RR.w9.D2.anexo,
            (g.d + theme(axis.text.y = element_blank())),
            ncol = 1, align = "v", rel_heights = c(.15, 1,.2)) %>% 
    annotate_figure(top = text_grob(extract_subtitle(RR.W9D2.det.sts), face = "plain", size = 5, y=-.1), 
                    fig.lab = "Day 2", fig.lab.pos ="top",
                    bottom = text_grob(extract_caption(RR.W9D2.det.sts), face = "plain", size = 5, x=.6)),
  plot_grid(g.a,
            RR.w9.D3.anexo,
            (g.d + theme(axis.text.y = element_blank())),
            ncol = 1, align = "v", rel_heights = c(.15, 1,.2)) %>% 
    annotate_figure(top = text_grob(extract_subtitle(RR.W9D3.det.sts), face = "plain", size = 5, y=-.1), 
                    fig.lab = "Day 3", fig.lab.pos ="top",
                    bottom = text_grob(extract_caption(RR.W9D3.det.sts), face = "plain", size = 5, x=.6)),
  ncol = 3, align = "hv", rel_widths = c(1.3,1,1))

RR.W9.anexo

ggsave("graphs/RR.DaysW9.png", width = 2000, height = 1300, units = "px", dpi = 300, bg=NULL)


# Force -------------------------------------------------------------------


# S6 ----------------------------------------------------------------------

inputd<- left_join(Force.Data, 
                   Main, 
                   by=c("Short_ID","ATF","ColorCode"))

inputd <- filter(inputd, Age=="S6")

inputd$Age <- as.factor(inputd$Age)
inputd$ATF <- as.factor(inputd$ATF)
inputd$ATF<-ordered.fun(inputd,"ATF")
inputd$Force <- as.numeric(inputd$Force)

extract_stats( grouped_ggbetweenstats(
  ## arguments relevant for ggbetweenstats
  data = inputd,
  x = ATF,
  y = Force,
  grouping.var = Age))

RR.F6W <- grapher4.RR() +
  theme_classic () + labs(y="Force (N)") + theme (axis.text.x = element_blank(),
                                                  axis.title.x = element_blank(),
                                                  text = element_text(size = 7)) + ylim(.5,1.5)

RR.F6W <- plot_grid(g.a,RR.F6W,g.d2, ncol = 1, align = "v", rel_heights = c(.2, 1,.3))
RR.F6W

ggsave("graphs/RR.ForceW6.png", width = 1200, height = 1400, units = "px", dpi = 300, bg=NULL)


# S9 ----------------------------------------------------------------------

inputd<- left_join(Force.Data, 
                   Main, 
                   by=c("Short_ID","ATF","ColorCode"))

inputd <- filter(inputd, Age=="S9")


inputd$Age <- as.factor(inputd$Age)
inputd$ATF <- as.factor(inputd$ATF)
inputd$ATF<-ordered.fun(inputd,"ATF")
inputd$Force <- as.numeric(inputd$Force)

extract_stats( grouped_ggbetweenstats(
  ## arguments relevant for ggbetweenstats
  data = inputd,
  x = ATF,
  y = Force,
  grouping.var = Age))

RR.F9W <- grapher4.RR() +
  theme_classic () + labs(y="Force (N)") + theme (axis.text.x = element_blank(),
                                                  axis.title.x = element_blank(),
                                                  text = element_text(size = 7)) + ylim(0.5,1.5)

RR.F9W <- ggarrange((g.a),RR.F9W,g.d2,ncol = 1, nrow = 3,align = "v",heights = c(.2, 1,.3))

ggsave("graphs/RR.ForceW9.png", width = 1200, height = 1400, units = "px", dpi = 300, bg=NULL)

# integrator --------------------------------------------------------------

ggarrange(RR.F6W,RR.F9W,ncol = 1, nrow = 2, align = "hv")
ggarrange(RR.F6W,RR.F9W,ncol = 1, nrow = 2, align = "hv")
# Weight ------------------------------------------------------------------


# S6 ----------------------------------------------------------------------

inputd<- left_join(Force.Data, 
                   Main, 
                   by=c("Short_ID","ATF","ColorCode"))

inputd <- filter(inputd, Age=="S6")


inputd$Age <- as.factor(inputd$Age)
inputd$ATF <- as.factor(inputd$ATF)
inputd$ATF<-ordered.fun(inputd,"ATF")
inputd$Force <- as.numeric(inputd$Force)

extract_stats( grouped_ggbetweenstats(
  ## arguments relevant for ggbetweenstats
  data = inputd,
  x = ATF,
  y = Force,
  grouping.var = Age))



RR.W6W <- grapher5.RR() +
  theme_classic () + labs(y="Weight (gr)") + theme (axis.text.x = element_blank(),
                                                    axis.title.x = element_blank(),
                                                    text = element_text(size = 7)) + ylim(10,25)

RR.W6W
RR.W6W <- ggarrange((g.a),RR.W6W,g.d2,ncol = 1, nrow = 3,align = "v",heights = c(.2, 1,.3))

ggsave("graphs/RR.WeightW6.png", width = 1200, height = 1400, units = "px", dpi = 300, bg=NULL)


# S9 ----------------------------------------------------------------------

inputd<- left_join(Force.Data, 
                   Main, 
                   by=c("Short_ID","ATF","ColorCode"))

inputd <- filter(inputd, Age=="S9")


inputd$Age <- as.factor(inputd$Age)
inputd$ATF <- as.factor(inputd$ATF)
inputd$ATF<-ordered.fun(inputd,"ATF")
inputd$Force <- as.numeric(inputd$Force)

extract_stats( grouped_ggbetweenstats(
  ## arguments relevant for ggbetweenstats
  data = inputd,
  x = ATF,
  y = Force,
  grouping.var = Age))

g<- grapher5.RR()

g
A2 <- g +
  theme_classic () + labs(y="Weight (gr)") + theme (axis.text.x = element_blank(),
                                                    axis.title.x = element_blank(),
                                                    text = element_text(size = 7)) + ylim(10,25)

RR.W9W <- ggarrange((g.a),A2,g.d2,ncol = 1, nrow = 3,align = "v",heights = c(.2, 1,.3))

RR.W9W
ggsave("graphs/RR.WeightW9.png", width = 1200, height = 1400, units = "px", dpi = 300, bg=NULL)

# comp --------------------------------------------------------------------
total <- ggarrange(
  (ggarrange(
    ggarrange(RR.W6,labels = c("A"), heights = c(0.5)),
    ggarrange(RR.F6W,RR.W6W, ncols=2,align = "hv", labels = c("C","D")), nrow = 2
  )),
  (ggarrange(
    ggarrange(RR.W9,labels = c("B")),
    ggarrange(RR.F9W,RR.W9W, ncols=2,align = "hv", labels = c("E","F")), nrow = 2
  )),
  ncol=2, align = "hv"
)

total <-
  ggdraw()+
  draw_plot(RR.W6, x = 0, y = .5, width = .5, height = .47)+
  draw_plot(RR.W9, x = .5, y = .5, width = .5, height = .47)+
  
  draw_plot(RR.F6W, x = 0, y = 0, width = .25, height = .48)+
  draw_plot(RR.W6W, x = .25, y = 0, width = .25, height = .48)+
  
  draw_plot(RR.F9W, x = .5, y = 0, width = .25, height = .48)+
  draw_plot(RR.W9W, x = .75, y = 0, width = .25, height = .48)+
  
  
  draw_plot_label(label = c("A", "B","C","D","E","F"), size = 15,
                  x = c(0, .5, 0, .25, .5, .75), y = c(1, 1, .5,.5, .5, .5))

total
ggsave("graphs/RR.test.png", width = 18.6, height = 16.6, units = "cm", dpi = 300, bg=NULL)
ggsave("graphs/RR.test.png", width = 18.6, height = 18.6, units = "cm", dpi = 300, bg=NULL)

C1 <- ggarrange(RR.F6W,RR.W6W, ncol=2, nrow = 1, align = "h")
C1

# anexos ------------------------------------------------------------------

total.anexo <- 
    ggarrange(RR.W6.anexo, RR.W9.anexo, ncol = 1, heights = c(.9, .9), labels = c("A","B"),
  align = "hv")

total.anexo <-
  ggdraw()+
  draw_plot(RR.W6.anexo, x = 0, y = .51, width = 1, height = .48)+
  draw_plot(RR.W9.anexo, x = 0, y = 0, width = 1, height = .48)+
draw_plot_label(label = c("A", "B"), size = 15,
                x = c(0, 0), y = c(1, 0.5))
total.anexo
ggsave("graphs/RR.lat.Anexo.png", width = 20, height = 25, units = "cm", dpi = 300, bg=NULL)

# Funs ----------------------------------------------------------------------

grapher2.RR <- function () {
  
g<- 
  ggplot(inputd,aes(x= ATF, y=time))+
  geom_boxplot(width=0.3, fill=NA,linewidth = 0.1, size=0, varwidth = FALSE, outlier.alpha = 0)+
  geom_point(inherit.aes = TRUE,
    colour   = inputd$ColorCode,
    fill     = inputd$ColorCode,
    shape    = inputd$Symbol,
    alpha    = 0.5, 
    size     = 2, 
    position = position_jitter(width = 0.1, seed = 1)) +
  theme_classic () + labs(y="Latency to fall (s)") + theme (axis.line.y = element_blank(),
                                                              axis.title.y = element_blank(),
                                                              axis.text.y = element_blank(),
                                                              axis.ticks.y = element_blank(),
                                                              axis.text.x = element_blank(),
                                                              axis.title.x = element_blank(),
                                                              text = element_text(size = 5))+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 400))
  
  if(length(graph.sts$groups)!=0) {
    g <- g + geom_signif(
      comparisons = graph.sts$groups,
      map_signif_level = TRUE,
      annotations = graph.sts$asterisk_label,
      textsize    = 3,
      y_position  = 233,
      step_increase = 0.17)
    return(g)
  } else {
    return (g)
  }
  
}
##

grapher4.RR <- function () {
  
  
  g<- ggplot(inputd,aes(x= ATF, y=Force))+
    geom_boxplot(width=0.3, fill=NA,linewidth = 0.1, size=0, varwidth = FALSE, outlier.alpha = 0)+
    stat_summary(aes(group=ATF), data = inputd,  fun =median, color = "black", geom = "line", size=0.5, alpha=0.2) +
    geom_point(inherit.aes = TRUE,
               colour   = inputd$ColorCode,
               fill     = inputd$ColorCode,
               shape    = inputd$Symbol,
               alpha    = 0.5, 
               size     = 2, 
               position = position_jitter(width = 0.1, seed = 1)) 
  return(g)
}

grapher5.RR <- function () {
  
  
  g<- ggplot(inputd,aes(x= ATF, y=Weight))+
    geom_boxplot(width=0.3, fill=NA,linewidth = 0.1, size=0, varwidth = FALSE, outlier.alpha = 0)+
    stat_summary(aes(group=ATF), data = inputd,  fun =median, color = "black", geom = "line", size=0.5, alpha=0.2) +
    geom_point(inherit.aes = TRUE,
               colour   = inputd$ColorCode,
               fill     = inputd$ColorCode,
               shape    = inputd$Symbol,
               alpha    = 0.5, 
               size     = 2, 
               position = position_jitter(width = 0.1, seed = 1))
    
    return(g)
}

grapher6.RR <- function (input.x) {
  
g<- 
  ggplot(inputd,aes(x= ATF, y=time))+
  geom_boxplot(width=0.3, fill=NA,linewidth = 0.1, size=0, varwidth = FALSE, outlier.alpha = 0)+
  geom_point(inherit.aes = TRUE,
    colour   = inputd$ColorCode,
    fill     = inputd$ColorCode,
    shape    = inputd$Symbol,
    alpha    = 0.5, 
    size     = 2, 
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
  theme_classic () + labs(y="Latency to fall (s)") + theme (axis.line.y = element_blank(),
                                                              axis.title.y = element_blank(),
                                                              axis.text.y = element_blank(),
                                                              axis.ticks.y = element_blank(),
                                                              axis.text.x = element_blank(),
                                                              axis.title.x = element_blank(),
                                                              text = element_text(size = 5))+
    scale_y_continuous(expand = c(0, 0), limits = c(0, 390))###
  
  if(length(graph.sts$groups)!=" ") {
    g <- g + geom_signif(
      comparisons = graph.sts$groups,
      map_signif_level = TRUE,
      annotations = as.character(graph.sts$expression),
      textsize    = 2,
      y_position  = (max(inputd$time)+.5),
      step_increase = 0.17,
      
      parse = TRUE)
    return(g)
  } else {
    return (g)
  }
  
}
