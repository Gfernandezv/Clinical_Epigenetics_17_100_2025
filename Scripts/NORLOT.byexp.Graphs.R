
# HTTn --------------------------------------------------------------------

# NOROLT.HTTn.7wks.graph -------------------------------------------------------
inputd<- left_join(filter(NOROLT.Data, ATF=="HTT-NI"), 
                   filter(Main, ExpCode_5=="BO07"), 
                   by=c("Short_ID","ATF","ColorCode"))

level.check.fun2(inputd,"Test")
inputd$ATF<-ordered.fun2(inputd,"Test")

## Estadisticas descriptivas
group_by(inputd, ATF) %>%
  summarise(
    count = n(),
    mean  = mean(Ratio, na.rm = TRUE),
    sd    = sd(Ratio, na.rm = TRUE)
  )
## normalidad
ggqqplot(inputd$Ratio)
shapiro.test(inputd$Ratio)
## lavene test
car::leveneTest(Ratio ~ (as.factor(ATF)), data = inputd) #<- no parametricos
bartlett.test(Ratio ~ (as.factor(ATF)), data = inputd) # en ambos, si el p>0.05 entonces las varianzas son homogeneas

b<-
  ggwithinstats(
    data = inputd,             ##Dataframe
    x = Test,                             ## grupo de x
    y = index,
    type="p")

extract_stats(b)
b
graph.sts<-filter((sigfltr.ppairwise.fun(inputd,"p")), p.value<=0.05)

g <-
  ggplot(inputd,aes(Test,index))+
  geom_boxplot(width=0.3, fill=NA,linewidth = 0.1, size=0, varwidth = FALSE, outlier.alpha = 0)+
  geom_point(inherit.aes = TRUE,
             colour   = inputd$ColorCode,
             fill     = inputd$ColorCode,
             shape    = inputd$Symbol,
             alpha    = 0.5, 
             size     = 3, 
             position = position_jitter(width = 0.1, seed = 1)) +
  geom_hline(yintercept=0.5,
             linetype="dashed", color = "grey")+
  geom_line(aes(group = Short_ID),
            colour   = "grey",
            alpha    = 0.5, 
            position = position_jitter(width = 0.1, seed = 1),
            linetype = "dashed"
  )+
  ggsignif::geom_signif(
    comparisons = graph.sts$groups,
    map_signif_level = TRUE,
    annotations = as.character(graph.sts$expression),
    textsize    = 2,
    y_position  = 1,
    step_increase = 0.2,
    test        = NULL,
    na.rm       = TRUE,
    parse       = TRUE)+
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
  labs(
    subtitle = extract_subtitle(b),
    caption = extract_caption(b),
    y = "PSD95/Actin norm. ratio")+
  theme_classic () + plot.theme.box + scale_y_continuous(expand = c(0, 0), limits = c(0, 1.5))+ scale_x_discrete(limits = c("Training","OLT","NORT"))
g
#+ scale_x_discrete(limits = c("Training","OLT","NORT")

# NOROLT.HTTn.7wks.graph.save --------------------------------------------------------------------
extract_stats(b)
ggsave("graphs/NOROLT.HTTn.graph.png", width = 1200, height = 1400, units = "px", dpi = 300, bg=NULL)


# HTTp --------------------------------------------------------------------

# NOROLT.HTTp.7wks.graph -------------------------------------------------------
inputd<- left_join(filter(NOROLT.Data, ATF=="HTT+NI"), 
                   filter(Main, ExpCode_5=="BO07"), 
                   by=c("Short_ID","ATF","ColorCode"))

level.check.fun2(inputd,"Test")
inputd$ATF<-ordered.fun2(inputd,"Test")

## Estadisticas descriptivas
group_by(inputd, ATF) %>%
  summarise(
    count = n(),
    mean  = mean(Ratio, na.rm = TRUE),
    sd    = sd(Ratio, na.rm = TRUE)
  )
## normalidad
ggqqplot(inputd$Ratio)
shapiro.test(inputd$Ratio)
## lavene test
car::leveneTest(Ratio ~ (as.factor(ATF)), data = inputd) #<- no parametricos
bartlett.test(Ratio ~ (as.factor(ATF)), data = inputd) # en ambos, si el p>0.05 entonces las varianzas son homogeneas

b<-
  ggwithinstats(
    data = inputd,             ##Dataframe
    x = Test,                             ## grupo de x
    y = index,
    type="p")

extract_stats(b)
b
graph.sts<-filter((sigfltr.ppairwise.fun(inputd,"p")), p.value<=0.05)

g <-
  ggplot(inputd,aes(Test,index))+
  geom_boxplot(width=0.3, fill=NA,linewidth = 0.1, size=0, varwidth = FALSE, outlier.alpha = 0)+
  geom_point(inherit.aes = TRUE,
             colour   = inputd$ColorCode,
             fill     = inputd$ColorCode,
             shape    = inputd$Symbol,
             alpha    = 0.5, 
             size     = 3, 
             position = position_jitter(width = 0.1, seed = 1)) +
  geom_hline(yintercept=0.5,
             linetype="dashed", color = "grey")+
  geom_line(aes(group = Short_ID),
            colour   = "grey",
            alpha    = 0.5, 
            position = position_jitter(width = 0.1, seed = 1),
            linetype = "dashed"
  )+
  ggsignif::geom_signif(
    comparisons = graph.sts$groups,
    map_signif_level = TRUE,
    annotations = as.character(graph.sts$expression),
    textsize    = 2,
    y_position  = 1.2,
    step_increase = 0.1,
    test        = NULL,
    na.rm       = TRUE,
    parse       = TRUE)+
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
  labs(
    subtitle = extract_subtitle(b),
    caption = extract_caption(b),
    y = "PSD95/Actin norm. ratio")+
  theme_classic () + plot.theme.box + scale_y_continuous(expand = c(0, 0), limits = c(0, 1.5))+ scale_x_discrete(limits = c("Training","OLT","NORT"))
g
#+ scale_x_discrete(limits = c("Training","OLT","NORT")
ggsignif::geom_signif(
  comparisons = graph.sts$groups,
  map_signif_level = TRUE,
  annotations = as.character(graph.sts$expression),
  textsize    = 2,
  y_position  = 1.5,
  step_increase = 0.1,
  test        = NULL,
  na.rm       = TRUE,
  parse       = TRUE)+
  # NOROLT.HTTn.7wks.graph.save --------------------------------------------------------------------
extract_stats(b)
ggsave("graphs/NOROLT.HTTp.graph.png", width = 1200, height = 1400, units = "px", dpi = 300, bg=NULL)

# HTT-VP64 --------------------------------------------------------------------

# NOROLT.VP64.7wks.graph -------------------------------------------------------
inputd<- left_join(filter(NOROLT.Data, ATF=="PSD95-6ZF-VP64"), 
                   filter(Main, ExpCode_5=="BO07"), 
                   by=c("Short_ID","ATF","ColorCode"))

level.check.fun2(inputd,"Test")
inputd$ATF<-ordered.fun2(inputd,"Test")

## Estadisticas descriptivas
group_by(inputd, ATF) %>%
  summarise(
    count = n(),
    mean  = mean(Ratio, na.rm = TRUE),
    sd    = sd(Ratio, na.rm = TRUE)
  )
## normalidad
ggqqplot(inputd$Ratio)
shapiro.test(inputd$Ratio)
## lavene test
car::leveneTest(Ratio ~ (as.factor(ATF)), data = inputd) #<- no parametricos
bartlett.test(Ratio ~ (as.factor(ATF)), data = inputd) # en ambos, si el p>0.05 entonces las varianzas son homogeneas

b<-
  ggwithinstats(
    data = inputd,             ##Dataframe
    x = Test,                             ## grupo de x
    y = index,
    type="p")

extract_stats(b)
b
graph.sts<-filter((sigfltr.ppairwise.fun(inputd,"p")), p.value<=0.05)

g <-
  ggplot(inputd,aes(Test,index))+
  geom_boxplot(width=0.3, fill=NA,linewidth = 0.1, size=0, varwidth = FALSE, outlier.alpha = 0)+
  geom_point(inherit.aes = TRUE,
             colour   = inputd$ColorCode,
             fill     = inputd$ColorCode,
             shape    = inputd$Symbol,
             alpha    = 0.5, 
             size     = 3, 
             position = position_jitter(width = 0.1, seed = 1)) +
  geom_hline(yintercept=0.5,
             linetype="dashed", color = "grey")+
  geom_line(aes(group = Short_ID),
            colour   = "grey",
            alpha    = 0.5, 
            position = position_jitter(width = 0.1, seed = 1),
            linetype = "dashed"
  )+
  ggsignif::geom_signif(
    comparisons = graph.sts$groups,
    map_signif_level = TRUE,
    annotations = as.character(graph.sts$expression),
    textsize    = 2,
    y_position  = 1.2,
    step_increase = 0.3,
    test        = NULL,
    na.rm       = TRUE,
    parse       = TRUE)+
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
  labs(
    subtitle = extract_subtitle(b),
    caption = extract_caption(b),
    y = "PSD95/Actin norm. ratio")+
  theme_classic () + plot.theme.box + scale_y_continuous(expand = c(0, 0), limits = c(0, 1.5))+ scale_x_discrete(limits = c("Training","OLT","NORT"))
g
#+ scale_x_discrete(limits = c("Training","OLT","NORT")
ggsignif::geom_signif(
  comparisons = graph.sts$groups,
  map_signif_level = TRUE,
  annotations = as.character(graph.sts$expression),
  textsize    = 2,
  y_position  = 1.5,
  step_increase = 0.1,
  test        = NULL,
  na.rm       = TRUE,
  parse       = TRUE)+
  # NOROLT.VP64.7wks.graph.save --------------------------------------------------------------------
extract_stats(b)
ggsave("graphs/NOROLT.VP64.graph.png", width = 1200, height = 1400, units = "px", dpi = 300, bg=NULL)

# HTT-NoED --------------------------------------------------------------------

# NOROLT.NoED.7wks.graph -------------------------------------------------------
inputd<- left_join(filter(NOROLT.Data, ATF=="PSD95-6ZF-NoED"), 
                   filter(Main, ExpCode_5=="BO07"), 
                   by=c("Short_ID","ATF","ColorCode"))

level.check.fun2(inputd,"Test")
inputd$ATF<-ordered.fun2(inputd,"Test")

## Estadisticas descriptivas
group_by(inputd, ATF) %>%
  summarise(
    count = n(),
    mean  = mean(Ratio, na.rm = TRUE),
    sd    = sd(Ratio, na.rm = TRUE)
  )
## normalidad
ggqqplot(inputd$Ratio)
shapiro.test(inputd$Ratio)
## lavene test
car::leveneTest(Ratio ~ (as.factor(ATF)), data = inputd) #<- no parametricos
bartlett.test(Ratio ~ (as.factor(ATF)), data = inputd) # en ambos, si el p>0.05 entonces las varianzas son homogeneas

b<-
  ggwithinstats(
    data = inputd,             ##Dataframe
    x = Test,                             ## grupo de x
    y = index,
    type="p")

extract_stats(b)
b
graph.sts<-filter((sigfltr.ppairwise.fun(inputd,"p")), p.value<=0.05)

g <-
  ggplot(inputd,aes(Test,index))+
  geom_boxplot(width=0.3, fill=NA,linewidth = 0.1, size=0, varwidth = FALSE, outlier.alpha = 0)+
  geom_point(inherit.aes = TRUE,
             colour   = inputd$ColorCode,
             fill     = inputd$ColorCode,
             shape    = inputd$Symbol,
             alpha    = 0.5, 
             size     = 3, 
             position = position_jitter(width = 0.1, seed = 1)) +
  geom_hline(yintercept=0.5,
             linetype="dashed", color = "grey")+
  geom_line(aes(group = Short_ID),
            colour   = "grey",
            alpha    = 0.5, 
            position = position_jitter(width = 0.1, seed = 1),
            linetype = "dashed"
  )+
  ggsignif::geom_signif(
    comparisons = graph.sts$groups,
    map_signif_level = TRUE,
    annotations = as.character(graph.sts$expression),
    textsize    = 2,
    y_position  = 1.2,
    step_increase = 0.3,
    test        = NULL,
    na.rm       = TRUE,
    parse       = TRUE)+
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
  labs(
    subtitle = extract_subtitle(b),
    caption = extract_caption(b),
    y = "PSD95/Actin norm. ratio")+
  theme_classic () + plot.theme.box + scale_y_continuous(expand = c(0, 0), limits = c(0, 1.5))+ scale_x_discrete(limits = c("Training","OLT","NORT"))
g
#+ scale_x_discrete(limits = c("Training","OLT","NORT")
ggsignif::geom_signif(
  comparisons = graph.sts$groups,
  map_signif_level = TRUE,
  annotations = as.character(graph.sts$expression),
  textsize    = 2,
  y_position  = 1.5,
  step_increase = 0.1,
  test        = NULL,
  na.rm       = TRUE,
  parse       = TRUE)+
  # NOROLT.VP64.7wks.graph.save --------------------------------------------------------------------
extract_stats(b)
ggsave("graphs/NOROLT.NoED.graph.png", width = 1200, height = 1400, units = "px", dpi = 300, bg=NULL)
