# Training ----------------------------------------------------------------

# NOROLT.Training.7wks.graph -------------------------------------------------------
inputd<- left_join(filter(NOROLT.Data, Test=="Training"), 
                   filter(Main, ExpCode_5=="BO07"), 
                   by=c("Short_ID","ATF","ColorCode"))



level.check.fun(inputd,"ATF")
inputd$ATF<-ordered.fun(inputd,"ATF")

## Estadisticas descriptivas
group_by(inputd, ATF) %>%
  summarise(
    count = n(),
    mean  = mean(index, na.rm = TRUE), #dif
    sd    = sd(index, na.rm = TRUE) #dif
  )
## normalidad
ggqqplot(inputd$d_ddCq)
shapiro.test(inputd$d_ddCq)
## lavene test
car::leveneTest(d_ddCq ~ (as.factor(ATF)), data = inputd) #<- no parametricos
bartlett.test(d_ddCq ~ (as.factor(ATF)), data = inputd) # en ambos, si el p>0.05 entonces las varianzas son homogeneas

b<-
  ggbetweenstats(               #dif
    data = inputd,             ##Dataframe
    x = ATF,                             ## grupo de x
    y = index,
    type="p")

extract_stats(b)

graph.sts<-filter((sigfltr.ppairwise.fun()), p.value<=0.05)

g<-
ggplot(inputd,aes(x= ATF, y=index))+
  geom_boxplot(width=0.3, fill=NA,linewidth = 0.1, size=0, varwidth = FALSE, outlier.alpha = 0)+
  geom_point(inherit.aes = TRUE,
             colour   = inputd$ColorCode,
             fill     = inputd$ColorCode,
             shape    = inputd$Symbol,
             alpha    = 0.5, 
             size     = 3, 
             position = position_jitter(width = 0.1, seed = 1)) +
  geom_line(aes(group = Short_ID),
            colour   = inputd$ColorCode,
            alpha    = 0.5, 
            position = position_jitter(width = 0.1, seed = 1)
  )+
  geom_hline(yintercept=0.5,
             linetype="dashed", color = "grey")+
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
    y = "Recognition Index")+
  theme_classic () + plot.theme.box + scale_y_continuous(breaks=c(0, 0.2, 0.4, 0.6, 0.8, 1), expand = c(0, 0), limits = c(0, 1.2)) + 
  scale_x_discrete(labels=(paste(levels(inputd$ATF),"\n(n=",table(inputd$ATF),")",sep="")))
g
# # NOROLT.Training.7wks.graph.save --------------------------------------------------------------------
extract_stats(b)
ggsave("graphs/NOROLT.Training.graph.png", width = 1200, height = 1400, units = "px", dpi = 300, bg=NULL)

# OLT ---------------------------------------------------------------------

# # NOROLT.OLT.7wks.graph -------------------------------------------------------
inputd<- left_join(filter(NOROLT.Data, Test=="OLT"), 
                   filter(Main, ExpCode_5=="BO07"), 
                   by=c("Short_ID","ATF","ColorCode"))

level.check.fun(inputd,"ATF")
inputd$ATF<-ordered.fun(inputd,"ATF")

## Estadisticas descriptivas
group_by(inputd, ATF) %>%
  summarise(
    count = n(),
    mean  = mean(index, na.rm = TRUE), #dif
    sd    = sd(index, na.rm = TRUE) #dif
  )
## normalidad
ggqqplot(inputd$index)
shapiro.test(inputd$index)
## lavene test
car::leveneTest(index ~ (as.factor(ATF)), data = inputd) #<- no parametricos
bartlett.test(index ~ (as.factor(ATF)), data = inputd) # en ambos, si el p>0.05 entonces las varianzas son homogeneas

b<-
  ggbetweenstats(               #dif
    data = inputd,             ##Dataframe
    x = ATF,                             ## grupo de x
    y = index,
    type="p")
b
extract_stats(b)

graph.sts<-filter((sigfltr.ppairwise.fun()), p.value<=0.05)
graph.sts

ggplot(inputd,aes(x= ATF, y=index))+
  geom_boxplot(width=0.3, fill=NA,linewidth = 0.1, size=0, varwidth = FALSE, outlier.alpha = 0)+
  geom_point(inherit.aes = TRUE,
             colour   = inputd$ColorCode,
             fill     = inputd$ColorCode,
             shape    = inputd$Symbol,
             alpha    = 0.5, 
             size     = 3, 
             position = position_jitter(width = 0.1, seed = 1)) +
  geom_line(aes(group = Short_ID),
            colour   = inputd$ColorCode,
            alpha    = 0.5, 
            position = position_jitter(width = 0.1, seed = 1)
  )+
  geom_hline(yintercept=0.5,
             linetype="dashed", color = "grey")+
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
    y = "Recognition Index")+
  theme_classic () + plot.theme.box + scale_y_continuous(breaks=c(0, 0.2, 0.4, 0.6, 0.8, 1), expand = c(0, 0), limits = c(0, 1.2)) + 
  scale_x_discrete(labels=(paste(levels(inputd$ATF),"\n(n=",table(inputd$ATF),")",sep="")))

# # NOROLT.OLT.7wks.graph.save --------------------------------------------------------------------
extract_stats(b)
ggsave("graphs/NOROLT.OLT.graph.png", width = 1200, height = 1400, units = "px", dpi = 300, bg=NULL)


# NORT --------------------------------------------------------------------
# # NOROLT.NORT.7wks.graph -------------------------------------------------------
inputd<- left_join(filter(NOROLT.Data, Test=="NORT"), 
                   filter(Main, ExpCode_5=="BO07"), 
                   by=c("Short_ID","ATF","ColorCode"))

level.check.fun(inputd,"ATF")
inputd$ATF<-ordered.fun(inputd,"ATF")

## Estadisticas descriptivas
group_by(inputd, ATF) %>%
  summarise(
    count = n(),
    mean  = mean(index, na.rm = TRUE), #dif
    sd    = sd(index, na.rm = TRUE) #dif
  )
## normalidad
ggqqplot(inputd$d_ddCq)
shapiro.test(inputd$d_ddCq)
## lavene test
car::leveneTest(d_ddCq ~ (as.factor(ATF)), data = inputd) #<- no parametricos
bartlett.test(d_ddCq ~ (as.factor(ATF)), data = inputd) # en ambos, si el p>0.05 entonces las varianzas son homogeneas

b<-
  ggbetweenstats(               #dif
    data = inputd,             ##Dataframe
    x = ATF,                             ## grupo de x
    y = index,
    type="p")

extract_stats(b)

graph.sts<-filter((sigfltr.ppairwise.fun()), p.value<=0.05)

ggplot(inputd,aes(x= ATF, y=index))+
  geom_boxplot(width=0.3, fill=NA,linewidth = 0.1, size=0, varwidth = FALSE, outlier.alpha = 0)+
  geom_point(inherit.aes = TRUE,
             colour   = inputd$ColorCode,
             fill     = inputd$ColorCode,
             shape    = inputd$Symbol,
             alpha    = 0.5, 
             size     = 3, 
             position = position_jitter(width = 0.1, seed = 1)) +
  geom_line(aes(group = Short_ID),
            colour   = inputd$ColorCode,
            alpha    = 0.5, 
            position = position_jitter(width = 0.1, seed = 1)
  )+
  geom_hline(yintercept=0.5,
             linetype="dashed", color = "grey")+
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
    y = "Recognition Index")+
  theme_classic () + plot.theme.box + scale_y_continuous(breaks=c(0, 0.2, 0.4, 0.6, 0.8, 1), expand = c(0, 0), limits = c(0, 1.2)) + 
  scale_x_discrete(labels=(paste(levels(inputd$ATF),"\n(n=",table(inputd$ATF),")",sep="")))

# # NOROLT.NORT.7wks.graph.save --------------------------------------------------------------------
extract_stats(b)
ggsave("graphs/NOROLT.NORT.graph.png", width = 1200, height = 1400, units = "px", dpi = 300, bg=NULL)



