# 7wks.graph -------------------------------------------------------
inputd<-left_join((filter(WB.inj.Data, Age == 7)), 
                  (filter(Main, ExpCode_2=="WB07")), 
                  by=c("Short_ID","ATF","ColorCode"))

level.check.fun(inputd,"ATF")
inputd$ATF<-ordered.fun(inputd,"ATF")

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

df <-
  pairwise_comparisons(inputd, ATF, Ratio) %>%
  dplyr::mutate(groups = purrr::pmap(.l = list(group1, group2), .f = c)) %>%
  dplyr::arrange(group1) %>%
  dplyr::mutate(asterisk_label = starmaker(df$p.value, p.levels=c(.001, .01, .05, .1), symbols=c("***", "**", "*", "")))

graph.sts<-filter(df, p.value<=.05)
df

g.b <-
  ggplot(inputd,aes(ATF,Ratio))+
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
  ggsignif::geom_signif(
    comparisons = graph.sts$groups,
    map_signif_level = TRUE,
    annotations = graph.sts$asterisk_label,
    textsize    = 5,
    y_position  = 1.6,
    step_increase = 0.2)+
  xlab("")+
  theme_classic () + plot.theme.box + 
  theme (axis.text.x= element_blank(),
         axis.line = element_line(colour = "black",size = .5, linetype = "solid"),
         panel.background = element_blank(),
         axis.text.y = element_text(size = 12),
         axis.title.y = element_blank()) +
  scale_y_continuous(breaks=c(0,.5,1,1.5,2,2.5),
                     expand = expansion(mult = c(0, 0.1)), limits = c(0, 2.5)) 
g.b

WB.7wks.figure <-
          ggarrange(g.a,
          g.b,
          g.d+ theme(axis.text.y = element_text(size = 9)),
          ncol = 1, nrow = 4,  
          align = "v",
          heights = c(.2,2,0.7))

WB.7wks.graph <-
  annotate_figure(WB.7wks.figure, top=text_grob("7 Weeks", size = 14, hjust  = 0),
                  left = text_grob("PSD95/β-Actin norm. ratio", rot = 90, hjust=0.3, x = 2))

ggsave("graphs/WB7.graph1.png", width = 1200, height = 1500, units = "px", dpi = 300, bg=NULL)

# 14wks.graph -------------------------------------------------------
inputd<-left_join((filter(WB.inj.Data, Age == 14)), 
                  (filter(Main, ExpCode_2=="WB14")), 
                  by=c("Short_ID","ATF","ColorCode"))

level.check.fun(inputd,"ATF")
inputd$ATF<-ordered.fun(inputd,"ATF")

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

df <-
  pairwise_comparisons(inputd, ATF, Ratio, p.adjust.method = "none") %>%
  dplyr::mutate(groups = purrr::pmap(.l = list(group1, group2), .f = c)) %>%
  dplyr::arrange(group1) %>%
  dplyr::mutate(asterisk_label = starmaker(df$p.value, p.levels=c(.001, .01, .05, .1), symbols=c("***", "**", "*", "")))

graph.sts<-filter(df, p.value<=.05)

g.b <-
  ggplot(inputd,aes(ATF,Ratio))+
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
  ggsignif::geom_signif(
    comparisons = graph.sts$groups,
    map_signif_level = TRUE,
    annotations = graph.sts$asterisk_label,
    textsize    = 5,
    y_position  = 1.5,
    step_increase = 0.2)+
  xlab("")+
  theme_classic () + plot.theme.box + 
  theme (axis.text.x= element_blank(),
         axis.line = element_line(colour = "black",size = .5, linetype = "solid"),
         panel.background = element_blank(),
         axis.text.y = element_text(size = 12),
         axis.title.y = element_blank()) +
  scale_y_continuous(breaks=c(0,.5,1,1.5,2,2.5),
                     expand = expansion(mult = c(0, 0.1)), limits = c(0, 2.5)) 
g.b

WB.14wks.figure <-
  ggarrange(g.a,
            g.b,
            g.d+ theme(axis.text.y = element_text(size = 9)),
            ncol = 1, nrow = 4,  
            align = "v",
            heights = c(.2,2,0.7))

WB.14wks.graph <-
  annotate_figure(WB.14wks.figure, top=text_grob("14 Weeks", size = 14, hjust  = 0),
                  left = text_grob("PSD95/β-Actin norm. ratio", rot = 90, hjust=0.3, x = 2))

ggsave("graphs/WB14.graph.png", width = 1200, height = 1500, units = "px", dpi = 300, bg=NULL)

# 14wks.graph.save --------------------------------------------------------------------

ggsave("graphs/WB14.graph1.png", width = 1200, height = 1400, units = "px", dpi = 300, bg=NULL)

# Table -------------------------------------------------------------------
inputd<-WB.inj.Data
RR.data.list <- list((inputd %>%  group_by(ATF) %>%  
  summarise(count.1=sum(Age == 7), 
  Mean.1=mean(Ratio[Age == 7]), 
  SD.1=sd(Ratio[Age == 7]),
  SE.1= (sd(Ratio[Age == 7]))/(sqrt(sum(Age == 7))))
), 
(inputd %>%  group_by(ATF) %>%  
   summarise(count.1=sum(Age == 14), 
  Mean.1=mean(Ratio[Age == 14]), 
  SD.1=sd(Ratio[Age == 14]),
  SE.1= (sd(Ratio[Age == 14]))/(sqrt(sum(Age == 14))))
)
)

kbl((RR.data.list %>% reduce(inner_join, by='ATF')), align = "c", digits = 2, 
  col.names = c("ATF","n","Mean (s)","SD","SE",
                      "n","Mean (s)","SD","SE"),booktabs =TRUE) %>%
  kable_classic(full_width = T, font_size = 26) %>% 
  add_header_above(c(" "=1, "Week 7"=4,"Week 14"=4)) #%>% 
  as_image(height = 12, file = "graphs/wks6.table.stats.png")

# WB.ANEXO.7wks.graph -------------------------------------------------------
inputd<-left_join((filter(WB.inj.Data, Age == 7)), 
                  (filter(Main, ExpCode_2=="WB07")), 
                  by=c("Short_ID","ATF","ColorCode"))

level.check.fun(inputd,"ATF")
inputd$ATF<-ordered.fun(inputd,"ATF")

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

b.WB7Anexo<-
  ggwithinstats(
    data = inputd,             ##Dataframe
    x = ATF,                             ## grupo de x
    y = Ratio,
    type="p")

extract_stats(b.WB7Anexo)

graph.sts<-filter((sigfltr.ppairwise.fun(inputd,"p")), p.value<=0.05)

g <-
ggplot(inputd,aes(ATF,Ratio))+
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
    y = "PSD95/Actin norm. ratio", x="")+
  theme_classic () + plot.theme.box + 
  theme (axis.text.x= element_blank(),
         axis.line = element_line(colour = "black",size = .5, linetype = "solid"),
         panel.background = element_blank() )
g

Anexus.WB7<-ggarrange(g.a,
                          g,
                          g.d,
                          ncol = 1, 
                          nrow = 3,  
                          align = "v",
                          heights = c(.5,2,0.5))
Anexus.WB7
annotate_figure(Anexus.WB7,
                top = text_grob(extract_subtitle(b.WB7Anexo), face = "bold", size = 7, hjust = 1, x=1, y=0),
                bottom = text_grob(extract_caption(b.WB7Anexo), hjust = 1, x = 1, y=1, face = "bold", size = 7))

ggsave("graphs/test.png", width = 1200, height = 1400, units = "px", dpi = 300, bg=NULL)


# WB.inj.14wks.graph -------------------------------------------------------
inputd<-left_join((filter(WB.inj.Data, Age == 14)), 
                  (filter(Main, ExpCode_2=="WB14")), 
                  by=c("Short_ID","ATF","ColorCode"))

level.check.fun(inputd,"ATF")
inputd$ATF<-ordered.fun(inputd,"ATF")

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

b.WB14Anexo<-
  ggwithinstats(
    data = inputd,             ##Dataframe
    x = ATF,                             ## grupo de x
    y = Ratio,
    type="p")

extract_stats(b.WB14Anexo)

graph.sts<-filter((sigfltr.ppairwise.fun(inputd,"p")), p.value<=0.05)

g <-
  ggplot(inputd,aes(ATF,Ratio))+
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
    y = "PSD95/Actin norm. ratio", x="")+
  theme_classic () + plot.theme.box + 
  theme (axis.text.x= element_blank(),
         axis.line = element_line(colour = "black",size = .5, linetype = "solid"),
         panel.background = element_blank() )
g

Anexus.WB14<-ggarrange(g.a,
                      g,
                      g.d,
                      ncol = 1, 
                      nrow = 3,  
                      align = "v",
                      heights = c(.5,2,0.5))
Anexus.WB14
annotate_figure(Anexus.WB14,
                top = text_grob(extract_subtitle(b.WB14Anexo), face = "bold", size = 7, hjust = 1, x=1, y=0),
                bottom = text_grob(extract_caption(b.WB14Anexo), hjust = 1, x = 1, y=1, face = "bold", size = 7))

ggsave("graphs/test.png", width = 1200, height = 1400, units = "px", dpi = 300, bg=NULL)

ggarrange(
  (annotate_figure(Anexus.WB7,
                   top = text_grob(extract_subtitle(b.WB7Anexo), face = "bold", size = 7, hjust = 1, x=1, y=-.5),
                   bottom = text_grob(extract_caption(b.WB7Anexo), hjust = 1, x = 1, y=1, face = "bold", size = 7))),
  (annotate_figure(Anexus.WB14,
                   top = text_grob(extract_subtitle(b.WB14Anexo), face = "bold", size = 7, hjust = 1, x=1, y=-.5),
                   bottom = text_grob(extract_caption(b.WB14Anexo), hjust = 1, x = 1, y=1, face = "bold", size = 7))),
  ncol = 2, labels = c("A","B")
)
ggsave("graphs/WB.Anexo.png", width = 2400, height = 1400, units = "px", dpi = 300, bg=NULL)
