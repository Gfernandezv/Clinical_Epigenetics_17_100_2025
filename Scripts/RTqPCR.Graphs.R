# 7wks.graph -------------------------------------------------------
inputd<- left_join(filter(RTqPCR.Data, Age == 7), 
                   filter(Main, ExpCode_1=="QP07"), 
                   by=c("Short_ID","ATF","ColorCode"))

level.check.fun(inputd,"ATF")
inputd$ATF<-ordered.fun(inputd,"ATF")
colnames(inputd)[16]="d_ddCq"         #dif

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

RTqPCR.7wks.sts <-
  pairwise_comparisons(inputd, ATF, d_ddCq) 
RTqPCR.7wks.sts <- RTqPCR.7wks.sts %>%
  dplyr::mutate(groups = purrr::pmap(.l = list(group1, group2), .f = c)) %>%
  dplyr::arrange(group1) %>%
  dplyr::mutate(asterisk_label = starmaker(RTqPCR.7wks.sts$p.value, p.levels=c(.001, .01, .05, .1), symbols=c("***", "**", "*", "")))
RTqPCR.7wks.sts
graph.sts<-filter(RTqPCR.7wks.sts, p.value<=.05)

RTqPCR.7wks.des.sts<- two_sample_test(inputd, ATF, index, paired = TRUE) %>%
  dplyr::mutate(asterisk_label = starmaker(OLT.WT.sts$p.value, p.levels=c(.001, .01, .05, .1), symbols=c("***", "**", "*", "")))
RTqPCR.7wks.des.sts

RTqPCR.7wks.g <-
ggplot(inputd,aes(ATF,d_ddCq))+
geom_boxplot(width=0.3, fill=NA,linewidth = 0.1, size=0, varwidth = FALSE, outlier.alpha = 0)+
# Se utiliza en la tesis para el contorno de las figuras distintas.
# geom_point(inherit.aes = TRUE,
#   colour   = inputd$ColorCode,
#   shape    = 1,
#   alpha    = 0.5, 
#   size     = 5, 
#   position = position_jitter(width = 0.1, seed = 1)) +
# para el paper se utiliza solo el simbolo circulo, para la tesis se utiliza la columna Symbol (inputd$Symbol). En las figuras accesorias se incluirá con simbolos
geom_point(
  colour   = "black",
  fill     = inputd$ColorCode,
  alpha    = 0.7,
  shape    = 21,
  size     = 4,
  position = position_jitter(width = 0.1, seed = 1)) +
ggsignif::geom_signif(
    comparisons = graph.sts$groups,
    map_signif_level = TRUE,
    annotations = graph.sts$asterisk_label,
    size = 1,
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
RTqPCR.7wks.g
figure <- ggarrange(g.a,
                    RTqPCR.7wks.g,
            g.d+ theme(axis.text.y = element_text(size = 12)),
            ncol = 1, nrow = 4,  
            align = "v",
            heights = c(.2,2,0.7))

RTqPCR.7wks.graph <-
  annotate_figure(figure, top=text_grob("7 Weeks", size = 14, hjust  = 0),
                  left = text_grob(bquote("2"^"-ΔΔCq"), rot = 90, size = 12, hjust=0, x = 2))


# 7wks.cq -----------------------------------------------------------------
# se incluyen calculos y graficos de los valores de Cq debido a observaciones de la comisión.
RTqPCR.Cq.7wks.sts <-
  pairwise_comparisons(inputd, ATF, dCq, p.adjust.method="holm") 
RTqPCR.Cq.7wks.sts <- RTqPCR.Cq.7wks.sts %>%
  dplyr::mutate(groups = purrr::pmap(.l = list(group1, group2), .f = c)) %>%
  dplyr::arrange(group1) %>%
  dplyr::mutate(asterisk_label = starmaker(RTqPCR.Cq.7wks.sts$p.value, p.levels=c(.001, .01, .05, .1), symbols=c("***", "**", "*", "")))
RTqPCR.Cq.7wks.sts
graph.Cq.7wks.sts<-filter(RTqPCR.Cq.7wks.sts, p.value<=.05)

RTqPCR.Cq.7wks.g <-
  ggplot(inputd,aes(ATF,dCq))+
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
    comparisons = graph.Cq.7wks.sts$groups,
    map_signif_level = TRUE,
    annotations = graph.Cq.7wks.sts$asterisk_label,
    textsize    = 5,
    y_position  = 5.3,
    step_increase = 0.2)+
  xlab("")+
  theme_classic () + plot.theme.box + 
  theme (axis.text.x= element_blank(),
         axis.line = element_line(colour = "black",size = .5, linetype = "solid"),
         axis.text.y = element_text(size = 10),
         axis.title.y = element_text(size = 10),
         panel.background = element_blank(),
         axis.title.y.left = element_blank()) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)), limits = c(2.5, 8))
RTqPCR.Cq.7wks.g
figure.Cq.7wks.g <-
  ggarrange(g.a,
            RTqPCR.Cq.7wks.g,
            g.d+ theme(axis.text.y = element_text(size = 8)),
            ncol = 1, nrow = 4,  
            align = "v",
            heights = c(.3,1.7,0.5))

RTqPCR.Cq.7wks.graph <-
  annotate_figure(figure.Cq.7wks.g, top=text_grob("7 Weeks", size = 14, hjust  = 0),
                  left = text_grob(bquote("ΔCq"), rot = 90, hjust=0, x = 2))



# 7wks.graph.save --------------------------------------------------------------------

ggsave("graphs/RTqPCR7.graph1.png", width = 900, height = 1400, units = "px", dpi = 300, bg=NULL)


# 14wks.graph -------------------------------------------------------
#cargar datos
inputd<- left_join(filter(RTqPCR.Data, Age == 14), 
                   filter(Main, ExpCode_1=="QP14"), 
                   by=c("Short_ID","ATF","ColorCode"))

level.check.fun(inputd,"ATF")
inputd$ATF<-ordered.fun(inputd,"ATF")
colnames(inputd)[16]="d_ddCq"         #dif

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

RTqPCR.14wks.sts <-
  pairwise_comparisons(inputd, ATF, d_ddCq, p.adjust.method = "none") 
RTqPCR.14wks.sts <- RTqPCR.14wks.sts %>%
  dplyr::mutate(groups = purrr::pmap(.l = list(group1, group2), .f = c)) %>%
  dplyr::arrange(group1) %>%
  dplyr::mutate(asterisk_label = starmaker(RTqPCR.14wks.sts$p.value, p.levels=c(.001, .01, .05, .1), symbols=c("***", "**", "*", "")))
RTqPCR.14wks.sts

graph.sts<-filter(RTqPCR.14wks.sts, p.value<=.05)

RTqPCR.14wks.g <-
  ggplot(inputd,aes(ATF,d_ddCq))+
  geom_boxplot(width=0.3, fill=NA,linewidth = 0.1, size=0, varwidth = FALSE, outlier.alpha = 0)+
  # Se utiliza en la tesis para el contorno de las figuras distintas.
  # geom_point(inherit.aes = TRUE,
  #   colour   = inputd$ColorCode,
  #   shape    = 1,
  #   alpha    = 0.5, 
  #   size     = 5, 
  #   position = position_jitter(width = 0.1, seed = 1)) +
  # para el paper se utiliza solo el simbolo circulo, para la tesis se utiliza la columna Symbol (inputd$Symbol). En las figuras accesorias se incluirá con simbolos
  geom_point(
    colour   = "black",
    fill     = inputd$ColorCode,
    alpha    = 0.7,
    shape    = 21,
    size     = 4,
    position = position_jitter(width = 0.1, seed = 1)) +
  ggsignif::geom_signif(
    comparisons = graph.sts$groups,
    map_signif_level = TRUE,
    annotations = graph.sts$asterisk_label,
    size = 1,
    textsize    = 5,
    y_position  = 1.6,
    step_increase = 0.4)+
  xlab("")+
  theme_classic () + plot.theme.box + 
  theme (axis.text.x= element_blank(),
         axis.line = element_line(colour = "black",size = .5, linetype = "solid"),
         panel.background = element_blank(),
         axis.text.y = element_text(size = 12),
         axis.title.y = element_blank()) +
  scale_y_continuous(breaks=c(0,.5,1,1.5,2,2.5),
                     expand = expansion(mult = c(0, 0.1)), limits = c(0, 2.5)) 
RTqPCR.14wks.g
figure <- ggarrange(g.a,
                    RTqPCR.14wks.g,
                    g.d+ theme(axis.text.y = element_text(size = 12)),
                    ncol = 1, nrow = 4,  
                    align = "v",
                    heights = c(.2,2,0.7))

RTqPCR.14wks.graph <-
  annotate_figure(figure, top=text_grob("14 Weeks", size = 14, hjust  = 0),
                  left = text_grob(bquote("2"^"-ΔΔCq"), rot = 90, size = 12, hjust=0, x = 2))

# 14wks.Cq ----------------------------------------------------------------

RTqPCR.Cq.14wks.sts <-
  pairwise_comparisons(inputd, ATF, dCq, p.adjust.method = "none") 
RTqPCR.Cq.14wks.sts <- RTqPCR.Cq.14wks.sts %>%
  dplyr::mutate(groups = purrr::pmap(.l = list(group1, group2), .f = c)) %>%
  dplyr::arrange(group1) %>%
  dplyr::mutate(asterisk_label = starmaker(RTqPCR.Cq.14wks.sts$p.value, p.levels=c(.001, .01, .05, .1), symbols=c("***", "**", "*", "")))
RTqPCR.Cq.14wks.sts

graph.Cq.14wks.sts<-filter(RTqPCR.Cq.14wks.sts, p.value<=.05)

RTqPCR.Cq.14wks.g <-
  ggplot(inputd,aes(ATF,dCq))+
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
    comparisons = graph.Cq.14wks.sts$groups,
    map_signif_level = TRUE,
    annotations = graph.Cq.14wks.sts$asterisk_label,
    textsize    = 5,
    y_position  = 5.5,
    step_increase = 0.5) +
  xlab("")+
  theme_classic () + plot.theme.box + 
  theme (axis.text.x= element_blank(),
         axis.line = element_line(colour = "black",size = .5, linetype = "solid"),
         panel.background = element_blank(),
         axis.text.y = element_text(size = 10),
         axis.title.y = element_blank()) +
  scale_y_continuous(name=bquote("ΔCq"),
                     expand = expansion(mult = c(0, 0.1)), limits = c(3, 6.5)) 
RTqPCR.Cq.14wks.g 
figure.14wks.Cq<-
  
  ggarrange(g.a,
            RTqPCR.Cq.14wks.g,
            g.d+ theme(axis.text.y = element_text(size = 8)),
            ncol = 1, nrow = 4,  
            align = "v",
            heights = c(.3,1.7,0.5))

RTqPCR.Cq.14wks.graph <-
  annotate_figure(figure.14wks.Cq, top=text_grob("14 Weeks", size = 14, hjust  = 0),
                  left = text_grob(bquote("ΔCq"), rot = 90, hjust=0, x = 2))
# 14WKS.graph.save --------------------------------------------------------------------

ggsave("graphs/RTqPCR14.Cq.graph1.png", width = 900, height = 1400, units = "px", dpi = 300, bg=NULL)

# total -------------------------------------------------------------------

ggdraw() +
  draw_plot(RTqPCR.7wks.graph, x = 0, y = 0, width = .5, height = 0.95) +
  draw_plot(RTqPCR.14wks.graph, x = .5, y = 0, width = .5, height = 0.95) +
  draw_plot_label(label = c("A", "B"), size = 15,
                  x = c(0, 0.5), y = c(1, 1))

ggdraw() +
  draw_plot(RTqPCR.Cq.7wks.graph, x = 0, y = 0, width = .5, height = 0.95) +
  draw_plot(RTqPCR.Cq.14wks.graph, x = .5, y = 0, width = .5, height = 0.95) +
  draw_plot_label(label = c("A", "B"), size = 15,
                  x = c(0, 0.5), y = c(1, 1))

ggsave("graphs/RTqPCR.Cq.graph.png", width = 16, height = 12, units = "cm", dpi = 300, bg=NULL)
ggsave("graphs/RTqPCR.graph.png", width = 16, height = 12, units = "cm", dpi = 300, bg=NULL)
# 7wks.ANEXOS.graph -------------------------------------------------------
inputd<- left_join(filter(RTqPCR.Data, Age == 7), 
                   filter(Main, ExpCode_1=="QP07"), 
                   by=c("Short_ID","ATF","ColorCode"))

level.check.fun(inputd,"ATF")
inputd$ATF<-ordered.fun(inputd,"ATF")
colnames(inputd)[16]="d_ddCq"         #dif

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

b7anexo<-
  ggbetweenstats(               #dif
    data = inputd,             ##Dataframe
    x = ATF,                             ## grupo de x
    y = d_ddCq,
    type="p")

df <-
  extract_stats(b7anexo)$pairwise_comparisons_data %>% 
  dplyr::mutate(groups = purrr::pmap(.l = list(group1, group2), .f = c))%>%
  dplyr::arrange(group1)

graph.sts<-filter(df, p.value<=.05)

g <-
ggplot(inputd,aes(ATF,d_ddCq))+
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
  y_position  = 1.7,
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
  position = position_jitter(width = 0.1, seed = 1)) +
  xlab("")+
  labs(y=bquote("2"^"-ΔΔCq") )+
  theme_classic () + plot.theme.box + 
  theme (axis.text.x= element_blank(),
         axis.line = element_line(colour = "black",size = .5, linetype = "solid"),
         panel.background = element_blank() )

g

Anexus.RTqPCR7<-ggarrange(g.a,
                           g,
                           g.d,
                           ncol = 1, 
                           nrow = 3,  
                           align = "v",
                           heights = c(.5,2,0.5))
Anexus.RTqPCR7
annotate_figure(Anexus.RTqPCR7,
                top = text_grob(extract_subtitle(b7anexo), face = "bold", size = 7, hjust = 1, x=1, y=-1),
                bottom = text_grob(extract_caption(b7anexo), hjust = 1, x = 1, y=1.5, face = "bold", size = 7))

ggsave("graphs/RTqPCR7.Anexo.png", width = 1200, height = 2000, units = "px", dpi = 300, bg=NULL)

# 14wks.ANEXOS.graph -------------------------------------------------------
inputd<- left_join(filter(RTqPCR.Data, Age == 14), 
                   filter(Main, ExpCode_1=="QP14"), 
                   by=c("Short_ID","ATF","ColorCode"))

level.check.fun(inputd,"ATF")
inputd$ATF<-ordered.fun(inputd,"ATF")
colnames(inputd)[16]="d_ddCq"         #dif

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

b14anexo<-
  ggbetweenstats(               #dif
    data = inputd,             ##Dataframe
    x = ATF,                             ## grupo de x
    y = d_ddCq,
    type="p",
    p.adjust="none")

df <-
  extract_stats(b14anexo)$pairwise_comparisons_data %>% 
  dplyr::mutate(groups = purrr::pmap(.l = list(group1, group2), .f = c))%>%
  dplyr::arrange(group1)

graph.sts<-filter(df, p.value<=.05)

g <-
  ggplot(inputd,aes(ATF,d_ddCq))+
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
    y_position  = 1.1,
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
                  position = position_jitter(width = 0.1, seed = 1)) +
  xlab("")+
  labs(y=bquote("2"^"-ΔΔCq") )+
  theme_classic () + plot.theme.box + 
  theme (axis.text.x= element_blank(),
         axis.line = element_line(colour = "black",size = .5, linetype = "solid"),
         panel.background = element_blank() )

##
g


Anexus.RTqPCR14<-ggarrange(g.a,
                          g,
                          g.d,
                          ncol = 1, 
                          nrow = 3,  
                          align = "v",
                          heights = c(.5,2,0.5))

ggarrange(
  (annotate_figure(Anexus.RTqPCR7,
                  top = text_grob(extract_subtitle(b7anexo), face = "bold", size = 7, hjust = 1, x=1, y=-1),
                  bottom = text_grob(extract_caption(b7anexo), hjust = 1, x = 1, y=1.5, face = "bold", size = 7))),
  (annotate_figure(Anexus.RTqPCR14,
                   top = text_grob(extract_subtitle(b14anexo), face = "bold", size = 7, hjust = 1, x=1, y=-1),
                   bottom = text_grob(extract_caption(b14anexo), hjust = 1, x = 1, y=1.5, face = "bold", size = 7))),
  ncol = 2, labels = c("A","B")
)

ggarrange(
  (annotate_figure(RTqPCR.Cq.7wks.graph,
                   top = text_grob(extract_subtitle(b7anexo), face = "bold", size = 7, hjust = 1, x=1, y=-1),
                   bottom = text_grob(extract_caption(b7anexo), hjust = 1, x = 1, y=1.5, face = "bold", size = 7))),
  (annotate_figure(RTqPCR.Cq.14wks.graph,
                   top = text_grob(extract_subtitle(b14anexo), face = "bold", size = 7, hjust = 1, x=1, y=-1),
                   bottom = text_grob(extract_caption(b14anexo), hjust = 1, x = 1, y=1.5, face = "bold", size = 7))),
  ncol = 2, labels = c("A","B")
)

ggsave("graphs/RTqPCR.Anexo.png", width = 2400, height = 2000, units = "px", dpi = 300, bg=NULL)

# table -------------------------------------------------------------------
inputd<- RTqPCR.Data

inputd$ATF <- as.factor(inputd$ATF)
colnames(inputd)[16]="d_ddCq"

Coloc.data.list <- list(
  (inputd %>%  group_by(ATF) %>%  
     summarise(count.1=sum(Age == 7), 
               Mean.1=mean(d_ddCq[Age == 7]), 
               SD.1=sd(d_ddCq[Age == 7]),
               SE.1= (sd(d_ddCq[Age == 7]))/(sqrt(sum(Age == 7))))), 
  (inputd %>%  group_by(ATF) %>%  
     summarise(count.1=sum(Age == 14), 
               Mean.1=mean(d_ddCq[Age == 14]), 
               SD.1=sd(d_ddCq[Age == 14]),
               SE.1= (sd(d_ddCq[Age == 14]))/(sqrt(sum(Age == 14)))))
)

kbl((Coloc.data.list %>% reduce(inner_join, by='ATF')), align = "c", digits = 2, 
    col.names = c("ATF" ,"n","2^-ΔΔCq","SD","SE","n","2^-ΔΔCq","SD","SE"),booktabs =TRUE) %>%
  kable_classic(full_width = T, font_size = 26) %>% 
  add_header_above(c(" "=1, "7 Weeks"=4,"14 Weeks"=4)) #%>% 
as_image(height = 12, file = "graphs/wks6.table.stats.png")

# table 2 -----------------------------------------------------------------
inputd<- RTqPCR.Data

inputd$ATF <- as.factor(inputd$ATF)
colnames(inputd)[16]="d_ddCq"
colnames(inputd)[14]="d_Cq"

Coloc.data.list.cq <- list(
  
  (inputd %>%  group_by(ATF) %>%  
     summarise(count.1=sum(Age == 7), 
               Mean.dlg4=mean(dlg4.mean[Age == 7]),
               SE.dlg4= (sd(dlg4.mean[Age == 7]))/(sqrt(sum(Age == 7))), 
               
               Mean.gapdh=mean(gapdh.mean[Age == 7]),
               SE.gapdh= (sd(gapdh.mean[Age == 7]))/(sqrt(sum(Age == 7))), 
               
               Mean.dCq=mean(d_Cq[Age == 7]),
               SE.dCq= (sd(d_Cq[Age == 7]))/(sqrt(sum(Age == 7))),
  
               Mean.ddCq=mean(d_ddCq[Age == 7]),
               SE.ddCq= (sd(d_ddCq[Age == 7]))/(sqrt(sum(Age == 7))))),

     (inputd %>%  group_by(ATF) %>%  
     summarise(count.2=sum(Age == 14), 
               Mean2.dlg4=mean(dlg4.mean[Age == 14]),
               SE2.dlg4= (sd(dlg4.mean[Age == 14]))/(sqrt(sum(Age == 14))), 
               
               Mean2.gapdh=mean(gapdh.mean[Age == 14]),
               SE2.gapdh= (sd(gapdh.mean[Age == 14]))/(sqrt(sum(Age == 14))), 
               
               Mean2.dCq=mean(d_Cq[Age == 14]),
               SE2.dCq= (sd(d_Cq[Age == 14]))/(sqrt(sum(Age == 14))),
               
               Mean2.ddCq=mean(d_ddCq[Age == 14]),
               SE2.ddCq= (sd(d_ddCq[Age == 14]))/(sqrt(sum(Age == 14)))))
)

kbl((Coloc.data.list.cq %>% reduce(inner_join, by='ATF')), align = "c", digits = 2, 
    col.names = c("ATF" ,"n","Dlg4 Cq","SE","gapdh Cq","SE","ΔCq","SE","2ΔΔ Cq","SE","n","Dlg4 Cq","SE","gapdh Cq","SE","ΔCq","SE","2ΔΔ Cq","SE"),booktabs =TRUE) %>%
  kable_classic(full_width = T, font_size = 24) %>% 
  add_header_above(c(" "=1, "7 Weeks"=9,"14 Weeks"=9)) %>% 
as_image(height = 30, file = "graphs/wks6.table.stats.Cq.png")


# auxiliares --------------------------------------------------------------


g.c<- ggplot(shapes2, aes(x, y))+
  geom_text(aes(label = shape), size = 5)+
  scale_y_discrete(limits = c("PSD95- \n 6ZF-")) +
  theme.minigraph + theme(plot.margin = margin(0,0,0,0, "cm"))

g.d<- ggplot(shapes, aes(x, y)) + 
  geom_text(aes(label = shape), size = 5) + 
  scale_y_discrete(limits = c("NoED","VP64")) +
  theme.minigraph + theme(plot.margin = margin(0,0,0,0, "cm"))
