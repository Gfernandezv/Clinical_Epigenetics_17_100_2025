
# ColocVP64.graph -------------------------------------------------------
inputd<- filter(coloc.Data, ATF=="PSD95-6ZF-VP64")

inputd$Comb <- as.factor(inputd$Comb)

## Estadisticas descriptivas
group_by(inputd, Comb) %>%
  summarise(
    count = n(),
    mean  = mean(Coloc_fromAB, na.rm = TRUE), #dif
    sd    = sd(Coloc_fromAB, na.rm = TRUE) #dif
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
    x = Comb,                             ## grupo de x
    y = Coloc_fromAB,
    type="p")
 b

df <-
    pairwise_comparisons(inputd, Comb, Coloc_fromAB) %>%
    dplyr::mutate(groups = purrr::pmap(.l = list(group1, group2), .f = c)) %>%
    dplyr::arrange(group1) %>%
    dplyr::mutate(asterisk_label = starmaker(df$p.value, p.levels=c(.001, .01, .05, .1), symbols=c("***", "**", "*", "+")))

g<-
ggplot(inputd,aes(x= Comb, y=Coloc_fromAB))+
geom_boxplot(width=0.3, fill=NA,linewidth = 0.1, size=0, varwidth = FALSE, outlier.alpha = 0)+
geom_point(inherit.aes = TRUE,
  colour   = "grey90",
  fill     = inputd$ColorCode,
  shape    = inputd$Symbol,
  alpha    = 0.5, 
  size     = 4, 
  position = position_jitter(width = 0.1, seed = 1)) +
geom_hline(
  yintercept=5,
  linetype="dashed", color = "grey")+
  ggsignif::geom_signif(
    comparisons = df$groups,
    map_signif_level = TRUE,
    annotations = df$asterisk_label,
)+
labs(
    #subtitle = extract_subtitle(b),
    #caption = extract_caption(b),
    y = "Colocalization (%)",
    x = "Combination") +
theme_classic () + plot.theme.box + theme (axis.text.x= element_text(angle = 0, hjust = .5),
                                           axis.text       = element_text(size = 10), 
                                           text            = element_text(size = 10)) +
scale_y_continuous(breaks=c(0, 20, 40, 60, 80, 100), expand = c(0, 1.5), limits = c(0, 100)) + 
scale_x_discrete(labels=(paste(levels(inputd$Comb),"\n(n=",table(inputd$Comb),")",sep="")))

g

ggsave("graphs/Coloc.VP64.percent.graph.png", width = 1200, height = 1400, units = "px", dpi = 300, bg=NULL)


# Coloc.NoED.graph -------------------------------------------------------
inputd<- filter(coloc.Data, ATF=="PSD95-6ZF-NoED")

inputd$Comb <- as.factor(inputd$Comb)

## Estadisticas descriptivas
group_by(inputd, Comb) %>%
  summarise(
    count = n(),
    mean  = mean(Coloc_fromAB, na.rm = TRUE), #dif
    sd    = sd(Coloc_fromAB, na.rm = TRUE) #dif
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
    x = Comb,                             ## grupo de x
    y = Coloc_fromAB,
    type="p")
b

df <-
  pairwise_comparisons(inputd, Comb, Coloc_fromAB) %>%
  dplyr::mutate(groups = purrr::pmap(.l = list(group1, group2), .f = c)) %>%
  dplyr::arrange(group1) %>%
  dplyr::mutate(asterisk_label = starmaker(df$p.value, p.levels=c(.001, .01, .05, .1), symbols=c("***", "**", "*", "+")))

g<-
  ggplot(inputd,aes(x= Comb, y=Coloc_fromAB))+
  geom_boxplot(width=0.3, fill=NA,linewidth = 0.1, size=0, varwidth = FALSE, outlier.alpha = 0)+
  geom_point(inherit.aes = TRUE,
             colour   = "grey90",
             fill     = inputd$ColorCode,
             shape    = inputd$Symbol,
             alpha    = 0.5, 
             size     = 4, 
             position = position_jitter(width = 0.1, seed = 1)) +
  geom_hline(
    yintercept=5,
    linetype="dashed", color = "grey")+
  ggsignif::geom_signif(
    comparisons = df$groups,
    map_signif_level = TRUE,
    annotations = df$asterisk_label,
  )+
  labs(
    #subtitle = extract_subtitle(b),
    #caption = extract_caption(b),
    y = "Colocalization (%)",
    x = "Combination") +
  theme_classic () + plot.theme.box + theme (axis.text.x= element_text(angle = 0, hjust = .5),
                                             axis.text       = element_text(size = 10), 
                                             text            = element_text(size = 10)) +
  scale_y_continuous(breaks=c(0, 20, 40, 60, 80, 100), expand = c(0, 1.5), limits = c(0, 100)) + 
  scale_x_discrete(labels=(paste(levels(inputd$Comb),"\n(n=",table(inputd$Comb),")",sep="")))

g

ggsave("graphs/Coloc.NoED.percent.graph.png", width = 1200, height = 1400, units = "px", dpi = 300, bg=NULL)
