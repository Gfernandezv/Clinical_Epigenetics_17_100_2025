library(ggplot2)
library(ggsignif)
library(ggstatsplot)
library(rstatix)
library(dplyr)
library(readr)
library(ggrepel)
{
Main <- read_delim("C:/Users/germa/Lab Dropbox/Lab Neuroepigenetics/Lab of Neuroepigenetics/4. Huntington team - German Kevin/2-German - tesis/Analisis/GitHub/Thesis/Data/Main.csv", 
                   delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",", grouping_mark = "."), trim_ws = TRUE)
RTqPCR_Data <- read_delim("C:/Users/germa/Lab Dropbox/Lab Neuroepigenetics/Lab of Neuroepigenetics/4. Huntington team - German Kevin/2-German - tesis/Analisis/GitHub/Thesis/Data/RTqPCR.Data.csv", 
                          delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",",grouping_mark = "."), trim_ws = TRUE)


plot.theme.box <- theme(axis.text = element_text(size = 7), 
                        text = element_text(size=7), 
                        axis.text.x = element_text(angle = -45, hjust = 0), 
                        legend.position="none", 
                        legend.title = element_blank(), plot.subtitle = element_text(size = 7, face = "bold")
)

 # ejecutar una vez luego de leer los datos.
  RTqPCR.Main7wks=filter(Main, ExpCode_1=="QP07")
  RTqPCR.Main14wks=filter(Main, ExpCode_1=="QP14")
  RTqPCR.Data7wks7wks=filter(RTqPCR_Data, Age == 7)    ## Grupos para 7 semanas
  RTqPCR.Data7wks14wks=filter(RTqPCR_Data, Age == 14)  ## Grupos para 14 semanas
  
  ## Es necesario separar por fecha antes de unir con los detalles del grafico, esto para evitar confusiones con iguales ID.
  
  RTqPCR.Data7wks7wks<- left_join(RTqPCR.Data7wks7wks, RTqPCR.Main7wks, by=c("Short_ID","ATF","ColorCode"))
  RTqPCR.Data7wks14wks<- left_join(RTqPCR.Data7wks14wks, RTqPCR.Main14wks, by=c("Short_ID","ATF","ColorCode"))

  
  ## Revisión de parametros antes de graficar
  {
if( length((levels(RTqPCR.Data7wks7wks$ATF))) == 0 ) {       
  RTqPCR.Data7wks7wks$ATF <-as.factor(RTqPCR.Data7wks7wks$ATF)
}

  if( length((levels(RTqPCR.Data7wks14wks$ATF))) == 0 ) {       
    RTqPCR.Data7wks14wks$ATF <-as.factor(RTqPCR.Data7wks14wks$ATF)
  }
  }
  
  ## Inspección visual
levels(RTqPCR.Data7wks7wks$ATF)
levels(RTqPCR.Data7wks14wks$ATF)

  ## Confirmación de orden
RTqPCR.Data7wks7wks$ATF <- ordered(RTqPCR.Data7wks7wks$ATF, levels=c("HTT-NI", "HTT+NI", "PSD95-6ZF-VP64", "PSD95-6ZF-NoED"))
RTqPCR.Data7wks14wks$ATF <- ordered(RTqPCR.Data7wks14wks$ATF, levels=c("HTT-NI", "HTT+NI", "PSD95-6ZF-VP64", "PSD95-6ZF-NoED"))

colnames(RTqPCR.Data7wks14wks)[15]="d_ddCq"
colnames(RTqPCR.Data7wks7wks)[15]="d_ddCq"
} ## inicio de datos

  ## Estadisticas descriptivas para 7wks
sts.7wks <- RTqPCR.Data7wks7wks
group_by(sts.7wks, ATF) %>%
  summarise(
    count = n(),
    mean = mean(d_ddCq, na.rm = TRUE),
    sd = sd(d_ddCq, na.rm = TRUE)
  )
## anova
res.aov <- aov(d_ddCq ~ ATF, data = sts.7wks)
summary(res.aov)

# 1. Homogeneity of variances
plot(res.aov, 1)
## lavene test
library(car)
leveneTest(d_ddCq ~ ATF, data = sts.7wks)


# Grafico for Age=7wks

## geom label

{g.7<-
    ggbetweenstats(
      data = RTqPCR.Data7wks7wks,             ##Dataframe
      x = ATF,                             ## grupo de x
      y = d_ddCq,
      ylab = bquote("2"^"-ΔΔCq"),
      type = "p",
      conf.level = 0.95,
      pairwise.comparisons = TRUE,
      p.adjust.method = "none",
      pairwise.display = "s",
      plot.type = "NULL",
      ggsignif.args = list(alpha=0),
      centrality.plotting = TRUE,
      centrality.point.args = list(alpha = 0),
      centrality.label.args = list(size = 2, nudge_x = 0, segment.linetype = 0, nudge_y = -0.4, alpha=0.7),
      centrality.path = FALSE,
      point.args = list(alpha = 0)
    )
  
  ## extracción de analisis estadisticos
  g <- extract_stats(g.7)
  annotation_df <- g$pairwise_comparisons_data
  wks7.sig <- character()
  wks7.sig <-c(wks7.sig, (ifelse(g$pairwise_comparisons_data$p.value < 0.01, "***" ,
                                 ifelse(g$pairwise_comparisons_data$p.value > 0.01 & g$pairwise_comparisons_data$p.value < 0.05, "**", 'ns'))))
  
  g.7+theme_classic () + plot.theme.box + scale_y_continuous(expand = c(0, 0), limits = c(0, 2.3))+
    geom_point(inherit.aes = TRUE,
               colour=RTqPCR.Data7wks7wks$ColorCode,
               fill=RTqPCR.Data7wks7wks$ColorCode,
               shape=RTqPCR.Data7wks7wks$Symbol,
               alpha = 0.5, 
               size = 3, position = position_jitter(width = 0.1, seed = 1)) +
    geom_point(colour=RTqPCR.Data7wks7wks$ColorCode,
               shape=RTqPCR.Data7wks7wks$Symbol,
               fill=RTqPCR.Data7wks7wks$ColorCode,
               position = position_jitter(width = 0.1, seed = 1)) +
    geom_text_repel(aes(label=RTqPCR.Data7wks7wks$Short_ID ),
                    colour=RTqPCR.Data7wks7wks$ColorCode,
                    point.padding     = 0.5,
                    force             = 1,
                    direction         = "both",
                    size              = 2,
                    segment.size      = 0.3,
                    segment.curvature = -0.1,
                    position = position_jitter(width = 0.1, seed = 1))+
    geom_boxplot(width=0.3, fill=NA,linewidth = 0.1, size=0, varwidth = FALSE, outlier.alpha = 0) +
    geom_signif(    xmin=c(annotation_df$group1[2],annotation_df$group1[3],annotation_df$group1[4],annotation_df$group1[5],annotation_df$group1[6]), 
                    xmax=c(annotation_df$group2[2],annotation_df$group2[3],annotation_df$group2[4],annotation_df$group2[5],annotation_df$group2[6]), 
                    annotations=c(wks7.sig[2],wks7.sig[3],wks7.sig[4],wks7.sig[5],wks7.sig[6]), 
                    y_position=c(1.7,1.6,2.05,1.9,1.8),   ## utilizar el y_position para ordenar
                    tip_length = 0.01, textsize = 3, vjust = -0.2, color="black",
                    manual=FALSE)} ## Grafico P7

## inspección visual
g$pairwise_comparisons_data
wks7.sig
## salva la imagen CTM!
ggsave("graphs/wks7.graph.RTqPCR.png", width = 1200, height = 1400, units = "px", dpi = 300, bg=NULL)

g.14<-
  ggbetweenstats(
    data = RTqPCR.Data7wks14wks,             ##Dataframe
    x = ATF,                             ## grupo de x
    y = d_ddCq,
    type = "p",
    conf.level = 0.95,
    pairwise.comparisons = TRUE,
    p.adjust.method = "none",
    pairwise.display = "s",
    plot.type = "NULL",
    ggsignif.args = list(alpha=0),
    centrality.plotting = TRUE,
    centrality.point.args = list(alpha = 0),
    centrality.label.args = list(size = 2, nudge_x = 0, segment.linetype = 0, nudge_y = -0.4, alpha=0.7),
    centrality.path = FALSE,
    point.args = list(alpha = 0)
  )

## extracción de analisis estadisticos
g <- extract_stats(g.14)
annotation_df <- g$pairwise_comparisons_data
wks14.sig <- character()
wks14.sig <-c(wks14.sig, (ifelse(g$pairwise_comparisons_data$p.value < 0.01, "***" ,
                                 ifelse(g$pairwise_comparisons_data$p.value > 0.01 & g$pairwise_comparisons_data$p.value < 0.05, "**", 'ns'))))

g.14+theme_classic () + plot.theme.box + scale_y_continuous(expand = c(0, 0), limits = c(0, 2.3))+
  geom_point(inherit.aes = TRUE,
             colour=RTqPCR.Data7wks14wks$ColorCode,
             fill=RTqPCR.Data7wks14wks$ColorCode,
             shape=RTqPCR.Data7wks14wks$Symbol,
             alpha = 0.5, 
             size = 3, position = position_jitter(width = 0.1, seed = 1)) +
  geom_point(colour=RTqPCR.Data7wks14wks$ColorCode,
             shape=RTqPCR.Data7wks14wks$Symbol,
             fill=RTqPCR.Data7wks14wks$ColorCode,
             position = position_jitter(width = 0.1, seed = 1)) +
  geom_text_repel(aes(label=RTqPCR.Data7wks14wks$Short_ID ),
                  colour=RTqPCR.Data7wks14wks$ColorCode,
                  point.padding     = 0.5,
                  force             = 1,
                  direction         = "both",
                  size              = 2,
                  segment.size      = 0.3,
                  segment.curvature = -0.1,
                  position = position_jitter(width = 0.1, seed = 1))+
  geom_boxplot(width=0.3, fill=NA,linewidth = 0.1, size=0, varwidth = FALSE, outlier.alpha = 0) +
  geom_signif(    xmin=c(annotation_df$group1[3],annotation_df$group1[4],annotation_df$group1[5]), 
                  xmax=c(annotation_df$group2[3],annotation_df$group2[4],annotation_df$group2[5]), 
                  annotations=c(wks14.sig[3],wks14.sig[4],wks14.sig[5]), 
                  y_position=c(1.3,1.7,1.5),                                                        ## utilizar el y_position para ordenar
                  tip_length = 0.01, textsize = 3, vjust = -0.2, color="black",
                  manual=FALSE)
## inspección visual
g

## salva la imagen CTM!
ggsave("graphs/wks14.graph.RTqPCR.png", width = 1200, height = 1400, units = "px", dpi = 300, bg=NULL)
