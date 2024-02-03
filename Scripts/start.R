
# Dependencies ------------------------------------------------------------

library(magick)
library(forcats)
library(cowplot)
library(statsExpressions)
library(ggplot2)
library(ggsignif)
library(ggstatsplot)
library(rstatix)
library(dplyr)
library(readr)
library(ggrepel)
library("ggpubr")
library(rlang)
library("tidyverse")
library(kableExtra)
library("dplyr")
library("purrr")
library("knitr")
library("docxtools")
library(weights)
theme_update(plot.title = element_text(hjust = 0.5))

# Opciones generales ------------------------------------------------------

options(digits=3)
#starmaker(x, p.levels=c(.001, .01, .05, .1), symbols=c("***", "**", "*", "+"))

# Data Loading ------------------------------------------------------------

Main <- read_delim("C:/Users/germa/Lab Dropbox/Lab Neuroepigenetics/Lab of Neuroepigenetics/4. Huntington team - German Kevin/2-German - tesis/Analisis/GitHub/Thesis/Data/Main.csv", 
                   delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",", grouping_mark = "."), trim_ws = TRUE)

RTqPCR.Data <- read_delim("C:/Users/germa/Lab Dropbox/Lab Neuroepigenetics/Lab of Neuroepigenetics/4. Huntington team - German Kevin/2-German - tesis/Analisis/GitHub/Thesis/Data/RTqPCR.Data.csv", 
                          delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",",grouping_mark = "."), trim_ws = TRUE)

WB.inj.Data <- readr::read_delim("Data/WB.inj.Data.csv",delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",",grouping_mark = "."), trim_ws = TRUE)

RR.Data <- read_delim("Data/RR.Data.csv", delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",",grouping_mark = "."), trim_ws = TRUE)

NOROLT.Data <- read_delim("Data/NOROLT.Data.csv", 
                          delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",", 
                                                                              grouping_mark = "."), trim_ws = TRUE)

BM.Data <- read_delim("Data/BM.Data.csv", 
                      delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",", 
                                                                          grouping_mark = "."), trim_ws = TRUE)
BM.Strategy <- read_delim("Data/BM.Data2.csv", 
                      delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",", 
                                                                          grouping_mark = "."), trim_ws = TRUE)
Fluo.Data <- read_delim("Data/Spine.Data.csv", 
                                 delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",", 
                                                                                     grouping_mark = "."), trim_ws = TRUE)
coloc.Data <- read_delim("Data/coloc.Data.csv", 
                         delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",", 
                                                                             grouping_mark = "."), trim_ws = TRUE)

Weight.Data <- read_delim("Data/Weight.Data.csv", 
                          delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",", 
                                                                              grouping_mark = "."), trim_ws = TRUE)

Force.Data <- read_delim("Data/Force.Data.csv", 
                         delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",", 
                                                                             grouping_mark = "."), trim_ws = TRUE)

# Themes ------------------------------------------------------------------

plot.theme.box <- theme(axis.text       = element_text(size = 7), 
                        text            = element_text(size=7), 
                        axis.text.x     = element_text(angle = -45, hjust = 0), 

                        plot.subtitle   = element_text(size = 7, face = "bold")
                       )
##                        


# Auxiliary functions -----------------------------------------------------

sigfltr.ppairwise.fun <- function (datai,typei) {
  (extract_stats(b)$pairwise_comparisons_data
  )  %>% dplyr::mutate(groups = purrr::pmap(.l = list(group1, group2), .f = c))%>%
    dplyr::arrange(group1) %>%
    dplyr::mutate(asterisk_label = starmaker(df$p.value, p.levels=c(.001, .01, .05, .1), symbols=c("***", "**", "*", "")))
}

sigfltr.ppairwise.fun2 <- function (datai,typei) {
  (extract_stats(b)$pairwise_comparisons_data
  )  %>% dplyr::mutate(groups = purrr::pmap(.l = list(group1, group2), .f = c))%>%
    dplyr::arrange(group1) %>%
    dplyr::mutate(asterisk_label = starmaker(df$p.value, p.levels=c(.001, .01, .05, .1), symbols=c("***", "**", "*", "")))
}

ordered.fun <- function (dfi,lvli) {
  dfi[[lvli]] <-ordered(dfi[[lvli]], levels=c("HTT-NI", "HTT+NI", "PSD95-6ZF-VP64", "PSD95-6ZF-NoED"))

}

ordered.fun2 <- function (dfi,lvli) {
  dfi[[lvli]] <-ordered(dfi[[lvli]], levels=c("Training","NORT","OLT"))
  
}

ordered.fun3 <- function (dfi,lvli) {
  dfi[[lvli]] <-ordered(dfi[[lvli]], levels=c("1","2","3","4","5","7"))
  
}

level.check.fun <- function (dfi,lvli) {
  if(length((levels(dfi[[lvli]]))) == 0 ) {       
    dfi[[lvli]] <-as.factor(dfi[[lvli]])
    dfi[[lvli]] <-ordered(dfi[[lvli]], levels=c("HTT-NI", "HTT+NI", "PSD95-6ZF-VP64", "PSD95-6ZF-NoED"))
  }
  print (dfi[[lvli]])
}

level.check.fun3 <- function (dfi,lvli) {
  if(length((levels(dfi[[lvli]]))) == 0 ) {       
    dfi[[lvli]] <-as.factor(dfi[[lvli]])
    dfi[[lvli]] <-ordered(dfi[[lvli]], levels=c("1","2","3","4","5","7"))
  }
  print (dfi[[lvli]])
}

level.check.fun2 <- function (dfi,lvli) {
  if(length((levels(dfi[[lvli]]))) == 0 ) {       
    dfi[[lvli]] <-as.factor(dfi[[lvli]])
    dfi[[lvli]] <-ordered(dfi[[lvli]], levels=c("Training","OLT","NORT"))
  }
  print (dfi[[lvli]])
}

pptest.fun <- function (datai,atfi) {
  a<- dimnames(datai)
  b<- as.data.frame((pairwise.t.test((subset(datai, ATF == atfi))$time, 
                   (subset(datai, ATF == atfi))$Day, 
                   p.adjust.method = "BH", pool.sd = FALSE))$p.value) %>% 
                   format_engr(., sigdig = 2, ambig_0_adj = TRUE)
  b[is.na(b)] <- "" 
  c <- data.frame(Day=c("Day 2", "Day 3"), b)
  return(c)
  
}

pptest.fun2 <- function (datai,atfi) {
  a<- dimnames(datai)
  b<- as.data.frame((pairwise.t.test((subset(datai, ATF == atfi))$time, 
                                     (subset(datai, ATF == atfi))$Day, 
                                     p.adjust.method = "BH", pool.sd = FALSE, paired = TRUE))$p.value)

  return(b)
  #  c <- data.frame(Day=c("Day 2", "Day 3"), b)
}

pptest.ATF.fun <- function (datai,Dayi) {
  a<- dimnames(datai)
  b<- as.data.frame((pairwise.t.test((subset(datai, Day == Dayi))$time, 
                                     (subset(datai, Day == Dayi))$ATF, 
                                     p.adjust.method = "BH", pool.sd = FALSE))$p.value) 
  c <- data.frame(ATF=c("HTT+NI", "6ZF-PSD95-NoED", "6ZF-PSD95-VP64"), b)
  return(c)

}

pptest.ATF.fun2 <- function (datai,Dayi) {
  a<- dimnames(datai)
  b<- as.data.frame((pairwise.t.test((subset(datai, Day == Dayi))$time, 
                                     (subset(datai, Day == Dayi))$ATF, 
                                     p.adjust.method = "BH", pool.sd = FALSE))$p.value) 

  return(b)
 #  c <- data.frame(ATF=c("HTT+NI", "6ZF-PSD95-NoED", "6ZF-PSD95-VP64"), b) 
}

stscalc.OLT <- function (v1.i,v2.i ) {
  
  test <- inputd %>%  
    summarise(count.1=sum(Test == v1.i), 
              Mean.1=mean(index[Test == v1.i]), 
              SD.1=sd(index[Test == v1.i]),
              SE.1= (sd(index[Test == v1.i]))/(sqrt(sum(Test == v1.i))),
              count.2=sum(Test == v2.i), 
              Mean.2=mean(index[Test == v2.i]), 
              SD.2=sd(index[Test == v2.i]),
              SE.2= (sd(index[Test == v2.i]))/(sqrt(sum(Test == v2.i))))
  return(test)
}

sts.BM.calc.OLT <- function (v1.i,v2.i ) {
  
  test <- inputd %>%  
    summarise(count.1=sum(Day == v1.i), 
              Mean.1=mean(Latency[Day == v1.i]), 
              SD.1=sd(Latency[Day == v1.i]),
              SE.1= (sd(Latency[Day == v1.i]))/(sqrt(sum(Day == v1.i))),
              count.2=sum(Day == v2.i), 
              Mean.2=mean(Latency[Day == v2.i]), 
              SD.2=sd(Latency[Day == v2.i]),
              SE.2= (sd(Latency[Day == v2.i]))/(sqrt(sum(Day == v2.i))))
  return(test)
}

grph.layers <- function(column.i) {
  g <-
    ggdraw(xlim = c(0, 1), ylim = c(0, 1)) +
    draw_plot(column.i, 0, 0.15, 1, 0.85) +
    draw_label("-", x = .2, y = .11, hjust = 0, vjust = 0, size = 15, fontface = "bold")+
    draw_label("-", x = .2, y = .03, hjust = 0, vjust = 0, size = 15, fontface = "bold")+
    draw_label("-", x = .45, y = .11, hjust = 0, vjust = 0, size = 15, fontface = "bold")+
    draw_label("-", x = .45, y = .03, hjust = 0, vjust = 0, size = 15, fontface = "bold")+
    draw_label("+", x = .66, y = .11, hjust = 0, vjust = 0, size = 15, fontface = "bold")+
    draw_label("-", x = .66, y = .03, hjust = 0, vjust = 0, size = 15, fontface = "bold")+#
    draw_label("-", x = .88, y = .11, hjust = 0, vjust = 0, size = 15, fontface = "bold")+
    draw_label("+", x = .88, y = .03, hjust = 0, vjust = 0, size = 15, fontface = "bold")+
    draw_label("PSD95-6ZF-\nVP64", x = 0, y = .11, hjust = 0, vjust = 0, size = 6, fontface = "plain")+
    draw_label("NoED", x = 0, y = .03, hjust = 0, vjust = 0, size = 6, fontface = "plain")

return(g)
}
# grap anexus -------------------------------------------------------------

shapes <- data.frame(
  shape = c("-","-","-","+","-","-","+","-"),
  x = rep((1:4), times = 2),
  y = rep((1:2), each = 4)
)

shapes2 <- data.frame(
  shape = c("","","",""),
  x = rep((1:4), times = 1),
  y = c(1)
)

shapes$x <- as.factor(shapes$x)
shapes2$x <- as.factor(shapes2$x)
theme.minigraph <- theme(panel.grid   = element_blank(), 
                         axis.text.x  = element_blank(),
                         axis.ticks.x = element_blank(),
                         axis.title.x =element_blank(),
                         axis.title.y = element_blank(),
                         axis.ticks.y = element_blank(),
                         text = element_text(size = 7),
                         panel.background = element_blank())

g.a<- ggplot(shapes2, aes(x, y))+
  geom_text(aes(label = shape), size = 10)+
  scale_y_discrete(limits = c("")) +
  theme.minigraph + theme(plot.margin = margin(0,0,0,0, "cm")) +
  annotate("text", x = c(1,3), y = 1, label = c("Wt","Transgenic"), colour = "black", size = 3) +
  geom_segment(aes(x = 0.7, y = 0.7, xend = 1.5, yend = 0.7)) +
  geom_segment(aes(x = 1.7, y = 0.7, xend = 4.5, yend = 0.7)) 

g.c<- ggplot(shapes2, aes(x, y))+
  geom_text(aes(label = shape), size = 5)+
  scale_y_discrete(limits = c("PSD95- \n 6ZF-")) +
  theme.minigraph + theme(plot.margin = margin(0,0,0,0, "cm"))

g.c2<- ggplot(shapes2, aes(x, y))+
  geom_text(aes(label = shape), size = 5)+
  scale_y_discrete(limits = c("PSD95-6ZF-")) +
  theme.minigraph + theme(plot.margin = margin(0,0,0,0, "cm"))

g.d<- ggplot(shapes, aes(x, y)) + 
  geom_text(aes(label = shape), size = 5) + 
  scale_y_discrete(limits = c("PSD95-6ZF \n NoED","PSD95-6ZF \n VP64")) +
  theme.minigraph + theme(plot.margin = margin(0,0,0,0, "cm"), axis.text.y = element_text(size = 12))

g.d2<- ggplot(shapes, aes(x, y)) + 
  geom_text(aes(label = shape), size = 12) + 
  scale_y_discrete(limits = c(" \n NoED","PSD95-6ZF \n VP64")) +
  theme.minigraph + theme(plot.margin = margin(0,0,0,0, "cm"))

