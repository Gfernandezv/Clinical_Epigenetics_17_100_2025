

# OLT.WT ------------------------------------------------------------------
library(statsExpressions)
inputd<- left_join(filter(NOROLT.Data, ATF=="HTT-NI"), 
                   filter(Main, ExpCode_5=="BO07"), 
                   by=c("Short_ID","ATF","ColorCode"))

inputd <- filter(inputd,Test!=("NORT"))
level.check.fun2(inputd,"Test")
inputd$ATF<-ordered.fun2(inputd,"Test")
inputd$Test <- factor(inputd$Test, levels = c("Training","OLT"))

OLT.WT.sts<- two_sample_test(inputd, Test, index, paired = TRUE)%>%
  dplyr::mutate(asterisk_label = starmaker(OLT.WT.sts$p.value, p.levels=c(.001, .01, .05, .1), symbols=c("***", "**", "*", "")))
OLT.WT.sts

OLT.WT.des.sts <- cbind(stscalc.OLT("Training","OLT"), OLT.WT.sts[6], OLT.WT.sts[3], OLT.WT.sts[5], OLT.WT.sts[17], 
              OLT.WT.sts[8:12])
OLT.WT.des.sts

OLT.WT.anexo <- grapher.V2.OLT.anexo(list(levels(inputd$Test)), (OLT.WT.sts$expression)) +
  theme(axis.line.y = element_line(size = 0.5, colour = "black", linetype=1), 
        axis.title.y = element_text(size = rel(1), angle = 90),
        axis.text.y = element_text(size=7, colour = "black"),
        axis.ticks.y = element_line(size = 0.5, colour = "black", linetype=1),
        axis.text.x = element_text(size=7, colour = "black")) +
  labs(y="Recognition index")

OLT.WT <- grapher.V2.OLT(list(levels(inputd$Test)), (OLT.WT.sts$asterisk_label)) +
  theme(axis.line.y = element_line(size = 0.5, colour = "black", linetype=1), 
        axis.title.y = element_text(size = rel(1), angle = 90),
        axis.text.y = element_text(size=7, colour = "black"),
        axis.ticks.y = element_line(size = 0.5, colour = "black", linetype=1),
        axis.text.x = element_text(size=7, colour = "black")) +
  labs(y="Recognition index")


# OLT.HTT+ ----------------------------------------------------------------

inputd<- left_join(filter(NOROLT.Data, ATF=="HTT+NI", Test!=("NORT")), 
                   filter(Main, ExpCode_5=="BO07"), 
                   by=c("Short_ID","ATF","ColorCode"))

level.check.fun2(inputd,"Test")
inputd$ATF<-ordered.fun2(inputd,"Test")
inputd$Test <- factor(inputd$Test, levels = c("Training","OLT"))

OLT.HTT.sts<- two_sample_test(inputd, Test, index, paired = TRUE)%>%
  dplyr::mutate(asterisk_label = starmaker(OLT.HTT.sts$p.value, p.levels=c(.001, .01, .05, .1), symbols=c("***", "**", "*", "")))
OLT.HTT.sts

OLT.HTT.des.sts <- cbind(stscalc.OLT("Training","OLT"), OLT.HTT.sts[6], OLT.HTT.sts[3], OLT.HTT.sts[5], OLT.HTT.sts[17], 
                        OLT.HTT.sts[8:12])
OLT.HTT.des.sts

OLT.HTT <- grapher.V2.OLT(list(levels(inputd$Test)), OLT.HTT.sts$asterisk_label)
OLT.HTT.anexo <- grapher.V2.OLT.anexo(list(levels(inputd$Test)), OLT.HTT.sts$asterisk_label) 

# OLT.NoED ----------------------------------------------------------------

inputd<- left_join(filter(NOROLT.Data, ATF=="PSD95-6ZF-NoED", Test!=("NORT")), 
                   filter(Main, ExpCode_5=="BO07"), 
                   by=c("Short_ID","ATF","ColorCode"))

level.check.fun2(inputd,"Test")
inputd$ATF<-ordered.fun2(inputd,"Test")
inputd$Test <- factor(inputd$Test, levels = c("Training","OLT"))

OLT.NoED.sts<- two_sample_test(inputd, Test, index, paired = TRUE) %>%
  dplyr::mutate(asterisk_label = starmaker(OLT.NoED.sts$p.value, p.levels=c(.001, .01, .05, .1), symbols=c("***", "**", "*", "")))
OLT.NoED.sts

OLT.NoED.des.sts <- cbind(stscalc.OLT("Training","OLT"), OLT.NoED.sts[6], OLT.NoED.sts[3], OLT.NoED.sts[5], OLT.NoED.sts[17], 
                        OLT.NoED.sts[8:12])
OLT.NoED.des.sts

OLT.NoED <- grapher.V2.OLT(list(levels(inputd$Test)), OLT.NoED.sts$asterisk_label)
OLT.NoED.anexo <- grapher.V2.OLT.anexo(list(levels(inputd$Test)), OLT.NoED.sts$asterisk_label)

# OLT.VP64 ----------------------------------------------------------------

inputd<- left_join(filter(NOROLT.Data, ATF=="PSD95-6ZF-VP64", Test!=("NORT")), 
                   filter(Main, ExpCode_5=="BO07"), 
                   by=c("Short_ID","ATF","ColorCode"))

level.check.fun2(inputd,"Test")
inputd$ATF<-ordered.fun2(inputd,"Test")
inputd$Test <- factor(inputd$Test, levels = c("Training","OLT"))

OLT.VP64.sts<- two_sample_test(inputd, Test, index, paired = TRUE) %>%
  dplyr::mutate(asterisk_label = starmaker(OLT.VP64.sts$p.value, p.levels=c(.001, .01, .05, .1), symbols=c("***", "**", "*", "")))
OLT.VP64.sts

OLT.VP64.des.sts <- cbind(stscalc.OLT("Training","OLT"), OLT.VP64.sts[6], OLT.VP64.sts[3], OLT.VP64.sts[5], OLT.VP64.sts[17], 
                          OLT.VP64.sts[8:12])
OLT.VP64.des.sts

OLT.VP64 <- grapher.V2.OLT(list(levels(inputd$Test)), OLT.VP64.sts$asterisk_label)
OLT.VP64.anexo <- grapher.V2.OLT.anexo(list(levels(inputd$Test)), OLT.VP64.sts$asterisk_label)


# Integrador --------------------------------------------------------------
library(cowplot)

# Principales -------------------------------------------------------------------------


OLT.plot <- plot_grid(g.a,
                       plot_grid(
                         OLT.WT,
                         OLT.HTT,
                         OLT.VP64,
                         OLT.NoED,
                         ncol = 4, align = "h", rel_widths = c(1.5, 1,1,1)),
                       ncol = 1, align = "hv", rel_heights = c(.15, 1))
OLT.plot.cmp <-
  ggdraw(xlim = c(0, 1), ylim = c(0, 1)) +
  draw_plot(OLT.plot, 0, 0.15, 1, 0.85) +
  draw_label("-", x = .2, y = .11, hjust = 0, vjust = 0, size = 15, fontface = "bold")+
  draw_label("-", x = .2, y = .03, hjust = 0, vjust = 0, size = 15, fontface = "bold")+
  draw_label("-", x = .45, y = .11, hjust = 0, vjust = 0, size = 15, fontface = "bold")+
  draw_label("-", x = .45, y = .03, hjust = 0, vjust = 0, size = 15, fontface = "bold")+
  draw_label("+", x = .66, y = .11, hjust = 0, vjust = 0, size = 15, fontface = "bold")+
  draw_label("-", x = .66, y = .03, hjust = 0, vjust = 0, size = 15, fontface = "bold")+#
  draw_label("-", x = .88, y = .11, hjust = 0, vjust = 0, size = 15, fontface = "bold")+
  draw_label("+", x = .88, y = .03, hjust = 0, vjust = 0, size = 15, fontface = "bold")+
  draw_label("PSD95-6ZF-\nVP64", x = 0, y = .11, hjust = 0, vjust = 0, size = 7, fontface = "plain")+
  draw_label("NoED", x = 0, y = .03, hjust = 0, vjust = 0, size = 7, fontface = "plain")#+
  draw_image(OLT.logo_file,  x = .37, y = -.21, scale = .2)

ggsave("graphs/OLT.graph.png", width = 11, height = 13, units = "cm", dpi = 300, bg=NULL)

# Anexas ------------------------------------------------------------------

OLT.plot.anexo <- plot_grid(g.a,
                      plot_grid(
                        OLT.WT.anexo,
                        OLT.HTT.anexo,
                        OLT.VP64.anexo,
                        OLT.NoED.anexo,
                        ncol = 4, align = "h", rel_widths = c(1.5, 1,1,1)),
                      ncol = 1, align = "hv", rel_heights = c(.15, 1))
OLT.plot.cmp.anexo <-
  ggdraw(xlim = c(0, 1), ylim = c(0, 1)) +
  draw_plot(OLT.plot.anexo, 0, 0.15, 1, 0.85) +
  draw_label("-", x = .2, y = .11, hjust = 0, vjust = 0, size = 15, fontface = "bold")+
  draw_label("-", x = .2, y = .03, hjust = 0, vjust = 0, size = 15, fontface = "bold")+
  draw_label("-", x = .45, y = .11, hjust = 0, vjust = 0, size = 15, fontface = "bold")+
  draw_label("-", x = .45, y = .03, hjust = 0, vjust = 0, size = 15, fontface = "bold")+
  draw_label("+", x = .66, y = .11, hjust = 0, vjust = 0, size = 15, fontface = "bold")+
  draw_label("-", x = .66, y = .03, hjust = 0, vjust = 0, size = 15, fontface = "bold")+#
  draw_label("-", x = .88, y = .11, hjust = 0, vjust = 0, size = 15, fontface = "bold")+
  draw_label("+", x = .88, y = .03, hjust = 0, vjust = 0, size = 15, fontface = "bold")+
  draw_label("PSD95-6ZF\n-VP64", x = 0, y = .11, hjust = 0, vjust = 0, size = 8, fontface = "plain")+
  draw_label("PSD95-6ZF\n-NoED", x = 0, y = .03, hjust = 0, vjust = 0, size = 8, fontface = "plain")+
  draw_image(OLT.logo_file,  x = .37, y = -.20, scale = .2)

ggsave("graphs/test.png", width = 18.6, height = 18.6, units = "cm", dpi = 300, bg=NULL)
# NORT.WT ------------------------------------------------------------------

inputd<- left_join(filter(NOROLT.Data, ATF=="HTT-NI"), 
                   filter(Main, ExpCode_5=="BO07"), 
                   by=c("Short_ID","ATF","ColorCode"))

inputd <- filter(inputd,Test!=("OLT"))
inputd$Test <- as.factor(inputd$Test)
inputd$Test <- factor(inputd$Test, levels = c("Training","NORT"))

NORT.WT.sts<- two_sample_test(inputd, Test, index, paired = TRUE) %>%
  dplyr::mutate(asterisk_label = starmaker(NORT.WT.sts$p.value, p.levels=c(.001, .01, .05, .1), symbols=c("***", "**", "*", "")))
NORT.WT.sts

NORT.WT.des.sts <- cbind(stscalc.OLT("Training","NORT"), NORT.WT.sts[6], NORT.WT.sts[3], NORT.WT.sts[5], NORT.WT.sts[17], 
                         NORT.WT.sts[8:12])
NORT.WT.des.sts

NORT.WT <- grapher.V2.OLT(list(levels(inputd$Test)), NORT.WT.sts$asterisk_label) +
  theme(axis.line.y = element_line(size = 0.5, colour = "black", linetype=1), 
        axis.title.y = element_text(size = rel(1), angle = 90),
        axis.text.y = element_text(size=7, colour = "black"),
        axis.ticks.y = element_line(size = 0.5, colour = "black", linetype=1),
        axis.text.x = element_text(size=7, colour = "black")) + scale_x_discrete(labels=c("O", "N")) +
  labs(y="Recognition index")

NORT.WT.anexo <- grapher.V2.OLT.anexo(list(levels(inputd$Test)), NORT.WT.sts$expression) +
  theme(axis.line.y = element_line(size = 0.5, colour = "black", linetype=1), 
        axis.title.y = element_text(size = rel(1), angle = 90),
        axis.text.y = element_text(size=7, colour = "black"),
        axis.ticks.y = element_line(size = 0.5, colour = "black", linetype=1),
        axis.text.x = element_text(size=7, colour = "black")) +
  labs(y="Recognition index")

# NORT.HTT+ ----------------------------------------------------------------

inputd<- left_join(filter(NOROLT.Data, ATF=="HTT+NI", Test!=("OLT")), 
                   filter(Main, ExpCode_5=="BO07"), 
                   by=c("Short_ID","ATF","ColorCode"))

level.check.fun2(inputd,"Test")
inputd$ATF<-ordered.fun2(inputd,"Test")

inputd$Test <- factor(inputd$Test, levels = c("Training","NORT"))

NORT.HTT.sts<- two_sample_test(inputd, Test, index, paired = TRUE) %>%
  dplyr::mutate(asterisk_label = starmaker(NORT.HTT.sts$p.value, p.levels=c(.001, .01, .05, .1), symbols=c("***", "**", "*", "")))
NORT.HTT.sts

NORT.HTT.des.sts <- cbind(stscalc.OLT("Training","NORT"), NORT.HTT.sts[6], NORT.HTT.sts[3], NORT.HTT.sts[5], NORT.HTT.sts[17], 
                         NORT.HTT.sts[8:12])
NORT.HTT.des.sts

NORT.HTT <- grapher.V2.OLT(list(levels(inputd$Test)), NORT.HTT.sts$asterisk_label) + scale_x_discrete(labels=c("O", "N"))
NORT.HTT.anexo <- grapher.V2.OLT.anexo(list(levels(inputd$Test)), NORT.HTT.sts$asterisk_label) + scale_x_discrete(labels=c("T", "N"))
NORT.HTT
# NORT.NoED ----------------------------------------------------------------

inputd<- left_join(filter(NOROLT.Data, ATF=="PSD95-6ZF-NoED", Test!=("OLT")), 
                   filter(Main, ExpCode_5=="BO07"), 
                   by=c("Short_ID","ATF","ColorCode"))

level.check.fun2(inputd,"Test")
inputd$ATF<-ordered.fun2(inputd,"Test")
inputd$Test <- factor(inputd$Test, levels = c("Training","NORT"))

NORT.NoED.sts<- two_sample_test(inputd, Test, index, paired = TRUE) %>%
  dplyr::mutate(asterisk_label = starmaker(NORT.NoED.sts$p.value, p.levels=c(.001, .01, .05, .1), symbols=c("***", "**", "*", "")))
NORT.NoED.sts

NORT.NoED.des.sts <- cbind(stscalc.OLT("Training","NORT"), NORT.NoED.sts[6], NORT.NoED.sts[3], NORT.NoED.sts[5], NORT.NoED.sts[17], 
                          NORT.NoED.sts[8:12])
NORT.NoED.des.sts

NORT.NoED <- grapher.V2.OLT(list(levels(inputd$Test)), NORT.NoED.sts$asterisk_label) + scale_x_discrete(labels=c("O", "N"))
NORT.NoED.anexo <- grapher.V2.OLT.anexo(list(levels(inputd$Test)), NORT.NoED.sts$asterisk_label) + scale_x_discrete(labels=c("T", "N"))

# NORT.VP64 ----------------------------------------------------------------

inputd<- left_join(filter(NOROLT.Data, ATF=="PSD95-6ZF-VP64", Test!=("OLT")), 
                   filter(Main, ExpCode_5=="BO07"), 
                   by=c("Short_ID","ATF","ColorCode"))

level.check.fun2(inputd,"Test")
inputd$ATF<-ordered.fun2(inputd,"Test")
inputd$Test <- factor(inputd$Test, levels = c("Training","NORT"))

NORT.VP64.sts<- two_sample_test(inputd, Test, index, paired = TRUE) %>%
  dplyr::mutate(asterisk_label = starmaker(NORT.VP64.sts$p.value, p.levels=c(.001, .01, .05, .1), symbols=c("***", "**", "*", "")))
NORT.VP64.sts

NORT.VP64.des.sts <- cbind(stscalc.OLT("Training","NORT"), NORT.VP64.sts[6], NORT.VP64.sts[3], NORT.VP64.sts[5], NORT.VP64.sts[17], 
                           NORT.VP64.sts[8:12])
NORT.VP64.des.sts

NORT.VP64 <- grapher.V2.OLT(list(levels(inputd$Test)), NORT.VP64.sts$asterisk_label) + scale_x_discrete(labels=c("O", "N"))
NORT.VP64.anexo <- grapher.V2.OLT.anexo(list(levels(inputd$Test)), NORT.VP64.sts$asterisk_label) + scale_x_discrete(labels=c("T", "N"))


# Integrador --------------------------------------------------------------
  NORT.plot <- plot_grid(g.a,
                        plot_grid(
                          NORT.WT,
                          NORT.HTT,
                          NORT.VP64,
                          NORT.NoED,
                          ncol = 4, align = "h", rel_widths = c(1.5, 1,1,1)),
                        ncol = 1, align = "hv", rel_heights = c(.15, 1))
  
  NORT.plot.cmp <-
    ggdraw(xlim = c(0, 1), ylim = c(0, 1)) +
    draw_plot(NORT.plot, 0, 0.15, 1, 0.85) +
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

NORT.CD<- ggarrange(OLT.plot.cmp,NORT.plot.cmp, ncols=2, align = "hv")

ggdraw()+
  draw_plot(NORT.CD, 0, -0.5, 1, 1)+
  draw_image(NORT.squema,  x = 0, y = .28, scale = .9)+
  draw_label("A", x = .01, y = .95, hjust = 0, vjust = 0, size = 15, fontface = "bold")+
  draw_label("B", x = .01, y = .67, hjust = 0, vjust = 0, size = 15, fontface = "bold")+
  draw_label("C", x = .01, y = .5, hjust = 0, vjust = 0, size = 15, fontface = "bold")+
  draw_label("D", x = .51, y = .5, hjust = 0, vjust = 0, size = 15, fontface = "bold")

NORT.logo_file <- "C:/Users/germa/Lab Dropbox/Lab Neuroepigenetics/Lab of Neuroepigenetics/4. Huntington team - German Kevin/2-German - tesis/Analisis/GitHub/Thesis/Data/NORT.sqm.png"
OLT.logo_file <- "C:/Users/germa/Lab Dropbox/Lab Neuroepigenetics/Lab of Neuroepigenetics/4. Huntington team - German Kevin/2-German - tesis/Analisis/GitHub/Thesis/Data/OLT.sqm.png"
NORT.squema <- "C:/Users/germa/Lab Dropbox/Lab Neuroepigenetics/Lab of Neuroepigenetics/4. Huntington team - German Kevin/2-German - tesis/Analisis/GitHub/Thesis/Data/NORT.OLT.sqm.png"
ggsave("graphs/NORTOLT.cmp.png", width = 15, height = 13, units = "cm", dpi = 300, bg=NULL)

# anexos ------------------------------------------------------------------
NORT.plot.anexo <- plot_grid(g.a,
                       plot_grid(
                         NORT.WT.anexo,
                         NORT.HTT.anexo,
                         NORT.VP64.anexo,
                         NORT.NoED.anexo,
                         ncol = 4, align = "h", rel_widths = c(1.5, 1,1,1)),
                       ncol = 1, align = "hv", rel_heights = c(.15, 1))

NORT.plot.cmp.anexo <-
  ggdraw(xlim = c(0, 1), ylim = c(0, 1)) +
  draw_plot(NORT.plot.anexo, 0, 0.15, 1, 0.85) +
  draw_label("-", x = .2, y = .11, hjust = 0, vjust = 0, size = 15, fontface = "bold")+
  draw_label("-", x = .2, y = .03, hjust = 0, vjust = 0, size = 15, fontface = "bold")+
  draw_label("-", x = .45, y = .11, hjust = 0, vjust = 0, size = 15, fontface = "bold")+
  draw_label("-", x = .45, y = .03, hjust = 0, vjust = 0, size = 15, fontface = "bold")+
  draw_label("+", x = .66, y = .11, hjust = 0, vjust = 0, size = 15, fontface = "bold")+
  draw_label("-", x = .66, y = .03, hjust = 0, vjust = 0, size = 15, fontface = "bold")+#
  draw_label("-", x = .88, y = .11, hjust = 0, vjust = 0, size = 15, fontface = "bold")+
  draw_label("+", x = .88, y = .03, hjust = 0, vjust = 0, size = 15, fontface = "bold")+
  draw_label("PSD95-6ZF\n-VP64", x = 0, y = .11, hjust = 0, vjust = 0, size = 8, fontface = "plain")+
  draw_label("PSD95-6ZF\n-NoED", x = 0, y = .03, hjust = 0, vjust = 0, size = 8, fontface = "plain")+
  draw_image(NORT.logo_file,  x = .37, y = -.21, scale = .2)

NORT.OLT.Anexos<- ggarrange(OLT.plot.cmp.anexo,NORT.plot.cmp.anexo, ncols=1, nrows=2, labels = c("C","D"))
NORT.OLT.Anexos <-
  ggdraw(xlim = c(0, 1), ylim = c(0, 1)) +
  draw_plot(OLT.plot.cmp.anexo,0, .5, 1, .5)+
  draw_plot(NORT.plot.cmp.anexo,0, 0, 1, .5)+
  draw_label("A", x = 0, y = .97, hjust = 0, vjust = 0, size = 15, fontface = "bold")+
  draw_label("B", x = 0, y = .5, hjust = 0, vjust = 0, size = 15, fontface = "bold")

ggsave("graphs/NORT.OLT.Anexos.png", width = 20, height = 22, units = "cm", dpi = 300, bg=NULL)
# tablas ------------------------------------------------------------------
OLT.table <- list(OLT.WT.des.sts, OLT.HTT.des.sts, OLT.VP64.des.sts, OLT.NoED.des.sts) %>% bind_rows()
OLT.table2<-cbind(c("Wt","Transgenic","Transgenic","Transgenic"),c("-","-","+","-"),c("-","-","-","+"),OLT.table)

# Tabla OLT ---------------------------------------------------------------


row.names(OLT.table2) <- NULL
OLT.table2$p.value2 <- OLT.table2$p.value
OLT.table2$p.value <- as.character(formatC(OLT.table2$p.value, digits = 2, format = 'f')) 
OLT.table2$p.value = cell_spec(OLT.table2$p.value, bold = ifelse(OLT.table2$p.value2 <= 0.05, T, F))

OLT.table.test <- kbl (OLT.table2[1:20], align = "c",digits = 2, col.names = c(" ","PSD95-6ZF-\nVP64",
                                                                               "PSD95-6ZF-\nNoED", 
                                                                               "n","Mean","SD","SE",
                                                                               "n","Mean","SD","SE",
                                                                               "Method","Statistic","p.value"," ","Effect size",
                                                                               "Estimate","conf.level","conf.low","conf.high"),
                       booktabs =TRUE, escape = F) %>% 
  collapse_rows(columns = 1, valign = "middle") %>% 
  kable_classic("striped", full_width = T, font_size = 20) %>%
  add_header_above(c(" "=3, "Trained"=4,"Displaced"=4, " "=9))%>%  
  as_image(file = "graphs/OLT.table.png", width = 3 )

OLT.plot <- plot_grid(g.a,
                      plot_grid(
                        OLT.WT,
                        OLT.HTT,
                        OLT.VP64,
                        OLT.NoED,
                        ncol = 4, align = "h", rel_widths = c(1.5, 1,1,1)),
                      ncol = 1, align = "hv", rel_heights = c(.15, 1))

# Tabla NORT --------------------------------------------------------------

NORT.table <- list(NORT.WT.des.sts, NORT.HTT.des.sts, NORT.VP64.des.sts, NORT.NoED.des.sts) %>% bind_rows()
NORT.table2<-cbind(c("Wt","Transgenic","Transgenic","Transgenic"),c("-","-","+","-"),c("-","-","-","+"),NORT.table)

row.names(NORT.table2) <- NULL
NORT.table2$p.value2 <- NORT.table2$p.value
NORT.table2$p.value <- as.character(formatC(NORT.table2$p.value, digits = 2, format = 'g')) 
NORT.table2$p.value = cell_spec(NORT.table2$p.value, bold = ifelse(NORT.table2$p.value2 <= 0.05, T, F))

NORT.table.test <- kbl (NORT.table2[1:20], align = "c",digits = 2, col.names = c(" ","PSD95-6ZF-\nVP64",
                                                                               "PSD95-6ZF-\nNoED", 
                                                                               "n","Mean","SD","SE",
                                                                               "n","Mean","SD","SE",
                                                                               "Method","Statistic","p.value"," ","Effect size",
                                                                               "Estimate","conf.level","conf.low","conf.high"),
                       booktabs =TRUE, escape = F) %>% 
  collapse_rows(columns = 1, valign = "middle") %>% 
  kable_classic( full_width = F, font_size = 20) %>%
  add_header_above(c(" "=3, "Trained"=4,"Novel"=4, " "=9))%>%  
  as_image(file = "graphs/NORT.table.png", width = 3 )



# Funs ----------------------------------------------------------------------


##

  
grapher.V2.OLT <- function (level.i, label.i) {
  
  g <-  ggplot(inputd,aes(x=Test, y=index))+
    geom_boxplot(width=0.3, fill=NA,linewidth = 0.1, size=0, varwidth = FALSE, outlier.alpha = 0)+
    stat_summary(aes(group=Test), data = inputd,  fun =median, color = "black", geom = "line", size=0.5, alpha=0.2) +
    geom_point(inherit.aes = TRUE,
               colour   = inputd$ColorCode,
               fill     = inputd$ColorCode,
               shape    = inputd$Symbol,
               alpha    = 0.5, 
               size     = 3, 
               position = position_jitter(width = 0.1, seed = 1)) +
    geom_line(aes(group = Short_ID),
              colour   = "grey",
              alpha    = 0.5, 
              position = position_jitter(width = 0.1, seed = 1),
              linetype = "dashed")+
    theme_classic () + labs(y="Recognition index") + theme (axis.line.y = element_blank(),
                                                            axis.title.y = element_blank(),
                                                            axis.text.y = element_blank(),
                                                            axis.ticks.y = element_blank(),
                                                            axis.title.x = element_blank(),
                                                            axis.text.x = element_text(size=7, colour = "black"),
                                                            plot.margin = margin(.5,0,0,0, "cm"))+
    scale_y_continuous(expand = c(0, 0), limits = c(.35, 1)) + scale_x_discrete(labels=c("ND", "D"))
  
  if((label.i)!="") {
    g <- g + geom_signif(
      comparisons = level.i,
      map_signif_level = TRUE,
      annotations = label.i,
      y_position  = max(inputd$index)+.05,
      textsize    = 5,
      step_increase = 0.17)
    return(g)
  } else {
    return (g)
  }
  
}

grapher.V2.OLT.anexo <- function (level.i, label.i) {
  
  g <-  
  ggplot(inputd,aes(x=Test, y=index))+
  geom_boxplot(width=0.3, fill=NA,linewidth = 0.1, size=0, varwidth = FALSE, outlier.alpha = 0)+
  stat_summary(aes(group=Test), data = inputd,  fun =median, color = "black", geom = "line", size=0.5, alpha=0.2) +
  geom_point(inherit.aes = TRUE,
    colour   = inputd$ColorCode,
    fill     = inputd$ColorCode,
    shape    = inputd$Symbol,
    alpha    = 0.5, 
    size     = 3, 
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
  theme_classic () + labs(y="Recognition index") + theme (axis.line.y = element_blank(),
                                                            axis.title.y = element_blank(),
                                                            axis.text.y = element_blank(),
                                                            axis.ticks.y = element_blank(),
                                                            axis.title.x = element_blank(),
                                                          axis.text.x = element_text(size=7, colour = "black"),
                                                            text = element_text(size = 7),
                                                            plot.margin = margin(.5,0,0,0, "cm"))+
    scale_y_continuous(expand = c(0, 0), limits = c(0, 1.3)) + scale_x_discrete(labels=c("T", "D"))
  
  if((label.i)!="") {
    g <- g + geom_signif(
      comparisons = level.i,
      map_signif_level = TRUE,
      annotations = as.character(label.i),
      y_position  = max(inputd$index)+.05,
      parse = TRUE,
      textsize    = 2,
      step_increase = 0.17)
    return(g)
  } else {
    return (g)
  }
  
}

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
                position = position_jitter(width = 0.1, seed = 1))

