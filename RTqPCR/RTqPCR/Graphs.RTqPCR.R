library(ggplot2)
library(ggsignif)
library(ggstatsplot)
library(rstatix)
library(dplyr)

plot.theme.box <- theme(axis.text = element_text(size = 7), 
                        text = element_text(size=7), 
                        axis.text.x = element_text(angle = -45, hjust = 0), 
                        legend.position="none", 
                        legend.title = element_blank(), plot.subtitle = element_text(size = 7, face = "bold")
)
data_7wks$ATF <-as.factor(data_7wks$ATF)
levels(data_7wks$ATF)
data_7wks$ATF <- ordered(data_7wks$ATF, levels=c("NI-Htt-", "NI-Htt+", "PSD95-6ZF-VP64", "PSD95-6ZF-NoED"))

data_14wks$ATF <-as.factor(data_14wks$ATF)
levels(data_14wks$ATF)
data_14wks$ATF <- ordered(data_14wks$ATF, levels=c("NI-Htt-", "NI-Htt+", "PSD95-6ZF-VP64", "PSD95-6ZF-NoED"))

colnames(data_14wks)[15]="d_ddCq"
colnames(data_7wks)[15]="d_ddCq"

##descriptivas para 7wks
sts.7wks <- data_7wks
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

# table for Age=7wks
p7<-  ggbetweenstats(
  data = data_7wks,
  x = ATF,
  y = d_ddCq,
  ylab = bquote("2"^"-ΔΔCq"),
  type = "p",
  conf.level = 0.95,
  plot.type = "box",
  pairwise.comparisons = TRUE,
  pairwise.display = "s",
  ggsignif.args = list(textsize =2, step_increase = 0.1),
  centrality.plotting = TRUE,
  centrality.point.args = list(size = 0, color = "darkred"),
  centrality.label.args = list(size = 2, nudge_x = -0.4, segment.linetype = 4, nudge_y = -0.3, alpha=0.7),
  centrality.path = FALSE,
  centrality.path.args = list(linewidth = 1, color = "red", alpha = 0.5),
  p.adjust.method = "none"
) +  theme_classic () + plot.theme.box + ylim(0,2.5) +
  scale_color_manual(values=c('#0C7BDC','#994F00',"#E69F00","#009E73")) 
p7

ggsave("graphs/wks7.graph.RTqPCR.png", width = 1200, height = 1400, units = "px", dpi = 300, bg=NULL)

p14<-  ggbetweenstats(
  data = data_14wks,
  x = ATF,
  y = d_ddCq,
  ylab = bquote("2"^"-ΔΔCq"),
  type = "p",
  conf.level = 0.95,
  plot.type = "box",
  pairwise.comparisons = TRUE,
  pairwise.display = "s",
  ggsignif.args = list(textsize =2, step_increase = 1),
  centrality.plotting = TRUE,
  centrality.point.args = list(size = 0, color = "darkred"),
  centrality.label.args = list(size = 2, nudge_x = -0.4, segment.linetype = 4, nudge_y = -0.3, alpha=0.7),
  centrality.path = FALSE,
  centrality.path.args = list(linewidth = 1, color = "red", alpha = 0.5),
  boxplot.args = list(outlier.shape = NA),
  p.adjust.method = "none"
) +  theme_classic () + theme (legend.position='none'
) + ylim(0,2.5) + plot.theme.box +
  scale_color_manual(values=c('#0C7BDC','#994F00',"#E69F00","#009E73")) 
p14

ggsave("graphs/wks14.graph.RTqPCR.png", width = 1200, height = 1400, units = "px", dpi = 300, bg=NULL)


