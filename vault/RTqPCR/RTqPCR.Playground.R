library(ggplot2)
library(ggsignif)
library(ggstatsplot)
library(rstatix)

colnames(data_14wks)[15]="d_ddCq"
colnames(data_7wks)[15]="d_ddCq"

# table for Age=7wks
p7<-  ggbetweenstats(
  data = data_7wks,
  x = ATF,
  y = d_ddCq,
  title = "Dlg4 expression relative to Gapdh at week 7", 
  grouping.var = Age,
  type = "p",
  conf.level = 0.99,
  plot.type = "box",
  outlier.tagging = TRUE,
  outlier.label = ID,
  pairwise.display = "significant", ## display only significant pairwise comparisons
  p.adjust.method = "fdr",
) +  theme_classic () + theme (legend.position='none'
)  + scale_color_manual(values=c('#0C7BDC','#994F00',"#E69F00","#009E73"))

p7
# table for Age=14wks
p14<-  ggbetweenstats(
  data = data_14wks,
  x = ATF,
  y = d_ddCq,
  title = "Dlg4 expression relative to Gapdh at week 14", 
  type = "p",
  conf.level = 0.99,
  plot.type = "box",
  outlier.tagging = FALSE,    ## marcar outliners
  outlier.label = ID,
  pairwise.comparisons = FALSE,
  pairwise.display = "significant", ## display only significant pairwise comparisons
  p.adjust.method = "none",
) +  theme_classic () + theme (legend.position='none'
) + ylim(0,3)
.
