

# test2 -------------------------------------------------------------------

b <-
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
    annotations = graph.sts$asterisk_label,
    textsize    = 5,
    y_position  = 1.2,
    step_increase = 0.1)+
  labs(
    y = bquote("2"^"-ΔΔCq"),
    x = "") + plot.theme.box + theme (axis.text.x= element_blank(),
                                      axis.line = element_line(colour = "black",size = .5, linetype = "solid"),
                                      panel.background = element_blank() )+
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)), limits = c(0, 2)) 




a<- ggplot(shapes2, aes(x, y))+
  geom_text(aes(label = shape), size = 5)+
  scale_y_discrete(limits = c("")) +
  theme.minigraph +
  annotate("text", x = c(1,3), y = 1, label = c("Wt","Transgenic"), colour = "black", size = 3) +
  geom_segment(aes(x = 0.5, y = 0, xend = 1.5, yend = 0)) +
  geom_segment(aes(x = 1.7, y = 0, xend = 4.5, yend = 0))

c<- ggplot(shapes2, aes(x, y))+
  geom_text(aes(label = shape), size = 5)+
  scale_y_discrete(limits = c("PSD95-6ZF-")) +
  theme.minigraph + 
  theme(plot.margin = margin(-0.2, .5, 0.5, 0.5, "cm"))

d<- ggplot(shapes, aes(x, y)) + 
  geom_text(aes(label = shape), size = 5) + 
  scale_y_discrete(limits = c("NoED","VP64")) +
  theme(plot.margin = margin(-0.5, .5, 0.5, 0.5, "cm")
        ) + theme.minigraph

ggarrange(a,b,c,d,
          ncol = 1, nrow = 4,  
          align = "v",
          heights = c(.3,2,.1,0.5))

ggsave("graphs/test.png", width = 1200, height = 1400, units = "px", dpi = 300, bg=NULL)         
