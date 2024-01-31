# BM.graph -------------------------------------------------------
inputd<- left_join(filter(BM.Data, ColorCode!="error"), 
                   filter(Main, ExpCode_6=="BHBM"), 
                   by=c("Short_ID","ATF","ColorCode"))
##try 1
inputd$Day <- as.factor(inputd$Day)

##try 2
level.check.fun(inputd,"ATF")
inputd$ATF<-ordered.fun(inputd,"ATF")


g <-
  ggplot(inputd,aes(x= Day, y=Latency))+
  facet_wrap(~ATF, ncol=1)+
  stat_summary(aes(group=ATF), data = inputd,  fun =median, color = "black", geom = "line", size=0.5, alpha=0.7)+
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
            linetype = "dashed"
  )+
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
  geom_boxplot(width=0.3, fill=NA,linewidth = 0.1, size=0, varwidth = FALSE, outlier.alpha = 0)+
  theme_classic () + plot.theme.box + theme (axis.text.x= element_text(angle = 0, hjust = 0.5),
                                             axis.text = element_text(size = 10), 
                                             text = element_text(size=10))+
  labs(y="Primary Latency (s)") + scale_y_continuous(expand = c(0.05, 0), 
                                                     limits = c(0, 350))

g
ggsave("graphs/BM.graph.png", width = 2000, height = 3000, units = "px", dpi = 300, bg=NULL)

# BM.Summary.Table.stats -------------------------------------------------------------

RR.data.list <- list((inputd %>%  group_by(ATF) %>%  
                        summarise(count.1=sum(Day == "Day 1"), 
                                  Mean.1=mean(Latency[Day == "Day 1"]), 
                                  SD.1=sd(Latency[Day == "Day 1"]),
                                  SE.1= (sd(Latency[Day == "Day 1"]))/(sqrt(sum(Day == "Day 1"))))
                      ), 
                      (inputd %>%  group_by(ATF) %>%  
                         summarise(count.1=sum(Day == "Day 2"), 
                                   Mean.1=mean(Latency[Day == "Day 2"]), 
                                   SD.1=sd(Latency[Day == "Day 2"]),
                                   SE.1= (sd(Latency[Day == "Day 2"]))/(sqrt(sum(Day == "Day 2"))))
                      ),
                      (inputd %>%  group_by(ATF) %>%  
                         summarise(count.1=sum(Day == "Day 3"), 
                                   Mean.1=mean(Latency[Day == "Day 3"]), 
                                   SD.1=sd(Latency[Day == "Day 3"]),
                                   SE.1= (sd(Latency[Day == "Day 3"]))/(sqrt(sum(Day == "Day 3"))))
                      ),
                     (inputd %>%  group_by(ATF) %>%  
                        summarise(count.1=sum(Day == "Day 4"), 
                                  Mean.1=mean(Latency[Day == "Day 4"]), 
                                  SD.1=sd(Latency[Day == "Day 4"]),
                                  SE.1= (sd(Latency[Day == "Day 4"]))/(sqrt(sum(Day == "Day 4"))))
                     ),
                     (inputd %>%  group_by(ATF) %>%  
                        summarise(count.1=sum(Day == "Day 5"), 
                                  Mean.1=mean(Latency[Day == "Day 5"]), 
                                  SD.1=sd(Latency[Day == "Day 5"]),
                                  SE.1= (sd(Latency[Day == "Day 5"]))/(sqrt(sum(Day == "Day 5"))))
                     ),
                     (inputd %>%  group_by(ATF) %>%  
                        summarise(count.1=sum(Day == "Day 7"), 
                                  Mean.1=mean(Latency[Day == "Day 7"]), 
                                  SD.1=sd(Latency[Day == "Day 7"]),
                                  SE.1= (sd(Latency[Day == "Day 7"]))/(sqrt(sum(Day == "Day 7"))))
                     )
) 
tblt <- (RR.data.list %>% reduce(inner_join, by='ATF'))
      
tblt 

kbl((RR.data.list %>% reduce(inner_join, by='ATF'))[1:3], align = "c", digits = 2, 
    col.names = c("ATF" ,"n","Mean (s)","SD","SE",
                         "n","Mean (s)","SD","SE",
                         "n","Mean (s)","SD","SE",
                         "n","Mean (s)","SD","SE",
                         "n","Mean (s)","SD","SE",
                         "n","Mean (s)","SD","SE"),booktabs =TRUE) %>%
  kable_classic(full_width = F, font_size = 26) %>% 
  add_header_above(c(" "=1, "Day 1"=4,"Day 2"=4,"Day 3"=4,"Day 4"=4,"Day 5"=4,"Day 7"=4)) #%>%
  as_image(height = 12, file = "graphs/BM.table.Summarystats.png")
