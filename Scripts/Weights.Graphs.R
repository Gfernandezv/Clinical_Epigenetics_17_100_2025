# BM.graph -------------------------------------------------------
inputd<- left_join(filter(Weight.Data, Sex=="Male"), 
                   filter(Main), 
                   by=c("Short_ID","ATF","ColorCode"))
##try 1
inputd$ATF <- as.factor(inputd$ATF)
inputd$Sex.x <- as.factor(inputd$Sex.x)
inputd$Age <- as.factor(inputd$Age)

##try 2
level.check.fun(inputd,"ATF")
inputd$ATF<-ordered.fun(inputd,"ATF")

g <-
  ggplot(data=inputd, aes(x=Age, y=Weight, Colour=ATF)) +
  geom_point(inherit.aes = TRUE,
             colour   = inputd$ColorCode,
             fill     = inputd$ColorCode,
             shape    = 22,
             alpha    = 0.5, 
             size     = 3, 
             position = position_jitter(width = 0.1, seed = 1))

g +

geom_text_repel(aes(label=inputd$Short_ID),
                colour=inputd$ColorCode,
                point.padding     = 0.5,
                max.overlaps      = Inf,
                force             = 1,
                direction         = "both",
                size              = 2,
                segment.size      = 0.3,
                segment.curvature = -0.1,
                position = position_jitter(width = 0.1, seed = 1))
# metodo 2 Male ----------------------------------------------------------------

inputd <- filter(Weight.Data, Sex=="Male", ## <- sex filter
                              Age > 4, Age < 14,
                              Short_ID!="C2-1", Short_ID!="1-00", Short_ID!="C2-1")

inputd$ATF <- as.factor(inputd$ATF)
inputd$Sex <- as.factor(inputd$Sex)
inputd$ATF<-ordered.fun(inputd,"ATF")
inputd$Age <- as.factor(inputd$Age)

ggplot(inputd, aes(x=Age, y=Weight, fill=ATF)) + 
  geom_boxplot()
# metodo 2 Female ----------------------------------------------------------------
inputd <- filter(Weight.Data, Sex=="Female", ## <- sex filter
                 Age > 4, Age < 14,
                 Short_ID!="8515")#, Short_ID!="1-00", Short_ID!="C2-1")

inputd$ATF <- as.factor(inputd$ATF)
inputd$Sex <- as.factor(inputd$Sex)
inputd$Age <- as.factor(inputd$Age)
inputd$ATF<-ordered.fun(inputd,"ATF")

ggplot(inputd, aes(x=Age, y=Weight, fill=ATF)) + 
  geom_boxplot()

# metodo 2 all ----------------------------------------------------------------
inputd <- filter(Weight.Data, ## <- sex filter
                 Age > 4, Age < 14,
                 Short_ID!="8515", Short_ID!="C2-1", Short_ID!="1-00", Short_ID!="C2-1")

inputd$ATF <- as.factor(inputd$ATF)
inputd$Sex <- as.factor(inputd$Sex)
inputd$Age <- as.factor(inputd$Age)
inputd$ATF<-ordered.fun(inputd,"ATF")

ggplot(inputd, aes(x=Age, y=Weight)) + 
  geom_boxplot(width=0.3, fill=NA,linewidth = 0.1, size=0, varwidth = FALSE, outlier.alpha = 0)+
  geom_point(
    colour   = "black",
    fill     = inputd$ColorCode,
    alpha    = 0.7,
    shape    = 21,
    size     = 4,
    position = position_jitter(width = 0.1, seed = 1))
