# RTqPCR.7wks.stats -------------------------------------------------------
  inputd<- left_join(filter(RTqPCR.Data, Age == 7), 
                     filter(Main, ExpCode_1=="QP07"), 
                     by=c("Short_ID","ATF","ColorCode"))
  
  
  RTqPCR.7wks.inputd <- filter(inputd,Genotipo==("Htt+") | Genotipo==("VP64"))
  level.check.fun(RTqPCR.7wks.inputd,"ATF")
  inputd$ATF<-ordered.fun(RTqPCR.7wks.inputd,"ATF")
  colnames(RTqPCR.7wks.inputd)[16]="d_ddCq"         #dif

inputd<- left_join(filter(RTqPCR.Data, Age == 14), 
                     filter(Main, ExpCode_1=="QP14"), 
                     by=c("Short_ID","ATF","ColorCode"))
  
RTqPCR.14wks.inputd <- filter(inputd,Genotipo==("Htt+") | Genotipo==("VP64"))
  level.check.fun(RTqPCR.14wks.inputd,"ATF")
  colnames(RTqPCR.14wks.inputd)[16]="d_ddCq"
  
  
RTqPCR.7wks.des.sts<- two_sample_test(RTqPCR.7wks.inputd, ATF, d_ddCq, paired = FALSE)%>%
  dplyr::mutate(asterisk_label = starmaker(RTqPCR.7wks.des.sts$p.value, p.levels=c(.001, .01, .05, .1), symbols=c("***", "**", "*", "")))
RTqPCR.7wks.des.sts

RTqPCR.14wks.des.sts<- two_sample_test(RTqPCR.14wks.inputd, ATF, d_ddCq, paired = FALSE)##%>%
  dplyr::mutate(asterisk_label = starmaker(RTqPCR.14wks.des.sts$p.value, p.levels=c(.001, .01, .05, .1), symbols=c("***", "**", "*", "")))
RTqPCR.14wks.des.sts

library(effectsize)
interpret_cohens_d(-5.79, rules = "cohen1988")
interpret_cohens_d(-1.10, rules = "cohen1988")  
  
# 7wks.graph -------------------------------------------------------
WB.inputd.7wks<-left_join((filter(WB.inj.Data, Age == 7)), 
                  (filter(Main, ExpCode_2=="WB07")), 
                  by=c("Short_ID","ATF","ColorCode"))

WB.inputd.7wks <- filter(WB.inputd.7wks,ATF==("HTT+NI") | ATF==("PSD95-6ZF-VP64"))
level.check.fun(WB.inputd.7wks,"ATF")
WB.inputd.7wks$ATF<-ordered.fun(WB.inputd.7wks,"ATF")

WB.7wks.des.sts<- two_sample_test(WB.inputd.7wks, ATF, Ratio, paired = FALSE)%>%
  dplyr::mutate(asterisk_label = starmaker(WB.7wks.des.sts$p.value, p.levels=c(.001, .01, .05, .1), symbols=c("***", "**", "*", "")))
WB.7wks.des.sts

