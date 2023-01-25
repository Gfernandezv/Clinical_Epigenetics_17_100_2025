levels(data_7wks$ATF)
data_7wks$ATF <- as.factor(data_7wks$ATF)
data_7wks$ATF <- ordered(data_7wks$ATF,
                         levels = c("NI-Htt-", "NI-Htt+", "PSD95-6ZF-VP64", "PSD95-6ZF-NoED")) ## Asegurarse que el orden de los factores es el correcto.
library(dplyr)
group_by(data_7wks, ATF) %>% 
  summarise(
    count = n(),
    mean = mean(dlg4.mean, na.rm = TRUE),
    sd = sd(dlg4.mean, na.rm = TRUE)
  )

install.packages("ggpubr")

library("ggpubr")
ggboxplot(data_7wks, x = "ATF", y = "2-ddCq", 
          color = "ATF", palette = c("#00AFBB", "#E7B800", "#FC4E07", "red"),
          order = c("NI-Htt-", "NI-Htt+", "PSD95-6ZF-VP64", "PSD95-6ZF-NoED"),
          ylab = "2-ddCq", xlab = "Treatment")

library(gplots)
plotmeans(d_ddCq ~ ATF, data= data_7wks, frame=FALSE,
          xlab = "Treatment", ylab = "ddCq", main="mean")

res.aov<- aov(d_ddCq ~ ATF, data = data_7wks)
summary(res.aov)

library(multcomp)
g <- summary(glht(res.aov, linfct = mcp(ATF = "Tukey")))
g2<- plot(res.aov, 1)

## para 14 semanas
levels(data_14wks$ATF)
data_14wks$ATF <- as.factor(data_14wks$ATF)
data_14wks$ATF <- ordered(data_14wks$ATF,
                         levels = c("NI-Htt-", "NI-Htt+", "PSD95-6ZF-VP64", "PSD95-6ZF-NoED")) ## Asegurarse que el orden de los factores es el correcto.
library(dplyr)
group_by(data_14wks, ATF) %>% 
  summarise(
    count = n(),
    mean = mean(dlg4.mean, na.rm = TRUE),
    sd = sd(dlg4.mean, na.rm = TRUE)
  )

colnames(data_14wks)[15]="d_ddCq"
library("ggpubr")
ggboxplot(data_14wks, x = "ATF", y = "2-ddCq", 
          color = "ATF", palette = c("#00AFBB", "#E7B800", "#FC4E07", "red"),
          order = c("NI-Htt-", "NI-Htt+", "PSD95-6ZF-VP64", "PSD95-6ZF-NoED"),
          ylab = "2-ddCq", xlab = "Treatment")

library(gplots)
plotmeans(d_ddCq ~ ATF, data= data_14wks, frame=FALSE,
          xlab = "Treatment", ylab = "ddCq", main="mean")

res.aov<- aov(d_ddCq ~ ATF, data = data_14wks)
summary(res.aov)

library(multcomp)
summary(glht(res.aov, linfct = mcp(ATF = "Tukey")))
plot(res.aov, 1)


g$test$sigma
slm_dat <- as.data.frame(g1[])


pq <- summary(g)$test
mtests <- cbind(pq$coefficients, pq$sigma, pq$tstat, pq$pvalues)
error <- attr(pq$pvalues, "error")
pname <- switch((glht(res.aov, linfct = mcp(ATF = "Tukey")))$alternativ, less = paste("Pr(<", ifelse((glht(res.aov, linfct = mcp(ATF = "Tukey")))$df ==0, "z", "t"), ")", sep = ""), 
                greater = paste("Pr(>", ifelse((glht(res.aov, linfct = mcp(ATF = "Tukey")))$df == 0, "z", "t"), ")", sep = ""), two.sided = paste("Pr(>|",ifelse((glht(res.aov, linfct = mcp(ATF = "Tukey")))$df == 0, "z", "t"), "|)", sep = ""))
colnames(mtests) <- c("Estimate", "Std. Error", ifelse((glht(res.aov, linfct = mcp(ATF = "Tukey")))$df ==0, "z value", "t value"), pname)
library(kableExtra)
kbl(mtests, align = "c", digits = 3) %>% kable_classic(bootstrap_options = "striped", full_width = F)
 
