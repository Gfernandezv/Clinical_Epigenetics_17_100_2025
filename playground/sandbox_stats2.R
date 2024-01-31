library("tidyverse")
library(kableExtra)

df <- data.frame(x = c("a", "b", "c", "d"), y = c(3, 4, 1, 2))
bars <- ggplot(df, aes(x, y, fill = y)) + 
  geom_bar(stat = "identity") + 
  labs(x = NULL, y = NULL) 

point <-  ggplot(df, aes(x, y, colour=x, fill = x)) + 
  geom_point()
point

RR.data.list <- list((inputd %>%  group_by(ATF) %>%  
                        summarise(count.1=sum(Day == "Day 1"), 
                        Mean.1=mean(time[Day == "Day 1"]), 
                        SE.1= (sd(time[Day == "Day 1"]))/(sqrt(sum(Day == "Day 1"))))
                      ), 
                     (inputd %>%  group_by(ATF) %>%  
                       summarise(count.1=sum(Day == "Day 2"), 
                       Mean.1=mean(time[Day == "Day 2"]), 
                       SE.1= (sd(time[Day == "Day 2"]))/(sqrt(sum(Day == "Day 2"))))
                     ),
                     (inputd %>%  group_by(ATF) %>%  
                        summarise(count.1=sum(Day == "Day 3"), 
                        Mean.1=mean(time[Day == "Day 3"]), 
                        SE.1= (sd(time[Day == "Day 3"]))/(sqrt(sum(Day == "Day 3"))))
                     )
                     ) 

kbl((RR.data.list %>% reduce(inner_join, by='ATF')), align = "c", digits = 2, 
    col.names = c("ATF" ,"N","Mean (s)","SE","N","Mean (s)","SE","N","Mean (s)","SE"),booktabs =TRUE) %>%
  kable_classic(full_width = T, font_size = 26) %>% 
  add_header_above(c(" "=1, "Day 1"=3,"Day 2"=3,"Day 3"=3)) %>% 
  add_header_above(c("Latency to fall, Week 6"=10), align = "l") %>% 
  as_image(height = 12, file = "graphs/wks6.table.D2.png")

p<- pairwise.t.test((subset(inputd, Day == 'Day 1'))$time, 
                    (subset(inputd, Day == 'Day 1'))$ATF, 
                    p.adjust.method = "BH", pool.sd = FALSE)
kbl(p$p.value, align = "c", digits = 3) %>% 
      kable_classic(full_width = F, font_size = 26) 
    #%>% add_header_above(c("*p* values for day 1"=4), align = "l") %>% as_image(height = 12, file = "graphs/wks6.sttable.D1.png")

ptt.list <- list(pptest.fun(inputd,'HTT-NI'),
                 pptest.fun(inputd,'HTT+NI'),
                 pptest.fun(inputd,'PSD95-6ZF-VP64'),
                 pptest.fun(inputd,'PSD95-6ZF-NoED'))

kbl((ptt.list %>% reduce(inner_join, by='Day')), 
    align = "c", digits = 2,
    col.names = c("" ,"Day 1","Day 2","Day 1","Day 2","Day 1","Day 2","Day 1","Day 2"),booktabs =TRUE) %>%
    kable_classic(full_width = T, font_size = 26) %>% 
    add_header_above(c(" "=1, "HTT-NI"=2,"HTT+NI"=2,"PSD95-6ZF-VP64"=2,"PSD95-6ZF-NoED"=2)) ## guardar de forma manual ya que no renderiza bien.

# tabla -------------------------------------------------------------------


ptt.list.ATF <- list(pptest.ATF.fun2(inputd,'Day 1'),
                     pptest.ATF.fun2(inputd,'Day 2'),
                     pptest.ATF.fun2(inputd,'Day 3')
)

pp.test <- bind_rows(ptt.list.ATF) 
row.names(pp.test) <- NULL

pp.ATF <- pp.test %>% 
  mutate(
    across(2:4, 
           ~ if_else((.x)<0.05,(paste((format(.x, digits=3,scientific=TRUE)),starmaker(.x))),
                     format(.x, digits=3,scientific=TRUE),
                     ""))
  ) 

kbl (pp.ATF, align = "l", col.names = c("" ,"HTT-NI","HTT+NI","PSD95-6ZF-NoED"),booktabs =TRUE) %>% 
  collapse_rows(columns = 1, valign = "middle") %>%
  kable_classic(full_width = T, font_size = 26) %>%
  pack_rows("Day 1", 1, 3) %>%
  pack_rows("Day 2", 4, 6) %>%
  pack_rows("Day 3", 7, 9) %>%
  as_image(height = 12, file = "graphs/wks6.table.byATF.png")


##############################
ptt.list <- list(pptest.fun2(inputd,'HTT-NI'),
                 pptest.fun2(inputd,'HTT+NI'),
                 pptest.fun2(inputd,'PSD95-6ZF-VP64'),
                 pptest.fun2(inputd,'PSD95-6ZF-NoED'))

pp.Day <- ptt.list %>% 
          reduce(inner_join, by='Day') %>% 
          mutate(
                across(2:9, 
                      ~ if_else((.x)<0.05,(paste((format(.x, digits=3,scientific=TRUE)),starmaker(.x))),
                                          format(.x, digits=3,scientific=TRUE),
                                          ""))
                 )

kbl(pp.ATF, align = "c", col.names = c("" ,"Day 1","Day 2","Day 1","Day 2","Day 1","Day 2","Day 1","Day 2"),booktabs =TRUE) %>%
  kable_classic(full_width = T, font_size = 26) %>% 
  add_header_above(c(" "=1, "HTT-NI"=2,"HTT+NI"=2,"PSD95-6ZF-VP64"=2,"PSD95-6ZF-NoED"=2)) %>% as_image(height = 12, file = "graphs/wks6.table.byDays.png")

pptest.ATF.fun(inputd,'Day 1')

pp<- pp.tableformat(ptt.list,'Day 1',9)
##
pp.tableformat <- function (lista.i, var.i, col.i)
  lista.i %>% 
  reduce(inner_join, by=var.i) %>% 
  mutate(
    across(2:col.i, 
           ~ if_else((.x)<0.05,(paste((format(.x, digits=3,scientific=TRUE)),starmaker(.x))),
                     format(.x, digits=3,scientific=TRUE),
                     ""))
  )
###**********************
  table_eye_color <- starwars %>% 
  dplyr::filter(eye_color %in% c("black", "blue", "yellow", "brown")) %>% 
  group_by (gender, eye_color) %>% 
  count () %>% 
  mutate (n_2 = n + 2) %>% 
  na.omit (gender) %>% 
  dplyr::filter (gender != "none")

table_hair_color <- starwars %>% 
  dplyr::filter(eye_color %in% c("black", "brown", "none", "white")) %>% 
  group_by (gender, hair_color) %>% 
  count () %>% 
  mutate (n_2 = n + 2) %>% 
  na.omit (gender)%>% 
  dplyr::filter (gender != "none")

my_kable <- function (df, x){
  x <- enquo(x)
  
  table <- df %>% 
    kable ("latex",
           booktabs = TRUE,
           linesep = "",
           caption = paste0 ("Gender by ", x)) %>% 
    kable_styling(latex_options = "HOLD_position") %>% 
    add_header_above(c(" ", " ", "N and N + 2" = 2)) %>% 
    column_spec(3:4, width = "1cm") %>% 
    collapse_rows(columns = 1, valign = "middle")
  
  return (table)
}

my_kable(table_eye_color, "Eye Color")
my_kable(table_hair_color, "Hair Colour")