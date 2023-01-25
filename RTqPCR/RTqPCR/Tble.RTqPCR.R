g7 <- extract_stats(p7)
g7.subtitle <- g7$subtitle_data
g7.pairwise <- g7$pairwise_comparisons_data
g7.pairwise$expression=NULL
g7.pairwise$test=NULL
g7.pairwise$p.adjust.method=NULL

g14 <- extract_stats(p14)
g14.subtitle <- g14$subtitle_data
g14.pairwise <- g14$pairwise_comparisons_data
g14.caption <- g14$caption_data
g14.pairwise$expression=NULL
g14.pairwise$test=NULL
g14.pairwise$p.adjust.method=NULL

t7.1 <- kbl(dplyr::filter(data, Age == 7), align = "c", 
    digits = 3, col.names = c("ID", "Treatment", 'Age', 'Cq1','Cq2','Cq3','Mean Cq', 'Cq1', 'Cq2', 'Cq3','Mean Cq', 'dCq', 'ddCq', '2^-ddCq')) %>% kable_classic(bootstrap_options = "striped", full_width = F) %>% column_spec(7, bold = T) %>%
  column_spec(11, bold = T)%>% column_spec(14, bold = T) %>% row_spec(1:5, background = '#ecf0e9') %>% row_spec(9:12, background = '#ecf0e9') %>% 
   add_header_above(c(" "=3, "Dlg4"=4, "Gapdh"=4, " "=3))
t7.1
t7.2 <- kbl(g7.pairwise, align = "c", digits = 3, col.names = c("Group 1", "Group 2", 'Statistic', 'p value','alternative','distribution')) %>% kable_classic(bootstrap_options = "striped", full_width = F) %>% 
  row_spec(which(g7.pairwise$p.value < 0.05 ), bold = T)
t7.2
t14.1 <- kbl(data_14wks, align = "c", 
            digits = 3, col.names = c("ID", "Treatment", 'Age', 'Cq1','Cq2','Cq3','Mean Cq', 'Cq1', 'Cq2', 'Cq3','Mean Cq', 'dCq', 'ddCq', '2^-ddCq')) %>% kable_classic(bootstrap_options = "striped", full_width = F) %>% column_spec(7, bold = T) %>%
  column_spec(11, bold = T)%>% column_spec(14, bold = T) %>% row_spec(1:6, background = '#ecf0e9') %>% row_spec(11:13, background = '#ecf0e9') %>% 
  add_header_above(c(" "=3, "Dlg4"=4, "Gapdh"=4, " "=3))
t14.1
t14.2 <- kbl(g14.pairwise, align = "c", digits = 3, col.names = c("Group 1", "Group 2", 'Statistic', 'p value','alternative','distribution')) %>% kable_classic(bootstrap_options = "striped", full_width = F)
t14.2
g14.caption
pairwise_comparisons(data_14wks, ATF, d_ddCq)
