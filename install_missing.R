req <- c(
  "magick","forcats","cowplot","statsExpressions","ggplot2",
  "ggsignif","ggstatsplot","rstatix","dplyr","readr","ggrepel",
  "ggpubr","rlang","tidyverse","kableExtra","purrr","knitr",
  "docxtools","weights"
)
missing <- setdiff(req, rownames(installed.packages()))
if(length(missing)) {
  install.packages(missing)
} else {
  message("All required packages are already installed")
}
