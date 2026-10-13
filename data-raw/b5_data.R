# Build the b5_items and b5_scales datasets from the tidy dictionary
# prototype/data_b5_tutorial_v1.xlsx (see prototype/build_data_b5_tutorial.R).
# OECD SSES items and definitions are not distributed with the package.

library(readxl)
library(dplyr)

src <- "prototype/data_b5_tutorial_v1.xlsx"

b5_items <- read_excel(src, sheet = "items") %>%
  filter(test != "ocde") %>%
  mutate(pole = as.integer(pole), seman_pairs = as.integer(seman_pairs),
         item_num = as.integer(item_num)) %>%
  as.data.frame()

b5_scales <- read_excel(src, sheet = "scales") %>%
  filter(test != "ocde") %>%
  mutate(n_items = as.integer(n_items)) %>%
  as.data.frame()

stopifnot(nrow(anti_join(b5_items, b5_scales, by = c("test", "facet"))) == 0)

usethis::use_data(b5_items, b5_scales, overwrite = TRUE, compress = "xz")
