# Precompile the vignettes that need embedding APIs / local Python models.
#
# The *.Rmd.orig files hold the real code. Knitting them here runs the code
# (using cached embeddings in vignettes/emb_cache/ when available) and writes
# *.Rmd files whose output is already embedded, plus figures in
# vignettes/figures/. Those .Rmd files build anywhere without API keys.
#
# Run from the package root, after installing the current version:
#   devtools::install(); source("vignettes/precompile.R")

old_wd <- setwd("vignettes")
on.exit(setwd(old_wd), add = TRUE)

for (v in c("bfi2_facetmap", "b5_jingle_jangle")) {
  knitr::knit(paste0(v, ".Rmd.orig"), paste0(v, ".Rmd"), envir = new.env())
}
