#!/usr/bin/env Rscript

install.packages(c("tidyverse",
                   "data.table",
                   "tidymodels",
                   "dials",
                   "finetune",
                   "stacks",
                   "butcher",
                   "bundle",
                   "doParallel",
                   "ranger"),
                 repos = "https://cran.r-project.org",
                 dependencies = TRUE)
