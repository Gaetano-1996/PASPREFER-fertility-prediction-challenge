#!/usr/bin/env Rscript

install.packages(c("tidyverse",
                   "data.table",
                   "tidymodels",
                   "dials",
                   "butcher",
                   "bundle",
                   "doParallel"),
                 repos = "https://cran.r-project.org",
                 dependencies = TRUE)
