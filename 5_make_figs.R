Sys.setenv(RSTUDIO_PANDOC="/usr/lib/rstudio-server/bin/pandoc") 
rmarkdown::render(input = "figs.Rmd")