# filesets

_outputs = outputs/paper.pdf outputs/pres_short.pdf outputs/pres_long.pdf

# Building

all: outputs
outputs: $(_outputs)
data: data/states/* data/cities
figs: 5_figs.html
# Cleaning

clean: # output
	rm -rf outputs/* 5_figs.html figs/*

clean_throughput: 
	rm -rf throughput/*

clean_data: 
	rm -rf data/*

# Dependencies


data/states/*: assumptions/cities.csv assumptions/lookup.csv 1_get_states.R
	Rscript 1_get_states.R

data/cities: data/states/* 2_make_cities.R
	Rscript 2_make_cities.R

throughput/info_cache.csv: data/cities 3_local_info.R
	Rscript 3_local_info.R

throughput/loss_curves.csv: data/cities 4_cluster.R
	Rscript 4_cluster.R

5_figs.html: 5_figs.Rmd  throughput/info_cache.csv throughput/loss_curves.csv
	Rscript -e 'rmarkdown::render("5_figs.rmd")'

outputs/paper.pdf: paper.tex 5_figs.html tex/*
	$(xl) paper.tex 
	bibtex paper
	$(xl) paper
	$(xl) paper
	mv paper.pdf outputs

xl            = xelatex -interaction=batchmode
short_version = "\def\short{1} \input{slides.tex}"
long_version  = slides.tex

outputs/pres_short.pdf: slides.tex 5_figs.html tex/* 
	$(xl) $(short_version)
	bibtex slides
	$(xl) $(short_version)
	$(xl) $(short_version)
	mv slides.pdf outputs/pres_short.pdf

outputs/pres_long.pdf: slides.tex 5_figs.html tex/* 
	$(xl) $(long_version)
	bibtex slides
	$(xl) $(long_version)
	$(xl) $(long_version)
	mv slides.pdf outputs/pres_long.pdf


