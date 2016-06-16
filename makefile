# filesets

_outputs = outputs/paper.pdf 


# Building

all: outputs

outputs: $(_outputs)


# Cleaning

clean: # output
	rm -rf outputs/paper.pdf 5_figs.html figs/*

clean_throughput: 
	rm -rf throughput/*

clean_data: 
	rm -rf data/*

# Dependencies

data/states/*: assumptions/cities.csv assumptions/lookup.csv
	Rscript 1_get_states.R

data/cities: data/states/*
	Rscript 2_make_cities.R

throughput/info_cache.csv: data/cities
	Rscript 3_local_info.R

throughput/loss_curves.csv: data/cities
	Rscript 4_cluster.R

5_figs.html: 5_figs.Rmd 
	Rscript -e 'rmarkdown::render("5_figs.rmd")'

outputs/paper.pdf: paper.tex 5_figs.html tex/*
	pdflatex -interaction=batchmode paper.tex 
	bibtex paper
	pdflatex -interaction=batchmode paper
	pdflatex -interaction=batchmode paper
	rm paper.bbl paper.blg paper.log paper.aux
	mv paper.pdf outputs


# Eventually include a file for rendering a slide deck in here too. 


