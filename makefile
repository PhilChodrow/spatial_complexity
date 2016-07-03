# filesets

_outputs = outputs/paper.pdf outputs/slides.pdf


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
	pdflatex -interaction=batchmode paper.tex 
	bibtex paper
	pdflatex -interaction=batchmode paper
	pdflatex -interaction=batchmode paper
	rm paper.bbl paper.blg paper.log paper.aux paper.fdb_latexmk paper.fls
	mv paper.pdf outputs

outputs/slides.pdf: slides.tex 5_figs.html tex/* 
	xelatex -interaction=batchmode slides.tex 
	bibtex slides
	xelatex -interaction=batchmode slides
	xelatex -interaction=batchmode slides
	rm slides.bbl slides.blg slides.log slides.aux slides.out slides.snm slides.toc slides.nav slides.fls slides.fdb_latexmk
	mv slides.pdf outputs

# Eventually include a file for rendering a slide deck in here too. 


