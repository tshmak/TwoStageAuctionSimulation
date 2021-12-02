# Scripts for running the simulations in the paper Two-Stage Auction Mechanism for Long-Term Participation in Crowdsourcing

### Pre-requisites
- R version 3.5+
- The `devtools` package. (`install.packages("devtools")` to install from CRAN)
- The [Tmisc](https://github.com/tshmak/Tmisc) library
- The flexalloc submodule (see below)

### Download submodule (flexalloc)
```
git submodule init
git submodule update
```

### Installation of Tmisc
```
devtools::load_all("/path/to/Tmisc")
```

### Simulations
First, go into the `simulations` folder. 

To run the sensitivity analyses (Table II), run the following scripts: 
- `sensitivity3A.R` 
- `sensitivity3Aresults.R` 
- `sensitivity3Agraphs.R` 

To run the simulations for Figures 2-5, run the following scripts: 
- `sim2.R`
- `sim2results.R`
- `sim2graphsB.R`
