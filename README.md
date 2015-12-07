# Example remake workflow

This code repository contains code for analysing the [Biomass and Allometry Database (BAAD)](https://github.com/dfalster/baad), using the [remake](https://github.com/richfitz/remake) package for R. See the info there for installation instructions.

## Running

Download this repo, then open a new R session with this project set as working directory. We use a number of packages, these can be easily installed by remake:

```r
remake::install_missing_packages()
```

Then, to generate the figures and tables, run

```r
remake::make("")
```

To generate the knitr report, run

```r
remake::make("report.docx")
```
