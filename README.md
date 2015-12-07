# Example remake workflow

This code repository contains code for analysing the [Biomass and Allometry Database (BAAD)](https://github.com/dfalster/baad), using the [remake](https://github.com/richfitz/remake) package for R.

## Installing remake

First install some dependencies from cran as follows:

```r
install.packages(c("R6", "yaml", "digest", "crayon", "optparse"))
```

Now we'll install some packages from [github](github.com). For this, you'll need the package [devtools](https://github.com/hadley/devtools). If you don't have devtools installed you will see an error "there is no package called 'devtools'"; if that happens install devtools with `install.packages("devtools")`.


Then install the following two packages

```r
devtools::install_github("richfitz/storr")
```
and

```r
devtools::install_github("richfitz/remake")
```
See the info in the [remake readme](https://github.com/richfitz/remake) for further details if needed.

## Running

Download this repo, then open a new R session with this project set as working directory. We use a number of packages, these can be easily installed by remake:

```r
remake::install_missing_packages()
```

Then, to generate the figures and tables, run

```r
remake::make()
```

To generate the knitr report, run

```r
remake::make("report.docx")
```
