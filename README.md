# Shiny App: AdRI_GAMLSS

![](https://img.shields.io/github/license/SandraKla/AdRI_GAMLSS.svg)
![](https://img.shields.io/github/last-commit/SandraKla/AdRI_GAMLSS.svg)
![](https://img.shields.io/github/languages/count/SandraKla/AdRI_GAMLSS.svg)
![](https://img.shields.io/github/languages/top/SandraKla/AdRI_GAMLSS.svg)

<img src="www/Logo.svg" width="225px" height="150px" align="right"/>

**Shiny App for calculating Age-dependent Reference Intervals!**

This Shiny App was developed to create **A**ge-**d**ependent **R**eference **I**ntervals (**AdRI**) using [Generalized Additive Models for Location, Scale and Shape (GAMLSS)](https://github.com/SandraKla/AdRI_GAMLSS/wiki).

*** **Warning!** This Shiny App has not been enough validated for the basis of a medical diagnosis! There is no warranty for the app and/or the reference intervals! ***

<img src="www/shiny_overview.png" align="center"/>

## Installation 

**Method 1:**
Use the function ```runGitHub()``` from the package [shiny](https://cran.r-project.org/web/packages/shiny/index.html):

```bash
if("shiny" %in% rownames(installed.packages())){
  library(shiny)} else{
  install.packages("shiny")
  library(shiny)}
runGitHub("AdRI_GAMLSS", "SandraKla")
```

**Method 2:**
Download the Zip-File from this Shiny App. Unzip the file and set your working direction to the path of the folder. 
The package [shiny](https://cran.r-project.org/web/packages/shiny/index.html) must be installed before using the Shiny App:

```bash
# Test if shiny is installed:
if("shiny" %in% rownames(installed.packages())){
  library(shiny)} else{
  install.packages("shiny")
  library(shiny)}
```
And then start the app with the following code:
```bash
runApp("app.R")
```

All required packages are downloaded when starting this app or imported if they already exist. For more information about the required packages use the [Wiki](https://github.com/SandraKla/AdRI_GAMLSS/wiki).
