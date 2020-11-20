Shield: [![CC BY 4.0][cc-by-shield]][cc-by]

This work is licensed under a
[Creative Commons Attribution 4.0 International License][cc-by].

[![CC BY 4.0][cc-by-image]][cc-by]

[cc-by]: http://creativecommons.org/licenses/by/4.0/
[cc-by-image]: https://i.creativecommons.org/l/by/4.0/88x31.png
[cc-by-shield]: https://img.shields.io/badge/License-CC%20BY%204.0-lightgrey.svg

# Objetives

R-scripts for evaluating demographic changes in Denmark for the period 2008-2020 with a focus on immigrants and their descendants.  

# Data

All the data I use here are public from the following data sources:

  - [Statistic Denmark](https://www.dst.dk/en): R-package [danstat](https://cran.r-project.org/web/packages/danstat/vignettes/Introduction_to_danstat.html).
  - [Geographic Information System of the Commission](https://ec.europa.eu/eurostat/web/gisco): R-package [giscoR](https://dieghernan.github.io/giscoR/) 

# Codes

Each R-script is independent, with enough information for downloading the required data and produces the plots.  

  - *DK_adm_units*: get *sf* files of Denmark at diverse administrative unit levels (i.e. country, regions, provinces, LAUs, and communes).
  - *pop_pyramid*: population pyramid of Denmark.
  - *pop_growth_DK*: population growth in Denmark.
  - *pop_growth_LAUs*: population changes by Local Administrative Units (LAUs).
  - *migrants_LAUs*: immigrants and their descendants bu LAUs. 

# Notes

I have created this repository for sharing some R-scripts that I have developed during my work as postdoctoral researcher at Aalborg University, in the project "[Global flows of migrants and their impact on north European welfare states - FLOW](https://www.flow.aau.dk/)".

My only aim with this repository is that it serves for learning R. It is not endorsed by the university or the project, and it is not maintained. For more information about migration and the project outcomes please visit the project’s website: https://www.flow.aau.dk. 
