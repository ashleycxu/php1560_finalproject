# PHP1560 Final Project

**Research Question:** \
What factors impact disease recurrence and survival in breast cancer patients? More specifically, how can we model the relationship between age/tumor grade and the time to recurrence/death?

**Data:** \
The two breast cancer datasets used for this analysis are the `rotterdam` data from the `survival` package and the `mammaca` data from the `psfmi` package in R.

**Analysis Setup:** 
* The first main component of the analysis involves simulating recurrence and survival data for the `mammaca` dataset based on the information in the `rotterdam` dataset.
* The second main component of the analysis involves fitting a Cox model to the data to analyze the relationship between different covariates and the time to recurrence/death.

**Scripts:** 
* The `pipeline.R` script runs the entire data analysis, simulation, and visualization pipeline.
* The `prelim_vis.R` script contains code for generating the initial tables and visuals for the `rotterdam` data.
* The `simulation.R` script contains functions for simulating new recurrence and survival data.
* The `cox.R` script contains functions for computing Cox models and plotting survival curves.

**Results:** \
The results folder contains results from preliminary data analysis as well as the results after simulation and Cox modeling.
* Summary statistic tables for the `rotterdam` and `mammaca` data
* Plots from fitting a linear regression model looking at age, tumor grade, and time to recurrence/death (`rotterdam` data and simulated `mammaca` data)
* Plot results from fitting a Weibull distribution to the time to recurrence/death in the `rotterdam` data
* Histograms showing the distribution of time to recurrence/death in the `rotterdam` data and simulated `mammaca` data
* Csv files with coefficients from Cox models for time to recurrence/death in the `rotterdam` data and simulated `mammaca` data
* Recurrence and survival curves from fitting the Cox models for different covariates
