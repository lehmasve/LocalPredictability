# Local Predictability in High-Dimensions

This repository contains the source code for the paper **Local Predictability in High-Dimensions** by [Adämmer, Lehmann and Schüssler (2024)](https://dx.doi.org/10.2139/ssrn.4342487). The forecasting method introduced in the paper is available through our R-Package [hdflex](https://github.com/lehmasve/hdflex). Please remember to cite the paper when using the code from this repository or our forecasting method.

## Data

Due to licensing constraints, we are unable to freely upload the data to this platform.

* For our application on forecasting daily aggregate stock returns, we use time series collected from the [CRSP](https://www.crsp.org) and [Datastream](http://www.lseg.com/en/data-analytics) databases, as well as newspaper articles from *The New York Times* and *The Washington Post*. The text data were collected using the online tool [NexisUni](https://www.lexisnexis.com/en-int/products/nexis-uni). 
* For our application on forecasting quarterly inflation, we use the dataset (and forecasts) compiled by [Koop and Korobilis (2023)](https://sites.google.com/site/dimitriskorobilis/matlab/vbdvs).

## Notes

The folder **Code** contains the following functions and scripts:
* The file `_cmodels.R` contains functions to fit the models for (part of) the F-Signals
* The file `_fmodels.R` contains functions to fit the benchmark models
* The file `_helpers.R` contains several helper functions -- among others, a function to convert the jupyter notebooks to plain R-scripts
* The file `_sim.R` contains functions to generate the data for the simulation 
* The notebook `finance_signal_sets.ipynb` generates / preprocesses all signals used in the finance application
* The notebook `finance_main_script.ipynb` generates all results, plots, and tables for the application on forecasting daily aggregate US stock returns
* The notebook `inflation_signal_sets.ipynv generates / preprocesses all signals used in the inflation application
* The notebook `inflation_main_script.ipynb` generates all results, plots, and tables for the application on forecasting quarterly US inflation
* The notebook `simulation.ipynb` generates all results, plots, and tables for the simulation study

The file `renv.lock` is a lockfile with the current state of dependencies in the project library. The lockfile can be used to restore these dependencies as required.

For our benchmark method PCDMA, we use the R-Package [eDMA](https://cran.r-project.org/package=eDMA). However, to evaluate the density forecasting accuracy, some additional objects have to be returned, which was not supported in the package. Before running the code, you will have to install the adapted (local) version of the **eDMA** package. 
```r
install.packages("/eDMA_1.5-3.tar.gz", repos = NULL, type = "source")
