# Local Predictability in High Dimensions

This repository contains the source code for the paper **Local Predictability in High Dimensions** by [Adämmer, Lehmann and Schüssler (2025)](https://doi.org/10.1080/07350015.2025.2526424).

The forecasting method introduced in the paper is available through our R-Package [hdflex](https://github.com/lehmasve/hdflex). If you use the code or the forecasting method provided in this repository, please cite the associated paper [see Citation](#citation).

## Prerequisites

*   **R**: The code is written in R.
*   **R Packages**: Required R packages are listed in the `renv.lock` file.
*   **Jupyter Notebook Environment**: To run the `.ipynb` scripts (e.g., Jupyter Lab, VS Code with Jupyter extension).

## Setup and Installation

1.  **Clone the repository:**
    ```bash
    git clone <repository-url>
    cd LocalPredictability
    ```

2.  **Restore R Environment:**
    This project uses `renv` to manage R package dependencies. To restore the environment:
    ```R
    # Within an R session in the project root directory
    if (!require("renv")) install.packages("renv")
    renv::restore()
    ```

3.  **Install Custom `eDMA` Package:**
    For the PCDMA benchmark method, a modified version of the `eDMA` R-Package is used. Install it from the local `.tar.gz` file:
    ```R
    # Within an R session
    install.packages("eDMA_1.5-3.tar.gz", repos = NULL, type = "source")
    ```
    **Note**: The CRAN version of `eDMA` may not support all required features (e.g., density forecast evaluation).

## Data

Due to licensing constraints, not all data used in the paper can be shared via this repository.

* **Finance application**: Daily aggregate US stock return forecasts are based on time series from the [CRSP](https://www.crsp.org) and [Datastream](http://www.lseg.com/en/data-analytics) databases, as well as newspaper articles from *The New York Times* and *The Washington Post*, collected using [NexisUni](https://www.lexisnexis.com/en-int/products/nexis-uni).
* **Inflation application**: We use the quarterly inflation dataset compiled by [Koop and Korobilis (2023)](https://sites.google.com/site/dimitriskorobilis/matlab/vbdvs).

Please refer to the paper for detailed descriptions of data sources and preprocessing steps. Scripts in the `Code/` folder expect data to be placed in the `Data/` directory.

## Code Structure

The `Code/` directory contains all core scripts and notebooks:

### Helper R Scripts
* [`_cmodels.R`](Code/_cmodels.R): Functions for generating (part of the) F-signals.
* [`_fmodels.R`](Code/_fmodels.R): Functions for fitting benchmark models.
* [`_helpers.R`](Code/_helpers.R): Utility functions (e.g., notebook-to-R script conversion).
* [`_sim.R`](Code/_sim.R): Simulation data generation.

### Jupyter Notebooks
* **Signal Preparation**
    * [`finance_signal_sets.ipynb`](Code/finance_signal_sets.ipynb): Prepares signals for the finance application.
    * [`inflation_signal_sets.ipynb`](Code/inflation_signal_sets.ipynb): Prepares signals for the inflation application.
* **Main Analysis**
    * [`finance_main_script.ipynb`](Code/finance_main_script.ipynb): Runs forecasting and evaluation for the finance application.
    * [`inflation_main_script.ipynb`](Code/inflation_main_script.ipynb): Runs forecasting and evaluation for the inflation application.
    * [`simulation.ipynb`](Code/simulation.ipynb): Runs the simulation study and outputs.

## Running the Analysis

1. **Set Working Path**
    Each notebook defines a `path` variable. Ensure it is set to the root of this project on your local system.

2. **Execution Order**
    Run signal generation notebooks before main analyses:
    ```text
    1. Code/inflation_signal_sets.ipynb (if applicable)
    2. Code/finance_signal_sets.ipynb (if applicable)
    3. Code/inflation_main_script.ipynb
    4. Code/finance_main_script.ipynb
    5. Code/simulation.ipynb
    ```

3. **Notebook Execution**
    Open each `.ipynb` file in a Jupyter environment and execute cell-by-cell. R scripts (e.g., `_cmodels.R`) are sourced as needed.

## Results

Running the analysis scripts will generate:

* Processed datasets and intermediate objects.
* Forecast evaluation tables and plots.

Outputs are saved in the `Results/` directory, organized as follows:

* `Results/ES1/` — Finance application  
* `Results/ES2/` — Inflation application  
* `Results/SIM/` — Simulation study

## Citation

If you use this repository or our forecasting method (`hdflex`), please cite the following paper:

> Adämmer, P., Lehmann, S., & Schüssler, R. (2025). *Local Predictability in High Dimensions*. Journal of Business & Economic Statistics, 1-24. [https://doi.org/10.1080/07350015.2025.2526424](https://doi.org/10.1080/07350015.2025.2526424)