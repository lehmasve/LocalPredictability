# Local Predictability in High Dimensions

This repository contains the source code for the paper **Local Predictability in High Dimensions** by [Adämmer, Lehmann and Schüssler (2024)](https://dx.doi.org/10.2139/ssrn.4342487).

The forecasting method introduced in the paper is available through our R-Package [hdflex](https://github.com/lehmasve/hdflex). Please remember to cite the paper when using the code from this repository or our forecasting method.

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

3.  **Install Custom eDMA Package:**
    For the PCDMA benchmark method, a modified version of the `eDMA` R-Package is used. Install it from the local `.tar.gz` file:
    ```R
    # Within an R session
    install.packages("eDMA_1.5-3.tar.gz", repos = NULL, type = "source")
    ```
    This step is crucial as the standard CRAN version of `eDMA` may not support all functionalities required by the scripts (e.g., for density forecasting accuracy evaluation).

## Data

Due to licensing constraints, we are unable to freely upload all the data used in our research to this platform.

*   For our application on **forecasting daily aggregate stock returns**, we use time series collected from the [CRSP](https://www.crsp.org) and [Datastream](http://www.lseg.com/en/data-analytics) databases, as well as newspaper articles from *The New York Times* and *The Washington Post*. The text data were collected using the online tool [NexisUni](https://www.lexisnexis.com/en-int/products/nexis-uni).
*   For our application on **forecasting quarterly inflation**, we use the dataset (and forecasts) compiled by [Koop and Korobilis (2023)](https://sites.google.com/site/dimitriskorobilis/matlab/vbdvs).

Please see the paper for detailed information about the specific data (sources) and preprocessing steps. The scripts in `Code/` expect data to be structured in the `Data/` directory as per the project's conventions.

## Code Structure

The `Code/` directory contains the core scripts and functions:

*   **Helper R Scripts:**
    *   [`_cmodels.R`](Code/_cmodels.R): Functions to fit models for (part of) the F-Signals.
    *   [`_fmodels.R`](Code/_fmodels.R): Functions to fit the benchmark models.
    *   [`_helpers.R`](Code/_helpers.R): Various helper functions, including one to convert Jupyter notebooks to plain R-scripts (`convert_ipynb_to_r`).
    *   [`_sim.R`](Code/_sim.R): Functions to generate data for the simulation study.

*   **Jupyter Notebooks for Analysis:**
    *   Signal Generation:
        *   [`finance_signal_sets.ipynb`](Code/finance_signal_sets.ipynb): Generates/preprocesses all signals for the finance application.
        *   `inflation_signal_sets.ipynb`: (Assumed, based on pattern - `Code/inflation_signal_sets.ipynb` if present) Generates/preprocesses signals for the inflation application.
    *   Main Analysis & Results:
        *   [`finance_main_script.ipynb`](Code/finance_main_script.ipynb): Generates all results, plots, and tables for the application on forecasting daily aggregate US stock returns.
        *   [`inflation_main_script.ipynb`](Code/inflation_main_script.ipynb): Generates all results, plots, and tables for the application on forecasting quarterly US inflation.
        *   [`simulation.ipynb`](Code/simulation.ipynb): Generates all results, plots, and tables for the simulation study.

## Running the Analysis

1.  **Configure Path:**
    The Jupyter notebooks ([`finance_main_script.ipynb`](Code/finance_main_script.ipynb), [`inflation_main_script.ipynb`](Code/inflation_main_script.ipynb), [`simulation.ipynb`](Code/simulation.ipynb), etc.) start by setting a `path` variable.
    Ensure this path points to the root directory of this project on your system.

2.  **Execution Order:**
    It's generally recommended to run the signal generation notebooks before the main analysis scripts:
    *   First, run [`Code/finance_signal_sets.ipynb`](Code/finance_signal_sets.ipynb) (and `Code/inflation_signal_sets.ipynb` if applicable).
    *   Then, run the main analysis notebooks: [`Code/finance_main_script.ipynb`](Code/finance_main_script.ipynb), [`Code/inflation_main_script.ipynb`](Code/inflation_main_script.ipynb), and [`Code/simulation.ipynb`](Code/simulation.ipynb).

3.  **Running Notebooks:**
    Execute the `.ipynb` files cell by cell within a Jupyter environment (like Jupyter Lab or VS Code with the Jupyter extension). The helper R scripts (`_cmodels.R`, `_fmodels.R`, etc.) are sourced directly by these notebooks.

## Results

The analysis scripts will generate various outputs, including:
*   Processed data and intermediate results.
*   Final forecast evaluations, tables, and plots.
These are typically saved within the `Results/` directory, organized into subfolders like `ES1/` (finance application), `ES2/` (inflation application), and `SIM/` (simulation).

## Citation

If you use the code from this repository or our forecasting method (`hdflex`), please cite our paper:

Adämmer, P., Lehmann, S., & Schüssler, R. (2024). Local Predictability in High Dimensions. *Available at SSRN 4342487*. [https://dx.doi.org/10.2139/ssrn.4342487](https://dx.doi.org/10.2139/ssrn.4342487)