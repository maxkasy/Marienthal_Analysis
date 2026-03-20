# Analysis code for *Employing the unemployed of Marienthal: Evaluation of a guaranteed job program*

The following readme gives an overview of the replication files for the empirical analysis in our paper.
This complements the replication files for the experimental design, which can be found in a separate archive, [https://github.com/maxkasy/Marienthal](https://github.com/maxkasy/Marienthal).

## Data access

Data protection regulations prohibit us from sharing any of the data directly.
The AMS (Austrian labor market service) has, however, agreed to handle data access requests. 
Please address such requests to [statistik.niederoesterreich@ams.at](mailto:statistik.niederoesterreich@ams.at).
Data will be shared in the form of an archive with sub-folders, as described below.


## Structure of folders

1. Raw data: Read from the data archive. The root path has to be set in *master.R*.
    1. Survey data (Marienthal and Control towns): *jobguarantee/2021-02-survey-data-raw/*, *jobguarantee/2022-02-survey-data-raw/*.
    2. Admin data (Marienthal and Control towns): *jobguarantee/2021-09-admin-data-raw/*, *jobguarantee/2022-02-admin-data-raw/*.
    3. Aggregate data: */jobguarantee/2021-09-municipal-data-raw/* (outcomes), *jobguarantee/2020-09-municipal-data-raw/* (controls)
    4. Hazard rates data: */jobguarantee/2024-04-hazard-rates-raw/* (provided by AMS), */jobguarantee/2024-04-hazard-rates-processed/* (merged with AMDB)
    5. Earnings analysis data for Figure 8 / and earnings for cost comparison (AMDB-based): *jobguarantee/2025-10-admin-earnings-raw/*, *jobguarantee/2025-10-admin-earnings-processed/*

2. Processed data: Also included in the archive, but not necessary for replication.  
    1. Survey data (Marienthal and Control towns): 
    *jobguarantee/2021-02-survey-data-processed*,
    *jobguarantee/2022-02-survey-data-processed*
    2. Admin data (Marienthal and Control towns): *jobguarantee/2020-09-admin-data-processed/*, *jobguarantee/2020-12-control-admin-data-processed*,
    *jobguarantee/2021-09-admin-data-processed*,
    *jobguarantee/2022-02-admin-data-processed*
    3. Aggregate (municipal) data: 
    *jobguarantee/2020-09-municipal-data-processed*,
    *jobguarantee/2021-09-municipal-data-processed*,
    *jobguarantee/2022-02-municipal-data-processed*.
    4. In the same folder as the code: 
      *synthetic_control_weights.csv* (synthetic control weights), *synthetic_permutation_weights.csv*,
      *variable_description.csv* (variable descriptions).

3. Figures and tables:
    - Saved in subfolder */Figures* of code folder.

## Structure of code

Our empirical results can be produced from the raw data by running *master.R*, after setting the data root path.



### Data preparation

1. Survey data: *0a_survey_responses_aggregation_2021_2022.R*.
1. Administrative data:  
    *0b_i_admin_data_prep_2021_2022.R* (Marienthal, 2021 and 2022),
    *0b_i_admin_data_prep_control_towns-2021_2022.R* (Control towns, 2021 and 2022).
1. Synthetic control data: 
    *0c_i_synth_data_prep_outcome_data.R*,
    *0c_ii_synth_data_prep_control_data.R*,
    *0c_iii_synth_data_prep_merge_control_outcome_data.R*,
    *0c_iv_synth_data_prep_merge_new_lzbl.R*.
1. Hazard rates data:
    *4a-hazard_rates_prep_data.R*.


### Experimental analysis for Marienthal

- *1b_ii_plot_functions.R*
- *1b_i_Inference_functions.R*
- *1b_Marienthal_responses_analysis.R*


### Synthetic control analysis

- *2a_synthetic_control_analysis.R*
- *2b_synthetic_control_plots.R*

### Analysis comparing to control town individuals

- *3b_i_Control-town_individuals_analysis_2021.R*
- *3b_ii_Control-town_individuals_analysis_2022.R*
- *3c_leebounds.R*

### Hazard rates analysis

- *4b-hazard_rates_analysis.R*

### Cost comparison analysis

- *5-cost-comparison_analysis.R* (in AMS computing environment)
- *5b-cost-comparison_plot.R*

### Differential survey response analysis

- *6-differential_response_analysis.R*

### Employment status analysis

- *7a-cumulative_employment_prep.R*
- *7b-cumulative_employment_analysis.R*

### Earnings analysis (restricted environment for extraction)

- *8a-admin-earnings_prep_costs.R*

### Notes on restricted-computing steps

- *4a-hazard_rates_prep_data.R* merges AMS-provided short-term unemployment IDs with AMDB spell data. This must be run in a computing environment with access to [Arbeitsmarktdatenbank (AMDB)](https://arbeitsmarktdatenbank.at/), which can be obtained for research purposes at [https://arbeitsmarktdatenbank.at](https://arbeitsmarktdatenbank.at). The output is a processed CSV used by 4b-hazard_rates_analysis.R.

- *5-cost-comparison.R* uses protected person-level cost data and must be run in the AMS Niederösterreich computing environment where those files are stored.

- *8a-admin-earnings_prep_costs.R* contains an extraction step that requires access to [Arbeitsmarktdatenbank (AMDB)](https://arbeitsmarktdatenbank.at/)  earnings data; the script is structured as a “VM PART” (extraction of relevant AMDB data) and “LOCAL PART” (analysis). The output is then used for Figure 8 and the cost-related tables.

## How to run

1. Open *master.R* and set the data root path:
  - Edit veracrypt_path

2. Ensure all required packages are installed (see “Software and packages” below).

3. Run *master.R* from the code directory (so relative source() paths work).

4. Outputs (figures/tables) will be written to Figures/.

## Software and packages

The analysis is written in R and uses the following packages:
`tidyverse`, `readr`, `lubridate`, `readxl`, `nbpMatching`, `furrr`, `estimatr`, `kableExtra`, `patchwork`, `ggtext`, `data.table`, `broom`, `zoo`.

## Reproducible environment using Docker

To ensure future reproducibility of our analysis, we have included a *Docker* file.
Executing the analysis in a *Docker* container will ensure exact reproduction of our results, on any machine, and independently of future updates to the software (*R*) and packages.

To execute the analysis in such a container, execute the following steps:
- Make sure *Docker* and *Docker Desktop* are installed on your machine.
- Copy the data files (obtained from the *AMS*), in their respective folders, into a subfolder of the folder containing this code.
- Change *veracrypt_path* on line 20 of *master.R* to point to this subfolder.
- Open the terminal and set your path to the repository that contains the *Dockerfile*.
- Execute the *docker build* command:  
*docker build --tag marienthal_analysis:latest*  
This may take several minutes but only needs to be done once.
-  After the image has been built, execute the *docker run* command to start the container:  
*docker run --rm -d -p 8787:8787 marienthal_analysis:latest* 
- To access the running container, click on its port *8787:8787* hyperlink in *Docker Desktop* in the *Containers* tab. The browser will open a new tab with RStudio from where all files inside the container can be accessed and the analysis can be replicated.


## Pre-registration and code

The experiment is pre-registered at the AEA RCT registry, [https://www.socialscienceregistry.org/trials/6706](https://www.socialscienceregistry.org/trials/6706).
The code for the experiment design is publicly available at [https://github.com/maxkasy/Marienthal](https://github.com/maxkasy/Marienthal).
The code for the empirical analysis is publicly available at [https://github.com/maxkasy/Marienthal_Analysis](https://github.com/maxkasy/Marienthal_Analysis).



