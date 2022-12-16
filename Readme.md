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
2. Processed data: Also included in the archive, but not necessary for replication.  
    1. Survey data (Marienthal and Control towns): 
    *jobguarantee/2021-02-survey-data-processed*,
    *jobguarantee/2022-02-survey-data-processed*
    2. Admin data (Marienthal and Control towns): *jobguarantee/2020-09-admin-data-processed/*, *jobguarantee/2020-12-control-admin-data-processed*,
    *jobguarantee/2021-09-admin-data-processed*,
    *jobguarantee/2022-02-admin-data-processed*
    3. Aggregate data: 
    *jobguarantee/2020-09-municipal-data-processed*,
    *jobguarantee/2021-09-municipal-data-processed*,
    *jobguarantee/2022-02-municipal-data-processed*.
    4. In the same folder as the code: 
      *synthetic_control_weights.csv* (synthetic control weights), *synthetic_permutation_weights.csv*,
      *variable_description.csv* (variable descriptions).
3. Figures and tables:
    - Saved in subfolder */Figures* of code folder.




## Structure of code

All of our empirical results can be produced from the raw data by running *master.R*.


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



