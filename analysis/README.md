# Processing and Analysis Code

The flow of processing was as follows:
- **Combined Processing.ipynb:** Python code for calculating the phase of synchronization taps, the inter-tap intervals of continuation taps, and other scores from the raw response data. Also includes code for processing spontaneous motor tempos and audiometry data. Outputs the file processed_data/processed_taps.csv.
- **Combined Data Review.ipynb:** Python code for visualizing summary reports of each participant's performance on the task. Used primarily for identifying participants for exclusion.
- **Combined Analysis.ipynb:** Python code for loading the preprocessed data, splitting it into separate synchronization and continuation tapping datasets (sync_data.csv and cont_data.csv), and generating all figures in the manuscript. Also includes some additional preprocessing, such as converting the phases of synchronization taps to their percent asynchrony. Additionally contains the code used to calculate the age and gender distributions of participants using survey data that we have _not_ made open-access for participant privacy.
- **Combined Stats.R:** R code for running all statistical models used in our study. Uses the fully preprocessed sync_data.csv and cont_data.csv datasets output by Combined Analysis.ipynb.
