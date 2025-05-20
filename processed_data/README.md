# Processed Data from Both Experiments

This folder contains all processed versions of our data. A codebook for each data table is provided below.

## Codebook for Processed Musical Training Data (musicianship.csv)
Please note the information in this data file was extracted and summarized from raw responses to the full participant questionnaire, which we have not made open-access to protect participant privacy.

- **ID:** The ID number of the participant.
- **Age:** The age of the participant in years.
- **years_lessons:** The number of years the participant received formal lessons on at least one instrument (including voice).
- **years_since:** The number of years elapsed since the participant last had formal training on an instrument (including voice). 0 for any participants currently taking formal lessons at the time of the study.
- **Wind:** Indicates whether the participant has ever received formal training on a wind instrument ("yes" or "no").
- **Piano:** Indicates whether the participant has ever received formal piano training ("yes" or "no").
- **Strings:** Indicates whether the participant has ever received formal training on a string instrument ("yes" or "no").
- **Percussion:** Indicates whether the participant has ever received formal training on a percussion instrument ("yes" or "no").
- **Vocals:** Indicates whether the participant has ever received formal vocal training ("yes" or "no").

## Codebook for Stage-1 Processed Tapping Scores (processed_taps.csv)
This data file contains one row of data for each tap a participant made during the main (non-practice) experimental trials, and combines data from all participants from both experiments into a single data table.

- **subject:** The ID number of the participant that produced that tap.
- **experiment:** The experiment that tap originated from ("ITM" for Experiment 1 or "ITMSR" for Experiment 2).
- **version:** The code version of the experiment that tap originated from. Should always be 1.1 for both experiments.
- **experimenter:** The initials of the experimenter who conducted the session that tap originated from. Only available for Experiment 2.
- **handedness:** Indicates whether the participant who generated that tap was right ("R") or left ("L") handed. Note that we did not monitor whether the participant used their dominant hand on all trials.
- **smt:** The spontaneous motor tempo in milliseconds of the participant who generated that tap, as calculated from the self-paced tapping task at the start of the experiment.
- **block:** The trial block on which the tap took place (1-5).
- **trial:** The trial number during which the tap took place (1-120).
- **register:** The register to which the participant who generated that tap was assigned. All Experiment 1 trials are labeled "B" (both), whereas Experiment 2 trials are labeled either "L" (lower) or "U" (upper).
- **pitch:** The MIDI note number of the pitch used on the trial during which that tap occurred. See https://en.wikipedia.org/wiki/Piano_key_frequencies for a helpful reference table.  
- **ioi:** The interonset interval (in milliseconds) of the tones played during the synchronization phase of the trial on which that tap occurred.
- **hearing_threshold:** The participant's estimated hearing threshold for pure tones of the same pitch as used on that trial, based on their audiogram.
- **tap_type:** The phase of the trial during which the tap took place. "S" indicates the tap occurred during synchronization tapping. "C" indicates the tap occurred during continuation tapping. "SC" denotes the synchronization tap that triggered the first continuation tone.
- **interval_number:** Indicates the interval number during which the tap took place (1-23). For example, interval 2 encompasses the time between the onset of the second and third tone of the trial.
- **tap_duration:** How many milliseconds the participant's finger remained on the tapping pad while producing that tap.
- **peak_pressure:** The maximum pressure level measured during that tap. The force sensitive resistor's reading could be any integer from 0-255. Only available for Experiment 2.
- **tap_phase:** The phase at which the participant's tap onset occurred (from 0 to 2*pi) within the interval between two tones. Available only for S and SC taps.
- **tap_peak_phase:** The phase at which the partipant's peak tap pressure occurred (from 0 to 2*pi) within the interval between two tones. Available only for S and SC taps from Experiment 2.
- **iti:** (ITI = inter-tap interval) The number of milliseconds that elapsed between the previous tap's onset and the onset of the current tap. Available only for C taps.
- **peak_pressure_iti:** The number of milliseconds that elapsed between the previous tap's peak pressure and the present tap's peak pressure. Available only for C taps from Experiment 2.
- **rel_iti:** The inter-tap interval preceding the current tone, divided by the target interval (i.e., the interonset interval of the synchronization tones). Available only for C taps.
- **usable:** 1 if the tap was not marked for exclusion during preprocessing, 0 if it was marked for exclusion.

## Codebook for Stage-2 Processed Synchronziation Tapping Data (sync_data.csv)
Contains most of the same fields as musicianship.csv and processed_taps.csv. The following additional fields are specific to sync_data.csv only:

- **index:** Unused; added by Pandas upon saving to CSV.
- **tone_number:** Which tone (from 1 to 9) the tap was inferred to have been targeted at.
- **phase_async:** Asynchrony of the tap, expressed as a phase relative to the target tone. Negative phases indicate the tap was before the target tone, positive numbers indicate the tap was after the target tone.
- **perc_async:** Asynchrony of the tap, expressed as a percent of the interonset interval. Negative percents indicate the tap was before the target tone, positive numbers indicate the tap was after the target tone.
- **ms_async:** Asynchrony of the tap, expressed as an absolute number of milliseconds before or after the target tone. Negative numbers indicate the tap was before the target tone, positive numbers indicate the tap was after the target tone.

## Codebook for Stage-2 Processed Continuation Tapping Data (cont_data.csv)
Contains the same fields as musicianship.csv and processed_taps.csv combined.



