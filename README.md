# Siberian jays help conspecifics in distress regardless of social familiarity
## Andrea Meltzer, Eli D. Strauss, Vlad Demartsev, Michael Griesser
Data and code associated with manuscript "Siberian jays help conspecifics in distress regardless of social familiarity".

**Find exemplary playback stimuli (presented in Figure 1 in manuscript) together with their spectograms in folder "Example_calls_Figure_1"**


~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Script "Spectral_cross_correlation.R"
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
- Calculates SNR
- Calculates spectral cross correlation for each distress call element in the distress call recordings.
- Find these recordings in folder "WAV". (Note that these are the recordings used for the spectral cross correlation. For the playback experiment, some of those were modified (e.g. elements duplicated), see information on this in "mydata.csv" - described below)
- Uses "selection_table.txt" which contains information on beginning/end of each element to be analysed.


~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Script "Analysis_distress_2025.R"
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
--- First part for Results (Tables and Figures) for Main text, second part for Supplemental Material (Tables and Figures).

Description of "mydata.csv":
- target.ID: factor with levels 1-27, for individual identities
- response category: 0-5: response strength category (to playback)
- treatment: playback treatment: "distress familiar"   "distress unfamiliar" "social unfamiliar"   "pine grosbeak"
- status_caller: breeding status for playback of "distress familiar". "breeder" and "non-breeder"
- length_call: playback track: length [s] of distress call
- call_nr_elements: playback track: number of distress call elements within track
- call_elongated_duplication: playback track: any distress call elements duplicated? no/yes, plus number of duplications
- order: playback order: "exp"=distress familiar, "ctr"=distress unfamiliar, "S"=social unfamiliar, "P=pine grosbeak"
- date: date of experiment
- calling: number of calls in response to hearing playback (all call-types)
- calling_distress: number of distress calls given in response to hearing playback
- min_dist: minimum approach distance to playback speaker (0-5m, where the feeding dvice was placed at 5m)
- approach_duration: duration (s) staying closer than 5m from the playback
- reaction_duration: time (s) from first reaction to playback to returning to the feeding station
- OBS_PerchDurPerEv: measure of social bond before playback of "distress familiar": Social Observation before: mean co-perching bout length for each dyad, where perching refers to individuals being present on the feeding device
