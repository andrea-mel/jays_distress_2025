# Siberian jays help conspecifics in distress regardless of social familiarity
## Andrea Meltzer, Eli D. Strauss, Michael Griesser
Data and code associated with manuscript "Siberian jays help conspecifics in distress regardless of social familiarity".
First part of script "Analysis_distress_2025.R" for Results (Tables and Figures) for Main text, second part for Supplemental Material (Tables and Figures).

Description of "mydata.csv":
- target.ID: factor with levels 1-27, for individual identities
- category: 0-5: response category (to playback)
- treatment: playback treatment: "distress familiar"   "distress unfamiliar" "social unfamiliar"   "pine grosbeak"
- status_caller: breeding status for playback of "distress familiar". "breeder" and "non-breeder"
- length_call: playback track: length [s] of distress call
- call_nr_elements: playback track: number of distress call elements within track
- call_elongated_duplication: playback track: any distress call elements duplicated? no/yes, plus number of duplications
- calling: number of calls in response to hearing playback (all call-types)
- calling_distress: number of distress calls given in response to hearing playback
- min_dist: minimum distance to playback (0-5m, where the feeding dvice was placed at 5m)
- approach_duration: duration (s) staying closer than 5m from the playback
- reaction_duration: time (s) from first reaction to playback to returning to the feeding station
- OBS_PerchDurPerEv: measure of social bond before playback of "distress familiar": Social Observation before: mean co-perching bout length for each dyad, where perching refers to individuals being present on the feeding device
