# Siberian jays help conspecifics in distress regardless of social familiarity
## Andrea Meltzer, Eli D. Strauss, Michael Griesser
Data and code associated with manuscript "Siberian jays help conspecifics in distress regardless of social familiarity".
First part of script "Analysis_distress_2024.R" for Results (Tables and Figures) for Main text, second part for Supplemental Material (Tables and Figures).

Description of "mydata.csv":
- target.ID: factor with levels 1-27, for individual identities
- category: 0-4: response category (to playback)
- treatment: playback treatment: "distress familiar"   "distress unfamiliar" "social unfamiliar"   "pine grosbeak"
- status_caller: breeding status for playback of "distress familiar". "br"=breeder and "non-breeder"
- calling: calling in response to hearing playback (all call-types)
- calling_distress: giving distress call in response to hearing playback
- min_dist: minimum distance to playback (0-5m, where the feeding dvice was placed at 5m)
- approach_duration: duration (s) staying closer than 5m from the playback
- reaction_duration: time (s) from first reaction to playback to returning to the feeding station
- OBS_PercEatDyad: measure of social bond before playback of "distress familiar": Social Observation before: [s] focal and caller feeding in dyad devided by [s] both are present -> percentage of time feeding together
