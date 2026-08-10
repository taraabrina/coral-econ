### Datasets
1. centroid.csv - centroid information (n=2404)
* ReefID
* Reef Area
* Latitude
* Longitude
* Predicted Type
* Starting HCC
* Gravity penalty
* Typhoon Probability
* Bleaching Probability
2. sourcesink.csv - reef edges information (n=73,571)
* sinkxsource_i
* SinkReef ID
* Latitude Sink
* Longitude Sink
* SourceReef ID
* Latitude Source
* Longitude Source
* Probability of Settlement (Source->Sink)
* Retention
3. composition.csv - composition data by top ten taxa of the reef type
* Reef Type
* HCC ave
* Taxonomic Amalgamation Unit (tau)
* tau truncated
* Contribution to average HCC
* Standard Error
* Growth per Year (mm)
* Bleaching Mortality Rate
* Typhoon Mortality Rate
* Carrying Capacity/Max HCC
* Percent contribution to average HCC of reef type
* Bleaching Mortality Rate of TAU (Expected mortality rate after DHW > 8)
* Typhoon Mortality Rate of TAU (Expected mortality rate after typhoon winds > 118 kmph within 500 km)
4. type.csv - reef type summaries
* Reef Type
* Maximum HCC (hacc)
* Growth rate (mm per year)
