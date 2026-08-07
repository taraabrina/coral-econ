## Modeling coral reef restoration outcomes for economic analysis

Hello and welcome to Tara's github.

### Quick Start

1. Copy the folder structure of this repository, i.e. ensure you have
   /data and /output folders in the same working directory as your
   R project file (`coral-econ.proj`).
2. Copy all files from the /data folder into your working directory.
   This provides the model with starting conditions related to 
   If you will be changing the underlying data
4. Run `model_v3.R`.

#### Running the script will:
* pull from the data files in /data
* save a `.xlsx` of model outcomes in /output

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
