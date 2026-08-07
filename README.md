## Modeling coral reef restoration outcomes for economic analysis

Hello and welcome to Tara's github.

### Quick Start

1. Copy the folder structure of this repository, i.e. ensure you have
   /data and /output folders in the same working directory as your
   R project file (`coral-econ.proj`).
2. Copy all files from the `/data` folder into your working directory.
   This provides the model with starting conditions related to 
   If you will be changing the underlying data
4. Run `model_v3.R`.

#### Running the script will:
* pull from the data files in `/data`
* save a `.xlsx` of model outcomes in `/output`

### Datasets
1. `centroid.csv` - centroid information (n=2404)
2. `sourcesink.csv` - reef edges information (n=73,571)
3. `composition.csv` - composition data by top ten taxa of the reef type

(See README.md in data folder for dataset structure)
