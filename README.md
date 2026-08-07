## Modeling coral reef restoration outcomes for economic analysis

Hello and welcome to Tara's github.

### Quick Start

1. Copy the folder structure of this repository, i.e. ensure you have
   /data and /output folders in the same working directory as your
   R project file.
3. Open `ch2model_v3.R`.

#### Running the script will:
* build the 10 coral growth forms and save a combined preview image
  (`0_model/growth_form_shapes.png`/`.pdf`) so you can see what each
  functional type looks like at its current size and shape settings
* create a timestamped output folder under `0_model/1_simulation_output/`
* run the simulation for `runs` runs of `timesteps` weekly steps each,
  printing progress to the console as it goes
* save a `.xlsx` of community metrics per timestep, plus periodic snapshots
  of the 3D world, into that output folder
