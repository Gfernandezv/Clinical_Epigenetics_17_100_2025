# Clinical Epigenetics Thesis

[![Binder](https://mybinder.org/badge_logo.svg)](https://mybinder.org/v2/gh/Gfernandezv/Thesis/HEAD)

This repository contains the code and data used for a thesis project examining the effects of various constructs in a Huntington's disease mouse model. The analysis combines behavioural tests (e.g., Barnes Maze, rotarod, object recognition) with qPCR and western blot results. Scripts in the `Scripts` directory generate summary statistics and figures for each dataset.

## Requirements

The environment was built using R `r-2022-01-01` as noted in `runtime.txt`. Package dependencies can be installed by running:

```r
source("Install.R")
```

`Install.R` installs packages such as `ggplot2`, `dplyr`, `cowplot`, and other tidyverse utilities required for the graphs and statistical calculations.

## Data

All raw data files are stored in the `Data/` directory. Key files include:

- `Main.csv` / `Main.xlsx` – metadata describing each subject.
- `BM.Data.csv` and related files – Barnes Maze measurements.
- `RR.Data.csv` – rotarod performance.
- `RTqPCR.Data.csv` – RT-qPCR Ct values.
- `WB.inj.Data.csv` – western blot intensity readings.

The repository also contains two poster PDFs (`poster_ASCB2024.pdf` and `poster_ASCB2024_2.pdf`, ~10 MB each) used for presentation purposes.

## Running the analysis

Scripts under `Scripts/` produce individual figures. For example,

```bash
Rscript Scripts/RTqPCR.Graphs.R
```

creates RT‑qPCR plots and saves them to a `graphs/` folder (created automatically). Other scripts follow the same pattern (`BM.Graphs.R`, `WB.inj.Graphs.R`, `RR.Graphs.R`, etc.).

Figures are written as `.png` files in the `graphs/` directory and can be inspected to interpret behavioural or molecular changes across experimental groups.

## Repository structure

```
Data/       # raw input data (CSV/XLSX files)
Scripts/    # analysis scripts generating statistics and plots
Thesis/     # RStudio project files
playground/ # assorted experimental code
```

