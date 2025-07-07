# Thesis
[![Binder](https://mybinder.org/badge_logo.svg)](https://mybinder.org/v2/gh/Gfernandezv/Thesis/HEAD)

## Installing Dependencies

All R package dependencies are listed in `Install.R`. To install them run either of the following commands from the project root:

```sh
# using Rscript
./install_packages.sh
```

or within an R session:

```R
source("Install.R")
```

The project uses R `r-2022-01-01` (see `runtime.txt`). For reproducible package versions you may create an `renv.lock` file with:

```R
install.packages("renv")
renv::snapshot()
```

A helper script `install_missing.R` is provided to install only packages that are not already present.
