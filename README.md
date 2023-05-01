# BMI510Docs
Final documentation of R functions


This repository contains 20 R functions with Roxygen documentation for the BMI 510 course final project at Emory University, Dept of Biomedical Informatics.

## Table of Contents

- [Dependencies](#dependencies)
- [Getting Started](#getting-started)
- [License](#license)

## Dependencies
The following R packages are required:
```sh
testthat
Roxygen
```


## Getting Started

To get started with this project, you can clone the repository locally and browse the documentation and assignments. To do so, run the following command in your terminal:

```sh
git clone https://github.com/fensorechase/BMI510Docs.git
```

Or to install the package locally, run the following in the R console: 
```sh
devtools::install_github("https://github.com/fensorechase/BMI510Docs")
```

Testing can be performed by running the following in the R console: 
```sh
testthat::test_file("R/bmi510-tests.R")
```

## Materials

This repository contains the following materials testing the 20 functions and modifying the Roxygen documentation:

- **R:** Folder containing the R files (file with documented functions, file with tests for these functions).
- **man:** Folder containing man files corresponding to each function in R/bmi510-template.R).


## License

The contents of this repository are licensed under the [MIT License](./LICENSE).
