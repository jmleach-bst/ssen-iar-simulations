
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Simulations

## Data Generation

The University of Alabama at Birmingham’s (UAB) research computing
resource, cheaha (<https://docs.uabgrid.uab.edu/wiki/cheaha>), was used
to generate the simulated datasets, since generating all of them takes
several hours. The folder `data-generation.zip` contains `R` code and
scripts to generate the data. However, there are caveats. The first is
that the scripts are specific to slurm
(<https://docs.uabgrid.uab.edu/wiki/Slurm>), and may require editing for
use on other systems; no such issue should occur with the `R` code here.
The second is that obviously file paths will need to be altered in both
the `R` files and slurm scripts in order to save the data. It is not
recommended to change the file names themselves, because these are used
later to munge the data and generate output using the `R` package
`drake`. Simulations were run using `R` version 3.6.0.

## Analyses

While no given analysis takes all that long, performing all the analyses
reported in the paper would take several weeks on your standard laptop.
Again, cheaha was used, and with the same caveats about slurm scripts
and file paths, we’ve included `R` code and slurm scripts for
reproducing the analysis results. Additionally, for the analyses some
slurm specific code was used to run array jobs, specifically in naming
identifiers for each array job: `runID <-
as.numeric(Sys.getenv("SLURM_ARRAY_TASK_ID"))`. This may require editing
for use on other systems. In any case, these files are included in the
`analyses.zip` folder. Analyses were run using `R` version 3.6.0.

## Results

Due to large file/folder sizes, the results themselves have not been
included in this repository, but are available upon request
(<jleach@uab.edu>). These are what you would obtain upon running the
analyses yourself.

## Drake

Given the results of the analyses, i.e., the contents of the `results`
folder, the `R` package `drake` is used to control the workflow for
munging the results into proper form and obtaining the output for the
paper. This can be done by running the contents of the file `make.R`,
then running `make(plan)` in `R`.

Note that the `drake` plan (`plan.R`) has several functions with
arguments that consist of paths - you may need to change these to match
where you’ve stored the results. We recommend to store the results in
sub-diretories named `results/B01` when non-zero \(\beta_j = 0.1\) and
`results/B05` when non-zero \(\beta_j = 0.5\), which would then
necessitate no changes to path names in `plan.R`.

Tables and figures from the paper and supplementary materials can be
found in `summary_report.pdf`, and can be reproduced by kniting
`summary_report.Rmd`.

# ADNI Application

I am not permitted to share ADNI data, and so I have not included the
`R` code used to wrangle or analyze that data, since reviewers/readers
would not be able to use the data to reproduce the code. However, the
same `R` package, `ssnet`, was used to perform the analyses for both
simulations and ADNI data.
