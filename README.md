# pelagichypoxia
Data, R and Stan code for Moriarty et al. "Unexpected food web responses to low dissolved oxygen in an estuarine fjord"

This package includes all of the datafiles used, listed in the main package documentation.  For more information on the data objects, use help(dataobject)

To run plotting functions or to run the bayesian analysis, create in your working directory a subdirectory called "graphics", and another called "outputs/modelselection".  You will also need to download the Stan subdirectory from github and the two files therein.

Some of the plotting functions may not run from the package alone. You will have to download the git repository to gain access to the MCMC files (or generate them yourself using run_all_bayesian)
