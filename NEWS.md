# pmxtricks 0.0.11
## New function egdt - expand.grid for data.tables
merge.data.table does not support merging by nothing, as opposed to
merge.data.frame where merge is useful to give all combinations of
rows in df1 and df2. Use egdt to get this functionality for
data.tables.
## NMcompRes taken out
For now, NMcompRes is not available. It depends on NMdata and was the only
function in the package to do so. Maybe it will be included in NMdata
instead at some point.
# pmxtricks 0.0.10
## Many data functions taken out
Most notably, a lot of functions have been removed from the package. They now live in the NMdata package. pmxtricks continues its life with several great functions for calculations frequently used in pharmacometrics, plus simulation and plotting functionality.
## metadata functions
A few functions have been added to generate and print documentation of variables in datasets. 
## Post 0.0.4.800
## findVars: New function that finds columns that vary within other columns (say subjects).
## findCovs: Implemented in data.table. Should be much faster.
## ggwrite: Added a show argument. write and show are independent of each other.
## NMwriteOutput: support for args.stamp, a list of args passed to stampObj.
## means: Introduced na.rm argument. 
## signif2: bugfixes related to zero values.
