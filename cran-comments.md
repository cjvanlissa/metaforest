## Resubmission
This is a resubmission. Many thanks to CRAN reviewer Swetlana Herbrandt
for her time.

Reviewer's comment:
* 	Thanks, please add more small executable examples in your Rd-files
	for checks on CRAN.
	
Actions taken:
*	In this version, I have included small executable examples in every Rd file,
	including for S3 methods (using \dontshow).	I have tried to include as many
	possible combinations of settings in these examples to ensure the functions
	are robust against, e.g., different data types. I have also added examples
	using real datasets for the main functions, MetaForest and ClusterMF.


## Test environments
* local Windows 10 Pro x64 install, R 3.4.1
* win-builder (devel and release)

## R CMD check results
There were no ERRORs or WARNINGs. 

There was 1 NOTE:

* 	checking CRAN incoming feasibility ... NOTE
	Maintainer: 'Caspar J. van Lissa <c.j.vanlissa@gmail.com>'

	New submission

## Downstream dependencies
This is a new package without downstream dependencies