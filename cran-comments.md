# Version 0.1.6

* Fixed NOTES in CRAN check
* No other changes

## Test environments

* local Windows 10 Enterprise x64 install, R version 4.0.3 (2020-10-10)
* win-builder R version 3.6.2 (2019-12-12)
* win-builder R Under development (unstable) (2020-01-03 r77629)
* Travis Ubuntu 16.04.6 LTS, R version 3.6.1
* R-hub 	Ubuntu Linux 16.04 LTS, R-release, GCC
* R-hub Windows Server 2008 R2 SP1, R-devel, 32/64 bit

## R CMD check results

0 errors v | 0 warnings v | 0 notes v

## Downstream dependencies

None of the changes break existing functionality.

# Version 0.1.2

## First submission of new version
See NEWS.md for changes since version 0.1.0

## Test environments
* local Windows 7 Enterprise x64 install, R 3.5.0
* win-builder (devel and release)

## R CMD check results
There were no ERRORs or WARNINGs. 

There was 1 NOTE:

* 	checking CRAN incoming feasibility ... NOTE
	Maintainer: 'Caspar J. van Lissa <c.j.vanlissa@gmail.com>'

	New submission

## Downstream dependencies
I tried to check downstream dependencies on https://github.com/yihui/crandalf,
however, crandalf appears to be temporarily out of service because R 3.5.0 is 
not yet available for Linux servers. It is very unlikely that my package has
reverse dependencies, because 1) it is very recent (september 2017), and 2) it
provides high-level convenience functions for end-users, and not computational
functions for otherR-developers.

# Version 0.1

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
