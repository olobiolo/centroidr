# centroidr
centroids and distances

The first package I ever made, more for practice than necessity.

In order to primitively assess the variance of a multidimensional sample, i.e. a set of points, 
we obtain the centroid of the sample, or its center of gravity, and compute the mean distance 
between the points and the centroid.

The package deals with matrices (as is) and data frames (selected columns) and vectors, 
in case of strange ideas. One function just returns the centroid coordinates and the other 
returns the point-centroid distances. One can then proceed with the distances as one wishes. 

Check the vignette for my thoughts on the matter.

All code is in R and the functions are very simple but I did get a chance to practise documenting S3 methods.


Install the package with:

devtools::install_github('olobiolo/centroidr')

or, if you want to be able to view the vignette:

devtools::install_github('olobiolo/centroidr', build = TRUE, build_opts = c("--no-resave-data", "--no-manual")


AC
