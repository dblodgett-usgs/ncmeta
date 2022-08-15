
<!-- badges: start -->

[![R-CMD-check](https://github.com/hypertidy/ncmeta/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/hypertidy/ncmeta/actions/workflows/R-CMD-check.yaml)
[![CRAN
status](http://www.r-pkg.org/badges/version/ncmeta)](https://cran.r-project.org/package=ncmeta)
[![](https://cranlogs.r-pkg.org/badges/ncmeta)](https://cran.r-project.org/package=ncmeta)

<!-- badges: end -->
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ncmeta

The `ncmeta` package provides straightforward NetCDF metadata, with a
set of consistent entity-based functions for extracting metadata from a
file or online source. We aim to fill a gap in between the generality
and power of the NetCDF framework and ease of use.

There are two main packages for using NetCDF in R, `RNetCDF` and `ncdf4`
and ncmeta uses both, where appropriate. Both packages are very close
the native API of NetCDF itself, and ncmeta simply provides an easier
high-level interpretation.

## About NetCDF

NetCDF is both a data model and an API, and provides a very general
framework for expressing data formats. The explicit entities in NetCDF
are **variables**, **dimensions** and **attributes** and ncmeta provides
functions `nc_vars`, `nc_dims`, and `nc_atts` to extract their names,
order and other metadata. There are matching functions `nc_var`,
`nc_dim`, and `nc_att` with an extra identifier to extract specific
information about an individual variable, dimension, or attribute.

Also includes functions for implicit entities, these are **grids** and
**axes**. These don’t exist in the NetCDF specification explicitly, but
are meaningful and worth making explicit. Many NetCDF tools don’t
explicitly present these concepts so grab hold of them with ncmeta!

A **grid** is an ordered set of dimensions, and the [Unidata
site](https://www.unidata.ucar.edu/software/netcdf/) refers *informally*
to this concept as **shape**.

An **axis** is an *instance* of a dimension, the use of that dimension
within a particular variable.

These functions provide a more developer-friendly scheme for working
with the range of formats provided by the NetCDF ecosystem.

## Installation

Install ncmeta from CRAN with:

``` r
install.packages("ncmeta")
```

You can install the development version of ncmeta from github with:

``` r
# install.packages("devtools")
devtools::install_github("hypertidy/ncmeta")
```

## Example

This example shows some of the functions for extracting information from
a NetCDF source.

``` r
library(ncmeta)
filename <- system.file("extdata", "S2008001.L3m_DAY_CHL_chlor_a_9km.nc", package = "ncmeta")
nc_inq(filename) # one-row summary of file

nc_dim(filename, 0)  ## first dimension
nc_dims(filename)  ## all dimensions
```

## Get involved!

Please let us know if you have any feedback, see the [Issues
tab](https://github.com/hypertidy/ncmeta) if you found a bug or have a
question. Feel free to email the maintainer directly for other
questions.

------------------------------------------------------------------------

Please note that this project is released with a [Contributor Code of
Conduct](https://github.com/hypertidy/ncmeta/blob/master/CODE_OF_CONDUCT.md#contributor-code-of-conduct).
By participating in this project you agree to abide by its terms.
