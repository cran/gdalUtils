\name{gdal_translate}
\alias{gdal_translate}
\title{gdal_translate}
\usage{
gdal_translate(src_dataset, dst_dataset, ot, strict, of = "GTiff", b, mask,
  expand, outsize, scale, unscale, srcwin, projwin, epo, eco, a_srs, a_ullr,
  a_nodata, mo, co, gcp, q, sds, stats, additional_commands, sd_index,
  output_Raster = FALSE, verbose = FALSE, ...)
}
\arguments{
  \item{src_dataset}{Character. The source dataset name. It
  can be either file name, URL of data source or subdataset
  name for multi-dataset files.}

  \item{dst_dataset}{Character. The destination file name.}

  \item{ot}{Character.
  ("Byte"/"Int16"/"UInt16"/"UInt32"/"Int32"/"Float32"/"Float64"/"CInt16"/"CInt32"/"CFloat32"/"CFloat64").
  For the output bands to be of the indicated data type.}

  \item{strict}{Logical. Don't be forgiving of mismatches
  and lost data when translating to the output format.}

  \item{of}{Character. Select the output format. The
  default is GeoTIFF (GTiff). Use the short format name.}

  \item{b}{Numeric or Character. Select an input band band
  for output. Bands are numbered from 1. Multiple bands may
  be used to select a set of input bands to write to the
  output file, or to reorder bands. Starting with GDAL
  1.8.0, band can also be set to "mask,1" (or just "mask")
  to mean the mask band of the first band of the input
  dataset.}

  \item{mask}{Numeric. (GDAL >= 1.8.0) Select an input band
  band to create output dataset mask band. Bands are
  numbered from 1. band can be set to "none" to avoid
  copying the global mask of the input dataset if it
  exists. Otherwise it is copied by default ("auto"),
  unless the mask is an alpha channel, or if it is
  explicitly used to be a regular band of the output
  dataset ("-b mask"). band can also be set to "mask,1" (or
  just "mask") to mean the mask band of the 1st band of the
  input dataset.}

  \item{expand}{Character. ("gray"|"rgb"|"rgba").  (From
  GDAL 1.6.0) To expose a dataset with 1 band with a color
  table as a dataset with 3 (RGB) or 4 (RGBA) bands. Useful
  for output drivers such as JPEG, JPEG2000, MrSID, ECW
  that don't support color indexed datasets. The 'gray'
  value (from GDAL 1.7.0) enables to expand a dataset with
  a color table that only contains gray levels to a gray
  indexed dataset.}

  \item{outsize}{Numeric. (c(xsize[%],ysize[%])). Set the
  size of the output file. Outsize is in pixels and lines
  unless '%' is attached in which case it is as a fraction
  of the input image size.}

  \item{scale}{Numeric.
  (c(src_min,src_max,dst_min,dst_max)). Rescale the input
  pixels values from the range src_min to src_max to the
  range dst_min to dst_max. If omitted the output range is
  0 to 255. If omitted the input range is automatically
  computed from the source data.}

  \item{unscale}{Logical. Apply the scale/offset metadata
  for the bands to convert scaled values to unscaled
  values. It is also often necessary to reset the output
  datatype with the -ot switch.}

  \item{srcwin}{Numeric. (c(xoff,yoff,xsize,ysize)).
  Selects a subwindow from the source image for copying
  based on pixel/line location.}

  \item{projwin}{Numeric. (c(ulx,uly,lrx,lry)).  Selects a
  subwindow from the source image for copying (like
  -srcwin) but with the corners given in georeferenced
  coordinates.}

  \item{epo}{Logical. (Error when Partially Outside) (GDAL
  >= 1.10) If this option is set, -srcwin or -projwin
  values that falls partially outside the source raster
  extent will be considered as an error. The default
  behaviour starting with GDAL 1.10 is to accept such
  requests, when they were considered as an error before.}

  \item{eco}{Logical. (Error when Completely Outside) (GDAL
  >= 1.10) Same as -epo, except that the criterion for
  erroring out is when the request falls completely outside
  the source raster extent.}

  \item{a_srs}{Character.  Override the projection for the
  output file. The srs_def may be any of the usual GDAL/OGR
  forms, complete WKT, PROJ.4, EPSG:n or a file containing
  the WKT.}

  \item{a_ullr}{Numeric. (c(ulx,uly,lrx,lry)).
  Assign/override the georeferenced bounds of the output
  file. This assigns georeferenced bounds to the output
  file, ignoring what would have been derived from the
  source file.}

  \item{a_nodata}{Numeric. Assign a specified nodata value
  to output bands. Starting with GDAL 1.8.0, can be set to
  none to avoid setting a nodata value to the output file
  if one exists for the source file}

  \item{mo}{Character. ("META-TAG=VALUE").  Passes a
  metadata key and value to set on the output dataset if
  possible.}

  \item{co}{Character. ("NAME=VALUE"). Passes a creation
  option to the output format driver. Multiple -co options
  may be listed. See format specific documentation for
  legal creation options for each format.}

  \item{gcp}{Numeric.
  (c(pixel,line,easting,northing(,elevation))). Add the
  indicated ground control point to the output dataset.
  This option may be provided multiple times to provide a
  set of GCPs.}

  \item{q}{Logical. Suppress progress monitor and other
  non-error output.}

  \item{sds}{Logical. Copy all subdatasets of this file to
  individual output files. Use with formats like HDF or
  OGDI that have subdatasets.}

  \item{stats}{Logical. (GDAL >= 1.8.0) Force
  (re)computation of statistics.}

  \item{additional_commands}{Character. Additional commands
  to pass directly to gdal_translate.}

  \item{sd_index}{Numeric. If the file is an HDF4 or HDF5
  file, which subdataset should be returned (1 to the
  number of subdatasets)?  If this flag is used,
  src_dataset should be the filename of the multipart
  file.}

  \item{output_Raster}{Logical. Return output dst_dataset
  as a RasterBrick?}

  \item{verbose}{Logical.}

  \item{...}{Other parameters to pass to gdal_translate.}
}
\value{
NULL or if(output_Raster), a RasterBrick.
}
\description{
R wrapper for gdal_translate
}
\details{
This is an R wrapper for the 'gdal_translate' function that
is part of the Geospatial Data Abstraction Library (GDAL).
It follows the parameter naming conventions of the original
function, with some modifications to allow for more R-like
parameters.  For all parameters, the user can use a single
character string following, precisely, the gdal_translate
format (\url{http://www.gdal.org/gdal_translate.html}), or,
in some cases, can use R vectors to achieve the same end.

This function assumes the user has a working GDAL on their
system.  If the "gdalUtils_gdalPath" option has been set
(usually by gdal_setInstallation), the GDAL found in that
path will be used.  If nothing is found,
gdal_setInstallation will be executed to attempt to find a
working GDAL that has the right drivers as specified with
the "of" (output format) parameter.

The user can choose to (optionally) return a RasterBrick of
the output file (assuming raster/rgdal supports the
particular output format).
}
\examples{
\dontrun{
# Example from the original gdal_translate documentation:
src_dataset <- system.file("external/tahoe_highrez.tif", package="gdalUtils")
# Original gdal_translate call:
# gdal_translate -of GTiff -co "TILED=YES" tahoe_highrez.tif tahoe_highrez_tiled.tif
gdal_translate(src_dataset,"tahoe_highrez_tiled.tif",of="GTiff",co="TILED=YES",verbose=TRUE)
# Pull out a chunk and return as a raster:
gdal_translate(src_dataset,"tahoe_highrez_tiled.tif",of="GTiff",
srcwin=c(1,1,100,100),output_Raster=TRUE,verbose=TRUE)
# Notice this is the equivalent, but follows gdal_translate's parameter format:
gdal_translate(src_dataset,"tahoe_highrez_tiled.tif",of="GTiff",
srcwin="1 1 100 100",output_Raster=TRUE,verbose=TRUE)
# Extract the first subdataset from an HDF4 file:
hdf4_dataset <- system.file("external/test_modis.hdf", package="gdalUtils")
gdal_translate(hdf4_dataset,"test_modis_sd1.tif",sd_index=1)
}
}
\author{
Jonathan A. Greenberg (\email{gdalUtils@estarcion.net})
(wrapper) and Frank Warmerdam (GDAL lead developer).
}
\references{
\url{http://www.gdal.org/gdal_translate.html}
}

