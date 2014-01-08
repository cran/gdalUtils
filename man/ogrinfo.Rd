\name{ogrinfo}
\alias{ogrinfo}
\title{ogrinfo}
\usage{
ogrinfo(datasource_name, layer, ro, q, where, spat, geomfield, fid, sql,
  dialect, al, so, fields, geom, formats, additional_commands,
  verbose = FALSE)
}
\arguments{
  \item{datasource_name}{Character. The data source to
  open. May be a filename, directory or other virtual name.
  See the OGR Vector Formats list for supported
  datasources.}

  \item{layer}{Character. One or more layer names may be
  reported.}

  \item{ro}{Logical. Open the data source in read-only
  mode.}

  \item{al}{Logical. List all features of all layers (used
  instead of having to give layer names as arguments).}

  \item{so}{Logical. Summary Only: supress listing of
  features, show only the summary information like
  projection, schema, feature count and extents.}

  \item{q}{Logical. Quiet verbose reporting of various
  information, including coordinate system, layer schema,
  extents, and feature count.}

  \item{where}{Character. An attribute query in a
  restricted form of the queries used in the SQL WHERE
  statement. Only features matching the attribute query
  will be reported.}

  \item{sql}{Character. Execute the indicated SQL statement
  and return the result.}

  \item{dialect}{Character. SQL dialect. In some cases can
  be used to use (unoptimized) OGR SQL instead of the
  native SQL of an RDBMS by passing OGRSQL. Starting with
  GDAL 1.10, the "SQLITE" dialect can also be used with any
  datasource.}

  \item{spat}{Numeric. c(xmin,ymin,xmax,ymax) The area of
  interest. Only features within the rectangle will be
  reported.}

  \item{geomfield}{Character. (OGR >= 2.0) Name of the
  geometry field on which the spatial filter operates on.}

  \item{fid}{Numeric. If provided, only the feature with
  this feature id will be reported. Operates exclusive of
  the spatial or attribute queries. Note: if you want to
  select several features based on their feature id, you
  can also use the fact the 'fid' is a special field
  recognized by OGR SQL. So, '-where "fid in (1,3,5)"'
  would select features 1, 3 and 5.}

  \item{fields}{Character. ("YES"|"NO") (starting with GDAL
  1.6.0) If set to NO, the feature dump will not display
  field values. Default value is YES.}

  \item{geom}{Character. ("YES"|"NO"|"SUMMARY") (starting
  with GDAL 1.6.0) If set to NO, the feature dump will not
  display the geometry. If set to SUMMARY, only a summary
  of the geometry will be displayed. If set to YES, the
  geometry will be reported in full OGC WKT format. Default
  value is YES.}

  \item{formats}{Logical. List the format drivers that are
  enabled.}

  \item{additional_commands}{Character. Additional commands
  to pass directly to ogrinfo.}

  \item{verbose}{Logical.}
}
\value{
character
}
\description{
R wrapper for ogrinfo: lists information about an OGR
supported data source
}
\details{
This is an R wrapper for the 'ogrinfo' function that is
part of the Geospatial Data Abstraction Library (GDAL).  It
follows the parameter naming conventions of the original
function, with some modifications to allow for more R-like
parameters.  For all parameters, the user can use a single
character string following, precisely, the gdalinfo format
(\url{http://gdal.org/ogrinfo.html}), or, in some cases,
can use R vectors to achieve the same end.

This function assumes the user has a working GDAL on their
system.  If the "gdalUtils_gdalPath" option has been set
(usually by gdal_setInstallation), the GDAL found in that
path will be used.  If nothing is found,
gdal_setInstallation will be executed to attempt to find a
working GDAL.
}
\examples{
\dontrun{
datasource_name <- system.file("external/tahoe_highrez_training.shp", package="gdalUtils")
# Display all available formats:
# Command-line ogrinfo call:
# ogrinfo --formats
ogrinfo(formats=TRUE)

# Get info on an entire shapefile:
# ogrinfo tahoe_highrez_training.shp
ogrinfo(datasource_name)

# Get info on the layer of the shapefile:
# ogrinfo tahoe_highrez_training.shp tahoe_highrez_training
ogrinfo(datasource_name,"tahoe_highrez_training")
}
}
\author{
Jonathan A. Greenberg (\email{gdalUtils@estarcion.net})
(wrapper) and Frank Warmerdam (GDAL lead developer).
}
\references{
\url{http://www.gdal.org/ogrinfo.html}
}

