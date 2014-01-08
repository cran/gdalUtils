\name{gdal_setInstallation}
\alias{gdal_setInstallation}
\title{gdal_setInstallation}
\usage{
gdal_setInstallation(rescan = FALSE)
}
\arguments{
  \item{rescan}{Logical. Force a rescan if neccessary (e.g.
  if you updated your GDAL install).}
}
\value{
Sets an option "gdalUtils_gdalPath" with GDAL installation
information.
}
\description{
Sets local GDAL installation options
}
\details{
This function searches the local system for valid
installations of GDAL, and returns a list, one item per
valid GDAL install, containing the path to the
installation, the version, the release date, available
drivers, and available python utilities.  The list will be
sorted by release date, so in general the first entry is
the one that is used by the various GDAL utilities. Note
that this will automatically run every time a GDAL wrapper
function is called, so the user does not have to explicitly
run it.
}
\examples{
\dontrun{
# Assumes you have GDAL installed on your local machine.
getOption("gdalUtils_gdalPath")
gdal_setInstallation()
getOption("gdalUtils_gdalPath")
# If there is more than one installation of GDAL, this is the
# most recent installation:
getOption("gdalUtils_gdalPath")[[1]]
# The version number:
getOption("gdalUtils_gdalPath")[[1]]$version
}
}
\author{
Jonathan A. Greenberg (\email{gdalUtils@estarcion.net}) and
Matteo Mattiuzzi
}
\references{
\url{http://www.gdal.org/gdal_translate.html}
}

