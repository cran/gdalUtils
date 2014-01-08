\docType{data}
\name{tahoe_lidar_highesthit.tif}
\alias{tahoe_lidar_highesthit.tif}
\title{Lidar-derived highest hit (aka canopy) digital elevation model from the Lake Tahoe Basin.}
\description{
Lidar-derived highest hit (aka canopy) digital elevation
model from the Lake Tahoe Basin.
}
\examples{
\dontrun{
tahoe_lidar_highesthit <-
raster(system.file("external/tahoe_lidar_highesthit.tif", package="gdalUtils"))
plot(tahoe_lidar_highesthit)
}
}
\author{
Jonathan A. Greenberg \email{gdalUtils@estarcion.net}
}
\keyword{data}

