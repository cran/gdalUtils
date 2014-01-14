\name{ogr2ogr}
\alias{ogr2ogr}
\title{ogr2ogr}
\usage{
ogr2ogr(src_datasource_name, dst_datasource_name, layer, skipfailures, append,
  update, select, where, progress, sql, dialect, preserve_fid, fid, spat,
  geomfield, a_srs, t_srs, s_srs, f, overwrite, dsco, lco, nln, nlt, dim, gt,
  clipsrc, clipsrcsql, clipsrclayer, clipsrcwhere, clipdst, clipdstsql,
  clipdstlayer, clipdstwhere, wrapdateline, datelineoffset, simplify,
  segmentize, addfields, fieldTypeToString, unsetFieldWidth, fieldmap,
  splitlistfields, maxsubfields, explodecollections, zfield, gcp, order,
  additional_commands, ignore.full_scan = TRUE, verbose = FALSE)
}
\arguments{
  \item{src_datasource_name}{Character. Input vector file.}

  \item{dst_datasource_name}{Character. Output vector
  file.}

  \item{layer}{Character. Layer to use.}

  \item{skipfailures}{Logical. Continue after a failure,
  skipping the failed feature.}

  \item{append}{Logical. Append to existing layer instead
  of creating new}

  \item{update}{Logical. Open existing output datasource in
  update mode rather than trying to create a new one}

  \item{select}{Character. Comma-delimited list of fields
  from input layer to copy to the new layer. A field is
  skipped if mentioned previously in the list even if the
  input layer has duplicate field names. (Defaults to all;
  any field is skipped if a subsequent field with same name
  is found.) Starting with OGR 2.0, geometry fields can
  also be specified in the list.}

  \item{where}{Character. Attribute query (like SQL
  WHERE).}

  \item{progress}{Logical. (starting with GDAL 1.7.0)
  Display progress on terminal. Only works if input layers
  have the "fast feature count" capability.}

  \item{sql}{Character. SQL statement to execute. The
  resulting table/layer will be saved to the output.}

  \item{dialect}{Character. SQL dialect. In some cases can
  be used to use (unoptimized) OGR SQL instead of the
  native SQL of an RDBMS by passing OGRSQL. Starting with
  GDAL 1.10, the "SQLITE" dialect can also be used with any
  datasource.}

  \item{preserve_fid}{Logical. Use the FID of the source
  features instead of letting the output driver to
  automatically assign a new one.}

  \item{fid}{Character. If provided, only the feature with
  this feature id will be reported. Operates exclusive of
  the spatial or attribute queries. Note: if you want to
  select several features based on their feature id, you
  can also use the fact the 'fid' is a special field
  recognized by OGR SQL. So, '-where "fid in (1,3,5)"'
  would select features 1, 3 and 5.}

  \item{spat}{Numeric. c(xmin,ymin,xmax,ymax) spatial query
  extents. Only features whose geometry intersects the
  extents will be selected. The geometries will not be
  clipped unless -clipsrc is specified}

  \item{geomfield}{Character. (OGR >= 2.0) Name of the
  geometry field on which the spatial filter operates on.}

  \item{a_srs}{Character. Assign an output SRS.}

  \item{t_srs}{Character. Reproject/transform to this SRS
  on output.}

  \item{s_srs}{Character. Override source SRS.}

  \item{f}{Character. output file format name (default is
  ESRI Shapefile), some possible values are: "ESRI
  Shapefile", "TIGER", "MapInfo File", "GML", "PostgreSQL"}

  \item{overwrite}{Logical. Delete the output layer and
  recreate it empty.}

  \item{dsco}{Character. Dataset creation option (format
  specific).}

  \item{lco}{Character. Layer creation option (format
  specific).}

  \item{nln}{Character. Assign an alternate name to the new
  layer.}

  \item{nlt}{Character. Define the geometry type for the
  created layer. One of NONE, GEOMETRY, POINT, LINESTRING,
  POLYGON, GEOMETRYCOLLECTION, MULTIPOINT, MULTIPOLYGON or
  MULTILINESTRING. Add "25D" to the name to get 2.5D
  versions. Starting with GDAL 1.10, PROMOTE_TO_MULTI can
  be used to automatically promote layers that mix polygon
  or multipolygons to multipolygons, and layers that mix
  linestrings or multilinestrings to multilinestrings. Can
  be usefull when converting shapefiles to PostGIS (and
  other target drivers) that implements strict checks for
  geometry type.}

  \item{dim}{Character. (starting with GDAL 1.10) Force the
  coordinate dimension to val (valid values are 2 or 3).
  This affects both the layer geometry type, and feature
  geometries. Starting with GDAL 2.0, the value can be set
  to "layer_dim" to instruct feature geometries to be
  promoted to the coordinate dimension declared by the
  layer.}

  \item{gt}{Numeric. group n features per transaction
  (default 200). Increase the value for better performance
  when writing into DBMS drivers that have transaction
  support.}

  \item{clipsrc}{Character.  [xmin ymin xmax
  ymax]|WKT|datasource|spat_extent: (starting with GDAL
  1.7.0) clip geometries to the specified bounding box
  (expressed in source SRS), WKT geometry (POLYGON or
  MULTIPOLYGON), from a datasource or to the spatial extent
  of the -spat option if you use the spat_extent keyword.
  When specifying a datasource, you will generally want to
  use it in combination of the -clipsrclayer, -clipsrcwhere
  or -clipsrcsql options}

  \item{clipsrcsql}{Character. Select desired geometries
  using an SQL query instead.}

  \item{clipsrclayer}{Character. Select the named layer
  from the source clip datasource.}

  \item{clipsrcwhere}{Character. Restrict desired
  geometries based on attribute query.}

  \item{clipdst}{Character. (starting with GDAL 1.7.0) clip
  geometries after reprojection to the specified bounding
  box (expressed in dest SRS), WKT geometry (POLYGON or
  MULTIPOLYGON) or from a datasource. When specifying a
  datasource, you will generally want to use it in
  combination of the -clipdstlayer, -clipdstwhere or
  -clipdstsql options}

  \item{clipdstsql}{Character. Select desired geometries
  using an SQL query instead.}

  \item{clipdstlayer}{Character. Select the named layer
  from the destination clip datasource.}

  \item{clipdstwhere}{Character. Restrict desired
  geometries based on attribute query.}

  \item{wrapdateline}{Logical. (starting with GDAL 1.7.0)
  split geometries crossing the dateline meridian (long. =
  +/- 180deg).}

  \item{datelineoffset}{Logical. (starting with GDAL 1.10)
  offset from dateline in degrees (default long. = +/-
  10deg, geometries within 170deg to -170deg will be
  splited)}

  \item{simplify}{Numeric. (starting with GDAL 1.9.0)
  distance tolerance for simplification. Note: the
  algorithm used preserves topology per feature, in
  particular for polygon geometries, but not for a whole
  layer.}

  \item{segmentize}{Numeric. (starting with GDAL 1.6.0)
  maximum distance between 2 nodes. Used to create
  intermediate points}

  \item{addfields}{Logical. (starting with GDAL 2.0) This
  is a specialized version of -append. Contrary to -append,
  -addfields has the effect of adding, to existing target
  layers, the new fields found in source layers. This
  option is usefull when merging files that have
  non-strictly identical structures. This might not work
  for output formats that don't support adding fields to
  existing non-empty layers.}

  \item{fieldTypeToString}{Character. (starting with GDAL
  1.7.0) converts any field of the specified type to a
  field of type string in the destination layer. Valid
  types are : Integer, Real, String, Date, Time, DateTime,
  Binary, IntegerList, RealList, StringList. Special value
  All can be used to convert all fields to strings. This is
  an alternate way to using the CAST operator of OGR SQL,
  that may avoid typing a long SQL query.}

  \item{unsetFieldWidth}{Logical. (starting with GDAL 2.0)
  set field width and precision to 0.}

  \item{fieldmap}{Character. (starting with GDAL 1.10.0)
  Specifies the list of field indexes to be copied from the
  source to the destination. The (n)th value specified in
  the list is the index of the field in the target layer
  definition in which the n(th) field of the source layer
  must be copied. Index count starts at zero. There must be
  exactly as many values in the list as the count of the
  fields in the source layer. We can use the 'identity'
  setting to specify that the fields should be transferred
  by using the same order. This setting should be used
  along with the -append setting.}

  \item{splitlistfields}{Logical. (starting with GDAL
  1.8.0) split fields of type StringList, RealList or
  IntegerList into as many fields of type String, Real or
  Integer as necessary.}

  \item{maxsubfields}{Numeric. To be combined with
  -splitlistfields to limit the number of subfields created
  for each split field.}

  \item{explodecollections}{Logical. (starting with GDAL
  1.8.0) produce one feature for each geometry in any kind
  of geometry collection in the source file.}

  \item{zfield}{Character. (starting with GDAL 1.8.0) Uses
  the specified field to fill the Z coordinate of
  geometries.}

  \item{gcp}{Numeric. c(ungeoref_x,ungeoref_y,georef_x
  georef_y,elevation) (starting with GDAL 1.10.0) Add the
  indicated ground control point. This option may be
  provided multiple times to provide a set of GCPs.}

  \item{order}{Numeric. (starting with GDAL 1.10.0) order
  of polynomial used for warping (1 to 3). The default is
  to select a polynomial order based on the number of
  GCPs.}

  \item{additional_commands}{Character. Additional commands
  to pass directly to ogrinfo.}

  \item{ignore.full_scan}{Logical. If FALSE, perform a
  brute-force scan if other installs are not found.
  Default is TRUE.}

  \item{verbose}{Logical. Enable verbose execution? Default
  is FALSE.}
}
\value{
character
}
\description{
R wrapper for ogr2ogr: converts simple features data
between file formats
}
\details{
This is an R wrapper for the 'ogr2ogr' function that is
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
# We'll pre-check to make sure there is a valid GDAL install.
# Note this isn't strictly neccessary, as executing the function will
# force a search for a valid GDAL install.
gdal_setInstallation()
valid_install <- !is.null(getOption("gdalUtils_gdalPath"))
if(valid_install)
{
src_datasource_name <- system.file("external/tahoe_highrez_training.shp", package="gdalUtils")
dst_datasource_name <- paste(tempfile(),".shp",sep="")
ogrinfo(src_datasource_name,"tahoe_highrez_training")
# reproject the input to mercator
ogr2ogr(src_datasource_name,dst_datasource_name,t_srs="EPSG:3395",verbose=TRUE)
ogrinfo(dirname(dst_datasource_name),layer=remove_file_extension(basename(dst_datasource_name)))
}
}
\author{
Jonathan A. Greenberg (\email{gdalUtils@estarcion.net})
(wrapper) and Frank Warmerdam (GDAL lead developer).
}
\references{
\url{http://www.gdal.org/ogr2ogr.html}
}

