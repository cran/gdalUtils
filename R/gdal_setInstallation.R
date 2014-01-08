#' gdal_setInstallation
#' 
#' Sets local GDAL installation options
#' 
#' @param rescan Logical. Force a rescan if neccessary (e.g. if you updated your GDAL install).
#' 
#' @return Sets an option "gdalUtils_gdalPath" with GDAL installation information.
#' @author Jonathan A. Greenberg (\email{gdalUtils@@estarcion.net}) and Matteo Mattiuzzi
#' 
#' @details This function searches the local system for valid installations of
#' GDAL, and returns a list, one item per valid GDAL install, containing 
#' the path to the installation, the version, the release date, available drivers,
#' and available python utilities.  The list will be sorted by release date, so
#' in general the first entry is the one that is used by the various GDAL utilities.
#' Note that this will automatically run every time a GDAL wrapper function is called,
#' so the user does not have to explicitly run it.
#'
#' @references \url{http://www.gdal.org/gdal_translate.html}
#' @examples \dontrun{ 
#' # Assumes you have GDAL installed on your local machine.
#' getOption("gdalUtils_gdalPath")
#' gdal_setInstallation()
#' getOption("gdalUtils_gdalPath")
#' # If there is more than one installation of GDAL, this is the 
#' # most recent installation:
#' getOption("gdalUtils_gdalPath")[[1]]
#' # The version number:
#' getOption("gdalUtils_gdalPath")[[1]]$version
#' }
#' @export

# TODO: interface with gdal_chooseInstallation to remove some installs.
# TODO: set permanently in e.g. .Rprofile (Matteo)
# TODO: force a re-scan in gdal_setInstallation
# TODO: allow the user to specify a single installation path in gdal_setInstallation
# TODO: if nothing is found, give suggestions on where to download GDAL
# TODO: check if the user has permission to execute the commands

gdal_setInstallation <- function(rescan=FALSE)
{
	
# Returns the available GDAL python utilities
	gdal_python_utilities <- function(path)
	{
		if(missing(path)) { path <- gdal_path() }
		sapply(path,list.files,pattern="\\.py")
	}
	
# Returns the available GDAL drivers
	gdal_drivers <- function(path, verbose=FALSE)   
	{
		if(missing(path)) path <- gdal_path(checkValidity=TRUE)
		
		cmd <- file.path(path, "gdalinfo")
		cmd <- paste0('"',cmd,'"'," --formats")
		
		drivers_raw <- lapply(cmd,system,intern=TRUE)
		
		result <- vector(mode='list',length(path))
		names(result) <- path
		for(i in seq_along(drivers_raw))
		{
			drivers_raw[[i]] <- drivers_raw[[i]][-1]
			drivers=strsplit(drivers_raw[[i]],":")
			driver_names=gsub("^ ","",sapply(drivers,function(x) { x[2] })) # Need to remove spaces
			driver_codes_perm=strsplit(sapply(drivers,function(x) { x[1] }),"\\(")
			driver_codes=gsub(" ","",sapply(driver_codes_perm,function(x) { x[1] }),fixed=TRUE)
			driver_perm=gsub("\\)","",sapply(driver_codes_perm,function(x) { x[2] }))
			
			r <- w <- u <- v <- s <- rep(FALSE,length(driver_perm))
			r[grep(driver_perm, pattern="r")]   <- TRUE
			w[grep(driver_perm, pattern="w")]   <- TRUE
			u[grep(driver_perm, pattern="\\+")] <- TRUE
			v[grep(driver_perm, pattern="v")]   <- TRUE
			s[grep(driver_perm, pattern="s")]   <- TRUE	
			
			result[[i]] <- data.frame(format_code=driver_codes,read=r,write=w,update=u,virtualIO=v,subdatasets=s,format_name=driver_names)
		}
		return(result[[1]])
	}
	
# Returns the GDAL version
	gdal_version <- function(path, newerThan=NULL, verbose=FALSE)
	{
		if(missing(path)) { path <- gdal_path() }
		
		cmd <- normalizePath(list.files(path, "gdalinfo",full.names=TRUE))
		cmd <- paste0('"',cmd,'"'," --version")
		
		result <- lapply(cmd,system,intern=TRUE)
		
		# It seams that system works also in Windows (MAC?), do you confirm?
		# Does shell work here?
		# if (.Platform$OS=="unix") 
		#{
		# gdal_version <- system(cmd,intern=TRUE) 
		# else 
		#{
		#gdal_version <- shell(cmd,intern=TRUE)
		#}
		
		res <- sapply(result,length)
		
		if(sum(res)!=length(result))
		{
			message("Probably broken install of gdal at '",paste0(path[which(res!=1)],collapse= "' and '"),"'")
		}
		result <- result[res==1]
		
		date <- version <- vector(mode = "list", length = length(result))
		
		for(i in seq_along(result))
		{
			ingd         <- strsplit(result[[i]],",")[[1]]
			version[[i]] <- gsub(ingd[1],pattern="GDAL ",replacement="")
			ind          <- grep(ingd,pattern="releas") # should this be: glob2rx("GDAL*")?
			date[[i]]    <- as.character(as.Date(gsub(ingd[ind],pattern=" released ",replacement=""),format="%Y/%m/%d"))
		}
		
		if(!is.null(newerThan))
		{
			test <- try(as.Date(newerThan),silent=TRUE)
			if(!inherits(test,"try-error"))
			{
				datein <- lapply(date,as.Date)
				res    <- sapply(datein,">=",as.Date(newerThan)) 
			} else
			{
				version   <- gsub(tolower(version),pattern="[a-z]",replacement="")
				res       <- sapply(version,strsplit,"\\.") 
				newerThan <- strsplit(newerThan,"\\.")[[1]]
				
				for(i in seq_along(res))
				{
					difs <- as.numeric(res[[i]]) - as.numeric(newerThan)
					difs <- sign(difs)
					
					if(sum(difs==-1)==0)
					{
						res[[i]] <- TRUE
					} else
					{
						if(difs[1]<0)
						{
							res[[i]] <- FALSE
						} else if(difs[1]>0)
						{
							res[[i]] <- TRUE
						} else if(difs[1]==0)
						{
							if(difs[2]<0)
							{
								res[[i]] <- FALSE
							} else if(difs[2]>0)
							{
								res[[i]] <- FALSE
							} else
							{  
								if(difs[3]>=0)
								{
									res[[i]] <- TRUE                  
								} else if (difs[3]<0)
								{
									res[[i]] <- FALSE
								}
							}
						}
					}
				}
			}
			names(res) <- path
			return(res)
		}
		result <- as.data.frame(cbind(path=path[res==1],version=version,date=date), stringsAsFactors=FALSE)
		return(result)
	}
	
	correctPath <- function(x)
	{
		if(!is.null(x))
		{
			if (.Platform$OS.type=="windows")
			{
				x <- shortPathName(x)
			} else
			{
				x <- path.expand(x)
			}
			x      <- gsub(x,pattern="\\\\",replacement="/") # switch "\\" to "/
			ind    <- substr(x,nchar(x),nchar(x))!="/"       # some x without "/" at the end?
			x[ind] <- paste0(x[ind],"/")                     # add "/" at the end
		}
		return(x)
	}
	
# Checks if GDAL is functional
	gdal_check_validity <- function(path)
	{
		checkValidity <- sapply(path,
				function(x)
				{
					cmd <- normalizePath(
							list.files(path=x,pattern="gdalinfo",full.names=TRUE))
					
					if(length(cmd)==0)
					{
						return(FALSE)
					} else
					{
						cmd <- paste0('"',cmd,'"'," --version")
						validity = length(try(gdal <- system(cmd,intern=TRUE),silent=TRUE))
						
						return(as.logical(validity))
					}
				}
		)
	}
	
# Determines the path to GDAL installations
	gdal_path <- function(
			search_path,
			ignore.options=FALSE,
			ignore.which=FALSE,
			ignore.common=FALSE,
			force_full_scan = FALSE, 
			checkValidity, 
			search_path_recursive=FALSE,
			verbose = FALSE)
	{
		owarn <- getOption("warn")
		options(warn=-2)
		on.exit(options(warn=owarn))
		
		if(missing(checkValidity))
		{
			if(is.null(getOption("gdalUtils_gdalPath"))) checkValidity=TRUE else checkValidity=FALSE
		}
		
		path <- NULL
		# Rescan will override everything.
		if(!force_full_scan)
		{
			# Check options first.
			if(!ignore.options)
			{
				if(verbose) message("Checking the gdalUtils_gdalPath option...")
				option_paths <- unlist(
						sapply(getOption("gdalUtils_gdalPath"),function(x) return(x$path)))
				if(!is.null(option_paths) && checkValidity)
				{
					option_paths_check <- gdal_check_validity(option_paths)
					option_paths <- option_paths[option_paths_check]
				}
				path <- c(path,option_paths)
			}
			
			# Next try Sys.which unless ignored:
			if(!ignore.options && length(path)==0)
			{
				if(verbose) message("Checking Sys.which...")
				Sys.which_path <- dirname(Sys.which("gdalinfo"))
				if(Sys.which_path=="") Sys.which_path <- NULL
				if(!is.null(Sys.which_path) && checkValidity)
				{
					Sys.which_path_check <- gdal_check_validity(Sys.which_path)
					Sys.which_path <- Sys.which_path[Sys.which_path_check]
				}
				path <- c(path,Sys.which_path)
			}
			
			# Next, try scanning the search path
			if(!missing(search_path) && length(path)==0)
			{
				if(verbose) message("Checking the search path...")
				search_paths <- normalizePath(dirname(
								list.files(path=search_path,pattern="gdalinfo",
										recursive=search_path_recursive,full.names=TRUE)))
				if(length(search_paths)==0) search_paths <- NULL
				if(!is.null(search_paths) && checkValidity)
				{
					search_paths_check <- gdal_check_validity(search_paths)
					search_paths <- search_paths[search_paths_check]
				}
				path <- c(path,search_paths)
				
			}
			
			# If nothing is still found, look in common locations
			if(!ignore.common && length(path)==0)
			{
				if(verbose) message("Checking common locations...")
				if (.Platform$OS=="unix")
				{
					common_locations <- c(
							# UNIX systems
							"/usr/bin",
							"/usr/local/bin",
							# Mac
							# Kyngchaos frameworks:
							"/Library/Frameworks/GDAL.framework/Programs",
							# MacPorts:
							"/opt/local/bin"
					)
				}
				
				if (.Platform$OS=="windows")
				{
					common_locations <- c(
							"C:\\Program Files",
							"C:\\Program Files (x86)",
							"C:\\OSGeo4W"
					)
				}
				
				if(length(common_locations != 0))
				{
					common_paths <- unlist(sapply(common_locations,
									function(x)
									{
										search_common_paths <- normalizePath(dirname(
														list.files(path=x,pattern="gdalinfo",recursive=TRUE,full.names=TRUE)))
										return(search_common_paths)
									}))
					if(length(common_paths)==0) common_paths <- NULL
					if(!is.null(common_paths) && checkValidity)
					{
						common_paths_check <- gdal_check_validity(common_paths)
						common_paths <- common_paths[common_paths_check]
					}
					path <- c(path,common_paths)
				}
			}
			if(length(path)==0)
			{
				force_full_scan=TRUE
			}
		}
		
		if(force_full_scan)
		{
			if(verbose) message("Scanning your root-dir for available GDAL installations,... This could take some time...")
			if (.Platform$OS=="unix")
			{
				root_dir <- "/"	
			}
			
			if (.Platform$OS=="windows")
			{
				root_dir <- "C:\\"
			}
			
			search_full_path <- normalizePath(dirname(
							list.files(path=root_dir,pattern="gdalinfo",
									recursive=TRUE,full.names=TRUE)))
			if(length(search_full_path)==0) search_full_path <- NULL
			if(!is.null(search_full_path) && checkValidity)
			{
				search_full_path_check <- gdal_check_validity(search_full_path)
				search_full_path <- search_full_path[search_full_path_check]
			}
			path <- c(path,search_paths)
		}
		
		if(length(path)==0)
		{
			#add QGIS?
			stop("No GDAL installation found. Please install 'gdal' before continuing:\n\t- www.gdal.org (no HDF4 support!)\n\t- www.trac.osgeo.org/osgeo4w/ (with HDF4 support RECOMMENDED)\n\t- www.fwtools.maptools.org (with HDF4 support)\n") # why not stop?
		}
		
		return(correctPath(path))
	}
	
# Returns the full GDAL installation status
	gdal_installation <- function(
			return_versions=TRUE,
			return_drivers=TRUE,
			return_python_utilities=TRUE,
			sort_most_current=TRUE,
			rescan=FALSE
	)
	{
		path <- gdal_path(ignore.options=rescan)
		
		gdal_installation_results <- lapply(path,
				function(x,return_drivers,return_python_utilities,return_versions)
				{
					result <- list(path=x)
					
					if(return_versions)
					{
						version <- gdal_version(x)
						result$version <- version$version
						result$date <- version$date
					}
					
					if(return_drivers)
					{
						result$drivers <- gdal_drivers(x)    
					}
					
					if(return_python_utilities)
					{
						result$python_utilities <- gdal_python_utilities(x)    
					}
					return(result)
				},return_drivers=return_drivers,
				return_python_utilities=return_python_utilities,return_versions=return_versions)
		if(sort_most_current)
		{
			versions <- unlist(sapply(gdal_installation_results,function(x) return(x$date)))
			gdal_installation_results <- gdal_installation_results[
					order(as.Date(unlist(versions)),decreasing=TRUE)]
		}
		return(gdal_installation_results)    
	}
	
# Sets the installation for this session.
	
#	path <- gdal_path(ignore.options=TRUE)
	options(gdalUtils_gdalPath=gdal_installation(rescan=rescan))
}