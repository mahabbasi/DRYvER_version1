library(data.table)
library(fst) #Make sure to update Rcpp if you get the error "function 'Rcpp_precious_remove' not provided by package 'Rcpp'"
library(magrittr)
library(rprojroot)
library(terra)

#~~~~~~~~~~~~~~~ Project directory and file structure ~~~~~~~~~~~~~~~~~~~~~~~~~~
rootdir = rprojroot::find_root(has_dir('src'))
datdir = file.path(rootdir, 'data')
resdir = file.path(rootdir, 'results')

#Create directory to write output files
watergap_extractdir <- file.path(resdir, 'watergap_netrextract')
if (!file.exists(watergap_extractdir)) {
  dir.create(watergap_extractdir)
}

prpts_path = file.path(resdir, 'dryvernet_pourpoints.csv')
watergap_dis_path = file.path(datdir, 'watergap2-2c_gfdl-esm2m_ewembi_picontrol_1860soc_co2_dis_global_daily_1671_1680.nc4')
#Can replace with a tiff like watergap_dis_path = file.path(datdir, '15sec_dis_12_2019.tiff')

#~~~~~~~~~~~~~~~ Utility function ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Iteratively extract values for a raster for a set of locations
#'
#' Takes paths a inputs, iteratively run terra::extract on a given subet of 
#' locations, and write the extracted values to fst format in output directory.
#'
#' @param in_rast_path (character) path to raster file to be read by terra::rast.
#' Note that this function is most efficient if run on multi-band rasters
#' (netcdf or tiff) rather than running separately on the bands.
#' @param lyrs (integer vector) indices of raster layers (bands) to extract.
#' @param in_coordtab_path (character) path to table of locations to use for 
#' extraction.Should have at least three columns: an ID col and two 
#' coordinate columns.
#' @param dropcol (integer vector) vector of column indices to drop when 
#' reading table of locations (for faster loading).
#' @xcol (character) name of the numeric column holding longitude coordinates
#' @ycol (character) name of the numeric column holding latitude coordinates
#' @outdir (character) path to directory where function outputs will be written
#' @out_pathroot (character) base name of the files that will be written
#' @iterstep (integer) number of locations to process at a time. Increasing this
#' number will speed up the analysis but require more memory.
#' @overwrite (boolean) whether to run extraction even if output file already exists
#' 
#' @details the output is written as {fst}. It can be read back as a data.frame
#' with read_fst(path).
#'
#' @return \link[data.table]{data.table} of paths to output fst files
#'
#' @export
iter_ras_extract <- function(in_rast_path, lyrs=1,
                             in_coordtab_path, dropcol=NULL, idcol="DRYVER_RIVID",
                             xcol='X', ycol='Y',
                             out_dir, out_pathroot, iterstep,
                             overwrite=F) {
  
  #Read raster to extract
  ras <- terra::rast(in_rast_path, lyrs=lyrs)
  
  #Read points, setting column names for coordinates to work with extract
  pts <- data.table::fread(in_coordtab_path, drop=1) %>%
    data.table::setnames(c(xcol, ycol), c('X', 'Y'))
  
  #Get number of points
  pts_n <- nrow(pts)
  
  #Create intervals of rows to select at a time
  if (pts_n > iterstep) {
    binlist <- seq(1, pts_n, iterstep) %>%
      data.table(
        l = .,
        u = c(.[2:length(.)], pts_n+1)-1
      )
  } else {
    binlist <- data.table(l = 1, u = pts_n)
  }
  
  #Iterate through ID intervals
  outputf_list <- lapply(
    seq(1, nrow(binlist)), function(i) {
      #Create output file path
      outf <- file.path(out_dir,
                        paste0(out_pathroot,
                               '_', format(binlist[i, l], scientific = FALSE),
                               "_", format(binlist[i, u], scientific=FALSE),
                               '.fst')
      )
      #Check whether it exists
      if ((!file.exists(outf)) | (overwrite == T)) {
        print(paste('Processing', outf))
        #Extract raster values by point, merge with original ID column
        terra::extract(
          x = ras,
          y = pts[binlist[i, l]:binlist[i, u], .(X, Y)],
          method="simple"
        ) %>%
          cbind(pts[binlist[i, l]:binlist[i, u], .(get(idcol))], .) %>%
          .[, ID := NULL] %>%
          write_fst(path=outf) #Write it all to fast read/write format
      }
      return(outf)
    })
}

#~~~~~~~~~~~~~~~ Run function ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
iter_ras_extract(in_rast_path = watergap_dis_path,
                 lyrs = 1,
                 in_coordtab_path = prpts_path,
                 dropcol = 'OBJECTID',
                 idcol = 'DRYVER_RIVID',
                 xcol = 'POINT_X',
                 ycol = 'POINT_Y',
                 out_dir = watergap_extractdir,
                 out_pathroot = 'watergap_dis_net',
                 iterstep = 1000000)
