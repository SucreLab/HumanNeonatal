set.seed(42) # For reproducability
N_WORKERS <- 2
n_print <- 1:20
options(future.globals.maxSize = 20 * 1024 * 1024^2)

#' @param x A sparse matrix from the Matrix package.
#' @param file A filename that ends in ".gz".
#' From: https://slowkow.com/notes/sparse-matrix/#writemmgz
writeMMgz <- function(x,
                      file) {
  mtype <- "real"
  if (is(x, "ngCMatrix")) {
    mtype <- "integer"
  }
  writeLines(
    c(
      sprintf("%%%%MatrixMarket matrix coordinate %s general", mtype),
      sprintf("%s %s %s", x@Dim[1], x@Dim[2], length(x@x))
    ),
    gzfile(file)
  )
  data.table::fwrite(
    x = summary(x),
    file = file,
    append = TRUE,
    sep = " ",
    row.names = FALSE,
    col.names = FALSE
  )
}

pbzip2_location <- Sys.which("pbzip2")
if (pbzip2_location != "") {
  print("pbzip2 available")
} else {
  errorCondition("Please install pbzip2 before running")
}

saveTiff <- function(path,
                     image,
                     width = 5,
                     height = 5,
                     dpi = 300,
                     units = "in") {
  if (!file.exists(path)) {
    dir.create(dirname(path), showWarnings = FALSE)
  }

  if (Sys.info()["sysname"] == "Darwin") {
    # lzw doesn't work on mac with quartz
    tmp_path <- suppressWarnings(normalizePath(paste0(path, "_tmp.tiff")))
    out_path <- suppressWarnings(normalizePath(path))

    tiff(tmp_path, width = width, height = height, units = units, res = dpi,
         compression = "lzw", type = "quartz")
    print(image)
    dev.off()
    # requires imagemagick
    Sys.sleep(0.5)
    system(paste0("convert ", tmp_path, " -density ", dpi,  " -resize ",
                  width * dpi, "x", height * dpi, "\\> -compress lzw ",
                  out_path),
           ignore.stdout = TRUE)

    if (file.exists(out_path)) {
      #Delete file if it exists
      file.remove(tmp_path)
    }

  } else {
    tiff(path, width = width, height = height, units = units, res = dpi,
         compression = "lzw")
    print(image)
    dev.off()
  }

}

# From https://gist.github.com/retrography/359e0cc56d2cf1acd161b5645bc801a8
# The functions below use parallelized versions of gzip, xz, and bzip2 to
# improve compression/decompression performance of RDS serialization in R.
# Each function searches for the appropriate program (based on the required
# compression format) and if found, offloads the compression handling to the
# external program and therefore leaves R free to do the data import/export.
# The two main functions (saveRDS and readRDS) mask R's native read and write
# functions. The functions have been only tested on macOS, but they must work
# on any Linux/Unix.
#
# Requires the following packages: pxz, pbzip2, and pigz.
#
# Run the following line at the command prompt before using the functions.
#
#     brew install pigz pbzip2 pigz
#

library(parallel)

cmdAvail <- function(cmd) as.logical(nchar(Sys.which(cmd)))

writeRDS <- function(object, con) {
  tryCatch({
    base::saveRDS(
      object,
      file = con
    )
  }, warning = function(w) {
    print(paste("WARNING: ", w))
  }, error = function(e) {
    print(paste("ERROR: ", e))
  }, finally = {
    close(con)
  })
}

loadRDS <- function(con) {
  tryCatch({
    base::readRDS(
      file = con
    )
  }, warning = function(w) {
    print(paste("WARNING: ", w))
  }, error = function(e) {
    print(paste("ERROR: ", e))
  }, finally = {
    close(con)
  })
}

saveRDS.xz <- function(object,
                       file,
                       threads = parallel::detectCores(),
                       compression_level = 6) {
  if (cmdAvail("pxz")) {
    writeRDS(
      object,
      pipe(
        paste0(
          "pxz -c -k -T",
          threads,
          " -",
          compression_level,
          " > ",
          file
        ),
        "wb"
      )
    )
  } else {
    base::saveRDS(
      object,
      file = file,
      compress = "xz"
    )
  }
}

readRDS.xz <- function(file,
                       threads = parallel::detectCores()) {
  if (cmdAvail("pxz")) {
    object <-
      loadRDS(
        pipe(
          paste0(
            "pxz -d -k -c -T",
            threads,
            " ",
            file
          )
        )
      )
  } else {
    object <-
      base::readRDS(
        file
      )
  }
  return(object)
}

saveRDS.gz <- function(object,
                       file,
                       threads = parallel::detectCores(),
                       compression_level = 6) {
  if (cmdAvail("pigz")) {
    writeRDS(
      object,
      pipe(
        paste0(
          "pigz -c -k -p",
          threads,
          " -",
          compression_level,
          " > ",
          file
        ),
        "wb"
      )
    )
  } else {
    base::saveRDS(
      object,
      file = file,
      compress = "gzip"
    )
  }
}

readRDS.gz <- function(file,
                       threads = parallel::detectCores()) {
  if (cmdAvail("pigz")) {
    object <-
      loadRDS(
        pipe(
          paste0(
            "pigz -d -k -c -p",
            threads,
            " ",
            file
          )
        )
      )
  } else {
    object <-
      base::readRDS(
        file
      )
  }
  return(object)
}

saveRDS.bz2 <- function(object,
                        file,
                        threads = parallel::detectCores(),
                        compression_level = 9) {
  if (cmdAvail("pbzip2")) {
    writeRDS(
      object,
      pipe(
        paste0(
          "pbzip2 -c -k -p",
          threads,
          " -",
          compression_level,
          " > ",
          file
        ),
        "wb"
      )
    )
  } else {
    base::saveRDS(
      object,
      file = file,
      compress = "bzip2"
    )
  }
}

readRDS.bz2 <- function(file,
                        threads = parallel::detectCores()) {
  if (cmdAvail("pbzip2")) {
    object <-
      loadRDS(
        pipe(
          paste0(
            "pbzip2 -d -k -c -p",
            threads,
            " ",
            file
          )
        )
      )
  } else {
    object <-
      base::readRDS(
        file
      )
  }
  return(object)
}

readRDS <- function(file,
                    threads = parallel::detectCores()) {
  if (!file.exists(file)) {
    stop(
      paste0(
        file,
        " does not exist!"
      )
    )
  }
  fileDetails <-
    system2(
      "file",
      args = file,
      stdout = TRUE
    )
  selector <-
    sapply(
      c("gzip", "XZ", "bzip2"),
      function(x) grepl(x, fileDetails)
    )
  format <-
    names(selector)[selector]
  if (length(format) == 0) format <- "none"
  if (format == "gzip") {
    object <- readRDS.gz(file, threads = threads)
  } else if (format == "XZ") {
    object <- readRDS.xz(file, threads = threads)
  } else if (format == "bzip2") {
    object <- readRDS.bz2(file, threads = threads)
  } else {
    object <- force(base::readRDS(file))
  }
  return(object)
}

saveRDS <- function(object,
                    file = "",
                    compress = TRUE) {
  if (compress %in% c(TRUE, "gz", "gzip")) {
    saveRDS.gz(object, file)
  } else if (compress %in% c("bzip", "bzip2", "bz", "bz2")) {
    saveRDS.bz2(object, file)
  } else if (compress %in% c("xz", "7zip", "7z")) {
    saveRDS.xz(object, file)
  } else if (compress == FALSE) {
    base::saveRDS(object, file)
  } else {
    stop(paste0(compress, " is not a recognized compression method!"))
  }
}

ReadCB_h5 <- function(filename,
                      use.names = TRUE,
                      unique.features = TRUE) {
  if (!requireNamespace("hdf5r", quietly = TRUE)) {
    stop("Please install hdf5r to read HDF5 files")
  }
  if (!file.exists(filename)) {
    stop("File not found")
  }
  infile <- hdf5r::H5File$new(filename = filename, mode = "r")
  genomes <- c("matrix")
  output <- list()
  if (hdf5r::existsGroup(infile, "matrix")) {
    # cellranger version 3+
    message("CellRanger version 3+ format H5")
    if (use.names) {
      feature_slot <- "features/name"
    } else {
      feature_slot <- "features/id"
    }
  } else {
    message("CellRanger version 2 format H5")
    if (use.names) {
      feature_slot <- "gene_names"
    } else {
      feature_slot <- "genes"
    }
  }
  for (genome in genomes) {
    counts <- infile[[paste0(genome, "/data")]]
    indices <- infile[[paste0(genome, "/indices")]]
    indptr <- infile[[paste0(genome, "/indptr")]]
    shp <- infile[[paste0(genome, "/shape")]]
    features <- infile[[paste0(genome, "/", feature_slot)]][]
    barcodes <- infile[[paste0(genome, "/barcodes")]]
    sparse.mat <- sparseMatrix(
      i = indices[] + 1,
      p = indptr[],
      x = as.numeric(x = counts[]),
      dims = shp[],
      giveCsparse = FALSE
    )
    if (unique.features) {
      features <- make.unique(names = features)
    }
    rownames(x = sparse.mat) <- features
    colnames(x = sparse.mat) <- barcodes[]
    sparse.mat <- as(object = sparse.mat, Class = "CsparseMatrix")
    # Split v3 multimodal
    if (infile$exists(name = paste0(genome, "/features"))) {
      types <- infile[[paste0(genome, "/features/feature_type")]][]
      types.unique <- unique(x = types)
      if (length(x = types.unique) > 1) {
        message("Genome ", genome, " has multiple modalities, returning a list 
                                    of matrices for this genome")
        sparse.mat <- sapply(
          X = types.unique,
          FUN = function(x) {
            return(sparse.mat[which(x = types == x), ])
          },
          simplify = FALSE,
          USE.NAMES = TRUE
        )
      }
    }
    output[[genome]] <- sparse.mat
  }
  infile$close_all()
  if (length(x = output) == 1) {
    return(output[[genome]])
  } else{
    return(output)
  }
}
