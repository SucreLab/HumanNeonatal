# Title     : Cluster & Marker Gene Helper
# Objective : Combine the clustering steps into one function;
#             Multithread the process of marker gene finding
# Author : Shawyon Shirazi & Nick Negretti
# Date : 3/13/24


#' Run the common Seurat clustering, PCA, and UMAP
#' Includes the option to harmonize the data to counter batch effects
#'
#' @param dims_umap dimensions to run UMAP
#' @param dims_neighbors How many dims for neighbor finding
#' @param k_param k for finding neighbors
#' @param cluster_res Resolution for clustering
#' @return Seurat object
cluster_pca_umap <- function(obj,
                             dims_umap = 1:15,
                             dims_neighbors = 1:15,
                             k_param = 10,
                             cluster_res = 0.3,
                             umap_neighbors = 30,
                             harmony = TRUE,
                             harmonylambda = 1,
                             harmonysigma = 0.1,
                             assay = "SCT",
                             algorithm = 1) {
  if ("integrated" %in% names(obj)) {
    Seurat::DefaultAssay(obj) <- "integrated"
  }
  obj <- Seurat::RunPCA(obj, verbose = FALSE)
  if (harmony) {
    obj <- RunHarmony(obj,
                      "dataset",
                      assay.use = assay,
                      lambda = harmonylambda,
                      sigma = harmonysigma)
    obj <- Seurat::RunUMAP(obj,
                           reduction = "harmony",
                           dims = dims_umap,
                           verbose = FALSE,
                           n.neighbors = umap_neighbors)
    obj <- Seurat::FindNeighbors(obj,
                                 reduction = "harmony",
                                 dims = dims_neighbors,
                                 k.param = k_param)
  } else {
    obj <- Seurat::RunUMAP(obj,
                           dims = dims_umap,
                           verbose = FALSE,
                           n.neighbors = umap_neighbors)
    obj <- Seurat::FindNeighbors(obj,
                                 dims = dims_neighbors,
                                 k.param = k_param)
  }
  obj <- Seurat::FindClusters(obj,
                              resolution = cluster_res,
                              algorithm = algorithm)
  if ("integrated" %in% names(obj)) {
    Seurat::DefaultAssay(obj) <- "SCT"
  }
  return(obj)
}

#' This can use a ton of memory, but it will save a lot of time
#' Important: The output is the same order as levels(Idents(obj))
#'
#' This needs to export the Seurat obj to all workers - this takes a lot of RAM.
#'
#' @param obj Seurat object
#' @return List of data tables, one for each numeric cluster
parallel_find_all_markers <- function(obj) {

  all_markers <- future.apply::future_lapply(levels(Seurat::Idents(obj)),
                                             function(x) {
                                                        Seurat::FindMarkers(obj,
                                                        ident.1 = x,
                                                        ident.2 = NULL,
                                                        test.use = "MAST")
    }
  )
  return(future::value(all_markers))
}