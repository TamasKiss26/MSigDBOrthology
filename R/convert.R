#' convert ids of MSigDB to another organism
#'
#' This function gets two files: one is downloaded from the MSigDB, it is .gmt file, the other is a .txt file downloaded form the HCOP database.
#'
#'
#' @param msigdbPath path to the MSigDB file
#'
#' @param hcopPaht path to the hcop annotation file
#'
#' @param idType 'entrez' or 'ensembl'
#'
convertMSigDB <- function(msigdbPath, hcopPath, idType = 'ensembl'){

  # get msigdb data
  msigdb <- GSEABase::getGmt(msigdbPath)
  msigdb <- GSEABase::geneIds(msigdb)

  # get hcop data
  hcop <- readr::read_delim(hcopPath, delim = '\t')

  pb <- dplyr::progress_estimated(length(msigdb))

  res <- purrr::map2(
    .x = msigdb,
    .y = names(msigdb),
    .f = function(origID, nm){

      pb$tick()$print()

      altID <- hcop%>%
        dplyr::filter(human_entrez_gene %in% origID)%>%
        dplyr::pull(paste('mouse_', idType, '_gene', sep = ''))%>%
        base::unique()

      altID <- altID[!altID == '-']

      altID <- GSEABase::GeneSet(altID, setName = nm)

      return(altID)
    }

  )

  #res <- GSEABase::GeneSetCollection(res)

  rm(msigdb, hcop)

  return(res)

}
