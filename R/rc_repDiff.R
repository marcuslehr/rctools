#' @name rc_repDiff
#'
#' @title Report differences between technical replicates
#' @description  Take differences for each sequential pair of replicate variables
#' @details For each variable root supplied, all variables containing the root in the
#' record data will be collected and a new column containing difference values for
#' each sequential pair will be created.
#'
#' @param records A raw data export from REDCap.
#' @param varRoots A vector of root names which are common and unique to all of the associated
#' technical replicates.
#' @param bundle A bundle object created by \code{exportBundle} containing project metadata.
#'
#' @author Marcus Lehr
#' @export

rc_repDiff = function(records, varRoots, bundle = NULL) {


# Checks ------------------------------------------------------------------

  ## record_id field ---

  if (!is.null(bundle[["meta_data"]])) {
    # Ensure record_id field is named appropriately
    colnames(records)[colnames(records)==bundle[["meta_data"]][1,1]] = 'record_id'

  } else {
    origID = names(records)[grepl('hnrcid|hnrc_id|record_id', names(records), ignore.case = T)]

    if (length(origID) == 1) {
      message("[['meta_data']] not found in bundle object. 'record_id' field will be assumed to be: '", origID, "'")
      names(records)[names(records)==origID] = 'record_id'

    } else if (length(origID != 1)) {
      names(records)[1] = 'record_id'
      message("'record_id' field could not be found, it will assumed to be the first column.
              For better reproducibility, please supply a bundle object containing [['meta_data']]")
    }
  }

# Code --------------------------------------------------------------------

  diffData = dplyr::select(records, record_id, redcap_event_name)

  for (v in 1:length(varRoots)){
    currentRep = as.data.frame(select(records, contains(varRoots[v])))

    for (r in 1:(ncol(currentRep)-1)) {
      currentRep = mutate(currentRep, !!paste0(varRoots[v],'_diff',r) := abs(currentRep[,r]-currentRep[,r+1]))
      attr(currentRep[,ncol(currentRep)], "label") =
        paste0('|',colnames(currentRep)[r],' - ',colnames(currentRep)[r+1],'|')
    }
    diffData = cbind(diffData, select(currentRep, contains('_diff')))
  }

  records = cbind(records, diffData)
}
