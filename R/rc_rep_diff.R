#' @name rc_rep_diff
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
#' @param data_dict REDCap project data data_dictionary. By default, 
#' $data_dict is expected in a REDCap bundle object, as created by \code{rc_setup}.
#' Otherwise, a data.frame containing the metadata must be supplied.
#'
#' @author Marcus Lehr
#' @export

rc_rep_diff = function(records, varRoots, 
                      data_dict = getOption("redcap_bundle")$data_dict) {


# Checks ------------------------------------------------------------------

  ## record_id field ---
  
  if (!is.null(data_dict)) {
    # Ensure record_id field is named appropriately
    colnames(records)[colnames(records)==data_dict[1,1]] = 'record_id'
    
  } else {
    message("$data_dict not found in REDCap bundle. 'record_id' field will be assumed to be the first column")
    names(records)[1] = 'record_id'
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
