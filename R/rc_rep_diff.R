#' @name rc_rep_diff
#'
#' @title Take differences for each sequential pair of replicate variables
#' @description For each variable root supplied, all variables containing the root in the
#' record data will be collected and a new column containing difference values for
#' each sequential pair will be created.
#'
#' @param record_data A raw data export from REDCap.
#' @param var_roots A vector of root names which are common and unique to all of the associated
#' technical replicates.
#'
#' @author Marcus Lehr
#' 
#' @export

rc_rep_diff = function(record_data, var_roots) {

  
  validate_args(required = c('record_data','var_roots'),
                record_data = record_data, var_roots = var_roots)
  
  # get ID field name
  id_field = getID(record_data)


  # Instantiate dataframe to collect difference data
  diffData = dplyr::select(record_data, !!dplyr::sym(id_field), redcap_event_name)

  for (v in 1:length(var_roots)){
    # Select columns
    currentRep = as.data.frame(dplyr::select(record_data, dplyr::contains(var_roots[v])))

    for (r in 1:(ncol(currentRep)-1)) {
      # Move through columns sequentially creating a subtraction for each pair
      currentRep = mutate(currentRep, !!paste0(var_roots[v],'_diff',r) := abs(currentRep[,r]-currentRep[,r+1]))
      # Label new columns
      attr(currentRep[,ncol(currentRep)], "label") =
        paste0('|',colnames(currentRep)[r],' - ',colnames(currentRep)[r+1],'|')
    }
    diffData = cbind(diffData, select(currentRep, contains('_diff')))
  }

  record_data = cbind(record_data, diffData)
}
