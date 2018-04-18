#' Return a data.table with possible models supported by caret along with tags and library dependencies
#' @param use.cache boolean to indicate whether to use the cached table or to generate it live
#' @return caret model browser table
#' @export
caretModelInfoTable <- function(compact = T,
                                use.cache = T) {
  require(data.table)
  require(caret)

  if (compact) {
    dt <- as.data.table(modelLookup())
    return(dt[,list(parameters = .N),by = c("model", "forClass", "forReg", "probModel")])
  }

  if (use.cache) {
    dt <- as.data.table(shinypipe:::t.modelBrowser)
  } else {
    require(caret)
    getInfoTableFromObj <- function (model, info){
      data.table(Name=model,
                 Description=paste(info$label, collapse=", "),
                 Type=paste(info$type, collapse=", "),
                 Library.Dependencies=paste(info$library, collapse=", "),
                 Tags=paste(info$tags, collapse=", "))
    }

    m <- names(getModelInfo())
    dt <- rbindlist(lapply(1:length(m), function(i) { getInfoTableFromObj(m[i], getModelInfo(m[i], F)[[m[i]]])}))
  }
  return (dt)
}


#' Check whether a caret supported model is runnable on the current machine in terms of
#' satisfying library dependencies
#' @param model name of the model
#' @return boolean
#' @export
caretModelLibCheck <- function(model) {
  pckgs <- rownames(installed.packages())
  reqs <- trimws(unlist(strsplit(caretModelInfoTable(F)[Name==model,Library.Dependencies], split=",")))
  return (sum(unlist(lapply(reqs, function(l) !(l %in% pckgs)))) == 0)
}

