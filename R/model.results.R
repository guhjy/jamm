mr.initTable<-function(infos,resultsTable,n64,ciType,ciWidth,tableOptions) {

    resultsTable$getColumn('ci.lower')$setSuperTitle(jmvcore::format('{}% C.I. (a)', ciWidth))
    resultsTable$getColumn('ci.upper')$setSuperTitle(jmvcore::format('{}% C.I. (a)', ciWidth))
    ierecoded<-lapply(infos$ieffects, function(x) gsub(":","____",x))
    components<-list()
    for (i in seq_along(ierecoded)) {
        ie <- ierecoded[[i]]
        ienames<-infos$ieffects[[i]]
        rowKey=paste0(ie,collapse = "_")
        aRow=list(source=.nicifychain64(ienames,n64),type="Indirect")
        resultsTable$addRow(rowKey=rowKey,aRow)
#        resultsTable$addFormat(rowKey=rowKey, col=1, jmvcore::Cell.BEGIN_GROUP)
        
        for (j in seq_len(length(ie)-1)) {
          valueName=c(ienames[[j]],ienames[[j+1]])
          valueKey=c(ie[[j]],ie[[j+1]])
          crowKey <- paste0(valueKey,collapse = "_")
          row<-list(source=.nicifychain64(valueName,n64),type="Component")
          components[[crowKey]]<-row
        }
        
    }
    resultsTable$addFormat(rowKey=rowKey, col=1, jmvcore::Cell.END_GROUP)
    if ("component" %in% tableOptions)
         for (rowKey in names(components)) {
             resultsTable$addRow(rowKey=rowKey,components[[rowKey]])
             n<-length(resultsTable$rowKeys)
             resultsTable$addFormat(rowNo=n, col=2, jmvcore::Cell.INDENTED)
         }
    
    totalrecoded<-lapply(infos$totaleffects, function(x) gsub(":","____",x))
    
    for (i in seq_along(totalrecoded)) {
        teKey<-totalrecoded[[i]]
        teName<-infos$totaleffects[[i]]
        rowKey=paste0(teKey,collapse = "_")
        row<-list(source=.nicifychain64(teName,n64),type="Direct")
        resultsTable$addRow(rowKey,row)
        }

    resultsTable$addFormat(rowKey=paste0(infos$totaleffects[[1]],collapse = "_"),col=1,jmvcore::Cell.BEGIN_GROUP)
    for (i  in seq_along(totalrecoded)) {
         teKey<-totalrecoded[[i]]
         teName<-infos$totaleffects[[i]]
         rowKey=paste0(teKey,collapse = "_t_")
         row<-list(source=.nicifychain64(teName,n64),type="Total")
         resultsTable$addRow(rowKey,row)
    }
    resultsTable$addFormat(rowKey=paste0(infos$totaleffects[[1]],collapse = "_t_"),col=1,jmvcore::Cell.BEGIN_GROUP)


    add<-ifelse(ciType=="standard" || ciType=="none","",". This may take a while")
    .note<-paste0(NOTES[["ci"]][[ciType]],add)
    resultsTable$setNote("cinote",paste("(a) Confidence intervals computed with method:",.note))
}