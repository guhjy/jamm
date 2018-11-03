mr.initTable<-function(infos,resultsTable,n64,ciType,ciWidth) {

    resultsTable$getColumn('ci.lower')$setSuperTitle(jmvcore::format('{}% C.I. (a)', ciWidth))
    resultsTable$getColumn('ci.upper')$setSuperTitle(jmvcore::format('{}% C.I. (a)', ciWidth))
    
    for (i in seq_along(infos$ieffects)) {
        ie <- infos$ieffects[[i]]
        rowKey=paste0(ie,collapse = "_")
        aRow=list(source=.nicifychain64(ie,n64),type="Indirect")
        resultsTable$addRow(rowKey=rowKey,aRow)
        resultsTable$addFormat(rowKey=rowKey, col=1, jmvcore::Cell.BEGIN_GROUP)
        for (j in seq_len(length(ie)-1)) {
           value=c(ie[[j]],ie[[j+1]])
           rowKey <- paste0(value,collapse = "_")
           row<-list(source=.nicifychain64(value,n64),type="Component")
           resultsTable$addRow(rowKey=rowKey,row)
           n<-length(resultsTable$rowKeys)
           resultsTable$addFormat(rowNo=n, col=2, jmvcore::Cell.INDENTED)
        }
    }
    resultsTable$addFormat(rowKey=rowKey, col=1, jmvcore::Cell.END_GROUP)

    for (te in infos$totaleffects) {
        rowKey=paste0(te,collapse = "_")
        row<-list(source=.nicifychain64(te,n64),type="Direct")
        resultsTable$addRow(rowKey,row)
        }

    resultsTable$addFormat(rowKey=paste0(infos$totaleffects[[1]],collapse = "_"),col=1,jmvcore::Cell.BEGIN_GROUP)

    for (te in infos$totaleffects) {
         rowKey=paste0(te,collapse = "_t_")
         row<-list(source=.nicifychain64(te,n64),type="Total")
         resultsTable$addRow(rowKey,row)
    }
    resultsTable$addFormat(rowKey=paste0(infos$totaleffects[[1]],collapse = "_t_"),col=1,jmvcore::Cell.BEGIN_GROUP)


    add<-ifelse(ciType=="standard" || ciType=="none","",". This may take a while")
    .note<-paste0(NOTES[["ci"]][[ciType]],add)
    resultsTable$setNote("cinote",paste("(a) Confidence intervals computed with method:",.note))
}