
jmf.mediationSummary<-function(infos,data,se="standard", nboot=1000) {

  models<-infos$original_medmodels
  models[[length(models)+1]]<-infos$original_fullmodel
  formulas<-.modelFormulas(models)
  lavformula<-paste(formulas,collapse = " ; ")
  for (ie in infos$ieffects) {
    modifiers<-list()
    for (i in 1:(length(ie)-1))
      modifiers[[i]]<-paste0(ie[i],"_",ie[i+1])
   amodifier<-paste(modifiers,collapse = "*")
   amodifier<-paste(paste0(ie,collapse = "_"),amodifier,sep = ":=")
   lavformula<-paste(lavformula,amodifier,sep=";")
  }
  fit<-try(lavaan::sem(lavformula,data=data,se = se, bootstrap = nboot))
  if (jmvcore::isError(fit)) {
   msg<-jmvcore::extractErrorMessage(fit)
   if (is.something(grep("positive definite",msg)))
       jmvcore::reject("The model cannot be estimated. Please check whether the independent variables are too correlated or the model is ill-defined")
  }
  fit
}

jmf.mediationInference<-function(fit,level=.95, boot.ci=NULL) {
    if (is.null(boot.ci))
          lavaan::parameterestimates(fit,level=level,standardized = T)
  else
          lavaan::parameterestimates(fit,level=level,boot.ci.type = boot.ci,standardized = T)

}

jmf.mediationTable<-function(params,infos) {
  res<-list()
  for (ie in infos$ieffects) {
    label<-paste0(ie,collapse ="_")
    arow<-params[params$label==label,]
    res[[label]]<-arow
  }
  res
}  

.modelFormulas<-function(models) {
  lapply(models, function(m) {
    .modelFormula(m)
  })
}

.modelFormula<-function(alist) {
  dep <- alist$dep
  terms<-sapply(alist$ind,function(a) {
    if (is.something(grep(":",a,fixed = T)))
       a    
    else
      paste(paste0(a,"_",dep),a,sep=" * ")
  })
  terms<-paste(terms,collapse = " + ")
  lformula<-paste(dep,"~",terms)
  return(lformula)
  
}

