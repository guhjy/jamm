jammGLMClass <- R6::R6Class(
  "jammGLMClass",
  inherit = jammGLMBase,
  private=list(
    .model=NA,
    .names64=NA,
    .infos=NULL,
    .cov_condition=conditioning$new(),
    .init=function() {
      mark("init")
      private$.names64<-names64$new()
      dep<-self$options$dep
      covs<-self$options$covs
      factors<-self$options$factors
      mediators<-self$options$mediators
      ciWidth<-self$options$ciWidth
      ciType<-self$options$ciType
      
      ### here we initialize things ####
      data<-private$.cleandata()
      infos<-private$.prepareDiagram() 

      if (infos$isImpossible)   return()
      if (infos$hasRequired())   return()
      
      meds<-lapply(infos$original_medmodels, function(m) {
        m$ind=private$.names64$factorize(m$ind)
        m
      })
      
      full<-infos$original_fullmodel
      full$ind<-private$.names64$factorize(full$ind)
      
      mods<-lapply(infos$moderators, function(m) {
        private$.names64$factorize(m)
      })
      
      infos<-smartMediation$new(meds,full,moderators = mods)
      private$.infos<-infos
      ## prepare main result table
      table<-self$results$models$main
      table$getColumn('ci.lower')$setSuperTitle(jmvcore::format('{}% C.I. (a)', ciWidth))
      table$getColumn('ci.upper')$setSuperTitle(jmvcore::format('{}% C.I. (a)', ciWidth))
      for (i in seq_along(infos$ieffects)) {
        ie <- infos$ieffects[[i]]
        rowKey=paste0(ie,collapse = "_")
        aRow=list(source=.nicifychain64(ie,private$.names64),type="Indirect")
        table$addRow(rowKey=rowKey,aRow)
        table$addFormat(rowKey=rowKey, col=1, jmvcore::Cell.BEGIN_GROUP)
        for (j in seq_len(length(ie)-1)) {
          value=c(ie[[j]],ie[[j+1]])
          rowKey <- paste0(value,collapse = "_")
          row<-list(source=.nicifychain64(value,private$.names64),type="Component")
          table$addRow(rowKey=rowKey,row)
          n<-length(table$rowKeys)
          table$addFormat(rowNo=n, col=2, jmvcore::Cell.INDENTED)
        }
        }
        table$addFormat(rowKey=rowKey, col=1, jmvcore::Cell.END_GROUP)
      
      for (te in infos$totaleffects) {
        rowKey=paste0(te,collapse = "_")
        row<-list(source=.nicifychain64(te,private$.names64),type="Total")
        table$addRow(rowKey,row)
      }
      if (is.something(infos$moderators))
          table$setTitle("Indirect and Total Effects (averaged across moderators)")
      
      add<-ifelse(ciType=="standard" || ciType=="none","",". This may take a while")
      .note<-paste0(NOTES[["ci"]][[ciType]],add)
      table$setNote("cinote",paste("(a) Confidence intervals computed with method:",.note))
        
      ### here we go, enter statistics     
      mi.initContrastCode(data,self$options,self$results,n64)
    },
    .run=function() {
      n64<-private$.names64
      mark("run")
      # collect some option
      dep <- self$options$dep
      factors <- self$options$factors
      covs <- self$options$covs
      mediators <- self$options$mediators
      infos<-private$.infos
      ciWidth<-self$options$ciWidth/100
      ciType<-self$options$ciType
      bootN<-self$options$bootN
      
      if (is.null(dep))
        return()
      if (is.null(mediators))
        return()
      if (is.null(covs) && is.null(factors))
        return()
      if (self$options$simpleScale=="mean_sd" && self$options$cvalue==0)
          return()
      if (self$options$simpleScale=="percent" && self$options$percvalue==0)
         return()
      ###############      
      data<-private$.cleandata()
      for (scaling in self$options$scaling) {
        data[[jmvcore::toB64(scaling$var)]]<-lf.scaleContinuous(data[[jmvcore::toB64(scaling$var)]],scaling$type)  
      }

      if (!is.null(covs)) {
        private$.cov_condition$storeValues(data)
        private$.cov_condition$labels_type=self$options$simpleScaleLabels
      }
      infos<-private$.infos
      ## fill main mediational results
      ## notice that jmf.modelSummaries return first the individual coefficients
      ## and then the mediated effect. Because in .init the mediated effec is defined ad
      ## the first row, it format the table well because it uses the rowKey appropriately
      
      table<-self$results$models$main
      se<-ifelse(ciType=="standard" || ciType=="none",ciType,"bootstrap")
      fit<-jmf.mediationSummary(infos,data, se=se,bootN=bootN)
      params<-jmf.mediationInference(fit,level = ciWidth, boot.ci=ciType)

      table$setNote("cinote",paste("(a) Confidence intervals computed with method:",NOTES[["ci"]][[ciType]]))
      for (rowKey in table$rowKeys) {
      row<-params[params$label==rowKey,]
      table$setRow(rowKey=rowKey,row)
      }
    },
  .cleandata=function() {
      n64<-private$.names64
      dep <- self$options$dep
      factors <- self$options$factors
      covs <- self$options$covs
      mediators<-self$options$mediators

      dataRaw <- self$data
      data <- list()
      for (factor in factors) {
        ### we need this for Rinterface ####
        if (!("factor" %in% class(dataRaw[[factor]]))) {
          info(paste("Warning, variable",factor," has been coerced to factor"))
          dataRaw[[factor]]<-factor(dataRaw[[factor]])
        }
        data[[jmvcore::toB64(factor)]] <- dataRaw[[factor]]
        levels <- base::levels(data[[jmvcore::toB64(factor)]])
        stats::contrasts(data[[jmvcore::toB64(factor)]]) <- lf.createContrasts(levels,"deviation")
        n64$addFactor(factor,levels)
        n64$addLabel(factor,lf.contrastLabels(levels, "deviation")) 
        attr(data[[jmvcore::toB64(factor)]],"jcontrast")<-"deviation"
      }
      
      for (contrast in self$options$contrasts) {
        levels <- base::levels(data[[jmvcore::toB64(contrast$var)]])
        stats::contrasts(data[[jmvcore::toB64(contrast$var)]]) <- lf.createContrasts(levels, contrast$type)
        n64$addLabel(contrast$var,lf.contrastLabels(levels, contrast$type)) 
        attr(data[[jmvcore::toB64(contrast$var)]],"jcontrast")<-contrast$type
        dummies<-model.matrix(as.formula(paste0("~",jmvcore::toB64(contrast$var))),data=data)
        dummies<-dummies[,-1]
        dummies<-data.frame(dummies)
        names(dummies)<-unlist(n64$contrasts(contrast$var))
        data<-cbind(data,dummies)
      }
      
      if ( ! is.null(dep)) {
        data[[jmvcore::toB64(dep)]] <- jmvcore::toNumeric(dataRaw[[dep]])
        n64$addVar(dep)
      }
      
      for (covariate in covs) {
        data[[jmvcore::toB64(covariate)]] <- jmvcore::toNumeric(dataRaw[[covariate]])
        n64$addVar(covariate)
      }
      
      for (med in mediators) {
        data[[jmvcore::toB64(med)]] <- jmvcore::toNumeric(dataRaw[[med]])
        n64$addVar(med)
      }
      
      private$.names64<-n64
      data<-as.data.frame(data)      

      data <- jmvcore::naOmit(data)
      return(data)
      
    },


.prepareDiagram=function() {

  infoTable<-self$results$info
  
  n64<-private$.names64
  
  dep64<-jmvcore::toB64(self$options$dep)
  covs64<-jmvcore::toB64(self$options$covs)
  factors64<-jmvcore::toB64(self$options$factors)
  mediators64<-jmvcore::toB64(self$options$mediators)
  modelTerms<-self$options$modelTerms

  mediatorsTerms<-self$options$mediatorsTerms
  moderatorsTerms<-self$options$moderatorsTerms

  n64<-private$.names64
  
  ### update model info table
  goon<-ds.initModelInfo(self)  
  
  ## build the models list
  medmodels64<-list()
  for (i in seq_along(mediators64))  
       medmodels64[[i]]<-list(dep=mediators64[i],ind=sapply(mediatorsTerms[[i]],jmvcore::toB64))

  fullmodel64<-list(dep=dep64,ind=sapply(modelTerms,jmvcore::toB64))

  modTerms64<-moderatorsTerms
    for (i in seq_along(mediators64))  
       for (j in seq_along(moderatorsTerms[[i]]))
         modTerms64[[i]][[j]]<-jmvcore::toB64(moderatorsTerms[[i]][[j]])
       

  #### let smart do the magic ####
  infos<-smartMediation$new(medmodels64,fullmodel64,moderators = modTerms64)
  #### prepare the diagram
  image <- self$results$pathmodelgroup$get('pathmodel')
  paths<-diag.paths(infos,suggested = T)

  # for (i in seq_along(paths$labs))
  #         if (paths$labs[[i]] %in% factors) {
  #            n<-length(n64$nicecontrasts(paths$labs[[i]]))
  #            paths$labs[[i]]<-paste0(paths$labs[[i]]," (",paste(1:n,collapse=","),")")
  #         }
  ## save the results for showing later      
  image$setState(list(paths=paths,infos=infos))
  #### includes possible diagrams notes 
      notes<-self$results$pathmodelgroup$pathnotes
      ds.annotate.diagram(infos,paths,notes,self$options,n64)       
   ds.modelInfo(infos,self,n64)
   return(infos)      
      
},  

.showDiagram=function(image, ggtheme, theme, ...) {

    if (is.null(image$state))
        return()
  
  
  infos<-image$state$infos
  paths<-image$state$paths
  box.size=.1+(max(length(infos$mediators),length(infos$independents))+3)^-8
  box.text=.80+(infos$nvars)^-2
  arr.lenght=1/(infos$nvars-1)
  labs<-jmvcore::fromB64(paths$labs)
  ### first we plot the linear models paths diagram
  plot<-diagram::plotmat(paths$paths, pos=paths$pos, 
                  name= labs,box.size = box.size,
                  box.type = "rect", box.prop=.4, box.cex = box.text , curve=paths$curves,
                  endhead=F, arr.type="triangle",arr.length = arr.lenght,
                  ,arr.pos=.7,arr.col = "gray",lcol = paths$colors,box.lcol=paths$bcolors)

  ### then we add the moderators
  mp<-infos$moderatedPaths()
   for (i in seq_along(mp)) 
     for (j in seq_along(mp[[i]])) {
          coord=mp[[i]][[j]]
          diag.plot_mod_arr(plot,coord$from,coord$to,i)
        }

  diag.plot_mods(plot,infos$moderators)

  TRUE
},
.sourcifyOption = function(option) {
        name <- option$name
        value <- option$value
        
        super$.sourcifyOption(option)
}
))


