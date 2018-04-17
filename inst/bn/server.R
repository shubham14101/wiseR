library('bnlearn')
library('networkD3')
library('rhandsontable')
library('shiny')
library('shinydashboard')
library('dplyr')
library('visNetwork')
library('shinyWidgets')
library('missRanger')
library('tools')
library('shinyalert')
library('shinycssloaders')
library('rintrojs')
library('arules')
library('rcompanion')
library('psych')
library('DescTools')
library("DT")
library("linkcomm")
library('igraph')
library("parallel")
library("snow")
library("shinyBS")
source('error.bar.R')
source('graph.custom.R')
source('graph.custom.assoc.R')
source('custom.discretize.R')
source('check.NA.R')
source('check.discrete.R')
source('custom.association.R')
source('custom.Modules.R')
source('tooltip.R')

shinyServer(function(input, output,session) {
  withProgress(message = "Initializing Dashboard", value = 0, {
  #Data upload limit and other options
  options(shiny.maxRequestSize=1500*1024^2)
  options(warn=-1)
  options("getSymbols.warning4.0"=FALSE)
  #tooltips
  tooltip(session)
  #Structure Initialization
  DiscreteData <- alarm
  trueData<-DiscreteData
  #Sanity check
  sanity<-1
  confidence<-1
  check<-1
  reset<-1
  assocReset<-1
  simple<-1
  upload<-1
  uploadtype<-1
  type<-1
  #Initialization
  rvs <<- reactiveValues(evidence = list(),values = list(),evidenceObserve = list(),valueObserve = list())
  insertedV <- c()
  inserted <- c()
  rvs$evidence <- c()
  rvs$value <- c()
  rvs$evidenceObserve <- c()
  rvs$valueObserve <- c()
  nodeNames <- c()
  EventNode <- c()
  EvidenceNode <- c()
  shapeVector<- c()
  weight<-1
  value<-1
  communities<-NULL
  graph<-NULL
  blacklistEdges<-c()
  whitelistEdges<-c()
  bn.start<- empty.graph(names(DiscreteData))
  NetworkGraph <- NULL
  assocNetwork<-NULL
  predError<-NULL
  updateSelectInput(session,"freqSelect",choices = names(DiscreteData))
  updateSelectInput(session,"delSelect",choices = names(DiscreteData))
  updateSelectInput(session,'event',choices = "")
  updateSelectizeInput(session,'varselect',choices = "")
  updateSelectizeInput(session,'Avarselect',choices = "")
  updateSelectInput(session,'paramSelect',choices = "")
  updateSelectInput(session,"tableName",choices = c("Association Graph","Bayesian Graph","Cross Validation Results","blacklist edges","whitelist edges"))
  updateSelectInput(session,'varshape',choices = c( "dot","square", "triangle", "box", "circle", "star","ellipse", "database", "text", "diamond"))
  updateSelectInput(session,'varshape2',choices = c( "dot","square", "triangle", "box", "circle", "star","ellipse", "database", "text", "diamond"))
  updateSelectInput(session,'varshape3',choices = c( "dot","square", "triangle", "box", "circle", "star","ellipse", "database", "text", "diamond"))
  updateSelectInput(session,'modGroup',choices = "")
  updateSelectInput(session,'Avarshape',choices = c( "dot","square", "triangle", "box", "circle", "star","ellipse", "database", "text", "diamond"))
  updateSelectInput(session,'Avarshape2',choices = c( "dot","square", "triangle", "box", "circle", "star","ellipse", "database", "text", "diamond"))
  updateSelectInput(session,'graph_layout',choices = c("layout_nicely","layout_as_star","layout_as_tree","layout_in_circle","layout_with_sugiyama","layout_on_sphere","layout_randomly","layout_with_fr","layout_with_kk","layout_with_lgl","layout_with_mds","layout_on_grid","layout_with_graphopt","layout_with_gem","layout_with_dh"))
  updateSelectInput(session,'Agraph_layout',choices = c("layout_nicely","layout_as_star","layout_as_tree","layout_in_circle","layout_with_sugiyama","layout_on_sphere","layout_randomly","layout_with_fr","layout_with_kk","layout_with_lgl","layout_with_mds","layout_on_grid","layout_with_graphopt","layout_with_gem","layout_with_dh"))
  updateSelectInput(session,"moduleSelection",choices = "")
  updateSelectInput(session,"AmoduleSelection",choices = "")
  updateSelectInput(session,"neighbornodes",choices = "")
  updateSelectInput(session,"Aneighbornodes",choices = "")
  updateSliderInput(session,"NumBar",min = 1, max = 2,value = 1)
  updateSelectInput(session,"fromarc",choices=c())
  updateSelectInput(session,"toarc",choices = c())
  updateSelectInput(session,"fromarc1",choices = names(DiscreteData))
  output$valLoss<-renderText({0})
  output$netScore<-renderText({0})
  output$assocPlot<-renderVisNetwork({validate("Please build an association plot on the data")})
  output$netPlot<-renderVisNetwork({validate("Please do structure learning on the data")})
  output$parameterPlot<-renderPlot({validate("Please do structure learning on the data")})
  output$distPlot<-renderPlot({validate("Please do structure learning on the data and then derive inferences")})
  output$freqPlot<-renderPlot({validate("Please preprocess data to build plot")})
  output$datasetTable<-DT::renderDataTable({DiscreteData},options = list(scrollX = TRUE,pageLength = 10),selection = list(target = 'column'))
  output$priorout<-DT::renderDataTable({bn.start$arcs},options = list(scrollX = TRUE,pageLength = 10),selection = 'single')
  output$postout<-DT::renderDataTable({NULL},options = list(scrollX = TRUE,pageLength = 10),selection = 'single')
  tooltip(session)
  })
  #observe events
  observeEvent(input$start,{
    updateTabItems(session, "sidebarMenu", "Structure")
    tooltip(session)
  })
  observeEvent(input$tableName,{
    tryCatch({
      if(input$tableName == "Association Graph")
      {
        output$tableOut<- DT::renderDataTable({assocNetwork},options = list(scrollX = TRUE,pageLength = 10))
      }
      else if(input$tableName=="Bayesian Graph")
      {
        output$tableOut<- DT::renderDataTable({NetworkGraph},options = list(scrollX = TRUE,pageLength = 10))
      }
      else if(input$tableName=="Cross Validation Results")
      {
        output$tableOut<- DT::renderDataTable({predError},options = list(scrollX = TRUE,pageLength = 10))
      }
      else if(input$tableName=="blacklist edges")
      {
        output$tableOut<- DT::renderDataTable({blacklistEdges},options = list(scrollX = TRUE,pageLength = 10))
      }
      else if(input$tableName=="whitelist edges")
      {
        output$tableOut<- DT::renderDataTable({whitelistEdges},options = list(scrollX = TRUE,pageLength = 10))
      }
    },error=function(e){
      shinyalert(toString(e), type = "error")
    })
    tooltip(session)
  })
  observeEvent(input$listFile,{
    tryCatch({
      file=input$listFile
      if(input$listType=="Blacklist")
      {
        blacklistEdges=read.csv(file$datapath,stringsAsFactors = T,na.strings = c("NA","na","Na","nA","","?","-"))
        if(dim(blacklistEdges)[2]!=2)
        {
          blacklistEdges<<-c()
          shinyalert("Please upload a .csv file containg edges in format 'from' and 'to'",type="error")
        }
        else if(!(unique(blacklistEdges[,1],blacklistEdges[,2]) %in% colnames(DiscreteData)))
        {
          blacklistEdges<<-c()
          shinyalert("please upload a correct file containg only nodes as observed in the data",type="error")
        }
      }
      else
      {
        whitelistEdges=read.csv(file$datapath,stringsAsFactors = T,na.strings = c("NA","na","Na","nA","","?","-"))
        if(dim(whitelistEdges)[2]!=2)
        {
          whitelistEdges<<-c()
          shinyalert("Please upload a .csv file containg edges in format 'from' and 'to'",type="error")
        }
        else if(!(unique(blacklistEdges[,1],blacklistEdges[,2]) %in% colnames(DiscreteData)))
        {
          whitelistEdges<<-c()
          shinyalert("please upload a correct file containg only nodes as observed in the data",type="error")
        }
      }
    },error=function(e){
      shinyalert(toString(e), type = "error")
    })
    tooltip(session)
  })
  observeEvent(input$priorFile,{
    file = input$priorFile
    tryCatch({
      bn.start <<- get(load(file))
      output$priorout<-DT::renderDataTable({bn.start$arcs},options = list(scrollX = TRUE,pageLength = 10),selection = 'single')
      shinyalert("File Successfully Uploaded",type="success")
    },error = function(e){
      bn.start<<- readRDS(file)
      output$priorout<-DT::renderDataTable({bn.start$arcs},options = list(scrollX = TRUE,pageLength = 10),selection = 'single')
    })
    tooltip(session)
  })
  observeEvent(input$fromarc1,{
    tryCatch({
      updateSelectInput(session,"toarc1",choices = setdiff(names(DiscreteData),input$fromarc1))
    },error=function(e){
      shinyalert(toString(e), type = "error")
    })
    tooltip(session)
  })
  observeEvent(input$addarc1,{
    tryCatch({
      bn.start<<-set.arc(bn.start,input$fromarc1,input$toarc1)
      output$priorout<-DT::renderDataTable({bn.start$arcs},options = list(scrollX = TRUE,pageLength = 10),selection = 'single')
      shinyalert("Arc successfully added",type="success")
    },error=function(e){
      shinyalert(toString(e), type = "error")
    })
    tooltip(session)
  })
  observeEvent(input$RemoveArc,{
    tryCatch({
      if(!is.null(input$priorout_rows_selected))
      {
        bn.start<<-drop.arc(bn.start,bn.start$arcs[input$priorout_rows_selected,1],bn.start$arcs[input$priorout_rows_selected,2])
        output$priorout<-DT::renderDataTable({bn.start$arcs},options = list(scrollX = TRUE,pageLength = 10),selection = 'single')
        shinyalert("Arc successfully removed",type="success")
      }
    },error=function(e){
      shinyalert(toString(e), type = "error")
    })
    tooltip(session)
  })
  observeEvent(input$ReverseArc,{
    tryCatch({
      if(!is.null(input$priorout_rows_selected))
      {
        bn.start<<-reverse.arc(bn.start,bn.start$arcs[input$priorout_rows_selected,1],bn.start$arcs[input$priorout_rows_selected,2])
        output$priorout<-DT::renderDataTable({bn.start$arcs},options = list(scrollX = TRUE,pageLength = 10),selection = 'single')
        shinyalert("Arc Successfully reversed",type="success")
      }
    },error=function(e){
      shinyalert(toString(e), type = "error")
    })
    tooltip(session)
  })
  observeEvent(input$threshold,{
    tryCatch({
      if(assocReset==2)
      {
        if(check.NA(DiscreteData))
        {
          shinyalert("Please impute missingness in the data first",type="info")
        }
        else if(check.discrete(DiscreteData))
        {
          shinyalert("Please discritize the data first",type="info")
        }
        else
        {
          assocNetworkprune<<- assocNetwork[which(assocNetwork[,3]>input$threshold),]
          shapeVectorAssoc<<- rep('dot',length(unique(c(assocNetworkprune[,1],assocNetworkprune[,2]))))
          updateSelectizeInput(session,'Avarselect',choices = unique(c(assocNetworkprune[,1],assocNetworkprune[,2])))
          output$assocPlot<-renderVisNetwork({graph.custom.assoc(assocNetworkprune,unique(c(assocNetworkprune[,1],assocNetworkprune[,2])),input$Adegree,input$Agraph_layout,shapeVectorAssoc)})
          Agraph<<-graph_from_edgelist(as.matrix(assocNetworkprune[,1:2]),directed = F)
          updateSelectInput(session,"Aneighbornodes",choices = "")
        }
      }
    },error=function(e){
      shinyalert(toString(e), type = "error")
    })
    tooltip(session)
  })
  observeEvent(input$association,{
    withProgress(message = "Building Association Graph", value = 0, {
      tryCatch({
        if(check.NA(DiscreteData))
        {
          shinyalert("Please impute missingness in the data first",type="info")
        }
        else if(check.discrete(DiscreteData))
        {
          shinyalert("Please discritize the data first",type="info")
        }
        else
        {
          assocNetwork<<-custom.association(DiscreteData,input$assocType)
          assocNetworkprune<<- assocNetwork[which(assocNetwork[,3]>input$threshold),]
          shapeVectorAssoc<<- rep('dot',length(unique(c(assocNetworkprune[,1],assocNetworkprune[,2]))))
          updateSelectizeInput(session,'Avarselect',choices = unique(c(assocNetworkprune[,1],assocNetworkprune[,2])))
          output$assocPlot<-renderVisNetwork({graph.custom.assoc(assocNetworkprune,unique(c(assocNetworkprune[,1],assocNetworkprune[,2])),input$Adegree,input$Agraph_layout,shapeVectorAssoc)})
          assocReset<<-2
          updateSelectInput(session,"Aneighbornodes",choices = "")
          Agraph<<-graph_from_edgelist(as.matrix(assocNetworkprune[,1:2]),directed = F)
          shinyalert("association graph Successfully buit",type="success")
        }
      },error=function(e){
        shinyalert(toString(e), type = "error")
      })
    })
    tooltip(session)
  })
  observeEvent(input$calLoss,{
    tryCatch({
      if(reset==2)
      {
        withProgress(message = "Validating Model", value = 0, {
          if(input$parallel==T)
          {
            bn.validate<<-bn.cv(DiscreteData[,nodeNames],bn=bn.hc.boot.average,fit = input$paramMethod3,method = input$crossFunc,cluster = cl)
            predError<<-c()
            for(n in nodeNames)
            {
              targetLoss<<-bn.cv(DiscreteData[,nodeNames],bn=bn.hc.boot.average,fit = input$paramMethod3,loss = input$lossFunc,method = input$crossFunc,loss.args = list(target = n),cluster = cl)
              predError<<-rbind(predError,targetLoss[[1]]$loss)
            }
            rownames(predError)<<-nodeNames
            colnames(predError)<<-"Classification Error"
            output$valLoss<<-renderText({bn.validate[[1]]$loss})
          }
          else
          {
            bn.validate<<-bn.cv(DiscreteData[,nodeNames],bn=bn.hc.boot.average,fit = input$paramMethod3,method = input$crossFunc)
            predError<<-c()
            for(n in nodeNames)
            {
              targetLoss<<-bn.cv(DiscreteData[,nodeNames],bn=bn.hc.boot.average,fit = input$paramMethod3,loss = input$lossFunc,method = input$crossFunc,loss.args = list(target = n))
              predError<<-rbind(predError,targetLoss[[1]]$loss)
            }
            rownames(predError)<<-nodeNames
            colnames(predError)<<-"Classification Error"
            output$valLoss<<-renderText({bn.validate[[1]]$loss})
          }
        })
      }
      else
      {
        shinyalert("Please learn network structure first", type = "info")
      }
    },error=function(e){
      shinyalert(toString(e), type = "error")
    })
    tooltip(session)
  })
  observeEvent(input$getScore,{
    tryCatch({
      if(reset==2)
      {
        scoreVal<<- score(bn.hc.boot.average,DiscreteData,type=input$scoreAlgo)
        output$netScore<<-renderText({scoreVal})
      }
      else
      {
        shinyalert("Please learn network structure first", type = "info")
      }
    },error=function(e){
      shinyalert(toString(e), type = "error")
    })
    tooltip(session)
  })
  output$downloadDataset<-downloadHandler(
    filename = function(){
      paste('dataset',".csv",sep = "")
    },
    content = function(filename){
      write.csv(DiscreteData,file=filename,row.names = F)
    }
  )
  output$downloadData <- downloadHandler(
    filename = function() {
      paste(input$tableName, ".csv", sep = "")
    },
    content = function(file) {
      if(input$tableName == "Association Graph")
      {
        write.csv(assocNetwork, file,row.names = F)
      }
      else if(input$tableName=="Bayesian Graph")
      {
        write.csv(NetworkGraph, file,row.names = F)
      }
      else if(input$tableName=="Cross Validation Results")
      {
        write.csv(predError, file,row.names=F)
      }
      else if(input$tableName=="blacklist edges")
      {
        write.csv(blacklistC, file,row.names = F)
      }
      else if(input$tableName=="whitelist edges")
      {
        write.csv(whitelistC, file,row.names=F)
      }
    }
  )
  output$saveBtn<-downloadHandler(
    filename = function() {
      paste('structure', ".RData", sep = "")
    },
    content = function(filename) {
      tryCatch({
        if(reset==2)
        {
          save(bn.hc.boot.average,file=filename)
        }
        else
        {
          shinyalert("Please learn network structure first",type="info")
        }
      })
    }
  )
  #Data Frame From User
  observeEvent(input$dataFile,{
    inFile <- input$dataFile
    if (is.null(inFile))
    {
      shinyalert("Data file is empty, pls upload a valid datafile",type = "error")
    }
    else
    {
      tryCatch({
        if(input$format==".RData")
        {
          if(file_ext(inFile$datapath) == "RData")
          {
            tryCatch({
              DiscreteData <<- get(load(inFile$datapath))
              },error = function(e){
                DiscreteData<<- readRDS(inFile$datapath)
              })
          }
          else
          {
            shinyalert("Added file is not a .RData file.Please upload a RData file.", type = "error")
          }

        }
        else
        {
          if(file_ext(inFile$datapath) == "csv")
          {
            DiscreteData <<- read.csv(inFile$datapath,stringsAsFactors = T,na.strings = c("NA","na","Na","nA","","?","-"))
          }
          else
          {
            shinyalert("Added file is not a .csv file.Please upload a CSV file.", type = "error")
          }
        }
        check.discrete(DiscreteData)
        check.NA(DiscreteData)
        DiscreteData<<-as.data.frame(DiscreteData)
        trueData<<-DiscreteData
        #Reset APP
        reset<<-1
        assocReset<<-1
        blacklistEdges<<-c()
        whitelistEdges<<-c()
        output$valLoss<<-renderText({0})
        output$netScore<<-renderText({0})
        output$assocPlot<<-renderVisNetwork({validate("Please build an association plot on the data")})
        output$netPlot<<-renderVisNetwork({validate("Please do structure learning on the data")})
        output$parameterPlot<<-renderPlot({validate("Please do structure learning on the data")})
        output$distPlot<<-renderPlot({validate("Please do structure learning on the data and then derive inferences")})
        output$datasetTable<-DT::renderDataTable({DiscreteData},options = list(scrollX = TRUE,pageLength = 10),selection = list(target = 'column'))
        NetworkGraph <<- NULL
        assocNetwork<<-NULL
        predError<<-NULL
        for(elem in 1:length(inserted))
        {
          removeUI(
            ## pass in appropriate div id
            selector = paste0('#', inserted[elem])
          )

        }
        inserted <<- c()
        for(elem2 in 1:length(insertedV))
        {
          removeUI(
            ## pass in appropriate div id
            selector = paste0('#', insertedV[elem2])
          )

        }
        insertedV <<- c()
        rvs$evidence <<- c()
        rvs$value <<- c()
        rvs$evidenceObserve <<- c()
        rvs$valueObserve <<- c()
        nodeNames <<- c()
        EventNode <<- c()
        EvidenceNode <<- c()
        shapeVector<<- c()
        weight<<-1
        value<<-1
        bn.start<<- empty.graph(names(DiscreteData))
        communities<<-NULL
        graph<<-NULL
        updateSelectInput(session,'event',choices = "")
        updateSelectizeInput(session,'varselect',choices = "")
        updateSelectInput(session,'paramSelect',choices = "")
        updateSelectInput(session,"moduleSelection",choices = "")
        updateSelectInput(session,"neighbornodes",choices = "")
        updateSelectInput(session,"Aneighbornodes",choices = "")
        updateSliderInput(session,"NumBar",min = 1, max = 2,value = 1)
        updateSelectInput(session,"freqSelect",choices = names(DiscreteData))
        updateSelectInput(session,"delSelect",choices = names(DiscreteData))
        updateSelectInput(session,"fromarc",choices=c())
        updateSelectInput(session,"toarc",choices = c())
        updateSelectInput(session,"fromarc1",choices = names(DiscreteData))
        updateSelectInput(session,'varshape3',choices = c( "dot","square", "triangle", "box", "circle", "star","ellipse", "database", "text", "diamond"))
        updateSelectInput(session,'modGroup',choices = "")
        output$postout<-DT::renderDataTable({NULL},options = list(scrollX = TRUE,pageLength = 10),selection = 'single')
        },error = function(e){
             shinyalert(c("Error in loading data: ",toString(e)), type = "error")
           })
    }
    tooltip(session)
    })
  observeEvent(input$discretize,{
    tryCatch({
      if(check.NA(DiscreteData))
      {
        shinyalert("Data has missing values, please impute the missing data first",type="info")
      }
      else
      {
        withProgress(message = "Discretizing data", value = 0, {
          tempDiscreteData <- DiscreteData
          for(n in colnames(tempDiscreteData))
          {
            if(is.numeric(tempDiscreteData[,n])|| is.integer(tempDiscreteData[,n]))
            {
              temp = custom.discretize(as.numeric(tempDiscreteData[,n]),input$dtype)
              tempDiscreteData[,n]<-temp
            }
          }
          tempDiscreteData[,which(lapply(tempDiscreteData,nlevels)<2)] = NULL
          tempDiscreteData <- droplevels(tempDiscreteData)
          DiscreteData <<-tempDiscreteData
          check.NA(DiscreteData)
          check.discrete(DiscreteData)
          trueData<<-DiscreteData
          output$datasetTable<-DT::renderDataTable({DiscreteData},options = list(scrollX = TRUE,pageLength = 10),selection = list(target = 'column'))
          reset<<-1
          assocReset<<-1
          weight<<-1
          value<<-1
          blacklistEdges<<-c()
          whitelistEdges<<-c()
          output$valLoss<<-renderText({0})
          output$netScore<<-renderText({0})
          output$assocPlot<<-renderVisNetwork({validate("Please build an association plot on the data")})
          output$netPlot<<-renderVisNetwork({validate("Please do structure learning on the data")})
          output$parameterPlot<<-renderPlot({validate("Please do structure learning on the data")})
          output$distPlot<<-renderPlot({validate("Please do structure learning on the data and then derive inferences")})
          output$datasetTable<-DT::renderDataTable({DiscreteData},options = list(scrollX = TRUE,pageLength = 10),selection = list(target = 'column'))
          NetworkGraph <<- NULL
          assocNetwork<<-NULL
          predError<<-NULL
          for(elem in 1:length(inserted))
          {
            removeUI(
              ## pass in appropriate div id
              selector = paste0('#', inserted[elem])
            )

          }
          inserted <<- c()
          for(elem2 in 1:length(insertedV))
          {
            removeUI(
              ## pass in appropriate div id
              selector = paste0('#', insertedV[elem2])
            )

          }
          insertedV <<- c()
          rvs$evidence <<- c()
          rvs$value <<- c()
          rvs$evidenceObserve <<- c()
          rvs$valueObserve <<- c()
          nodeNames <<- c()
          EventNode <<- c()
          EvidenceNode <<- c()
          shapeVector<<- c()
          bn.start<<- empty.graph(names(DiscreteData))
          communities<<-NULL
          graph<<-NULL
          updateSelectInput(session,'event',choices = "")
          updateSelectizeInput(session,'varselect',choices = "")
          updateSelectInput(session,'paramSelect',choices = "")
          updateSelectInput(session,"moduleSelection",choices = "")
          updateSelectInput(session,"neighbornodes",choices = "")
          updateSelectInput(session,"Aneighbornodes",choices = "")
          updateSliderInput(session,"NumBar",min = 1, max = 2,value = 1)
          updateSelectInput(session,"freqSelect",choices = names(DiscreteData))
          updateSelectInput(session,"delSelect",choices = names(DiscreteData))
          updateSelectInput(session,"fromarc",choices=c())
          updateSelectInput(session,"toarc",choices = c())
          updateSelectInput(session,"fromarc1",choices = names(DiscreteData))
          updateSelectInput(session,'modGroup',choices = "")
          output$postout<-DT::renderDataTable({NULL},options = list(scrollX = TRUE,pageLength = 10),selection = 'single')
        })
      }
      },error = function(e){
        type <- toString(input$dtype)
        messageString <- paste(c("Error is discretising using method ", type, ". Try using other method or upload pre-discretised data."), collapse = '')
        shinyalert(messageString, type = "error")
      })
    tooltip(session)
  })

  observeEvent(input$impute,{
    tryCatch({
      withProgress(message = "Imputing missing data", value = 0, {
      for(n in colnames(DiscreteData))
      {
        if(is.character(DiscreteData[,n]))
        {
          DiscreteData[,n]<<-as.factor(DiscreteData[,n])
        }
      }
      DiscreteData <<- missRanger(DiscreteData,maxiter = 1,num.tree = 100)
      output$datasetTable<-DT::renderDataTable({DiscreteData},options = list(scrollX = TRUE,pageLength = 10),selection = list(target = 'column'))
      trueData<<-DiscreteData
      reset<<-1
      assocReset<<-1
      weight<<-1
      value<<-1
      blacklistEdges<<-c()
      whitelistEdges<<-c()
      output$valLoss<<-renderText({0})
      output$netScore<<-renderText({0})
      output$assocPlot<<-renderVisNetwork({validate("Please build an association plot on the data")})
      output$netPlot<<-renderVisNetwork({validate("Please do structure learning on the data")})
      output$parameterPlot<<-renderPlot({validate("Please do structure learning on the data")})
      output$distPlot<<-renderPlot({validate("Please do structure learning on the data and then derive inferences")})
      output$datasetTable<-DT::renderDataTable({DiscreteData},options = list(scrollX = TRUE,pageLength = 10),selection = list(target = 'column'))
      NetworkGraph <<- NULL
      assocNetwork<<-NULL
      predError<<-NULL
      for(elem in 1:length(inserted))
      {
        removeUI(
          ## pass in appropriate div id
          selector = paste0('#', inserted[elem])
        )

      }
      inserted <<- c()
      for(elem2 in 1:length(insertedV))
      {
        removeUI(
          ## pass in appropriate div id
          selector = paste0('#', insertedV[elem2])
        )

      }
      insertedV <<- c()
      rvs$evidence <<- c()
      rvs$value <<- c()
      rvs$evidenceObserve <<- c()
      rvs$valueObserve <<- c()
      nodeNames <<- c()
      EventNode <<- c()
      EvidenceNode <<- c()
      shapeVector<<- c()
      bn.start<<- empty.graph(names(DiscreteData))
      communities<<-NULL
      graph<<-NULL
      updateSelectInput(session,'event',choices = "")
      updateSelectizeInput(session,'varselect',choices = "")
      updateSelectInput(session,'paramSelect',choices = "")
      updateSelectInput(session,"moduleSelection",choices = "")
      updateSelectInput(session,"neighbornodes",choices = "")
      updateSelectInput(session,"Aneighbornodes",choices = "")
      updateSliderInput(session,"NumBar",min = 1, max = 2,value = 1)
      updateSelectInput(session,"freqSelect",choices = names(DiscreteData))
      updateSelectInput(session,"delSelect",choices = names(DiscreteData))
      updateSelectInput(session,"fromarc",choices=c())
      updateSelectInput(session,"toarc",choices = c())
      updateSelectInput(session,"fromarc1",choices = names(DiscreteData))
      updateSelectInput(session,'modGroup',choices = "")
      output$postout<-DT::renderDataTable({NULL},options = list(scrollX = TRUE,pageLength = 10),selection = 'single')
    })}, error = function(e){
      type <- toString(input$dtype)
      messageString <- "Error imputing missingness using missRanger method. Try uploading pre-imputed data."
      shinyalert(messageString, type = "error")
    })
    tooltip(session)
  })
  observeEvent(input$delete,{
    tryCatch({
      DiscreteData[,input$delSelect]=NULL
      DiscreteData<<-DiscreteData
      updateSelectInput(session,"delSelect",choices = names(DiscreteData))
      updateSelectInput(session,"freqSelect",choices = names(DiscreteData))
      output$datasetTable<-DT::renderDataTable({DiscreteData},options = list(scrollX = TRUE,pageLength = 10),selection = list(target = 'column'))
      reset<<-1
      weight<<-1
      value<<-1
      assocReset<<-1
      blacklistEdges<<-c()
      whitelistEdges<<-c()
      output$valLoss<<-renderText({0})
      output$netScore<<-renderText({0})
      output$assocPlot<<-renderVisNetwork({validate("Please build an association plot on the data")})
      output$netPlot<<-renderVisNetwork({validate("Please do structure learning on the data")})
      output$parameterPlot<<-renderPlot({validate("Please do structure learning on the data")})
      output$distPlot<<-renderPlot({validate("Please do structure learning on the data and then derive inferences")})
      output$datasetTable<-DT::renderDataTable({DiscreteData},options = list(scrollX = TRUE,pageLength = 10),selection = list(target = 'column'))
      NetworkGraph <<- NULL
      assocNetwork<<-NULL
      predError<<-NULL
      for(elem in 1:length(inserted))
      {
        removeUI(
          ## pass in appropriate div id
          selector = paste0('#', inserted[elem])
        )

      }
      inserted <<- c()
      for(elem2 in 1:length(insertedV))
      {
        removeUI(
          ## pass in appropriate div id
          selector = paste0('#', insertedV[elem2])
        )

      }
      insertedV <<- c()
      rvs$evidence <<- c()
      rvs$value <<- c()
      rvs$evidenceObserve <<- c()
      rvs$valueObserve <<- c()
      nodeNames <<- c()
      EventNode <<- c()
      EvidenceNode <<- c()
      shapeVector<<- c()
      bn.start<<- empty.graph(names(DiscreteData))
      communities<<-NULL
      graph<<-NULL
      updateSelectInput(session,'event',choices = "")
      updateSelectizeInput(session,'varselect',choices = "")
      updateSelectInput(session,'paramSelect',choices = "")
      updateSelectInput(session,"moduleSelection",choices = "")
      updateSelectInput(session,"neighbornodes",choices = "")
      updateSelectInput(session,"Aneighbornodes",choices = "")
      updateSliderInput(session,"NumBar",min = 1, max = 2,value = 1)
      updateSelectInput(session,"freqSelect",choices = names(DiscreteData))
      updateSelectInput(session,"delSelect",choices = names(DiscreteData))
      updateSelectInput(session,"fromarc",choices=c())
      updateSelectInput(session,"toarc",choices = c())
      updateSelectInput(session,"fromarc1",choices = names(DiscreteData))
      updateSelectInput(session,'modGroup',choices = "")
      output$postout<-DT::renderDataTable({NULL},options = list(scrollX = TRUE,pageLength = 10),selection = 'single')
      bn.start<<- empty.graph(names(DiscreteData))
      output$priorout<-DT::renderDataTable({bn.start$arcs},options = list(scrollX = TRUE,pageLength = 10),selection = 'single')
    },error=function(e){

    })
    tooltip(session)
  })
  observeEvent(input$sort,{
    DiscreteData<<-DiscreteData[,order(names(DiscreteData))]
    updateSelectInput(session,"delSelect",choices = names(DiscreteData))
    updateSelectInput(session,"freqSelect",choices = names(DiscreteData))
    output$datasetTable<-DT::renderDataTable({DiscreteData},options = list(scrollX = TRUE,pageLength = 10),selection = list(target = 'column'))
    reset<<-1
    assocReset<<-1
    weight<<-1
    value<<-1
    blacklistEdges<<-c()
    whitelistEdges<<-c()
    output$valLoss<<-renderText({0})
    output$netScore<<-renderText({0})
    output$assocPlot<<-renderVisNetwork({validate("Please build an association plot on the data")})
    output$netPlot<<-renderVisNetwork({validate("Please do structure learning on the data")})
    output$parameterPlot<<-renderPlot({validate("Please do structure learning on the data")})
    output$distPlot<<-renderPlot({validate("Please do structure learning on the data and then derive inferences")})
    output$datasetTable<-DT::renderDataTable({DiscreteData},options = list(scrollX = TRUE,pageLength = 10),selection = list(target = 'column'))
    NetworkGraph <<- NULL
    assocNetwork<<-NULL
    predError<<-NULL
    for(elem in 1:length(inserted))
    {
      removeUI(
        ## pass in appropriate div id
        selector = paste0('#', inserted[elem])
      )

    }
    inserted <<- c()
    for(elem2 in 1:length(insertedV))
    {
      removeUI(
        ## pass in appropriate div id
        selector = paste0('#', insertedV[elem2])
      )

    }
    insertedV <<- c()
    rvs$evidence <<- c()
    rvs$value <<- c()
    rvs$evidenceObserve <<- c()
    rvs$valueObserve <<- c()
    nodeNames <<- c()
    EventNode <<- c()
    EvidenceNode <<- c()
    shapeVector<<- c()
    bn.start<<- empty.graph(names(DiscreteData))
    communities<<-NULL
    graph<<-NULL
    updateSelectInput(session,'event',choices = "")
    updateSelectizeInput(session,'varselect',choices = "")
    updateSelectInput(session,'paramSelect',choices = "")
    updateSelectInput(session,"moduleSelection",choices = "")
    updateSelectInput(session,"neighbornodes",choices = "")
    updateSelectInput(session,"Aneighbornodes",choices = "")
    updateSliderInput(session,"NumBar",min = 1, max = 2,value = 1)
    updateSelectInput(session,"freqSelect",choices = names(DiscreteData))
    updateSelectInput(session,"delSelect",choices = names(DiscreteData))
    updateSelectInput(session,"fromarc",choices=c())
    updateSelectInput(session,"toarc",choices = c())
    updateSelectInput(session,"fromarc1",choices = names(DiscreteData))
    updateSelectInput(session,'modGroup',choices = "")
    output$postout<-DT::renderDataTable({NULL},options = list(scrollX = TRUE,pageLength = 10),selection = 'single')
    bn.start<<- empty.graph(names(DiscreteData))
    output$priorout<-DT::renderDataTable({bn.start$arcs},options = list(scrollX = TRUE,pageLength = 10),selection = 'single')
    tooltip(session)
  })
  observeEvent(input$reset,{
    tryCatch({
      DiscreteData<<-trueData
      updateSelectInput(session,"delSelect",choices = names(DiscreteData))
      updateSelectInput(session,"freqSelect",choices = names(DiscreteData))
      output$datasetTable<-DT::renderDataTable({DiscreteData},options = list(scrollX = TRUE,pageLength = 10),selection = list(target = 'column'))
      reset<<-1
      assocReset<<-1
      weight<<-1
      value<<-1
      blacklistEdges<<-c()
      whitelistEdges<<-c()
      output$valLoss<<-renderText({0})
      output$netScore<<-renderText({0})
      output$assocPlot<<-renderVisNetwork({validate("Please build an association plot on the data")})
      output$netPlot<<-renderVisNetwork({validate("Please do structure learning on the data")})
      output$parameterPlot<<-renderPlot({validate("Please do structure learning on the data")})
      output$distPlot<<-renderPlot({validate("Please do structure learning on the data and then derive inferences")})
      output$datasetTable<-DT::renderDataTable({DiscreteData},options = list(scrollX = TRUE,pageLength = 10),selection = list(target = 'column'))
      NetworkGraph <<- NULL
      assocNetwork<<-NULL
      predError<<-NULL
      for(elem in 1:length(inserted))
      {
        removeUI(
          ## pass in appropriate div id
          selector = paste0('#', inserted[elem])
        )

      }
      inserted <<- c()
      for(elem2 in 1:length(insertedV))
      {
        removeUI(
          ## pass in appropriate div id
          selector = paste0('#', insertedV[elem2])
        )

      }
      insertedV <<- c()
      rvs$evidence <<- c()
      rvs$value <<- c()
      rvs$evidenceObserve <<- c()
      rvs$valueObserve <<- c()
      nodeNames <<- c()
      EventNode <<- c()
      EvidenceNode <<- c()
      shapeVector<<- c()
      bn.start<<- empty.graph(names(DiscreteData))
      communities<<-NULL
      graph<<-NULL
      updateSelectInput(session,'event',choices = "")
      updateSelectizeInput(session,'varselect',choices = "")
      updateSelectInput(session,'paramSelect',choices = "")
      updateSelectInput(session,"moduleSelection",choices = "")
      updateSelectInput(session,"neighbornodes",choices = "")
      updateSelectInput(session,"Aneighbornodes",choices = "")
      updateSliderInput(session,"NumBar",min = 1, max = 2,value = 1)
      updateSelectInput(session,"freqSelect",choices = names(DiscreteData))
      updateSelectInput(session,"delSelect",choices = names(DiscreteData))
      updateSelectInput(session,"fromarc",choices=c())
      updateSelectInput(session,"toarc",choices = c())
      updateSelectInput(session,"fromarc1",choices = names(DiscreteData))
      updateSelectInput(session,'modGroup',choices = "")
      output$postout<-DT::renderDataTable({NULL},options = list(scrollX = TRUE,pageLength = 10),selection = 'single')
      bn.start<<- empty.graph(names(DiscreteData))
      output$priorout<-DT::renderDataTable({bn.start$arcs},options = list(scrollX = TRUE,pageLength = 10),selection = 'single')
    },error=function(e){
      shinyalert(toString(e), type = "error")
    })
    tooltip(session)
  })
  observeEvent(input$transpose,{
    tryCatch({
      if(dim(DiscreteData)[1]>dim(DiscreteData)[2])
      {
        shinyalert("Transpose is only possible for datasest with #variables more than #samples",type="info")
      }
      else
      {
        DiscreteData<<-t(DiscreteData)
        updateSelectInput(session,"delSelect",choices = names(DiscreteData))
        updateSelectInput(session,"freqSelect",choices = names(DiscreteData))
        output$datasetTable<-DT::renderDataTable({DiscreteData},options = list(scrollX = TRUE,pageLength = 10),selection = list(target = 'column'))
        reset<<-1
        weight<<-1
        value<<-1
        assocReset<<-1
        blacklistEdges<<-c()
        whitelistEdges<<-c()
        output$valLoss<<-renderText({0})
        output$netScore<<-renderText({0})
        output$assocPlot<<-renderVisNetwork({validate("Please build an association plot on the data")})
        output$netPlot<<-renderVisNetwork({validate("Please do structure learning on the data")})
        output$parameterPlot<<-renderPlot({validate("Please do structure learning on the data")})
        output$distPlot<<-renderPlot({validate("Please do structure learning on the data and then derive inferences")})
        output$datasetTable<-DT::renderDataTable({DiscreteData},options = list(scrollX = TRUE,pageLength = 10),selection = list(target = 'column'))
        NetworkGraph <<- NULL
        assocNetwork<<-NULL
        predError<<-NULL
        for(elem in 1:length(inserted))
        {
          removeUI(
            ## pass in appropriate div id
            selector = paste0('#', inserted[elem])
          )

        }
        inserted <<- c()
        for(elem2 in 1:length(insertedV))
        {
          removeUI(
            ## pass in appropriate div id
            selector = paste0('#', insertedV[elem2])
          )

        }
        insertedV <<- c()
        rvs$evidence <<- c()
        rvs$value <<- c()
        rvs$evidenceObserve <<- c()
        rvs$valueObserve <<- c()
        nodeNames <<- c()
        EventNode <<- c()
        EvidenceNode <<- c()
        shapeVector<<- c()
        bn.start<<- empty.graph(names(DiscreteData))
        communities<<-NULL
        graph<<-NULL
        updateSelectInput(session,'event',choices = "")
        updateSelectizeInput(session,'varselect',choices = "")
        updateSelectInput(session,'paramSelect',choices = "")
        updateSelectInput(session,"moduleSelection",choices = "")
        updateSelectInput(session,"neighbornodes",choices = "")
        updateSelectInput(session,"Aneighbornodes",choices = "")
        updateSliderInput(session,"NumBar",min = 1, max = 2,value = 1)
        updateSelectInput(session,"freqSelect",choices = names(DiscreteData))
        updateSelectInput(session,"delSelect",choices = names(DiscreteData))
        updateSelectInput(session,"fromarc",choices=c())
        updateSelectInput(session,"toarc",choices = c())
        updateSelectInput(session,"fromarc1",choices = names(DiscreteData))
        updateSelectInput(session,'modGroup',choices = "")
        output$postout<-DT::renderDataTable({NULL},options = list(scrollX = TRUE,pageLength = 10),selection = 'single')
        bn.start<<- empty.graph(names(DiscreteData))
        output$priorout<-DT::renderDataTable({bn.start$arcs},options = list(scrollX = TRUE,pageLength = 10),selection = 'single')
      }
    },error=function(e){
      shinyalert(toString(e),type = 'error')
    })
    tooltip(session)
  })
  observeEvent(input$freqSelect,{
    if((check.discrete(DiscreteData)||check.NA(DiscreteData)))
    {
      output$freqPlot<<-renderPlot({validate("Please make sure data is complete and discretized before using the feature")})
    }
    else
    {
      tryCatch({
        val = table(DiscreteData[,input$freqSelect])/nrow(DiscreteData)
        output$freqPlot = renderPlot({par(mar=c(5,3,3,3))
          par(oma=c(5,3,3,3))
          barx <<-barplot(val,
                          col = "lightblue",
                          main = paste("Background frequency of ",input$freqSelect),
                          border = NA,
                          xlab = "",
                          ylab = "Frequency",
                          ylim = c(0,1),
                          las=2)
          text(x = barx,y = round(val,digits = 4),label = round(val,digits = 4), pos = 3, cex = 0.8, col = "black")})
      },error=function(e){
        if(input$freqSelect=="")
        {

        }
        else
        {
          shinyalert(toString(e),type="error")
        }
      })
    }
    tooltip(session)
  })
  # Get the data selection from user
  observeEvent(input$structFile,{# Get the uploaded file from user
        if(check.NA(DiscreteData))
        {
          shinyalert("Please impute missingness in the data first",type="info")
        }
        else if(check.discrete(DiscreteData))
        {
          shinyalert("Please discritize the data first",type="info")
        }
       else
       {
         inFile <- input$structFile
         if (is.null(inFile))
         {
           shinyalert("Structure File is empty",type='error')
         }
         else
         {
           if(is.null(DiscreteData))
           {
             shinyalert("Please Upload Data File First",type = 'error')
           }
           else
           {
             tryCatch({
               tryCatch({
                 bn.hc.boot.average <<- get(load(inFile$datapath))
               },error = function(e){
                 bn.hc.boot.average <<- readRDS(inFile$datapath)
               })
               if(input$parallel==T)
               {
                 bn.hc.boot.fit <<- bn.fit(bn.hc.boot.average,DiscreteData[,names(bn.hc.boot.average$nodes)],method = input$paramMethod,cluster = cl)
               }
               else
               {
                 bn.hc.boot.fit <<- bn.fit(bn.hc.boot.average,DiscreteData[,names(bn.hc.boot.average$nodes)],method = input$paramMethod)
               }
               shinyalert("Learned Structure loaded",type = "success")
               for(elem in 1:length(inserted))
               {
                 removeUI(
                   ## pass in appropriate div id
                   selector = paste0('#', inserted[elem])
                 )

               }
               inserted <<- c()
               for(elem2 in 1:length(insertedV))
               {
                 removeUI(
                   ## pass in appropriate div id
                   selector = paste0('#', insertedV[elem2])
                 )

               }
               insertedV <<- c()
               rvs$evidence <<- c()
               rvs$value <<- c()
               rvs$evidenceObserve <<- c()
               rvs$valueObserve <<- c()
               output$distPlot <<- renderPlot(NULL)
               NetworkGraph <<- data.frame(directed.arcs(bn.hc.boot.average))
               nodeNames <<- names(bn.hc.boot.average$nodes)
               EventNode <<- nodeNames[1]
               EvidenceNode <<- c()
               shapeVector<<- rep('dot',length(nodeNames))
               updateSelectInput(session,'event',choices = nodeNames)
               weight <<- 1
               value <<- 1
               output$netPlot<-renderVisNetwork({graph.custom(NetworkGraph,nodeNames,shapeVector,EvidenceNode,EventNode,input$degree,input$graph_layout,weight,value)})
               updateSelectizeInput(session,'varselect',choices = nodeNames)
               updateSelectInput(session,'varshape',choices = c( "dot","square", "triangle", "box", "circle", "star",
                                                                 "ellipse", "database", "text", "diamond"))
               updateSelectInput(session,'varshape2',choices = c( "dot","square", "triangle", "box", "circle", "star",
                                                                  "ellipse", "database", "text", "diamond"))
               updateSelectInput(session,'graph_layout',choices = c("layout_nicely","layout_as_star","layout_as_tree","layout_in_circle","layout_with_sugiyama","layout_on_sphere","layout_randomly","layout_with_fr","layout_with_kk","layout_with_lgl","layout_with_mds","layout_on_grid","layout_with_graphopt","layout_with_gem","layout_with_dh"))
               updateSelectInput(session,'paramSelect',choices = nodeNames)
               graph<<-graph_from_edgelist(as.matrix(NetworkGraph),directed = TRUE)
               updateSelectInput(session,"neighbornodes",choices = "")
               updateSelectInput(session,"fromarc",choices = nodeNames)
               updateSelectInput(session,'varshape3',choices = c( "dot","square", "triangle", "box", "circle", "star",
                                                                  "ellipse", "database", "text", "diamond"))
               updateSelectInput(session,'modGroup',choices = "")
               updateSliderInput(session,"NumBar",min = 1, max = nlevels(DiscreteData[,nodeNames[1]]),value = nlevels(DiscreteData[,nodeNames[1]]))
               output$postout<-DT::renderDataTable({bn.hc.boot.average$arcs},options = list(scrollX = TRUE,pageLength = 10),selection = 'single')
               reset<<-2
               upload<<-1
               uploadtype<<-1
               type<<-1
               save(DiscreteData,file="customDashboard/inst/cd/data.RData")
               save(bn.hc.boot.average,file="customDashboard/inst/cd/structure.RData")
               write.csv(input$name,file = "customDashboard/inst/cd/name.txt",row.names = FALSE)
             },error = function(e){
               shinyalert(toString(e), type = "error")
             })
           }
         }
       }
    tooltip(session)
    })

  observeEvent(input$bootFile,{
    if(check.NA(DiscreteData))
    {
      shinyalert("Please impute missingness in the data first",type="info")
    }
    else if(check.discrete(DiscreteData))
    {
      shinyalert("Please discritize the data first",type="info")
    }
    else
    {
      inFile <- input$bootFile
      if (is.null(inFile))
      {
        shinyalert("Structure File is empty",type='error')
      }
      else
      {
        if(is.null(DiscreteData))
        {
          shinyalert("Please Upload Data File First",type = 'error')
        }
        else
        {
          tryCatch({
            tryCatch({
              bn.hc.boot <<- get(load(inFile$datapath))
            },error = function(e){
              bn.hc.boot <<- readRDS(inFile$datapath)
            })
            bn.hc.boot.pruned <<- bn.hc.boot[bn.hc.boot$strength > input$edgeStrengthU & bn.hc.boot$direction > input$directionStrengthU,]
            bn.hc.boot.average <<- cextend(averaged.network(bn.hc.boot.pruned))
            if(input$parallel==T)
            {
              bn.hc.boot.fit <<- bn.fit(bn.hc.boot.average,DiscreteData[,names(bn.hc.boot.average$nodes)],method = input$paramMethod,cluster = cl)
            }
            else
            {
              bn.hc.boot.fit <<- bn.fit(bn.hc.boot.average,DiscreteData[,names(bn.hc.boot.average$nodes)],method = input$paramMethod)
            }
            shinyalert("Learned Structure loaded",type = "success")
            for(elem in 1:length(inserted))
            {
              removeUI(
                ## pass in appropriate div id
                selector = paste0('#', inserted[elem])
              )

            }
            inserted <<- c()
            for(elem2 in 1:length(insertedV))
            {
              removeUI(
                ## pass in appropriate div id
                selector = paste0('#', insertedV[elem2])
              )

            }
            insertedV <<- c()
            rvs$evidence <<- c()
            rvs$value <<- c()
            rvs$evidenceObserve <<- c()
            rvs$valueObserve <<- c()
            output$distPlot <<- renderPlot(NULL)
            NetworkGraph <<- data.frame(directed.arcs(bn.hc.boot.average))
            nodeNames <<- names(bn.hc.boot.average$nodes)
            EventNode <<- nodeNames[1]
            EvidenceNode <<- c()
            shapeVector<<- rep('dot',length(nodeNames))
            updateSelectInput(session,'event',choices = nodeNames)
            weight <<- bn.hc.boot.pruned[dim(NetworkGraph)[1],3]
            value <<- bn.hc.boot.pruned[dim(NetworkGraph)[1],3]
            output$netPlot<-renderVisNetwork({graph.custom(NetworkGraph,nodeNames,shapeVector,EvidenceNode,EventNode,input$degree,input$graph_layout,weight,value)})
            updateSelectizeInput(session,'varselect',choices = nodeNames)
            updateSelectInput(session,'varshape',choices = c( "dot","square", "triangle", "box", "circle", "star",
                                                              "ellipse", "database", "text", "diamond"))
            updateSelectInput(session,'varshape2',choices = c( "dot","square", "triangle", "box", "circle", "star",
                                                               "ellipse", "database", "text", "diamond"))
            updateSelectInput(session,'graph_layout',choices = c("layout_nicely","layout_as_star","layout_as_tree","layout_in_circle","layout_with_sugiyama","layout_on_sphere","layout_randomly","layout_with_fr","layout_with_kk","layout_with_lgl","layout_with_mds","layout_on_grid","layout_with_graphopt","layout_with_gem","layout_with_dh"))
            updateSelectInput(session,'paramSelect',choices = nodeNames)
            graph<<-graph_from_edgelist(as.matrix(NetworkGraph),directed = TRUE)
            updateSelectInput(session,"neighbornodes",choices = "")
            updateSelectInput(session,"fromarc",choices = nodeNames)
            updateSelectInput(session,'varshape3',choices = c( "dot","square", "triangle", "box", "circle", "star",
                                                               "ellipse", "database", "text", "diamond"))
            updateSelectInput(session,'modGroup',choices = "")
            updateSliderInput(session,"NumBar",min = 1, max = nlevels(DiscreteData[,nodeNames[1]]),value = nlevels(DiscreteData[,nodeNames[1]]))
            output$postout<-DT::renderDataTable({bn.hc.boot.average$arcs},options = list(scrollX = TRUE,pageLength = 10),selection = 'single')
            reset<<-2
            upload<<-1
            uploadtype<<-2
            type<<-2
            save(DiscreteData,file="customDashboard/inst/cd/data.RData")
            save(bn.hc.boot.average,file="customDashboard/inst/cd/structure.RData")
            write.csv(input$name,file = "customDashboard/inst/cd/name.txt",row.names = FALSE)
          },error = function(e){
            shinyalert(toString(e), type = "error")
          })
        }
      }
    }
    tooltip(session)
  })
  observeEvent(input$parameterTuningU,{
    if(upload==1)
    {
      if(reset==2)
      {
        if(uploadtype==1)
        {
          if(input$parallel==T)
          {
            bn.hc.boot.fit <<- bn.fit(bn.hc.boot.average,DiscreteData[,names(bn.hc.boot.average$nodes)],method = input$paramMethod,cluster = cl)
          }
          else
          {
            bn.hc.boot.fit <<- bn.fit(bn.hc.boot.average,DiscreteData[,names(bn.hc.boot.average$nodes)],method = input$paramMethod)
          }
          shinyalert("Learned Structure loaded",type = "success")
          for(elem in 1:length(inserted))
          {
            removeUI(
              ## pass in appropriate div id
              selector = paste0('#', inserted[elem])
            )

          }
          inserted <<- c()
          for(elem2 in 1:length(insertedV))
          {
            removeUI(
              ## pass in appropriate div id
              selector = paste0('#', insertedV[elem2])
            )

          }
          insertedV <<- c()
          rvs$evidence <<- c()
          rvs$value <<- c()
          rvs$evidenceObserve <<- c()
          rvs$valueObserve <<- c()
          output$distPlot <<- renderPlot(NULL)
          NetworkGraph <<- data.frame(directed.arcs(bn.hc.boot.average))
          nodeNames <<- names(bn.hc.boot.average$nodes)
          EventNode <<- nodeNames[1]
          EvidenceNode <<- c()
          shapeVector<<- rep('dot',length(nodeNames))
          updateSelectInput(session,'event',choices = nodeNames)
          weight <<- 1
          value <<- 1
          output$netPlot<-renderVisNetwork({graph.custom(NetworkGraph,nodeNames,shapeVector,EvidenceNode,EventNode,input$degree,input$graph_layout,weight,value)})
          updateSelectizeInput(session,'varselect',choices = nodeNames)
          updateSelectInput(session,'varshape',choices = c( "dot","square", "triangle", "box", "circle", "star",
                                                            "ellipse", "database", "text", "diamond"))
          updateSelectInput(session,'varshape2',choices = c( "dot","square", "triangle", "box", "circle", "star",
                                                             "ellipse", "database", "text", "diamond"))
          updateSelectInput(session,'graph_layout',choices = c("layout_nicely","layout_as_star","layout_as_tree","layout_in_circle","layout_with_sugiyama","layout_on_sphere","layout_randomly","layout_with_fr","layout_with_kk","layout_with_lgl","layout_with_mds","layout_on_grid","layout_with_graphopt","layout_with_gem","layout_with_dh"))
          updateSelectInput(session,'paramSelect',choices = nodeNames)
          graph<<-graph_from_edgelist(as.matrix(NetworkGraph),directed = TRUE)
          updateSelectInput(session,"neighbornodes",choices = "")
          updateSelectInput(session,"fromarc",choices = nodeNames)
          updateSelectInput(session,'varshape3',choices = c( "dot","square", "triangle", "box", "circle", "star",
                                                             "ellipse", "database", "text", "diamond"))
          updateSelectInput(session,'modGroup',choices = "")
          updateSliderInput(session,"NumBar",min = 1, max = nlevels(DiscreteData[,nodeNames[1]]),value = nlevels(DiscreteData[,nodeNames[1]]))
          output$postout<-DT::renderDataTable({bn.hc.boot.average$arcs},options = list(scrollX = TRUE,pageLength = 10),selection = 'single')
          reset<<-2
          upload<<-1
          uploadtype<<-1
          type<<-1
          save(DiscreteData,file="customDashboard/inst/cd/data.RData")
          save(bn.hc.boot.average,file="customDashboard/inst/cd/structure.RData")
          write.csv(input$name,file = "customDashboard/inst/cd/name.txt",row.names = FALSE)
        }
        else
        {
          bn.hc.boot.pruned <<- bn.hc.boot[bn.hc.boot$strength > input$edgeStrengthU & bn.hc.boot$direction > input$directionStrengthU,]
          bn.hc.boot.average <<- cextend(averaged.network(bn.hc.boot.pruned))
          if(input$parallel==T)
          {
            bn.hc.boot.fit <<- bn.fit(bn.hc.boot.average,DiscreteData[,names(bn.hc.boot.average$nodes)],method = input$paramMethod,cluster = cl)
          }
          else
          {
            bn.hc.boot.fit <<- bn.fit(bn.hc.boot.average,DiscreteData[,names(bn.hc.boot.average$nodes)],method = input$paramMethod)
          }
          shinyalert("Learned Structure loaded",type = "success")
          for(elem in 1:length(inserted))
          {
            removeUI(
              ## pass in appropriate div id
              selector = paste0('#', inserted[elem])
            )

          }
          inserted <<- c()
          for(elem2 in 1:length(insertedV))
          {
            removeUI(
              ## pass in appropriate div id
              selector = paste0('#', insertedV[elem2])
            )

          }
          insertedV <<- c()
          rvs$evidence <<- c()
          rvs$value <<- c()
          rvs$evidenceObserve <<- c()
          rvs$valueObserve <<- c()
          output$distPlot <<- renderPlot(NULL)
          NetworkGraph <<- data.frame(directed.arcs(bn.hc.boot.average))
          nodeNames <<- names(bn.hc.boot.average$nodes)
          EventNode <<- nodeNames[1]
          EvidenceNode <<- c()
          shapeVector<<- rep('dot',length(nodeNames))
          updateSelectInput(session,'event',choices = nodeNames)
          weight <<- bn.hc.boot.pruned[dim(NetworkGraph)[1],3]
          value <<- bn.hc.boot.pruned[dim(NetworkGraph)[1],3]
          output$netPlot<-renderVisNetwork({graph.custom(NetworkGraph,nodeNames,shapeVector,EvidenceNode,EventNode,input$degree,input$graph_layout,weight,value)})
          updateSelectizeInput(session,'varselect',choices = nodeNames)
          updateSelectInput(session,'varshape',choices = c( "dot","square", "triangle", "box", "circle", "star",
                                                            "ellipse", "database", "text", "diamond"))
          updateSelectInput(session,'varshape2',choices = c( "dot","square", "triangle", "box", "circle", "star",
                                                             "ellipse", "database", "text", "diamond"))
          updateSelectInput(session,'graph_layout',choices = c("layout_nicely","layout_as_star","layout_as_tree","layout_in_circle","layout_with_sugiyama","layout_on_sphere","layout_randomly","layout_with_fr","layout_with_kk","layout_with_lgl","layout_with_mds","layout_on_grid","layout_with_graphopt","layout_with_gem","layout_with_dh"))
          updateSelectInput(session,'paramSelect',choices = nodeNames)
          graph<<-graph_from_edgelist(as.matrix(NetworkGraph),directed = TRUE)
          updateSelectInput(session,"neighbornodes",choices = "")
          updateSelectInput(session,"fromarc",choices = nodeNames)
          updateSelectInput(session,'varshape3',choices = c( "dot","square", "triangle", "box", "circle", "star",
                                                             "ellipse", "database", "text", "diamond"))
          updateSelectInput(session,'modGroup',choices = "")
          updateSliderInput(session,"NumBar",min = 1, max = nlevels(DiscreteData[,nodeNames[1]]),value = nlevels(DiscreteData[,nodeNames[1]]))
          output$postout<-DT::renderDataTable({bn.hc.boot.average$arcs},options = list(scrollX = TRUE,pageLength = 10),selection = 'single')
          reset<<-2
          upload<<-1
          uploadtype<<-2
          type<<-2
          save(DiscreteData,file="customDashboard/inst/cd/data.RData")
          save(bn.hc.boot.average,file="customDashboard/inst/cd/structure.RData")
          write.csv(input$name,file = "customDashboard/inst/cd/name.txt",row.names = FALSE)
        }
      }
    }
    tooltip(session)
  })
  # Learn the structure of the network
  observeEvent(input$learnBtn, {
    if(check.NA(DiscreteData))
    {
      shinyalert("Please impute missingness in the data first",type="info")
    }
    else if(check.discrete(DiscreteData))
    {
      shinyalert("Please discritize the data first",type="info")
    }
    else
    {
      tryCatch({
        shinyalert("Structure Learning started",type="info")
        if (is.null(DiscreteData))
          return(NULL)

        # Create a Progress object
        progress <- shiny::Progress$new()

        # Make sure it closes when we exit this reactive, even if there's an error
        on.exit(progress$close())
        progress$set(message = "Learning network structure", value = 0)

        # Get the selected learning algorithm from the user and learn the network
        if(input$parallel==T)
        {
          bn.hc.boot <<- boot.strength(data = DiscreteData, R = input$boot, m = ceiling(nrow(DiscreteData)*input$SampleSize), algorithm = input$alg,algorithm.args=list(blacklist=blacklistEdges,whitelist=whitelistEdges,start=bn.start),cluster = cl)
          bn.hc.boot.pruned <<- bn.hc.boot[bn.hc.boot$strength > input$edgeStrength & bn.hc.boot$direction > input$directionStrength,]
          bn.hc.boot.average <<- cextend(averaged.network(bn.hc.boot.pruned))
          bn.hc.boot.fit <<- bn.fit(bn.hc.boot.average,DiscreteData[,names(bn.hc.boot.average$nodes)],method = input$paramMethod2,cluster = cl)
        }
        else
        {
          bn.hc.boot <<- boot.strength(data = DiscreteData, R = input$boot, m = ceiling(nrow(DiscreteData)*input$SampleSize), algorithm = input$alg,algorithm.args=list(blacklist=blacklistEdges,whitelist=whitelistEdges,start=bn.start))
          bn.hc.boot.pruned <<- bn.hc.boot[bn.hc.boot$strength > input$edgeStrength & bn.hc.boot$direction > input$directionStrength,]
          bn.hc.boot.average <<- cextend(averaged.network(bn.hc.boot.pruned))
          bn.hc.boot.fit <<- bn.fit(bn.hc.boot.average,DiscreteData[,names(bn.hc.boot.average$nodes)],method = input$paramMethod2)
        }
        shinyalert("Structure learning done",type="success")
        simple<<-2
        upload<<-2
        for(elem in 1:length(inserted))
        {
          removeUI(
            ## pass in appropriate div id
            selector = paste0('#', inserted[elem])
          )

        }
        inserted <<- c()
        for(elem2 in 1:length(insertedV))
        {
          removeUI(
            ## pass in appropriate div id
            selector = paste0('#', insertedV[elem2])
          )

        }
        insertedV <<- c()
        rvs$evidence <<- c()
        rvs$value <<- c()
        rvs$evidenceObserve <<- c()
        rvs$valueObserve <<- c()
        output$distPlot <<- renderPlot(NULL)
        NetworkGraph <<- data.frame(directed.arcs(bn.hc.boot.average))
        nodeNames <<- names(bn.hc.boot.average$nodes)
        EventNode <<- nodeNames[1]
        EvidenceNode <<- c()
        shapeVector<<- rep('dot',length(nodeNames))
        updateSelectInput(session,'event',choices = nodeNames)
        weight <<- bn.hc.boot.pruned[dim(NetworkGraph)[1],3]
        value <<- bn.hc.boot.pruned[dim(NetworkGraph)[1],3]
        output$netPlot<-renderVisNetwork({graph.custom(NetworkGraph,nodeNames,shapeVector,EvidenceNode,EventNode,input$degree,input$graph_layout,weight,value)})
        updateSelectInput(session,'event',choices = nodeNames)
        updateSelectizeInput(session,'varselect',choices = nodeNames)
        updateSelectInput(session,'varshape',choices = c( "dot","square", "triangle", "box", "circle", "star",
                                                          "ellipse", "database", "text", "diamond"))
        updateSelectInput(session,'varshape2',choices = c( "dot","square", "triangle", "box", "circle", "star",
                                                           "ellipse", "database", "text", "diamond"))
        updateSelectInput(session,'graph_layout',choices = c("layout_nicely","layout_as_star","layout_as_tree","layout_in_circle","layout_with_sugiyama","layout_on_sphere","layout_randomly","layout_with_fr","layout_with_kk","layout_with_lgl","layout_with_mds","layout_on_grid","layout_with_graphopt","layout_with_gem","layout_with_dh"))
        updateSelectInput(session,'paramSelect',choices = nodeNames)
        updateSelectInput(session,"moduleSelection",choices = "graph")
        graph<<-graph_from_edgelist(as.matrix(NetworkGraph),directed = TRUE)
        updateSelectInput(session,"neighbornodes",choices = "")
        updateSelectInput(session,'varshape3',choices = c( "dot","square", "triangle", "box", "circle", "star",
                                                           "ellipse", "database", "text", "diamond"))
        updateSelectInput(session,'modGroup',choices = "")
        updateSliderInput(session,"NumBar",min = 1, max = nlevels(DiscreteData[,nodeNames[1]]),value = nlevels(DiscreteData[,nodeNames[1]]))
        reset<<-2
        type<<-2
        updateSelectInput(session,"fromarc",choices = nodeNames)
        output$postout<-DT::renderDataTable({bn.hc.boot.average$arcs},options = list(scrollX = TRUE,pageLength = 10),selection = 'single')
        save(DiscreteData,file="customDashboard/inst/cd/data.RData")
        save(bn.hc.boot.average,file="customDashboard/inst/cd/structure.RData")
        write.csv(input$name,file = "customDashboard/inst/cd/name.txt",row.names = FALSE)
      },error = function(e){
        shinyalert(toString(e), type = "error")
      })
    }
    tooltip(session)
  })
  observeEvent(input$PruneBtn,{
    tryCatch({
      if(upload==2)
      {
        if(reset==2)
        {
          if(simple==1)
          {
            if(input$parallel==T)
            {
              bn.hc.boot.fit <<- bn.fit(bn.hc.boot.average,DiscreteData[,names(bn.hc.boot.average$nodes)],method = input$paramMethod2,cluster = cl)
            }
            else
            {
              bn.hc.boot.fit <<- bn.fit(bn.hc.boot.average,DiscreteData[,names(bn.hc.boot.average$nodes)],method = input$paramMethod2)
            }
            simple<<-1
            for(elem in 1:length(inserted))
            {
              removeUI(
                ## pass in appropriate div id
                selector = paste0('#', inserted[elem])
              )

            }
            inserted <<- c()
            for(elem2 in 1:length(insertedV))
            {
              removeUI(
                ## pass in appropriate div id
                selector = paste0('#', insertedV[elem2])
              )

            }
            insertedV <<- c()
            rvs$evidence <<- c()
            rvs$value <<- c()
            rvs$evidenceObserve <<- c()
            rvs$valueObserve <<- c()
            output$distPlot <<- renderPlot(NULL)
            NetworkGraph <<- data.frame(directed.arcs(bn.hc.boot.average))
            nodeNames <<- names(bn.hc.boot.average$nodes)
            EventNode <<- nodeNames[1]
            EvidenceNode <<- c()
            shapeVector<<- rep('dot',length(nodeNames))
            updateSelectInput(session,'event',choices = nodeNames)
            weight <<- 1
            value <<- 1
            output$netPlot<-renderVisNetwork({graph.custom(NetworkGraph,nodeNames,shapeVector,EvidenceNode,EventNode,input$degree,input$graph_layout,weight,value)})
            updateSelectInput(session,'event',choices = nodeNames)
            updateSelectizeInput(session,'varselect',choices = nodeNames)
            updateSelectInput(session,'varshape',choices = c( "dot","square", "triangle", "box", "circle", "star",
                                                              "ellipse", "database", "text", "diamond"))
            updateSelectInput(session,'varshape2',choices = c( "dot","square", "triangle", "box", "circle", "star",
                                                               "ellipse", "database", "text", "diamond"))
            updateSelectInput(session,'graph_layout',choices = c("layout_nicely","layout_as_star","layout_as_tree","layout_in_circle","layout_with_sugiyama","layout_on_sphere","layout_randomly","layout_with_fr","layout_with_kk","layout_with_lgl","layout_with_mds","layout_on_grid","layout_with_graphopt","layout_with_gem","layout_with_dh"))
            updateSelectInput(session,'paramSelect',choices = nodeNames)
            updateSelectInput(session,"moduleSelection",choices = "graph")
            graph<<-graph_from_edgelist(as.matrix(NetworkGraph),directed = TRUE)
            updateSelectInput(session,"neighbornodes",choices = "")
            updateSelectInput(session,'varshape3',choices = c( "dot","square", "triangle", "box", "circle", "star",
                                                               "ellipse", "database", "text", "diamond"))
            updateSelectInput(session,'modGroup',choices = "")
            updateSliderInput(session,"NumBar",min = 1, max = nlevels(DiscreteData[,nodeNames[1]]),value = nlevels(DiscreteData[,nodeNames[1]]))
            reset<<-2
            type<<-1
            updateSelectInput(session,"fromarc",choices = nodeNames)
            output$postout<-DT::renderDataTable({bn.hc.boot.average$arcs},options = list(scrollX = TRUE,pageLength = 10),selection = 'single')
          }
          else
          {
            if(input$parallel==T)
            {
              bn.hc.boot.pruned <<- bn.hc.boot[bn.hc.boot$strength > input$edgeStrength & bn.hc.boot$direction > input$directionStrength,]
              bn.hc.boot.average <<- cextend(averaged.network(bn.hc.boot.pruned))
              bn.hc.boot.fit <<- bn.fit(bn.hc.boot.average,DiscreteData[,names(bn.hc.boot.average$nodes)],method = input$paramMethod2,cluster = cl)
            }
            else
            {
              bn.hc.boot.pruned <<- bn.hc.boot[bn.hc.boot$strength > input$edgeStrength & bn.hc.boot$direction > input$directionStrength,]
              bn.hc.boot.average <<- cextend(averaged.network(bn.hc.boot.pruned))
              bn.hc.boot.fit <<- bn.fit(bn.hc.boot.average,DiscreteData[,names(bn.hc.boot.average$nodes)],method = input$paramMethod2)
            }
            simple<<-2
            for(elem in 1:length(inserted))
            {
              removeUI(
                ## pass in appropriate div id
                selector = paste0('#', inserted[elem])
              )

            }
            inserted <<- c()
            for(elem2 in 1:length(insertedV))
            {
              removeUI(
                ## pass in appropriate div id
                selector = paste0('#', insertedV[elem2])
              )

            }
            insertedV <<- c()
            rvs$evidence <<- c()
            rvs$value <<- c()
            rvs$evidenceObserve <<- c()
            rvs$valueObserve <<- c()
            output$distPlot <<- renderPlot(NULL)
            NetworkGraph <<- data.frame(directed.arcs(bn.hc.boot.average))
            nodeNames <<- names(bn.hc.boot.average$nodes)
            EventNode <<- nodeNames[1]
            EvidenceNode <<- c()
            shapeVector<<- rep('dot',length(nodeNames))
            updateSelectInput(session,'event',choices = nodeNames)
            weight <<- bn.hc.boot.pruned[dim(NetworkGraph)[1],3]
            value <<- bn.hc.boot.pruned[dim(NetworkGraph)[1],3]
            output$netPlot<-renderVisNetwork({graph.custom(NetworkGraph,nodeNames,shapeVector,EvidenceNode,EventNode,input$degree,input$graph_layout,weight,value)})
            updateSelectInput(session,'event',choices = nodeNames)
            updateSelectizeInput(session,'varselect',choices = nodeNames)
            updateSelectInput(session,'varshape',choices = c( "dot","square", "triangle", "box", "circle", "star",
                                                              "ellipse", "database", "text", "diamond"))
            updateSelectInput(session,'varshape2',choices = c( "dot","square", "triangle", "box", "circle", "star",
                                                               "ellipse", "database", "text", "diamond"))
            updateSelectInput(session,'graph_layout',choices = c("layout_nicely","layout_as_star","layout_as_tree","layout_in_circle","layout_with_sugiyama","layout_on_sphere","layout_randomly","layout_with_fr","layout_with_kk","layout_with_lgl","layout_with_mds","layout_on_grid","layout_with_graphopt","layout_with_gem","layout_with_dh"))
            updateSelectInput(session,'paramSelect',choices = nodeNames)
            updateSelectInput(session,"moduleSelection",choices = "graph")
            graph<<-graph_from_edgelist(as.matrix(NetworkGraph),directed = TRUE)
            updateSelectInput(session,"neighbornodes",choices = "")
            updateSelectInput(session,'varshape3',choices = c( "dot","square", "triangle", "box", "circle", "star",
                                                               "ellipse", "database", "text", "diamond"))
            updateSelectInput(session,'modGroup',choices = "")
            updateSliderInput(session,"NumBar",min = 1, max = nlevels(DiscreteData[,nodeNames[1]]),value = nlevels(DiscreteData[,nodeNames[1]]))
            reset<<-2
            type<<-2
            updateSelectInput(session,"fromarc",choices = nodeNames)
            output$postout<-DT::renderDataTable({bn.hc.boot.average$arcs},options = list(scrollX = TRUE,pageLength = 10),selection = 'single')
            save(DiscreteData,file="customDashboard/inst/cd/data.RData")
            save(bn.hc.boot.average,file="customDashboard/inst/cd/structure.RData")
            write.csv(input$name,file = "customDashboard/inst/cd/name.txt",row.names = FALSE)
          }
        }
      }
    },error=function(e){
      shinyalert(toString(e), type = "error")
    })
    tooltip(session)
  })
  observeEvent(input$learnSBtn, {
    if(check.NA(DiscreteData))
    {
      shinyalert("Please impute missingness in the data first",type="info")
    }
    else if(check.discrete(DiscreteData))
    {
      shinyalert("Please discritize the data first",type="info")
    }
    else
    {
      tryCatch({
        shinyalert("Structure Learning started",type="info")
        if (is.null(DiscreteData))
          return(NULL)

        # Create a Progress object
        progress <- shiny::Progress$new()
        # Make sure it closes when we exit this reactive, even if there's an error
        on.exit(progress$close())
        progress$set(message = "Learning network structure", value = 0)

        # Get the selected learning algorithm from the user and learn the network
        if(input$parallel==T)
        {
          if(input$alg == 'hc')
          {
            bn.hc.boot.average <<- cextend(bnlearn::hc(DiscreteData,blacklist=blacklistEdges,whitelist=whitelistEdges,cluster = cl,start=bn.start))
          }
          else if(input$alg =="pc.stable")
          {
            bn.hc.boot.average <<- cextend(bnlearn::pc.stable(DiscreteData,blacklist=blacklistEdges,whitelist=whitelistEdges,cluster = cl,start=bn.start))
          }
          else if(input$alg == 'tabu')
          {
            bn.hc.boot.average <<- cextend(bnlearn::tabu(DiscreteData,blacklist=blacklistEdges,whitelist=whitelistEdges,cluster = cl,start=bn.start))
          }
          else if(input$alg == 'gs')
          {
            bn.hc.boot.average <<- cextend(bnlearn::gs(DiscreteData,blacklist=blacklistEdges,whitelist=whitelistEdges,cluster=cl,start=bn.start))
          }
          else if(input$alg == 'iamb')
          {
            bn.hc.boot.average <<- cextend(bnlearn::iamb(DiscreteData,blacklist=blacklistEdges,whitelist=whitelistEdges,cluster=cl,start=bn.start))
          }
          else if(input$alg == 'fast.iamb')
          {
            bn.hc.boot.average <<- cextend(bnlearn::fast.iamb(DiscreteData,blacklist=blacklistEdges,whitelist=whitelistEdges,cluster=cl,start=bn.start))
          }
          else if(input$alg=='inter.iamb')
          {
            bn.hc.boot.average <<- cextend(bnlearn::inter.iamb(DiscreteData,blacklist=blacklistEdges,whitelist=whitelistEdges,cluster=cl,start=bn.start))
          }
          else if(input$alg == 'mmhc')
          {
            bn.hc.boot.average <<- cextend(bnlearn::mmhc(DiscreteData,blacklist=blacklistEdges,whitelist=whitelistEdges,cluster=cl,start=bn.start))
          }
          else if(input$alg == 'rsmax2')
          {
            bn.hc.boot.average <<- cextend(bnlearn::rsmax2(DiscreteData,blacklist=blacklistEdges,whitelist=whitelistEdges,cluster = cl,start=bn.start))
          }
          else if(input$alg == 'mmpc')
          {
            bn.hc.boot.average <<- cextend(bnlearn::mmpc(DiscreteData,blacklist=blacklistEdges,whitelist=whitelistEdges,cluster = cl,start=bn.start))
          }
          else if(input$alg == 'si.hiton.pc')
          {
            bn.hc.boot.average <<- cextend(bnlearn::si.hiton.pc(DiscreteData,blacklist=blacklistEdges,whitelist=whitelistEdges,cluster = cl,start=bn.start))
          }
          else if(input$alg == 'aracne')
          {
            bn.hc.boot.average <<- cextend(bnlearn::aracne(DiscreteData,blacklist=blacklistEdges,whitelist=whitelistEdges,cluster = cl,start=bn.start))
          }
          else
          {
            bn.hc.boot.average <<- cextend(bnlearn::chow.liu(DiscreteData,blacklist=blacklistEdges,whitelist=whitelistEdges,cluster = cl,start=bn.start))
          }
          #bn.hc.boot.average <<- bnlearn::hc(DiscreteData)
          bn.hc.boot.fit <<- bn.fit(bn.hc.boot.average,DiscreteData[,names(bn.hc.boot.average$nodes)],method = input$paramMethod2,cluster=cl)
        }
        else
        {
          if(input$alg == 'hc')
          {
            bn.hc.boot.average <<- cextend(bnlearn::hc(DiscreteData,blacklist=blacklistEdges,whitelist=whitelistEdges,start=bn.start))
          }
          else if(input$alg =="pc.stable")
          {
            bn.hc.boot.average <<- cextend(bnlearn::pc.stable(DiscreteData,blacklist=blacklistEdges,whitelist=whitelistEdges,start=bn.start))
          }
          else if(input$alg == 'tabu')
          {
            bn.hc.boot.average <<- cextend(bnlearn::tabu(DiscreteData,blacklist=blacklistEdges,whitelist=whitelistEdges,start=bn.start))
          }
          else if(input$alg == 'gs')
          {
            bn.hc.boot.average <<- cextend(bnlearn::gs(DiscreteData,blacklist=blacklistEdges,whitelist=whitelistEdges,start=bn.start))
          }
          else if(input$alg == 'iamb')
          {
            bn.hc.boot.average <<- cextend(bnlearn::iamb(DiscreteData,blacklist=blacklistEdges,whitelist=whitelistEdges,start=bn.start))
          }
          else if(input$alg == 'fast.iamb')
          {
            bn.hc.boot.average <<- cextend(bnlearn::fast.iamb(DiscreteData,blacklist=blacklistEdges,whitelist=whitelistEdges,start=bn.start))
          }
          else if(input$alg=='inter.iamb')
          {
            bn.hc.boot.average <<- cextend(bnlearn::inter.iamb(DiscreteData,blacklist=blacklistEdges,whitelist=whitelistEdges,start=bn.start))
          }
          else if(input$alg == 'mmhc')
          {
            bn.hc.boot.average <<- cextend(bnlearn::mmhc(DiscreteData,blacklist=blacklistEdges,whitelist=whitelistEdges,start=bn.start))
          }
          else if(input$alg == 'rsmax2')
          {
            bn.hc.boot.average <<- cextend(bnlearn::rsmax2(DiscreteData,blacklist=blacklistEdges,whitelist=whitelistEdges,start=bn.start))
          }
          else if(input$alg == 'mmpc')
          {
            bn.hc.boot.average <<- cextend(bnlearn::mmpc(DiscreteData,blacklist=blacklistEdges,whitelist=whitelistEdges,start=bn.start))
          }
          else if(input$alg == 'si.hiton.pc')
          {
            bn.hc.boot.average <<- cextend(bnlearn::si.hiton.pc(DiscreteData,blacklist=blacklistEdges,whitelist=whitelistEdges,start=bn.start))
          }
          else if(input$alg == 'aracne')
          {
            bn.hc.boot.average <<- cextend(bnlearn::aracne(DiscreteData,blacklist=blacklistEdges,whitelist=whitelistEdges,start=bn.start))
          }
          else
          {
            bn.hc.boot.average <<- cextend(bnlearn::chow.liu(DiscreteData,blacklist=blacklistEdges,whitelist=whitelistEdges,start=bn.start))
          }
          #bn.hc.boot.average <<- bnlearn::hc(DiscreteData)
          bn.hc.boot.fit <<- bn.fit(bn.hc.boot.average,DiscreteData[,names(bn.hc.boot.average$nodes)],method = input$paramMethod2)
        }
        shinyalert("Structure learning done",type="success")
        simple<<-1
        upload<<-2
        type<<-1
        for(elem in 1:length(inserted))
        {
          removeUI(
            ## pass in appropriate div id
            selector = paste0('#', inserted[elem])
          )

        }
        inserted <<- c()
        for(elem2 in 1:length(insertedV))
        {
          removeUI(
            ## pass in appropriate div id
            selector = paste0('#', insertedV[elem2])
          )

        }
        insertedV <<- c()
        rvs$evidence <<- c()
        rvs$value <<- c()
        rvs$evidenceObserve <<- c()
        rvs$valueObserve <<- c()
        output$distPlot <<- renderPlot(NULL)
        NetworkGraph <<- data.frame(directed.arcs(bn.hc.boot.average))
        nodeNames <<- names(bn.hc.boot.average$nodes)
        EventNode <<- nodeNames[1]
        EvidenceNode <<- c()
        shapeVector<<- rep('dot',length(nodeNames))
        updateSelectInput(session,'event',choices = nodeNames)
        weight <<- 1
        value <<- 1
        output$netPlot<-renderVisNetwork({graph.custom(NetworkGraph,nodeNames,shapeVector,EvidenceNode,EventNode,input$degree,input$graph_layout,weight,value)})
        updateSelectInput(session,'event',choices = nodeNames)
        updateSelectizeInput(session,'varselect',choices = nodeNames)
        updateSelectInput(session,'varshape',choices = c( "dot","square", "triangle", "box", "circle", "star",
                                                          "ellipse", "database", "text", "diamond"))
        updateSelectInput(session,'varshape2',choices = c( "dot","square", "triangle", "box", "circle", "star",
                                                           "ellipse", "database", "text", "diamond"))
        updateSelectInput(session,'graph_layout',choices = c("layout_nicely","layout_as_star","layout_as_tree","layout_in_circle","layout_with_sugiyama","layout_on_sphere","layout_randomly","layout_with_fr","layout_with_kk","layout_with_lgl","layout_with_mds","layout_on_grid","layout_with_graphopt","layout_with_gem","layout_with_dh"))
        updateSelectInput(session,'paramSelect',choices = nodeNames)
        updateSelectInput(session,"moduleSelection",choices = "graph")
        updateSelectInput(session,'varshape3',choices = c( "dot","square", "triangle", "box", "circle", "star",
                                                           "ellipse", "database", "text", "diamond"))
        updateSelectInput(session,'modGroup',choices = "")
        graph<<-graph_from_edgelist(as.matrix(NetworkGraph),directed = TRUE)
        updateSelectInput(session,"neighbornodes",choices = "")
        updateSliderInput(session,"NumBar",min = 1, max = nlevels(DiscreteData[,nodeNames[1]]),value = nlevels(DiscreteData[,nodeNames[1]]))
        reset<<-2
        updateSelectInput(session,"fromarc",choices = nodeNames)
        output$postout<-DT::renderDataTable({bn.hc.boot.average$arcs},options = list(scrollX = TRUE,pageLength = 10),selection = 'single')
        save(DiscreteData,file="customDashboard/inst/cd/data.RData")
        save(bn.hc.boot.average,file="customDashboard/inst/cd/structure.RData")
        write.csv(input$name,file = "customDashboard/inst/cd/name.txt",row.names = FALSE)
      },error = function(e){
        shinyalert(toString(e), type = "error")
      })
    }
    tooltip(session)
  })
  observeEvent(input$fromarc,{
    tryCatch({
      if(reset==2)
      {
        updateSelectInput(session,"toarc",choices = setdiff(nodeNames,input$fromarc))
      }
    },error = function(e){
      shinyalert(toString(e), type = "error")
    })
    tooltip(session)
  })
  observeEvent(input$addarc,{
    tryCatch({
      if(input$parallel==T)
      {
        bn.hc.boot.average<<-set.arc(bn.hc.boot.average,input$fromarc,input$toarc)
        bn.hc.boot.fit <<- bn.fit(bn.hc.boot.average,DiscreteData[,names(bn.hc.boot.average$nodes)],method = input$paramMethod2,cluster = cl)
      }
      else
      {
        bn.hc.boot.average<<-set.arc(bn.hc.boot.average,input$fromarc,input$toarc)
        bn.hc.boot.fit <<- bn.fit(bn.hc.boot.average,DiscreteData[,names(bn.hc.boot.average$nodes)],method = input$paramMethod2)
      }
      for(elem in 1:length(inserted))
      {
        removeUI(
          ## pass in appropriate div id
          selector = paste0('#', inserted[elem])
        )

      }
      inserted <<- c()
      for(elem2 in 1:length(insertedV))
      {
        removeUI(
          ## pass in appropriate div id
          selector = paste0('#', insertedV[elem2])
        )

      }
      insertedV <<- c()
      rvs$evidence <<- c()
      rvs$value <<- c()
      rvs$evidenceObserve <<- c()
      rvs$valueObserve <<- c()
      output$distPlot <<- renderPlot(NULL)
      NetworkGraph <<- data.frame(directed.arcs(bn.hc.boot.average))
      nodeNames <<- names(bn.hc.boot.average$nodes)
      EventNode <<- nodeNames[1]
      EvidenceNode <<- c()
      type<<-1
      shapeVector<<- rep('dot',length(nodeNames))
      updateSelectInput(session,'event',choices = nodeNames)
      weight <<- 1
      value <<- 1
      output$netPlot<-renderVisNetwork({graph.custom(NetworkGraph,nodeNames,shapeVector,EvidenceNode,EventNode,input$degree,input$graph_layout,weight,value)})
      updateSelectInput(session,'event',choices = nodeNames)
      updateSelectizeInput(session,'varselect',choices = nodeNames)
      updateSelectInput(session,'varshape',choices = c( "dot","square", "triangle", "box", "circle", "star",
                                                        "ellipse", "database", "text", "diamond"))
      updateSelectInput(session,'varshape2',choices = c( "dot","square", "triangle", "box", "circle", "star",
                                                         "ellipse", "database", "text", "diamond"))
      updateSelectInput(session,'graph_layout',choices = c("layout_nicely","layout_as_star","layout_as_tree","layout_in_circle","layout_with_sugiyama","layout_on_sphere","layout_randomly","layout_with_fr","layout_with_kk","layout_with_lgl","layout_with_mds","layout_on_grid","layout_with_graphopt","layout_with_gem","layout_with_dh"))
      updateSelectInput(session,'paramSelect',choices = nodeNames)
      updateSelectInput(session,"moduleSelection",choices = "graph")
      updateSelectInput(session,'varshape3',choices = c( "dot","square", "triangle", "box", "circle", "star",
                                                         "ellipse", "database", "text", "diamond"))
      updateSelectInput(session,'modGroup',choices = "")
      graph<<-graph_from_edgelist(as.matrix(NetworkGraph),directed = TRUE)
      updateSelectInput(session,"neighbornodes",choices = "")
      updateSliderInput(session,"NumBar",min = 1, max = nlevels(DiscreteData[,nodeNames[1]]),value = nlevels(DiscreteData[,nodeNames[1]]))
      reset<<-2
      updateSelectInput(session,"fromarc",choices = nodeNames)
      output$postout<-DT::renderDataTable({bn.hc.boot.average$arcs},options = list(scrollX = TRUE,pageLength = 10),selection = 'single')
      save(DiscreteData,file="customDashboard/inst/cd/data.RData")
      save(bn.hc.boot.average,file="customDashboard/inst/cd/structure.RData")
      write.csv(input$name,file = "customDashboard/inst/cd/name.txt",row.names = FALSE)
    },error = function(e){
      shinyalert(toString(e), type = "error")
    })
    tooltip(session)
  })
  observeEvent(input$RemoveArc2,{
    tryCatch({
      if(reset==2)
      {
        if(!is.null(input$postout_rows_selected))
        {
          bn.hc.boot.average<<-drop.arc(bn.hc.boot.average,bn.hc.boot.average$arcs[input$postout_rows_selected,1],bn.hc.boot.average$arcs[input$postout_rows_selected,2])
          if(input$parallel==T)
          {
            bn.hc.boot.fit <<- bn.fit(bn.hc.boot.average,DiscreteData[,names(bn.hc.boot.average$nodes)],method = input$paramMethod2,cluster = cl)
          }
          else
          {
            bn.hc.boot.fit <<- bn.fit(bn.hc.boot.average,DiscreteData[,names(bn.hc.boot.average$nodes)],method = input$paramMethod2)
          }
          for(elem in 1:length(inserted))
          {
            removeUI(
              ## pass in appropriate div id
              selector = paste0('#', inserted[elem])
            )

          }
          inserted <<- c()
          for(elem2 in 1:length(insertedV))
          {
            removeUI(
              ## pass in appropriate div id
              selector = paste0('#', insertedV[elem2])
            )

          }
          insertedV <<- c()
          rvs$evidence <<- c()
          rvs$value <<- c()
          rvs$evidenceObserve <<- c()
          rvs$valueObserve <<- c()
          output$distPlot <<- renderPlot(NULL)
          NetworkGraph <<- data.frame(directed.arcs(bn.hc.boot.average))
          nodeNames <<- names(bn.hc.boot.average$nodes)
          EventNode <<- nodeNames[1]
          EvidenceNode <<- c()
          shapeVector<<- rep('dot',length(nodeNames))
          updateSelectInput(session,'event',choices = nodeNames)
          weight <<- 1
          value <<- 1
          type<<-1
          output$netPlot<-renderVisNetwork({graph.custom(NetworkGraph,nodeNames,shapeVector,EvidenceNode,EventNode,input$degree,input$graph_layout,weight,value)})
          updateSelectInput(session,'event',choices = nodeNames)
          updateSelectizeInput(session,'varselect',choices = nodeNames)
          updateSelectInput(session,'varshape',choices = c( "dot","square", "triangle", "box", "circle", "star",
                                                            "ellipse", "database", "text", "diamond"))
          updateSelectInput(session,'varshape2',choices = c( "dot","square", "triangle", "box", "circle", "star",
                                                             "ellipse", "database", "text", "diamond"))
          updateSelectInput(session,'graph_layout',choices = c("layout_nicely","layout_as_star","layout_as_tree","layout_in_circle","layout_with_sugiyama","layout_on_sphere","layout_randomly","layout_with_fr","layout_with_kk","layout_with_lgl","layout_with_mds","layout_on_grid","layout_with_graphopt","layout_with_gem","layout_with_dh"))
          updateSelectInput(session,'paramSelect',choices = nodeNames)
          updateSelectInput(session,"moduleSelection",choices = "graph")
          graph<<-graph_from_edgelist(as.matrix(NetworkGraph),directed = TRUE)
          updateSelectInput(session,"neighbornodes",choices = "")
          updateSliderInput(session,"NumBar",min = 1, max = nlevels(DiscreteData[,nodeNames[1]]),value = nlevels(DiscreteData[,nodeNames[1]]))
          reset<<-2
          updateSelectInput(session,"fromarc",choices = nodeNames)
          updateSelectInput(session,'varshape3',choices = c( "dot","square", "triangle", "box", "circle", "star",
                                                             "ellipse", "database", "text", "diamond"))
          updateSelectInput(session,'modGroup',choices = "")
          output$postout<-DT::renderDataTable({bn.hc.boot.average$arcs},options = list(scrollX = TRUE,pageLength = 10),selection = 'single')
          save(DiscreteData,file="customDashboard/inst/cd/data.RData")
          save(bn.hc.boot.average,file="customDashboard/inst/cd/structure.RData")
          write.csv(input$name,file = "customDashboard/inst/cd/name.txt",row.names = FALSE)
        }
      }
    },error = function(e){
      shinyalert(toString(e), type = "error")
    })
    tooltip(session)
  })
  observeEvent(input$ReverseArc2,{
    tryCatch({
      if(reset==2)
      {
        if(!is.null(input$postout__rows_selected))
        {
          bn.hc.boot.average<<-reverse.arc(bn.hc.boot.average,bn.hc.boot.average$arcs[input$postout_rows_selected,1],bn.hc.boot.average$arcs[input$postout_rows_selected,2])
          if(input$parallel==T)
          {
            bn.hc.boot.fit <<- bn.fit(bn.hc.boot.average,DiscreteData[,names(bn.hc.boot.average$nodes)],method = input$paramMethod2,cluster = cl)
          }
          else
          {
            bn.hc.boot.fit <<- bn.fit(bn.hc.boot.average,DiscreteData[,names(bn.hc.boot.average$nodes)],method = input$paramMethod2)
          }
          for(elem in 1:length(inserted))
          {
            removeUI(
              ## pass in appropriate div id
              selector = paste0('#', inserted[elem])
            )

          }
          inserted <<- c()
          for(elem2 in 1:length(insertedV))
          {
            removeUI(
              ## pass in appropriate div id
              selector = paste0('#', insertedV[elem2])
            )

          }
          insertedV <<- c()
          rvs$evidence <<- c()
          rvs$value <<- c()
          rvs$evidenceObserve <<- c()
          rvs$valueObserve <<- c()
          output$distPlot <<- renderPlot(NULL)
          NetworkGraph <<- data.frame(directed.arcs(bn.hc.boot.average))
          nodeNames <<- names(bn.hc.boot.average$nodes)
          EventNode <<- nodeNames[1]
          EvidenceNode <<- c()
          shapeVector<<- rep('dot',length(nodeNames))
          updateSelectInput(session,'event',choices = nodeNames)
          type<<-1
          weight <<- 1
          value <<- 1
          output$netPlot<-renderVisNetwork({graph.custom(NetworkGraph,nodeNames,shapeVector,EvidenceNode,EventNode,input$degree,input$graph_layout,weight,value)})
          updateSelectInput(session,'event',choices = nodeNames)
          updateSelectizeInput(session,'varselect',choices = nodeNames)
          updateSelectInput(session,'varshape',choices = c( "dot","square", "triangle", "box", "circle", "star",
                                                            "ellipse", "database", "text", "diamond"))
          updateSelectInput(session,'varshape2',choices = c( "dot","square", "triangle", "box", "circle", "star",
                                                             "ellipse", "database", "text", "diamond"))
          updateSelectInput(session,'graph_layout',choices = c("layout_nicely","layout_as_star","layout_as_tree","layout_in_circle","layout_with_sugiyama","layout_on_sphere","layout_randomly","layout_with_fr","layout_with_kk","layout_with_lgl","layout_with_mds","layout_on_grid","layout_with_graphopt","layout_with_gem","layout_with_dh"))
          updateSelectInput(session,'paramSelect',choices = nodeNames)
          updateSelectInput(session,"moduleSelection",choices = "graph")
          graph<<-graph_from_edgelist(as.matrix(NetworkGraph),directed = TRUE)
          updateSelectInput(session,"neighbornodes",choices = "")
          updateSliderInput(session,"NumBar",min = 1, max = nlevels(DiscreteData[,nodeNames[1]]),value = nlevels(DiscreteData[,nodeNames[1]]))
          reset<<-2
          updateSelectInput(session,"fromarc",choices = nodeNames)
          updateSelectInput(session,'varshape3',choices = c( "dot","square", "triangle", "box", "circle", "star",
                                                             "ellipse", "database", "text", "diamond"))
          updateSelectInput(session,'modGroup',choices = "")
          output$postout<-DT::renderDataTable({bn.hc.boot.average$arcs},options = list(scrollX = TRUE,pageLength = 10),selection = 'single')
          save(DiscreteData,file="customDashboard/inst/cd/data.RData")
          save(bn.hc.boot.average,file="customDashboard/inst/cd/structure.RData")
          write.csv(input$name,file = "customDashboard/inst/cd/name.txt",row.names = FALSE)
        }
      }
    },error = function(e){
      shinyalert(toString(e), type = "error")
    })
    tooltip(session)
  })
  observeEvent(input$paramSelect,{
    if(reset==2)
    {
      tryCatch({
        output$parameterPlot<-renderPlot({bn.fit.barchart(bn.hc.boot.fit[[input$paramSelect]])})
      },error = function(e){
        shinyalert(toString(e), type = "error")
      })
    }
    tooltip(session)
  })
  observeEvent(input$parallel,{
    withProgress(message = "Building Clusters", value = 0, {
      tryCatch({
        if(input$parallel==TRUE)
        {
          check<<-2
          cl <<- makeCluster(strtoi(input$clusters), type = "SOCK")
          shinyalert("Parallel clusters successfully created",type="success")
        }
        else
        {
          if(check==2)
          {
            stopCluster(cl)
            ckeck<<-1
          }
        }
      },error=function(e){
        shinyalert(toString(e), type = "error")
      })
    })
    tooltip(session)
  })
  observeEvent(input$insertBtn, {
    withProgress(message = "Inserting Evidence", value = 0, {
      if(reset==2)
      {
        tryCatch({
          btn <<- input$insertBtn
          id <- paste0('Evidence', btn)
          idL <- paste("Evidence", btn)
          idV <- paste0('Value', btn)
          idVL <- paste("Value", btn)
          insertUI(selector = '#placeholder1',
                   ui = tags$div(selectInput(id,'Evidence',nodeNames),
                                 id = id
                   )
          )
          insertUI(selector = '#placeholder2',
                   ui = tags$div(selectInput(idV,'Value',levels(DiscreteData[,nodeNames[1]])),
                                 id = idV
                   )
          )
          inserted <<- c(id, inserted)
          insertedV <<- c(idV,insertedV)
          rvs$evidence <<- c(rvs$evidence,id)
          rvs$value <<- c(rvs$value,id)
          rvs$evidenceObserve <<- c(rvs$evidenceObserve,observeEvent(input[[id]],{
            tryCatch({
              valID = insertedV[which(inserted == id)]
              updateSelectInput(session,valID, choices = levels(DiscreteData[,input[[id]]]))
            },error = function(e){
              shinyalert(toString("Please learn structure or upload structure on new data uploaded to make infrences"), type = "error")
            })
          }))

        },error = function(e){
          shinyalert(toString(e), type = "error")
        })
      }
    })
    tooltip(session)
  })

  observeEvent(input$removeBtn, {
    if(reset==2)
    {
      tryCatch({
        removeUI(
          ## pass in appropriate div id
          selector = paste0('#', inserted[length(inserted)])
        )
        inserted <<- inserted[-length(inserted)]
        removeUI(
          ## pass in appropriate div id
          selector = paste0('#', insertedV[length(insertedV)])
        )
        insertedV <<- insertedV[-length(insertedV)]
        rvs$evidence <<- rvs$evidence[-length(inserted)]
        rvs$value <<- rvs$value[-length(insertedV)]
        rvs$evidenceObserve <<- rvs$evidenceObserve[-length(inserted)]
        rvs$valueObserve <<- rvs$valueObserve[-length(insertedV)]
      },error=function(e){
        shinyalert(toString(e), type = "error")
      })
    }
    tooltip(session)
  })
  observeEvent(input$event,{
    if(reset==2)
    {
      tryCatch({
        if(input$event=="")
        {
          updateSliderInput(session,"NumBar",min = 1, max = nlevels(DiscreteData[,nodeNames[1]]),value = nlevels(DiscreteData[,nodeNames[1]]))
        }
        else
        {
          updateSliderInput(session,"NumBar",min = 1, max = nlevels(DiscreteData[,input$event]),value = nlevels(DiscreteData[,input$event]))
        }
      },error=function(e){
        shinyalert(toString(e), type = "error")
      })
    }
    tooltip(session)
  })
  observeEvent(input$plotBtn,{
    withProgress(message = "Learning Inference", value = 0, {
      if(reset==2)
      {
        tryCatch({
          confidence<<-1
          str1 <<- ""
          count =1
          for(elem in inserted)
          {
            vid = insertedV[which(inserted == elem)]
            str1 <<- paste0(str1,"(", input[[elem]], "=='", input[[vid]], "')")
            if(count!=length(inserted))
            {
              str1 <<- paste0(str1," & ")
            }
            count = count + 1
          }
          probs = prop.table(table(cpdist(bn.hc.boot.fit,input$event,evidence = eval(parse(text = str1)))))[1:input$NumBar]
          output$distPlot = renderPlot({par(mar=c(5,3,3,3))
            par(oma=c(5,3,3,3))
            barx<<-barplot(probs,
                           col = "lightblue",
                           main = paste("Conditional Probabilities on ",input$event),
                           border = NA,
                           xlab = "",
                           ylab = "Probabilities",
                           ylim = c(0,1),
                           las=2)
            text(x = barx,y = round(probs,digits = 4),label = round(probs,digits = 4), pos = 3, cex = 0.8, col = "black")
          })
        },error = function(e){
          shinyalert(toString(e), type = "error")
        })
      }
    })
    tooltip(session)
  })
  observeEvent(input$plotStrengthBtn,{
    withProgress(message = "Learning Inference", value = 0, {
      if(reset==2)
      {
        tryCatch({
          confidence<<-2
          probT = c()
          for(i in 1:input$plotStrengthBtn)
          {
            str1 <<- ""
            count =1
            for(elem in inserted)
            {
              vid = insertedV[which(inserted == elem)]
              str1 <<- paste0(str1,"(", input[[elem]], "=='", input[[vid]], "')")
              if(count!=length(inserted))
              {
                str1 <<- paste0(str1," & ")
              }
              count = count + 1
            }
            probs = prop.table(table(cpdist(bn.hc.boot.fit,input$event,evidence = eval(parse(text = str1)))))
            probT = rbind(probT,probs)
          }
          ee = 1
          ee$mean = colMeans(probT)
          ee$sd = apply(probT, 2, sd)
          output$distPlot = renderPlot({par(mar=c(5,3,3,3))
            par(oma=c(5,3,3,3))
            barx <<-barplot(ee$mean[1:input$NumBar],
                            col = "lightblue",
                            main = paste("Conditional Probabilities on ",input$event),
                            border = NA,
                            xlab = "",
                            ylab = "Probabilities",
                            ylim = c(0,1),
                            las=2)
            text(x = barx,y = round(ee$mean[1:input$NumBar],digits = 4),label = round(ee$mean[1:input$NumBar],digits = 4), pos = 3, cex = 0.8, col = "black")
            error.bar(barx,ee$mean[1:input$NumBar], 1.96*ee$sd[1:input$NumBar]/sqrt(input$plotStrengthBtn))})

        },error = function(e){
          shinyalert(toString(e), type = "error")
        })
      }
    })
    tooltip(session)
  })
  observeEvent(input$sortPlot,{
    withProgress(message = "Learning Inference", value = 0, {
      if(reset==2)
      {
        if(confidence==1)
        {
          tryCatch({
            confidence<<-1
            str1 <<- ""
            count =1
            for(elem in inserted)
            {
              vid = insertedV[which(inserted == elem)]
              str1 <<- paste0(str1,"(", input[[elem]], "=='", input[[vid]], "')")
              if(count!=length(inserted))
              {
                str1 <<- paste0(str1," & ")
              }
              count = count + 1
            }
            probs = sort(prop.table(table(cpdist(bn.hc.boot.fit,input$event,evidence = eval(parse(text = str1))))),decreasing = T)[1:input$NumBar]
            output$distPlot = renderPlot({par(mar=c(5,3,3,3))
              par(oma=c(5,3,3,3))
              barx<<-barplot(probs,
                             col = "lightblue",
                             main = paste("Conditional Probabilities on ",input$event),
                             border = NA,
                             xlab = "",
                             ylab = "Probabilities",
                             ylim = c(0,1),
                             las=2)
              text(x = barx,y = round(probs,digits = 4),label = round(probs,digits = 4), pos = 3, cex = 0.8, col = "black")
            })


          },error = function(e){
            shinyalert(toString(e), type = "error")
          })
        }
        else
        {
          tryCatch({
            confidence<<-2
            probT = c()
            for(i in 1:input$plotStrengthBtn)
            {
              str1 <<- ""
              count =1
              for(elem in inserted)
              {
                vid = insertedV[which(inserted == elem)]
                str1 <<- paste0(str1,"(", input[[elem]], "=='", input[[vid]], "')")
                if(count!=length(inserted))
                {
                  str1 <<- paste0(str1," & ")
                }
                count = count + 1
              }
              probs = prop.table(table(cpdist(bn.hc.boot.fit,input$event,evidence = eval(parse(text = str1)))))
              probT = rbind(probT,probs)
            }
            ee = 1
            ee$mean = colMeans(probT)
            ee$sd = apply(probT, 2, sd)
            nm = names(sort(ee$mean,decreasing = T))[1:input$NumBar]
            output$distPlot = renderPlot({par(mar=c(5,3,3,3))
              par(oma=c(5,3,3,3))
              barx <<-barplot(ee$mean[nm],
                              col = "lightblue",
                              main = paste("Conditional Probabilities on ",input$event),
                              border = NA,
                              xlab = "",
                              ylab = "Probabilities",
                              ylim = c(0,1),
                              las=2)
              text(x = barx,y = round(ee$mean[nm],digits = 4),label = round(ee$mean[nm],digits = 4), pos = 3, cex = 0.8, col = "black")
              error.bar(barx,ee$mean[nm], 1.96*ee$sd[nm]/sqrt(input$plotStrengthBtn))})

          },error = function(e){
            shinyalert(toString(e), type = "error")
          })
        }
      }
    })
    tooltip(session)
  })
  observeEvent(input$moduleSelection,{
    withProgress(message = "Loading Module", value = 0, {
      if(reset==2)
      {
        tryCatch({
          if(input$moduleSelection!='graph')
          {
            selectedNodes<<-communities[[lengthCom[input$moduleSelection]]]
            from<-c()
            to<-c()
            for(i in 1:length(data.frame(directed.arcs(bn.hc.boot.average))[,1]))
            {
              if(is.element(data.frame(directed.arcs(bn.hc.boot.average))[i,1],selectedNodes))
              {
                from<-c(from,i)
              }
              if(is.element(data.frame(directed.arcs(bn.hc.boot.average))[i,2],selectedNodes))
              {
                to<-c(to,i)
              }
            }
            pruneGraph<<-data.frame(directed.arcs(bn.hc.boot.average))[intersect(from,to),]
            NetworkGraph<<-pruneGraph
            shapeVector<<-rep('dot',length(communities[[input$moduleSelection]]))
            for(elem in 1:length(inserted))
            {
              removeUI(
                selector = paste0('#', inserted[elem])
              )

            }
            inserted <<- c()
            for(elem2 in 1:length(insertedV))
            {
              removeUI(
                selector = paste0('#', insertedV[elem2])
              )

            }
            insertedV <<- c()
            rvs$evidence <<- c()
            rvs$value <<- c()
            rvs$evidenceObserve <<- c()
            rvs$valueObserve <<- c()
            output$distPlot <<- renderPlot(NULL)
            nodeNames <<- selectedNodes
            EventNode <<- nodeNames[1]
            EvidenceNode <<- c()
            shapeVector<<- rep('dot',length(nodeNames))
            updateSelectInput(session,'event',choices = nodeNames)
            weight <<- 1
            value <<- 1
            output$netPlot<-renderVisNetwork({graph.custom(pruneGraph,nodeNames,shapeVector,EvidenceNode,EventNode,input$degree,input$graph_layout,weight,value)})
            updateSelectizeInput(session,'varselect',choices = nodeNames)
            updateSelectInput(session,'varshape',choices = c( "dot","square", "triangle", "box", "circle", "star",
                                                              "ellipse", "database", "text", "diamond"))
            updateSelectInput(session,'varshape2',choices = c( "dot","square", "triangle", "box", "circle", "star",
                                                               "ellipse", "database", "text", "diamond"))
            updateSelectInput(session,'graph_layout',choices = c("layout_nicely","layout_as_star","layout_as_tree","layout_in_circle","layout_with_sugiyama","layout_on_sphere","layout_randomly","layout_with_fr","layout_with_kk","layout_with_lgl","layout_with_mds","layout_on_grid","layout_with_graphopt","layout_with_gem","layout_with_dh"))
            updateSelectInput(session,'paramSelect',choices = nodeNames)
            graph<<-graph_from_edgelist(as.matrix(pruneGraph),directed = TRUE)
            updateSelectInput(session,"neighbornodes",choices = "")
            updateSelectInput(session,'varshape3',choices = c( "dot","square", "triangle", "box", "circle", "star",
                                                               "ellipse", "database", "text", "diamond"))
            updateSelectInput(session,'modGroup',choices = input$moduleSelection)
            updateSliderInput(session,"NumBar",min = 1, max = nlevels(DiscreteData[,nodeNames[1]]),value = nlevels(DiscreteData[,nodeNames[1]]))
          }
          else
          {
            for(elem in 1:length(inserted))
            {
              removeUI(
                selector = paste0('#', inserted[elem])
              )

            }
            inserted <<- c()
            for(elem2 in 1:length(insertedV))
            {
              removeUI(
                selector = paste0('#', insertedV[elem2])
              )

            }
            insertedV <<- c()
            rvs$evidence <<- c()
            rvs$value <<- c()
            rvs$evidenceObserve <<- c()
            rvs$valueObserve <<- c()
            output$distPlot <<- renderPlot(NULL)
            NetworkGraph <<- data.frame(directed.arcs(bn.hc.boot.average))
            nodeNames <<- names(bn.hc.boot.average$nodes)
            EventNode <<- nodeNames[1]
            EvidenceNode <<- c()
            shapeVector<<- rep('dot',length(nodeNames))
            updateSelectInput(session,'event',choices = nodeNames)
            if(type==2)
            {
              weight <<- bn.hc.boot.pruned[dim(NetworkGraph)[1],3]
              value <<- bn.hc.boot.pruned[dim(NetworkGraph)[1],3]
              output$netPlot<-renderVisNetwork({graph.custom(NetworkGraph,nodeNames,shapeVector,EvidenceNode,EventNode,input$degree,input$graph_layout,weight,value)})
            }
            else
            {
              weight <<- 1
              value <<- 1
              output$netPlot<-renderVisNetwork({graph.custom(NetworkGraph,nodeNames,shapeVector,EvidenceNode,EventNode,input$degree,input$graph_layout,weight,value)})
            }
            updateSelectInput(session,'event',choices = nodeNames)
            updateSelectizeInput(session,'varselect',choices = nodeNames)
            updateSelectInput(session,'varshape',choices = c( "dot","square", "triangle", "box", "circle", "star",
                                                              "ellipse", "database", "text", "diamond"))
            updateSelectInput(session,'varshape2',choices = c( "dot","square", "triangle", "box", "circle", "star",
                                                               "ellipse", "database", "text", "diamond"))
            updateSelectInput(session,'graph_layout',choices = c("layout_nicely","layout_as_star","layout_as_tree","layout_in_circle","layout_with_sugiyama","layout_on_sphere","layout_randomly","layout_with_fr","layout_with_kk","layout_with_lgl","layout_with_mds","layout_on_grid","layout_with_graphopt","layout_with_gem","layout_with_dh"))
            updateSelectInput(session,'paramSelect',choices = nodeNames)
            graph<<-graph_from_edgelist(as.matrix(NetworkGraph),directed = TRUE)
            updateSelectInput(session,"neighbornodes",choices = "")
            updateSelectInput(session,'varshape3',choices = c( "dot","square", "triangle", "box", "circle", "star",
                                                               "ellipse", "database", "text", "diamond"))
            updateSelectInput(session,'modGroup',choices = names(communities))
            updateSliderInput(session,"NumBar",min = 1, max = nlevels(DiscreteData[,nodeNames[1]]),value = nlevels(DiscreteData[,nodeNames[1]]))
          }
        },error=function(e){
          shinyalert(toString(e), type = "error")
        })
      }
    })
    tooltip(session)
  })
  observeEvent(input$current_node_id,{
    if(reset==2)
    {
      tryCatch({
        if(!is.null(input$current_node_id))
        {
          if(input$degreeN>1)
          {
            nlist<<-ego(graph,input$degreeN,nodes = input$current_node_id, mode = c("all", "out", "in"),mindist = 0)
            nlistP<<-ego(graph,input$degreeN-1,nodes = input$current_node_id, mode = c("all", "out", "in"),mindist = 0)
            diffList<<-setdiff(nlist[[1]]$name,nlistP[[1]]$name)
            updateSelectInput(session,"neighbornodes",choices = diffList)
          }
          else
          {
            nlist<<-ego(graph,input$degreeN,nodes = input$current_node_id, mode = c("all", "out", "in"),mindist = 0)
            updateSelectInput(session,"neighbornodes",choices = setdiff(nlist[[1]]$name,input$current_node_id))
          }

        }
      },error=function(e){
        shinyalert(toString(e), type = "error")
      })
    }
    tooltip(session)
  })
  observeEvent(input$Acurrent_node_id,{
    if(assocReset==2)
    {
      tryCatch({
        if(!is.null(input$Acurrent_node_id))
        {
          if(input$AdegreeN>1)
          {
            nlist<<-ego(Agraph,input$AdegreeN,nodes = input$Acurrent_node_id, mode = c("all", "out", "in"),mindist = 0)
            nlistP<<-ego(Agraph,input$AdegreeN-1,nodes = input$Acurrent_node_id, mode = c("all", "out", "in"),mindist = 0)
            diffList<<-setdiff(nlist[[1]]$name,nlistP[[1]]$name)
            updateSelectInput(session,"Aneighbornodes",choices = diffList)
          }
          else
          {
            nlist<<-ego(Agraph,input$AdegreeN,nodes = input$Acurrent_node_id, mode = c("all", "out", "in"),mindist = 0)
            updateSelectInput(session,"Aneighbornodes",choices = setdiff(nlist[[1]]$name,input$Acurrent_node_id))
          }

        }
      },error=function(e){
        shinyalert(toString(e), type = "error")
      })
    }
    tooltip(session)
  })
  observeEvent(input$Bcommunities,{
    tryCatch({
      if(reset==2)
      {
        communities<<-custom.Modules(NetworkGraph,input$moduleAlgo)
        names(communities)<<-paste("Module",c(1:length(communities)),sep=" ")
        lengthCom<<-c()
        for(n in names(communities))
        {
          lengthCom<<-c(lengthCom,length(communities[[n]]))
        }
        lengthCom<<-order(lengthCom,decreasing = T)
        names(lengthCom)<<-paste("Module",c(1:length(communities)),sep=" ")
        updateSelectInput(session,"moduleSelection",choices = c("graph",names(communities)))
        updateSelectInput(session,'modGroup',choices = names(communities))
        shinyalert("Module detection successfull",type="success")
      }
    },error=function(e){
      shinyalert("Module detection failed",type="error")
      updateSelectInput(session,"moduleSelection",choices = "graph")
      updateSelectInput(session,'modGroup',choices = "")
    })
    tooltip(session)
  })
  observeEvent(input$group3,{
    if(reset==2)
    {
      if(input$modGroup!="")
      {
        tryCatch({
          selectedNodes<<-communities[[lengthCom[input$modGroup]]]
          shapeVector[which(nodeNames %in% selectedNodes)] <<- input$varshape3
          for(elem in inserted)
          {
            EvidenceNode = c(EvidenceNode,input[[elem]])
          }
          if(sanity==1)
          {
            EventNode = nodeNames[1]
            sanity=sanity + 1
          }
          else
          {
            EventNode = input$event
          }
          output$netPlot<-renderVisNetwork({graph.custom(NetworkGraph,nodeNames,shapeVector,EvidenceNode,EventNode,input$degree,input$graph_layout,weight,value)})
          updateSelectInput(session,"neighbornodes",choices = "")
        },error = function(e){
          shinyalert(toString(e), type = "error")

        })
      }
    }
    tooltip(session)
  })
  observeEvent(input$degree,{
    if(reset==2)
    {
      tryCatch({
        for(elem in inserted)
        {
          EvidenceNode = c(EvidenceNode,input[[elem]])
        }
        if(sanity==1)
        {
          EventNode = nodeNames[1]
          sanity=sanity + 1
        }
        else
        {
          EventNode = input$event
        }
        output$netPlot<-renderVisNetwork({graph.custom(NetworkGraph,nodeNames,shapeVector,EvidenceNode,EventNode,input$degree,input$graph_layout,weight,value)})
        updateSelectInput(session,"neighbornodes",choices = "")
      },error = function(e){
        shinyalert(toString(e), type = "error")

      })
    }
    tooltip(session)
  })
  observeEvent(input$Adegree,{
    if(assocReset==2)
    {
      output$assocPlot<-renderVisNetwork({graph.custom.assoc(assocNetworkprune,unique(c(assocNetworkprune[,1],assocNetworkprune[,2])),input$Adegree,input$Agraph_layout,shapeVectorAssoc)})
      updateSelectInput(session,"Aneighbornodes",choices = "")
    }
    tooltip(session)
  })
  observeEvent(input$graph_layout,{
    if(reset==2)
    {
      tryCatch({
        for(elem in inserted)
        {
          EvidenceNode = c(EvidenceNode,input[[elem]])
        }
        if(sanity==1)
        {
          EventNode = nodeNames[1]
          sanity=sanity + 1
        }
        else
        {
          EventNode = input$event
        }
        output$netPlot<-renderVisNetwork({graph.custom(NetworkGraph,nodeNames,shapeVector,EvidenceNode,EventNode,input$degree,input$graph_layout,weight,value)})
        updateSelectInput(session,"neighbornodes",choices = "")
      },error = function(e){
        shinyalert(toString(e), type = "error")

      })
    }
    tooltip(session)
  })
  observeEvent(input$Agraph_layout,{
    if(assocReset==2)
    {
      output$assocPlot<-renderVisNetwork({graph.custom.assoc(assocNetworkprune,unique(c(assocNetworkprune[,1],assocNetworkprune[,2])),input$Adegree,input$Agraph_layout,shapeVectorAssoc)})
      updateSelectInput(session,"Aneighbornodes",choices = "")
    }
    tooltip(session)
  })
  observeEvent(input$graphBtn,{
    if(reset==2)
    {
      tryCatch({
        for(elem in inserted)
        {
          EvidenceNode = c(EvidenceNode,input[[elem]])
        }
        if(sanity==1)
        {
          EventNode = nodeNames[1]
          sanity=sanity + 1
        }
        else
        {
          EventNode = input$event
        }
        output$netPlot<-renderVisNetwork({graph.custom(NetworkGraph,nodeNames,shapeVector,EvidenceNode,EventNode,input$degree,input$graph_layout,weight,value)})
        updateSelectInput(session,"neighbornodes",choices = "")
      },error = function(e){
        shinyalert(toString(e), type = "error")

      })
    }
    tooltip(session)
  })
  observeEvent(input$graphBtn2,{
    if(assocReset==2)
    {
      tryCatch({
        assocNetworkprune<<- assocNetwork[which(assocNetwork[,3]>input$threshold),]
        output$assocPlot<-renderVisNetwork({graph.custom.assoc(assocNetworkprune,unique(c(assocNetworkprune[,1],assocNetworkprune[,2])),input$Adegree,input$Agraph_layout,shapeVectorAssoc)})
        updateSelectInput(session,"Aneighbornodes",choices = "")
      },error = function(e){
        shinyalert(toString(e), type = "error")

      })
    }
    tooltip(session)
  })
  observeEvent(input$group,{
    if(reset==2)
    {
      tryCatch({
        shapeVector[which(nodeNames %in% input$varselect)] <<- input$varshape
        for(elem in inserted)
        {
          EvidenceNode = c(EvidenceNode,input[[elem]])
        }
        if(sanity==1)
        {
          EventNode = nodeNames[1]
          sanity=sanity + 1
        }
        else
        {
          EventNode = input$event
        }
        output$netPlot<-renderVisNetwork({graph.custom(NetworkGraph,nodeNames,shapeVector,EvidenceNode,EventNode,input$degree,input$graph_layout,weight,value)})
        updateSelectInput(session,"neighbornodes",choices = "")
      },error = function(e){
        shinyalert(toString(e), type = "error")

      })
    }
    tooltip(session)
  })
  observeEvent(input$Agroup,{
    if(assocReset==2)
    {
      shapeVectorAssoc[which(unique(c(assocNetworkprune[,1],assocNetworkprune[,2])) %in% input$Avarselect)] <<- input$Avarshape
      output$assocPlot<-renderVisNetwork({graph.custom.assoc(assocNetworkprune,unique(c(assocNetworkprune[,1],assocNetworkprune[,2])),input$Adegree,input$Agraph_layout,shapeVectorAssoc)})
      updateSelectInput(session,"Aneighbornodes",choices = "")
    }
    tooltip(session)
  })
  observeEvent(input$Agroup2,{
    if(assocReset==2)
    {
      shapeVectorAssoc<<-shapeVectorAssoc[1:length(unique(c(assocNetworkprune[,1],assocNetworkprune[,2])))]
      shapeVectorAssoc[eval(parse(text = input$Avarselectvector))] <<- input$Avarshape2
      output$assocPlot<-renderVisNetwork({graph.custom.assoc(assocNetworkprune,unique(c(assocNetworkprune[,1],assocNetworkprune[,2])),input$Adegree,input$Agraph_layout,shapeVectorAssoc)})
      updateSelectInput(session,"Aneighbornodes",choices = "")
    }
    tooltip(session)
  })
  observeEvent(input$group2,{
    if(reset==2)
    {
      tryCatch({
        shapeVector<<-shapeVector[1:length(nodeNames)]
        shapeVector[eval(parse(text = input$varselectvector))] <<- input$varshape2
        for(elem in inserted)
        {
          EvidenceNode = c(EvidenceNode,input[[elem]])
        }
        if(sanity==1)
        {
          EventNode = nodeNames[1]
          sanity=sanity + 1
        }
        else
        {
          EventNode = input$event
        }
        output$netPlot<-renderVisNetwork({graph.custom(NetworkGraph,nodeNames,shapeVector,EvidenceNode,EventNode,input$degree,input$graph_layout,weight,value)})
        updateSelectInput(session,"neighbornodes",choices = "")
      },error = function(e){
        shinyalert(toString(e), type = "error")

      })
    }
    tooltip(session)
  })


  #homeIntroduction Event
  observeEvent(input$homeIntro,{
      print(input$sidebarMenu)
      if(input$sidebarMenu == "Home")
      {introjs(session, options = list(steps = homeHelp))}
      else if(input$sidebarMenu == "Structure")
      {
        print(input$control_tabs)
        if(input$control_tabs == "Data")
        {
          introjs(session, options = list(steps = dataHelp))
        }
        else if(input$control_tabs == "Graph")
        introjs(session, options = list(steps = graphHelp))

      }
    })
  output$dashboard<-downloadHandler(
    filename = "customDashboard.tar.gz",
    content = function(filename){
      if(reset==2)
      {
        tar(filename,"customDashboard", compression = 'gzip', tar=Sys.getenv("tar"))
      }
    }
  )
})
