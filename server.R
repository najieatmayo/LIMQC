
function(input, output, session){

  myround <- function(x){
    y <- x
    x <- abs(x)
    if(x> 10){
      x <- round(x, 0)
    }else if(x> 1){
      x <- round(x, 1)
    } else if (x > 0.1) {
      x <- round(x, 2)
    }  else if (x > 0.01) {
      x <- round(x, 3)
    }  else if (x > 0.001) {
      x <- round(x, 4)
    }
    return(sign(y)*x)
  }

  shiny:::flushReact()
  
  rv <- reactiveValues(default=TRUE,
                       data = pt)
  
  observeEvent({if (isTruthy(input$rb == "Upload Your Own Data")) {return(TRUE)}
    else {
      return()}}, {

    rv$default <- FALSE
  }, priority = 1)
  
  
  observe({
    
    req(!rv$default)
    req(input$inFile)
    print(input$rb)
    if(grep("\\.csv", input$inFile$datapath)){
      test <- read.csv(input$inFile$datapath, header = T, stringsAsFactors=FALSE)
    } else {
      test <- read.table(input$inFile$datapath, header = T, sep = "\t", stringsAsFactors=FALSE)
    }
    
    test$pdate <- as.Date(test$pdate , format = "%m/%d/%Y")
    req(input$dateRange)
    rv$data <- test %>% filter(pdate <= input$dateRange[2] & pdate >= input$dateRange[1]) %>% arrange(pdate)
  })
  
  
  observeEvent({if (input$rb == "Demo Data") TRUE
    else return()}, {
    print(input$rb)
    
    rv$data <- pt ##%>% filter(pdate <= input$dateRange[2] & pdate >= input$dateRange[1]) %>% arrange(pdate)
    rv$default <- TRUE
    
    shinyjs::reset('inFile')
  }, priority = -1)
  
  mydata <- reactive({
    ppt <- rv$data
    req(input$dateRange)
    
    out <- ppt %>% filter(pdate <= input$dateRange[2] & pdate >= input$dateRange[1]) %>% arrange(pdate)
    
    as.data.frame(out)
  })
  
  
  STypes2 <- reactive({
    ttypes <- as.character(unique(mydata()$sample.type))
    names(ttypes) <- ttypes
    
    return(ttypes)
  })
  
  col2display2 <- reactive({
    return(names(mydata()))
  })
  
  metricCols1 <-  reactive({ ## continouts
    return(names(mydata())[which(sapply(mydata(), function(x) is.numeric(x)))])
  })
  metricCols2 <-  reactive({ ## catgorical excluding sample.ID
    catnames <- setdiff(names(mydata())[which(sapply(mydata(), function(x) is.character(x)))], "sample.ID")
    return(catnames)
  })
  
  metricCols <- reactive({ ## all
    return(c(metricCols1(), metricCols2()))
  })
  
  observe({
    
    updatePickerInput(session= session, inputId="ind_sample_groups", choices = STypes2(), selected= STypes2())
    updatePickerInput(session = session, inputId="col2show", choices = col2display2(), selected= col2display2())
    
    updatePickerInput(session= session, inputId="ind_sample_groupsQC", choices = STypes2(), selected= STypes2()[1])
    updateSelectInput(session = session, inputId="CorrST", choices = STypes2(), selected= STypes2()[1])
    updateSelectInput(session = session, inputId="sampleTG", choices = STypes2(), selected= STypes2()[1])
    updateSelectInput(session = session, inputId="sampleTG2", choices = STypes2(), selected= STypes2()[1])
    
    ##updateSelectInput(session = session, inputId="varIntersted", choices = c("age", "bmi", "version"))
    
    updatePickerInput(session = session, inputId="varIntersted", choices = c(metricCols1(), metricCols2()), selected= c(metricCols1(), metricCols2())[1])
    
    updatePickerInput(session = session, inputId="ind_metric", choices = metricCols1(), selected= metricCols1()[1])
    updateSelectInput(session = session, inputId="metric_by", choices = metricCols1(), selected= metricCols1()[1])
    
    updateSelectInput(session = session, inputId="by_group2", choices = metricCols2(), selected= metricCols2()[1])
    updateSelectInput(session = session, inputId="by_group", choices = metricCols2(), selected= metricCols2()[1])
    updateSelectInput(session = session, inputId="metric_x", choices = metricCols1(), selected= metricCols1()[1])
    updateSelectInput(session = session, inputId="metric_y", choices = metricCols1(), selected= metricCols1()[2])
    updateSelectInput(session = session, inputId="px", choices = metricCols1(), selected= metricCols1()[1])
    updateSelectInput(session = session, inputId="py", choices = metricCols1(), selected= metricCols1()[2])
    updateSelectInput(session = session, inputId="pz", choices = metricCols1(), selected= metricCols1()[3])
    
    
    updatePickerInput(session = session, inputId="ConditioningVariables", choices = metricCols1(), selected= NULL)
    updatePickerInput(session = session, inputId="ConditioningVariables2", choices = metricCols2(), selected= NULL)
    updatePickerInput(session = session, inputId="ConditioningVariablesR", choices = metricCols1(), selected= NULL)
    updatePickerInput(session = session, inputId="ConditioningVariables2R", choices = metricCols2(), selected= NULL)
    
  })
  
  observe({
    req(input$varIntersted)
    updatePickerInput(session = session, inputId="AgainstVars", choices = c(metricCols1(), metricCols2()), selected= setdiff(c(metricCols1(), metricCols2()), input$varIntersted))
  })
  mydataR<- reactive({
    
    stype <- input$ind_sample_groups
    mydata() %>% filter(sample.type %in% stype) %>% arrange(pdate)
    
  })
  
  mydataRQC <- reactive({
    req(input$ind_sample_groupsQC)
    stype <- input$ind_sample_groupsQC
    mydata() %>% filter(sample.type %in% stype) %>% arrange(pdate)
    
  })
  
  ### get the number of runs
  nruns <- reactive({
    length(unique(mydataR2show()$run.ID))
  })
  ### get the number of samples
  nsample <- reactive({
    nrow(mydataR2show())
  })
  
  ### get the number of qc pass samples
  nsamplePass <- reactive({
    nrow(mydataRQC() %>% filter(Sample.QC %in% c("Pass", "")))
  })
  
  ### get the number of qc warning samples
  nsampleWarn<- reactive({
    nrow(mydataRQC() %>% filter(Sample.QC %nin% c("Pass", "")))
  })
  
  mydata2s <- reactive({
    dt <- mydata()[, c(metricCols1(), metricCols2())]
    ## remove the single items from summary
    factorCols <- NULL
    for(coli in metricCols2()){
      dt[, coli] <- as.factor(dt[, coli])
      nlv <- nlevels(dt[, coli])
      if(nlv < nruns()) factorCols <- c(factorCols, coli)
    }
    
    dt[, c(metricCols1(), factorCols)]
  })
  
  output$arsTable <- renderTable({
    as.data.frame(summary(tableby(~., data = mydata2s(), control=tableby.control(numeric.stats=c("N", "Nmiss", "medianq1q3", "range"))), text = "html"))
  }, sanitize.text.function = function(x) x)
  
  assoctable <- reactive({
    
    x <- mydata()[, input$varIntersted]
    contid <- which(sapply(mydata()[input$AgainstVars], function(x) is.numeric(x)))
    catid <- which(sapply(mydata()[input$AgainstVars], function(x) is.character(x)))
    if(length(contid)>0){
      y1 <- mydata()[, input$AgainstVars[contid]]
    }
    if(length(catid)>0){
      y2 <- mydata()[, input$AgainstVars[catid]]
    }
    if(is.numeric(x)){## var of interest is a cont
      out1 <- out2 <- NULL
      if(length(contid)>0){ ## correlation with other cont
        ps <- NULL; spr <- NULL
        for(conti in 1:length(contid)){
          yi <- y1[,conti]
          cor1 <- cor.test(x, yi, method = "spearman", exact = FALSE)
          ps <- c(ps, -log10(cor1$p.value))
          spr <- c(spr, cor1$estimate) 
        }
        out1 <- data.frame(Against = colnames(y1), Spearman_rho = spr, ANOVA_F = NA, Chisq = NA, neglog10pvalue=ps)
      }
      
      if(length(catid)>0){ ## ANOVA vs cats
        ps <- NULL; anovaF <- NULL
        for(cati in 1:length(catid)){
          
          dt <- data.frame(x = x, y = y2[, cati])
          dt <- data.frame(dt[complete.cases(dt),])
          dt$y <- droplevels(dt$y)
          if(length(levels(dt$y))>1){
            myfit <- aov(x~as.factor(y), data = dt)
            ps <- c(ps, -log10(summary(myfit)[[1]][["Pr(>F)"]][1]))
            anovaF <- c(anovaF, summary(myfit)[[1]][["F value"]][1])
          } else {
            ps <- c(ps , NA)
            anovaF <- c(anovaF, NA)
          }
        }
        out2 <- data.frame(Against = colnames(y2), Spearman_rho = NA, ANOVA_F = anovaF, Chisq = NA, neglog10pvalue=ps)
      }
      out <- rbind(out1, out2)
    } else if(is.character(x)){ ## var of interest is a cat
      out1 <- out2 <- NULL
      if(length(contid)>0){ ## ANOVA vs cont
        ps <- NULL; anovaF <- NULL
        for(conti in 1:length(contid)){
          
          dt <- data.frame(x = x, y = y1[, conti] )
          dt <- data.frame(dt[complete.cases(dt),])
          dt$x <- droplevels(dt$x)
          if(length(levels(dt$x))>1){
            myfit <- aov(y~as.factor(x), data = dt)
            ps <- c(ps,  -log10(summary(myfit)[[1]][["Pr(>F)"]][1]))
            anovaF <- c(anovaF, summary(myfit)[[1]][["F value"]][1])
          }else {
            ps <- c(ps , NA)
            anovaF <- c(anovaF, NA)
          }
        }
        out1 <- data.frame(Against = colnames(y1), Spearman_rho = NA, ANOVA_F = anovaF, Chisq = NA, neglog10pvalue=ps)
      }
      
      if(length(catid)>0){ ## Chisq vs other cat
        ##browser()
        ps <- NULL; X2 <- NULL
        for(cati in 1:length(catid)){
          y2i <- y2[, cati] 
          tbl <- table(x, y2i)
          ps <- c(ps, -log10( ifelse(any(tbl)<5, fisher.test(tbl, simulate.p.value=TRUE)$p.value, chisq.test(tbl)$p.value)))
          est <- fisher.test(tbl, simulate.p.value=TRUE)$estimate
          X2 <- c(X2, chisq.test(tbl)$stati)
        }
        ##Pearson's Chi-squared test with Yates' continuity correction
        
        out2 <- data.frame(Against = colnames(y2), Spearman_rho = NA, ANOVA_F = NA, Chisq = X2, neglog10pvalue=ps)
      }
      out <- rbind(out1, out2)
    }
    
    out %>% arrange(desc(neglog10pvalue)) %>%
      datatable(caption = 'Association table: selected variable against others,  Columns are Spearman Rho Correlation Coefficient for continuous variables, ANOVA F statistics for categorical varaibles, Chisq, -log10 of pvalues of the tests (correlation test, ANOVA, Chisq or Fisher Exact test if cell number less than 5 where they apply') %>%
      formatRound(columns=c('Spearman_rho', 'ANOVA_F', 'Chisq', 'neglog10pvalue'), digits=2)
  })
  
  
  output$assocTable <- DT::renderDataTable(
    assoctable()
  )

    
  ## QC
  qcdata <- reactive({
    dt <- mydataRQC()
    
    lddate <- max(dt$pdate, na.rm = T)
    ld <- dt %>% mutate() %>% filter(Sample.QC %nin% c("Pass", "")) %>% filter(pdate == lddate ) %>% group_by(Sample.QC) %>% tally()
    colnames(ld) <- c("Sample.QC", "RDay")
    
    lw <- dt %>% mutate(weeks = round((max(dt$pdate, na.rm = T) - dt$pdate) /dweeks(1))) %>% filter(Sample.QC %nin% c("Pass", "")) %>% filter(weeks == min(weeks, na.rm = T) ) %>% group_by(Sample.QC) %>% tally()
    colnames(lw) <- c("Sample.QC", "RWeek")
    
    lastm <- month(lddate); lasty <- year(lddate)
    lm <- dt %>% mutate(Month = month(pdate), YEAR = year(pdate)) %>% filter(Sample.QC %nin% c("Pass", "")) %>% filter(Month == lastm & YEAR == lasty) %>% group_by(Sample.QC) %>% tally()
    colnames(lm) <- c("Sample.QC", "RMonth")
    
    ly <- dt %>% mutate(Month = month(pdate), YEAR = year(pdate)) %>% filter(Sample.QC %nin% c("Pass", "")) %>% filter(YEAR == lasty) %>% group_by(Sample.QC) %>% tally()
    colnames(ly) <- c("Sample.QC", "RYear")
    
    all <- dt %>%  filter(Sample.QC %nin% c("Pass", "")) %>% group_by(Sample.QC) %>% tally()
    colnames(all) <- c("Sample.QC", "Alive")
    
    ## merge all
    out <- full_join(ld, full_join(lw, full_join(lm, full_join(ly, all))))
    out[is.na(out)] <- 0
    return(out)
  })
  ## stuff for dashboard tab
  output$vbox1 <- renderValueBox({ 
    valueBox(
      subtitle ="Number of runs",
      value = nruns(),
      color = "green",
      icon = icon("empire"))
  })
  
  output$vbox2 <- renderValueBox({
    valueBox(
      subtitle = "Number of samples",
      value = nsample(),
      color = "blue",
      icon = icon("bullseye"))
  })
  
  output$vbox3 <- renderValueBox({
    valueBox(
      subtitle = "QC pass samples",
      value = nsamplePass(),
      color = "green",
      icon = icon("empire"))
  })
  output$vbox4 <- renderValueBox({
    valueBox(
      subtitle = "QC warning samples",
      value = nsampleWarn(),
      color = "orange",
      icon = icon("bullseye"))
  })
  
  output$TVolume <- renderPlotly({
    V2plot <- mydataRQC() %>% mutate(Month = month(pdate), YEAR = year(pdate)) %>% filter(!is.na(pdate))
    
    V2plot <- V2plot %>%
      group_by(Month, YEAR) %>%
      dplyr::tally()
    
    V2plot$pdate <- as.Date(paste0(V2plot$YEAR,"-",  V2plot$Month, "-01" ),"%Y-%m-%d")
    V2plot <- arrange(V2plot, pdate)
    
    xt <- list(title = "Date")
    yt <- list(title = "Test Volume")
    
    plot_ly(V2plot, x = ~pdate, y = ~n, type = "bar") %>%
      layout(xaxis = xt, yaxis = yt) 
  })
  
  output$Wrate <- renderPlotly({
    
    V2plot <- mydataRQC() %>% mutate(Month = month(pdate), YEAR = year(pdate), QC = Sample.QC %in% c("Pass", "")) %>% filter(!is.na(pdate))
    
    dt0 <-  V2plot %>%
      group_by(Month, YEAR) %>% tally()
    
    V2plot <- V2plot %>% filter(QC == FALSE) %>%
      group_by(Month, YEAR) %>% count()
    
    
    V2plot <- left_join(dt0, V2plot, by = c("Month", "YEAR"))
    
    V2plot$n.y[is.na(V2plot$n.y)] <- 0
    V2plot$Wrate <- V2plot$n.y / V2plot$n.x *100
    V2plot$pdate <- as.Date(paste0(V2plot$YEAR,"-",  V2plot$Month, "-01" ),"%Y-%m-%d")
    V2plot <- arrange(V2plot, pdate)
    
    ## cal the rate
    xt <- list(title = "Date")
    yt <- list(title = "Warning Rate (%)")
    
    plot_ly(V2plot, x = ~pdate, y = ~Wrate, mode = 'markers') %>%
      layout(xaxis = xt, yaxis = yt) 
    
  })
  
  
  output$display.qcda.tab <- DT::renderDataTable(
    datatable(data = qcdata(), 
              extensions = "Buttons", 
              options = list(dom = "Blfrtip", 
                             buttons =
                               list("copy", "print", list(
                                 extend = "collection",
                                 buttons = c("csv", "excel", "pdf"),
                                 text = "Download")
                               ) ## end of buttons customization
                             ## customize the length menu
                             , lengthMenu = list( c(5, 10, 20, -1) # declare values
                                                  , c(5, 10, 20, "All") # declare titles
                             ) # end of lengthMenu customization
                             , pageLength = 5
              ) # end of options
    )# end of datatables
    
  )
  
  mydataR2show <- reactive({
    req(input$col2show)
    dt <- mydataR() %>% dplyr::select(input$col2show)

    if(length(input$ConditioningVariables2R)){
      
      for(i in 1: length(input$ConditioningVariables2R)){
        dt <- dt %>% filter(get(input$ConditioningVariables2R[i]) %in% c(input[[input$ConditioningVariables2R[i]]]))
      }
    }
    if(length(input$ConditioningVariablesR)){
      for(i in 1: length(input$ConditioningVariablesR)){
        dt <- dt %>% filter(get(input$ConditioningVariablesR[i]) > (input[[input$ConditioningVariablesR[i]]])[1] & get(input$ConditioningVariablesR[i]) < (input[[input$ConditioningVariablesR[i]]])[2])
      }
    }
    
    dt
    
  })
  
  ColsN <- reactive({
    ##which(apply(, 2, FUN = is.numeric))
    which(sapply(mydataR2show(), function(x) is.numeric(x)))
  })
  
  vardfR <- reactive({
    dt <- data.frame(varnames = metricCols1(), 
                     varmin = rep(NA, length(metricCols1())), 
                     varmax = rep(NA, length(metricCols1())), 
                     varinit1= rep(NA, length(metricCols1())),
                     varinit2= rep(NA, length(metricCols1())))
    
    dt$varmin <- as.vector(apply(mydataR()[, metricCols1()], 2, FUN = min, na.rm = T))
    dt$varmax <- as.vector(apply(mydataR()[, metricCols1()], 2, FUN = max, na.rm = T))
    dt$varinit1 <- as.vector(apply(mydataR()[, metricCols1()], 2, FUN = quantile, probs = 0.25, na.rm = T))
    dt$varinit2 <- as.vector(apply(mydataR()[, metricCols1()], 2, FUN = quantile, probs = 0.75, na.rm = T))
    dt$step <- as.vector((dt$varmax - dt$varmin)/100)
    dt
  })
  
  output$ControlWidgetsofConditioningVariablesR <- renderUI({
    
    if (is.null(input$ConditioningVariablesR)){
      return() 
    } else {
      varnames <-  metricCols1()
      selvarnames = sort(input$ConditioningVariablesR)
      selpos = sapply(selvarnames,function(x) which(varnames==x))
      
      # create a taglist of dynamic widgets
      ListofDynamicWidgets <- lapply(selpos, function(x){sliderInput(as.character(vardfR()[x,1]),
                                                                     as.character(vardfR()[x,1]),
                                                                     myround(vardfR()[x,2]),myround(vardfR()[x,3]),
                                                                     c(myround(vardfR()[x,4]), myround(vardfR()[x, 5])),
                                                                     myround(vardfR()[x, 6]))})
      do.call(tagList, ListofDynamicWidgets)
    }
  })
  
  catlistR <- reactive({

    varnames <-  metricCols2()
    outlist <- list()
    for(i in 1:length(varnames)){
      varlevels <- as.character(unique(mydataR()[, varnames[i]]))
      outlist[[varnames[i]]] <- varlevels
    }
    
    outlist
  })
  
  output$ControlWidgetsofConditioningVariables2R <- renderUI({
    
    if (is.null(input$ConditioningVariables2R)){

      return() 
    } else {
      varnames <-  metricCols2()
      selvarnames = sort(input$ConditioningVariables2R)
      selpos = sapply(selvarnames,function(x) which(varnames==x))
      
      # create a taglist of dynamic widgets
      ListofDynamicWidgets <- lapply(selpos, function(x){selectizeInput(inputId=names(catlistR())[x], 
                                                                        label=names(catlistR())[x], 
                                                                        choices = catlistR()[[x]],
                                                                        selected = NULL, 
                                                                        multiple = TRUE,
                                                                        options = NULL)})
      do.call(tagList, ListofDynamicWidgets)
    }
  })
  
  vardf <- reactive({
    dt <- data.frame(varnames = metricCols1(), 
                     varmin = rep(NA, length(metricCols1())), 
                     varmax = rep(NA, length(metricCols1())), 
                     varinit1= rep(NA, length(metricCols1())),
                     varinit2= rep(NA, length(metricCols1())))
    
    dt$varmin <- as.vector(apply(mydata()[, metricCols1()], 2, FUN = min, na.rm = T))
    dt$varmax <- as.vector(apply(mydata()[, metricCols1()], 2, FUN = max, na.rm = T))
    dt$varinit1 <- as.vector(apply(mydata()[, metricCols1()], 2, FUN = quantile, probs = 0.25, na.rm = T))
    dt$varinit2 <- as.vector(apply(mydata()[, metricCols1()], 2, FUN = quantile, probs = 0.75, na.rm = T))
    dt$step <- as.vector((dt$varmax - dt$varmin)/100)
    dt
  })
  
  output$ControlWidgetsofConditioningVariables <- renderUI({
    
    if (is.null(input$ConditioningVariables)){
      return() 
    } else {
      varnames <-  metricCols1()
      selvarnames = sort(input$ConditioningVariables)
      selpos = sapply(selvarnames,function(x) which(varnames==x))
      
      # create a taglist of dynamic widgets
      ListofDynamicWidgets <- lapply(selpos, function(x){sliderInput(as.character(vardf()[x,1]),
                                                                     as.character(vardf()[x,1]),
                                                                     myround(vardf()[x,2]),myround(vardf()[x,3]),
                                                                     c(myround(vardf()[x,4]), myround(vardf()[x, 5])),
                                                                     myround(vardf()[x, 6]))})
      do.call(tagList, ListofDynamicWidgets)
    }
  })
  
  catlist <- reactive({
    varnames <-  metricCols2()
    
    outlist <- list()
    for(i in 1:length(varnames)){
      varlevels <- as.character(unique(mydata()[, varnames[i]]))
      outlist[[varnames[i]]] <- varlevels
    }
    
    outlist
  })
  
  output$ControlWidgetsofConditioningVariables2 <- renderUI({
    
    if (is.null(input$ConditioningVariables2)){

      return() 
    } else {
      varnames <-  metricCols2()
      
      selvarnames = sort(input$ConditioningVariables2)
      selpos = sapply(selvarnames,function(x) which(varnames==x))
      
      # create a taglist of dynamic widgets
      ListofDynamicWidgets <- lapply(selpos, function(x){selectizeInput(inputId=names(catlist())[x], 
                                                                        label=names(catlist())[x], 
                                                                        choices = catlist()[[x]],
                                                                        selected = NULL, 
                                                                        multiple = TRUE,
                                                                        options = NULL)})
      do.call(tagList, ListofDynamicWidgets)
    }
  })
  
  
  ## stuff for graph : trending. 
  
  selectMetrics  <- reactive({
    
    dat2plot <- mydata()
    if(length(input$ConditioningVariables2)){
      for(i in 1: length(input$ConditioningVariables2)){
        dat2plot <- dat2plot %>% filter(get(input$ConditioningVariables2[i]) %in% c(input[[input$ConditioningVariables2[i]]]))
      }
    }
    if(length(input$ConditioningVariables)){
      for(i in 1: length(input$ConditioningVariables)){
        
        dat2plot <- dat2plot %>% filter(get(input$ConditioningVariables[i]) > (input[[input$ConditioningVariables[i]]])[1] & get(input$ConditioningVariables[i]) < (input[[input$ConditioningVariables[i]]])[2])
      }
    }
    dat2plot <- dat2plot %>% dplyr::select(pdate, run.ID, sample.ID, input$ind_metric, sample.type)
    colnames(dat2plot) <- c("pdate", "run.ID", "sample.ID", "metric", "sample.type")
    dat2plot
  })
  
  
  ## stuffs for graph : coorelation  
  
  select2Metrics  <- reactive({
    req(input$CorrST)
    stype <- input$CorrST
    
    dat2plot <- mydata()  %>% filter(sample.type %in% stype) %>% dplyr::select(input$metric_x, input$metric_y, sample.type, input$lab_metric)
    colnames(dat2plot) <- c("Metric1", "Metric2", "sample.type", input$lab_metric)
    dat2plot
  })
  
  
  ## stuffs for graph : comparison  
  
  selectbyMetrics  <- reactive({
    req(input$sampleTG)
    stype <- input$sampleTG
    ##if("All" %in% stype) stype = types
    dat2plot <- mydata() %>% filter(sample.type %in% stype) %>% dplyr::select(input$metric_by, input$by_group, sample.type) 
    colnames(dat2plot) <- c("Metric", "Group", "sample.type")
    dat2plot
  })
  
  
  ## stuff for data tab
  ##data table output
  
  output$display.da.tab <- DT::renderDataTable(
    datatable(data = mydataR2show(), 
              extensions = "Buttons", 
              ##style = "bootstrap",
              ##filter = list(position = 'top', clear = T, plain = F),
              options = list(dom = "Blfrtip", 
                             buttons =
                               list("copy", "print", list(
                                 extend = "collection",
                                 buttons = c("csv", "excel", "pdf"),
                                 text = "Download")
                               ) ## end of buttons customization
                             ## customize the length menu
                             , lengthMenu = list( c(5, 10, 20, -1) # declare values
                                                  , c(5, 10, 20, "All") # declare titles
                             ) # end of lengthMenu customization
                             , pageLength = 5
              ) # end of options
    )# end of datatables
    %>% formatRound(columns = ColsN(), 2)
  )
  
  output$trending <- renderPlotly({
    pal <- c("red", "blue", "green")
    xt <- list(title = "Date")
    yt <- list(title = input$ind_metric)
    
    plot_ly(selectMetrics(), x = ~pdate, y = ~metric, color = ~sample.type, text = ~paste(run.ID, sample.ID), colors = pal, type = 'scatter', mode = 'markers') %>%
      layout(xaxis = xt, yaxis = yt) 
  })
  output$datetrending <- renderPlotly({
    xt <- list(title = "Date")
    yt <- list(title = input$ind_metric)
    
    plot_ly(selectMetrics(), y = ~metric, color = ~as.factor(pdate), type = "box") %>%
      layout(xaxis = xt, yaxis = yt) %>% layout(showlegend = FALSE)
  })
  
  output$Mdatetrending <- renderPlotly({
    data2plot <- selectMetrics() %>% group_by(pdate) %>% summarise(mymedian=mean(metric, na.rm = T))
    xt <- list(title = "Date")
    yt <- list(title = input$ind_metric)
    
    plot_ly(data2plot, x = ~pdate, y = ~mymedian, type = 'scatter', mode = 'markers') %>%
      layout(xaxis = xt, yaxis = yt) 
  })
  
  
  output$runtrending <- renderPlotly({
    xt <- list(title = "Run")
    yt <- list(title = input$ind_metric)
    data2plot <- selectMetrics() %>% mutate(run.ID.bydate = paste(pdate, run.ID, sep = "_")) 
    plot_ly(data2plot, y = ~metric, color = ~as.factor(run.ID.bydate), type = "box") %>%
      layout(xaxis = xt, yaxis = yt) %>% layout(showlegend = FALSE)
  })
  
  output$Mruntrending <- renderPlotly({
    data2plot <- selectMetrics() %>% mutate(run.ID.bydate = paste(pdate, run.ID, sep = "_")) %>% group_by(run.ID.bydate) %>% summarise(mymedian=mean(metric, na.rm = T))
    xt <- list(title = "Run")
    yt <- list(title = input$ind_metric)
    
    plot_ly(data2plot, x = ~run.ID.bydate, y = ~mymedian, type = 'scatter', mode = 'markers') %>%
      layout(xaxis = xt, yaxis = yt) 
  })
  
  output$MWtrending <- renderPlotly({ ##
    # moving mean for that day and previous days (e.g. 5 represents the mean of that day and the for previous days)
    data2plot = selectMetrics() %>% filter(!is.na(metric)) %>%
      arrange(pdate) %>%
      mutate(temp.5 = rollapply(metric, width=50, FUN=mean, na.rm = T, by=1, fill=NA, align="right"))
    
    xt <- list(title = "Date")
    yt <- list(title = input$ind_metric)
    
    plot_ly(data2plot, x=~pdate, y = ~temp.5, type = 'scatter', mode = 'markers', text = ~ paste(run.ID,"/", pdate)) %>%
      layout(xaxis = xt, yaxis = yt) 
  })
  
  output$trending2 <- renderPlotly({
    plot_ly(selectMetrics(), y = ~metric, type = "histogram") %>%
      layout(yaxis = list(title = input$ind_metric)) 
    
  })
  
  output$stats <- renderPrint({
    summary(selectMetrics()$metric)
  })
  
  ## subsetted data frame - for corr plotting
  corrs.to.plot <- reactive({
    
    dt <- select2Metrics()[complete.cases(select2Metrics()),] 
    
    if(input$FilterX){
      dt <- dt %>% filter(Metric1 >= input$FX[1] & Metric1 <= input$FX[2]) 
    } 
    if(input$FilterY){
      dt <- dt %>% filter(Metric2 >= input$FY[1] & Metric2 <= input$FY[2])
    }
    dt
  })
  
  ## update UI for correlation plot filter
  observeEvent(input$FilterX, {
    min.FX <- min(corrs.to.plot()[,"Metric1"], na.rm = T)
    max.FX <- max(corrs.to.plot()[,"Metric1"], na.rm = T)
    min.FX <- ifelse(is.finite(min.FX), min.FX, -100)
    max.FX <- ifelse(is.finite(max.FX), max.FX, 100)
    
    steps <- 
      updateSliderInput(session, "FX", step = myround((max.FX - min.FX)/100), 
                        value = c(myround(min.FX), myround(max.FX)),
                        min = myround(min.FX), max = myround(max.FX))
    
  })
  observeEvent(input$FilterY, {
    min.FY <- min(corrs.to.plot()[,"Metric2"], na.rm = T)
    max.FY <- max(corrs.to.plot()[,"Metric2"], na.rm = T)
    min.FY <- ifelse(is.finite(min.FY), min.FY, -100)
    max.FY <- ifelse(is.finite(max.FY), max.FY, 100)
    ##print(min.FY)
    updateSliderInput(session, "FY", step = myround((max.FY - min.FY)/100), 
                      value = c(myround(min.FY), myround(max.FY)),
                      min = myround(min.FY), max = myround(max.FY))
    
  })
  
  
  
  output$Corr <- renderPlotly({
    xt <- list(title = input$metric_x)
    yt <- list(title = input$metric_y)
    
    pal <- c("red", "blue", "green")
    dt2plot <- corrs.to.plot()
    
    if(length( input$lab_metric) > 1) {
      textcontent <- apply(dt2plot[, input$lab_metric], 1, FUN = paste, collapse = "-")
    } else {
      textcontent <- dt2plot[, input$lab_metric]
    }
    p <- plot_ly(dt2plot, x= ~Metric1, y = ~Metric2, color = ~sample.type, colors = pal, mode = 'markers', text = ~textcontent) %>% layout(xaxis = xt, yaxis = yt)
    
    if(input$logx){
      p <- layout(p, xaxis = list(type = "log"))
    }
    if(input$logy){
      p <- layout(p, yaxis = list(type = "log"))
    }
    if(input$addtrend){
      
      if(!input$logx & !input$logy){
        x <- (dt2plot$Metric1)
        y <- (dt2plot$Metric2)
        myfit <- lm(y~x)
        p <- plot_ly(dt2plot, x= ~Metric1) %>%
          add_markers(y = ~Metric2, text = ~textcontent, showlegend = FALSE) %>% 
          layout(xaxis = xt, yaxis = yt) %>% 
          add_lines(y = ~fitted(myfit), line = list(dash="dashdot"), name="regression line")
      } else if(input$logx & !input$logy){
        x <- log(dt2plot$Metric1)
        y <- (dt2plot$Metric2)
        
        myfit <- lm(y~x)
        p <- plot_ly(x= x, y = y, mode = 'markers', text = ~textcontent) %>% layout(xaxis = xt, yaxis = yt)
        p <- add_trace(p, x = x, y = fitted(myfit), mode = 'lines', line = list(dash="dashdot"), name="regression line")
      } else if(!input$logx & input$logy){
        x <- (dt2plot$Metric1)
        y <- log(dt2plot$Metric2)
        
        myfit <- lm(y~x)
        p <- plot_ly(x= x, y = y, mode = 'markers', text = ~textcontent) %>% layout(xaxis = xt, yaxis = yt)
        p <- add_trace(p, x = x, y = fitted(myfit), mode = 'lines',  line = list(dash="dashdot"), name="regression line")
      } else {
        x <- log(dt2plot$Metric1)
        y <- log(dt2plot$Metric2)
        
        myfit <- lm(y~x)
        p <- plot_ly(x= x, y = y, mode = 'markers', text = ~textcontent) %>% layout(xaxis = xt, yaxis = yt)
        p <- add_trace(p, x = x, y = fitted(myfit), mode = 'lines', line = list(dash="dashdot"), name="regression line")
      }
      r2 <- round(summary(myfit)$r.squared, 2)
      ttext <- bquote(paste("R"^"2", "= ")~ .(r2))
      p <- p %>% layout(title = paste(ttext)) ### come back here later
    }
    p
  })
  
  output$corrstats <- renderPrint({
    if(input$addtrend){
      dt2plot <- corrs.to.plot()
      
      if(!input$logx & !input$logy){
        myfit <- lm(Metric2~Metric1, data=dt2plot)
      } else if(input$logx & !input$logy){
        x <- log(dt2plot$Metric1)
        y <- (dt2plot$Metric2)
        
        myfit <- lm(y~x)
      } else if(!input$logx & input$logy){
        x <- (dt2plot$Metric1)
        y <- log(dt2plot$Metric2)
        
        myfit <- lm(y~x)
      } else {
        x <- log(dt2plot$Metric1)
        y <- log(dt2plot$Metric2)
        
        myfit <- lm(y~x)
      }
      summary(myfit)  
    }
    
  })
  
  output$Comp <- renderPlotly({
    groups <- unique(selectbyMetrics()$Group)
    
    
    if(length(groups) >2){
      p <- plot_ly(alpha = 0.6) %>%
        add_histogram(x = selectbyMetrics()$Metric[selectbyMetrics()$Group == groups[1]], name = groups[1] )
      for(i in 2:length(groups)){
        p <- p %>% add_histogram(x = selectbyMetrics()$Metric[selectbyMetrics()$Group == groups[i]], name = groups[i]) %>%
          layout(barmode = "overlay")
      }
      p
    } else {
      plot_ly(alpha = 0.6) %>%
        add_histogram(x = selectbyMetrics()$Metric[selectbyMetrics()$Group == groups[1]] , name = groups[1]) %>%
        add_histogram(x = selectbyMetrics()$Metric[selectbyMetrics()$Group == groups[2]], name = groups[2]) %>%
        layout(barmode = "overlay")
    }
    
  })
  
  output$p1 <- renderPlotly({
    if(input$sampleTG2 == "All"){
      dat2plot <- mydata() %>% dplyr::select("sample.type", anames,input$px, input$py, input$pz, input$by_group2)
    } else {
      dat2plot <- mydata() %>% filter(sample.type %in% input$sampleTG2) %>% dplyr::select("sample.type", anames,input$px, input$py, input$pz, input$by_group2)
    }
    
    dat2plot$x <- dat2plot[, input$px]
    dat2plot$y <- dat2plot[, input$py]
    dat2plot$group <- dat2plot[, input$by_group2]
    
    dat2plot$uid <- apply(dat2plot[,anames], 1, FUN = paste, collapse = ".")
    
    d <- event_data("plotly_selected")
    p <- plot_ly(dat2plot, x = ~x, y = ~y) %>%
      add_markers(key = ~uid, color = ~group) %>%
      layout(xaxis = list(title = input$px), yaxis = list(title = input$py))
    if (!is.null(d)) {
      m <- dat2plot[dat2plot$uid %in% d[["key"]], ]
      p <- add_markers(p, data = m, color = I("red"))
    }
    layout(p, dragmode = "select", showlegend = FALSE)
  })
  
  output$p2 <- renderPlotly({
    if(input$sampleTG2 == "All"){
      dat2plot <- mydata() %>% dplyr::select("sample.type", anames,input$px, input$py, input$pz, input$by_group2)
    } else {
      dat2plot <- mydata() %>% filter(sample.type %in% input$sampleTG2) %>% dplyr::select("sample.type", anames,input$px, input$py, input$pz, input$by_group2)
    }
    
    dat2plot$x <- dat2plot[, input$px]
    dat2plot$z <- dat2plot[, input$pz]
    dat2plot$group <- dat2plot[, input$by_group2]
    
    dat2plot$uid <-  apply(dat2plot[,anames], 1, FUN = paste, collapse = ".")
    
    d <- event_data("plotly_selected")
    p <- plot_ly(dat2plot, x = ~x, y = ~z) %>%
      add_markers(key = ~uid, color = ~group) %>%
      layout(xaxis = list(title = input$px), yaxis = list(title = input$pz))
    if (!is.null(d)) {
      m <- dat2plot[dat2plot$uid %in% d[["key"]], ]
      p <- add_markers(p, data = m, color = I("red"))
    }
    layout(p, dragmode = "lasso", showlegend = FALSE)
  })
  
  
  seldata <- reactive({
    if(input$sampleTG2 == "All"){
      dat2plot <- mydata() %>% dplyr::select("sample.type", anames,input$px, input$py, input$pz, input$by_group2)
    } else {
      dat2plot <- mydata() %>% filter(sample.type %in% input$sampleTG2) %>% dplyr::select("sample.type", anames,input$px, input$py, input$pz, input$by_group2)
    }
    
    dat2plot$x <- dat2plot[, input$px]
    dat2plot$z <- dat2plot[, input$pz]
    dat2plot$group <- dat2plot[, input$by_group2]
    
    d <- event_data("plotly_selected")
    p <- plot_ly(dat2plot, x = ~x, y = ~z) %>%
      add_markers(key = ~uid, color = ~group)
    if (!is.null(d)) {
      dat2plot$uid <-  apply(dat2plot[,anames], 1, FUN = paste, collapse = ".")
      m <- dat2plot[dat2plot$uid %in% d[["key"]], ]
    } else {
      m <- dat2plot
    }
    m
  })
  
  output$display.sel.da.tab <- DT::renderDataTable(
    
    datatable(data =seldata(), 
              extensions = "Buttons", 
              options = list(dom = "Blfrtip", 
                             buttons =
                               list("copy", "print", list(
                                 extend = "collection",
                                 buttons = c("csv", "excel", "pdf"),
                                 text = "Download")
                               ) ## end of buttons customization
                             ## customize the length menu
                             , lengthMenu = list( c(5, 10, 20, -1) # declare values
                                                  , c(5, 10, 20, "All") # declare titles
                             ) # end of lengthMenu customization
                             , pageLength = 5
              ) # end of options
    )# end of datatables
  )
  
}
