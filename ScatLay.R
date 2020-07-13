# Sys.setenv(RETICULATE_PYTHON = "C:\\Users\\BUITT\\AppData\\Local\\Programs\\Python\\Python38")

# sync working directory with current file
wd <- dirname(rstudioapi::getActiveDocumentContext()$path)  #set wd as the current folder
print(wd == getwd())
print(wd)
print(getwd())
if(! wd == getwd()){
  setwd(wd)}
getwd()

################################################################################
print("start loading")
start.load <- Sys.time()   ### time

if(length(find.package(package = 'shiny',quiet = T))>0){
  library(shiny)
}else{
  print("Package shiny not installed")
  install.packages("shiny")
  print("Package shiny installed")
  library(shiny)
}

if(length(find.package(package = 'shinythemes',quiet = T))>0){
  library(shinythemes)
}else{
  print("Package shinythemes not installed")
  install.packages("shinythemes")
  print("Package shinythemes installed")
  library(shinythemes)
}

if(length(find.package(package = 'rstudioapi',quiet = T))>0){
  library(rstudioapi)
}else{
  install.packages("rstudioapi")
  library(rstudioapi)
}

if(length(find.package(package = 'reticulate',quiet = T))>0){
  library(reticulate)
}else{
  install.packages("reticulate")
  library(reticulate)
}
#################################################################################

#
#### sourcing util files
source(paste0("./www/utils_scatlay.R"))
source_python("./www/df_converter.py")
source_python("./www/ScatterOverlay_ABT_test.py")

loadPkg()

end.load <- Sys.time()
print("loading time")
print(end.load-start.load)
##### UI from here ###########
ui <- navbarPage(id = "navbar",
  theme = shinytheme("flatly"),
  title = 'ScatLay',
  tabPanel('Home',
           # useShinyjs(),
           sidebarPanel(
             h6("Fields with * are compulsory"),
             radioButtons('file_type',"File Type *",
                          c('Normalised file'='norm', 'Raw file (read count)'='raw')),
             conditionalPanel(
               condition = "input.file_type=='raw'",  # raw
               p("Example ",a("here", href="https://github.com/buithuytien/ABioTrans/blob/master/Test%20data/Eg_raw.png")),  # ADD EXAMPLE
               fileInput('file1','Raw Counts *'),
               # radioButtons('norm_method',"Normalisation method",
               #              c('RPKM','FPKM','TPM')),
               p("Example ",a("here", href = "https://github.com/buithuytien/ABioTrans/blob/master/Test%20data/Eg_gene_length.png")),  # ADD EXAMPLE
               fileInput('length1','Gene Length'), #gene id + length
               p("Example ",a("here", href = "https://github.com/buithuytien/ABioTrans/blob/master/Test%20data/Eg_negative_control_genes.png")),  # ADD EXAMPLE
               fileInput('spikes1','Negative Control Genes')
               # helpText("* Format requirement: CSV file. The first column contains gene names; the read counts of each genotype (conditions: wildtype, mutants, replicates, etc.) are in the following columns.Each genotype column should have a column name. ")
             ),
             conditionalPanel(
               condition = "input.file_type=='norm'", # normalized
               p("Example ",a("here", href = "https://github.com/buithuytien/ABioTrans/blob/master/Test%20data/Eg_normalised.png")),  # ADD EXAMPLE
               fileInput('file2','Normalized Expression *')
               # helpText("* Format requirement: CSV file. Gene names in rows and genotypes in columns, following the usual format of files deposited in the GEO database.")
             ),
             p("Example ",a("here", href="https://github.com/buithuytien/ABioTrans/blob/master/Test%20data/Eg_metadata.png")),  # ADD EXAMPLE
             fileInput('metafile1','Meta Data File *'),
             actionButton("submit_input","Submit")
           ),
           mainPanel(
             h3('Welcome to ScatLay --'),
             h3('Finding Differentially Expressed genes by overLAYing SCATters')
             # img(src="scatlay_logo.png",
             #     width = 694*2,height = 138*2)
           )
  ),
  tabPanel('Preprocessing',
           sidebarPanel(
             h4("Filtering"),
             splitLayout(
               numericInput("min_val","Min. value", min=0.1,step=0.1,value=1.0),
               numericInput("min_col","Min. columns", min=1, value=2)
             ),
             conditionalPanel(
               condition = "input.file_type=='raw'",
               radioButtons('norm_method',"Normalisation method",
                            c("None (Black)"="None",
                              'RPKM (Blue)'='RPKM','FPKM (Dark cyan)'='FPKM',
                              'TPM (Dark green)'='TPM',
                              "RUV (Brown)"='RUV'))
             ),
             actionButton("submit_preprocessing","Submit"),
             conditionalPanel(
               condition = "input.preprocessing_tabs == 'Data table' ",
               br(),
               br(),
               downloadButton("download_norm_data", "Download table (csv)")
             )
           ),
           mainPanel(
             tabsetPanel(type = "tabs",id="preprocessing_tabs",
                         tabPanel("RLE plot",
                                  conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                                                   div(img(src="load.gif",width=240,height=180),
                                                       h4("Processing ... Please wait"),style="text-align: center;")
                                  ),
                                  conditionalPanel(condition="!$('html').hasClass('shiny-busy')",
                                                   plotOutput("RLE.plot2")
                                  ),

                                  conditionalPanel(
                                    condition = "input.file_type=='raw'",
                                    conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                                                     div(img(src="load.gif",width=240,height=180),
                                                         h4("Processing ... Please wait"),style="text-align: center;")
                                    ),
                                    conditionalPanel(condition="!$('html').hasClass('shiny-busy')",
                                                     plotOutput("RLE.plot")
                                    )
                                  )
                         ),
                         tabPanel("Data table",
                                  h3("Normalized data"),
                                  DT::dataTableOutput("norm_table")
                         ),
                         tabPanel("Description table",
                                  h3("Data description"),
                                  DT::dataTableOutput("meta_table")
                         )
             )
           )
  ),

    tabPanel('Overlay',
             sidebarPanel(
               h4('First Condition'),
               selectInput(inputId = 'overlay.x1',label = 'Replicate 1',choices = ""),
               selectInput(inputId = 'overlay.y1',label = 'Replicate 2',choices = ""),
               h4('Second Condition'),
               selectInput(inputId = 'overlay.x2',label = 'Replicate 1',choices = ""),
               selectInput(inputId = 'overlay.y2',label = 'Replicate 2',choices = ""),

               conditionalPanel(
                 condition = "input.overlay_tabs=='DE Gene Table'",
                 downloadButton("overlay.table.download", "Download as CSV")
               ),

               h4('ScatLay parameter'),
               numericInput(inputId = 'theta', label = 'Scatter dot size', value = 0.01,
                            min = 0.001, max = 0.5, step = 0.001),
               numericInput(inputId = 'pval_KDE', label = 'p-value threshold', value = 0.1,
                            min = 0.01, max = 1, step = 0.01),
               selectInput(inputId = 'overlay_option',label = 'Overlay Option',
                           choices = c("Intersect", "Union", "First Replicate", "Second Replicate"),
                           selected = 'Intersect' ),

               actionButton("submit_overlay","Submit")),
             # mainPanel(
             #   plotOutput('overlay.plot')
             # )
             mainPanel(
               tabsetPanel(type = "tabs",id="overlay_tabs",
                           tabPanel("Overlay Scatter",
                                    conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                                                     div(img(src="load.gif",width=240,height=180),
                                                         h4("Processing ... Please wait"),
                                                         style="text-align: center;")
                                    ),
                                    conditionalPanel(condition="!$('html').hasClass('shiny-busy')",
                                                     div(plotOutput("overlay.plot") )
                                    )),
                           tabPanel("DE Gene Table",
                                    conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                                                     div(img(src="load.gif",width=240,height=180),
                                                         h4("Processing ... Please wait"),style="text-align: center;")
                                    ),
                                    conditionalPanel(condition="!$('html').hasClass('shiny-busy')",
                                                     div(h4("Table of Differentially Expressed Genes"),
                                                         h6("Differentially expressed genes have Non_overlap = TRUE and p-value < threshold"),
                                                         DT::dataTableOutput('overlay.table'),
                                                         style = "font-size:80%")
                                    )),
                           tabPanel("2D Kernel Density",
                                    conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                                                     div(img(src="load.gif",width=240,height=180),
                                                         h4("Processing ... Please wait"),
                                                         style="text-align: center;")
                                    ),
                                    conditionalPanel(condition="!$('html').hasClass('shiny-busy')",
                                                     div(h4("2D Kernel Density"),
                                                         h6("2D Kernel Density estimated from scatter of 2 replicates, used for p-value estimation of each gene"),
                                                         plotlyOutput("overlayKDE.plot") )
                                    ))
                         )
                     )

             )
)
  ####################################################

server <- function(input,output,session){

  ########################################
  ##### get variable names for input #####
  ########################################

  observe({
    type <- input$file_type
    if(type=='norm'){
      DS <- df_norm()
    }else if(type=='raw'){
      DS <- df_raw()
    }
    nms <- colnames(DS)

    ### preprocessing tab
    f <- group_names()
    f <- unique(as.character(f))
    if(is.null(f)){
      hideTab(inputId="preprocessing_tabs", target="Description table")
      # hideTab(inputId="preprocessing_tabs", target="Description table")
    } else {
      showTab(inputId="preprocessing_tabs", target="Description table")
      updateSelectInput(session,"f1",choices=f,selected =f[1])
      updateSelectInput(session,"f2",choices=f,selected =f[2])
    }

    ### overlay tab
    updateSelectInput(session, "overlay.x1", choices = nms, selected = nms[1])
    updateSelectInput(session, "overlay.y1", choices = nms, selected = nms[2])
    updateSelectInput(session, "overlay.x2", choices = nms, selected = nms[3])
    updateSelectInput(session, "overlay.y2", choices = nms, selected = nms[4])

  })


  observeEvent(input$submit_input, {
    type <- input$file_type
    if(type=='norm'){
      DS <- df_norm()
      lengths <- 0
    }else if(type=='raw'){
      DS <- df_raw()
      lengths <- gene_length()
      # if( length(intersect(rownames(lengths), rownames(DS))) < 1000 )
      #   length <- NULL
    }


    f <- group_names()
    print("line 283")
    print(f)
    print("line 288")
    print(head(DS))

    spikes <- neg_control()

    # if any NULL value, throw error. TO CHANGE TO BE MORE SPECIFIC
    input.list <- list(DS, f)
    input.null <- sapply(input.list, is.null)
    names(input.null) <- c("Expression/Counts","Meta Data")

    if( any(input.null) ){
      index.null <- which(input.null)
      errors <- paste(names(input.null)[index.null],collapse = ', ')
      # print(errors)
      showModal(modalDialog(
        type = "Error",
        paste("Please check these input:",errors,"and try again!")
      ))
    } else{
      updateNavbarPage(session, inputId="navbar",selected="Preprocessing")
    }

    # update input
    updateNumericInput(session,"min_col",max=ncol(DS))   # update max column nunmber in filtering
    if(is.null(spikes)){
      updateRadioButtons(session,"norm_method",choices = c("None (Black)"="None",
                                                           'RPKM (Blue)'='RPKM','FPKM (Dark cyan)'='FPKM',
                                                           'TPM (Dark green)'='TPM',
                                                           "Upper Quartile (Brown)"='RUV') )
      #c("None",'RPKM','FPKM','TPM',"Upper Quartile"="RUV")
    } else {
      updateRadioButtons(session,"norm_method",choices = c("None (Black)"="None",
                                                           'RPKM (Blue)'='RPKM','FPKM (Dark cyan)'='FPKM',
                                                           'TPM (Dark green)'='TPM',
                                                           "RUV (Brown)"='RUV'))
    }
    if(is.null(lengths) & !(is.null(spikes)) ){
      updateRadioButtons(session,"norm_method",choices = c("None (Black)"="None","RUV (Brown)"="RUV"))
    } else if(is.null(lengths) & (is.null(spikes)) ){
      updateRadioButtons(session,"norm_method",choices = c("None (Black)"="None","Upper Quartile (Brown)"="RUV"))
    }

  })


  ######################################
  ######### read in / get data #########
  ######################################

  #####################
  ## get data #########
  #####################

  # get normalized counts
  df_norm <- reactive({        # get normalized counts
    if (is.null(input$file2))
      return (NULL)
    parts <- strsplit(input$file2$datapath,".",fixed=TRUE)
    type <- parts[[1]][length(parts[[1]])]
    if(type!="csv"){
      showModal(modalDialog(
        title = "Error",
        "Please input a csv file!"
      ))
      return (NULL)
    }
    ds <- read.csv(input$file2$datapath)
    ds <- na.omit(ds)
    ds <- ds[!duplicated(ds[,1]),]   # remove duplicated gene names

    row_names <- ds[,1]
    DS <- data.frame(ds)
    if(ncol(DS)<=1){
      showModal(modalDialog(
        title = "Error",
        "Please check normalised data file format (Eg_normalised.png) and try again!"
      ))
      return(NULL)
    }
    DS <- DS[,-1]
    row.names(DS) <- row_names
    for (i in 1:ncol(DS)){
      if(class(DS[,i])!="numeric" & class(DS[,i])!="integer"){
        showModal(modalDialog(
          title = "Error",
          "Please check normalised data file format (Eg_normalised.png) and try again!"
        ))
        return(NULL)
      }
    }
    return(DS)
  })

  # get raw counts
  df_raw <- reactive ({
    if(is.null(input$file1))
      return(NULL)
    parts <- strsplit(input$file1$datapath,".",fixed=TRUE)
    type <- parts[[1]][length(parts[[1]])]
    if(type!="csv"){
      showModal(modalDialog(
        title = "Error",
        "Please input a csv file!"
      ))
      return (NULL)
    }
    raw_ds <- read.csv(input$file1$datapath)
    raw_ds <- na.omit(raw_ds)
    raw_ds <- raw_ds[!duplicated(raw_ds[,1]),]   # remove duplicated gene names

    # raw_ds <- as.data.frame(raw_ds)
    if(ncol(raw_ds)<=1){
      showModal(modalDialog(
        title = "Error",
        "Data file must contain at least 2 columns. Please check raw data format and try again!"
      ))
      return(NULL)
    }

    row_names <- raw_ds[,1]
    rownames(raw_ds) <- row_names
    raw_DS <- raw_ds[,-1]  # remove the first column, which is gene Id

    for (i in 1:ncol(raw_DS)){
      if(class(raw_DS[,i])!="numeric" & class(raw_DS[,i])!="integer"){
        showModal(modalDialog(
          title = "Error",
          "Raw counts must be integer. Please check raw data formate and try again!"
        ))
        return(NULL)
      }
    }
    return(raw_DS)
  })

  # get gene length
  gene_length <- reactive({
    if (is.null(input$length1))
      return (NULL)
    lengths_df <- read.csv(input$length1$datapath)
    lengths_df2 <- data.frame("len" = lengths_df[,2]);
    rownames(lengths_df2) <- as.character(lengths_df[,1])
    return(lengths_df2)
  })

  # get spikes / negative control genes
  neg_control <- reactive({
    if(is.null(input$spikes1))
      return(NULL)
    spikes <- read.csv(input$spikes1$datapath,header=F)
    spikes <- as.character(spikes[,1])
    # print(spikes[1:10])
    return(spikes)
  })

  # get meta data table
  group_names <- reactive({
    # if no data
    if(is.null(input$metafile1))
      return(NULL)

    # read in group names (metadata)
    groups <- read.csv(input$metafile1$datapath)
    group_colnames <- as.character(groups[,1])

    type <- input$file_type
    if(type=='norm'){
      DS <- df_norm()
    }else if(type=='raw'){
      DS <- df_raw()
    }
    col_names <- colnames(DS)   # columm names of DS in order

    # check if groups and column names are similar
    if ( !all(col_names %in% group_colnames) || ncol(groups) < 2 ){
      showNotification(type = "error", "group names and DS column names not similar")
      return(NULL)
    }

    if(ncol(groups)==2){
      f <- groups[match(col_names,groups[,1]),] [,2]   # arrange f in the same order as col_names
    } else {
      f <- groups[match(col_names,groups[,1]),] [,2]
      for(i in 3:ncol(groups)){
        f <- paste0(f,"_",groups[,i])
      }
    }
    f <- as.factor(make.names(f))
    # return(as.factor(f))
    return(f)
  })


  ####################################
  ########## PREPROCESSING ###########
  ####################################

  # filter normalized counts
  df_shiny <- eventReactive(input$submit_preprocessing, {
    DS_norm <- df_norm()
    min_val <- input$min_val
    min_col <- input$min_col
    keep <- rowSums(DS_norm >= min_val) >= min_col
    DS <- DS_norm[keep,]
    # DS <- apply(DS_norm, 1, function(x) length(x[x>min_val])>=min_col)
    return(DS)
  })

  # filter raw counts
  df_raw_filt <- eventReactive(input$submit_preprocessing, {
    DS_raw <- df_raw()
    min_val <- input$min_val
    min_col <- input$min_col
    keep <- rowSums(DS_raw >= min_val) >= min_col
    DS_filt <- DS_raw[keep,]
    # DS_filt <- apply(DS_raw, 1, function(x) length(x[x>min_val])>=min_col)
    return(DS_filt)
  })

  # normalizing raw counts
  df_raw_shiny <- reactive({
    raw_DS <- df_raw_filt()     # get filtered raw counts
    method <- input$norm_method

    if(method%in% c("TPM","RPKM","FPKM")){
      lengths_df <- gene_length()
      merge_DS <- merge(raw_DS,lengths_df,by="row.names")
      rownames(merge_DS) <- merge_DS[,1]; merge_DS <- merge_DS[,-1];
      raw_DS <- merge_DS[,-ncol(merge_DS)]
      lengths <- merge_DS[,ncol(merge_DS)]
      # print("length")
      # print(head(merge_DS))
    }
    # print("from line 981 df_raw_shiny")
    # print(method)
    # print("raw_DS")
    # print(head(raw_DS[,1:4]))
    # print("dimension of raw_DS")
    # print(dim(raw_DS))

    if(method=='TPM'){
      tpm.matrix<- apply(raw_DS, 2, function(x) tpm(x, lengths))
      tpm.df <- data.frame(tpm.matrix)
      return (tpm.df)
    }else if(method=='RPKM'){
      rpkm.matrix <- edgeR::rpkm(raw_DS,lengths)
      rpkm.df <- data.frame(rpkm.matrix)
      return (rpkm.df)
    }else if(method=='FPKM'){
      fpkm.matrix<- apply(raw_DS, 2, function(x) fpkm(x, lengths))
      fpkm.df <- data.frame(fpkm.matrix)
      return (fpkm.df)
    }else if(method=='None'){
      return (raw_DS)
    }else if(method=='RUV'){
      spikes <- neg_control()
      if (!is.null(spikes))
        spikes <- intersect(spikes,rownames(raw_DS))
      # f <- group_names()
      # if( is.null(spikes) )
      #   spikes <- getEmpirical(rawDS,f)
      set1 <- RUVg.apply(raw_DS,spikes)
      RUV.df <- as.data.frame(normCounts(set1))
      return (RUV.df)
    }

  })


  ######### ANALYSIS FROM HERE ############
  ######## RLEplot and Preprocessing ###########
  #############################################
  RLE.plot <- reactive({
    type <- input$file_type
    if(type=='norm'){
      DS <- df_shiny()
    }else if(type=='raw'){
      DS <- df_raw_shiny()
    }
    set1 <- newSeqExpressionSet(as.matrix(DS))
    norm_method_name <- input$norm_method
    colors <- c('RPKM'='blue','FPKM'='darkcyan','TPM'='darkgreen',"RUV"='Brown',"Upper Quartile"='Brown')
    if(norm_method_name!="None" &input$submit_preprocessing != 0){
      spikes <- neg_control()
      if(norm_method_name == "RUV" & is.null(spikes))
        norm_method_name <- "Upper Quartile"
      plotRLE(set1, ylim=c(-1.5,1.5),outline=FALSE, col=colors[norm_method_name],
              main= paste(norm_method_name,"Normalized"))
    }
  })

  output$RLE.plot <- renderPlot({
    RLE.plot()
  })

  output$RLE.plot2 <- renderPlot({   # for raw data
    start.rle <- Sys.time()
    type <- input$file_type
    if(type=='norm'){
      raw_DS <- df_shiny()
      main_title <- "Input data"
    }else if(type=='raw'){
      raw_DS <- df_raw()
      main_title <- "Raw data"
    }
    set1 <- newSeqExpressionSet(as.matrix(raw_DS))
    if(input$submit_preprocessing != 0)
      plotRLE(set1, ylim=c(-1.5,1.5),outline=FALSE, main=main_title)
    end.rle <- Sys.time()
    print("time for RLE plot and preprocessing")
    print(end.rle - start.rle)
  })


  output$norm_table <- DT::renderDataTable({
    type <- input$file_type
    if(type=='norm'){
      DS <- df_shiny()
    }else if(type=='raw'){
      DS <- df_raw_shiny()
    }
    # if(input$submit_preprocessing != 0)
    DS   # with filtering and normalization
  })

  output$meta_table <- DT::renderDataTable({
    f <- group_names()
    type <- input$file_type
    if(type=='norm'){
      DS <- df_shiny()
    }else if(type=='raw'){
      DS <- df_raw_shiny()
    }
    if(! is.null(f)){
      meta_df <- data.frame("Column names"=colnames(DS),"Description"=f)
      meta_df
    }
  })

  output$download_norm_data <- downloadHandler(
    filename = function(){
      method <- input$norm_method
      paste(method,"normalized.csv")
    },
    content = function(file){
      type <- input$file_type
      if(type=='norm'){
        DS <- df_shiny()
      }else if(type=='raw'){
        DS <- df_raw_shiny()
      }
      write.csv(DS, file, row.names = F)
    }
  )



  ###################################
  ###################################
  ########  Scatter Overlay  ########
  ############# Python ##############
  ###################################

  plotOverlay <- eventReactive(input$submit_overlay, {
    overlay.start <- Sys.time()

    # get column names to plot overlay
    x1 <- input$overlay.x1
    y1 <- input$overlay.y1
    x2 <- input$overlay.x2
    y2 <- input$overlay.y2

    # get gene expression dataframe to plot overlay
    type <- input$file_type
    if(type=='norm'){
      DS <- df_shiny()
    }else if(type=='raw'){
      DS <- df_raw_shiny()
    }


    # get scatter dot size theta to plot overlay
    theta <- input$theta

    # get p-value threshold
    pval_KDE <- input$pval_KDE

    # get overlay option: either intersect, union, first replicate, or second replicate
    overlay_option <- input$overlay_option

    ### run scatter overlay ###
    # extract overlay dataframe with 4 columns: cond1_rep1, cond1_rep2, cond2_rep1, cond2_rep2
    overlay.data <- DS[,c(x1, y1, x2, y2)]

    # run overlay to get non-overlapping genes
    py_df <- r2py_DataFrame(overlay.data) # from python source

    scatlay_res <- deRun(df = py_df, theta = theta) # from python source

    df_temp  <- scatlay_res[[2]]
    df_temp2 <- scatlay_res[[3]]
    df_temp3 <- scatlay_res[[4]]
    exc1_names <- scatlay_res[[5]]
    exc2_names <- scatlay_res[[6]]

    # get non-overlapping genes from scatter overlaying
    if(overlay_option == "Intersect"){
      exc_common0 <- scatlay_res[[7]] # highlighted genes
    } else if(overlay_option == "Union"){
      exc_common0 <- union(exc1_names, exc2_names)
    } else if(overlay_option == "First Replicate"){
      exc_common0 <- exc1_names
    } else if(overlay_option == "Second Replicate"){
      exc_common0 <- exc2_names
    }

    # get pvalue threshold
    pval_res <- pvalRun(df = scatlay_res[[1]],
                          theta = theta,
                          exc_common0 = exc_common0,
                          pval_thres = pval_KDE)
    exc_common <- pval_res[[1]]
    pval_df <- pval_res[[2]]

    # if(pval_KDE == 1){
    #   exc_common <- exc_common0
    # }


    overlay.end <- Sys.time()
    print("Overlay plot time")
    print(overlay.end - overlay.start)
    return (list(DS, x1, y1, x2, y2, theta, pval_KDE, overlay_option, #1 - 8
                 overlay.data, df_temp, df_temp2, df_temp3, exc1_names, exc2_names, # 9-14
                 exc_common0, exc_common, pval_df )) # 15 16 17
  })


  overlayplot <- function(){
    # get data
    li <- plotOverlay()
    df_temp  <- li[[10]]
    df_temp2 <- li[[11]]
    df_temp3 <- li[[12]]
    exc_common0 <- li[[15]]
    exc_common <- li[[16]]
    theta <- li[[6]]

    runFig(df_temp, df_temp2, df_temp3,
           exc_common=exc_common, theta=theta) # from python source
  }


  output$overlay.plot <- renderPlot({
    overlayplot()
  })

  output$overlayKDE.plot <- renderPlotly({
    li <- plotOverlay()
    overlay.data <- li[[9]]

    df_tpm_log <- dfTransform(overlay.data, 10);
    x <- rbind(as.matrix(df_tpm_log[,1:2]), as.matrix(df_tpm_log[,3:4]) );

    # pre_process x
    # lfc_x <- abs(log2(x[,2] / x[,1]) ) %>% na.omit()
    # keep_x <- which(lfc_x <= 1)
    # x <- x[keep_x,]

    # generate 2D KDE plot in interactive 3D
    pdf2d_plot <- MASS::kde2d(x[,1], x[,2], h=MASS::bandwidth.nrd(x)*2, n = 50)
    p <- plot_ly(x = pdf2d_plot$x, y = pdf2d_plot$y, z = pdf2d_plot$z) %>%
          add_surface()
    p
  })

  output$overlay.table = DT::renderDataTable({
    li <- plotOverlay()
    pval_df <- li[[17]]
    exc_common <- li[[16]]

    pval_df[exc_common,]
  })

  output$overlay.table.download <- downloadHandler(
    filename = function() {
      theta <- input$theta
      paste0("ScatLay_theta",theta,".csv")
    },
    content = function(file) {
      li <- plotOverlay()
      pval_df <- li[[17]]
      write.csv(pval_df, file, row.names = FALSE)
    }
  )

  ###############################################################################
  #### HELPER FUNCTION FOR SCATTER OVERLAY - GENERATE P-VALUE BASED ON KDE  #####
  # pre-process dataframe
  dfTransform <- function(x, logbase=10){
    x_log <- log(x, base=logbase)
    x_log[x_log == -Inf] <- 0
    return(x_log)
  }


  # get p-value from kde
  pvalKDE <- function(pdf2d, x){
    # x = location to predict pvals
    est <- pdf2d$estimate
    est <- est - min(est)
    prob <- predict(pdf2d, x=x) - min(est)
    # pvals = integration from -Inf to location x
    pvals <- sum(est[est<prob])/ sum(est)
    return(pvals)
  }

  pvalAllGenes <- function(pdf2d, df_tpm_log, pval){
    # df_tpm_log = dataframe, 4 columns, log transformed
    pvals1 <- apply(df_tpm_log[,c(1,3)], MARGIN=1, function(x) pvalKDE(pdf2d, x)  )
    pvals2 <- apply(df_tpm_log[,c(2,4)], MARGIN=1, function(x) pvalKDE(pdf2d, x)  )

    pval_df <- data.frame("Gene" = rownames(df_tpm_log),
                          "P_value" = pmin(pvals1, pvals2))
    rownames(pval_df) <- rownames(df_tpm_log)

    return(pval_df)
  }

  pvalRun <- function(df, theta, exc_common0, pval_thres = 0.1){
    # use kde to get probability density function, and integrate to have p-value
    df_tpm_log <- dfTransform(df, 10);
    x <- rbind(as.matrix(df_tpm_log[,1:2]), as.matrix(df_tpm_log[,3:4]) );
    cat("head(x): ", "\n"); print(head(x))

    # pre_process x
    lfc_x <- abs(log2(x[,2] / x[,1]) ) %>% na.omit()
    keep_x <- which(lfc_x <= 1)
    x <- x[keep_x,]
    dim(x)

    # get bandwidth
    H_default = Hpi(x=x); cat("H_default =", H_default, "\n")
    f <- mean(H_default)/theta; cat("f =", f, "\n")
    H <- H_default/f; cat("H =", H, "\n")
    n_grid = 150 #as.integer(max(ecoli_full[,c("b1","c1")])/ mean(H) ) /5
    pdf2d <- kde(x=x, H=H, gridsize = c(n_grid, n_grid) )
    # plot(pdf2d)

    # get genes with p-value < threshold
    pval_df <- pvalAllGenes(pdf2d, df_tpm_log, pval=pval_thres);
    # if the genes are non-overlap from ScatLay python run (~ yellow dots, differentially expressed)
    non_overlapping <- sapply(pval_df$Gene, function(x) x %in% exc_common0 )
    pval_df[,"Non_overlap"] <- non_overlapping

    print("line 2988 - head(pval_df)");
    print(head(pval_df));

    pval_df_filt <- pval_df %>% filter(P_value < pval_thres)
    pval_genes <- pval_df_filt$Gene

    pval_df_filt <- pval_df %>% filter(P_value < pval_thres, Non_overlap == TRUE)
    diff_theta_pval <- pval_df_filt$Gene

    print(head(pval_genes));

    cat("length of genes with p-value below", pval_thres, ":", length(pval_genes), "\n")
    cat("length of de genes with p-value below", pval_thres, ":", length(diff_theta_pval), "\n")

    return(list(diff_theta_pval, pval_df))
  }


  ###############################################################################

  ###################################
  ###################################
  ###################################
  ###################################


  #session$onSessionEnded(stopApp)
}

shinyApp(ui,server)
