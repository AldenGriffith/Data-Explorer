
options(shiny.trace=FALSE)

source("globalVars.R")
source("startupChoices.R")
source("startupSelections.R")


#SHINY SERVER ----
shinyServer(function(input, output, session) {
    
    # DO NOT EDIT THIS SECTION  # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # ----- 
    # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
    # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
    
    
    new.upload <- FALSE  #global object uesd to suspend automatic dataset switching during new data upload
    switching <- FALSE
    
    # # # Creates list object to store uploaded data files and corresponding info (can be accessed within/across reactive expressions)
    V <- reactiveValues(
        
        Data = list(),  #data
        Name = NULL,  #dataset name (will be char vector)
        X = list(),  #X variables (continuous)
        Y = list(),  #Y variables (continuous and categorical)
        Cat = list(), #Categorical variables
        ON = FALSE,  #indicates whether a dataset has been uploaded
        Current = 0,  #which dataset is currently being used
        Sel = list(),  #stores information on selections for UI objects (for switching between datasets)
        Choices = list(), #stores information for UI objects with choices that change (for switching between datasets)
        Fig = NULL,
        Figs = list(),
        Figs.dim = list(),
        Group.Sym.Color = list(),  #these are set to FALSE in newData.R. Important for updating checkboxes, but then allowing them to be unchecked by user
        Group.Line.Color = list(),
        Group.Cat.Color = list(),
        Group.Fit.Linear = list(),
        Group.Fit.Quadratic = list(),
        Group.Fit.Power = list(),
        Group.Fit.Custom = list(),
        form = list(), #custom formula
        params = list() #custom parameters
        # last.switched = as.numeric(Sys.time()),  #used to store Sys.time() when last switched - pauses group observer
        # switched = FALSE
        
    )
    
    ## migth want to hide hidden objects here on startup?
    
    
    
    
    # # # Creates the file upload button. Done using renderUI so that it resets after each upload (reset later in the code)
    output$Upload <- renderUI({
        
        fileInput('file', 'Upload new dataset (.csv, .xls, .xlsx)'
                  # accept=c('text/csv',
                  #          'text/comma-separated-values,text/plain',
                  #          'application/vnd.ms-excel',
                  #          '.csv',
                  #          '.xls',
                  #          '.xlsx')
                  )
    })
    
    # # # Action: upload button clicked
    observeEvent(input$file,{
        
        #Determine file type
        last.dot <- as.numeric(regexpr("\\.[^\\.]*$", input$file$name))  #slick way to find last "." in string (found online)
        file.type <- substr(input$file$name, last.dot, nchar(input$file$name))
        
        #deal with name of dataset...
        source("naming.R", local=TRUE)
        
        #Creates text input for dataset name
        output$Data.Name <- renderUI({
            
            textInput("Data.Name", "Dataset name:", value=name) #the object "name" is created by naming.R
            
        })
        
        #Creates action button to submit data
        output$Submit.Button <- renderUI({
            
            actionButton("Submit.Button", "Submit Dataset", icon("thumbs-up"),
                         style="color: #fff; background-color: #337ab7; border-color: #2e6da4; display:center-align")
            
        })
        
        #If file type is a xls or xlsx (less straightforward due to worksheets)
        if (file.type == ".xlsx" | file.type == ".xls"){
            
            #Need to make sure extension is on the temp file (required by readxl package). Seems not always consistent among different OS.
            if (grepl(file.type, input$file$datapath)){ #is the extension found in the temp file?
                
                path <- input$file$datapath #new path is same as old
                
            } else {
                
                path <- paste(input$file$datapath, file.type, sep="") #new path with extension on it
                file.copy(input$file$datapath, path) #creates a copy of the file with the extension on it. Path stored in 'path'
            }
            
            #Creates dropdown list of worksheets
            output$Sheet <- renderUI({
                
                selectInput("Sheet","Which Excel worksheet?",
                            excel_sheets(path))
                
            })
        }
        
        #Action: submit button clicked
        observeEvent(input$Submit.Button,{
            
            #Stores uploaded file
            
            if (file.type == ".csv") {
                
                New.Data <- read.csv(input$file$datapath, header=TRUE, sep=",")
                
            } else if (file.type == ".xlsx" | file.type == ".xls"){
                
                New.Data <- as.data.frame(read_excel(path, sheet = input$Sheet))
                
            }
            
            if (V$ON) source("selSave.R", local=TRUE) #will not run if no dataset yet uploaded
            
            #Adds new dataset and switches to it
            switching <<- TRUE
            source("newData.R", local = TRUE)
            source("selNewData.R", local=TRUE)
            # V$last.switched <-as.numeric(Sys.time())
            # Sys.sleep(2)
            # V$switched <- TRUE
            
            #Removes post-upload ui objects
            output$Sheet <- renderUI(return()) #probably not necessary (doesn't come up with .csv file)
            output$Submit.Button <- renderUI(return())
            output$Data.Name <- renderUI(return())
            
        }, ignoreInit = TRUE, once = TRUE) #'ignoreInit' is set to TRUE to keep this from being run when button is (dynamically) created. 'once' = TRUE prevents stange repeat behavior...
        
        #switch.obs$resume()
        
    }) # # # end of upload file observeEvent
    
    
    # # # Action: switch datasets using dropdown list
    observe({
        
        if (length(V$Data) > 1) if (input$Use.Data != V$Name[V$Current]) { #user swtiches to different datasets (input$Use.Data)
            
            if (!new.upload) {
                message(paste(input$Use.Data, V$Name[V$Current], sep = "|"))
                
                #Save current input selections
                source("selSave.R", local=TRUE)
                
                #New Dataset
                i <- which(V$Name == input$Use.Data) #which dataset was selected?
                V$Current <- i  #sets new dataset
                
                switching <<- TRUE
                #Update input selections based on selected dataset
                source("selUpdate.R", local=TRUE)
                # switching <<- FALSE
                # V$last.switched <- as.numeric(Sys.time())
                
                # Sys.sleep(2)
                message("Switched!!!")
                message(paste(input$Use.Data, V$Name[V$Current], sep = "|"))
                
            } else {
                
                new.upload <<- FALSE #automatically resets to FALSE
                
            }
            
        }
        
    })
    
    
    
    # # # Display tabs and options when variables are selected
    
    observe({
        
        #Y variable
        if (input$Y_dy != "(none)"){
            if (!is.element(input$Y_dy,V$Choices[[V$Current]]$Group_dy)){
                updateSelectInput(session, "Y_dy", label = "Y variable (continuous)")
            }
        } else {
            updateSelectInput(session, "Y_dy", label = "Y variable")
        }
        
        #X variable
        if (input$X_dy != "(none)"){
            if (!is.element(input$X_dy,V$Choices[[V$Current]]$Group_dy)){
                updateSelectInput(session, "X_dy", label = "X variable (continuous)")
                # shinyjs::hide("div.CatOptions")
                # shinyjs::show("div.ScatterOptions")
                # shinyjs::show("div.Models")
                
            } else {
                updateSelectInput(session, "X_dy", label = "X variable (categorical)")
                # shinyjs::hide("div.ScatterOptions")
                # shinyjs::hide("div.Models")
                # shinyjs::show("div.CatOptions")
                
            }
            
        } else {
            updateSelectInput(session, "X_dy", label = "X variable")
        }
        
        #Axes inputs
        if (input$X_dy != "(none)" && input$Y_dy != "(none)"){
            
            shinyjs::show("div.FigHead")
            
            shinyjs::show("div.AxesOptions")
            
            shinyjs::hide("div.NoVar.Appearance")
            shinyjs::hide("div.NoVar.Axes")
            shinyjs::hide("div.NoVar.Models")
            
            
            # shinyjs::show("xlab")
            # shinyjs::show("ylab")
            # 
            # shinyjs::show("ymin")
            # shinyjs::show("ymax")
            
            if (!is.element(input$X_dy,V$Choices[[V$Current]]$Group_dy)){
                #X is continuous
                
                shinyjs::hide("div.CatOptions")
                shinyjs::show("div.ScatterOptions")
                shinyjs::show("div.Models")
                
                
                # shinyjs::show("xmin")
                # shinyjs::show("xmax")
                shinyjs::hide("div.xpad")
                shinyjs::show("div.xminmax")
                
                
                
            } else {
                #X is categorical
                
                shinyjs::hide("div.ScatterOptions")
                shinyjs::show("div.CatOptions")
                shinyjs::hide("div.Models")
                
                shinyjs::hide("div.xminmax")
                shinyjs::show("div.xpad")
            }
            
        } else {
            
            shinyjs::hide("div.AxesOptions")
            
            shinyjs::hide("div.ScatterOptions")
            shinyjs::hide("div.CatOptions")
            shinyjs::hide("div.Models")
            
            shinyjs::show("div.NoVar.Appearance")
            shinyjs::show("div.NoVar.Axes")
            shinyjs::show("div.NoVar.Models")
            
            
            shinyjs::hide("div.FigHead")
            
            # shinyjs::hide("xlab")
            # shinyjs::hide("ylab")
            # 
            # shinyjs::hide("ymin")
            # shinyjs::hide("ymax")
            
            # shinyjs::hide("xmin")
            # shinyjs::hide("xmax")
            shinyjs::hide("div.xminmax")
            
        }
    })
    
    
    # # # Disable/Enable color options based on group checkbox
    observe({
        
        #Symbols
        if (any(grepl("group", input$Symbols_dy))){
            
            shinyjs::disable("Sym.Color.Single")
            # shinyjs::show("Sym.Color.Group")
            
        } else {
            
            shinyjs::enable("Sym.Color.Single")
            # shinyjs::hide("Sym.Color.Group")
            
        }
        
        #Lines
        if (any(grepl("group", input$Lines_dy))){
            
            shinyjs::disable("Line.Color.Single")
            # shinyjs::show("Line.Color.Group")
            
        } else {
            
            shinyjs::enable("Line.Color.Single")
            # shinyjs::hide("Line.Color.Group")
            
        }
        
        #Categorical Fill
        if (input$Group_dy != "(none)" && any(grepl("Fill", input$Cat.Color.Boxes))){
            
            shinyjs::disable("div.Fill.Color")
            # shinyjs::show("Line.Color.Group")
            
        } else {
            
            shinyjs::enable("div.Fill.Color")
            # shinyjs::hide("Line.Color.Group")
            
        }
        
        #Categorical Edge
        if (input$Group_dy != "(none)" && any(grepl("Edge", input$Cat.Color.Boxes))){
            
            shinyjs::disable("div.Edge.Color")
            # shinyjs::show("Line.Color.Group")
            
        } else {
            
            shinyjs::enable("div.Edge.Color")
            # shinyjs::hide("Line.Color.Group")
            
        }
        
        
    })
    
    
    
    # # # Observe grouping variable and update symbol/line options
    observe({
        if(V$ON){
            # if(!switching){
                #message("group observer run")
                
                #Grouping variable selected
                if (input$Group_dy != "(none)") {
                    
                    if (!switching){
                    
                    message("--group observer: grouping var")
                    
                    
                    shinyjs::show("div.Scatter.Color.Theme")
                    shinyjs::show("div.Cat.Color.Theme")
                    shinyjs::show("div.Cat.Color.Boxes")
                    
                    
                    #Update symbol color options
                    if (!V$Group.Sym.Color[[V$Current]]){
                        current.symbols_dy <- input$Symbols_dy #save the currently selected boxes
                        V$Choices[[V$Current]]$Symbols_dy <- c(choices$Symbols_dy, "Color by group")
                        
                        updateCheckboxGroupInput(session, "Symbols_dy", inline = TRUE,
                                                 choices = V$Choices[[V$Current]]$Symbols_dy,
                                                 selected = c(current.symbols_dy, "Color by group"))
                        V$Group.Sym.Color[[V$Current]] <- TRUE #using this to make the input$Group seletion a one-time action. otherwise you can't uncheck the "color by group" box
                        
                    }
                    
                    #Update symbol shape options
                    current.shape <- input$Sym.Shape_dy #keep current shape. don't auto switch to group
                    V$Choices[[V$Current]]$Sym.Shape_dy <- c("Grouping Variable", shapes)
                    
                    updateSelectInput(session, "Sym.Shape_dy",
                                      choices = V$Choices[[V$Current]]$Sym.Shape_dy,
                                      selected = current.shape)  #should make this set to its current one
                    
                    
                    #Update line color options
                    if (!V$Group.Line.Color[[V$Current]]){
                        current.lines_dy <- input$Lines_dy #save the currently selected boxes  --- I think this is the problem!!?
                        V$Choices[[V$Current]]$Lines_dy <- c(choices$Lines_dy, "Color by group")
                        
                        updateCheckboxGroupInput(session, "Lines_dy", inline = TRUE,
                                                 choices = V$Choices[[V$Current]]$Lines_dy,
                                                 selected = c(current.lines_dy, "Color by group"))
                        V$Group.Line.Color[[V$Current]] <- TRUE #using this to make the input$Group seletion a one-time action. otherwise you can't uncheck the "color by group" box
                        
                    }
                    
                    
                    #Update line type options
                    current.lty <- input$Line.Type_dy #keep current line type. don't auto switch to group
                    V$Choices[[V$Current]]$Line.Type_dy <- c("Grouping Variable", lty)
                    
                    updateSelectInput(session, "Line.Type_dy",
                                      choices = V$Choices[[V$Current]]$Line.Type_dy,
                                      selected = current.lty)  #should make this set to its current one
                    
                    
                    #Update Cat fill color options
                    if (!V$Group.Cat.Color[[V$Current]]){
                        # current.cat.color.boxes <- input$Cat.Color.Boxes #save the currently selected boxes  - necessary?
                        
                        updateCheckboxGroupInput(session, "Cat.Color.Boxes", inline = TRUE,
                                                 choices = choices$Cat.Color.Boxes,
                                                 selected = "Fill")
                        V$Group.Cat.Color[[V$Current]] <- TRUE #using this to make the input$Group seletion a one-time action. otherwise you can't uncheck the "color by group" box
                        
                    }
                    
                    
                    #Update model fit options
                    if (!V$Group.Fit.Linear[[V$Current]]){  #if this isn't there you can end up with rapid switch back and forth - can't stop!
                        current.fit.lin <- input$Fit.Linear
                        updateCheckboxGroupInput(session, "Fit.Linear", choices = c("Fit by group", "Show results"),
                                                 selected = current.fit.lin)
                        V$Group.Fit.Linear[[V$Current]] <- TRUE
                        
                    }
                    
                    
                    if (!V$Group.Fit.Quadratic[[V$Current]]){ 
                        current.fit.qua <- input$Fit.Quadratic
                        updateCheckboxGroupInput(session, "Fit.Quadratic", choices = c("Fit by group", "Show results"),
                                                 selected = current.fit.qua)
                        V$Group.Fit.Quadratic[[V$Current]] <- TRUE
                        
                    }
                    
                    if (!V$Group.Fit.Power[[V$Current]]){ 
                        current.fit.pow <- input$Fit.Power
                        updateCheckboxGroupInput(session, "Fit.Power", choices = c("Fit by group", "Show results"),
                                                 selected = current.fit.pow)
                        V$Group.Fit.Power[[V$Current]] <- TRUE
                        
                    }
                    
                    if (!V$Group.Fit.Custom[[V$Current]]){ 
                        current.fit.custom <- input$Fit.Custom
                        updateCheckboxGroupInput(session, "Fit.Custom", choices = c("Fit by group", "Show results"),
                                                 selected = current.fit.custom)
                        V$Group.Fit.Custom[[V$Current]] <- TRUE
                        
                    }
                    
                    } else {
                        
                        switching <<- FALSE
                    }
                    
                } else { #restore to default choices   <--- default??  or saved?!!
                    
                    if (!switching){
                    
                    message("--group observer: no grouping var")
                    
                    shinyjs::hide("div.Scatter.Color.Theme")
                    shinyjs::hide("div.Cat.Color.Theme")
                    shinyjs::hide("div.Cat.Color.Boxes")
                    
                    #Update symbol color options
                    current.symbols_dy <- grep("group", input$Symbols_dy, value = TRUE, invert = TRUE) #get rid of "Color by group option" if checked
                    V$Choices[[V$Current]]$Symbols_dy <- choices$Symbols_dy #default choices
                    updateCheckboxGroupInput(session, "Symbols_dy", inline = TRUE,
                                             choices = choices$Symbols_dy,
                                             selected = current.symbols_dy)
                    V$Group.Sym.Color[[V$Current]] <- FALSE #using this to make the input$Group seletion a one-time action. otherwise you can't uncheck the "color by group" box
                    
                    
                    #Update symbol shape options
                    V$Choices[[V$Current]]$Sym.Shape_dy <- choices$Sym.Shape_dy
                    
                    updateSelectInput(session, "Sym.Shape_dy",
                                      choices = choices$Sym.Shape_dy,
                                      selected = choices$Sym.Shape_dy[1])
                    
                    
                    #Update line color options
                    current.lines_dy <- grep("group", input$Lines_dy, value = TRUE, invert = TRUE) #get rid of "Color by group option" if checked
                    V$Choices[[V$Current]]$Lines_dy <- choices$Lines_dy #default choices
                    updateCheckboxGroupInput(session, "Lines_dy", inline = TRUE,
                                             # choices = choices$Lines_dy,
                                             choices = V$Choices[[V$Current]]$Lines_dy,  #key!!??
                                             selected = current.lines_dy)
                    # selected = NULL)
                    V$Group.Line.Color[[V$Current]] <- FALSE #using this to make the input$Group seletion a one-time action. otherwise you can't uncheck the "color by group" box
                    
                    
                    #Update line type options
                    V$Choices[[V$Current]]$Line.Type_dy <- choices$Line.Type_dy
                    
                    updateSelectInput(session, "Line.Type_dy",
                                      choices = choices$Line.Type_dy,
                                      selected = choices$Line.Type_dy[1])
                    
                    # #Update Cat fill color options
                    # updateCheckboxGroupInput(session, "Cat.Color.Boxes", inline = TRUE,
                    #                          choices = choices$Cat.Color.Boxes,
                    #                          selected = NULL)
                    # V$Group.Cat.Color[[V$Current]] <- FALSE #using this to make the input$Group seletion a one-time action. otherwise you can't uncheck the "color by group" box
                    # 
                    
                    #Update model fit options
               
                    current.fit.lin <- grep("group", input$Fit.Linear, value = TRUE, invert = TRUE)
                    updateCheckboxGroupInput(session, "Fit.Linear", choices = c("Show results"),
                                             selected = current.fit.lin)
                    V$Group.Fit.Linear[[V$Current]] <- FALSE
                    
                    current.fit.qua <- grep("group", input$Fit.Quadratic, value = TRUE, invert = TRUE)
                    updateCheckboxGroupInput(session, "Fit.Quadratic", choices = c("Show results"),
                                             selected = current.fit.qua)
                    V$Group.Fit.Quadratic[[V$Current]] <- FALSE
                    
                    current.fit.pow <- grep("group", input$Fit.Power, value = TRUE, invert = TRUE)
                    updateCheckboxGroupInput(session, "Fit.Power", choices = c("Show results"),
                                             selected = current.fit.pow)
                    V$Group.Fit.Power[[V$Current]] <- FALSE
                    
                    current.fit.custom <- grep("group", input$Fit.Custom, value = TRUE, invert = TRUE)
                    updateCheckboxGroupInput(session, "Fit.Custom", choices = c("Show results"),
                                             selected = current.fit.custom)
                    V$Group.Fit.Custom[[V$Current]] <- FALSE
                    
                    
                    } else {
                        
                        switching <<- FALSE
                    }
                    
                }
                
            
            }
        
    })
    
    # debounce(group.obs, 1000)
    
    
    observeEvent(input$Do.Custom,{
        
        V$form[[V$Current]] <- input$Custom.Formula
        V$params[[V$Current]] <- input$Custom.Start
        
    })
    
    
    # # # Observe if bar plot
    observe({
        
        if (input$CatType == "Bar plot"){
            
            shinyjs::show("div.Error")
            
        } else {
            
            shinyjs::hide("div.Error")
        }
        
    })
    
    
    # # # Observe if Title
    observe({
        
        if (input$Title == ""){
            
            shinyjs::disable("div.Title.Position")
            
        } else {
            
            shinyjs::enable("div.Title.Position")
        }
        
    })
    
    # # # Observe subseting variable
    
    observe({
        
        if(V$ON){
            
            if (input$Subset_dy != "(none)"){
                
                # #need to refactor sometimes. might have to do with excel vs csv  - crashes on switch?!
                # V$Data[[V$Current]][,input$Subset_dy] <- factor(V$Data[[V$Current]][,input$Subset_dy])
                message(is.factor(V$Data[[V$Current]][,input$Subset_dy]))
                
                
                V$Choices[[V$Current]]$SubSel_dy <- levels(V$Data[[V$Current]][,input$Subset_dy])
                
                updateSelectInput(session,"SubSel_dy", "Subsetting groups", choices = V$Choices[[V$Current]]$SubSel_dy,
                                  selected = NULL)
                
                shinyjs::show("SubSel_dy")
                
            } else {
                
                V$Choices[[V$Current]]$SubSel_dy <- choices$SubSel_dy
                
                updateSelectInput(session,"SubSel_dy", "Subsetting groups", choices = choices$SubSel_dy,
                                  selected = NULL)
                
                shinyjs::hide("SubSel_dy")
            }
            
            
        }
    })
    
    
    # # # Observe if categorical X axis
    observe({
        
        if (input$X_dy != "(none)") {
            
            if (is.element(input$X_dy,V$Choices[[V$Current]]$Group_dy)){
                
                shinyjs::show("CatType")
                
            } else {
                
                shinyjs::hide("CatType")
                
            }
            
        } else {
            
            shinyjs::hide("CatType")
        }
        
        
    })
    
    # # # disable group spacing if no group color (x var = interaction)
    observe({
        
        if (is.null(input$Cat.Color.Boxes)){
            
            shinyjs::disable("div.Spacing.Group")
            
        } else {
            
            shinyjs::enable("div.Spacing.Group")
            
        }
        
    })
    
    
    
    # # # Display data table
    output$Data.Table <- renderDataTable({
        
        #Only display the table if there's at least one dataset
        if (V$ON){
            
            i <- V$Current
            
            V$Data[[i]]
            
        }
        
    }, options = list(paging = TRUE, pageLength = 15))
    
    
    # # # Main plot
    
    output$Figure <- renderPlot({
        
        
        #Interpret figure width and height inputs
        if (input$Width == ""){
            
            Width <- NULL
        } else {
            Width <- as.numeric(input$Width)
        }
        
        if (input$Height == ""){
            
            Height <- NULL
        } else {
            Height <- as.numeric(input$Height)
        }
        
        
        #Make plot
        if (input$X_dy == "(none)" | input$Y_dy == "(none)"){
            
            #plotly_empty() #empty plot if variables not selected
            return()
            
        } else {
            
            i <- V$Current
            
            D <- V$Data[[i]]
            
            D <- D[complete.cases(D[,input$X_dy], D[,input$Y_dy]),]
            
            #Subset data
            if (input$Subset_dy != "(none)" && !is.null(input$SubSel_dy)){
                
                D <- subset(D, is.element(D[,input$Subset_dy], input$SubSel_dy))
                
                D[,input$Subset_dy] <- factor (D[,input$Subset_dy])
            }
            
            #Transformations (X is conditional, below)
            if (is.element("Y", input$Trans.Log) && min(D[,input$Y_dy], na.rm=TRUE) > 0) D[,input$Y_dy] <- log(D[,input$Y_dy])
            if (is.element("Y", input$Trans.Std)) D[,input$Y_dy] <- scale(D[,input$Y_dy])
            
            
            #Define variables for easy reference (plot is made using data frame D)
            Y <- D[,input$Y_dy]
            Group <- input$Group_dy
            
            #Factor grouping variable
            if (Group != "(none)"){
                D[,Group] <- factor(D[,Group])
            }
            
            #Match symbol shape input to ggplot values
            fill.shapes <- any(grepl("Filled", input$Symbols_dy))
            i = (lookup$fill == fill.shapes) * (lookup$sym == input$Sym.Shape_dy)
            i = which (i == 1)
            shape = lookup$pch[i]
            
            
            #Y axis limits (X is conditional, below)
            if (input$ymin == ""){
                ymin = min(Y,na.rm=1) - diff(range(Y,na.rm=1))*.05
            } else {
                ymin = as.numeric(input$ymin)
            }
            
            if (input$ymax == ""){
                ymax = max(Y,na.rm=1) + diff(range(Y,na.rm=1))*.05
            } else {
                ymax = as.numeric(input$ymax)
            }
            
            
            #X is continuous
            if (is.numeric(D[,input$X_dy])){
                
                #Transformation
                if (is.element("X", input$Trans.Log) && min(D[,input$X_dy], na.rm=TRUE) > 0) D[,input$X_dy] <- log(D[,input$X_dy])
                if (is.element("X", input$Trans.Std)) D[,input$X_dy] <- scale(D[,input$X_dy])
                
                X <- D[,input$X_dy]
                
                #X axis limits
                if (input$xmin == ""){
                    xmin = min(X,na.rm=1) - diff(range(X,na.rm=1))*.05
                } else {
                    xmin = as.numeric(input$xmin)
                }
                
                if (input$xmax == ""){
                    xmax = max(X,na.rm=1) + diff(range(X,na.rm=1))*.05
                } else {
                    xmax = as.numeric(input$xmax)
                }
                
                
                
                #Lines
                if (any(grepl("Show", input$Lines_dy))){
                    
                    if (input$Line.Order == "X axis order") do_geom_line <- geom_line
                    if (input$Line.Order == "Dataset order") do_geom_line <- geom_path
                    
                    if (Group == "(none)") {
                        
                        gg_lines <- do_geom_line(color = input$Line.Color, linetype = gsub(" ","",tolower(input$Line.Type_dy)), size = input$Line.Size, alpha = 1-(input$Line.Trans/100))
                        gg_lines_lty_group <- NULL
                        # gg_lines_lty_group <- scale_linetype_manual(values = rep(tolower(lty),10))
                        
                    } else {
                        
                        
                        
                        #Linetype by grouping variable 
                        if (input$Line.Type_dy == "Grouping Variable") {
                            
                            if (any(grepl("group", input$Lines_dy))) { #color by group
                                
                                #Group lintype / group color
                                gg_lines <- do_geom_line(aes_string(color = Group, group = Group, linetype = Group), size = input$Line.Size, alpha = 1-(input$Line.Trans/100))
                                gg_lines_lty_group <- scale_linetype_manual(values = rep(gsub(" ","",tolower(lty)),10))
                                
                            } else {
                                
                                #Group lintype / single color
                                gg_lines <- do_geom_line(aes_string(group = Group, linetype = Group), color = input$Line.Color, size = input$Line.Size, alpha = 1-(input$Line.Trans/100))
                                gg_lines_lty_group <- scale_linetype_manual(values = rep(gsub(" ","",tolower(lty)),10))
                                
                            }
                            
                            #Single linetype    
                        } else {
                            
                            if (any(grepl("group", input$Lines_dy))) {
                                
                                #Single lintype / group color
                                gg_lines <- do_geom_line(aes_string(color = Group, group = Group), size = input$Line.Size, linetype = gsub(" ","",tolower(input$Line.Type_dy)), alpha = 1-(input$Line.Trans/100))
                                gg_lines_lty_group <- NULL
                                
                            } else {
                                
                                #Single lintype / single color
                                gg_lines <- do_geom_line(aes_string(group = Group), color = input$Line.Color, size = input$Line.Size, linetype = gsub(" ","",tolower(input$Line.Type_dy)), alpha = 1-(input$Line.Trans/100))
                                gg_lines_lty_group <- NULL
                                
                            }
                            
                        }
                        
                    }   
                    
                } else {
                    
                    gg_lines <- NULL
                    gg_lines_lty_group <- NULL
                    
                } #end lines 
                
                
                #Symbols
                # if (input$Symbols){
                if (any(grepl("Show", input$Symbols_dy))){
                    
                    #Shape by grouping variable 
                    if (input$Sym.Shape_dy == "Grouping Variable") {
                        
                        #set up shapes vector
                        if (any(grepl("Filled", input$Symbols_dy))) { #start vetor with filled shapes
                            gg_points_shape_group <- scale_shape_manual(values = rep(lookup$pch,10)[-(1:length(shapes))])
                        } else { #start vector with unfilled shapes
                            gg_points_shape_group <- scale_shape_manual(values = rep(lookup$pch,10))
                        }
                        
                        # if (input$Sym.Color_dy == "Grouping Variable") {
                        if (any(grepl("group", input$Symbols_dy))){
                            
                            gg_points <- geom_point(aes_string(color = Group, shape = Group), size = input$Sym.Size, stroke = input$Sym.Stroke, alpha = 1-(input$Sym.Trans/100))
                            # gg_points_shape_group <- scale_shape_manual(values = rep(lookup$pch,10))
                            
                        } else {
                            
                            gg_points <- geom_point(aes_string(shape = Group), color = input$Sym.Color, size = input$Sym.Size, stroke = input$Sym.Stroke, alpha = 1-(input$Sym.Trans/100))
                            # gg_points_shape_group <- scale_shape_manual(values = rep(lookup$pch,10))
                            
                        }
                        
                        #Single shape    
                    } else {
                        
                        # if (input$Sym.Color_dy == "Grouping Variable") {
                        if (any(grepl("group", input$Symbols_dy))){
                            gg_points <- geom_point(aes_string(color = Group), size = input$Sym.Size, shape = shape, stroke = input$Sym.Stroke, alpha = 1-(input$Sym.Trans/100))
                            gg_points_shape_group <- NULL
                            
                        } else {
                            
                            gg_points <- geom_point(color = input$Sym.Color, size = input$Sym.Size, shape = shape, stroke = input$Sym.Stroke, alpha = 1-(input$Sym.Trans/100))
                            gg_points_shape_group <- NULL
                            
                        }
                        
                    }
                    
                    
                } else {
                    
                    gg_points <- NULL
                    gg_points_shape_group <- NULL
                    
                } #end symbols  
                
                
                #Fit models
                
                #Linear model
                # if (any(grepl("model", input$Fit.Linear))) {
                if (any(grepl("Linear", input$Models))) {
                    
                    if (any(grepl("group", input$Fit.Linear)) && input$Group_dy != "(none)"){
                        
                        gg_fit_linear <- geom_smooth(aes_string(group = Group), method = 'lm', formula = y~x, se=FALSE, color = input$Color.Linear,
                                                     linetype = gsub(" ","",tolower(input$Type.Linear)),
                                                     size = input$Size.Linear)
                        
                    } else {
                    
                        gg_fit_linear <- geom_smooth(method = 'lm', formula = y~x, se=FALSE, color = input$Color.Linear,
                                                     linetype = gsub(" ","",tolower(input$Type.Linear)),
                                                     size = input$Size.Linear)
                    }
                    
                } else {
                    gg_fit_linear <- NULL
                }
                
                #Quadratic model
                if (any(grepl("Quadratic", input$Models))) {
                    
                    if (any(grepl("group", input$Fit.Quadratic)) && input$Group_dy != "(none)"){
                        
                        gg_fit_quadratic <- geom_smooth(aes_string(group = Group), method = 'lm', formula = y ~ x + I(x^2), se=FALSE, color = input$Color.Quadratic,
                                                     linetype = gsub(" ","",tolower(input$Type.Quadratic)),
                                                     size = input$Size.Quadratic)
                        
                    } else {
                        
                        gg_fit_quadratic <- geom_smooth(method = 'lm', formula = y ~ x + I(x^2), se=FALSE, color = input$Color.Quadratic,
                                                     linetype = gsub(" ","",tolower(input$Type.Quadratic)),
                                                     size = input$Size.Quadratic)
                    }
                    
                } else {
                    gg_fit_quadratic <- NULL
                }
                
                
                #Power model
                if (any(grepl("Power", input$Models))) {
                    
                    if (any(grepl("group", input$Fit.Power)) && input$Group_dy != "(none)"){
                        
                        gg_fit_power <- geom_smooth(aes_string(group = Group), method = 'nlsLM', formula = y~a*x^b, method.args = list(start = list(a = 1, b=1)),
                                                    se=FALSE, color = input$Color.Power,
                                                    linetype = gsub(" ","",tolower(input$Type.Power)),
                                                    size = input$Size.Power)
                        
                    } else {
                        
                        gg_fit_power <- geom_smooth(method = 'nlsLM', formula = y~a*x^b, method.args = list(start = list(a = 1, b=1)),
                                                    se=FALSE, color = input$Color.Power,
                                                    linetype = gsub(" ","",tolower(input$Type.Power)),
                                                    size = input$Size.Power)
                    }
                    
                } else {
                    gg_fit_power <- NULL
                }
                
                
                
                
                
                #Custom model
                if (any(grepl("Custom", input$Models)) && V$form[[V$Current]] != "not fitted") {
                    
                    # form <- gsub("x", "x", tolower(input$Custom.Formula)) #make x's lowercase
                    form <- gsub("x", "x", tolower(V$form[[V$Current]])) #make x's lowercase
                    
                    form <- as.formula(paste("y ~", form)) # add "y ~" to make formula
                    
                    params <- eval(parse(text = paste("list(", V$params[[V$Current]], ")"))) #make starting values list
                    
                    
                    if (any(grepl("group", input$Fit.Custom)) && input$Group_dy != "(none)"){

                        gg_fit_custom <- geom_smooth(aes_string(group = Group), method = 'nlsLM', formula = form, method.args = list(start = params),
                                                    se=FALSE, color = input$Color.Custom,
                                                    linetype = gsub(" ","",tolower(input$Type.Custom)),
                                                    size = input$Size.Custom)

                    } else {

                        gg_fit_custom <- geom_smooth(method = 'nlsLM', formula = form, method.args = list(start = params),
                                                    se=FALSE, color = input$Color.Custom,
                                                    linetype = gsub(" ","",tolower(input$Type.Custom)),
                                                    size = input$Size.Custom)
                    }
                    
                } else {
                    gg_fit_custom <- NULL
                }
                
                
                # if (input$Fit.Power) {
                #     
                #     gg_fit_power <- geom_smooth(method = 'nls', formula = y~a*x^b, se=FALSE, method.args = list(start = list(a = 1, b=1), control = list(maxiter = 50000)))
                #     
                # } else {
                #     gg_fit_power <- NULL
                # }
                    
               
                
                # Start ggplot
                fig <- ggplot(D, aes_string(x = input$X_dy, y = input$Y_dy)) +
                    
                    gg_lines +
                    gg_lines_lty_group +
                    gg_points +
                    gg_points_shape_group +
                    gg_fit_power +
                    gg_fit_linear +
                    gg_fit_quadratic +
                    gg_fit_custom +
                    
                    # {if (is.element("Linear", input$Fit.Models)) geom_smooth(method = 'lm', formula = y ~ x, se = FALSE)} +
                    # {if (is.element("Quadratic", input$Fit.Models)) geom_smooth(method = 'lm', formula = y ~ x + I(x^2), se = FALSE)} +
                    # {if (input$Fit.Power) gg_fit_power} +
                    
                    {if (input$xlab != "") xlab(input$xlab)} +
                    {if (input$ylab != "") ylab(input$ylab)} +
                    {if (any(grepl("group", c(input$Symbols_dy, input$Lines_dy)))) eval(parse(text = as.character(pals[which(pals[,1] == input$Scatter.Color.Theme),2])))} +
                    
                    
                    # scale_x_continuous(limits = c(xmin,xmax), expand = c(0, 0)) +
                    # scale_y_continuous(limits = c(ymin,ymax), expand = c(0, 0)) +
                    coord_cartesian(xlim=c(xmin, xmax), ylim=c(ymin, ymax), expand = FALSE)
                # theme(legend.title=element_blank())
                # xlim(c(xmin,xmax)) +
                # ylim(c(ymin,ymax)) +
                # guides(fill=guide_legend(title=NULL))
                
                # # # X is categorical   
            } else {
                
                
                D[,input$X_dy] <- factor(D[,input$X_dy])  #get rid of any "empty" levels
                
                if (input$Error == "Standard Deviation") fun.error <- mean_sd #function defined in global vars
                if (input$Error == "Standard Error") fun.error <- mean_se #built-in ggplot function
                if (input$Error == "(none)") fun.error <- no_error #function defined in global vars
                
                
                #deal with sspacing
                Spacing.X <- input$Spacing.X #slider
                Spacing.X <- Spacing.X*.5 #value of 100% (1) pushes groups to absolute edge of axis (not visible)
                Spacing.X <- (1-(Spacing.X/100))
                
                Spacing.Group <- input$Spacing.Group
                
                rescale <- function(x, new.range) {
                    old.range <- range(x)
                    (x - old.range[1])/diff(old.range) * diff(new.range) + new.range[1]
                }
                
                Spacing.Group <- rescale(c(0, Spacing.Group/100, 1), new.range = c(Spacing.X, 1))[2]
                
                
                #start ggplot object
                fig <- ggplot(D, aes_string(x = input$X_dy, y = input$Y_dy))
                
                
                #1a - no grouping variable
                if (Group == "(none)"){
                    #box, bar, violin
                    
                    if (input$CatType == "Bar plot"){
                        gg_cat <- geom_bar(width = Spacing.X, stat = "summary", fun.y = mean, color = input$Edge.Color, fill = input$Fill.Color, size = input$Cat.Edge)
                        gg_error <- stat_summary(geom = "errorbar", position = "dodge", fun.data = fun.error, width = input$Error.Cap/100, size = input$Cat.Edge)
                    }
                    
                    if (input$CatType == "Box plot"){
                        gg_cat <- geom_boxplot(width = Spacing.X, color = input$Edge.Color, fill = input$Fill.Color, size = input$Cat.Edge)
                        gg_error <- NULL
                    }
                    
                    if (input$CatType == "Violin plot"){
                        gg_cat <- geom_violin(width = Spacing.X, color = input$Edge.Color, fill = input$Fill.Color, draw_quantiles = c(.5), size = input$Cat.Edge)
                        gg_error <- NULL
                    }
                    
                    
                    
                    #1b - grouping variable
                } else {
                    
                    # if (input$CatType == "Bar plot"){
                    #     gg_cat <- geom_bar(aes_string(fill = Group), width = Spacing.X, position = position_dodge(Spacing.Group), stat = "summary", fun.y = mean)
                    #     gg_error <- stat_summary(geom = "errorbar", aes_string(group = Group), position = position_dodge(Spacing.Group), fun.data = fun.error, width = input$Error.Cap/100)
                    # }
                    # 
                    # if (input$CatType == "Box plot"){
                    #     gg_cat <- geom_boxplot(aes_string(fill = Group), width = Spacing.X, position = position_dodge(Spacing.Group))
                    #     gg_error <- NULL
                    # }
                    # 
                    # if (input$CatType == "Violin plot"){
                    #     gg_cat <- geom_violin(aes_string(fill = Group), width = Spacing.X, position = position_dodge(Spacing.Group), draw_quantiles = c(.5))
                    #     gg_error <- NULL
                    # }
                    
                    
                    #2a - fill color grouped
                    if (any(grepl("Fill", input$Cat.Color.Boxes))){
                        
                        #3a - edge color grouped
                        if (any(grepl("Edge", input$Cat.Color.Boxes))){
                            
                            #Fill grouped / Edge grouped
                            if (input$CatType == "Bar plot"){
                                gg_cat <- geom_bar(aes_string(fill = Group, color = Group), width = Spacing.X, position = position_dodge(Spacing.Group), stat = "summary", fun.y = mean, size = input$Cat.Edge)
                                gg_error <- stat_summary(geom = "errorbar", aes_string(group = Group, color = Group), position = position_dodge(Spacing.Group), fun.data = fun.error, width = input$Error.Cap/100, size = input$Cat.Edge)
                            }
                            
                            if (input$CatType == "Box plot"){
                                gg_cat <- geom_boxplot(aes_string(fill = Group, color = Group), width = Spacing.X, position = position_dodge(Spacing.Group), size = input$Cat.Edge)
                                gg_error <- NULL
                            }
                            
                            if (input$CatType == "Violin plot"){
                                gg_cat <- geom_violin(aes_string(fill = Group, color = Group), width = Spacing.X, position = position_dodge(Spacing.Group), draw_quantiles = c(.5), size = input$Cat.Edge)
                                gg_error <- NULL
                            }  
                            
                            #3b - edge color not grouped
                        } else {
                            
                            #Fill grouped / Edge not grouped
                            if (input$CatType == "Bar plot"){
                                gg_cat <- geom_bar(aes_string(fill = Group), color = input$Edge.Color, width = Spacing.X, position = position_dodge(Spacing.Group), stat = "summary", fun.y = mean, size = input$Cat.Edge)
                                gg_error <- stat_summary(geom = "errorbar", aes_string(group = Group), color = input$Edge.Color, position = position_dodge(Spacing.Group), fun.data = fun.error, width = input$Error.Cap/100, size = input$Cat.Edge)
                            }
                            
                            if (input$CatType == "Box plot"){
                                gg_cat <- geom_boxplot(aes_string(fill = Group), color = input$Edge.Color, width = Spacing.X, position = position_dodge(Spacing.Group), size = input$Cat.Edge)
                                gg_error <- NULL
                            }
                            
                            if (input$CatType == "Violin plot"){
                                gg_cat <- geom_violin(aes_string(fill = Group), color = input$Edge.Color, width = Spacing.X, position = position_dodge(Spacing.Group), draw_quantiles = c(.5), size = input$Cat.Edge)
                                gg_error <- NULL
                            }  
                            
                        }
                        
                        
                        
                        #2b - fill color not grouped
                    } else { 
                        
                        #4a - edge color grouped
                        if (any(grepl("Edge", input$Cat.Color.Boxes))){
                            
                            #Fill not grouped / Edge grouped
                            if (input$CatType == "Bar plot"){
                                gg_cat <- geom_bar(aes_string(color = Group), fill = input$Fill.Color, width = Spacing.X, position = position_dodge(Spacing.Group), stat = "summary", fun.y = mean, size = input$Cat.Edge)
                                gg_error <- stat_summary(geom = "errorbar", aes_string(group = Group, color = Group), position = position_dodge(Spacing.Group), fun.data = fun.error, width = input$Error.Cap/100, size = input$Cat.Edge)
                            }
                            
                            if (input$CatType == "Box plot"){
                                gg_cat <- geom_boxplot(aes_string(color = Group), fill = input$Fill.Color, width = Spacing.X, position = position_dodge(Spacing.Group), size = input$Cat.Edge)
                                gg_error <- NULL
                            }
                            
                            if (input$CatType == "Violin plot"){
                                gg_cat <- geom_violin(aes_string(color = Group), fill = input$Fill.Color, width = Spacing.X, position = position_dodge(Spacing.Group), draw_quantiles = c(.5), size = input$Cat.Edge)
                                gg_error <- NULL
                            }  
                            
                            #4b - edge color not grouped
                        } else {
                            
                            #Need to redefine ggplot with new x variable based on interaction between x and group
                            D$interaction_for_gg <- interaction(D[,input$X_dy], D[,Group], sep = " / ")
                            new.x <- "interaction_for_gg"
                            
                            fig <- ggplot(D, aes_string(x = new.x, y = input$Y_dy))
                            
                            #Fill not grouped / Edge not grouped - MAYBE NOT POSSIBLE?  NEED SOMETHING TO GROUP BY
                            if (input$CatType == "Bar plot"){
                                gg_cat <- geom_bar(fill = input$Fill.Color, color = input$Edge.Color, width = Spacing.X, position = position_dodge(Spacing.Group), stat = "summary", fun.y = mean, size = input$Cat.Edge)
                                gg_error <- stat_summary(geom = "errorbar", color = input$Edge.Color, position = position_dodge(Spacing.Group), fun.data = fun.error, width = input$Error.Cap/100, size = input$Cat.Edge)
                            }
                            
                            if (input$CatType == "Box plot"){
                                gg_cat <- geom_boxplot(fill = input$Fill.Color, color = input$Edge.Color, width = Spacing.X, position = position_dodge(Spacing.Group), size = input$Cat.Edge)
                                gg_error <- NULL
                            }
                            
                            if (input$CatType == "Violin plot"){
                                gg_cat <- geom_violin(fill = input$Fill.Color, color = input$Edge.Color, width = Spacing.X, position = position_dodge(Spacing.Group), draw_quantiles = c(.5), size = input$Cat.Edge)
                                gg_error <- NULL
                            }  
                            
                        }
                    }
                }
                
                #if (input$Error == "(none)") gg_error <- NULL #easiest way to get rid of error bars (calculated above, but then removed)
                
                fig <- fig +
                    gg_cat +
                    gg_error +
                    
                    {if (any(grepl("Edge", input$Cat.Color.Boxes))) eval(parse(text = as.character(pals[which(pals[,1] == input$Cat.Color.Theme),2])))} +
                    {if (any(grepl("Fill", input$Cat.Color.Boxes))) eval(parse(text = as.character(pals[which(pals[,1] == input$Cat.Color.Theme),3])))} +
                    {if (input$xlab != "") xlab(input$xlab)} +
                    {if (input$xlab == "" && is.null(input$Cat.Color.Boxes)) xlab("")} + #gets ride of x label if using interaction column
                    {if (input$ylab != "") ylab(input$ylab)} +
                    coord_cartesian(expand = FALSE) #sets ylim and xlim to exactly bound contents (temporarily)
                
                #gets current plot xlim and ylim (exact with no padding)
                Xlim <- ggplot_build(fig)$layout$panel_ranges[[1]]$x.range
                Ylim <- ggplot_build(fig)$layout$panel_ranges[[1]]$y.range
                
                #x axis padding
                Xlim <- c(Xlim[1] - diff(Xlim)*(input$xpad/100)*.5, Xlim[2] + diff(Xlim)*(input$xpad/100)*.5)
                
                #y axis scaling
                if (input$ymin == ""){
                    
                    if (input$CatType == "Bar plot"){
                        
                        if (Ylim[1] >= 0) Ylim[1] <- 0
                        if (Ylim[1] < 0) {
                            Ylim[1] <- Ylim[1] - diff(Ylim)*.06
                            fig <- fig + geom_hline(yintercept = 0)
                        }
                        
                    } else {
                        
                        Ylim[1] <- Ylim[1] - diff(Ylim)*.06
                    }
                    
                } else {
                    Ylim[1] <- as.numeric(input$ymin)
                }
                
                if (input$ymax == ""){
                    
                    if (input$CatType == "Bar plot"){
                        
                        if (Ylim[2] <= 0) Ylim[2] <- 0
                        if (Ylim[2] > 0) {
                            Ylim[2] <- Ylim[2] + diff(Ylim)*.06
                            fig <- fig + geom_hline(yintercept = 0)
                        }
                        
                    } else {
                        
                        Ylim[2] <- Ylim[2] + diff(Ylim)*.06
                    }
                    
                } else {
                    Ylim[2] <- as.numeric(input$ymax)
                }
                
                #apply axis limits
                fig <- fig +
                    coord_cartesian(xlim = Xlim, ylim = Ylim, expand = FALSE)
                
                
                
            }
            
            # #Axes text etc.
            #  theme(axis.text.x = element_text(colour="grey20",size=20,angle=90,hjust=.5,vjust=.5,face="plain"),
            #        axis.text.y = element_text(colour="grey20",size=12,angle=0,hjust=1,vjust=0,face="plain"),  
            #        axis.title.x = element_text(colour="grey20",size=12,angle=0,hjust=.5,vjust=0,face="plain"),
            #        axis.title.y = element_text(colour="grey20",size=12,angle=90,hjust=.5,vjust=.5,face="plain"))
            
            # i.pos <- which(input$Legend.Position == legend.position$label)
            # 
            # if (grepl("Out", input$Legend.Position)){
            # 
            #     position <- as.character(legend.position$outside[i.pos])
            # 
            # } else {
            # 
            #     position <- c(legend.position$in.x[i.pos],legend.position$in.x[i.pos])
            # 
            # }
            
            
            
            fig <- fig +
                # theme_few() +
                theme_base() +
                theme(plot.background = element_rect(fill = "white", colour = NA)) +
                theme(axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0))) +
                theme(axis.title.x = element_text(margin = margin(t = 15, r = 0, b = 0, l = 0))) +
                theme(plot.title = element_text(margin = margin(t = 0, r = 0, b = 20, l = 0))) +
                theme(axis.text = element_text(size = input$Size.Tick*13)) +
                theme(axis.title = element_text(size = input$Size.Lab*16)) +
                theme(title = element_text(size = input$Size.Title*18)) +
                
                theme(legend.title = element_text(size = input$Size.Legend*16)) +
                theme(legend.text = element_text(size = input$Size.Legend*13)) +
                
                theme(legend.position = tolower(input$Legend.Position)) +
                
                # theme(legend.position = position, legend.justification=c(1,1)) +
                
                theme(legend.key.size = unit(1.2*(input$Legend.Key.Size/100),"line")) +
                
                {if (input$xlab.Rotate) theme(axis.text.x = element_text(angle = 90, vjust = .5))} +
                
                {if (is.null(input$Legend)) theme(legend.position = "none")} +
                
                #iffy way to force the legend name to apply to any scaling type from above (not sure why I need to use "discrete")
                {if (input$Legend.Title != "") scale_fill_discrete(name = input$Legend.Title)} +
                {if (input$Legend.Title != "") scale_colour_discrete(name = input$Legend.Title)} +
                {if (input$Legend.Title != "") scale_shape_discrete(name = input$Legend.Title)} +
                {if (input$Legend.Title != "") scale_linetype_discrete(name = input$Legend.Title)} +
                
                {if (input$Title != "") ggtitle(input$Title)} +
                {if (input$Title.Position == "Left") theme(plot.title = element_text(hjust = 0))} +
                {if (input$Title.Position == "Center") theme(plot.title = element_text(hjust = .5))} +
                {if (input$Title.Position == "Right") theme(plot.title = element_text(hjust = 1))}
            
            
            V$Fig <- fig
            
            fig
            
        }  
        
    }, height = function(){
        
        
        if (input$Height == ""){
            
            return("auto")
            
        } else if (as.numeric(input$Height) < 200){
            
            return(200)
            
        } else {
            
            as.numeric(input$Height)
        }
        
        
        
    }, width = function(){
        
        if (input$Width == ""){
            
            return("auto")
            
            # } else if (grepl("%", input$Width)){
            #     
            #     return(input$Width)
            
        } else if (as.numeric(input$Width) < 200){
            
            return(200)
            
        } else {
            
            return(as.numeric(input$Width))
        }
        
    })
    
    
    output$Saved.Figs <- renderPlot({
        
        if (!is.null(V$Fig)) {
            
            # V$Fig
            # V$Figs[[V$Current]][[as.numeric(input$Saved.Figs_dy)]]
            V$Figs[[1]][[input$Saved.Figs_dy]]
        }
        
    }, height = function(){
        
        
        if (input$Saved.Height_dy == ""){
            
            return("auto")
            
        } else if (as.numeric(input$Saved.Height_dy) < 200){
            
            return(200)
            
        } else {
            
            as.numeric(input$Saved.Height_dy)
        }
        
    }, width = function(){
        
        if (input$Saved.Width_dy == ""){
            
            return("auto")
            
        } else if (as.numeric(input$Saved.Width_dy) < 200){
            
            return(200)
            
        } else {
            
            as.numeric(input$Saved.Width_dy)
        }
        
    })
    
    observeEvent(input$Save.Fig,{
        
        if (!is.null(V$Fig)) {
            
            #to have separate galleries for each dataset, replace [[1]] with [[V$Current]]
            
            i <- length(V$Figs[[1]]) + 1
            
            V$Figs[[1]][[i]] <- V$Fig
            
            names(V$Figs[[1]])[i] <- paste("(", i, ") ", input$Fig.Name, sep = "")
            
            updateSelectInput(session,"Saved.Figs_dy", choices = names(V$Figs[[1]]), selected = names(V$Figs[[1]])[i])
            
            updateTextInput(session, "Fig.Name", value = sels$Fig.Name)
            
            V$Figs.dim[[1]][[i]] <- c(input$Width, input$Height)
            
            
            # updateSelectInput(session,"Saved.Figs_dy", choices = as.character(1:i))
            
        }
        
    })
    
    
    observe({
        
        if (input$Saved.Figs_dy != "(none)"){
            
            i <- which(input$Saved.Figs_dy == names(V$Figs[[1]]))
            
            updateTextInput(session, "Saved.Width_dy", value = V$Figs.dim[[1]][[i]][1])
            updateTextInput(session, "Saved.Height_dy", value = V$Figs.dim[[1]][[i]][2])
            
            # if (input$Saved.Width != V$Figs.dim[[1]][[i]][1]) V$Figs.dim[[1]][[i]][1] <- input$Saved.Width
            # if (input$Saved.Height != V$Figs.dim[[1]][[i]][2]) V$Figs.dim[[1]][[i]][2] <- input$Saved.Height
            
        }
        
        
    })
    
    #Model panels observer
    observe({
        
        if (any(grepl("Linear", input$Models))){
            shinyjs::show("div.Linear")            
        } else {
            shinyjs::hide("div.Linear")
        }    
            
        if (any(grepl("Quadratic", input$Models))){
            shinyjs::show("div.Quadratic")            
        } else {
            shinyjs::hide("div.Quadratic")
        }    
        
        if (any(grepl("Power", input$Models))){
            shinyjs::show("div.Power")            
        } else {
            shinyjs::hide("div.Power")
        }    
        
        if (any(grepl("Custom", input$Models))){
            shinyjs::show("div.Custom")            
        } else {
            shinyjs::hide("div.Custom")
        }
        
        
    })
    
    
    
    #Model results observer
    observe({
        
        if (any(grepl("results", input$Fit.Linear)) && any(grepl("Linear", input$Models))){
            
            shinyjs::show("div.Linear.Results")            
        } else {
            shinyjs::hide("div.Linear.Results")    
            
        }
        
        if (any(grepl("results", input$Fit.Quadratic)) && any(grepl("Quadratic", input$Models))){
            
            shinyjs::show("div.Quadratic.Results")            
        } else {
            shinyjs::hide("div.Quadratic.Results")    
            
        }
        
        if (any(grepl("results", input$Fit.Power)) && any(grepl("Power", input$Models))){
            
            shinyjs::show("div.Power.Results")            
        } else {
            shinyjs::hide("div.Power.Results")    
            
        }
        
        if (any(grepl("results", input$Fit.Custom)) && any(grepl("Custom", input$Models))){

            shinyjs::show("div.Custom.Results")
        } else {
            shinyjs::hide("div.Custom.Results")

        }
        
    })
    
    #Linear model output
    output$Linear.Table <- renderTable({
        
        
        #only if x and y are selected
        if (input$X_dy != "(none)" & input$Y_dy != "(none)" & any(grepl("Linear", input$Models)) & any(grepl("results", input$Fit.Linear))){
            
            Data <- V$Data[[V$Current]]
            
            #Subset data
            if (input$Subset_dy != "(none)" && !is.null(input$SubSel_dy)){
                
                Data <- subset(Data, is.element(Data[,input$Subset_dy], input$SubSel_dy))
                
                Data[,input$Subset_dy] <- factor (Data[,input$Subset_dy])
            }
            
            X <- Data[,input$X_dy]
            Y <- Data[,input$Y_dy]
            
            D <- data.frame(X = X, Y = Y)
            
            #add in grouping variable
            if (any(grepl("group", input$Fit.Linear))) {
                Group <- TRUE
                D$Group <- Data[,input$Group_dy]
                D <- D[complete.cases(D$X, D$Y, D$Group),]
                D$Group <- factor(D$Group)
            } else {
                Group <- FALSE
                D <- D[complete.cases(D$X, D$Y),]
            }
            
            
            fit <- list()
            
            if (!Group){
                
                fit[[1]] <- lm(Y ~ X, data = D)
                
                # tab <- data.frame(NO.NAME = "Parameter values", a = signif(fit[[1]]$coefficients[1],3), b = signif(fit[[1]]$coefficients[2],3))
                tab <- data.frame(NO.NAME = "Parameter values",
                                  a = sprintf("%5.4g", fit[[1]]$coefficients[1]),
                                  b = sprintf("%5.4g", fit[[1]]$coefficients[2]))
                
                
                names(tab)[1] <- ""
                
                
            } else {
                
                n <- nlevels(D$Group)
                
                # tab <- data.frame(Group = character(n), a = numeric(n), b = numeric(n),
                #                   stringsAsFactors = FALSE)
                
                tab <- data.frame(Group = character(n), a = character(n), b = character(n),
                                  stringsAsFactors = FALSE)
                
                for (i in 1:n){
                    
                    d <- subset(D, Group == levels(D$Group)[i])
                    
                    fit[[i]] <- lm(Y ~ X, data = d)
                    
                    tab$Group[i] <- levels(D$Group)[i]
                    
                    tab$a[i] <- sprintf("%5.4g", fit[[i]]$coefficients[1])
                    tab$b[i] <- sprintf("%5.4g", fit[[i]]$coefficients[2])
                    
                    # tab$a[i] <- signif(fit[[i]]$coefficients[1],5)
                    # tab$b[i] <- signif(fit[[i]]$coefficients[2],5)
                    
                }
                
            }
            
            return(tab)
            
        }  else {
            
            return()
        }
        
        
    })
    
    
    
    #Polynomial model output
    output$Poly.Table <- renderTable({
        
        #only if x and y are selected
        if (input$X_dy != "(none)" & input$Y_dy != "(none)" & any(grepl("Quadratic", input$Models)) & any(grepl("results", input$Fit.Quadratic))){
            
            Data <- V$Data[[V$Current]]
            
            #Subset data
            if (input$Subset_dy != "(none)" && !is.null(input$SubSel_dy)){
                
                Data <- subset(Data, is.element(Data[,input$Subset_dy], input$SubSel_dy))
                
                Data[,input$Subset_dy] <- factor (Data[,input$Subset_dy])
            }
            
            X <- Data[,input$X_dy]
            Y <- Data[,input$Y_dy]
            
            D <- data.frame(X = X, Y = Y)
            
            #add in grouping variable
            if (any(grepl("group", input$Fit.Quadratic))) {
                Group <- TRUE
                D$Group <- Data[,input$Group_dy]
                D <- D[complete.cases(D$X, D$Y, D$Group),]
                D$Group <- factor(D$Group)
            } else {
                Group <- FALSE
                D <- D[complete.cases(D$X, D$Y),]
            }
            
            ord <- 2 #in place for higher order polynomials
            
            fit <- list()
            
            if (!Group){
                
                fit[[1]] <- lm(Y ~ poly(X, ord, raw = TRUE), data = D)
                
                tab <- data.frame(NO.NAME = "Parameter values")
                names(tab)[1] <- ""
                
                for (j in 1:(ord+1)){ #stucture in place for higher order polynomials
                    
                    # tab[1, letters[j]] <- signif(fit[[1]]$coefficients[j],5)
                    tab[1, letters[j]] <- sprintf("%5.4g", fit[[1]]$coefficients[j])
                    
                }
                
            } else {
                
                n <- nlevels(D$Group)
                
                tab <- data.frame(Group = character(n), stringsAsFactors = FALSE)
                
                for (i in 1:n){
                    
                    d <- subset(D, Group == levels(D$Group)[i])
                    
                    fit[[i]] <- lm(Y ~ poly(X, ord, raw = TRUE), data = d)
                    
                    tab$Group[i] <- levels(D$Group)[i]
                    
                    for (j in 1:3){ #stucture in place for higher order polynomials
                        
                        # tab[i, letters[j]] <- signif(fit[[i]]$coefficients[j],5)
                        tab[i, letters[j]] <- sprintf("%5.4g", fit[[i]]$coefficients[j])
                        
                    }
                    
                }
                
            }
            
            return(tab)
            
        }  else {
            
            return()
        }
        
        
    })
    
    
    #Power model output
    output$Power.Table <- renderTable({
        
        #only if x and y are selected
        if (input$X_dy != "(none)" & input$Y_dy != "(none)" & any(grepl("Power", input$Models)) & any(grepl("results", input$Fit.Power))){
            
            Data <- V$Data[[V$Current]]
            
            #Subset data
            if (input$Subset_dy != "(none)" && !is.null(input$SubSel_dy)){
                
                Data <- subset(Data, is.element(Data[,input$Subset_dy], input$SubSel_dy))
                
                Data[,input$Subset_dy] <- factor (Data[,input$Subset_dy])
            }
            
            X <- Data[,input$X_dy]
            Y <- Data[,input$Y_dy]
            
            D <- data.frame(X = X, Y = Y)
            
            #add in grouping variable
            if (any(grepl("group", input$Fit.Power))) {
                Group <- TRUE
                D$Group <- Data[,input$Group_dy]
                D <- D[complete.cases(D$X, D$Y, D$Group),]
                D$Group <- factor(D$Group)
            } else {
                Group <- FALSE
                D <- D[complete.cases(D$X, D$Y),]
            }
            
            
            fit <- list()
            
            if (!Group){
                
                fit[[1]] <- nlsLM(Y ~ a*X^b, data = D, start = list(a = 1, b=1))
                
                # tab <- data.frame(NO.NAME = "Parameter values", a = signif(fit[[1]]$coefficients[1],3), b = signif(fit[[1]]$coefficients[2],3))
                tab <- data.frame(NO.NAME = "Parameter values",
                                  a = sprintf("%5.4g", coefficients(fit[[1]])[1]),
                                  b = sprintf("%5.4g", coefficients(fit[[1]])[2]))
                
                
                names(tab)[1] <- ""
                
                
            } else {
                
                n <- nlevels(D$Group)
                
                # tab <- data.frame(Group = character(n), a = numeric(n), b = numeric(n),
                #                   stringsAsFactors = FALSE)
                
                tab <- data.frame(Group = character(n), a = character(n), b = character(n),
                                  stringsAsFactors = FALSE)
                
                for (i in 1:n){
                    
                    d <- subset(D, Group == levels(D$Group)[i])
                    
                    fit[[i]] <- nlsLM(Y ~ a*X^b, data = d, start = list(a = 1, b=1))
                    
                    tab$Group[i] <- levels(D$Group)[i]
                    
                    tab$a[i] <- sprintf("%5.4g", coefficients(fit[[i]])[1])
                    tab$b[i] <- sprintf("%5.4g", coefficients(fit[[i]])[2])
                    
                    # tab$a[i] <- signif(fit[[i]]$coefficients[1],5)
                    # tab$b[i] <- signif(fit[[i]]$coefficients[2],5)
                    
                }
                
            }
            
            return(tab)
            
        }  else {
            
            return()
        }
        
        
    })
    
    
    #Custom model output
    output$Custom.Table <- renderTable({
        
        #only if x and y are selected
        if (input$X_dy != "(none)" & input$Y_dy != "(none)" & any(grepl("Custom", input$Models)) & any(grepl("results", input$Fit.Custom)) & V$form[[V$Current]] != "not fitted"){
            
            Data <- V$Data[[V$Current]]
            
            #Subset data
            if (input$Subset_dy != "(none)" && !is.null(input$SubSel_dy)){
                
                Data <- subset(Data, is.element(Data[,input$Subset_dy], input$SubSel_dy))
                
                Data[,input$Subset_dy] <- factor (Data[,input$Subset_dy])
            }
            
            x <- Data[,input$X_dy]
            y <- Data[,input$Y_dy]
            
            D <- data.frame(x = x, y = y)
            
            #add in grouping variable
            if (any(grepl("group", input$Fit.Custom))) {
                Group <- TRUE
                D$Group <- Data[,input$Group_dy]
                D <- D[complete.cases(D$x, D$y, D$Group),]
                D$Group <- factor(D$Group)
            } else {
                Group <- FALSE
                D <- D[complete.cases(D$x, D$y),]
            }
            
            form <- gsub("x", "x", tolower(V$form[[V$Current]])) #make X's lowercase (otherwise exp issues)
            form <- as.formula(paste("y ~",form)) # add "Y ~" to make formula
            
            params <- eval(parse(text = paste("list(", V$params[[V$Current]], ")"))) #make starting values list
            
            n.params <- length(params)
            message(n.params)
            
            fit <- list()
            
            if (!Group){
                
                fit[[1]] <- nlsLM(form, data = D, start = params)
                
                # fit[[1]] <- nls(Y ~ a*X^b, data = D, start = list(a = 1, b=1), control = list(maxiter = 50000))
                
                # tab <- data.frame(NO.NAME = "Parameter values", a = signif(fit[[1]]$coefficients[1],3), b = signif(fit[[1]]$coefficients[2],3))
                
                tab <- data.frame(NO.NAME = "Parameter values")
                names(tab)[1] <- ""
                
                message(names(params))
                
                for (j in 1:n.params){ #stucture in place for higher order polynomials
                    
                    # tab[1, letters[j]] <- signif(fit[[1]]$coefficients[j],5)
                    tab[1, names(params)[j]] <- sprintf("%5.4g", coefficients(fit[[1]])[j])
                    
                }
               
                
            } else {
                
                n <- nlevels(D$Group)
                
                # tab <- data.frame(Group = character(n), a = numeric(n), b = numeric(n),
                #                   stringsAsFactors = FALSE)
                
                # tab <- data.frame(Group = character(n), a = character(n), b = character(n),
                #                   stringsAsFactors = FALSE)
                
                tab <- data.frame(Group = character(n), stringsAsFactors = FALSE)
                
                for (i in 1:n){
                    
                    d <- subset(D, Group == levels(D$Group)[i])
                    
                    fit[[i]] <- nlsLM(form, data = d, start = params)
                    
                    tab$Group[i] <- levels(D$Group)[i]
                    
                    for (j in 1:n.params){ #stucture in place for higher order polynomials
                        
                        # tab[i, letters[j]] <- signif(fit[[i]]$coefficients[j],5)
                        tab[i, names(params)[j]] <- sprintf("%5.4g", coefficients(fit[[i]])[j])
                        
                    }
                    
                    # tab$a[i] <- signif(fit[[i]]$coefficients[1],5)
                    # tab$b[i] <- signif(fit[[i]]$coefficients[2],5)
                    
                }
                
            }
            
            return(tab)
            
        }  else {
            
            return()
        }
        
        
    })
    
    
    
    # #Linear model results
    # output$Linear.Results <- renderPrint({
    #     
    #     #only if x and y are selected
    #     if (input$X_dy != "(none)" & input$Y_dy != "(none)" & any(grepl("Linear", input$Models)) & any(grepl("results", input$Fit.Linear))){
    #         
    #         X <- V$Data[[V$Current]][,input$X_dy]
    #         Y <- V$Data[[V$Current]][,input$Y_dy]
    #         
    #         D <- data.frame(X = X, Y = Y)
    #         
    #         #add in grouping variable
    #         if (any(grepl("group", input$Fit.Linear))) {
    #             Group <- TRUE
    #             D$Group <- V$Data[[V$Current]][,input$Group_dy]
    #             D <- D[complete.cases(D$X, D$Y, D$Group),]
    #         } else {
    #             Group <- FALSE
    #             D <- D[complete.cases(D$X, D$Y),]
    #         }
    #     
    #             
    #             fit <- list()
    #             
    #             if (!Group){
    #                 
    #                 fit[[1]] <- lm(Y ~ X, data = D)
    #                 
    #                 sum.fit <- paste("\t\t",            "\t", "\t", "a",                                    "\t\t", "b",         "\n",
    #                                    "Parameter values","\t", "\t", signif(fit[[1]]$coefficients[1],3), "\t\t", signif(fit[[1]]$coefficients[2],3),
    #                                    sep="")
    #                 
    #             } else {
    #                 
    #                 sum.fit <- paste("Group", "\t\t",            "\t", "\t", "a",                                    "\t\t", "b", sep="")
    #                 
    #                 for (i in 1:nlevels(D$Group)){
    #                     
    #                     d <- subset(D, Group == levels(D$Group)[i])
    #                     
    #                     fit[[i]] <- lm(Y ~ X, data = d)
    #                     
    #                     Group.name <- name.len(levels(D$Group)[i])
    #                     
    #                     sum.fit <- paste(sum.fit,"\n",
    #                                        Group.name, "\t", signif(fit[[i]]$coefficients[1],3), "\t\t", signif(fit[[i]]$coefficients[2],3),
    #                                        sep = "")
    #                     
    #                 }
    #                 
    #             }
    #             
    #             writeLines(sum.fit)
    #             
    #     }  
    #     
    # })
    # 
    # #Quadratic results
    # output$Quadratic.Results <- renderPrint({
    #     
    #     #only if x and y are selected
    #     if (input$X_dy != "(none)" & input$Y_dy != "(none)" & any(grepl("model", input$Fit.Quadratic)) & any(grepl("results", input$Fit.Quadratic))){
    #         
    #         X <- V$Data[[V$Current]][,input$X_dy]
    #         Y <- V$Data[[V$Current]][,input$Y_dy]
    #         
    #         D <- data.frame(X = X, Y = Y)
    #         
    #         #add in grouping variable
    #         if (any(grepl("group", input$Fit.Quadratic))) {
    #             Group <- TRUE
    #             D$Group <- V$Data[[V$Current]][,input$Group_dy]
    #             D <- D[complete.cases(D$X, D$Y, D$Group),]
    #         } else {
    #             Group <- FALSE
    #             D <- D[complete.cases(D$X, D$Y),]
    #         }
    #         
    #         #Linear fit
    #         # if (any(grepl("results", input$Fit.Linear))){
    #         
    #         fit <- list()
    #         
    #         if (!Group){
    #             
    #             fit[[1]] <- lm(Y ~ X + I(X^2), data = D)
    #             
    #             sum.fit <- paste("\t\t",              "\t", "\t", "a",                                  "\t\t", "b",                                   "\t\t", "c","\n",
    #                                "Parameter values","\t", "\t", signif(fit[[1]]$coefficients[1],3),   "\t\t", signif(fit[[1]]$coefficients[2],3),    "\t\t", signif(fit[[1]]$coefficients[3],3),
    #                                sep="")
    #             
    #         } else {
    #             
    #             sum.fit <- paste("Group", "\t\t",            "\t", "\t", "a",                                    "\t\t", "b", "\t\t", "c",sep="")
    #             
    #             for (i in 1:nlevels(D$Group)){
    #                 
    #                 d <- subset(D, Group == levels(D$Group)[i])
    #                 
    #                 fit[[i]] <- lm(Y ~ X + I(X^2), data = d)
    #                 
    #                 Group.name <- name.len(levels(D$Group)[i])
    #                 
    #                 sum.fit <- paste(sum.fit,"\n",
    #                                    Group.name, "\t", signif(fit[[i]]$coefficients[1],3),   "\t\t", signif(fit[[i]]$coefficients[2],3),    "\t\t", signif(fit[[i]]$coefficients[3],3),
    #                                    sep = "")
    #                 
    #             }
    #             
    #         }
    #         
    #         writeLines(sum.fit)
    #         
    #       
    #     }  
    #     
    # })
    # 
    # 
    # #Linear model results
    # output$Power.Results <- renderPrint({
    #     
    #     #only if x and y are selected
    #     if (input$X_dy != "(none)" & input$Y_dy != "(none)" & any(grepl("model", input$Fit.Power)) & any(grepl("results", input$Fit.Power))){
    #         
    #         X <- V$Data[[V$Current]][,input$X_dy]
    #         Y <- V$Data[[V$Current]][,input$Y_dy]
    #         
    #         D <- data.frame(X = X, Y = Y)
    #         
    #         #add in grouping variable
    #         if (any(grepl("group", input$Fit.Power))) {
    #             Group <- TRUE
    #             D$Group <- V$Data[[V$Current]][,input$Group_dy]
    #             D <- D[complete.cases(D$X, D$Y, D$Group),]
    #         } else {
    #             Group <- FALSE
    #             D <- D[complete.cases(D$X, D$Y),]
    #         }
    #         
    #         
    #         fit <- list()
    #         
    #         if (!Group){
    #             
    #             # 'nls', formula = y~a*x^b, method.args = list(start = list(a = 1, b=1), control = list(maxiter = 50000))
    #             
    #             fit[[1]] <- nls(Y ~ a*X^b, data = D, start = list(a = 1, b=1), control = list(maxiter = 50000))
    #             
    #             a <- summary(fit[[1]])$parameters[,1][1]
    #             b <- summary(fit[[1]])$parameters[,1][2]
    #             
    #             sum.fit <- paste("\t\t",            "\t", "\t", "a",                                    "\t\t", "b",         "\n",
    #                              "Parameter values","\t", "\t", signif(a,3), "\t\t", signif(b,3),
    #                              sep="")
    #             
    #         } else {
    #             
    #             sum.fit <- paste("Group", "\t\t",            "\t", "\t", "a",                                    "\t\t", "b", sep="")
    #             
    #             for (i in 1:nlevels(D$Group)){
    #                 
    #                 d <- subset(D, Group == levels(D$Group)[i])
    #                 
    #                 fit[[1]] <- nls(Y ~ a*X^b, data = d, start = list(a = 1, b=1), control = list(maxiter = 50000))
    #                 
    #                 a <- summary(fit[[1]])$parameters[,1][1]
    #                 b <- summary(fit[[1]])$parameters[,1][2]
    #                 
    #                 Group.name <- name.len(levels(D$Group)[i])
    #                 
    #                 sum.fit <- paste(sum.fit,"\n",
    #                                  Group.name, "\t", signif(a,3), "\t\t", signif(b,3),
    #                                  sep = "")
    #                 
    #             }
    #             
    #         }
    #         
    #         writeLines(sum.fit)
    #         
    #     }  
    #     
    # })
    
    # # # DISPLAY DIAGNOSTICS TEXT - need to enable ...textOutput("Text")... in ui.R
    output$Text <- renderPrint(
        
        # if (V$ON) {
        #
        #     # i <- which(V$Name == input$Use.Data)
        #     i <- V$Current
        #     print(V$Y[[i]])
        # }
        #
        # print(paste(input$file$datapath,
        #             input$file$name,
        #             input$Sheet,
        #             V$Excel.path,
        #             sep = " ------ "))
        # 
        # print(paste(input$X, input$Y, sep=","))
        
        # print(input$Fit.Models)
        
        # print("This text is a diagnotic place-holder...")
        # "Hello"
        # str(V$ON)
        # which(V$Name == input$Use.Data)
        # str(input$file)
        #length(V$Data)
        # str(as.numeric(input$which.file))
        # print(paste(V$New.Upload, V$Current, length(V$Data), sep=" || "))
        str(isolate(input),1)
        
        # print(paste(input$Trans.Log))
        # print(str(input,1))
        # str(reactiveValuesToList(input))
        # c(input$Edge.Color, input$Fill.Color)
        # V$Group.Cat.Color[[V$Current]]
        # input$Legend
        # tolower(input$Legend.Position)
        # writeLines(paste("Hello", "\t\t", "Intercept",        "\t", "Slope",         "\n",
        #                           "\t\t", round(2.35234,2), "\t\t", round(3.4235,2),
        #                  sep=""))
        # V$Figs.dim
        # c(input$Saved.Figs_dy, "|", names(V$Figs[[1]]))
        # c(any(grep("Show", input$Symbols_dy)),
        #   input$Sym.Color)
        # as.character(pals[which(pals[,1] == input$Sym.Color.Theme),2])
        # names(V$Figs[[V$Current]])
        # print(isolate(output))
        # print(output$X)
        
        # print(str(V,2))
        # print(paste(input$X, input$Y, sep=" ||"))
    )
    
    
    # EDIT BELOW THIS POINT # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # ----- 
    # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
    # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
    
    
    
    
    
    
    
}) #END of shinyServer

