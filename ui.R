
# See commented areas below to get a sense of the link between the main panel and the conditionalpanel that determines the sidebar panel content


source("globalVars.R")
source("startupChoices.R")
source("startupSelections.R")

# Define UI for random distribution application 
shinyUI(
    bootstrapPage(
        
        useShinyjs(),
        withMathJax(),
        
        tags$head(tags$style(
            type="text/css",
            ".shiny-output-error{color: white;}" #color of error messages (make white for release)
            
        )),
        
        
        theme = shinytheme("lumen"),
        pageWithSidebar(
            
            # Application title
            # headerPanel(h3("Wellesley Data Explorer", style="color: DarkBlue"), windowTitle = "Data Explorer"),
            headerPanel(NULL, windowTitle = "Data Explorer"),
            
            # Sidebar with controls to select the random distribution type
            # and number of observations to generate. Note the use of the br()
            # element to introduce extra vertical spacing
            sidebarPanel(
                h3("Wellesley Data Explorer (Dev 2.02a)", style="color: DarkBlue; margin-top: 0;"),
                
                ## UPLOAD DATA
                conditionalPanel(
                    condition = "input.mainPanelTabs == 'Datasets'",
                    
                    fluidRow(
                        column(width = 8,
                               selectInput("Use.Data", "Current dataset:", "(none)")
                               
                        )
                        
                    ),
                    
                    
                    uiOutput("Upload"),
                    
                    
                    uiOutput("Sheet"),
                    
                    
                    fluidRow(
                        column(width = 6,
                               uiOutput("Data.Name")
                               
                        ),
                        
                        column(width = 6,
                               br(),
                               uiOutput("Submit.Button")
                        )
                        
                        
                    )
                    
                    
                ), #end of Dataset conditional panel
                
                conditionalPanel(
                    condition = "input.mainPanelTabs == 'Plot'",
                    tabsetPanel(id = "plot-tabs",
                                tabPanel("Variables", 
                                         
                                         br(),
                                         
                                         fluidRow(
                                             column(width = 6,
                                                    selectInput("X_dy", label = ("X variable"), 
                                                                choices = choices$X_dy, selected = sels$X_dy)
                                                    
                                             ),
                                             
                                             column(width = 6,
                                                    selectInput("Y_dy", label = ("Y variable"), 
                                                                choices = choices$Y_dy, selected = sels$Y_dy)
                                             )
                                             
                                             
                                         ),
                                         
                                         
                                         
                                         
                                         fluidRow(
                                             column(width = 6,
                                                    selectInput("Group_dy", label = ("Select grouping variable"), 
                                                                choices = choices$Group_dy, selected = sels$Group_dy)
                                                    
                                             ),
                                             
                                             column(width = 6,
                                                    selectInput("Subset_dy", label = ("Select subsetting variable"), 
                                                                choices = choices$Subset_dy, selected = sels$Subset_dy)
                                             )
                                             
                                             
                                         ),
                                         
                                         shinyjs::hidden(div(id = "SubSel_dy", selectInput("SubSel_dy", "Subsetting groups", choices = choices$SubSel_dy,
                                                                                           selected = sels$SubSel_dy, multiple=TRUE))),
                                         
                                         checkboxInput("Show.Trans", "Show Transformation Options", value = FALSE),
                                         
                                         
                                         shinyjs::hidden(div(id = "div.Transform",
                                                             fluidRow(
                                                                 column(width = 4,
                                                                        checkboxGroupInput("Trans.Log", "Log", inline = TRUE,
                                                                                           choices = choices$Trans.Log, selected = sels$Trans.Log)
                                                                        
                                                                 ),
                                                                 
                                                                 column(width = 4,
                                                                        # checkboxGroupInput("Trans.Log10", "log10", inline = TRUE,
                                                                        #                    choices = c("X","Y"), selected = NULL)
                                                                        radioButtons("Log.Base", "Base", inline = TRUE,
                                                                                     choices = c("e", "10"), selected = "e")
                                                                        
                                                                 ),
                                                                 
                                                                 column(width = 4,
                                                                        checkboxGroupInput("Trans.Std", "Standardize", inline = TRUE,
                                                                                           choices = choices$Trans.Std, selected = sels$Trans.Std)
                                                                 )
                                                                 
                                                             )
                                         ))
                                         
                                         
                                ),
                                
                                
                                tabPanel("Appearance",
                                         
                                         div(id = "div.NoVar.Appearance",
                                             h5("Select X and Y variables", style=("font-weight: bold; color: DarkBlue"))),
                                         
                                         shinyjs::hidden(div(id = "div.ScatterOptions",
                                                             
                                                             h5("Scatterplot options", style=("font-weight: bold; color: DarkBlue")),
                                                             
                                                             
                                                             shinyjs::hidden(div(id = "div.Scatter.Color.Theme",
                                                                                 fluidRow(
                                                                                     column(width = 6,
                                                                                            
                                                                                            
                                                                                            selectInput("Scatter.Color.Theme", "Group color theme",
                                                                                                        choices = choices$Scatter.Color.Theme,
                                                                                                        selected = sels$Scatter.Color.Theme)
                                                                                     )
                                                                                 ),
                                                                                 hr(style = "margin: 0px 0 10px 0; border: .5px solid #00008B")
                                                                                 
                                                             )),
                                                             
                                                             
                                                             
                                                             checkboxGroupInput("Symbols_dy", "Symbols:", inline = TRUE,
                                                                                choices = choices$Symbols_dy,
                                                                                selected = sels$Symbols_dy),
                                                             
                                                             
                                                             fluidRow(
                                                                 
                                                                 column(width = 6,
                                                                        
                                                                        div(id = "Sym.Color.Single",
                                                                            colourInput("Sym.Color", "Symbol color", value = sels$Sym.Color,
                                                                                        palette = "limited", showColour = "background")),
                                                                        
                                                                        # shinyjs::hidden(div(id = "Sym.Color.Group",
                                                                        #                     selectInput("Sym.Color.Theme", "Group color theme",
                                                                        #                                 choices = choices$Sym.Color.Theme,
                                                                        #                                 selected = sels$Sym.Color.Theme)))
                                                                        
                                                                        shinyjs::hidden(div(id = "Sym.Color.Group",
                                                                                            h5("(Symbol color by group)")))
                                                                        
                                                                 ),
                                                                 
                                                                 
                                                                 column(width = 6,
                                                                        selectInput("Sym.Shape_dy", "Symbol shape", choices = choices$Sym.Shape_dy, selected = sels$Sym.Shape_dy)
                                                                 )
                                                                 
                                                                 
                                                             ),
                                                             
                                                             
                                                             fluidRow(
                                                                 column(width = 4,
                                                                        numericInput("Sym.Size", "Symbol size", value = sels$Sym.Size, min = .5, max = 6, step = .5)
                                                                 ),
                                                                 
                                                                 column(width = 4,
                                                                        numericInput("Sym.Stroke", "Edge thickness", value = sels$Sym.Stroke, min = .25, max = 4, step = .25)
                                                                 ),
                                                                 
                                                                 column(width = 4,
                                                                        numericInput("Sym.Trans", "Transparency (%)", value = sels$Sym.Trans, min = 0, max = 100, step = 10)
                                                                 )
                                                             ),
                                                             
                                                             hr(style = "margin: 0px 0 10px 0; border: .5px solid #00008B"),
                                                             
                                                             checkboxGroupInput("Lines_dy", "Connecting lines:", inline = TRUE,
                                                                                choices = choices$Lines_dy,
                                                                                selected = sels$Lines_dy),
                                                             
                                                             fluidRow(
                                                                 column(width = 6,
                                                                        
                                                                        div(id = "Line.Color.Single",
                                                                            colourInput("Line.Color", "Line color", value = sels$Line.Color,
                                                                                        palette = "limited", showColour = "background")),
                                                                        
                                                                        shinyjs::hidden(div(id = "Line.Color.Group",
                                                                                            selectInput("Line.Color.Theme", "Group color theme",
                                                                                                        choices = choices$Line.Color.Theme,
                                                                                                        selected = sels$Line.Color.Theme)))
                                                                        
                                                                 ),
                                                                 column(width = 6,
                                                                        selectInput("Line.Type_dy", "Line type", choices = choices$Line.Type_dy, selected = sels$Line.Type_dy)
                                                                 )
                                                             ),
                                                             
                                                             
                                                             fluidRow(
                                                                 column(width = 4,
                                                                        selectInput("Line.Order", "Connect order", choices = choices$Line.Order, selected = sels$Line.Order)
                                                                 ),
                                                                 column(width = 4,
                                                                        numericInput("Line.Size", "Line thickness", value = sels$Line.Size, min = 0.5, max = 5, step = 0.5)
                                                                 ),
                                                                 column(width = 4,
                                                                        numericInput("Line.Trans", "Transparency (%)", value = 0, min = 0, max = 100, step = 10)
                                                                 )
                                                             )
                                                             
                                         )),
                                         
                                         
                                         shinyjs::hidden(div(id = "div.CatOptions",
                                                             
                                                             h5("Categorical figure options", style=("font-weight: bold; color: DarkBlue")),
                                                             
                                                             fluidRow(
                                                                 column(width = 6,
                                                                        div(id = "CatType", selectInput("CatType", "Figure type", choices = choices$CatType),
                                                                            selected = sels$CatType)
                                                                        
                                                                 ),
                                                                 
                                                                 column(width = 6,
                                                                        #start hidden
                                                                        shinyjs::hidden(div(id = "div.Error",
                                                                                            selectInput("Error", "Error bars", choices = choices$Error, selected = sels$Error)
                                                                        ))
                                                                 )
                                                             ),
                                                             
                                                             hr(style = "margin: 0px 0 10px 0; border: .5px solid #00008B"),
                                                             
                                                             fluidRow(
                                                                 column(width = 6,
                                                                        
                                                                        #start hidden
                                                                        (div(id = "div.Cat.Color.Boxes",
                                                                             checkboxGroupInput("Cat.Color.Boxes", "Color by group:", inline = TRUE,
                                                                                                choices = choices$Cat.Color.Boxes,
                                                                                                selected = sels$Cat.Color.Boxes)
                                                                        ))
                                                                        
                                                                 ),
                                                                 
                                                                 column(width = 6,
                                                                        #start hidden
                                                                        (div(id = "div.Cat.Color.Theme",
                                                                             selectInput("Cat.Color.Theme", "Group color theme",
                                                                                         choices = choices$Cat.Color.Theme,
                                                                                         selected = sels$Cat.Color.Theme)
                                                                        ))
                                                                 )
                                                             ),
                                                             
                                                             
                                                             
                                                             fluidRow(
                                                                 column(width = 6,
                                                                        #can be disabled
                                                                        div(id = "div.Edge.Color",
                                                                            colourInput("Edge.Color", "Edge color", value = sels$Edge.Color,
                                                                                        palette = "limited", showColour = "background")
                                                                        )
                                                                 ),
                                                                 
                                                                 column(width = 6,
                                                                        #can be disabled
                                                                        div(id = "div.Fill.Color",
                                                                            colourInput("Fill.Color", "Fill color", value = sels$Fill.Color,
                                                                                        palette = "limited", showColour = "background")
                                                                        )
                                                                 )
                                                             ),
                                                             
                                                             
                                                             fluidRow(
                                                                 column(width = 6,
                                                                        
                                                                        numericInput("Cat.Edge", "Edge thickness", value = sels$Cat.Edge, min = 0, max = 3, step = 0.2)
                                                                        
                                                                 ),
                                                                 
                                                                 column(width = 6,
                                                                        
                                                                        numericInput("Error.Cap", "Errorbar cap width (%)", value = sels$Error.Cap, min = 0, max = 100, step = 5)
                                                                        
                                                                 )
                                                             ),
                                                             
                                                             
                                                             hr(style = "margin: 0px 0 10px 0; border: .5px solid #00008B"),
                                                             
                                                             
                                                             
                                                             fluidRow(
                                                                 column(width = 6,
                                                                        sliderInput("Spacing.X", "X variable spacing (%)", min = 0, max = 100, value = sels$Spacing.X, step = 10, ticks = FALSE)
                                                                 ),
                                                                 
                                                                 column(width = 6,
                                                                        #start hidden
                                                                        div(id = "div.Spacing.Group",
                                                                            sliderInput("Spacing.Group", "Group variable spacing (%)", min = 0, max = 100, value = sels$Spacing.Group, step = 10, ticks = FALSE)
                                                                        )
                                                                 )
                                                             )
                                                             
                                                             
                                                             
                                         ))
                                ),
                                
                                tabPanel("Axes",
                                         
                                         div(id = "div.NoVar.Axes",
                                             h5("Select X and Y variables", style=("font-weight: bold; color: DarkBlue"))),
                                         
                                         shinyjs::hidden(div(id = "div.AxesOptions",
                                                             
                                                             
                                                             
                                                             h5("Scaling", style="margin: 0px; font-weight: bold; color: DarkBlue"),
                                                             
                                                             
                                                             fluidRow(
                                                                 
                                                                 shinyjs::hidden(div(id = "div.xminmax",
                                                                                     column(width = 3,
                                                                                            textInput("xmin", "X min", value = sels$xmin)
                                                                                     ),
                                                                                     
                                                                                     column(width = 3,
                                                                                            textInput("xmax", "X max", value = sels$xmax)
                                                                                     ))),
                                                                 
                                                                 shinyjs::hidden(div(id = "div.xpad",
                                                                                     column(width = 6,
                                                                                            sliderInput("xpad", "X axis padding (%)", ticks = FALSE,
                                                                                                        min = 0, max = 100, value = sels$xpad, step = 5)
                                                                                     ))),
                                                                 
                                                                 
                                                                 column(width = 3,
                                                                        textInput("ymin", "Y min", value = sels$ymin)
                                                                 ),
                                                                 
                                                                 column(width = 3,
                                                                        textInput("ymax", "Y max", value = sels$ymax)
                                                                 )
                                                                 
                                                                 
                                                             ),
                                                             
                                                             hr(style = "margin: 0px 0 10px 0; border: .5px solid #00008B"),
                                                             h5("Labels", style="margin: 0px; font-weight: bold; color: DarkBlue"),
                                                             
                                                             fluidRow(
                                                                 column(width = 6,
                                                                        textInput("Title", "Figure title", value = sels$Title)
                                                                        
                                                                 ),
                                                                 
                                                                 column(width = 6,
                                                                        shinyjs::disabled(div(id = "div.Title.Position",
                                                                                              selectInput("Title.Position", "Title position",
                                                                                                          choices = choices$Title.Position,
                                                                                                          selected = sels$Title.Position)))
                                                                        
                                                                 )
                                                                 
                                                                 
                                                             ),
                                                             
                                                             
                                                             fluidRow(
                                                                 column(width = 6,
                                                                        div(style = "height: 55px", textInput("xlab", "X axis label", value = sels$xlab))
                                                                        
                                                                 ),
                                                                 
                                                                 column(width = 6,
                                                                        div(style = "height: 55px", textInput("ylab", "Y axis label", value = sels$ylab))
                                                                        
                                                                 )
                                                             ),
                                                             
                                                             checkboxInput("xlab.Rotate", "Rotate X tick labels", value = sels$xlab.Rotate),
                                                             
                                                             hr(style = "margin: 0px 0 10px 0; border: .5px solid #00008B"),
                                                             
                                                             h5("Text size", style="margin: 0px; font-weight: bold; color: DarkBlue"),
                                                             fluidRow(
                                                                 column(width = 4,
                                                                        numericInput("Size.Tick", "Tick labels", min = .1, max = 2, value = sels$Size.Tick, step = .1)
                                                                        
                                                                 ),
                                                                 
                                                                 column(width = 4,
                                                                        numericInput("Size.Lab", "Axis labels", min = .1, max = 2, value = sels$Size.Lab, step = .1)
                                                                        
                                                                 ),
                                                                 
                                                                 column(width = 4,
                                                                        numericInput("Size.Title", "Title ", min = .1, max = 2, value = sels$Size.Title, step = .1)
                                                                        
                                                                 )
                                                                 
                                                             ),
                                                             
                                                             
                                                             hr(style = "margin: 0px 0 10px 0; border: .5px solid #00008B"),
                                                             
                                                             h5("Legend", style="margin: 0px; font-weight: bold; color: DarkBlue"),
                                                             
                                                             fluidRow(
                                                                 column(width = 3,
                                                                        checkboxGroupInput("Legend", "", inline = TRUE,
                                                                                           choices = choices$Legend, selected = sels$Legend)
                                                                        
                                                                 ),
                                                                 
                                                                 
                                                                 column(width = 9,
                                                                        textInput("Legend.Title", "Title", sels$Legend.Title)
                                                                        
                                                                 )
                                                             ),
                                                             
                                                             fluidRow(
                                                                 column(width = 4,
                                                                        numericInput("Legend.Key.Size", "Key size (%)", min = 25, max = 300, value = sels$Legend.Key.Size, step = 25)                                                                        # checkboxInput("Legend", "Show", value = TRUE)
                                                                        
                                                                        
                                                                 ),
                                                                 
                                                                 column(width = 4,
                                                                        numericInput("Size.Legend", "Text size ", min = .1, max = 2, value = sels$Size.Legend, step = .1)
                                                                        
                                                                 ),
                                                                 
                                                                 column(width = 4,
                                                                        selectInput("Legend.Position", "Position ",
                                                                                    choices = choices$Legend.Position,
                                                                                    selected = sels$Legend.Position)
                                                                        
                                                                        # choices = c("Out: N", "Out: S", "Out: E", "Out: W",
                                                                        #             "In: NW", "In: NE", "In: SW", "In: SE",
                                                                        #             "In: N", "In: S", "In: E", "In: W"),
                                                                        # selected = "Out: E")
                                                                        
                                                                 )
                                                             )
                                                             
                                                             
                                         ))),
                                
                                tabPanel("Models",
                                         
                                         div(id = "div.NoVar.Models",
                                             h5("Select X and Y variables", style=("font-weight: bold; color: DarkBlue"))),
                                         
                                         shinyjs::hidden(div(id = "div.Models",
                                                             br(),
                                                             
                                                             h5("Fit model (X is continuous)", style="font-weight: bold; color: DarkBlue"),
                                                             
                                                             checkboxGroupInput("Models", NULL, inline = TRUE,
                                                                                choices = choices$Models,
                                                                                selected = sels$Models),
                                                             
                                                             
                                                             #Linear
                                                             shinyjs::hidden(div(id = "div.Linear",
                                                                                 hr(style = "margin: 0px 0 10px 0; border: .5px solid #00008B"),
                                                                                 h5("Linear model", style="margin: 0px; font-weight: bold; color: DarkBlue"),
                                                                                 
                                                                                 fluidRow(
                                                                                     column(width = 6,
                                                                                            checkboxGroupInput("Fit.Linear", NULL,
                                                                                                               choices = c("Show results"),
                                                                                                               selected = NULL)
                                                                                            
                                                                                     ),
                                                                                     column(width = 6,
                                                                                            colourInput("Color.Linear", "Color", value = "black",
                                                                                                        palette = "limited", showColour = "background")
                                                                                     )
                                                                                 ),
                                                                                 
                                                                                 fluidRow(
                                                                                     column(width = 6,
                                                                                            numericInput("Size.Linear", "Thickness", value = sels$Size.Linear, min = 0.5, max = 5, step = 0.5)
                                                                                     ),
                                                                                     
                                                                                     column(width = 6,
                                                                                            selectInput("Type.Linear", "Line type", choices = lty, selected = lty[1])
                                                                                     )
                                                                                 )
                                                             )),
                                                             
                                                             
                                                             #Quadratic
                                                             shinyjs::hidden(div(id = "div.Quadratic",
                                                                                 
                                                                                 hr(style = "margin: 0px 0 10px 0; border: .5px solid #00008B"),
                                                                                 h5("Quadratic model", style="margin: 0px; font-weight: bold; color: DarkBlue"),
                                                                                 
                                                                                 fluidRow(
                                                                                     column(width = 6,
                                                                                            checkboxGroupInput("Fit.Quadratic", NULL,
                                                                                                               choices = c("Show results"),
                                                                                                               selected = NULL)
                                                                                            
                                                                                     ),
                                                                                     column(width = 6,
                                                                                            colourInput("Color.Quadratic", "Color", value = "black",
                                                                                                        palette = "limited", showColour = "background")
                                                                                     )
                                                                                 ),
                                                                                 
                                                                                 fluidRow(
                                                                                     column(width = 6,
                                                                                            numericInput("Size.Quadratic", "Thickness", value = 1, min = 0.5, max = 5, step = 0.5)
                                                                                     ),
                                                                                     
                                                                                     column(width = 6,
                                                                                            selectInput("Type.Quadratic", "Line type", choices = lty, selected = lty[1])
                                                                                     )
                                                                                 )
                                                             )),
                                                             
                                                             
                                                             #Power    
                                                             shinyjs::hidden(div(id = "div.Power",
                                                                                 
                                                                                 hr(style = "margin: 0px 0 10px 0; border: .5px solid #00008B"),
                                                                                 h5("Power model", style="margin: 0px; font-weight: bold; color: DarkBlue"),
                                                                                 
                                                                                 fluidRow(
                                                                                     column(width = 6,
                                                                                            checkboxGroupInput("Fit.Power", NULL,
                                                                                                               choices = c("Show results"),
                                                                                                               selected = NULL)
                                                                                            
                                                                                     ),
                                                                                     column(width = 6,
                                                                                            colourInput("Color.Power", "Color", value = "black",
                                                                                                        palette = "limited", showColour = "background")
                                                                                     )
                                                                                 ),
                                                                                 
                                                                                 fluidRow(
                                                                                     column(width = 6,
                                                                                            numericInput("Size.Power", "Thickness", value = 1, min = 0.5, max = 5, step = 0.5)
                                                                                     ),
                                                                                     
                                                                                     column(width = 6,
                                                                                            selectInput("Type.Power", "Line type", choices = lty, selected = lty[1])
                                                                                     )
                                                                                 )
                                                                                 
                                                             )),
                                                             
                                                             
                                                             #Custom    
                                                             shinyjs::hidden(div(id = "div.Custom",
                                                                                 
                                                                                 hr(style = "margin: 0px 0 10px 0; border: .5px solid #00008B"),
                                                                                 h5("Custom model", style="margin: 0px; font-weight: bold; color: DarkBlue"),
                                                                                 
                                                                                 fluidRow(
                                                                                     column(width = 5,
                                                                                            textInput("Custom.Formula", "Y = ", value =  "a + b*x")
                                                                                            
                                                                                     ),
                                                                                     column(width = 5,
                                                                                            textInput("Custom.Start", "Starting values", value =  "a = 0, b = 0")
                                                                                     ),
                                                                                     
                                                                                     column(width = 2,
                                                                                            br(),
                                                                                            actionButton("Do.Custom", "Fit")
                                                                                     )
                                                                                 ),
                                                                                 
                                                                                 
                                                                                 fluidRow(
                                                                                     column(width = 6,
                                                                                            checkboxGroupInput("Fit.Custom", NULL,
                                                                                                               choices = c("Show results"),
                                                                                                               selected = NULL)
                                                                                            
                                                                                     ),
                                                                                     column(width = 6,
                                                                                            colourInput("Color.Custom", "Color", value = "black",
                                                                                                        palette = "limited", showColour = "background")
                                                                                     )
                                                                                     
                                                                                 ),
                                                                                 
                                                                                 fluidRow(
                                                                                     column(width = 6,
                                                                                            numericInput("Size.Custom", "Thickness", value = 1, min = 0.5, max = 5, step = 0.5)
                                                                                     ),
                                                                                     
                                                                                     column(width = 6,
                                                                                            selectInput("Type.Custom", "Line type", choices = lty, selected = lty[1])
                                                                                     )
                                                                                 )
                                                                                 
                                                             )),
                                                             
                                                             #Hidden inputs
                                                             shinyjs::hidden(div(
                                                                 checkboxGroupInput("Fit.Models", "Fit models:",
                                                                                    choices = choices$Fit.Models, selected = sels$Fit.Models),  #Can go!
                                                                 
                                                                 checkboxGroupInput("Trans.Log", "Log transformation",
                                                                                    choices = choices$Trans.Log, selected = sels$Fit.Models),
                                                                 
                                                                 checkboxGroupInput("Trans.Std", "Standardize",
                                                                                    choices = choices$Trans.Std, selected = sels$Trans.Std)
                                                                 
                                                             ))
                                                             
                                         )),
                                         
                                         
                                         shinyjs::hidden(div(id = "div.Models.Cat",
                                                             
                                                            
                                                             br(),
                                                             
                                                             h5("Fit model (X is categorical)", style="font-weight: bold; color: DarkBlue"),
                                                             
                                                             checkboxInput("ANOVA", "Perform Analysis of Variance (ANOVA)",
                                                                           value = FALSE)
                                                             
                                                             ))
                                         
                                )
                                
                    )
                ), # end of Plot conditional panel
                
                
                # # # - - New conditional panel on sidebar (left) - - - - - - - - - - - - - - - - - - - - - - - - - - # # #
                
                conditionalPanel(
                    condition = "input.mainPanelTabs == 'Figure Gallery'",  #This name must match the name of the main panel tab below
                    
                    
                    selectInput("Saved.Figs_dy", "Saved Figures", choices = choices$Saved.Figs_dy, selected = sels$Saved.Figs_dy),
                    
                    fluidRow(
                        column(width = 6,
                               textInput("Saved.Width_dy", "Width", value = sels$Saved.Width_dy)
                        ),
                        
                        column(width = 6,
                               textInput("Saved.Height_dy", "Height", value = sels$Saved.Height_dy)
                        )
                    )
                    
                    # colourInput("Pick.Color","Color", value = "black", showColour = "background", palette = "limited")
                    
                    
                    # h4("Hello from the sidebar panel!")
                    
                    #Could insert tab panel here if necessary. See conditional panels above for how to do it.
                    
                ), 
                # # # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - # # #
                
                
                # # # # # # # # # # #
                width = 4 
            ),  # end of sidebar panel
            
            
            mainPanel(
                tabsetPanel(id = "mainPanelTabs",
                            
                            tabPanel("Datasets",
                                     
                                     
                                     
                                     br(),
                                     dataTableOutput("Data.Table")
                                     
                                     
                            ),
                            
                            
                            tabPanel("Plot",
                                     
                                     shinyjs::hidden(div(id = "div.FigHead",
                                                         
                                                         fluidRow(
                                                             column(width = 6,
                                                                    div(
                                                                        splitLayout(
                                                                            
                                                                            div(br(), actionButton("Save.Fig", "Add to gallery", width = 120)),
                                                                            textInput("Fig.Name", "Figure name (optional)", "", width = 300),
                                                                            # textAreaInput("Fig.Notes", "Notes / Description", "", rows = 2),
                                                                            
                                                                            cellWidths = c("140","320")#,"100%")
                                                                        ), style = "padding-top: 10px;"
                                                                    )
                                                                    
                                                                    
                                                             ),
                                                             # column(width = 4,
                                                             #        textInput("Fig.Name", "Figure Name", "")
                                                             # ),
                                                             column(width = 6,
                                                                    div( #aligned right
                                                                        
                                                                        splitLayout(
                                                                            div(textInput("Width", "Figure width (px)", value = sels$Width, width = 100), align = "center"),
                                                                            div(textInput("Height", "Figure height (px)", value = sels$Height, width = 100), align = "center"),
                                                                            
                                                                            cellWidths = 130
                                                                            
                                                                            
                                                                        ), align = "right", style = "padding-top: 10px; padding-right: 20px"
                                                                    )
                                                             )
                                                         )
                                                         
                                     )),
                                     
                                     # # div(plotlyOutput("Figure"), align = "center")
                                     # tags$div(HTML("< script type='text/x-mathjax-config' >
                                     #               MathJax.Hub.Config({
                                     #               tex2jax: {inlineMath: [['$','$'], ['\\(','\\)']]}
                                     #               });
                                     #               < /script >
                                     #               ")),
                                     
                                     
                                     div(plotOutput("Figure", height = "auto"), align = "center"),
                                     
                                     br(),
                                     
                                     
                                     # tags$div(HTML("<script type='text/x-mathjax-config' >
                                     #            MathJax.Hub.Config({
                                     #               tex2jax: {inlineMath: [['$','$'], ['\\(','\\)']]}
                                     #               });
                                     #               < /script >")),
                                     # 
                                     shinyjs::hidden(div(id = "div.Linear.Results",
                                                         
                                                         # splitLayout(h4("Linear equation:", style="color: DarkBlue"),
                                                         #             withMathJax("$$Y = a + bX$$"),
                                                         #             h5(" "),
                                                         #             cellWidths = c("5%", "5%", "90%")),
                                                         
                                                         # fluidRow(column(width = 3, h4("Linear equation:", style="color: DarkBlue")),
                                                         #          column(width = 6, h4(withMathJax("$$Y = a + bX$$")))
                                                         #          
                                                         # ),
                                                         # 
                                                         # splitLayout(h4("Linear equation:", style="color: DarkBlue"),
                                                         #             h4(withMathJax("$$Y = a + bX$$"))
                                                         #             ),
                                                         hr(style = "margin: 0px 0 10px 0; border: .5px solid #00008B"),
                                                         
                                                         h5(withMathJax("\\(Linear \\space model: \\space\\space Y = a + bX\\)"), style = "color: DarkBlue"),
                                                         
                                                         tableOutput("Linear.Table")
                                                         
                                                         # verbatimTextOutput("Linear.Results")
                                                         
                                     )),
                                     
                                     shinyjs::hidden(div(id = "div.Quadratic.Results",
                                                         
                                                         hr(style = "margin: 0px 0 10px 0; border: .5px solid #00008B"),
                                                         
                                                         h5(withMathJax("\\(Quadratic \\space model: \\space\\space Y = a + bX + cX^2\\)"), style = "color: DarkBlue"),
                                                         
                                                         
                                                         
                                                         tableOutput("Poly.Table")
                                     )),
                                     
                                     shinyjs::hidden(div(id = "div.Power.Results",
                                                         
                                                         hr(style = "margin: 0px 0 10px 0; border: .5px solid #00008B"),
                                                         
                                                         h5(withMathJax("\\(Power \\space model: \\space\\space Y = aX^b\\)"), style = "color: DarkBlue"),
                                                         
                                                         tableOutput("Power.Table")
                                                         
                                     )),
                                     
                                     shinyjs::hidden(div(id = "div.Custom.Results",
                                                         
                                                         hr(style = "margin: 0px 0 10px 0; border: .5px solid #00008B"),
                                                         
                                                         h5(withMathJax("\\(Custom \\space model:\\)"), style = "color: DarkBlue"),
                                                         
                                                         tableOutput("Custom.Table")
                                                         
                                     )),
                                     
                                     shinyjs::hidden(div(id = "div.ANOVA.Results",
                                                         
                                                         hr(style = "margin: 0px 0 10px 0; border: .5px solid #00008B"),
                                                         
                                                         h5("Analysis of Variance (ANOVA):", style = "color: DarkBlue"),
                                                         #h5(withMathJax("\\(Custom \\space model:\\)"), style = "color: DarkBlue"),
                                                         
                                                         tableOutput("ANOVA.Table")
                                                         
                                     ))
                                     
                                     # verbatimTextOutput("Lin.Results")
                                     
                                     
                                     
                                     
                                     
                            ),
                            
                            
                            # # # - - New tab on main panel (right) - - - - - - - - - - - - - - - - - - - - - - - - - - # # #
                            tabPanel("Figure Gallery", #This name is used in the conditional statement for the sidebar panel tab above. Must match!
                                     
                                     # br(),
                                     # 
                                     # h4("Hello from the main panel!"),
                                     br(),
                                     # h5("Testing the ability to save ggplot objects..."),
                                     
                                     
                                     div(plotOutput("Saved.Figs"), align = "center")
                                     
                                     
                            ),
                            # # # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - # # #
                            
                            tabPanel("About",
                                     
                                     
                                     br(),
                                     
                                     h4("Version 2.01  (14 September 2017)"),
                                     br(),
                                     h5("Created by Alden Griffith (Environmental Studies) and Hannah Murphy ('19)"),
                                     br(),
                                     h5("Please send any comments / errors to agriffit@wellesley.edu"),
                                     br(),
                                     h5("Known issues:"),
                                     h5("- Fitted model selections are not specific to datasets and will remain the same when switching datasets (will be fixed soon)"),
                                     br(),
                                     h5("Not fully developed:"),
                                     h5("- Model fit colors / line type cannot yet be set according to the grouping variable"),
                                     h5("- Fitted model information/results still in early development"),
                                     
                                     br()
                                     
                                     
                                     
                            ),
                            
                            
                            tabPanel("Development",
                                     
                                     br(),
                                     
                                     #Diagnostic Panel
                                     h4("Diagnotic text output:"),
                                     verbatimTextOutput("Text")
                                     
                            )
                ) #end of tabset panel within mainpanel
                , width = 8) #end of mainPanel
        ) 
    )
)