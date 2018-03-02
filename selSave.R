
#Saves current input object selections

V$Sel[[V$Current]] <- list(
    
    # # # Dynamic objects with choices
    X_dy = input$X_dy,
    Y_dy = input$Y_dy,
    Group_dy = input$Group_dy,
    Subset_dy = input$Subset_dy,
    SubSel_dy = input$SubSel_dy,
    CatVars_dy = input$CatVars_dy,
    Symbols_dy = input$Symbols_dy,
    Sym.Shape_dy = input$Sym.Shape_dy,
    Lines_dy = input$Lines_dy,
    Line.Color_dy = input$Line.Color_dy,
    Line.Type_dy = input$Line.Type_dy,
    #Saved.Figs_dy is not updated when switching datasets (global)
    Fill.Color = input$Fill.Color, #not yet dynamic
    Edge.Color = input$Edge.Color, #not yet dynamic
    
    # # # Dynamic objects without choices
    #Saved.Width is not updated when switching datasets (global)
    #Saved.Height is not updated when switching datasets (global)
    
    # # # Static objects with choices
    Scatter.Color.Theme = input$Scatter.Color.Theme,
    Line.Order = input$Line.Order,
    CatType = input$CatType,
    Cat.Color.Boxes = input$Cat.Color.Boxes,
    Cat.Color.Theme = input$Cat.Color.Theme,
    Error = input$Error,
    Title.Position = input$Title.Position,
    Legend = input$Legend,
    Legend.Position = input$Legend.Position,
    
    Trans.Log = input$Trans.Log,
    Trans.Std = input$Trans.Std,
    Fit.Models = input$Fit.Models,
    Models = input$Models,  
    
    # # # Static objects without choices
    xlab = input$xlab,
    xlab.Rotate = input$xlab.Rotate,
    ylab = input$ylab,
    Title = input$Title,
    Legend.Title = input$Legend.Title,
    xmin = input$xmin,
    xmax = input$xmax,
    xpad = input$xpad,
    ymin = input$ymin,
    ymax = input$ymax,
    Width = input$Width,
    Height = input$Height,
    Sym.Color = input$Sym.Color,
    Sym.Size = input$Sym.Size,
    Sym.Stroke = input$Sym.Stroke,
    Sym.Trans = input$Sym.Trans,
    Line.Color = input$Line.Color,
    Line.Size = input$Line.Size,
    Fill.Color = input$Fill.Color,
    Edge.Color = input$Edge.Color,
    Cat.Edge = input$Cat.Edge,
    Error.Cap = input$Error.Cap,
    Spacing.X = input$Spacing.X,
    Spacing.Group = input$Spacing.Group,
    Size.Tick = input$Size.Tick,
    Size.Lab = input$Size.Lab,
    Size.Title = input$Size.Title,
    Size.Legend = input$Size.Legend,
    Legend.Key.Size = input$Legend.Key.Size,
    Size.Linear = input$Size.Linear,
    Fig.Name = input$Fig.Name
)


