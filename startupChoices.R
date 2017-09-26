
#For UI objects that require choices (mainly selectInput objects)

choices <- list(
    
    # # # Dynamic objects with choices
    X_dy = "(none)",
    Y_dy = "(none)",
    Group_dy = "(none)",
    Subset_dy = "(none)",
    SubSel_dy = NULL,
    Symbols_dy = c("Show", "Filled"),
    Sym.Shape_dy = shapes,
    Lines_dy = "Show",
    Line.Color_dy = colors,
    Line.Type_dy = lty,
    Saved.Figs_dy = "(none)",
    
    
    # # # Static objects with choices
    Scatter.Color.Theme = pals$label,
    Line.Order = c("X axis order", "Dataset order"),
    CatType = c("Box plot", "Bar plot", "Violin plot"),
    Cat.Color.Boxes = c("Fill", "Edge"),
    Cat.Color.Theme = pals$label,
    Error = c("Standard Deviation", "Standard Error", "(none)"),
    Title.Position = c("Left", "Center", "Right"),
    Legend = "Show",
    Legend.Position = c("Right","Top"),
    
    
    Fit.Models = c("Linear", "Quadratic"),
    Models = c("Linear", "Quadratic", "Power", "Custom"),
    Trans.Log = c("X", "Y"),
    Trans.Std = c("X", "Y")
    
)
