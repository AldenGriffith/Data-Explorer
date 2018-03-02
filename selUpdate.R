#Updates input widget selections based on current dataset

i <- V$Current

message("updated")
message(paste("current =", i))
message(V$Choices[[i]]$Lines_dy)
message(V$Sel[[i]]$Lines_dy)


# # # Dynamic objects with choices
updateSelectInput(session, "X_dy", choices = V$Choices[[i]]$X_dy, selected = V$Sel[[i]]$X_dy)
updateSelectInput(session, "Y_dy", choices = V$Choices[[i]]$Y_dy, selected = V$Sel[[i]]$Y_dy)
updateSelectInput(session, "Group_dy", choices = V$Choices[[i]]$Group_dy, selected = V$Sel[[i]]$Group_dy)
updateSelectInput(session, "Subset_dy", choices = V$Choices[[i]]$Subset_dy, selected = V$Sel[[i]]$Subset_dy)
updateSelectInput(session, "SubSel_dy", choices = V$Choices[[i]]$SubSel_dy, selected = V$Sel[[i]]$SubSel_dy)
updateSelectInput(session, "CatVars_dy", choices = V$Choices[[i]]$CatVars_dy, selected = V$Sel[[i]]$CatVars_dy)
updateCheckboxGroupInput(session, "Symbols_dy", inline = TRUE, choices = V$Choices[[i]]$Symbols_dy, selected = V$Sel[[i]]$Symbols_dy)
updateSelectInput(session, "Sym.Shape_dy", choices = V$Choices[[i]]$Sym.Shape_dy, selected = V$Sel[[i]]$Sym.Shape_dy)
updateCheckboxGroupInput(session, "Lines_dy", inline = TRUE, choices = V$Choices[[i]]$Lines_dy, selected = V$Sel[[i]]$Lines_dy)
updateSelectInput(session, "Line.Color_dy", choices = V$Choices[[i]]$Line.Color_dy, selected = V$Sel[[i]]$Line.Color_dy)
updateSelectInput(session, "Line.Type_dy", choices = V$Choices[[i]]$Line.Type_dy, selected = V$Sel[[i]]$Line.Type_dy)
updateSelectInput(session, "Line.Type_dy", choices = V$Choices[[i]]$Line.Type_dy, selected = V$Sel[[i]]$Line.Type_dy)
#Saved.Figs_dy is not updated when switching datasets (global)
updateSelectInput(session, "Fill.Color", choices = V$Choices[[i]]$Fill.Color, selected = V$Sel[[i]]$Fill.Color)
updateSelectInput(session, "Edge.Color", choices = V$Choices[[i]]$Edge.Color, selected = V$Sel[[i]]$Edge.Color)

# # # Dynamic objects without choices
#Saved.Width is not updated when switching datasets (global)
#Saved.Height is not updated when switching datasets (global)

# # # Static objects with choices
updateSelectInput(session, "Scatter.Color.Theme", choices = choices$Scatter.Color.Theme, selected = V$Sel[[i]]$Scatter.Color.Theme)
updateSelectInput(session, "Line.Order", choices = choices$Line.Order, selected = V$Sel[[i]]$Line.Order)
updateSelectInput(session, "CatType", choices = choices$CatType, selected = V$Sel[[i]]$CatType)
updateCheckboxGroupInput(session, "Cat.Color.Boxes", inline = TRUE, choices = choices$Cat.Color.Boxes, selected = V$Sel[[i]]$Cat.Color.Boxes)
updateSelectInput(session, "Cat.Color.Theme", choices = choices$Cat.Color.Theme, selected = V$Sel[[i]]$Cat.Color.Theme)
updateSelectInput(session, "Error", choices = choices$Error, selected = V$Sel[[i]]$Error)
updateSelectInput(session, "Title.Position", choices = choices$Title.Position, selected = V$Sel[[i]]$Title.Position)
checkboxGroupInput("Legend", "", inline = TRUE, choices = choices$Legend, selected = V$Sel[[i]]$Legend)
updateSelectInput(session, "Legend.Position", choices = choices$Legend.Position, selected = V$Sel[[i]]$Legend.Position)

updateCheckboxGroupInput(session, "Trans.Log", inline = TRUE, choices = choices$Trans.Log, selected = V$Sel[[i]]$Trans.Log)
updateCheckboxGroupInput(session, "Trans.Std", inline = TRUE, choices = choices$Trans.Std, selected = V$Sel[[i]]$Trans.Std)
updateCheckboxGroupInput(session, "Fit.Models", choices = choices$Fit.Models, selected = V$Sel[[i]]$Fit.Models)
updateCheckboxGroupInput(session, "Models", NULL, inline = TRUE, choices = choices$Models, selected = V$Sel[[i]]$Models)

# # # Static objects without choices
updateTextInput(session, "xlab", value = V$Sel[[i]]$xlab)
updateCheckboxInput(session, "xlab.Rotate", value = V$Sel[[i]]$xlab)
updateTextInput(session, "ylab", value = V$Sel[[i]]$ylab)
updateTextInput(session, "Title", value = V$Sel[[i]]$Title)
updateTextInput(session, "Legend.Title", value = V$Sel[[i]]$Legend.Title)
updateTextInput(session, "xmin", value = V$Sel[[i]]$xmin)
updateTextInput(session, "xmax", value = V$Sel[[i]]$xmax)
updateSliderInput(session, "xpad", value = V$Sel[[i]]$xpad)
updateTextInput(session, "ymin", value = V$Sel[[i]]$ymin)
updateTextInput(session, "ymax", value = V$Sel[[i]]$ymax)
updateTextInput(session, "Width", value = V$Sel[[i]]$Width)
updateTextInput(session, "Height", value = V$Sel[[i]]$Height)
updateColourInput(session, "Sym.Color", value = V$Sel[[i]]$Sym.Color)
updateNumericInput(session, "Sym.Size", value = V$Sel[[i]]$Sym.Size)
updateNumericInput(session, "Sym.Stroke", value = V$Sel[[i]]$Sym.Stroke)
updateNumericInput(session, "Sym.Trans", value = V$Sel[[i]]$Sym.Trans)
updateColourInput(session, "Line.Color", value = V$Sel[[i]]$Line.Color)
updateNumericInput(session, "Line.Size", value = V$Sel[[i]]$Line.Size)
updateColourInput(session, "Fill.Color", value = V$Sel[[i]]$Fill.Color)
updateColourInput(session, "Edge.Color", value = V$Sel[[i]]$Edge.Color)
updateNumericInput(session, "Cat.Edge", value = V$Sel[[i]]$Cat.Edge)
updateNumericInput(session, "Error.Cap", value = V$Sel[[i]]$Error.Cap)
updateSliderInput(session, "Spacing.X", value = V$Sel[[i]]$Spacing.X)
updateSliderInput(session, "Spacing.Group", value = V$Sel[[i]]$Spacing.Group)
updateNumericInput(session, "Size.Tick", value = V$Sel[[i]]$Size.Tick)
updateNumericInput(session, "Size.Lab", value = V$Sel[[i]]$Size.Lab)
updateNumericInput(session, "Size.Title", value = V$Sel[[i]]$Size.Title)
updateNumericInput(session, "Size.Legend", value = V$Sel[[i]]$Size.Legend)
updateNumericInput(session, "Legend.Key.Size", value = V$Sel[[i]]$Legend.Key.Size)
updateNumericInput(session, "Legend.Key.Size", value = V$Sel[[i]]$Legend.Key.Size)
updateNumericInput(session, "Size.Linear", value = V$Sel[[i]]$Size.Linear)
updateTextInput(session, "Fig.Name", value = V$Sel[[i]]$Fig.Name)





