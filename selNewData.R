#Default values and choices for input objects

i <- V$Current

# # # Dynamic objects with choices
#Theses objects already have their V$Choices data stored during newData.R
updateSelectInput(session, "X_dy", choices = V$Choices[[i]]$X_dy, selected = sels$X_dy)
updateSelectInput(session, "Y_dy", choices = V$Choices[[i]]$Y_dy, selected = sels$Y_dy)
updateSelectInput(session, "Group_dy", choices = V$Choices[[i]]$Group_dy, selected = sels$Group_dy)
updateSelectInput(session, "Subset_dy", choices = V$Choices[[i]]$Subset_dy, selected = sels$Subset_dy)
updateSelectInput(session, "SubSel_dy", choices = V$Choices[[i]]$SubSel_dy, selected = sels$SubSel_dy)
updateSelectInput(session, "CatVars_dy", choices = V$Choices[[i]]$CatVars_dy, selected = V$CatVars[[i]]$Sel)
updateSelectInput(session, "Scat.Error.X_dy", choices = V$Choices[[i]]$Scat.Error.X_dy, selected = sels$Scat.Error.X_dy)
updateSelectInput(session, "Scat.Error.Y_dy", choices = V$Choices[[i]]$Scat.Error.Y_dy, selected = sels$Scat.Error.Y_dy)


#These objects are being set to the startup choices
updateCheckboxGroupInput(session, "Symbols_dy", inline = TRUE, choices = choices$Symbols_dy, selected = sels$Symbols_dy)
updateSelectInput(session, "Sym.Shape_dy", choices = choices$Sym.Shape_dy, selected = sels$Sym.Shape_dy)
updateCheckboxGroupInput(session, "Lines_dy", inline = TRUE, choices = choices$Lines_dy, selected = sels$Lines_dy)
updateSelectInput(session, "Line.Color_dy", choices = choices$Line.Color_dy, selected = sels$Line.Color_dy)
updateSelectInput(session, "Line.Type_dy", choices = choices$Line.Type_dy, selected = sels$Line.Type_dy)
#Saved.Figs_dy is not updated when switching datasets (global)
updateSelectInput(session, "Fill.Color", choices = choices$Fill.Color, selected = sels$Fill.Color)
updateSelectInput(session, "Edge.Color", choices = choices$Edge.Color, selected = sels$Edge.Color)

# # # Dynamic objects without choices
#Saved.Width is not updated when switching datasets (global)
#Saved.Height is not updated when switching datasets (global)

# # # Static objects with choices
updateSelectInput(session, "Scatter.Color.Theme", choices = choices$Scatter.Color.Theme, selected = sels$Scatter.Color.Theme)
updateSelectInput(session, "Line.Order", choices = choices$Line.Order, selected = sels$Line.Order)
updateSelectInput(session, "CatType", choices = choices$CatType, selected = sels$CatType)
updateCheckboxGroupInput(session, "Cat.Color.Boxes", inline = TRUE, choices = choices$Cat.Color.Boxes, selected = sels$Cat.Color.Boxes)
updateSelectInput(session, "Cat.Color.Theme", choices = choices$Cat.Color.Theme, selected = sels$Cat.Color.Theme)
updateSelectInput(session, "Error", choices = choices$Error, selected = sels$Error)
updateSelectInput(session, "Title.Position", choices = choices$Title.Position, selected = sels$Title.Position)
updateCheckboxGroupInput(session, "Legend", "", inline = TRUE, choices = choices$Legend, selected = sels$Legend)
updateSelectInput(session, "Legend.Position", choices = choices$Legend.Position, selected = sels$Legend.Position)

updateCheckboxGroupInput(session, "TransLog", inline = TRUE, choices = choices$TransLog, selected = sels$TransLog)
updateCheckboxGroupInput(session, "TransStd", inline = TRUE, choices = choices$TransStd, selected = sels$TransStd)
updateCheckboxGroupInput(session, "Fit.Models", choices = choices$Fit.Models, selected = sels$Fit.Models)
updateCheckboxGroupInput(session, "Models", NULL, inline = TRUE, choices = choices$Models, selected = sels$Models)

# # # Static objects without choices
updateTextInput(session, "xlab", value = sels$xlab)
updateCheckboxInput(session, "xlab.Rotate", value = sels$xlab)
updateTextInput(session, "ylab", value = sels$ylab)
updateTextInput(session, "Title", value = sels$Title)
updateTextInput(session, "Legend.Title", value = sels$Legend.Title)
updateTextInput(session, "xmin", value = sels$xmin)
updateTextInput(session, "xmax", value = sels$xmax)
updateSliderInput(session, "xpad", value = sels$xpad)
updateTextInput(session, "ymin", value = sels$ymin)
updateTextInput(session, "ymax", value = sels$ymax)
updateTextInput(session, "Width", value = sels$Width)
updateTextInput(session, "Height", value = sels$Height)
updateColourInput(session, "Sym.Color", value = sels$Sym.Color)
updateNumericInput(session, "Sym.Size", value = sels$Sym.Size)
updateNumericInput(session, "Sym.Stroke", value = sels$Sym.Stroke)
updateNumericInput(session, "Sym.Trans", value = sels$Sym.Trans)
updateColourInput(session, "Line.Color", value = sels$Line.Color)
updateNumericInput(session, "Line.Size", value = sels$Line.Size)
updateColourInput(session, "Fill.Color", value = sels$Fill.Color)
updateColourInput(session, "Edge.Color", value = sels$Edge.Color)
updateNumericInput(session, "Cat.Edge", value = sels$Cat.Edge)
updateNumericInput(session, "Error.Cap", value = sels$Error.Cap)
updateSliderInput(session, "Spacing.X", value = sels$Spacing.X)
updateSliderInput(session, "Spacing.Group", value = sels$Spacing.Group)
updateNumericInput(session, "Size.Tick", value = sels$Size.Tick)
updateNumericInput(session, "Size.Lab", value = sels$Size.Lab)
updateNumericInput(session, "Size.Title", value = sels$Size.Title)
updateNumericInput(session, "Size.Legend", value = sels$Size.Legend)
updateNumericInput(session, "Legend.Key.Size", value = sels$Legend.Key.Size)
updateNumericInput(session, "Size.Linear", value = sels$Size.Linear)

updateTextInput(session, "Fig.Name", value = sels$Fig.Name)

