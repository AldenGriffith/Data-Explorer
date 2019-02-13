


V$ON <- TRUE #indicates that a dataset has been uploaded (redundant after first time)

#Gets index of most recent DATA object (number of uploaded datasets)
i <- length(V$Data) + 1

#Sets stored choices to startup   NECESSARY?
V$Choices[[i]] <- choices

# #replace spaces with dots, etc...
# names(New.Data) <- make.names(names(New.Data), unique=TRUE)


#which variables are numeric?
vars.num <- sapply(New.Data, is.numeric)
# message(vars.cat)
# message("got here!")
vars.cat <- sapply(New.Data, is.character) | sapply(New.Data, is.factor)
# message(vars.cat)
# message(which(vars.cat))



#factor all cat vars (just in case - not always done for excel data?)
for (j in which(vars.cat)){

    New.Data[,j] <- factor(New.Data[,j])
}

message(str(New.Data,1))

#Adds new dataset to DATA object
V$Data[[i]] <- New.Data

#Stores name of dataset
if (!is.null(V$Load.Data)){
    V$Name[i] <- Data.Name
} else {
    V$Name[i] <- input$Data.Name
}

#Stores vectors of variable names
# V$Y[[i]] <- c("(none)", names(New.Data[vars.num]))
# V$X[[i]] <- c("(none)", names(New.Data))
# V$Cat[[i]] <- c("(none)", names(New.Data[!vars.num]))

V$Choices[[i]]$Y_dy <- c("(none)", names(New.Data[vars.num]))
V$Choices[[i]]$X_dy <- c("(none)", names(New.Data))
V$Choices[[i]]$Group_dy <- c("(none)", names(New.Data[vars.cat]))

# V$CatFirm[[i]] <- names(New.Data[vars.cat])
V$Choices[[i]]$Subset_dy <- V$Choices[[i]]$Group
V$Choices[[i]]$CatVars_dy <- setdiff(V$Choices[[i]]$Y_dy, "(none)")

V$Choices[[i]]$Scat.Error.X_dy <- V$Choices[[i]]$Y_dy
V$Choices[[i]]$Scat.Error.Y_dy <- V$Choices[[i]]$Y_dy

# V$CatVars[[i]] <- V$Choices[[i]]$Group[-which(V$Choices[[i]]$Group == "(none)")]
V$CatVars[[i]] <- list(Sel = NULL)

#Setup list for saved figures (allows for separate galleries for datasets)
V$Figs[[i]] <- list()
V$Figs.dim[[i]] <- list()

#Defaults to no group coloring
V$Group.Sym.Color[[i]] <- FALSE
V$Group.Line.Color[[i]] <- FALSE
V$Group.Cat.Color[[i]] <- FALSE
V$Group.Fit.Linear[[i]] <- FALSE
V$Group.Fit.Quadratic[[i]] <- FALSE
V$Group.Fit.Power[[i]] <- FALSE
V$Group.Fit.Exp[[i]] <- FALSE
V$Group.Fit.Custom[[i]] <- FALSE
V$form[[i]] <- "not fitted"
V$params[[i]] <- "not fitted"
V$exp.form[[i]] <- y ~ a*exp(b*x)
V$exp.params[[i]] <- list(a = 1, b = 0)

#disable the dataset switch observer action
new.upload <<- TRUE

#Switch 'current' dataset to most recently uploaded (observer will set new.upload to FALSE
V$Current <- i     

new.upload <<- TRUE

#Update the list of possible datasets (observer will set new.upload to FALSE)
updateSelectInput(session, "Use.Data",
                  choices = V$Name,
                  selected = V$Name[i])

output$Upload <- renderUI({
    
    fileInput('file', 'Upload new dataset (.csv, .xls, .xlsx)'#,
              # accept=c('text/csv',
              #          'text/comma-separated-values,text/plain',
              #          '.csv',
              #          '.xls',
              #          '.xlsx')
              )
})



message("newdata run")
# V$ON <- TRUE #indicates that a dataset has been uploaded (redundant after first time)

