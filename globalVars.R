

colors <- c("Black", "Red", "Green", "Blue")

shapes <- c("Circle","Square","Triangle", "Diamond")

lty <- c("Solid","Dashed","Dotted","Long Dash", "Dot Dash", "Two Dash")

lookup = data.frame(
    sym = rep(shapes,2),
    fill = c(rep(FALSE,length(shapes)),rep(TRUE,length(shapes))),
    pch = c(1,0,2,5,
            16,15,17,18)
)

mean_sd <- function (x) {
    x <- stats::na.omit(x)
    SD <- stats::sd(x)
    MEAN <- mean(x)
    data.frame(y = MEAN, ymin = MEAN - SD, ymax = MEAN + SD)
}

no_error <- function (x) {
    x <- stats::na.omit(x)
    MEAN <- mean(x)
    data.frame(y = MEAN, ymin = MEAN, ymax = MEAN)
    
}

pals <- data.frame(
    label = c("Rainbow", "Tableau",               "Paul Tol",           "Colorblind",               "Google"),
    color = c("",        "scale_color_tableau()", "scale_color_ptol()", "scale_color_colorblind()", "scale_color_gdocs()"),
    fill =  c("",        "scale_fill_tableau()",  "scale_fill_ptol()",  "scale_fill_colorblind()",  "scale_fill_gdocs()")
    
)

legend.position <- data.frame(
    label =   c("Out: N", "Out: S", "Out: E", "Out: W", "In: NW", "In: NE", "In: SW", "In: SE", "In: N", "In: S", "In: E", "In: W"),
    outside = c("top",    "bottom", "right",  "left"),
    in.x =    c( NA,       NA,       NA,       NA,       .02,      .98,      0.02,     .98,      .5,      .5,      .98,     .02),
    in.y =    c( NA,       NA,       NA,       NA,       .98,      .98,      0.02,     .02,      .98,     .02,     .5,      .5)

)

#Function to deal with name lengths and tabs - surely a better way! with a loop etc...
name.len <- function(Group.name){
    
    if (nchar(Group.name) >= 25 ) {
        
        Group.name <- paste(substr(Group.name, 1, 25), "...", sep="")
        
    } else if (nchar(Group.name) < 24 & nchar(Group.name) >=16) {
        
        Group.name <- paste(Group.name, "\t", sep="")
        
    } else if (nchar(Group.name) < 16 & nchar(Group.name) >=8) {
        
        Group.name <- paste(Group.name, "\t\t", sep="")
        
    } else if (nchar(Group.name) < 8) {
        
        Group.name <- paste(Group.name, "\t\t\t", sep="")
    }
    
    return(Group.name)
}


#backticks function!! (https://code.i-harness.com/en/q/cd293b)
bt <- function(x) {
    if (class(x) != "character") {
        return(x)
    }
    y <- sapply(x, function(s) {
        if (!grepl("^`", s)) {
            s <- paste("`", s, sep="", collapse="")
        }
        if (!grepl("`$", s)) {
            s <- paste(s, "`", sep="", collapse="")
        }
    }
    )
    y 
}
