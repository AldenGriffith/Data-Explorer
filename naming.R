
#Shortens suggested file name if necessary
if (nchar(input$file$name) > 20){
    
    name <- substr(input$file$name,1,20)
    name <- paste(name,"...", sep="")
} else {
    name <- input$file$name
}

#Does the name already exist?
if (V$ON) if (is.element(name, V$Name)){
    
    #Does the name already have multiples?
    i.names <- grep(name, V$Name)
    
    if (length(i.names) == 1){
        
        #if not, just put (2) at the end
        name <- paste(name, "(2)", sep="")
        
    } else {
        
        mults <- numeric() #vector for collecting numbers associated with multiple names
        
        #loop for existing names that match new one
        for (x in i.names){
            
            #returns 1 if last 3 chars of name are a number within parentheses
            z <- regexpr("(\\()[1-9](\\))", substr(V$Name[x],nchar(V$Name[x])-2,nchar(V$Name[x])))[1] 
            
            if (z == 1) { #if name has a number
                
                m <- as.numeric(substr(V$Name[x], nchar(V$Name[x])-1, nchar(V$Name[x])-1)) #gets the number associated with name
                
                mults <- c(mults, m)
                
            } else { #if name has no number (i.e. first one uploaded)
                
                mults <- c(mults, 0)
            }
            
        }
        
        name <- paste(name, "(", max(mults)+1, ")", sep="") #name with next number added in parentheses
        
    }
    
}