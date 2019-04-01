
### Shiny module 
### UI part
moduleUI <- function(id, temp_df, add_tag ){
  
  ns <- NS(id)
  AA=tagList()
  # return a list of tags
  for (i in 1:length(colnames(temp_df) ) )
  {
    if (is.numeric(temp_df[[i]]))
    {
      AA[[i]]<-numericInput(ns(paste0(colnames(temp_df)[i],"_add",add_tag) ), paste0(colnames(temp_df)[i],":"),0)
    } 
    # else if( is.Date(temp_df[[i]])){
    #   AA[[i]]<-dateInput(paste0(colnames(temp_df)[i],"_add",add_tag),paste0(colnames(temp_df)[i],":"), Sys.Date())
    # }
    else{ 
      AA[[i]]<-textInput(ns(paste0(colnames(temp_df)[i],"_add",add_tag)), paste0(colnames(temp_df)[i],":") )
    }
  }
  return(AA)
}

### Server part 
module_server<- function(input, output, session,temp_df, add_tag) {
  
  
  dataframe=reactive({
    
    inputlist=list()
    for (j in 1:length(colnames(temp_df) ) ){
      inputlist[j]=input[[paste0(colnames(temp_df)[j],"_add",add_tag )]]
    }
    df_matrix=do.call(cbind.data.frame,inputlist)
    
    df_temp=data.frame(df_matrix)
    colnames(df_temp)=colnames(temp_df)
    df_temp
  })
  
  # Return the reactive that yields the data frame
  return(dataframe)
}
