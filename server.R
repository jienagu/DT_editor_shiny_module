
library(shiny)
library(shinyjs)
## shinysky is to customize buttons
library(shinysky)
library(DT)
library(data.table)
library(lubridate)
library(shinyalert)

rm(list = ls())
useShinyalert()
source("shiny_module.R")

### This app utilize shiny module that can fit for any data.table
### You just need to create your data.table and load it to the same location of your shiny app as note.rds
### Currently, I am working on an elegant way to handle Date format so please convert any date col to character/factor 
### before you load the data.table to this app

### For example:
### DT3=data.table(iris[1:3,])
### saveRDS(DT3, "path of your shiny app/note.rds")


shinyServer(function(input, output, session){
  
  ### interactive dataset 
  vals_trich<-reactiveValues()
  vals_trich$Data<-readRDS("note.rds")
  

  #### MainBody_trich is the id of DT table
  output$MainBody_trich<-renderUI({
    fluidPage(
          hr(),
          column(6,offset = 6,
                 HTML('<div class="btn-group" role="group" aria-label="Basic example" style = "padding:10px">'),
                 ### tags$head() This is to change the color of "Add a new row" button
                 tags$head(tags$style(".butt2{background-color:#231651;} .butt2{color: #e6ebef;}")),
                 div(style="display:inline-block;width:30%;text-align: center;",actionButton(inputId = "Add_row_head",label = "Add", class="butt2") ),
                 tags$head(tags$style(".butt4{background-color:#4d1566;} .butt4{color: #e6ebef;}")),
                 div(style="display:inline-block;width:30%;text-align: center;",actionButton(inputId = "mod_row_head",label = "Edit", class="butt4") ),
                 tags$head(tags$style(".butt3{background-color:#590b25;} .butt3{color: #e6ebef;}")),
                 div(style="display:inline-block;width:30%;text-align: center;",actionButton(inputId = "Del_row_head",label = "Delete", class="butt3") ),
                 ### Optional: a html button 
                 # HTML('<input type="submit" name="Add_row_head" value="Add">'),
                 HTML('</div>') ),
          
          column(12,dataTableOutput("Main_table_trich")),
            tags$script("$(document).on('click', '#Main_table_trich button', function () {
                    Shiny.onInputChange('lastClickId',this.id);
                    Shiny.onInputChange('lastClick', Math.random()) });")

      ) 
    })
  
  #### render DataTable part ####
  output$Main_table_trich<-renderDataTable({
    DT=vals_trich$Data
    datatable(DT,selection = 'single',
              escape=F) 
    })
  

  observeEvent(input$Add_row_head, {
    ### This is the pop up board for input a new row
    showModal(modalDialog(title = "Add a new row",
                          moduleUI("module_input_id",temp_df=vals_trich$Data, add_tag=input$Add_row_head),
                          actionButton("go", "Add item"),
                          easyClose = TRUE, footer = NULL ))
    
  })
  ### Add a new row to DT  
  observeEvent(input$go, {
    datafile <- callModule(module_server, "module_input_id", temp_df=vals_trich$Data, add_tag=input$Add_row_head )
    new_row=data.frame( datafile() )
    
    vals_trich$Data<-data.table(rbind(vals_trich$Data, new_row ) )
    removeModal()
  })
  
    
 

  ### save to RDS part 
  observeEvent(input$Updated_trich,{
    saveRDS(vals_trich$Data, "note.rds")
    shinyalert(title = "Saved!", type = "success")
  })
  
  ### delete selected rows part
  ### this is warning messge for deleting
  observeEvent(input$Del_row_head,{
    showModal(
      if(length(input$Main_table_trich_rows_selected)>=1 ){
        modalDialog(
        title = "Warning",
      paste("Are you sure delete",length(input$Main_table_trich_rows_selected),"rows?" ),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("ok", "Yes")
      ), easyClose = TRUE)
      }else{
        modalDialog(
          title = "Warning",
        paste("Please select row(s) that you want to delect!" ),easyClose = TRUE
        )
      }
    
    )
  })
  
  ### If user say OK, then delete the selected rows
  observeEvent(input$ok, {
     vals_trich$Data=vals_trich$Data[-input$Main_table_trich_rows_selected]
     removeModal()
  })
  
  ### edit button
  observeEvent(input$mod_row_head,{
    showModal(
      if(length(input$Main_table_trich_rows_selected)>=1 ){
        modalDialog(
          fluidPage(
            h3(strong("Modification"),align="center"),
            hr(),
            dataTableOutput('row_modif'),
            actionButton("save_changes","Save changes"),
            tags$script(HTML("$(document).on('click', '#save_changes', function () {
                             var list_value=[]
                             for (i = 0; i < $( '.new_input' ).length; i++)
                             {
                             list_value.push($( '.new_input' )[i].value)
                             }
                             Shiny.onInputChange('newValue', list_value) });")) ), size="l" )
      }else{
        modalDialog(
          title = "Warning",
          paste("Please select the row that you want to edit!" ),easyClose = TRUE
        )
      }
      
    )
  })
  

    

  #### modify part
  output$row_modif<-renderDataTable({
    selected_row=input$Main_table_trich_rows_selected
    old_row=vals_trich$Data[selected_row]
    row_change=list()
    for (i in colnames(old_row))
    {
      if (is.numeric(vals_trich$Data[[i]]))
      {
        row_change[[i]]<-paste0('<input class="new_input" value= ','"',old_row[[i]],'"','  type="number" id=new_',i,' ><br>')
      } 
      # else if( is.Date(vals_trich$Data[[i]])){
      #   row_change[[i]]<-paste0('<input class="new_input" value= ','"',old_row[[i]],'"',' type="date" id=new_  ',i,'  ><br>') 
      # }
      else{ 
        row_change[[i]]<-paste0('<input class="new_input" value= ','"',old_row[[i]],'"',' type="textarea"  id=new_',i,'><br>')
      }
    }
    row_change=as.data.table(row_change)
    setnames(row_change,colnames(old_row))
    DT=row_change
    DT 
    },escape=F,options=list(dom='t',ordering=F,scrollX = TRUE),selection="none" )
  
  

  ### This is to replace the modified row to existing row
  observeEvent(input$newValue,
               {
                 newValue=lapply(input$newValue, function(col) {
                   if (suppressWarnings(all(!is.na(as.numeric(as.character(col)))))) {
                     as.numeric(as.character(col))
                   } else {
                     col
                   }
                 })
                 DF=data.frame(lapply(newValue, function(x) t(data.frame(x))))
                 colnames(DF)=colnames(vals_trich$Data)
                 vals_trich$Data[input$Main_table_trich_rows_selected]<-DF
               }
  )
 ### This is nothing related to DT Editor but I think it is nice to have a download function in the Shiny so user 
 ### can download the table in csv
  output$Trich_csv<- downloadHandler(
    filename = function() {
      paste("Trich Project-Progress", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(data.frame(vals_trich$Data), file, row.names = F)
    }
  )
 
})
