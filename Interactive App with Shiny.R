#### Shiny Application 


server <- function(input, output) {
  output$force <- renderForceNetwork({
    forceNetwork(Links = iproj1_d3$links, Nodes = iproj1_d3$nodes,
                 Source = 'source', Target = 'target', NodeID = 'name',
                 Group = 'group')
  })
}
#### UI ####
ui <- basicPage(
  forceNetworkOutput("force")
)
#### Run ####
shinyApp(ui = ui, server = server)