source("convolution_funcs.R")
library(shiny)
library(shinyFiles)
library(jpeg)
library(magick)


plot_raster <- function(raster) {
  par(mfrow = c(1,1), mar = c(1,1,1,1))
  plot.new()
  as.raster(raster) |>
    rasterImage(xleft = 0, xright = 1, ytop = 0, ybottom = 1)
}



# ui.R ----
ui <- fluidPage(
  
  # SITE TITLE
  titlePanel(h1("Kernel convolutions")),
  
  sidebarLayout(
    position = "left",
    
   
    # SIDE PANEL            
    sidebarPanel(
      
      h3("Upload an image"),
      
      # Input: Select a file ----
      fileInput("img", "(supported filetype: jpg)",
                multiple = FALSE,
                accept = c("image/jpeg")),
    
      # Horizontal line
      tags$hr(),
      
      h3("Define a kernel"),
      
      splitLayout(
        textInput("11", label = "", value = "1"),
        textInput("12", label = "", value = "1"),
        textInput("13", label = "", value = "1")
      ),
      
      splitLayout(
        textInput("21", label = "", value = "1"),
        textInput("22", label = "", value = "1"),
        textInput("23", label = "", value = "1")
      ),
      
      splitLayout(
        textInput("31", label = "", value = "1"),
        textInput("32", label = "", value = "1"),
        textInput("33", label = "", value = "1")
      ),
      
      # Horizontal line
      tags$hr(),
      
      actionButton("generate", "Convolve!")
  
      ),
      
    
    
    # MAIN PANEL ----
    mainPanel(
      h2("Image output"),
      imageOutput("image_preview", width = "100px")
    )
    
  )
)

# server.R ----
server <- function(input, output, session) {
  
  output$image_preview <- renderImage({
    req(input$img)
    input_image <- input$img
    
    rgb_matrix <- reactive({
      if (is.null(input_image)) {
        return(NULL)
      }
      image <- image_read(input_image$datapath)
      matrix <- image_data(image)
      return(rgb_matrix)
    })
    
    print(rgb_matrix)
    print(dim(rgb_matrix))
    
    # Getting the matrix
    conv_mat <- reactive({
      input <- input$generate
      conv_mat <- c(
        input$"11", input$"12", input$"13",
        input$"21", input$"22", input$"23",
        input$"31", input$"32", input$"33"
      ) |> matrix(ncol=3, nrow=3, byrow=T)
      return(conv_mat)
    })
    
    print(conv_mat)
    
    
    list(src = as.character(input_image$datapath),
         contentType = input_image$type)
  
    }, deleteFile=T)
  
  
}

# Run the app ----
shinyApp(ui, server)
