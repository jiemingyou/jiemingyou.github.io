source("convolution_funcs.R")
library(shiny)
library(jpeg)


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
      fileInput("img", "(supported filetypes: jpg or png)",
                multiple = FALSE,
                accept = c(".jpg", ".jpeg")),
    
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

      imageOutput("imgoutput")
    
      )
  )
)

# server.R ----
server <- function(input, output, session) {
  
  output$imgoutput <- renderImage({
    
    req(input$img)
    
    # Reading the image
    tryCatch(
      {
        img_path = input$img["datapath"] |> dplyr::pull()
        img_type = input$img["type"]
        
        # Checks
        grepl("jpeg", img_type) == TRUE
        length(dim(img)) == 3
        
        img <- readJPEG(img_path, native = F)
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      }
    )
    
    # Getting the matrix
    observeEvent(input$generate, {

      conv_mat <- c(
        input$"11", input$"12", input$"13",
        input$"21", input$"22", input$"23",
        input$"31", input$"32", input$"33"
      ) |> matrix(ncol=3, nrow=3, byrow=T)
  
      print(conv_mat)
    })
    
    # Temp file for output
    outfile <- tempfile(fileext='.jpg')
    jpeg(img)
    plot_raster(img)
    dev.off()
    
    list(
      src = outfile,
      contentType = "image/jpeg"
    )
  
    }, deleteFile=T)
  
  
}

# Run the app ----
shinyApp(ui, server)
