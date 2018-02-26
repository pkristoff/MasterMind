#

library(shiny)
library(xtable)
library(DT)
library(dplyr)

updateRadio <-
  function (session,
            inputId,
            label = NULL,
            value = NULL)
  {
    message <- list(label = label, value = value)
    session$sendInputMessage(inputId, message)
  }

mmUI <- function() {
  generateRadioButton <- function(id, label) {
    htmlRed <- "<p style='color:red;'>Red</p>"
    htmlGreen <- "<p style='color:green;'>Green</p>"
    htmlBlue <- "<p style='color:blue;'>Blue</p>"
    htmlOrange <- "<p style='color:orange;'>Orange</p>"
    htmlBlack <- "<p style='color:black;'>Black</p>"
    htmlWhite <- "<p style='color:white;'>White</p>"

    column(
      4,
      radioButtons(
        id,
        label,
        choiceNames = list(
          HTML(htmlBlack),
          HTML(htmlWhite),
          HTML(htmlOrange),
          HTML(htmlBlue),
          HTML(htmlGreen),
          HTML(htmlRed)
        ),
        choiceValues = list("black", "white", "orange", "blue", "green", "red")
      ),
      textOutput(paste("txt", id))
    )
  }
  pageWithSidebar(
    headerPanel("Master Mind"),

    sidebarPanel(
      shinyjs::useShinyjs(),
      fluidRow(
        generateRadioButton("cell1", "Cell 1"),
        generateRadioButton("cell2", "Cell 2"),
        generateRadioButton("cell3", "Cell 3"),
        generateRadioButton("cell4", "Cell 4")
      )
    ),

    mainPanel(fixedRow(
      column(
        12,
        #style = "background-color:pink;",
        fixedRow(
          column(1, style = "background-color:aqua;", htmlOutput('txtcell1')),
          column(1, style = "background-color:aqua;", htmlOutput('txtcell2')),
          column(1, style = "background-color:aqua;", htmlOutput('txtcell3')),
          column(1, style = "background-color:aqua;", htmlOutput('txtcell4'))
        ),
        fixedRow(
          column(6, style = "background-color:pink;", dataTableOutput('board'))
        ),
        fixedRow(column(
          3, style = "background-color:aqua;", actionButton("showResults", "Show Results")
        ))
      )
    ))
  )
}
server <- function(input, output, session) {
  code = sample(c('black', 'blue', 'green', 'orange', 'red', 'white'), 4)
  print(code)
  localBoard <- matrix('',
                       ncol = 8,
                       nrow = 10,
                       byrow = TRUE)
  currentRowIndex <- 1
  # Update current row
  output$txtcell1 <- renderText({
    paste("<p style='color:", input$cell1, ";'>O</p>")
  })
  output$txtcell2 <- renderText({
    paste("<p style='color:", input$cell2, ";'>O</p>")
  })
  output$txtcell3 <- renderText({
    paste("<p style='color:", input$cell3, ";'>O</p>")
  })
  output$txtcell4 <- renderText({
    paste("<p style='color:", input$cell4, ";'>O</p>")
  })


  #' called if New Game button is called.
  #'
  #' @examples
  #'
  observeEvent(input$showResults, {
    nextPos <- 5
    result <- c(input$cell1,
                input$cell2,
                input$cell3,
                input$cell4,
                0,
                0,
                0,
                0)
    guess <- c(input$cell1,
               input$cell2,
               input$cell3,
               input$cell4)
    # print(paste('code:', code))
    # print(paste('guess:', guess))
    posFound <- c()
    posNotFound <- c()
    for (pos in 1:4) {
      if (guess[pos] == code[pos]) {
        posFound <- append(posFound, pos)
        result[nextPos] <- 'black'
        nextPos <- nextPos + 1
      } else {
        posNotFound <- append(posNotFound, pos)
      }
    }
    # print(paste('posFound:', posFound))
    # print(paste('posNotFound:', posNotFound))
    for (posNF in posNotFound) {
      color = guess[posNF]
      # print(paste('looking for color:',color))
      for (pos2 in 1:4) {
        if (match(pos2, posFound, nomatch = 0) == 0) {
          # print(paste('code:',code))
          # print(paste('pos2:',pos2,'code[pos2]:',code[pos2]))
          # print(paste('pos2:', pos2, 'code[pos2]:', code[pos2], 'color:', color))
          if (color == code[pos2]) {
            # print(paste('found color:',color))
            posFound <- append(posFound, pos2)
            result[nextPos] <- 'white'
            nextPos <- nextPos + 1
          } else{
            # print(paste('code[pos2]:',code[pos2]))
          }
        }
      }
    }
    localBoard[currentRowIndex,] <<- result
    printLocalBoard()
    outputBoard()
    currentRowIndex <<- currentRowIndex + 1
    updateRadio(session, 'cell1', label = 'Cell 1', value = 'black')
    updateRadio(session, 'cell2', label = 'Cell 2', value = 'black')
    updateRadio(session, 'cell3', label = 'Cell 3', value = 'black')
    updateRadio(session, 'cell4', label = 'Cell 4', value = 'black')
  })
  printLocalBoard <- function() {
    print(localBoard[currentRowIndex, ])
  }
  outputBoard <- function() {
    values = matrix(
      c('0', '0', '0', '0', 'p', 'p', 'p', 'p'),
      ncol = 8,
      nrow = 10,
      byrow = TRUE
    )
    columnNames <- c('1', '2', '3', '4', '1', '2', '3', '4')

    xxx <- renderDataTable({
      dat <-
        datatable(
          localBoard,
          colnames = columnNames,
          container = withTags(table(class = 'display',
                                     thead(
                                       tr(
                                         th(colspan = 4, style = "text-align:center", 'Guess'),
                                         th(colspan = 4, style = "text-align:center", 'Result')
                                       ),
                                       tr(lapply(paste(columnNames), th))
                                     ))),
          options = list(
            paging = FALSE,
            searching = FALSE,
            ordering = FALSE,
            dom = 't'
          )
        ) %>%
        formatStyle(
          columns = 1:4,
          valueColumns = 1:4,
          color = styleEqual(
            levels = c('black', 'blue', 'green', 'orange', 'red', 'white'),
            values = c('black', 'blue', 'green', 'orange', 'red', 'white')
          )
        )
      return(dat)
    })
    # print(xxx)
    output$board <- xxx
  }
}

shinyApp(ui = mmUI(), server = server)
