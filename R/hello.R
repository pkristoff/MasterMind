
library(shiny)
library(shinyjs)
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

setupRadio <-
  function (session,
            inputId,
            gameColors)
  {
    choiceNames <- list()
    choiceValues <- list()
    for (gameColor in gameColors) {
      # htmlColor <- paste0('<p style="color:',gameColor,';font-size:10px;">',gameColor,"</p>")
      htmlColor <- gameColor
      choiceNames <- append(htmlColor, choiceNames)
      choiceValues <- append(gameColor, choiceValues)
    }

    updateRadioButtons(session,
                       inputId,
                       choiceNames = choiceNames,
                       choiceValues = gameColors)
  }

# radio buttons for making guess
radioId1 <- 'radiocell1'
radioId2 <- 'radiocell2'
radioId3 <- 'radiocell3'
radioId4 <- 'radiocell4'

# radio buttons for showing the actual winning result
codecell1 <- 'codecell1'
codecell2 <- 'codecell2'
codecell3 <- 'codecell3'
codecell4 <- 'codecell4'

# radio buttons for showing the current guess
guesscell1 <- 'guesscell1'
guesscell2 <- 'guesscell2'
guesscell3 <- 'guesscell3'
guesscell4 <- 'guesscell4'

mmUI <- function() {
  print('starting mmUI')
  generateRadioButton <- function(id, label) {
    htmlRed <- "<p style='color:red;font-size:10px;'>Red</p>"
    htmlGreen <- "<p style='color:green;font-size:10px;'>Green</p>"
    htmlBlue <- "<p style='color:blue;font-size:10px;'>Blue</p>"
    htmlOrange <-
      "<p style='color:orange;font-size:10px;'>Orange</p>"
    htmlBlack <- "<p style='color:black;font-size:10px;'>Black</p>"
    htmlWhite <- "<p style='color:white;font-size:10px;'>White</p>"

    column(
      3,
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
      textOutput(id)
    )
  }

  fluidPage(
    useShinyjs(),
    conditionalPanel(
      condition = "output.mindState == 'preGame'",
      'Pre Game',
      fluid = TRUE,
      sidebarLayout(sidebarPanel(fluidRow(
        # column(1, ),
        column(
          6,
          radioButtons(
            "numOfPicks",
            "Number of Picks:",
            # inputId = 'numOfPicksii',
            choices = list('1', '2', '3', '4')
          ),
          textOutput("txtNumOfPicks")
        ),
        column(
          6,
          radioButtons(
            "numOfColors",
            "Number of Colors",
            # inputId = 'numOfColorsii',
            # choiceNames = list('1', '2', '3', '4', '5', '6'),
            choices = list('1', '2', '3', '4', '5', '6')
          ),
          textOutput("txtNumOfColors")
        )
      )),
      mainPanel(# buttons
        fixedRow(
          column(2, actionButton("startGame", "Start Game"))
        )))
    ),
    conditionalPanel(
      condition = "output.mindState == 'mindGame'",
      'Game',
      fluid = TRUE,
      sidebarLayout(
        sidebarPanel(
          shinyjs::useShinyjs(),
          fluidRow(
            style = "background-color:aqua;",
            # used for making a guess of the hidden code.
            generateRadioButton(radioId1, "Cell 1"),
            generateRadioButton(radioId2, "Cell 2"),
            generateRadioButton(radioId3, "Cell 3"),
            generateRadioButton(radioId4, "Cell 4")
          )
        ),

        mainPanel(fixedRow(
          column(
            12,
            fixedRow(
              style = "background-color:pink;",
              column(3, 'Code'),
              # the actual hidden code
              column(1, style = "", htmlOutput(codecell1)),
              column(1, style = "", htmlOutput(codecell2)),
              column(1, style = "", htmlOutput(codecell3)),
              column(1, style = "", htmlOutput(codecell4))
            ),
            fixedRow(
              style = "background-color:aqua;",
              column(3, 'Current Guess'),
              column(1, htmlOutput(guesscell1)),
              column(1, htmlOutput(guesscell2)),
              column(1, htmlOutput(guesscell3)),
              column(1, htmlOutput(guesscell4))
            ),
            fixedRow(
              column(12, style = "background-color:pink;", dataTableOutput('board'))
            ),
            # buttons
            fixedRow(column(
              2, actionButton("showResults", "Show result")
            )),
            fixedRow(column(
              2, actionButton("showCode", "Show code")
            )),
            fixedRow(column(
              2, actionButton("startNewGame", "Start new game")
            )),
            fixedRow(column(
              2, actionButton("quitGame", "Quit")
            ))
          )
        ))
      )
    ),
    htmlOutput('mindState')
  )
  # print('ending mmUI')
}
updateResults <- function (input, code, numOfPicks) {
  nextPos <- numOfPicks + 2

  if (numOfPicks == 1) {
    result <- c(input$cell1,
                '',
                '')
    guess <- c(input$cell1)
  } else if (numOfPicks == 2) {
    result <- c(input$cell1,
                input$cell2,
                '',
                '',
                '')
    guess <- c(input$cell1,
               input$cell2)
  } else if (numOfPicks == 3) {
    result <- c(input$cell1,
                input$cell2,
                input$cell3,
                '',
                '',
                '',
                '')
    guess <- c(input$cell1,
               input$cell2,
               input$cell3)
  } else{
    result <- c(input$guesscell1,
                input$guesscell2,
                input$guesscell3,
                input$guesscell4,
                '',
                '',
                '',
                '',
                '')
    guess <- c(input$guesscell1,
               input$guesscell2,
               input$guesscell3,
               input$guesscell4)
  }

  # print(paste('code:', code))
  # print(paste('guess:', guess))
  posFound <- c()
  posNotFound <- c()
  for (pos in 1:numOfPicks) {
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
    for (pos2 in 1:numOfPicks) {
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
  print(result)
  result
}
server <- function(input, output, session) {
  print('starting server')
  numOfColors <- 0
  numOfPicks <- 0
  output$mindState <- reactive({
    'preGame'
  })
  # values <- reactiveValues(mindState = 'preGame')
  availableColors <-
    c('black', 'blue', 'green', 'orange', 'red', 'white')
  gameColors <- NULL
  code <- NULL
  localBoard <- NULL
  currentRowIndex <- 1
  # Update current row
  output$guesscell1 <- renderText({
    print(paste('input$radiocell1', input$radiocell1))
    paste("<p style='color:", input$radiocell1, ";'>O</p>")
  })
  output$guesscell2 <- renderText({
    paste("<p style='color:", input$radiocell2, ";'>O</p>")
  })
  output$guesscell3 <- renderText({
    paste("<p style='color:", input$radiocell3, ";'>O</p>")
  })
  output$guesscell4 <- renderText({
    paste("<p style='color:", input$radiocell4, ";'>O</p>")
  })

  observeEvent(input$startNewGame, {
    output$mindState <- renderText('preGame')
  })

  observeEvent(input$quitGame, {
    stopApp()
  })

  observeEvent(input$startGame, {
    print("Start Game:")
    numOfColors <<- as.numeric(input$numOfColors)
    numOfPicks <<- as.numeric(input$numOfPicks)
    print(paste("  numOfPicks", input$numOfPicks))
    print(paste("  numOfColors", input$numOfColors))
    localBoard <<- matrix('',
                          ncol = numOfPicks * 2 + 1,
                          nrow = 10,
                          byrow = TRUE)
    gameColors <<- availableColors[1:numOfColors]
    print(paste("  gameColors", gameColors))

    # set radio buttons based on number of colors
    setupRadio(session, radioId1, gameColors)
    setupRadio(session, radioId2, gameColors)
    setupRadio(session, radioId3, gameColors)
    setupRadio(session, radioId4, gameColors)

    # enable/disable based on number of picks
    if_else(numOfPicks >0, enable(radioId1), disable(radioId1))
    if_else(numOfPicks >1, enable(radioId2), disable(radioId2))
    if_else(numOfPicks >2, enable(radioId3), disable(radioId3))
    if_else(numOfPicks >3, enable(radioId4), disable(radioId4))

    code <<- sample(gameColors, numOfPicks, replace = FALSE)
    print(paste("  code=", code))
    output$mindState <- renderText('mindGame')
  })

  observeEvent(input$showResults, {
    print(paste("localBoard.ncol=", ncol(localBoard)))
    localBoard[currentRowIndex,] <<-
      updateResults(input, code, numOfPicks)
    printLocalBoard()
    outputBoard()
    currentRowIndex <<- currentRowIndex + 1
    updateRadio(session, radioId1, label = 'Cell 1', value = 'black')
    updateRadio(session, radioId2, label = 'Cell 2', value = 'black')
    updateRadio(session, radioId3, label = 'Cell 3', value = 'black')
    updateRadio(session, radioId4, label = 'Cell 4', value = 'black')
  })
  observeEvent(input$showCode, {
    output$codecell1 <- renderText({
      paste("<p style='color:", code[1], ";'>O</p>")
    })
    output$codecell2 <- renderText({
      paste("<p style='color:", code[2], ";'>O</p>")
    })
    output$codecell3 <- renderText({
      paste("<p style='color:", code[3], ";'>O</p>")
    })
    output$codecell4 <- renderText({
      paste("<p style='color:", code[4], ";'>O</p>")
    })
  })
  printLocalBoard <- function() {
    # print(localBoard[currentRowIndex, ])
  }
  outputBoard <- function() {
    if (numOfPicks == 1) {
      val <- c('0', 'p')
      columnNames <- c('1', '')
    } else if (numOfPicks == 2) {
      val <- c('0', '0', 'p', 'p')
      columnNames <- c('1', '2', '', '', '')
    } else if (numOfPicks == 3) {
      val <- c('0', '0', '0', 'p', 'p', 'p')
      columnNames <- c('1', '2', '3', '', '', '', '')
    } else{
      val <- c('0', '0', '0', '0', 'p', 'p', 'p', 'p')
      columnNames <- c('1', '2', '3', '4', '', '', '', '', '')
    }
    values = matrix(val,
                    ncol = numOfPicks * 2,
                    nrow = 10,
                    byrow = TRUE)

    xxx <- renderDataTable({
      dat <-
        datatable(
          localBoard,
          colnames = columnNames,
          container = withTags(table(class = 'display',
                                     thead(
                                       tr(
                                         th(colspan = numOfPicks, style = "text-align:center", 'Guess'),
                                         th(colspan = 1, style = "text-align:center", ''),
                                         th(colspan = numOfPicks, style = "text-align:center", 'Result')
                                       ),
                                       tr(lapply(columnNames, th))
                                     ))),
          options = list(
            paging = FALSE,
            searching = FALSE,
            ordering = FALSE,
            dom = 't'
          )
        ) %>%
        formatStyle(
          columns = 1:numOfPicks,
          valueColumns = 1:numOfPicks,
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

print('done loading app')
shinyApp(ui = mmUI(), server = server)
