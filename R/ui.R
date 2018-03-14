
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
          radioButtons("numOfPicks",
                       "Number of Picks:",
                       # inputId = 'numOfPicksii',
                       choices = list('1', '2', '3', '4')),
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
            fixedRow(column(2, actionButton(
              "quitGame", "Quit"
            )))
          )
        ))
      )
    ),
    htmlOutput('mindState')
  )
  # print('ending mmUI')
}
