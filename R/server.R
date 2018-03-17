library(shiny)
library(shinyjs)

library(xtable)
library(DT)
library(dplyr)

mmServer <- function(funct) {
  hideCode <- function(out) {
    js$clearCircle('codeCell1')
    js$clearCircle('codeCell2')
    js$clearCircle('codeCell3')
    js$clearCircle('codeCell4')
    out
  }
  setupRadio <-
    function (session,
              inputId,
              gameColors,
              shouldEnable) {

      if (shouldEnable) {
        enable(inputId)
        updateRadioButtons(session,
                           inputId,
                           choices = gameColors)
      } else{
        disable(inputId)
        updateRadioButtons(session,
                           inputId,
                           choices = c('do not pick'))
      }
    }
  showCode <- function (out, code, numOfPicks) {
    js$drawCircle('codeCell1', code[1])
    if (numOfPicks > 1) {
      js$drawCircle('codeCell2', code[2])
    }
    if (numOfPicks > 2) {
      js$drawCircle('codeCell3', code[3])
    }
    if (numOfPicks > 3) {
      js$drawCircle('codeCell4', code[4])
    }
    out
  }
  updateCurrentGuess <- function(input, output, numOfPicks) {
    output$guesscell1 <- renderText({
      print(paste('numOfPicks',numOfPicks, 'input$radiocell1',input$radiocell1))
      if (numOfPicks > 0) {
        js$drawCircle('guesscell1js', input$radiocell1)
        ''
        # paste("<p style='color:", input$radiocell1, ";'>O</p>")
      } else{
        js$clearCircle('guesscell1js', input$radiocell1)
      }
      ''
    })
    output$guesscell2 <- renderText({
      if (numOfPicks > 1) {
        js$drawCircle('guesscell2js', input$radiocell2)
        # paste("<p style='color:", input$radiocell2, ";'>O</p>")
      } else{
        ''
      }
      ''
    })
    output$guesscell3 <- renderText({
      if (numOfPicks > 2) {
        js$drawCircle('guesscell3js', input$radiocell3)
        paste("<p style='color:", input$radiocell3, ";'>O</p>")
      } else{
        ''
      }
      ''
    })
    output$guesscell4 <- renderText({
      if (numOfPicks > 3) {
        js$drawCircle('guesscell4js', input$radiocell4)
        paste("<p style='color:", input$radiocell4, ";'>O</p>")
      } else{
        ''
      }
      ''
    })

    output
  }
  updateRadio <-
    function (session,
              inputId,
              label = NULL,
              value = NULL) {
      message <- list(label = label, value = value)
      session$sendInputMessage(inputId, message)
    }
  updateResults <-
    function (code,
              numOfPicks,
              guess1,
              guess2,
              guess3,
              guess4) {
      resultPos <- numOfPicks + 2
      guesses <- c(guess1,
                   guess2,
                   guess3,
                   guess4)
      result <- guesses[1:numOfPicks]
      result <- append(result, '')
      result <- c(result, c('')[1:numOfPicks])

      myGuess <- guesses[1:numOfPicks]

      # print(paste('code:', code))
      # print(paste('myGuess:', myGuess))
      posFound <- c()
      posNotFound <- c()
      for (pos in 1:numOfPicks) {
        # print(paste('pos=', pos, 'nextPos', nextPos))
        if (myGuess[pos] == code[pos]) {
          posFound <- append(posFound, pos)
          result[resultPos] <- 'black'
          resultPos <- resultPos + 1
        } else {
          posNotFound <- append(posNotFound, pos)
        }
      }
      # print(paste('posFound:', posFound))
      # print(paste('posNotFound:', posNotFound))
      for (posNF in posNotFound) {
        color = myGuess[posNF]
        # print(paste('looking for color:',color))
        for (pos2 in 1:numOfPicks) {
          if (match(pos2, posFound, nomatch = 0) == 0) {
            # print(paste('code:',code))
            # print(paste('pos2:',pos2,'code[pos2]:',code[pos2]))
            # print(paste('pos2:', pos2, 'code[pos2]:', code[pos2], 'color:', color))
            if (color == code[pos2]) {
              # print(paste('found color:',color))
              posFound <- append(posFound, pos2)
              result[resultPos] <- 'white'
              resultPos <- resultPos + 1
            } else{
              # print(paste('code[pos2]:',code[pos2]))
            }
          }
        }
      }
      if (length(result) >= resultPos) {
        for (pos in resultPos:length(result)) {
          result[pos] <- ''
        }
      }
      result
    }

  serve <- function(input, output, session) {
    print('starting server')
    numOfColors <- 0
    numOfPicks <- 0
    output$mindState <- reactive({
      'preGame'
    })
    # values <- reactiveValues(mindState = 'preGame')
    availableColors <-
      c('pink', 'aqua', 'green', 'orange', 'red', 'purple')
    gameColors <- NULL
    code <- NULL
    localBoard <- NULL
    currentRowIndex <- 1

    # buttons
    observeEvent(input$startNewGame, {
      hideCode(output)
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
      currentRowIndex <- 1
      output$board <- outputBoard()
      gameColors <<- availableColors[1:numOfColors]
      print(paste("  gameColors", gameColors))

      updateCurrentGuess(input, output, numOfPicks)
      # set radio buttons based on number of colors
      setupRadio(session, radioId1, gameColors, numOfPicks > 0)
      setupRadio(session, radioId2, gameColors, numOfPicks > 1)
      setupRadio(session, radioId3, gameColors, numOfPicks > 2)
      setupRadio(session, radioId4, gameColors, numOfPicks > 3)

      # enable/disable based on number of picks
      # if_else(numOfPicks > 0, enable(radioId1), disable(radioId1))
      # if_else(numOfPicks > 1, enable(radioId2), disable(radioId2))
      # if_else(numOfPicks > 2, enable(radioId3), disable(radioId3))
      # if_else(numOfPicks > 3, enable(radioId4), disable(radioId4))

      code <<-
        sample(gameColors,
               numOfPicks,
               replace = length(gameColors) < numOfPicks)
      print(paste("  code=", code))
      output$mindState <- renderText('mindGame')
    })

    observeEvent(input$showResults, {
      # print(paste("localBoard.ncol=", ncol(localBoard)))
      localBoard[currentRowIndex,] <<-
        updateResults(
          code,
          numOfPicks,
          input$radiocell1,
          input$radiocell2,
          input$radiocell3,
          input$radiocell4
        )
      printLocalBoard()
      output$board <- outputBoard()
      currentRowIndex <<- currentRowIndex + 1
      updateRadio(session, radioId1, label = 'Cell 1', value = 'black')
      updateRadio(session, radioId2, label = 'Cell 2', value = 'black')
      updateRadio(session, radioId3, label = 'Cell 3', value = 'black')
      updateRadio(session, radioId4, label = 'Cell 4', value = 'black')
    })
    observeEvent(input$showCode, {
      showCode(output, code, numOfPicks)
    })
    printLocalBoard <- function() {
      # print(localBoard[currentRowIndex, ])
    }
    outputBoard <- function() {
      switch(numOfPicks,
             {
               val <- c('0', 'p')
               columnNames <- c('1', '')
             },
             {
               val <- c('0', '0', 'p', 'p')
               columnNames <- c('1', '2', '', '', '')
             },
             {
               val <- c('0', '0', '0', 'p', 'p', 'p')
               columnNames <- c('1', '2', '3', '', '', '', '')
             },
             {
               val <- c('0', '0', '0', '0', 'p', 'p', 'p', 'p')
               columnNames <-
                 c('1', '2', '3', '4', '', '', '', '', '')
             })
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
      xxx
    }
  }
  if (funct == 'serve'){
    serve
  } else if (funct == 'setupRadio') {
    setupRadio
  } else if (funct == 'hideCode') {
    hideCode
  } else if (funct == 'showCode') {
    showCode
  } else if (funct == 'updateCurrentGuess') {
    updateCurrentGuess
  } else if (funct == 'updateRadio') {
    # print(paste('funct=', funct))
    # print(showCode)
    updateRadio
    # showCode
  } else if (funct == 'updateResults') {
    updateResults
  } else {
    paste('unknown funct:', funct)
  }
}
