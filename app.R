library(shiny)

ui <- fluidPage(
  theme = bslib::bs_theme(version = 4),
  tags$head(
    tags$link(rel = "stylesheet", href = "custom.css")
  ),
  h1("Bingo", align = "center"),
  fluidRow(
    column(
      width = 6,
      class = "text-center",
      br(), br(), br(),
      shinyWidgets::actionBttn(
        inputId = "misturar",
        label = "Misturar as bolinhas",
        color = "success"
      ),
      br(), br(), br(),
      shinyWidgets::actionBttn(
        inputId = "sortear",
        label = "Sortear um número",
        color = "primary"
      )
    ),
    column(
      width = 6,
      class = "text-center",
      br(),
      reactable::reactableOutput("tabela")
    )
  )
)

server <- function(input, output, session) {

  tab_numeros <- tibble::tibble(
    B = 1:15,
    I = 16:30,
    N = 31:45,
    G = 46:60,
    O = 61:75
  )

  num_gaiola <- reactiveVal(as.numeric(unlist(tab_numeros)))

  observeEvent(input$misturar, {

    removeUI(selector = "#audio_gaiola")

    insertUI(
      selector = "#sortear",
      where = "afterEnd",
      ui = tags$audio(
        src = "bingo_cage.m4a",
        type = "audio/m4a",
        autoplay = TRUE,
        # controls = NA,
        id = "audio_gaiola",
        # style = "display: none;"
      )
    )

  })

  observeEvent(input$sortear, {

    removeUI(selector = "#audio_gaiola")

    if (length(num_gaiola()) == 0) {
      numero_sorteado <- "Não há mais números para sortear"
    } else if (length(num_gaiola()) == 1) {
      numero_sorteado <- num_gaiola()
      num_gaiola(numeric(0))
    } else {
      numero_sorteado <- sample(num_gaiola(), 1)

      nova_gaiola <- num_gaiola()[num_gaiola() != numero_sorteado]
      num_gaiola(nova_gaiola)
    }

    showModal(
      modalDialog(
        numero_sorteado
      )
    )

  })

  output$tabela <- reactable::renderReactable({
    tab_numeros |>
      dplyr::mutate(
        dplyr::across(
          .cols = dplyr::everything(),
          ~ ifelse(.x %in% num_gaiola(), "", .x)
        )
      ) |>
      reactable::reactable(
        pagination = FALSE,
        sortable = FALSE,
        fullWidth = FALSE
      )
  })

}

shinyApp(ui, server, options = list(launch.browser = FALSE, port = 4242))
