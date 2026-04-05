library(shiny)

# ── Shapley engine (sourced from shapley_values.R logic) ─────────────────────

make_value_fn <- function(values, names) {
  function(indices) {
    if (length(indices) == 0) return(0)
    key <- coalition_key(indices, names)
    val <- values[[key]]
    if (is.null(val)) stop(sprintf("No value defined for coalition: '%s'", key))
    val
  }
}

shapley_values <- function(n, value_fn) {
  players <- seq_len(n)
  phi <- numeric(n)
  for (i in players) {
    others   <- players[players != i]
    n_others <- length(others)
    for (mask in 0:(2^n_others - 1)) {
      S      <- others[as.logical(intToBits(mask)[seq_len(n_others)])]
      s_size <- length(S)
      weight <- factorial(s_size) * factorial(n - s_size - 1) / factorial(n)
      phi[i] <- phi[i] + weight * (value_fn(c(S, i)) - value_fn(S))
    }
  }
  phi
}

# Generate all non-empty subsets (as index vectors) for n players
all_subsets <- function(n) {
  players <- seq_len(n)
  result  <- list()
  for (mask in 1:(2^n - 1)) {
    result[[length(result) + 1]] <- players[as.logical(intToBits(mask)[seq_len(n)])]
  }
  result
}

coalition_key <- function(indices, names) paste(sort(names[indices]), collapse = " + ")

# ── UI ────────────────────────────────────────────────────────────────────────

ui <- fluidPage(
  tags$head(tags$style(HTML("
    body { font-family: 'Helvetica Neue', Arial, sans-serif; background: #f5f7fa; }
    .well { background: #fff; border: 1px solid #dde3ec; border-radius: 8px; box-shadow: none; }
    h2 { color: #2c3e50; font-weight: 600; margin-bottom: 4px; }
    .subtitle { color: #7f8c8d; margin-bottom: 24px; font-size: 14px; }
    .section-title { font-weight: 600; color: #34495e; margin: 16px 0 8px; font-size: 13px;
                     text-transform: uppercase; letter-spacing: 0.05em; }
    .coalition-group { background: #f8f9fb; border-radius: 6px; padding: 10px 14px;
                       margin-bottom: 10px; border: 1px solid #e8ecf0; }
    .result-box { background: #fff; border-radius: 8px; border: 1px solid #dde3ec;
                  padding: 20px; }
    .result-row { display: flex; justify-content: space-between; align-items: center;
                  padding: 10px 0; border-bottom: 1px solid #f0f2f5; }
    .result-row:last-child { border-bottom: none; }
    .player-label { font-weight: 500; color: #2c3e50; }
    .phi-value { font-size: 18px; font-weight: 700; color: #2980b9; }
    .sum-row { margin-top: 12px; padding-top: 12px; border-top: 2px solid #dde3ec;
               display: flex; justify-content: space-between; }
    .sum-label { font-weight: 600; color: #2c3e50; }
    .sum-value { font-size: 18px; font-weight: 700; color: #27ae60; }
    .bar-wrap { background: #eaf3fb; border-radius: 4px; height: 8px; width: 140px;
                overflow: hidden; display: inline-block; vertical-align: middle; margin-left: 12px; }
    .bar-fill { height: 100%; background: #2980b9; border-radius: 4px; }
    .error-msg { color: #e74c3c; font-size: 13px; margin-top: 8px; }
    hr { border-color: #eee; }
  "))),

  titlePanel(
    div(h2("Shapley Values Calculator"),
        div("Cooperative game theory — fair value attribution", class = "subtitle"))
  ),

  sidebarLayout(
    sidebarPanel(
      width = 5,

      # Number of players
      fluidRow(
        column(6,
          numericInput("n_players", "Number of players", value = 3, min = 2, max = 4, step = 1)
        )
      ),

      hr(),

      # Player name inputs
      div(class = "section-title", "Player names"),
      uiOutput("player_name_inputs"),

      hr(),

      # Coalition value inputs (dynamically generated)
      div(class = "section-title", "Coalition values"),
      uiOutput("coalition_inputs"),

      br(),
      actionButton("calculate", "Calculate Shapley Values",
                   class = "btn btn-primary btn-block",
                   style = "width:100%; background:#2980b9; border-color:#2471a3; font-weight:600;")
    ),

    mainPanel(
      width = 7,
      br(),
      uiOutput("results")
    )
  )
)

# ── Server ────────────────────────────────────────────────────────────────────

server <- function(input, output, session) {

  n <- reactive({
    n <- as.integer(input$n_players)
    if (is.na(n) || n < 2) return(2L)
    if (n > 4) return(4L)
    n
  })

  player_names <- reactive({
    sapply(seq_len(n()), function(i) {
      val <- input[[paste0("player_", i)]]
      if (is.null(val) || trimws(val) == "") paste0("Player ", i) else trimws(val)
    })
  })

  # Render player name text boxes
  output$player_name_inputs <- renderUI({
    defaults <- c("Alice", "Bob", "Carol", "Dave")
    lapply(seq_len(n()), function(i) {
      textInput(paste0("player_", i),
                label = paste("Player", i),
                value = isolate(input[[paste0("player_", i)]]) %||% defaults[i])
    })
  })

  # Render coalition value inputs grouped by coalition size
  output$coalition_inputs <- renderUI({
    nm  <- player_names()
    subs <- all_subsets(n())

    # Group by size
    by_size <- split(subs, sapply(subs, length))
    size_labels <- c("1" = "Singletons", "2" = "Pairs", "3" = "Triples", "4" = "Grand coalition")

    ui_blocks <- lapply(names(by_size), function(sz) {
      group <- by_size[[sz]]
      inputs <- lapply(group, function(idx) {
        key   <- coalition_key(idx, nm)
        input_id <- paste0("coal_", paste(sort(idx), collapse = "_"))
        prev  <- isolate(input[[input_id]])
        fluidRow(
          column(7, tags$label(key, style = "font-size:13px; color:#555;")),
          column(5, numericInput(input_id, label = NULL,
                                 value = if (!is.null(prev)) prev else 0,
                                 min = 0, step = 0.1))
        )
      })
      tagList(
        div(class = "section-title", style = "font-size:11px; margin-top:10px;",
            size_labels[sz]),
        div(class = "coalition-group", inputs)
      )
    })

    tagList(ui_blocks)
  })

  # Calculate on button press
  results <- eventReactive(input$calculate, {
    nm   <- player_names()
    subs <- all_subsets(n())

    # Read all coalition values from inputs
    values <- setNames(lapply(subs, function(idx) {
      input_id <- paste0("coal_", paste(sort(idx), collapse = "_"))
      val <- input[[input_id]]
      if (is.null(val) || is.na(val)) 0 else val
    }), sapply(subs, function(idx) coalition_key(idx, nm)))

    tryCatch({
      v   <- make_value_fn(values, nm)
      phi <- shapley_values(n(), v)
      list(phi = phi, names = nm, grand = v(seq_len(n())), error = NULL)
    }, error = function(e) list(error = conditionMessage(e)))
  })

  output$results <- renderUI({
    res <- results()
    if (is.null(res)) {
      return(div(style = "color:#aaa; margin-top:40px; text-align:center;",
                 "Set coalition values and press Calculate."))
    }
    if (!is.null(res$error)) {
      return(div(class = "error-msg", icon("exclamation-triangle"), " ", res$error))
    }

    phi   <- res$phi
    nm    <- res$names
    grand <- res$grand
    max_phi <- max(abs(phi), 1e-9)

    rows <- lapply(seq_along(phi), function(i) {
      pct <- max(0, phi[i] / max_phi * 100)
      div(class = "result-row",
          div(span(nm[i], class = "player-label"),
              div(class = "bar-wrap",
                  div(class = "bar-fill", style = sprintf("width:%.1f%%", pct)))),
          span(sprintf("%.4f", phi[i]), class = "phi-value")
      )
    })

    div(class = "result-box",
        h4("Shapley Values", style = "margin-top:0; color:#2c3e50;"),
        rows,
        div(class = "sum-row",
            span("Sum", class = "sum-label"),
            span(sprintf("%.4f  (grand coalition: %.4f)", sum(phi), grand),
                 class = "sum-value"))
    )
  })
}

# ── NULL coalescing operator ──────────────────────────────────────────────────
`%||%` <- function(a, b) if (!is.null(a)) a else b

shinyApp(ui, server)
