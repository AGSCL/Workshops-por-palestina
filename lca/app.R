library(shiny)
library(bslib)
library(ggplot2)
library(dplyr)

# Función para generar datos simulados
generate_data <- function(n = 100, pi = c(0.4, 0.6), 
                          rho1 = c(0.8, 0.2), rho2 = c(0.3, 0.7)) {
  z <- sample(1:2, n, replace = TRUE, prob = pi)
  x1 <- sapply(z, function(zi) sample(0:1, 1, prob = if(zi == 1) rho1 else c(1-rho1[1], rho1[1])))
  x2 <- sapply(z, function(zi) sample(0:1, 1, prob = if(zi == 1) rho2 else c(1-rho2[1], rho2[1])))
  return(list(x1 = x1, x2 = x2, true_class = z))
}

ui <- page_fluid(
  theme = bs_theme(bootswatch = "flatly"),
  
  h1("Algoritmo EM con Múltiples Puntos de Inicio", class = "text-center mb-4"),
  
  card(
    card_header("Datos"),
    card_body(
      HTML('
      <p>Este ejemplo simula datos de dos variables binarias (x₁ y x₂) con dos clases (C = 1 y C = 2).</p>
      <p>40% de los datos estarán en clase 1 (&pi;<sub>1</sub>) y el resto en clase 2 (&pi;<sub>2</sub>)</p>
      ')
    ),
    card_header("Tabla de Probabilidades de respuesta condicionales a la Clase"),
    card_body(
      HTML('
      <table class="table table-striped">
        <thead>
          <tr>
            <th>Clase (C)</th>
            <th>P(x₁ = 1)</th>
            <th>P(x₁ = 0)</th>
            <th>P(x₂ = 1)</th>
            <th>P(x₂ = 0)</th>
          </tr>
        </thead>
        <tbody>
          <tr>
            <td>C = 1</td>
            <td>0.8</td>
            <td>0.2</td>
            <td>0.3</td>
            <td>0.7</td>
          </tr>
          <tr>
            <td>C = 2</td>
            <td>0.2</td>
            <td>0.8</td>
            <td>0.7</td>
            <td>0.3</td>
          </tr>
        </tbody>
      </table>
      ')
    ),
    card_header("Simulación"),
    card_body(
      fluidRow(
        column(4,
               numericInput("n_samples", "Número de observaciones:", 100, min = 50, max = 500),
               numericInput("n_starts", "Número de puntos de inicio:", 3, min = 1, max = 5),
               numericInput("n_iterations", "Número de iteraciones:", 10, min = 1, max = 20),
               actionButton("generate", "Generar datos", class = "btn-primary"),
               actionButton("run_em", "Ejecutar EM", class = "btn-success")
        ),
        column(8,
               plotOutput("data_plot")
        )
      )
    )
  ),
  
  card(
    card_header("Puntos de inicio aleatorios"),
    card_body(
      verbatimTextOutput("starting_points")
    )
  ),
  
  card(
    card_header("Convergencia desde diferentes puntos de inicio"),
    card_body(
      plotOutput("convergence_plot"),
      hr(),
      h4("Mejor solución encontrada:"),
      verbatimTextOutput("best_solution")
    )
  )
)

server <- function(input, output, session) {
  rv <- reactiveValues(
    data = NULL,
    em_results = NULL,
    starting_points = NULL
  )
  
  # Función para un paso E
  e_step <- function(pi, rho1, rho2, x1, x2) {
    n <- length(x1)
    posterior <- matrix(0, n, 2)
    
    for(i in 1:n) {
      l1 <- (rho1[1]^x1[i] * (1-rho1[1])^(1-x1[i])) * 
        (rho2[1]^x2[i] * (1-rho2[1])^(1-x2[i])) * pi[1]
      l2 <- ((1-rho1[1])^x1[i] * rho1[1]^(1-x1[i])) * 
        ((1-rho2[1])^x2[i] * rho2[1]^(1-x2[i])) * pi[2]
      
      total <- l1 + l2
      posterior[i,] <- c(l1/total, l2/total)
    }
    return(posterior)
  }
  
  # Función para un paso M
  m_step <- function(posterior, x1, x2) {
    pi_new <- colMeans(posterior)
    
    rho1_new <- c(
      sum(posterior[,1] * x1) / sum(posterior[,1]),
      sum(posterior[,2] * (1-x1)) / sum(posterior[,2])
    )
    
    rho2_new <- c(
      sum(posterior[,1] * x2) / sum(posterior[,1]),
      sum(posterior[,2] * (1-x2)) / sum(posterior[,2])
    )
    
    return(list(pi = pi_new, rho1 = rho1_new, rho2 = rho2_new))
  }
  
  # Calcular log-verosimilitud
  calc_loglik <- function(pi, rho1, rho2, x1, x2) {
    n <- length(x1)
    loglik <- 0
    for(i in 1:n) {
      l1 <- (rho1[1]^x1[i] * (1-rho1[1])^(1-x1[i])) * 
        (rho2[1]^x2[i] * (1-rho2[1])^(1-x2[i])) * pi[1]
      l2 <- ((1-rho1[1])^x1[i] * rho1[1]^(1-x1[i])) * 
        ((1-rho2[1])^x2[i] * rho2[1]^(1-x2[i])) * pi[2]
      loglik <- loglik + log(l1 + l2)
    }
    return(loglik)
  }
  
  # Generar datos
  observeEvent(input$generate, {
    rv$data <- generate_data(input$n_samples)
    rv$em_results <- NULL
    rv$starting_points <- NULL
  })
  
  # Ejecutar EM
  observeEvent(input$run_em, {
    req(rv$data)
    
    starting_points <- list()
    all_results <- vector("list", input$n_starts)
    
    for(start in 1:input$n_starts) {
      # Generar punto de inicio aleatorio
      pi_start <- runif(1)
      pi_start <- c(pi_start, 1-pi_start)
      rho1_start <- runif(2)
      rho2_start <- runif(2)
      
      starting_points[[start]] <- list(
        pi = pi_start,
        rho1 = rho1_start,
        rho2 = rho2_start
      )
      
      # Variables para este inicio
      pi <- pi_start
      rho1 <- rho1_start
      rho2 <- rho2_start
      
      # Almacenar resultados de cada iteración
      iter_results <- vector("list", input$n_iterations)
      
      for(iter in 1:input$n_iterations) {
        posterior <- e_step(pi, rho1, rho2, rv$data$x1, rv$data$x2)
        params <- m_step(posterior, rv$data$x1, rv$data$x2)
        loglik <- calc_loglik(params$pi, params$rho1, params$rho2, 
                              rv$data$x1, rv$data$x2)
        
        iter_results[[iter]] <- list(
          pi = params$pi,
          rho1 = params$rho1,
          rho2 = params$rho2,
          loglik = loglik
        )
        
        pi <- params$pi
        rho1 <- params$rho1
        rho2 <- params$rho2
      }
      
      all_results[[start]] <- iter_results
    }
    
    rv$starting_points <- starting_points
    rv$em_results <- all_results
  })
  
  # Visualizar datos
  output$data_plot <- renderPlot({
    req(rv$data)
    df <- data.frame(
      x1 = factor(rv$data$x1),
      x2 = factor(rv$data$x2),
      true_class = factor(rv$data$true_class)
    )
    
    ggplot(df, aes(x = x1, y = x2, color = true_class)) +
      geom_jitter(width = 0.2, height = 0.2, size = 3, alpha = 0.6) +
      theme_minimal() +
      labs(title = "Datos Simulados",
           x = "Variable 1",
           y = "Variable 2",
           color = "Clase Verdadera")
  })
  
  # Mostrar puntos de inicio
  output$starting_points <- renderPrint({
    req(rv$starting_points)
    for(i in seq_along(rv$starting_points)) {
      cat("Inicio", i, ":\n")
      cat("pi =", round(rv$starting_points[[i]]$pi, 3), "\n")
      cat("rho1 =", round(rv$starting_points[[i]]$rho1, 3), "\n")
      cat("rho2 =", round(rv$starting_points[[i]]$rho2, 3), "\n\n")
    }
  })
  
  # Visualizar convergencia
  output$convergence_plot <- renderPlot({
    req(rv$em_results)
    
    # Crear dataframe para la visualización
    plot_data <- data.frame()
    
    for(start in seq_along(rv$em_results)) {
      for(iter in seq_along(rv$em_results[[start]])) {
        result <- rv$em_results[[start]][[iter]]
        plot_data <- rbind(plot_data, data.frame(
          iteration = iter,
          start = factor(start),
          loglik = result$loglik
        ))
      }
    }
    
    ggplot(plot_data, aes(x = iteration, y = loglik, color = start, group = start)) +
      geom_line(size = 1) +
      geom_point(size = 3) +
      theme_minimal() +
      labs(title = "Convergencia de la Log-verosimilitud",
           x = "Iteración",
           y = "Log-verosimilitud",
           color = "Punto de inicio")
  })
  
  # Mostrar mejor solución
  output$best_solution <- renderPrint({
    req(rv$em_results)
    
    # Encontrar la mejor solución
    final_logliks <- sapply(rv$em_results, function(x) x[[length(x)]]$loglik)
    best_start <- which.max(final_logliks)
    best_result <- rv$em_results[[best_start]][[length(rv$em_results[[best_start]])]]
    
    cat("Mejor solución (punto de inicio", best_start, "):\n")
    cat("Log-verosimilitud:", round(best_result$loglik, 3), "\n")
    cat("pi =", round(best_result$pi, 3), "\n")
    cat("rho1 =", round(best_result$rho1, 3), "\n")
    cat("rho2 =", round(best_result$rho2, 3), "\n")
  })
}

shinyApp(ui, server)