library(pracma)
library(shiny)

getDrs <- function(list, value, n) {
  for(i in 1:n) {
    if (grepl(list[i], value, fixed = TRUE)) {
      return(i)
    }
  }
}

## Get Covid data CSV
file <- read.csv("dados_covid_sp.csv", sep=";")
hospital <- read.csv("plano_sp_leitos_internacoes.csv", sep=";")
cities <- unique(file$nome_munic)
drs <- unique(file$nome_drs)
n <- length(cities)
nn <- length(drs)

# Create Variables
cdata <- list(data.frame("data"=as.Date("1970-01-01") ,"casos"=0L, "casos_novos"=0L ,"obitos"=0L, "obitos_novos"=0L))
for (i in 2:n) {
  cdata[[i]] <- data.frame("data"=as.Date("1970-01-01") ,"casos"=0L, "casos_novos"=0L ,"obitos"=0L, "obitos_novos"=0L)
}
pop <- integer(n)
area <- numeric(n)
nome_drs <- rep("", n)
first <- rep(TRUE, n)

print("0 %")
fs <- 1
for (i in 1:nrow(file)) {
  # File lookup status
  file_status = i / nrow(file)*100
  if (file_status >= 10 * fs ){
    print(paste(as.integer(file_status), "%"))
    fs <- fs + 1
  }
  
  j <- match(file[i, "nome_munic"], cities)
  d = as.Date(file[i, "datahora"], "%Y-%m-%d")
  if (first[j]) {
    cdata[[j]][1, "data"] <- d 
    cdata[[j]][1, "casos"] <- file[i, "casos"]
    cdata[[j]][1, "casos_novos"] <- file[i, "casos_novos"]
    cdata[[j]][1, "obitos"] <- file[i, "obitos"]
    cdata[[j]][1, "obitos_novos"] <- file[i, "obitos_novos"] 
    pop[j] <- file[i, "pop"]
    area[j] <- file[i, "area"]
    nome_drs[j] <- file[i, "nome_drs"]
    first[j]  <- FALSE
  }
  else {
    dt = list(d, file[i, "casos"], file[i, "casos_novos"], file[i, "obitos"], file[i, "obitos_novos"])
    cdata[[j]] <- rbind(cdata[[j]], dt)
  }
}
caseavg <- list(movavg(cdata[[1]]$casos_novos, 7, "s"))
deathavg <- list(movavg(cdata[[1]]$obitos_novos, 7, "s"))
for (j in 2:n) {
  caseavg[[j]] <- movavg(cdata[[j]]$casos_novos, 7, "s")
  deathavg[[j]] <- movavg(cdata[[j]]$obitos_novos, 7, "s")
}

hdata <- list(data.frame("data"=as.Date("1970-01-01") , "pacientes_uti_mm7d"=0, "total_covid_uti_mm7d"=0, "media_ocupacao"=0, "internacoes_7d"=0L))
for (i in 2:nn) {
  hdata[[i]] <- data.frame("data"=as.Date("1970-01-01") , "pacientes_uti_mm7d"=0, "total_covid_uti_mm7d"=0, "media_ocupacao"=0, "internacoes_7d"=0L)
}
first <- rep(TRUE, nn)

print("0 %")
fs <- 1
for (i in 1:nrow(hospital)) {
  # File lookup status
  file_status = i / nrow(hospital)*100
  if (file_status >= 10 * fs ){
    print(paste(as.integer(file_status), "%"))
    fs <- fs + 1
  }
  
  if (identical("Estado de São Paulo", hospital[i, "nome_drs"])) {}
  else if (grepl("Grande SP", hospital[i, "nome_drs"], fixed = TRUE)) {}
  else if (identical("Município de São Paulo", hospital[i, "nome_drs"])) {
    j <- getDrs(drs, "Grande São Paulo", nn)
    
    d = as.Date(hospital[i, "datahora"], "%Y-%m-%d")
    if (first[[j]]) {
      puti <- as.numeric(sub(",", ".", hospital[i, "pacientes_uti_mm7d"], fixed = TRUE))
      tuti <- as.numeric(sub(",", ".", hospital[i, "total_covid_uti_mm7d"], fixed = TRUE))
      tint <- as.numeric(sub(",", ".", hospital[i, "internacoes_7d"], fixed = TRUE))
      for (k in 1:5) {
        puti <- puti + as.numeric(sub(",", ".", hospital[i + k, "pacientes_uti_mm7d"], fixed = TRUE))
        tuti <- tuti + as.numeric(sub(",", ".", hospital[i + k, "total_covid_uti_mm7d"], fixed = TRUE))
        tint <- tint + as.numeric(sub(",", ".", hospital[i + k, "internacoes_7d"], fixed = TRUE))
      }
      
      mocup <- puti * 100 / tuti
      
      hdata[[j]][1, "data"] <- d
      hdata[[j]][1, "pacientes_uti_mm7d"] <- puti
      hdata[[j]][1, "total_covid_uti_mm7d"] <- tuti
      hdata[[j]][1, "media_ocupacao"] <- mocup
      hdata[[j]][1, "internacoes_7d"] <- tint
      first[[j]] <- FALSE
    }
    else {
      puti <- as.numeric(sub(",", ".", hospital[i, "pacientes_uti_mm7d"], fixed = TRUE))
      tuti <- as.numeric(sub(",", ".", hospital[i, "total_covid_uti_mm7d"], fixed = TRUE))
      tint <- as.numeric(sub(",", ".", hospital[i, "internacoes_7d"], fixed = TRUE))
      for (k in 1:5) {
        puti <- puti + as.numeric(sub(",", ".", hospital[i + k, "pacientes_uti_mm7d"], fixed = TRUE))
        tuti <- tuti + as.numeric(sub(",", ".", hospital[i + k, "total_covid_uti_mm7d"], fixed = TRUE))
        tint <- tint + as.numeric(sub(",", ".", hospital[i + k, "internacoes_7d"], fixed = TRUE))
      }
      
      mocup <- puti * 100 / tuti
      
      dt = list(d , puti, tuti, mocup, tint)
      hdata[[j]] <- rbind(hdata[[j]], dt)
    }
  }
  else {
    j <- getDrs(drs, hospital[i, "nome_drs"], nn)
    
    d = as.Date(hospital[i, "datahora"], "%Y-%m-%d")
    if (first[[j]]) {
      mocup <- as.numeric(sub(",", ".", hospital[i, "pacientes_uti_mm7d"], fixed = TRUE)) * 100 / as.numeric(sub(",", ".", hospital[i, "total_covid_uti_mm7d"], fixed = TRUE))
      
      hdata[[j]][1, "data"] <- d
      hdata[[j]][1, "pacientes_uti_mm7d"] <- as.numeric(sub(",", ".", hospital[i, "pacientes_uti_mm7d"], fixed = TRUE))
      hdata[[j]][1, "total_covid_uti_mm7d"] <- as.numeric(sub(",", ".", hospital[i, "total_covid_uti_mm7d"], fixed = TRUE))
      hdata[[j]][1, "media_ocupacao"] <- mocup
      hdata[[j]][1, "internacoes_7d"] <- as.numeric(sub(",", ".", hospital[i, "internacoes_7d"], fixed = TRUE))
      first[[j]] <- FALSE
    }
    else {
      mocup <- as.numeric(sub(",", ".", hospital[i, "pacientes_uti_mm7d"], fixed = TRUE)) * 100 / as.numeric(sub(",", ".", hospital[i, "total_covid_uti_mm7d"], fixed = TRUE))
      
      dt = list(d , as.numeric(sub(",", ".", hospital[i, "pacientes_uti_mm7d"], fixed = TRUE)), as.numeric(sub(",", ".", hospital[i, "total_covid_uti_mm7d"], fixed = TRUE)), mocup, as.numeric(sub(",", ".", hospital[i, "internacoes_7d"], fixed = TRUE)))
      hdata[[j]] <- rbind(hdata[[j]], dt)
    }
  }
}

idata <- list(data.frame("data"=as.Date("1970-01-01"), "internacoes_7d"=0L))
for (i in 2:nn) {
  idata[[i]] <- data.frame("data"=as.Date("1970-01-01"), "internacoes_7d"=0L)
}
first <- rep(TRUE, nn)

for (j in 1:nn) {
  hdays <- nrow(hdata[[j]])
  
  i <- (hdays - ((hdays %/% 7) * 7)) + 7
  while (i <= hdays) {
    if (first[[j]]) {
      idata[[j]][1, "data"] <- hdata[[j]][i, "data"]
      idata[[j]][1, "internacoes_7d"] <- hdata[[j]][i, "internacoes_7d"]
      first[[j]] <- FALSE
    }
    else {
      dt = list(hdata[[j]][i, "data"], hdata[[j]][i, "internacoes_7d"])
      idata[[j]] <- rbind(idata[[j]], dt)
    }
    i <- i + 7
  }
}

ui <- fluidPage(
  titlePanel("Covid-19 - Estado de São Paulo"),
  selectInput("city", "Selecione a cidade", choices = cities, selected = "São Paulo"),
  
  textOutput("title", container = h2),
  textOutput("general"),
  textOutput("drsname"),
  textOutput("cases"),
  textOutput("death"),
  textOutput("rate"),
  textOutput("cont"),
  textOutput("lost"),
  textOutput("casedense"),
  textOutput("deathdense"),
  
  plotOutput("p_cases"),
  textOutput("t_cases"),
  textOutput("z_cases"),
  textOutput("d_cases"),
  
  plotOutput("p_death"),
  textOutput("t_death"),
  textOutput("z_death"),
  textOutput("d_death"),
  
  plotOutput("p_uti"),
  textOutput("t_ocup"),

  plotOutput("p_int"),
  
  hr(),
  p("2020 - Rodolfo Vick - rodolfo.vick at gmail.com"),
  br()
)

server <- function(input, output, session) {
  observe({
    j <- match(input$city, cities)
    days <- nrow(cdata[[j]])
    
    # Data
    output$title <- renderText({
      paste("Dados de ", input$city, " em ", as.character(cdata[[j]][days, "data"], "%d/%m/%Y"))
    })
    output$general <- renderText({
      paste("População:", pop[j], "hab. Área: ", area[j], "Km2. Densidade:", format(round(pop[j] / area[j], 2), nsmall = 2), "hab/Km2.")
    })
    output$drsname <- renderText({
      paste("Região:", nome_drs[j], ".")
    })
    output$cases <- renderText({
      paste("Total de casos:", cdata[[j]][days, "casos"], ". No último dia:", cdata[[j]][days, "casos_novos"], ".")
    })
    output$death <- renderText({
      paste("Total de óbitos:", cdata[[j]][days, "obitos"], ". No último dia:", cdata[[j]][days, "obitos_novos"], ".")
    })
    output$rate <- renderText({
      paste("Taxa de mortalidade:", format(round(cdata[[j]][days, "obitos"] * 100 / cdata[[j]][days, "casos"], 2), nsmall = 2), "%.")
    })
    
    output$cont <- renderText({
      paste("Percentual de contaminação da população:", format(round(cdata[[j]][days, "casos"] * 100 / pop[j], 2), nsmall = 2), "%.")
    })

    output$lost <- renderText({
      paste("Perda de população:", format(round(cdata[[j]][days, "obitos"] * 100 / pop[j], 2), nsmall = 2), "%.")
    })

    output$casedense <- renderText({
      paste("Densidade de casos:", format(round(cdata[[j]][days, "casos"] / area[j], 5), nsmall = 5), "casos/Km2.")
    })

    output$deathdense <- renderText({
      paste("Densidade de óbitos:", format(round(cdata[[j]][days, "obitos"] / area[j], 5), nsmall = 5), "óbitos/Km2.")
    })
    
    # Cases
    casemax <- which.max(cdata[[j]]$casos_novos)
    diffcase <- caseavg[[j]][days] - caseavg[[j]][days - 7]
    output$p_cases <- renderPlot({
      plot(cdata[[j]]$data, cdata[[j]]$casos_novos, type = "h", main = "Novos casos por dia", xlab = NA, ylab = NA, col="blue")
      lines(cdata[[j]]$data, caseavg[[j]], col="red")
    })
    output$t_cases <- renderText({
      paste("O maior número de novos casos novos foi de ", cdata[[j]][casemax, "casos_novos"], "em", as.character(cdata[[j]][casemax, "data"], "%d/%m/%Y"), ".")
    })

    if (cdata[[j]][days, "casos_novos"] == 0) {
      zc <- 1
      i <- days -1;
      while ((cdata[[j]][i, "casos_novos"] == 0)  && i > 0) {
        i <- i-1
        zc <- zc+1
      }
      output$z_cases <- renderText({
        paste("Não houveram novos casos nos últimos ", zc, " dias.")
      })
    }
    else {
      output$z_cases <- renderText({""})
    }

    if (diffcase > 0) {
      tc <- format(round(((caseavg[[j]][days]/caseavg[[j]][days - 7])-1)*100, 2), 2)
      output$d_cases <- renderText({
        paste("A média móvel cresceu de ", format(round(caseavg[[j]][days - 7], 2), 2), " para ", format(round(caseavg[[j]][days], 2), 2), " nos últimos 7 dias, ", tc, "%.")
      })
    }
    else if (diffcase < 0) {
      tc <- format(round((1-(caseavg[[j]][days]/caseavg[[j]][days - 7]))*100, 2), 2)
      output$d_cases <- renderText({
        paste("A média móvel diminuiu de ", format(round(caseavg[[j]][days - 7], 2), 2), " para ", format(round(caseavg[[j]][days], 2), 2), " nos últimos 7 dias, ", tc, "%.")
      })
    }
    else {
      output$d_cases <- renderText({
        paste("A média móvel é estável na última semana.")
        })
    }

    # Death
    deathmax <- which.max(cdata[[j]]$obitos_novos)
    diffdeath <- deathavg[[j]][days] - deathavg[[j]][days - 7]
    output$p_death <- renderPlot({
      plot(cdata[[j]]$data, cdata[[j]]$obitos_novos, type = "h", main = "Novos óbitos por dia", xlab = NA, ylab = NA, col="blue")
      lines(cdata[[j]]$data, deathavg[[j]], col="red")
    })
    output$t_death <- renderText({
      paste("O maior número de novos óbitos foi de ", cdata[[j]][deathmax, "obitos_novos"], "em", as.character(cdata[[j]][deathmax, "data"], "%d/%m/%Y"), ".")
    })

    if (cdata[[j]][days, "obitos_novos"] == 0) {
      zd <- 1
      i <- days -1;
      while ((cdata[[j]][i, "obitos_novos"] == 0) && i > 0) {
        i <- i-1
        zd <- zd+1
      }
      output$z_death <- renderText({
        paste("Não houveram novos óbitos nos últimos ", zd, " dias.")
      })
    }
    else {
      output$z_death <- renderText({""})
    }

    if (diffdeath > 0) {
      td <- format(round(((deathavg[[j]][days]/deathavg[[j]][days - 7])-1)*100, 2), 2)
      output$d_death <- renderText({
        paste("A média móvel cresceu de ", format(round(deathavg[[j]][days - 7], 2), 2), " para ", format(round(deathavg[[j]][days], 2), 2), " nos últimos 7 dias, ", td, "%.")
      })
    }
    else if (diffdeath < 0) {
      td <- format(round((1-(deathavg[[j]][days]/deathavg[[j]][days - 7]))*100, 2), 2)
      output$d_death <- renderText({
        paste("A média móvel diminuiu de ", format(round(deathavg[[j]][days - 7], 2), 2), " para ", format(round(deathavg[[j]][days], 2), 2), " nos últimos 7 dias, ", td, "%.")
      })
    }
    else {
      output$d_death <- renderText({
        paste("A média móvel é estável na última semana.")
      })
    }
    
    # Hospital data
    k <- match(nome_drs[j], drs)
    hdays <- nrow(hdata[[k]])

    hmax <- max(c(max(hdata[[k]]$total_covid_uti_mm7d), max(hdata[[k]]$pacientes_uti_mm7d)))
    hmin <- min(c(min(hdata[[k]]$total_covid_uti_mm7d), min(hdata[[k]]$pacientes_uti_mm7d)))
    ocupmax <- which.max(hdata[[k]]$media_ocupacao)

    output$p_uti <- renderPlot({
      plot(hdata[[k]]$data, hdata[[k]]$total_covid_uti_mm7d, type = "h", main = paste("Média móvel para 7 dias de ocupação de UTI\nRegião:", nome_drs[j]), xlab = NA, ylab = NA, col="blue", ylim = c(hmin, hmax))
      par(new=TRUE)
      plot(hdata[[k]]$data, hdata[[k]]$pacientes_uti_mm7d, type = "h", xlab = NA, ylab = NA, col="red", ylim = c(hmin, hmax))
      legend("topright", legend=c("Oferta", "Ocupação"), pch=15, col=c("blue", "red"))
    })

    output$t_ocup <- renderText({
      paste("A maior taxa média de ocupação de UTI foi de ", format(round(hdata[[k]][ocupmax, "media_ocupacao"], 2), 2), "% em", as.character(hdata[[k]][ocupmax, "data"], "%d/%m/%Y"), "na região", nome_drs[j], ".")
    })

    # New hospitalization
    output$p_int <- renderPlot({
      plot(idata[[k]]$data, idata[[k]]$internacoes_7d, type = "h", main = paste("Acumulado de novas internações em 7 dias\nRegião:", nome_drs[j]), xlab = NA, ylab = NA, col="blue")
    })
  })
}

app <- shinyApp(ui, server)
runApp(app, port = 3071, host = "0.0.0.0")