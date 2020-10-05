library(pracma)
library(shiny)

## Get Covid data CSV
#download.file("https://raw.githubusercontent.com/seade-R/dados-covid-sp/master/data/dados_covid_sp.csv", "dados_covid_sp.csv")
file <- read.csv("dados_covid_sp.csv", sep=";")
cities <- unique(file$nome_munic)

#download.file("https://raw.githubusercontent.com/seade-R/dados-covid-sp/master/data/plano_sp_leitos_internacoes.csv", "plano_sp_leitos_internacoes.csv")
hospital <- read.csv("plano_sp_leitos_internacoes.csv", sep=";")

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
  # Create city data
  observe({
    first <- TRUE
    for (i in 1:nrow(file)) {
      if (identical(file[i, "nome_munic"], input$city)){
        d = as.Date(paste("2020-", toString(file[i, "mes"]), "-", toString(file[i, "dia"]), sep=""))
        if (first) {
          cdata <- data.frame("data"=d ,"casos"=file[i, "casos"], "casos_novos"=file[i, "casos_novos"] ,"obitos"=file[i, "obitos"], "obitos_novos"=file[i, "obitos_novos"])
          pop <- file[i, "pop"]
          area <- file[i, "area"]
          nome_drs <- file[i, "nome_drs"]
          first  <- FALSE
        }
        else {
          dt = list(d, file[i, "casos"], file[i, "casos_novos"], file[i, "obitos"], file[i, "obitos_novos"])
          cdata <- rbind(cdata, dt)
        }
      }
    }
    days <- nrow(cdata)
    
    # Data
    output$title <- renderText({
      paste("Dados de ", input$city, " em ", as.character(cdata[days, "data"], "%d/%m/%Y"))
    })
    output$general <- renderText({
      paste("População:", pop, "hab. Área: ", area, "Km2. Densidade:", format(round(pop / area, 2), nsmall = 2), "hab/Km2.")
    })
    output$drsname <- renderText({
      paste("Região:", nome_drs, ".")
    })
    output$cases <- renderText({
      paste("Total de casos:", cdata[days, "casos"], ". No último dia:", cdata[days, "casos_novos"], ".")
    })
    output$death <- renderText({
      paste("Total de óbitos:", cdata[days, "obitos"], ". No último dia:", cdata[days, "obitos_novos"], ".")
    })
    output$rate <- renderText({
      paste("Taxa de mortalidade:", format(round(cdata[days, "obitos"] * 100 / cdata[days, "casos"], 2), nsmall = 2), "%.")
    })
    
    output$cont <- renderText({
      paste("Percentual de contaminação da população:", format(round(cdata[days, "casos"] * 100 / pop, 2), nsmall = 2), "%.")
    })

    output$lost <- renderText({
      paste("Perda de população:", format(round(cdata[days, "obitos"] * 100 / pop, 2), nsmall = 2), "%.")
    })

    output$casedense <- renderText({
      paste("Densidade de casos:", format(round(cdata[days, "casos"] / area, 5), nsmall = 5), "casos/Km2.")
    })

    output$deathdense <- renderText({
      paste("Densidade de óbitos:", format(round(cdata[days, "obitos"] / area, 5), nsmall = 5), "óbitos/Km2.")
    })
    
    # Cases
    caseavg <- movavg(cdata$casos_novos, 7, "s")
    casemax <- which.max(cdata$casos_novos)
    diffcase <- caseavg[days] - caseavg[days - 7]
    output$p_cases <- renderPlot({
      plot(cdata$data, cdata$casos_novos, type = "h", main = "Novos casos por dia", xlab = NA, ylab = NA, col="blue")
      lines(cdata$data, caseavg, col="red")
    })
    output$t_cases <- renderText({
      paste("O maior número de novos casos novos foi de ", cdata[casemax, "casos_novos"], "em", as.character(cdata[casemax, "data"], "%d/%m/%Y"), ".")
    })
    
    if (cdata[days, "casos_novos"] == 0) {
      zc <- 1
      i <- days -1;
      while ((cdata[i, "casos_novos"] == 0)  && i > 0) {
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
      tc <- format(round(((caseavg[days]/caseavg[days - 7])-1)*100, 2), 2)
      output$d_cases <- renderText({
        paste("A média móvel cresceu de ", format(round(caseavg[days - 7], 2), 2), " para ", format(round(caseavg[days], 2), 2), " nos últimos 7 dias, ", tc, "%.")
      })
    }
    else if (diffcase < 0) {
      tc <- format(round((1-(caseavg[days]/caseavg[days - 7]))*100, 2), 2)
      output$d_cases <- renderText({
        paste("A média móvel diminuiu de ", format(round(caseavg[days - 7], 2), 2), " para ", format(round(caseavg[days], 2), 2), " nos últimos 7 dias, ", tc, "%.")
      })
    }
    else {
      output$d_cases <- renderText({
        paste("A média móvel é estável na última semana.")
        })
    }
    
    # Death
    deathavg <- movavg(cdata$obitos_novos, 7, "s")
    deathmax <- which.max(cdata$obitos_novos)
    diffdeath <- deathavg[days] - deathavg[days - 7]
    output$p_death <- renderPlot({
      plot(cdata$data, cdata$obitos_novos, type = "h", main = "Novos óbitos por dia", xlab = NA, ylab = NA, col="blue")
      lines(cdata$data, deathavg, col="red")
    })
    output$t_death <- renderText({
      paste("O maior número de novos óbitos foi de ", cdata[deathmax, "obitos_novos"], "em", as.character(cdata[deathmax, "data"], "%d/%m/%Y"), ".")
    })
    
    if (cdata[days, "obitos_novos"] == 0) {
      zd <- 1
      i <- days -1;
      while ((cdata[i, "obitos_novos"] == 0) && i > 0) {
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
      td <- format(round(((deathavg[days]/deathavg[days - 7])-1)*100, 2), 2)
      output$d_death <- renderText({
        paste("A média móvel cresceu de ", format(round(deathavg[days - 7], 2), 2), " para ", format(round(deathavg[days], 2), 2), " nos últimos 7 dias, ", td, "%.")
      })
    }
    else if (diffdeath < 0) {
      td <- format(round((1-(deathavg[days]/deathavg[days - 7]))*100, 2), 2)
      output$d_death <- renderText({
        paste("A média móvel diminuiu de ", format(round(deathavg[days - 7], 2), 2), " para ", format(round(deathavg[days], 2), 2), " nos últimos 7 dias, ", td, "%.")
      })
    }
    else {
      output$d_death <- renderText({
        paste("A média móvel é estável na última semana.")
      })
    }
    
    # Hospital data
    first <- TRUE
    for (i in 1:nrow(hospital)) {
      if (identical("Grande São Paulo", nome_drs)) {
        if (grepl("Município", hospital[i, "nome_drs"], fixed = TRUE)) {
          d = as.Date(hospital[i, "datahora"], "%Y-%m-%d")
          if (first) {
            puti <- as.numeric(sub(",", ".", hospital[i, "pacientes_uti_mm7d"], fixed = TRUE))
            tuti <- as.numeric(sub(",", ".", hospital[i, "total_covid_uti_mm7d"], fixed = TRUE))
            tint <- as.numeric(sub(",", ".", hospital[i, "internacoes_7d"], fixed = TRUE))
            for (j in 1:5) {
              puti <- puti + as.numeric(sub(",", ".", hospital[i + j, "pacientes_uti_mm7d"], fixed = TRUE))
              tuti <- tuti + as.numeric(sub(",", ".", hospital[i + j, "total_covid_uti_mm7d"], fixed = TRUE))
              tint <- tint + as.numeric(sub(",", ".", hospital[i + j, "internacoes_7d"], fixed = TRUE))
            }
            
            mocup <- puti * 100 / tuti
            
            hdata <- data.frame("data"=d , "pacientes_uti_mm7d"=puti, "total_covid_uti_mm7d"=tuti, "media_ocupacao"=mocup, "internacoes_7d"=tint)
            first <- FALSE
          }
          else {
            puti <- as.numeric(sub(",", ".", hospital[i, "pacientes_uti_mm7d"], fixed = TRUE))
            tuti <- as.numeric(sub(",", ".", hospital[i, "total_covid_uti_mm7d"], fixed = TRUE))
            tint <- as.numeric(sub(",", ".", hospital[i, "internacoes_7d"], fixed = TRUE))
            for (j in 1:5) {
              puti <- puti + as.numeric(sub(",", ".", hospital[i + j, "pacientes_uti_mm7d"], fixed = TRUE))
              tuti <- tuti + as.numeric(sub(",", ".", hospital[i + j, "total_covid_uti_mm7d"], fixed = TRUE))
              tint <- tint + as.numeric(sub(",", ".", hospital[i + j, "internacoes_7d"], fixed = TRUE))
            }
            
            mocup <- puti * 100 / tuti
            
            dt = list(d , puti, tuti, mocup, tint)
            hdata <- rbind(hdata, dt)
          }
        }
      }
      else {
        if (grepl(nome_drs, hospital[i, "nome_drs"], fixed = TRUE)) {
          d = as.Date(hospital[i, "datahora"], "%Y-%m-%d")
          if (first) {
            mocup <- as.numeric(sub(",", ".", hospital[i, "pacientes_uti_mm7d"], fixed = TRUE)) * 100 / as.numeric(sub(",", ".", hospital[i, "total_covid_uti_mm7d"], fixed = TRUE))
            
            hdata <- data.frame("data"=d , "pacientes_uti_mm7d"=as.numeric(sub(",", ".", hospital[i, "pacientes_uti_mm7d"], fixed = TRUE)), "total_covid_uti_mm7d"=as.numeric(sub(",", ".", hospital[i, "total_covid_uti_mm7d"], fixed = TRUE)), "media_ocupacao"=mocup, "internacoes_7d"=as.numeric(sub(",", ".", hospital[i, "internacoes_7d"], fixed = TRUE)))
            first <- FALSE
          }
          else {
            mocup <- as.numeric(sub(",", ".", hospital[i, "pacientes_uti_mm7d"], fixed = TRUE)) * 100 / as.numeric(sub(",", ".", hospital[i, "total_covid_uti_mm7d"], fixed = TRUE))
            
            dt = list(d , as.numeric(sub(",", ".", hospital[i, "pacientes_uti_mm7d"], fixed = TRUE)), as.numeric(sub(",", ".", hospital[i, "total_covid_uti_mm7d"], fixed = TRUE)), mocup, as.numeric(sub(",", ".", hospital[i, "internacoes_7d"], fixed = TRUE)))
            hdata <- rbind(hdata, dt)
          }
        }
      }
    }
    hdays <- nrow(hdata)
    
    hmax <- max(c(max(hdata$total_covid_uti_mm7d), max(hdata$pacientes_uti_mm7d)))
    hmin <- min(c(min(hdata$total_covid_uti_mm7d), min(hdata$pacientes_uti_mm7d)))
    ocupmax <- which.max(hdata$media_ocupacao)
    
    output$p_uti <- renderPlot({
      plot(hdata$data, hdata$total_covid_uti_mm7d, type = "h", main = paste("Média móvel para 7 dias de ocupação de UTI\nRegião:", nome_drs), xlab = NA, ylab = NA, col="blue", ylim = c(hmin, hmax))
      par(new=TRUE)
      plot(hdata$data, hdata$pacientes_uti_mm7d, type = "h", xlab = NA, ylab = NA, col="red", ylim = c(hmin, hmax))
      legend("topright", legend=c("Oferta", "Ocupação"), pch=15, col=c("blue", "red"))
    })
    
    output$t_ocup <- renderText({
      paste("A maior taxa média de ocupação de UTI foi de ", format(round(hdata[ocupmax, "media_ocupacao"], 2), 2), "% em", as.character(hdata[ocupmax, "data"], "%d/%m/%Y"), "na região", nome_drs, ".")
    })
    
    # New hospitalization
    i <- (hdays - ((hdays %/% 7) * 7)) + 7
    first <- TRUE
    while (i <= hdays) {
      if (first) {
        idata <- hdata[i, "data"]
        inter <- hdata[i, "internacoes_7d"]
        first <- FALSE
      }
      else {
        idata <- append(idata, hdata[i, "data"])
        inter <- append(inter, hdata[i, "internacoes_7d"])
      }
      i <- i + 7
    }
    
    output$p_int <- renderPlot({
      plot(idata, inter, type = "h", main = paste("Acumulado de novas internações em 7 dias\nRegião:", nome_drs), xlab = NA, ylab = NA, col="blue")
    })
  })
}

app <- shinyApp(ui, server)
runApp(app, port = 3071, host = "0.0.0.0")