library(pracma)
library(shiny)

## Get Covid data CSV
download.file("https://raw.githubusercontent.com/seade-R/dados-covid-sp/master/data/dados_covid_sp.csv", "dados_covid_sp.csv")
file <- read.csv("dados_covid_sp.csv", sep=";")
cities <- unique(file$nome_munic)

ui <- fluidPage(
  titlePanel("Covid-19 - Estado de São Paulo"),
  selectInput("city", "Selecione a cidade", choices = cities, selected = "São Paulo"),
  
  textOutput("title", container = h2),
  textOutput("general"),
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
    output$cases <- renderText({
      paste("Total de casos:", cdata[days, "casos"], ".")
    })
    output$death <- renderText({
      paste("Total de óbitos:", cdata[days, "obitos"], ".")
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
      while (cdata[i, "casos_novos"] == 0) {
        i <- i-1
        zc <- zc+1
      }
      output$z_cases <- renderText({
        paste("Não houveram novos casos nos últimos ", zc, " dias.")
      })
    }
    
    if (diffcase > 0) {
      tc <- format(round(((caseavg[days]/caseavg[days - 7])-1)*100, 2), 2)
      output$d_cases <- renderText({
        paste("A média móvel cresceu de ", format(round(caseavg[days - 7], 2), 2), " para ", format(round(caseavg[days], 2), 2), " na última semana, ", tc, "%.")
      })
    }
    else if (diffcase < 0) {
      tc <- format(round((1-(caseavg[days]/caseavg[days - 7]))*100, 2), 2)
      output$d_cases <- renderText({
        paste("A média móvel diminuiu de ", format(round(caseavg[days - 7], 2), 2), " para ", format(round(caseavg[days], 2), 2), " na última semana, ", tc, "%.")
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
      while (cdata[i, "obitos_novos"] == 0) {
        i <- i-1
        zd <- zd+1
      }
      output$z_death <- renderText({
        paste("Não houveram novos óbitos nos últimos ", zd, " dias.")
      })
    }
    
    if (diffdeath > 0) {
      td <- format(round(((deathavg[days]/deathavg[days - 7])-1)*100, 2), 2)
      output$d_death <- renderText({
        paste("A média móvel cresceu de ", format(round(deathavg[days - 7], 2), 2), " para ", format(round(deathavg[days], 2), 2), " na última semana, ", td, "%.")
      })
    }
    else if (diffdeath < 0) {
      td <- format(round((1-(deathavg[days]/deathavg[days - 7]))*100, 2), 2)
      output$d_death <- renderText({
        paste("A média móvel diminuiu de ", format(round(deathavg[days - 7], 2), 2), " para ", format(round(deathavg[days], 2), 2), " na última semana, ", td, "%.")
      })
    }
    else {
      output$d_death <- renderText({
        paste("A média móvel é estável na última semana.")
      })
    }
    
  })
}

app <- shinyApp(ui, server)
runApp(app, port = 3071, host = "0.0.0.0")