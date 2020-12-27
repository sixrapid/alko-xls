#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(tidyverse)
library(shinythemes)
library(readxl)
library(DT)

createLink <- function(id, orig) {
  sprintf('<a href="https://www.alko.fi/tuotteet/%s/" title="Alkon tuotesivu" target="_blank">%s</a>', id, orig)
}

download.file(url="https://www.alko.fi/INTERSHOP/static/WFS/Alko-OnlineShop-Site/-/Alko-OnlineShop/fi_FI/Alkon%20Hinnasto%20Tekstitiedostona/alkon-hinnasto-tekstitiedostona.xlsx", destfile="hinnasto.xlsx")
alko <- read_excel("hinnasto.xlsx", skip=2, guess_max = 10000)
title_str <- read_excel("hinnasto.xlsx", col_names = F, n_max = 1) %>% toString()
# date <- title_str %>% substring(16) %>% as.Date("%d.%m.%Y")

numeeriset = c("Numero"
               #, "Pullokoko"
               , "Hinta"
               , "Litrahinta"
               , "Vuosikerta"
               , "Alkoholi-%"
               , "Hapot g/l"
               , "Sokeri g/l"
               , "Kantavierrep-%"
               , "Väri EBC"
               , "Katkerot EBU"
               , "Energia kcal/100 ml")

faktorit = c("Uutuus"
             , "Tyyppi"
             , "Erityisryhmä"
             , "Oluttyyppi"
             , "Valmistusmaa"
             , "Alue"
             , "Pakkaustyyppi"
             , "Suljentatyyppi"
             , "Valikoima")

alko <- alko %>%
  mutate_at(numeeriset, as.numeric) %>%
  mutate_at(faktorit, as.factor) %>%
  mutate(Pullokoko = as.numeric(sub(",", ".", substr(Pullokoko, 1, nchar(Pullokoko) - 2), fixed = TRUE))) %>%
  rename(`Pullokoko, l` = Pullokoko, `Hinta, €` = Hinta) %>%
  mutate(`Tehosuhde (%/€)` = round(`Alkoholi-%` / Litrahinta, 2))

# Define UI
ui <- tagList(
  
  # CSS tags
  tags$head(
    tags$style(
      HTML("
        tfoot {
          display: table-header-group;
        }
      "
      )
    )
  )
  
  , navbarPage(
    windowTitle = title_str
    , title =
      div(
        img(
          src = "alcohol-bottle.svg"
          , height = 32
          , width = 32
          , style = "margin:-15px 2px"
        )
        , title_str
    )
    
    , theme = shinytheme("cosmo")

    # Main view
    , tabPanel(
      title = "Taulukko"
      
      # Column selection panel
      , sidebarPanel(
        checkboxGroupInput(
          'show_cols'
          , 'Valitut sarakkeet:'
          , names(alko)
          , selected = c(
            "Nimi"
            , "Pullokoko, l"
            , "Hinta, €"
            , "Tyyppi"
            , "Pakkaustyyppi"
            , "Alkoholi-%"
            , "Tehosuhde (%/€)"
          )
        )
        , width = 2
      )
    
      # Table
      , mainPanel(
        dataTableOutput("table")
        , width = 10
      )
    )
    
    , tabPanel(
      title = "Tietoa"
      , verticalLayout(
        mainPanel(
          h3("Alkon hinnasto + tehosuhteet taulukkona")
          , h4("Kasperi Kuuskoski")
          , div(
            "Toteutettu käyttäen R-ohjelmointikieltä sekä shiny, shinythemes, tidyverse/readxl ja DT -paketteja."
          )
          , div(
            "Hintatiedot perustuvat Alkon jakamaan XLS-taulukkoon, joka on saatavilla osoitteesta"
            , a("https://www.alko.fi/valikoimat-ja-hinnasto/hinnasto", href="https://www.alko.fi/valikoimat-ja-hinnasto/hinnasto")
            , "."
          )
          , br()
          , div(
            "Palautteet ja ehdotukset voi toimittaa githubissa @sixrapid."
          )
          , div(
            "Lähdekoodi:"
            , a("https://github.com/sixrapid/alkoholisti", href="https://github.com/sixrapid/alkoholisti")
            , "."
          )
          , br()
          , div(
            HTML(
              "Icon made by <a href='http://www.freepik.com' title='Freepik'>Freepik</a> 
              from <a href='https://www.flaticon.com/' title='Flaticon'>www.flaticon.com</a> 
              is licensed by <a href='http://creativecommons.org/licenses/by/3.0/' 
              title='Creative Commons BY 3.0' target='_blank'>CC 3.0 BY</a>"
            )
          )
        )
      )
    )
  )
)

# Define server logic
server <- function(input, output) {
  
  # table config
  output$table <- DT::renderDataTable(
    {
      alko$Nimi <- createLink(alko$Numero, alko$Nimi)
      return(alko[,input$show_cols])
    }
    , rownames = FALSE
    , filter = "top"
    , options = list(
      pageLength = 15
      , autoWidth = T
      , lengthMenu = c(5, 10, 15, 30, 50, 100)
      , dom = "ltipr"
      , language = list(url = "//cdn.datatables.net/plug-ins/1.10.19/i18n/Finnish.json")
    )
    , selection = "none"
    , escape = F
  )

}

# Run the application 
shinyApp(ui = ui, server = server)

