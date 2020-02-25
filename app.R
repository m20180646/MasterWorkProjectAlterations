# I'm cleaning the global environment
rm(list = ls())

library(shiny)
library(shinyjs)
library(shinyWidgets)
library(dplyr)

# This is how we load a R package

#Packages 
library(plotly)
library(rworldmap)
library(RColorBrewer)
library(ggplot2)

#Packages necessary to deploy app
library(htmltools)
library(vembedr)
library(V8)

#load("PRTExternalRelations.RData")


goods_c$'Country code' <- as.character(goods_c$'Country code')
data.mapGC <- joinCountryData2Map(goods_c, joinCode = "UN", nameJoinColumn = "Country code")

goods_d$'Country code' <- as.character(goods_d$'Country code')
data.mapGD <- joinCountryData2Map(goods_d, joinCode = "UN", nameJoinColumn = "Country code")

goods_n$'Country code' <- as.character(goods_n$'Country code')
data.mapGN <- joinCountryData2Map(goods_n, joinCode = "UN", nameJoinColumn = "Country code")

services_c$'Country code' <- as.character(services_c$'Country code')
data.mapSC <- joinCountryData2Map(services_c, joinCode = "UN", nameJoinColumn = "Country code")

services_d$'Country code' <- as.character(services_d$'Country code')
data.mapSD <- joinCountryData2Map(services_d, joinCode = "UN", nameJoinColumn = "Country code")

services_n$'Country code' <- as.character(services_n$'Country code')
data.mapSN <- joinCountryData2Map(services_n, joinCode = "UN", nameJoinColumn = "Country code")


### User interface
ui <- fluidPage(#theme="bootstrap.css"
  
  setSliderColor(c("#8E0000 ", "#FFFFFF", "#FFFFFF","#FFFFFF"), c(1, 2, 3, 4)),
  
  tags$head(
    tags$style(HTML(" 
                    h1 {
                    font-family: 'Times New Roman';
                    font-size:13;
                    font-weight: 100;
                    line-height: 0.5;
                    color: #8E0000;
                    }
                    
                    "))
    ),
  
  tags$head(tags$style(HTML('.js-irs-0 .irs-bar {
                            border-top-color: #8E0000;
                            border-bottom-color: #8E0000;
                            } 
                            
                            .js-irs-0 .irs-bar-edge {
                            border-color: #8E0000;
                            }
                            
                            .js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar {
                            background: #8E0000;
                            }
                            
                            .js-irs-1 .irs-bar {
                            border-top-color: #8E0000;
                            border-bottom-color: #8E0000;
                            } 
                            
                            .js-irs-1 .irs-bar-edge {
                            border-color: #8E0000;
                            }
                            
                            .js-irs-1 .irs-single, .js-irs-1 .irs-bar-edge, .js-irs-1 .irs-bar {
                            background: #8E0000;
                            }
                            
                            /* changes the colour of the number tags */
                            .irs-from, .irs-to, .irs-single { background: #8E0000 }
                            .fa-2x {color:#8E0000}'
                            
  ))),
  
  
  headerPanel(title=div(img(src="world_map_red.png",height = 100, width = 100),"PORTUGUESE EXTERNAL TRANSACTIONS")),
  br(),
  br(),
  br(),
  br(),
  br(),
  br(),
  br(),
  
  navbarPage(title = '',
             tabPanel("Foreword",HTML('<center><span style= color:rgb(142,0,0)><font face=Calibri size=6>Portuguese current and capital account</center> </font></span>'),
                      
                      HTML('<center><span style= color:rgb(142,142,142)><font face=Calibri size=3>Maria do Mar Viana <a href="https://www.linkedin.com/in/maria-do-mar-viana/">LinkedIn</a></center> </font></span>')
                      ,                       HTML('<center><span style= color:rgb(142,142,142)><font face=Calibri size=3>Vitor Lopes Silveira <a href="https://www.linkedin.com/in/vitor-lopes-silveira/">LinkedIn</a></center> </font></span>')
                      ,   HTML('<center><span style= color:rgb(142,142,142)><font face=Calibri size=3>   This prototype will be used in the Project Work "How to effectively use interactivity to improve visual analysis and communication in groups of novices or experts, using the R package Shiny" developed by Maria do Mar Viana.</a><//center> </font></span>')
                      
                      ,    uiOutput("video")
                      
                      
             ),
             tabPanel("By major items",
                      sidebarLayout(
                        sidebarPanel(
                          width = 2,
                          #  helpText('Select a time period:'),
                          
                          # Slider to select the years 
                          # tags$style(HTML(".js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar {background: #8E0000}")),
                          setSliderColor(c("#8E0000 ", "#8E0000", "#8E0000","#8E0000"), c(1, 2, 3,4)),
                          useShinyjs(),
                          
                          sliderInput("dates", "Select a time period in the slider:",
                                      min = 1996, max = 2018,
                                      sep = "", step= 3,
                                      ticks=TRUE,
                                      value = c(1996,2018)),
                          
                          actionButton("reset1V", "Reset")
                          #,br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),
                          #   HTML("<p align=right><font size=3><span style= color:rgb(142,0,0)><strong>Did you know that...</strong></font></span>
                          #<br><br><em><font size=2><span style= color:rgb(72,72,72)>
                          #Between 1996 and 2011 Portugal was a net external debtor. Services, secondary income (where migrants' remittances are included) and capital account contribute positively to the current and capital account balance,
                          #while goods and primary income (income earned from production, capital and labour factors) contribute
                          #negatively.</span></em></font></p>
                          #")
                        ),
                        mainPanel(
                          HTML('<center><span style= color:rgb(142,0,0)><font face=Calibri size=6>Current and capital account, by major items (EUR Millions)</center> </font></span>'),
                          
                          HTML("The <strong>current and capital account</strong> (<strong><span style = color:rgb(142,0,0)>red line</span></strong>) measures the surplus (+) or deficit (-) of the economy with regard to other countries. In this particular case, when positive, the Portuguese economy is lending to the rest of the world. When negative, the Portuguese economy is borrowing from the rest of the world. Current and capital account can be analyzed by major items: <br></br>
                               "), 
                          HTML("<div> <strong> <li style=margin-left:2em>  Goods and services account</strong> (<strong><span style = color:rgb(194,129,145)>light red bar </span></strong>and <strong><span style = color:rgb(255,236,114)>yellow bar</span></strong>, respectively) - transactions resulting from '<em>outcomes of the production process</em>' [Balance values (represented below) = Exports (credit)-Imports (debit)];</div> </li>
                               <div>  <strong> <li style=margin-left:2em> Primary income account</strong> (<strong><span style = color:rgb(139,177,154)>green bar</span></strong>) - flows related to '<em>income generated in the production process</em>'; for instance, dividends or interest. [Balance values (shown below) = Credit-Debit, where credit/debit reflect primary income received/paid by the Portuguese economy];</div></li>
                               <div>  <strong> <li style=margin-left:2em> Secondary income account </strong> (<strong><span style = color:rgb(108,173,225)>blue bar</span></strong>) - current transfers '<em>made in cash or in kind</em>'; for instance, international cooperation, donations, remittances, lotteries and other gambling [Balance values below = Credit-Debit];  </div></li>
                               <div> <strong> <li style=margin-left:2em> Capital account </strong> (<strong><span style = color:rgb(227,194,145)>dark yellow bar</strong></span>) - capital transfers not resulting from the production process; for instance, buying/selling natural resources, goodwill, marketing brands [Balance values below = Credit-Debit].</div></li> 
                               <div>  You can find more about each major item by moving the mouse over the bars in the stacked bar chart or the line in the line chart (<strong>tooltips</strong>). </div> 
                               
                               "),br(),
                          plotlyOutput("distPlot", width = "100%", height="350px"),br(), HTML('<span style = color:rgb(128,128,128)>Data source: Banco de Portugal, April 2019 <a href="https://www.bportugal.pt/EstatisticasWeb/(S(iczvpy45y4bm4r45n2n55045))/DEFAULT.ASPX?Lang=en-GB">Click here</a> <div>Methodological source: Balance of Payments and International Investment Position Manual, Sixth Edition (BPM 6), 2009 <a href="https://www.imf.org/external/pubs/ft/bop/2007/pdf/bpm6.pdf">Click here</a></span></div>')
                          )
                          )
                      ),tabPanel("By geographical counterpart",
                                 sidebarLayout(
                                   sidebarPanel(
                                     width = 2,
                                     setSliderColor(c("#8E0000 ", "#8E0000", "#8E0000","#8E0000"), c(1, 2, 3,4)),
                                     # Slider to select the years 
                                     useShinyjs(),
                                     sliderInput("mapdates", "Select a time period in the slider or press play:",
                                                 min = 1996, max = 2018,
                                                 value = 2018, step=1,
                                                 ticks= TRUE,
                                                 sep = "",
                                                 animate =
                                                   animationOptions(interval = 1000, loop = FALSE, playButton = icon('play', "fa-2x"),
                                                                    pauseButton = icon('pause', "fa-2x"))),
                                     tags$head(tags$style(type='text/css', ".slider-animate-button { font-size: 15pt !important; }")),
                                     
                                     selectInput('mapvariable','Select an item in the select box:',
                                                 c("Goods (Exports)","Goods (Imports)","Goods (Balance=Exports-Imports)","Services (Exports)",
                                                   "Services (Imports)","Services (Balance=Exports-Imports)")),
                                     actionButton("reset2V", "Reset")
                                     #  , br(),br(),br(),
                                     #       HTML("<p align=right><font size=3><span style= color:rgb(142,0,0)><strong>Did you know that...</strong></font></span>
                                     #<br><br><em><font size=2><span style= color:rgb(72,72,72)>
                                     #                    Portuguese international trade (both imports and exports) are highly concentrated in Europe.
                                     #<br><br>
                                     #                    However, for the goods item, after the international economic and financial crisis of 2009, non-European countries like the USA and China gained momentum.
                                     #                   </span></em></font></p>
                                     #                  ")
                                   ),
                                   mainPanel(
                                     
                                     HTML('<center><span style= color:rgb(142,0,0)><font face=Calibri size=6>Geographical breakdown of the selected item/year (EUR Millions)</center> </font></span>'),
                                     
                                     HTML('The choropleth map below represents, for the item/year selected, the relative value of each country in the world context with Portugal. The relative position of the counterpart economy was defined in terms of quartiles*:
                                          <li style=margin-left:2em>    <strong>Very High </strong> (<strong><span style = color:rgb(204,76,2)>dark orange colour</span></strong>) - country has very high values when compared with the other countries; <strong> top 25% of the world (fourth quartile); </li> </strong>
                                          <li style=margin-left:2em>   <strong> High </strong> (<strong><span style = color:rgb(254,153,41)>orange colour</span></strong>) - country has high values when compared to other countries; <strong> between top 50% and top 25% of the world (third quartile); </li> </strong>
                                          <li style=margin-left:2em>     <strong> Medium </strong> (<strong><span style = color:rgb(254,217,142)>light orange colour</span></strong>) - country has medium values when compared to other countries; <strong> between bottom 25% and bottom 50% of the world (second quartile); </li> </strong>
                                          <li style=margin-left:2em>       <strong> Low </strong> (<strong><span style = color:rgb(249,239,165)>yellow colour</span></strong>) - country has low values when compared with the other countries; <strong> bottom 25% of the world (first quartile). </li> </strong>
                                          '), br(),
                                     HTML('<span style = color:rgb(128,128,128)> *Quartiles: "A quartile is a type of quantile which divides the number of data points into four more or less equal parts, or quarters"</span>'),
                                     plotOutput("map",width = "100%", height="550px"), 
                                     HTML('<span style = color:rgb(128,128,128)>Data source: Banco de Portugal, April 2019 <a href="https://www.bportugal.pt/EstatisticasWeb/(S(iczvpy45y4bm4r45n2n55045))/DEFAULT.ASPX?Lang=en-GB">Click here</a> <div>Methodological source: Balance of Payments and International Investment Position Manual, Sixth Edition (BPM 6), 2009 <a href="https://www.imf.org/external/pubs/ft/bop/2007/pdf/bpm6.pdf">Click here</a></span></div>'))
                                   
                                   )),
             tabPanel("By monthly periodicity",
                      sidebarLayout(
                        sidebarPanel(
                          width = 2,
                          br(),
                          br(),
                          br(),
                          br(),
                          br(),
                          br(),
                          br(),
                          br(),
                          useShinyjs(),
                          selectInput('heatmapvariable','Select an item in the select box:',
                                      c("Current and capital account (Balance)","Current and capital account (Credit)","Current and capital account (Debit)",
                                        "Goods (Balance)",  "Goods (Exports)", "Goods (Imports)",
                                        "Services (Balance)", "Services (Exports)","Services (Imports)")),
                          actionButton("reset3V", "Reset")
                          #        ,br(),br(),br(),
                          #           HTML("<p align=right><font size=3><span style= color:rgb(142,0,0)><strong>Did you know that...</strong></font></span>
                          #               <br><br><em><font size=2><span style= color:rgb(72,72,72)>
                          #              From 2012 onwards the months of July and August have a determinant positive impact in the Portuguese current and capital account...
                          #
                          #                   <br><br>
                          #                  ...this is especially true in the services item. 
                          #
                          #                   </span></em></font></p>
                          #                  ")   
                        ),
                        mainPanel( 
                          HTML('<center><span style= color:rgb(142,0,0)><font face=Calibri size=6>Patterns of the selected items per year/month (EUR Millions)</center> </font></span>'),
                          HTML("The heatmap below exhibits the value of the select item in terms of year/month (defined by the grid). The values are defined in millions of Euros and should be interpreted as a function of the colour saturation; ranges from <strong><span style = color:rgb(255,0,0)>red</span></strong> (lower values) to <strong><span style = color:rgb(17,104,58)>dark green</span></strong> (higher values).
                               You can see the exact values of each item for each combination of year/month by moving the mouse over each square of colour (<strong>tooltips</strong>). 
                               
                               <br>  <br>
                               <em>Hint: Balance=Credit-Debit, where in the case of goods and services, credit/debit can be readable as exports/imports.</em>
                               "),
                          
                          plotlyOutput("heatmap", width = "100%", height="500px"),br(), HTML('<span style = color:rgb(128,128,128)>Data source: Banco de Portugal, April 2019 <a href="https://www.bportugal.pt/EstatisticasWeb/(S(iczvpy45y4bm4r45n2n55045))/DEFAULT.ASPX?Lang=en-GB">Click here</a> <div>Methodological source: Balance of Payments and International Investment Position Manual, Sixth Edition (BPM 6), 2009 <a href="https://www.imf.org/external/pubs/ft/bop/2007/pdf/bpm6.pdf">Click here</a></span></div>')
                          
                          # d3heatmapOutput("heatmap", width = "100%", height="600px")
                          )
                        )
                      ),
             tabPanel("By type of services",
                      sidebarLayout(
                        sidebarPanel(
                          width = 2,
                          useShinyjs(),
                          sliderInput("bubledates", "Select a time period in the slider or press play:",
                                      min = 1996, max = 2018,
                                      value = 2018, step=1,
                                      ticks=TRUE,
                                      sep = "",
                                      animate =
                                        animationOptions(interval = 1000, loop = FALSE, playButton = icon('play', "fa-2x"),
                                                         pauseButton = icon('pause', "fa-2x"))),
                          br(),
                          br(),
                          br(),
                          br(),
                          br(),
                          br(),
                          radioButtons("rd_services",label = "Select in the radio box which type of services to include:", choices = list("All types of services","All services except travel and transport"))
                          ,actionButton("reset4V", "Reset")
                          #  ,br(),br(),br(),
                          #        HTML("<p align=right><font size=3><span style= color:rgb(142,0,0)><strong>Did you know that...</strong></font></span>
                          #            <br><br><em><font size=2><span style= color:rgb(72,72,72)>
                          #           One of the main drivers of the Portuguese current and capital account reversal, after 2012, was the travel item.
                          #
                          #                   <br><br>
                          #                  In this period, travel exports almost doubled.
                          #
                          #                   </span></em></font></p>
                          #                  ")   
                        ),
                        mainPanel(   
                          HTML('<center><span style= color:rgb(142,0,0)><font face=Calibri size=6> Evolution of the exports/imports of the different type of services (EUR Millions)</center> </font></span>'),
                          HTML('The scatterplot chart below shows the evolution of exports/imports by type of service. 
                               The <strong><span style= color:rgb(142,0,0)>red line</span></strong> represents the <strong>bissectrice</strong>; 
                               when the bubble (service) is below the line, the service under analysis has, for the chosen year, 
                               exports greater than the imports (and vice-versa). <br> <br> You can find more about the services by moving the mouse over the bubble (<strong>tooltips</strong>), such as the specific values of exports and imports or the weight (value of exports of that service divided by the total value of exports for that given year). <br> There is also the option to see all the types of services displayed or <strong>all the types of services excluding the transport and travel</strong>.
                               <br>  <br>      
                               <em>  Hint: below the graph you may find the definition of each type of service.</em> 
                               '),br(),br(),
                          
                          plotlyOutput("bubles",width = "100%", height="500px"),br(),HTML('<span style = color:rgb(128,128,128)>Data source: Banco de Portugal, April 2019 <a href="https://www.bportugal.pt/EstatisticasWeb/(S(iczvpy45y4bm4r45n2n55045))/DEFAULT.ASPX?Lang=en-GB">Click here</a> <div>Methodological source: Balance of Payments and International Investment Position Manual, Sixth Edition (BPM 6), 2009 <a href="https://www.imf.org/external/pubs/ft/bop/2007/pdf/bpm6.pdf">Click here</a></span></div>')
                          ,br(),br(),HTML("<strong>Methodological definition of the different types of services:</strong>
                                          <br> </br>
                                          <li style=margin-left:2em><strong>Manufacturing services on physical inputs owned by others </strong> - 'cover processing, assembly, labeling,
                                          packing, and so forth undertaken by enterprises that do not own the goods concerned'; </li>
                                          
                                          <li style=margin-left:2em><strong> Maintenance and repair services not included elsewhere (n.i.e.) </strong>- 'cover maintenance and repair work by residents on goods that are owned by nonresidents (and vice versa)';
                                          </li>                 
                                          
                                          <li style=margin-left:2em> <strong> Transport </strong> - 'process of carriage of people and objects from one location to another as well as related supporting and auxiliary services. Also included are postal and courier services';
                                          </li>                 
                                          
                                          
                                          <li style=margin-left:2em><strong> Travel </strong> - 'travel credits cover goods and services for own use or to give away acquired from an economy by nonresidents during visits to that economy. Travel debits cover goods and services for own use or to give away acquired from other economies by residents during visits to these other economies';
                                          </li>                 
                                          
                                          
                                          <li style=margin-left:2em> <strong>  Construction </strong>- 'creation, renovation, repair, or extension of fixed assets in the form of buildings, land improvements of an engineering nature, and
                                          other such engineering constructions as roads, bridges, dams, and so forth';
                                          </li>                 
                                          
                                          <li style=margin-left:2em> <strong> Insurance and pension services </strong>- 'include services of providing life insurance and annuities, nonlife insurance, reinsurance, freight insurance, pensions, standardized guarantees, and auxiliary services to insurance, pension schemes, and standardized guarantee schemes';
                                          </li>
                                          
                                          <li style=margin-left:2em> <strong>  Financial services </strong> - 'cover financial intermediar and auxiliary services, except insurance and pension fund services'; 
                                          </li>
                                          
                                          <li style=margin-left:2em>   <strong> Charges for the use of intellectual property n.i.e. </strong> - 'charges for the use of proprietary rights (such as patents, trademarks, copyrights, industrial
                                          processes and designs including trade secrets, franchises). These rights can arise from research and development, as well as from marketing;
                                          and, Charges for licenses to reproduce or distribute (or both) intellectual property embodied in produced originals or prototypes (such as copyrights on books and manuscripts, computer software, cinematographic works, and sound recordings) and related rights (such as for live performances and television, cable, or satellite broadcast)';
                                          </li>
                                          
                                          <li style=margin-left:2em>   <strong>    Telecommunications, computer, and information services </strong> - 'computer and telecommunication services are defined in terms of the nature of the service, not the
                                          method of delivery. To illustrate, provision of business services, such as accounting services, is included under the appropriate heading under other business
                                          services, even if these services are entirely delivered by telephone, computer, or the Internet. Only amounts payable for transmission should be included under telecommunications services; downloaded content should be included in the appropriate item (computer, information, audiovisual, etc., services)';
                                          </li>
                                          
                                          <li style=margin-left:2em>    <strong>   Other business services </strong>- 'research and development services, professional and management consulting services, technical, trade related, and other business services, waste treatment and depollution, agricultural and mining services, operating leasing, trade-related services and other business services';
                                          </li>
                                          
                                          <li style=margin-left:2em>  <strong>   Personal, cultural and recreational services </strong>- 'consist of (a) audiovisual and related services and (b) other personal, cultural, and recreational services';
                                          </li>
                                          
                                          <li style=margin-left:2em>  <strong>     Government goods and services n.i.e. </strong> - 'cover: (a) goods and services supplied by and to enclaves, such as embassies, military bases, and international organizations; (b) goods and services acquired from the host economy by diplomats, consular staff, and military personnel located abroad and their dependents; and. services supplied by and to governments and not included in other categories of services'.
                                          </li> <br> </br>
                                          ")
                          
                          
                          )
                          )
                        ),
             tabPanel("By international comparison",
                      sidebarLayout(
                        sidebarPanel(
                          width = 2,
                          useShinyjs(),
                          sliderInput("internationaldates", "Select a time period in the slider or press play:",
                                      min = 2007, max = 2018,
                                      value = 2018, step=1,
                                      ticks=TRUE,
                                      sep = "",
                                      animate =
                                        animationOptions(interval = 1000, loop = FALSE, playButton = icon('play', "fa-2x"),
                                                         pauseButton = icon('pause', "fa-2x"))),
                          br(),
                          br(),
                          br(),
                          br(),
                          radioButtons("rd",label = "Select in the radio box which group of countries to compare Portugal with:", choices = list("All European countries","Italy, Greece and Spain","EU entry after 2004"))
                          ,actionButton("reset5V", "Reset")
                          #,br(),br(),br(),
                          #  HTML("<p align=right><font size=3><span style= color:rgb(142,0,0)><strong>Did you know that...</strong></font></span>
                          #      <br><br><em><font size=2><span style= color:rgb(72,72,72)>
                          #Portuguese current account reversal in 2012 was not a unique phenomenon within the European context.
                          #
                          #                
                          #               <br><br>
                          #              In 2018, when compared to Portugal, the majority of the European countries had a higher value in % of the GDP.
                          #
                          #                
                          #               </span></em></font></p>
                          #              ")   
                          
                        ),
                        #   setBackgroundColor(color = "yellow")
                        mainPanel(HTML('<center><span style= color:rgb(142,0,0)><font face=Calibri size=6> Current account balance per European country (% GDP*, three year average) </center> </font></span>')
                                  ,HTML("The current account balance is one of the indicators used by the European Commission to access macroeconomic imbalances; '<em>any trend giving rise to macroeconomic developments which are adversely affecting, or have the potential to adversely affect, the proper functioning of the economy</em>'.
                                        <br><br>In the graph below it is possible to compare the Portuguese economy (<span style = color:rgb(142,0,0)><strong>red bar</strong></span>) with other European countries (<span style = color:rgb(204,198,204)><strong>grey bars</strong></span>), for each given year . <div>  You can find more about the values of each country by moving the mouse over the bars in the bar chart (<strong>tooltips</strong>). </div>
                                        "),br(),br(),br(),
                                  plotlyOutput("international", width = "100%")
                                  , HTML('<span style = color:rgb(128,128,128)>Data source: Eurostat, April 2019 <a href="https://ec.europa.eu/eurostat/statistics-explained/index.php/The_Macroeconomic_Imbalance_Procedure_%28MIP%29_introduced">Click here</a> </span>')
                                  ,br(), HTML('<span style = color:rgb(128,128,128)> *GDP=Gross Domestic Product </span>')
                                  #      ,br(), HTML('<span style = color:rgb(128,128,128)> **PIGS= Acronym for Portugal, Italy, Greece and Spain </span>')    
                                  )  
                        )
                      
             ),
             tags$head(
               tags$style(type = 'text/css', 
                          HTML('.navbar { background-color: #F0F0F0;}
                               .navbar-default .navbar-brand{color: white;}
                               .tab-panel{ background-color: red; color: white}
                               .navbar-default .navbar-nav > .active > a, 
                               .navbar-default .navbar-nav > .active > a:focus, 
                               .navbar-default .navbar-nav > .active > a:hover {
                               color: #FFFFFF;
                               background-color: #8E0000;
                               }')
                          )
                          )      
                          ))

### Server component
server <- function(input, output) {
  
  
  
  output$distPlot <- renderPlotly({
    
    plot_ly(x = ~df$Year) %>% 
      add_bars(y = ~df$Goods,
               name = "Goods", marker = list(color = '#c28191')) %>% 
      add_bars(y = ~df$Services,
               name = "Services", marker = list(color = '#FFEC72')) %>%
      add_bars(y = ~df$`Primary income`,
               name = "Primary income", marker = list(color = '#8BB19A')) %>% 
      add_bars(y = ~df$`Secondary income`,
               name = "Secondary income", marker = list(color = '#6CADE1')) %>%
      add_bars(y = ~df$`Capital account`,
               name = "Capital account", marker = list(color = '#E3c291')) %>%
      add_lines(y = ~df$`Current and capital account`,
                name = "Current and capital account",
                yaxis = "y", marker = list(color = '#8E0000')) %>% 
      layout(barmode = "relative",
             yaxis = list(overlaying = "y",
                          side = "left"),
             barmode = "relative")%>%
      #  layout(yaxis = list(range = c(-32000, 22000))) %>%
      layout(xaxis = list(range = c(input$dates[1]-0.5,input$dates[2]+0.5))) %>%
      layout(   xaxis = list( ticktext = list(1996,1998,2000,2002,2004,2006,2008,2010,2012,2014,2016,2018), 
                              tickvals = list(1996,1998,2000,2002,2004,2006,2008,2010,2012,2014,2016,2018),
                              tickmode = "array",
                              title = '',
                              showgrid = TRUE,
                              gridcolor = toRGB("white")))%>%
      layout(separators= ". ", yaxis = list(title = "",
                                            showgrid = TRUE,
                                            gridcolor = toRGB("white"),
                                            exponentformat ='none'
      ))%>%
      layout(plot_bgcolor='#EBEBEB')  %>%
      layout(
        yaxis = list(zeroline = F, hoverformat = '.2f'))
    
    
    
    
    
  })
  
  observeEvent(input$reset1V, {
    reset("dates")
  })
  
  
  
  output$map <- renderPlot({
    
    colourPalette <- brewer.pal(4,'YlOrBr')
    
    if (input$mapvariable=='Goods (Exports)') {    
      
      #  colourPalette <- brewer.pal(4,'YlOrRd')
      #  colourPalette <- c("#FFEC72","#E3C291","#c28191","#8E0000") 
      
      #find quartile breaks
      cutVector <- quantile(data.mapGC@data[[paste("Y",input$mapdates, sep="")]],na.rm=TRUE)
      
      data.mapGC@data[[paste("Y",input$mapdates, sep="")]] <- cut(data.mapGC@data[[paste("Y",input$mapdates, sep="")]]
                                                                  , cutVector
                                                                  , include.lowest=TRUE )
      
      levels( data.mapGC@data[[paste("Y",input$mapdates, sep="")]]) <- c('Low', 'Medium', 'High', 'Very High')
      
      
      
      mapCountryData( data.mapGC
                      , nameColumnToPlot=paste("Y",input$mapdates, sep="")
                      , catMethod='categorical'
                      , mapTitle= paste("Year",input$mapdates, sep=" ")
                      , colourPalette= colourPalette
                      , oceanCol= NA
                      , missingCountryCol='white'
      )
      
      
    }
    
    if (input$mapvariable=='Goods (Imports)') {    
      
      
      #find quartile breaks
      cutVector <- quantile(data.mapGD@data[[paste("Y",input$mapdates, sep="")]],na.rm=TRUE)
      
      data.mapGD@data[[paste("Y",input$mapdates, sep="")]] <- cut(data.mapGD@data[[paste("Y",input$mapdates, sep="")]]
                                                                  , cutVector
                                                                  , include.lowest=TRUE )
      
      levels( data.mapGD@data[[paste("Y",input$mapdates, sep="")]]) <- c('Low', 'Medium', 'High', 'Very High')
      
      
      mapCountryData( data.mapGD
                      , nameColumnToPlot=paste("Y",input$mapdates, sep="")
                      , catMethod='categorical'
                      , mapTitle= paste("Year",input$mapdates, sep=" ")
                      , colourPalette= colourPalette
                      , oceanCol= NA
                      , missingCountryCol='white'
      )
      
      
      
    }
    
    if (input$mapvariable=='Goods (Balance=Exports-Imports)') {    
      
      
      #find quartile breaks
      cutVector <- quantile(data.mapGN@data[[paste("Y",input$mapdates, sep="")]],na.rm=TRUE)
      
      data.mapGN@data[[paste("Y",input$mapdates, sep="")]] <- cut(data.mapGN@data[[paste("Y",input$mapdates, sep="")]]
                                                                  , cutVector
                                                                  , include.lowest=TRUE )
      
      levels( data.mapGN@data[[paste("Y",input$mapdates, sep="")]]) <- c('Low', 'Medium', 'High', 'Very High')
      
      mapCountryData( data.mapGN
                      , nameColumnToPlot=paste("Y",input$mapdates, sep="")
                      , catMethod='categorical'
                      , mapTitle= paste("Year",input$mapdates, sep=" ")
                      , colourPalette= colourPalette
                      , oceanCol= NA
                      , missingCountryCol='white'
      )
      
    }
    
    if (input$mapvariable=='Services (Exports)') {    
      
      
      #find quartile breaks
      cutVector <- quantile(data.mapSC@data[[paste("Y",input$mapdates, sep="")]],na.rm=TRUE)
      
      data.mapSC@data[[paste("Y",input$mapdates, sep="")]] <- cut(data.mapSC@data[[paste("Y",input$mapdates, sep="")]]
                                                                  , cutVector
                                                                  , include.lowest=TRUE )
      
      levels( data.mapSC@data[[paste("Y",input$mapdates, sep="")]]) <- c('Low', 'Medium', 'High', 'Very High')
      
      
      mapCountryData( data.mapSC
                      , nameColumnToPlot=paste("Y",input$mapdates, sep="")
                      , catMethod='categorical'
                      , mapTitle= paste("Year",input$mapdates, sep=" ")
                      , colourPalette= colourPalette
                      , oceanCol=NA
                      , missingCountryCol='white'
      )
      
    }
    
    
    if (input$mapvariable=='Services (Imports)') {    
      
      
      #find quartile breaks
      cutVector <- quantile(data.mapSD@data[[paste("Y",input$mapdates, sep="")]],na.rm=TRUE)
      
      data.mapSD@data[[paste("Y",input$mapdates, sep="")]] <- cut(data.mapSD@data[[paste("Y",input$mapdates, sep="")]]
                                                                  , cutVector
                                                                  , include.lowest=TRUE )
      
      levels( data.mapSD@data[[paste("Y",input$mapdates, sep="")]]) <- c('Low', 'Medium', 'High', 'Very High')
      
      mapCountryData( data.mapSD
                      , nameColumnToPlot=paste("Y",input$mapdates, sep="")
                      , catMethod='categorical'
                      , mapTitle= paste("Year",input$mapdates, sep=" ")
                      , colourPalette= colourPalette
                      , oceanCol=NA
                      , missingCountryCol='white'
      )
      
      
    }
    
    
    if (input$mapvariable=='Services (Balance=Exports-Imports)') {    
      
      
      #find quartile breaks
      cutVector <- quantile(data.mapSN@data[[paste("Y",input$mapdates, sep="")]],na.rm=TRUE)
      
      data.mapSN@data[[paste("Y",input$mapdates, sep="")]] <- cut(data.mapSN@data[[paste("Y",input$mapdates, sep="")]]
                                                                  , cutVector
                                                                  , include.lowest=TRUE )
      
      levels( data.mapSN@data[[paste("Y",input$mapdates, sep="")]]) <- c('Low', 'Medium', 'High', 'Very High')
      
      mapCountryData( data.mapSN
                      , nameColumnToPlot=paste("Y",input$mapdates, sep="")
                      , catMethod='categorical'
                      , mapTitle= paste("Year",input$mapdates, sep=" ")
                      , colourPalette= colourPalette
                      , oceanCol=NA
                      , missingCountryCol='white'
      )
      
    }
    
  })
  
  
  
  observeEvent(input$reset2V, {
    reset("mapdates")
    reset("mapvariable")
  })
  
  
  
  colourPalette <- (brewer.pal(5,'RdYlGn'))
  
  
  output$heatmap <-  renderPlotly({
    
    ylab = list(categoryorder = "array", categoryarray = c("Dec","Nov","Out","Set","Ag","Jul","Jun","Mai","Apr","Mar","Feb","c"))
    #   colourPalette <- brewer.pal(9,'YlGn')
    
    
    if (input$heatmapvariable=='Current and capital account (Balance)') { 
      plot_ly(x=colnames(heat_cca_n), y=rownames(heat_cca_n),z = heat_cca_n_matrix,colors =colourPalette, type = "heatmap")%>%
        layout(yaxis = list(autorange = "reversed")) %>%
        layout(
          xaxis = list(
            ticktext = list("1996", "1997", "1998", "1999", "2000", "2001","2002","2003","2004","2005","2006","2007","2008","2009","2010","2011","2012","2013","2014","2015","2016","2017","2018"),
            tickvals = list("1996", "1997", "1998", "1999", "2000", "2001","2002","2003","2004","2005","2006","2007","2008","2009","2010","2011","2012","2013","2014","2015","2016","2017","2018"),
            tickmode = "array"
          ))%>%
        layout(separators= ".",
               xaxis = list(tickfont = list(size = 14)),
               yaxis = list(tickfont = list(size = 14))
        )
    }
    
    else if (input$heatmapvariable=='Current and capital account (Credit)') { 
      plot_ly(x=colnames(heat_cca_c), y=rownames(heat_cca_c),z = heat_cca_c_matrix,colors =colourPalette, type = "heatmap")%>%
        layout(yaxis = list(autorange = "reversed"))  %>%
        layout(
          xaxis = list(
            ticktext = list("1996", "1997", "1998", "1999", "2000", "2001","2002","2003","2004","2005","2006","2007","2008","2009","2010","2011","2012","2013","2014","2015","2016","2017","2018"),
            tickvals = list("1996", "1997", "1998", "1999", "2000", "2001","2002","2003","2004","2005","2006","2007","2008","2009","2010","2011","2012","2013","2014","2015","2016","2017","2018"),
            tickmode = "array"
          ))%>%
        layout(
          separators= ".",
          xaxis = list(tickfont = list(size = 14)),
          yaxis = list(tickfont = list(size = 14) )
        )
      
      
    }
    
    
    else if (input$heatmapvariable=='Goods (Exports)') { 
      plot_ly(x=colnames(heat_g_c), y=rownames(heat_g_c),z = heat_g_c_matrix,colors =colourPalette, type = "heatmap")%>%
        layout(yaxis = list(autorange = "reversed"))  %>%
        layout(
          xaxis = list(
            ticktext = list("1996", "1997", "1998", "1999", "2000", "2001","2002","2003","2004","2005","2006","2007","2008","2009","2010","2011","2012","2013","2014","2015","2016","2017","2018"),
            tickvals = list("1996", "1997", "1998", "1999", "2000", "2001","2002","2003","2004","2005","2006","2007","2008","2009","2010","2011","2012","2013","2014","2015","2016","2017","2018"),
            tickmode = "array"
          ))%>%
        layout(
          separators= ".",
          xaxis = list(tickfont = list(size = 14)),
          yaxis = list(tickfont = list(size = 14) )
        )
    }
    
    else if (input$heatmapvariable=='Services (Exports)') { 
      plot_ly(x=colnames(heat_s_c), y=rownames(heat_s_c),z = heat_s_c_matrix,colors =colourPalette, type = "heatmap")%>%
        layout(yaxis = list(autorange = "reversed"))  %>%
        layout(
          xaxis = list(
            ticktext = list("1996", "1997", "1998", "1999", "2000", "2001","2002","2003","2004","2005","2006","2007","2008","2009","2010","2011","2012","2013","2014","2015","2016","2017","2018"),
            tickvals = list("1996", "1997", "1998", "1999", "2000", "2001","2002","2003","2004","2005","2006","2007","2008","2009","2010","2011","2012","2013","2014","2015","2016","2017","2018"),
            tickmode = "array"
          ))%>%
        layout(
          separators= ".",
          xaxis = list(tickfont = list(size = 14)),
          yaxis = list(tickfont = list(size = 14) )
        )
    }
    
    
    else if (input$heatmapvariable=='Current and capital account (Debit)') { 
      plot_ly(x=colnames(heat_cca_d), y=rownames(heat_cca_d),z = heat_cca_d_matrix,colors =colourPalette, type = "heatmap")%>%
        layout(yaxis = list(autorange = "reversed"))  %>%
        layout(
          xaxis = list(
            ticktext = list("1996", "1997", "1998", "1999", "2000", "2001","2002","2003","2004","2005","2006","2007","2008","2009","2010","2011","2012","2013","2014","2015","2016","2017","2018"),
            tickvals = list("1996", "1997", "1998", "1999", "2000", "2001","2002","2003","2004","2005","2006","2007","2008","2009","2010","2011","2012","2013","2014","2015","2016","2017","2018"),
            tickmode = "array"
          ))%>%
        layout(
          separators= ".",
          xaxis = list(tickfont = list(size = 14)),
          yaxis = list(tickfont = list(size = 14) )
        )
      
      
    }
    
    else if (input$heatmapvariable=='Goods (Imports)') { 
      plot_ly(x=colnames(heat_g_d), y=rownames(heat_g_d),z = heat_g_d_matrix,colors =colourPalette, type = "heatmap") %>%
        layout(yaxis = list(autorange = "reversed"))  %>%
        layout(
          xaxis = list(
            ticktext = list("1996", "1997", "1998", "1999", "2000", "2001","2002","2003","2004","2005","2006","2007","2008","2009","2010","2011","2012","2013","2014","2015","2016","2017","2018"),
            tickvals = list("1996", "1997", "1998", "1999", "2000", "2001","2002","2003","2004","2005","2006","2007","2008","2009","2010","2011","2012","2013","2014","2015","2016","2017","2018"),
            tickmode = "array"
          ))%>%
        layout(
          separators= ".",
          xaxis = list(tickfont = list(size = 14)),
          yaxis = list(tickfont = list(size = 14) )
        )
    }
    
    else if (input$heatmapvariable=='Services (Imports)') { 
      plot_ly(x=colnames(heat_s_d), y=rownames(heat_s_d),z = heat_s_d_matrix,colors =colourPalette, type = "heatmap") %>%
        layout(yaxis = list(autorange = "reversed"))  %>%
        layout(
          xaxis = list(
            ticktext = list("1996", "1997", "1998", "1999", "2000", "2001","2002","2003","2004","2005","2006","2007","2008","2009","2010","2011","2012","2013","2014","2015","2016","2017","2018"),
            tickvals = list("1996", "1997", "1998", "1999", "2000", "2001","2002","2003","2004","2005","2006","2007","2008","2009","2010","2011","2012","2013","2014","2015","2016","2017","2018"),
            tickmode = "array"
          ))%>%
        layout(
          separators= ".",
          xaxis = list(tickfont = list(size = 14)),
          yaxis = list(tickfont = list(size = 14) )
        )
    }
    
    
    else if (input$heatmapvariable=='Goods (Balance)') { 
      plot_ly(x=colnames(heat_g_n), y=rownames(heat_g_n),z = heat_g_n_matrix,colors =colourPalette, type = "heatmap") %>%
        layout(yaxis = list(autorange = "reversed"))  %>%
        layout(
          xaxis = list(
            ticktext = list("1996", "1997", "1998", "1999", "2000", "2001","2002","2003","2004","2005","2006","2007","2008","2009","2010","2011","2012","2013","2014","2015","2016","2017","2018"),
            tickvals = list("1996", "1997", "1998", "1999", "2000", "2001","2002","2003","2004","2005","2006","2007","2008","2009","2010","2011","2012","2013","2014","2015","2016","2017","2018"),
            tickmode = "array"
          ))%>%
        layout(
          separators= ".",
          xaxis = list(tickfont = list(size = 14)),
          yaxis = list(tickfont = list(size = 14) )
        )
    }
    
    else if (input$heatmapvariable=='Services (Balance)') { 
      plot_ly(x=colnames(heat_s_n), y=rownames(heat_s_n),z = heat_s_n_matrix,colors =colourPalette, type = "heatmap") %>%
        layout(yaxis = list(autorange = "reversed"))  %>%
        layout(
          xaxis = list(
            ticktext = list("1996", "1997", "1998", "1999", "2000", "2001","2002","2003","2004","2005","2006","2007","2008","2009","2010","2011","2012","2013","2014","2015","2016","2017","2018"),
            tickvals = list("1996", "1997", "1998", "1999", "2000", "2001","2002","2003","2004","2005","2006","2007","2008","2009","2010","2011","2012","2013","2014","2015","2016","2017","2018"),
            tickmode = "array"
          ))%>%
        layout(
          separators= ".",
          xaxis = list(tickfont = list(size = 14)),
          yaxis = list(tickfont = list(size = 14) 
          )
          
        )
    }
    
    
  }) 
  
  
  observeEvent(input$reset3V, {
    reset("heatmapvariable")
  })
  
  
  output$bubles <-   renderPlotly({
    
    my_colors <- c("#808080","#ff0000","#ffc000","#ffff00","#92d050","#00b050","#00b0f0","#FF69B4","#002060","#7030a0","#3399ff","#ff0066")
    other_colors <- c("#808080","#ff0000","#ffc000","#ffff00","#92d050","#00b050","#00b0f0","#FF69B4","#002060","#7030a0")
    
    
    
    if((input$bubledates=="1996")&(input$rd_services=="All types of services")){
      ggplot(services_1996, aes(x=Exports, y=Imports, size=Weight,fill= Service)) +
        geom_point(alpha=0.8,stroke=0) +
        scale_fill_manual(values=my_colors)+
        # theme(legend.position="none")+
        #  geom_label(label=services_1996$Item, nudge_x=0.25, nudge_y =0.25,size=5) +
        geom_abline(intercept = 0,color="#8E0000")+
        xlim(0,17000)+
        ylim(0,5000)+
        scale_size(range = c(3, 12))+
        theme(axis.text.x= element_text(family='Arial',size=11,color='#505050')) +
        theme(axis.text.y= element_text(family='Arial',size=11,color='#505050')) +
        ggtitle(paste("Year",input$bubledates,sep=" "))+
        theme(legend.title=element_blank())+
        geom_point(data=services_1996_TT, color="black",stroke = 1.5)
      
      
      
    }
    
    
    else if((input$bubledates=="1996")&(input$rd_services=="All services except travel and transport")){
      ggplot(services_1996_WT, aes(x=Exports, y=Imports, size=Weight,fill= Service)) +
        geom_point(alpha=0.8,stroke=0) +
        scale_fill_manual(values=other_colors)+
        #  geom_label(label=services_1996$Item, nudge_x=0.25, nudge_y =0.25,size=5) +
        geom_abline(intercept = 0,color="#8E0000")+
        xlim(0,5000)+
        ylim(0,4000)+
        scale_size(range = c(3, 12))+
        theme(axis.text.x= element_text(family='Arial',size=11,color='#505050')) +
        theme(axis.text.y= element_text(family='Arial',size=11,color='#505050')) +
        ggtitle(paste("Year",input$bubledates,sep=" ")) +
        theme(legend.title=element_blank())
      
    }
    
    else if((input$bubledates=="1997")&(input$rd_services=="All types of services")){
      ggplot(services_1997, aes(x=Exports, y=Imports, size=Weight,fill= Service)) +
        geom_point(alpha=0.8,stroke=0)+
        scale_fill_manual(values=my_colors)+
        #  theme(legend.position="none")+
        geom_abline(intercept = 0,color="#8E0000") +
        xlim(0,17000)+
        ylim(0,5000)+
        scale_size(range = c(3, 12))+
        theme(axis.text.x= element_text(family='Arial',size=11,color='#505050')) +
        theme(axis.text.y= element_text(family='Arial',size=11,color='#505050')) +
        ggtitle(paste("Year",input$bubledates,sep=" "))+
        theme(legend.title=element_blank())+
        geom_point(data=services_1997_TT, color="black",stroke = 1.5)
      
    }
    else if((input$bubledates=="1997")&(input$rd_services=="All services except travel and transport")){
      ggplot(services_1997_WT, aes(x=Exports, y=Imports, size=Weight,fill= Service)) +
        geom_point(alpha=0.8,stroke=0) +
        scale_fill_manual(values=other_colors)+
        #  geom_label(label=services_1996$Item, nudge_x=0.25, nudge_y =0.25,size=5) +
        geom_abline(intercept = 0,color="#8E0000")+
        
        xlim(0,5000)+
        ylim(0,4000)+
        scale_size(range = c(3, 12))+
        theme(axis.text.x= element_text(family='Arial',size=11,color='#505050')) +
        theme(axis.text.y= element_text(family='Arial',size=11,color='#505050')) +
        ggtitle(paste("Year",input$bubledates,sep=" "))+
        theme(legend.title=element_blank())
      
    } 
    
    else if((input$bubledates=="1998")&(input$rd_services=="All types of services")){
      ggplot(services_1998, aes(x=Exports, y=Imports, size=Weight,fill= Service)) +
        geom_point(alpha=0.8,stroke=0)+
        geom_abline(intercept = 0,color="#8E0000") +
        scale_fill_manual(values=my_colors)+
        # theme(legend.position="none")+
        xlim(0,17000)+
        ylim(0,5000)+
        scale_size(range = c(3, 12))+
        theme(axis.text.x= element_text(family='Arial',size=11,color='#505050')) +
        theme(axis.text.y= element_text(family='Arial',size=11,color='#505050')) +
        ggtitle(paste("Year",input$bubledates,sep=" "))+
        theme(legend.title=element_blank())+
        geom_point(data=services_1998_TT, color="black",stroke = 1.5)
      
    }
    
    else if((input$bubledates=="1998")&(input$rd_services=="All services except travel and transport")){
      ggplot(services_1998_WT, aes(x=Exports, y=Imports, size=Weight,fill= Service)) +
        geom_point(alpha=0.8,stroke=0) +
        scale_fill_manual(values=other_colors)+
        #  geom_label(label=services_1996$Item, nudge_x=0.25, nudge_y =0.25,size=5) +
        geom_abline(intercept = 0,color="#8E0000")+
        xlim(0,5000)+
        ylim(0,4000)+
        scale_size(range = c(3, 12))+
        theme(axis.text.x= element_text(family='Arial',size=11,color='#505050')) +
        theme(axis.text.y= element_text(family='Arial',size=11,color='#505050')) +
        ggtitle(paste("Year",input$bubledates,sep=" "))+
        theme(legend.title=element_blank())
      
    }
    else if((input$bubledates=="1999")&(input$rd_services=="All types of services")){
      ggplot(services_1999, aes(x=Exports, y=Imports, size=Weight,fill= Service)) +
        geom_point(alpha=0.8,stroke=0)+
        geom_abline(intercept = 0,color="#8E0000") +
        scale_fill_manual(values=my_colors)+
        #  theme(legend.position="none")+
        xlim(0,17000)+
        ylim(0,5000)+
        scale_size(range = c(3, 12))+
        theme(axis.text.x= element_text(family='Arial',size=11,color='#505050')) +
        theme(axis.text.y= element_text(family='Arial',size=11,color='#505050')) +
        ggtitle(paste("Year",input$bubledates,sep=" "))+
        theme(legend.title=element_blank())+
        geom_point(data=services_1999_TT, color="black",stroke = 1.5)
      
    }
    
    else if((input$bubledates=="1999")&(input$rd_services=="All services except travel and transport")){
      ggplot(services_1999_WT, aes(x=Exports, y=Imports, size=Weight,fill= Service)) +
        geom_point(alpha=0.8,stroke=0) +
        scale_fill_manual(values=other_colors)+
        #  geom_label(label=services_1996$Item, nudge_x=0.25, nudge_y =0.25,size=5) +
        geom_abline(intercept = 0,color="#8E0000")+
        xlim(0,5000)+
        ylim(0,4000)+
        scale_size(range = c(3, 12))+
        theme(axis.text.x= element_text(family='Arial',size=11,color='#505050')) +
        theme(axis.text.y= element_text(family='Arial',size=11,color='#505050')) +
        ggtitle(paste("Year",input$bubledates,sep=" "))+
        theme(legend.title=element_blank())
      
    }
    else if((input$bubledates=="2000")&(input$rd_services=="All types of services")){
      ggplot(services_2000, aes(x=Exports, y=Imports, size=Weight,fill= Service)) +
        geom_point(alpha=0.8,stroke=0)+
        geom_abline(intercept = 0,color="#8E0000")+
        scale_fill_manual(values=my_colors)+
        # theme(legend.position="none")+
        xlim(0,17000)+
        ylim(0,5000)+
        scale_size(range = c(3, 12))+
        theme(axis.text.x= element_text(family='Arial',size=11,color='#505050')) +
        theme(axis.text.y= element_text(family='Arial',size=11,color='#505050')) +
        ggtitle(paste("Year",input$bubledates,sep=" "))+
        theme(legend.title=element_blank())+
        geom_point(data=services_2000_TT, color="black",stroke = 1.5)
      
    }
    
    else if((input$bubledates=="2000")&(input$rd_services=="All services except travel and transport")){
      ggplot(services_2000_WT, aes(x=Exports, y=Imports, size=Weight,fill= Service)) +
        geom_point(alpha=0.8,stroke=0) +
        scale_fill_manual(values=other_colors)+
        #  geom_label(label=services_1996$Item, nudge_x=0.25, nudge_y =0.25,size=5) +
        geom_abline(intercept = 0,color="#8E0000")+
        xlim(0,5000)+
        ylim(0,4000)+
        scale_size(range = c(3, 12))+
        theme(axis.text.x= element_text(family='Arial',size=11,color='#505050')) +
        theme(axis.text.y= element_text(family='Arial',size=11,color='#505050')) +
        ggtitle(paste("Year",input$bubledates,sep=" "))+
        theme(legend.title=element_blank())
      
    }
    else if((input$bubledates=="2001")&(input$rd_services=="All types of services")){
      ggplot(services_2001, aes(x=Exports, y=Imports, size=Weight,fill= Service)) +
        geom_point(alpha=0.8,stroke=0)+
        scale_fill_manual(values=my_colors)+
        #theme(legend.position="none")+
        geom_abline(intercept = 0,color="#8E0000")  +
        xlim(0,17000)+
        ylim(0,5000)+
        scale_size(range = c(3, 12))+
        theme(axis.text.x= element_text(family='Arial',size=11,color='#505050')) +
        theme(axis.text.y= element_text(family='Arial',size=11,color='#505050')) +
        ggtitle(paste("Year",input$bubledates,sep=" "))+
        theme(legend.title=element_blank())+
        geom_point(data=services_2001_TT, color="black",stroke = 1.5)
      
    }
    else if((input$bubledates=="2001")&(input$rd_services=="All services except travel and transport")){
      ggplot(services_2001_WT, aes(x=Exports, y=Imports, size=Weight,fill= Service)) +
        geom_point(alpha=0.8,stroke=0) +
        scale_fill_manual(values=other_colors)+
        #  geom_label(label=services_1996$Item, nudge_x=0.25, nudge_y =0.25,size=5) +
        geom_abline(intercept = 0,color="#8E0000")+
        xlim(0,5000)+
        ylim(0,4000)+
        scale_size(range = c(3, 12))+
        theme(axis.text.x= element_text(family='Arial',size=11,color='#505050')) +
        theme(axis.text.y= element_text(family='Arial',size=11,color='#505050')) +
        ggtitle(paste("Year",input$bubledates,sep=" "))+
        theme(legend.title=element_blank())
      
    }
    else if((input$bubledates=="2002")&(input$rd_services=="All types of services")){
      ggplot(services_2002, aes(x=Exports, y=Imports, size=Weight,fill= Service)) +
        geom_point(alpha=0.8,stroke=0)+
        scale_fill_manual(values=my_colors)+
        #  theme(legend.position="none")+
        geom_abline(intercept = 0,color="#8E0000")+
        xlim(0,17000)+
        ylim(0,5000)+
        scale_size(range = c(3, 12))+
        theme(axis.text.x= element_text(family='Arial',size=11,color='#505050')) +
        theme(axis.text.y= element_text(family='Arial',size=11,color='#505050')) +
        ggtitle(paste("Year",input$bubledates,sep=" "))+
        theme(legend.title=element_blank())+
        geom_point(data=services_2002_TT, color="black",stroke = 1.5)
      
    }
    else if((input$bubledates=="2002")&(input$rd_services=="All services except travel and transport")){
      ggplot(services_2002_WT, aes(x=Exports, y=Imports, size=Weight,fill= Service)) +
        geom_point(alpha=0.8,stroke=0) +
        scale_fill_manual(values=other_colors)+
        #  geom_label(label=services_1996$Item, nudge_x=0.25, nudge_y =0.25,size=5) +
        geom_abline(intercept = 0,color="#8E0000")+
        xlim(0,5000)+
        ylim(0,4000)+
        scale_size(range = c(3, 12))+
        theme(axis.text.x= element_text(family='Arial',size=11,color='#505050')) +
        theme(axis.text.y= element_text(family='Arial',size=11,color='#505050')) +
        ggtitle(paste("Year",input$bubledates,sep=" "))+
        theme(legend.title=element_blank())
      
    }
    else if((input$bubledates=="2003")&(input$rd_services=="All types of services")){
      ggplot(services_2003, aes(x=Exports, y=Imports, size=Weight,fill= Service)) +
        geom_point(alpha=0.8,stroke=0)+
        scale_fill_manual(values=my_colors)+
        # theme(legend.position="none")+
        geom_abline(intercept = 0,color="#8E0000")+
        xlim(0,17000)+
        ylim(0,5000)+
        scale_size(range = c(3, 12))+
        theme(axis.text.x= element_text(family='Arial',size=11,color='#505050')) +
        theme(axis.text.y= element_text(family='Arial',size=11,color='#505050')) +
        ggtitle(paste("Year",input$bubledates,sep=" "))+
        theme(legend.title=element_blank())+
        geom_point(data=services_2003_TT, color="black",stroke = 1.5)
      
    }
    else if((input$bubledates=="2003")&(input$rd_services=="All services except travel and transport")){
      ggplot(services_2003_WT, aes(x=Exports, y=Imports, size=Weight,fill= Service)) +
        geom_point(alpha=0.8,stroke=0) +
        scale_fill_manual(values=other_colors)+
        #  geom_label(label=services_1996$Item, nudge_x=0.25, nudge_y =0.25,size=5) +
        geom_abline(intercept = 0,color="#8E0000")+
        xlim(0,5000)+
        ylim(0,4000)+
        scale_size(range = c(3, 12))+
        theme(axis.text.x= element_text(family='Arial',size=11,color='#505050')) +
        theme(axis.text.y= element_text(family='Arial',size=11,color='#505050')) +
        ggtitle(paste("Year",input$bubledates,sep=" "))+
        theme(legend.title=element_blank())
      
    }
    else if((input$bubledates=="2004")&(input$rd_services=="All types of services")){
      ggplot(services_2004, aes(x=Exports, y=Imports, size=Weight,fill= Service)) +
        geom_point(alpha=0.8,stroke=0)+
        scale_fill_manual(values=my_colors)+
        #theme(legend.position="none")+
        geom_abline(intercept = 0,color="#8E0000")+
        xlim(0,17000)+
        ylim(0,5000)+
        scale_size(range = c(3, 12))+
        theme(axis.text.x= element_text(family='Arial',size=11,color='#505050')) +
        theme(axis.text.y= element_text(family='Arial',size=11,color='#505050')) +
        ggtitle(paste("Year",input$bubledates,sep=" "))+
        theme(legend.title=element_blank())+
        geom_point(data=services_2004_TT, color="black",stroke = 1.5)
      
    }
    else if((input$bubledates=="2004")&(input$rd_services=="All services except travel and transport")){
      ggplot(services_2004_WT, aes(x=Exports, y=Imports, size=Weight,fill= Service)) +
        geom_point(alpha=0.8,stroke=0) +
        scale_fill_manual(values=other_colors)+
        #  geom_label(label=services_1996$Item, nudge_x=0.25, nudge_y =0.25,size=5) +
        geom_abline(intercept = 0,color="#8E0000")+
        xlim(0,5000)+
        ylim(0,4000)+
        scale_size(range = c(3, 12))+
        theme(axis.text.x= element_text(family='Arial',size=11,color='#505050')) +
        theme(axis.text.y= element_text(family='Arial',size=11,color='#505050')) +
        ggtitle(paste("Year",input$bubledates,sep=" "))+
        theme(legend.title=element_blank())
      
    }
    else if((input$bubledates=="2005")&(input$rd_services=="All types of services")){
      ggplot(services_2005, aes(x=Exports, y=Imports, size=Weight,fill= Service)) +
        geom_point(alpha=0.8,stroke=0)+
        scale_fill_manual(values=my_colors)+
        #theme(legend.position="none")+
        geom_abline(intercept = 0,color="#8E0000")+
        xlim(0,17000)+
        ylim(0,5000)+
        scale_size(range = c(3, 12))+
        theme(axis.text.x= element_text(family='Arial',size=11,color='#505050')) +
        theme(axis.text.y= element_text(family='Arial',size=11,color='#505050')) +
        ggtitle(paste("Year",input$bubledates,sep=" "))+
        theme(legend.title=element_blank())+
        geom_point(data=services_2005_TT, color="black",stroke = 1.5)
      
    }
    else if((input$bubledates=="2005")&(input$rd_services=="All services except travel and transport")){
      ggplot(services_2005_WT, aes(x=Exports, y=Imports, size=Weight,fill= Service)) +
        geom_point(alpha=0.8,stroke=0) +
        scale_fill_manual(values=other_colors)+
        #  geom_label(label=services_1996$Item, nudge_x=0.25, nudge_y =0.25,size=5) +
        geom_abline(intercept = 0,color="#8E0000")+
        xlim(0,5000)+
        ylim(0,4000)+
        scale_size(range = c(3, 12))+
        theme(axis.text.x= element_text(family='Arial',size=11,color='#505050')) +
        theme(axis.text.y= element_text(family='Arial',size=11,color='#505050')) +
        ggtitle(paste("Year",input$bubledates,sep=" "))+
        theme(legend.title=element_blank())
      
    }
    else if((input$bubledates=="2006")&(input$rd_services=="All types of services")){
      ggplot(services_2006, aes(x=Exports, y=Imports, size=Weight,fill= Service)) +
        geom_point(alpha=0.8,stroke=0)+
        scale_fill_manual(values=my_colors)+
        #theme(legend.position="none")+
        geom_abline(intercept = 0,color="#8E0000")+
        xlim(0,17000)+
        ylim(0,5000)+
        scale_size(range = c(3, 12))+
        theme(axis.text.x= element_text(family='Arial',size=11,color='#505050')) +
        theme(axis.text.y= element_text(family='Arial',size=11,color='#505050')) +
        ggtitle(paste("Year",input$bubledates,sep=" "))+
        theme(legend.title=element_blank())+
        geom_point(data=services_2006_TT, color="black",stroke = 1.5)
      
    }
    
    else if((input$bubledates=="2006")&(input$rd_services=="All services except travel and transport")){
      ggplot(services_2006_WT, aes(x=Exports, y=Imports, size=Weight,fill= Service)) +
        geom_point(alpha=0.8,stroke=0) +
        scale_fill_manual(values=other_colors)+
        #  geom_label(label=services_1996$Item, nudge_x=0.25, nudge_y =0.25,size=5) +
        geom_abline(intercept = 0,color="#8E0000")+
        xlim(0,5000)+
        ylim(0,4000)+
        scale_size(range = c(3, 12))+
        theme(axis.text.x= element_text(family='Arial',size=11,color='#505050')) +
        theme(axis.text.y= element_text(family='Arial',size=11,color='#505050')) +
        ggtitle(paste("Year",input$bubledates,sep=" "))+
        theme(legend.title=element_blank())
      
    }
    else if((input$bubledates=="2007")&(input$rd_services=="All types of services")){
      ggplot(services_2007, aes(x=Exports, y=Imports, size=Weight,fill= Service)) +
        geom_point(alpha=0.8,stroke=0)+
        scale_fill_manual(values=my_colors)+
        #theme(legend.position="none")+
        geom_abline(intercept = 0,color="#8E0000")+
        xlim(0,17000)+
        ylim(0,5000)+
        scale_size(range = c(3, 12))+
        theme(axis.text.x= element_text(family='Arial',size=11,color='#505050')) +
        theme(axis.text.y= element_text(family='Arial',size=11,color='#505050')) +
        ggtitle(paste("Year",input$bubledates,sep=" "))+
        theme(legend.title=element_blank())+
        geom_point(data=services_2007_TT, color="black",stroke = 1.5)
      
    }
    else if((input$bubledates=="2007")&(input$rd_services=="All services except travel and transport")){
      ggplot(services_2007_WT, aes(x=Exports, y=Imports, size=Weight,fill= Service)) +
        geom_point(alpha=0.8,stroke=0) +
        scale_fill_manual(values=other_colors)+
        #  geom_label(label=services_1996$Item, nudge_x=0.25, nudge_y =0.25,size=5) +
        geom_abline(intercept = 0,color="#8E0000")+
        xlim(0,5000)+
        ylim(0,4000)+
        scale_size(range = c(3, 12))+
        theme(axis.text.x= element_text(family='Arial',size=11,color='#505050')) +
        theme(axis.text.y= element_text(family='Arial',size=11,color='#505050')) +
        ggtitle(paste("Year",input$bubledates,sep=" "))+
        theme(legend.title=element_blank())
      
    }
    else if((input$bubledates=="2008")&(input$rd_services=="All types of services")){
      ggplot(services_2008, aes(x=Exports, y=Imports, size=Weight,fill= Service)) +
        geom_point(alpha=0.8,stroke=0)+
        scale_fill_manual(values=my_colors)+
        #theme(legend.position="none")+
        geom_abline(intercept = 0,color="#8E0000")+
        xlim(0,17000)+
        ylim(0,5000)+
        scale_size(range = c(3, 12))+
        theme(axis.text.x= element_text(family='Arial',size=11,color='#505050')) +
        theme(axis.text.y= element_text(family='Arial',size=11,color='#505050')) +
        ggtitle(paste("Year",input$bubledates,sep=" "))+
        theme(legend.title=element_blank())+
        geom_point(data=services_2008_TT, color="black",stroke = 1.5)
      
    }
    else if((input$bubledates=="2008")&(input$rd_services=="All services except travel and transport")){
      ggplot(services_2008_WT, aes(x=Exports, y=Imports, size=Weight,fill= Service)) +
        geom_point(alpha=0.8,stroke=0) +
        scale_fill_manual(values=other_colors)+
        #  geom_label(label=services_1996$Item, nudge_x=0.25, nudge_y =0.25,size=5) +
        geom_abline(intercept = 0,color="#8E0000")+
        xlim(0,5000)+
        ylim(0,4000)+
        scale_size(range = c(3, 12))+
        theme(axis.text.x= element_text(family='Arial',size=11,color='#505050')) +
        theme(axis.text.y= element_text(family='Arial',size=11,color='#505050')) +
        ggtitle(paste("Year",input$bubledates,sep=" "))+
        theme(legend.title=element_blank())
      
    }
    else if((input$bubledates=="2009")&(input$rd_services=="All types of services")){
      ggplot(services_2009, aes(x=Exports, y=Imports, size=Weight,fill= Service)) +
        geom_point(alpha=0.8,stroke=0)+
        scale_fill_manual(values=my_colors)+
        #theme(legend.position="none")+
        geom_abline(intercept = 0,color="#8E0000")+
        xlim(0,17000)+
        ylim(0,5000)+
        scale_size(range = c(3, 12))+
        theme(axis.text.x= element_text(family='Arial',size=11,color='#505050')) +
        theme(axis.text.y= element_text(family='Arial',size=11,color='#505050')) +
        ggtitle(paste("Year",input$bubledates,sep=" "))+
        theme(legend.title=element_blank())+
        geom_point(data=services_2009_TT, color="black",stroke = 1.5)
      
    }
    else if((input$bubledates=="2009")&(input$rd_services=="All services except travel and transport")){
      ggplot(services_2009_WT, aes(x=Exports, y=Imports, size=Weight,fill= Service)) +
        geom_point(alpha=0.8,stroke=0) +
        scale_fill_manual(values=other_colors)+
        #  geom_label(label=services_1996$Item, nudge_x=0.25, nudge_y =0.25,size=5) +
        geom_abline(intercept = 0,color="#8E0000")+
        xlim(0,5000)+
        ylim(0,4000)+
        scale_size(range = c(3, 12))+
        theme(axis.text.x= element_text(family='Arial',size=11,color='#505050')) +
        theme(axis.text.y= element_text(family='Arial',size=11,color='#505050')) +
        ggtitle(paste("Year",input$bubledates,sep=" "))+
        theme(legend.title=element_blank())
      
    }
    else if((input$bubledates=="2010")&(input$rd_services=="All types of services")){
      ggplot(services_2010, aes(x=Exports, y=Imports, size=Weight,fill= Service)) +
        geom_point(alpha=0.8,stroke=0)+
        scale_fill_manual(values=my_colors)+
        #theme(legend.position="none")+
        geom_abline(intercept = 0,color="#8E0000")+
        xlim(0,17000)+
        ylim(0,5000)+
        scale_size(range = c(3, 12))+
        theme(axis.text.x= element_text(family='Arial',size=11,color='#505050')) +
        theme(axis.text.y= element_text(family='Arial',size=11,color='#505050')) +
        ggtitle(paste("Year",input$bubledates,sep=" "))+
        theme(legend.title=element_blank())+
        geom_point(data=services_2010_TT, color="black",stroke = 1.5)
      
    }
    else if((input$bubledates=="2010")&(input$rd_services=="All services except travel and transport")){
      ggplot(services_2010_WT, aes(x=Exports, y=Imports, size=Weight,fill= Service)) +
        geom_point(alpha=0.8,stroke=0) +
        scale_fill_manual(values=other_colors)+
        #  geom_label(label=services_1996$Item, nudge_x=0.25, nudge_y =0.25,size=5) +
        geom_abline(intercept = 0,color="#8E0000")+
        xlim(0,5000)+
        ylim(0,4000)+
        scale_size(range = c(3, 12))+
        theme(axis.text.x= element_text(family='Arial',size=11,color='#505050')) +
        theme(axis.text.y= element_text(family='Arial',size=11,color='#505050')) +
        ggtitle(paste("Year",input$bubledates,sep=" "))+
        theme(legend.title=element_blank())
      
    }
    else if((input$bubledates=="2011")&(input$rd_services=="All types of services")){
      ggplot(services_2011, aes(x=Exports, y=Imports, size=Weight,fill= Service)) +
        geom_point(alpha=0.8,stroke=0)+
        scale_fill_manual(values=my_colors)+
        #theme(legend.position="none")+
        geom_abline(intercept = 0,color="#8E0000")+
        xlim(0,17000)+
        ylim(0,5000)+
        scale_size(range = c(3, 12))+
        theme(axis.text.x= element_text(family='Arial',size=11,color='#505050')) +
        theme(axis.text.y= element_text(family='Arial',size=11,color='#505050')) +
        ggtitle(paste("Year",input$bubledates,sep=" "))+
        theme(legend.title=element_blank())+
        geom_point(data=services_2011_TT, color="black",stroke = 1.5)
      
    }
    else if((input$bubledates=="2011")&(input$rd_services=="All services except travel and transport")){
      ggplot(services_2011_WT, aes(x=Exports, y=Imports, size=Weight,fill= Service)) +
        geom_point(alpha=0.8,stroke=0) +
        scale_fill_manual(values=other_colors)+
        #  geom_label(label=services_1996$Item, nudge_x=0.25, nudge_y =0.25,size=5) +
        geom_abline(intercept = 0,color="#8E0000")+
        xlim(0,5000)+
        ylim(0,4000)+
        scale_size(range = c(3, 12))+
        theme(axis.text.x= element_text(family='Arial',size=11,color='#505050')) +
        theme(axis.text.y= element_text(family='Arial',size=11,color='#505050')) +
        ggtitle(paste("Year",input$bubledates,sep=" "))+
        theme(legend.title=element_blank())
      
    }
    else if((input$bubledates=="2012")&(input$rd_services=="All types of services")){
      ggplot(services_2012, aes(x=Exports, y=Imports, size=Weight,fill= Service)) +
        geom_point(alpha=0.8,stroke=0)+
        scale_fill_manual(values=my_colors)+
        #theme(legend.position="none")+
        geom_abline(intercept = 0,color="#8E0000")+
        xlim(0,17000)+
        ylim(0,5000)+
        scale_size(range = c(3, 12))+
        theme(axis.text.x= element_text(family='Arial',size=11,color='#505050')) +
        theme(axis.text.y= element_text(family='Arial',size=11,color='#505050')) +
        ggtitle(paste("Year",input$bubledates,sep=" "))+
        theme(legend.title=element_blank())+
        geom_point(data=services_2012_TT, color="black",stroke = 1.5)
      
    }
    else if((input$bubledates=="2012")&(input$rd_services=="All services except travel and transport")){
      ggplot(services_2012_WT, aes(x=Exports, y=Imports, size=Weight,fill= Service)) +
        geom_point(alpha=0.8,stroke=0) +
        scale_fill_manual(values=other_colors)+
        #  geom_label(label=services_1996$Item, nudge_x=0.25, nudge_y =0.25,size=5) +
        geom_abline(intercept = 0,color="#8E0000")+
        xlim(0,5000)+
        ylim(0,4000)+
        scale_size(range = c(3, 12))+
        theme(axis.text.x= element_text(family='Arial',size=11,color='#505050')) +
        theme(axis.text.y= element_text(family='Arial',size=11,color='#505050')) +
        ggtitle(paste("Year",input$bubledates,sep=" "))+
        theme(legend.title=element_blank())
      
    }
    else if((input$bubledates=="2013")&(input$rd_services=="All types of services")){
      ggplot(services_2013, aes(x=Exports, y=Imports, size=Weight,fill= Service)) +
        geom_point(alpha=0.8,stroke=0)+
        scale_fill_manual(values=my_colors)+
        #theme(legend.position="none")+
        geom_abline(intercept = 0,color="#8E0000")+
        xlim(0,17000)+
        ylim(0,5000)+
        scale_size(range = c(3, 12))+
        theme(axis.text.x= element_text(family='Arial',size=11,color='#505050')) +
        theme(axis.text.y= element_text(family='Arial',size=11,color='#505050')) +
        ggtitle(paste("Year",input$bubledates,sep=" "))+
        theme(legend.title=element_blank())+
        geom_point(data=services_2013_TT, color="black",stroke = 1.5)
      
    }
    else if((input$bubledates=="2013")&(input$rd_services=="All services except travel and transport")){
      ggplot(services_2013_WT, aes(x=Exports, y=Imports, size=Weight,fill= Service)) +
        geom_point(alpha=0.8,stroke=0) +
        scale_fill_manual(values=other_colors)+
        #  geom_label(label=services_1996$Item, nudge_x=0.25, nudge_y =0.25,size=5) +
        geom_abline(intercept = 0,color="#8E0000")+
        xlim(0,5000)+
        ylim(0,4000)+
        scale_size(range = c(3, 12))+
        theme(axis.text.x= element_text(family='Arial',size=11,color='#505050')) +
        theme(axis.text.y= element_text(family='Arial',size=11,color='#505050')) +
        ggtitle(paste("Year",input$bubledates,sep=" "))+
        theme(legend.title=element_blank())
      
    }
    else if((input$bubledates=="2014")&(input$rd_services=="All types of services")){
      ggplot(services_2014, aes(x=Exports, y=Imports, size=Weight,fill= Service)) +
        geom_point(alpha=0.8,stroke=0)+
        scale_fill_manual(values=my_colors)+
        #theme(legend.position="none")+
        geom_abline(intercept = 0,color="#8E0000")+
        xlim(0,17000)+
        ylim(0,5000)+
        scale_size(range = c(3, 12))+
        theme(axis.text.x= element_text(family='Arial',size=11,color='#505050')) +
        theme(axis.text.y= element_text(family='Arial',size=11,color='#505050')) +
        ggtitle(paste("Year",input$bubledates,sep=" "))+
        theme(legend.title=element_blank())+
        geom_point(data=services_2014_TT, color="black",stroke = 1.5)
      
    }
    else if((input$bubledates=="2014")&(input$rd_services=="All services except travel and transport")){
      ggplot(services_2014_WT, aes(x=Exports, y=Imports, size=Weight,fill= Service)) +
        geom_point(alpha=0.8,stroke=0) +
        scale_fill_manual(values=other_colors)+
        #  geom_label(label=services_1996$Item, nudge_x=0.25, nudge_y =0.25,size=5) +
        geom_abline(intercept = 0,color="#8E0000")+
        xlim(0,5000)+
        ylim(0,4000)+
        scale_size(range = c(3, 12))+
        theme(axis.text.x= element_text(family='Arial',size=11,color='#505050')) +
        theme(axis.text.y= element_text(family='Arial',size=11,color='#505050')) +
        ggtitle(paste("Year",input$bubledates,sep=" "))+
        theme(legend.title=element_blank())
      
    }
    else if((input$bubledates=="2015")&(input$rd_services=="All types of services")){
      ggplot(services_2015, aes(x=Exports, y=Imports, size=Weight,fill= Service)) +
        geom_point(alpha=0.8,stroke=0)+
        scale_fill_manual(values=my_colors)+
        #theme(legend.position="none")+
        geom_abline(intercept = 0,color="#8E0000")+
        xlim(0,17000)+
        ylim(0,5000)+
        scale_size(range = c(3, 12))+
        theme(axis.text.x= element_text(family='Arial',size=11,color='#505050')) +
        theme(axis.text.y= element_text(family='Arial',size=11,color='#505050')) +
        ggtitle(paste("Year",input$bubledates,sep=" "))+
        theme(legend.title=element_blank())+
        geom_point(data=services_2015_TT, color="black",stroke = 1.5)
      
    }
    else if((input$bubledates=="2015")&(input$rd_services=="All services except travel and transport")){
      ggplot(services_2015_WT, aes(x=Exports, y=Imports, size=Weight,fill= Service)) +
        geom_point(alpha=0.8,stroke=0) +
        scale_fill_manual(values=other_colors)+
        #  geom_label(label=services_1996$Item, nudge_x=0.25, nudge_y =0.25,size=5) +
        geom_abline(intercept = 0,color="#8E0000")+
        xlim(0,5000)+
        ylim(0,4000)+
        scale_size(range = c(3, 12))+
        theme(axis.text.x= element_text(family='Arial',size=11,color='#505050')) +
        theme(axis.text.y= element_text(family='Arial',size=11,color='#505050')) +
        ggtitle(paste("Year",input$bubledates,sep=" "))+
        theme(legend.title=element_blank())
      
    }
    else if((input$bubledates=="2016")&(input$rd_services=="All types of services")){
      ggplot(services_2016, aes(x=Exports, y=Imports, size=Weight,fill= Service)) +
        geom_point(alpha=0.8,stroke=0)+
        scale_fill_manual(values=my_colors)+
        #theme(legend.position="none")+
        geom_abline(intercept = 0,color="#8E0000")+
        xlim(0,17000)+
        ylim(0,5000)+
        scale_size(range = c(3, 12))+
        theme(axis.text.x= element_text(family='Arial',size=11,color='#505050')) +
        theme(axis.text.y= element_text(family='Arial',size=11,color='#505050')) +
        ggtitle(paste("Year",input$bubledates,sep=" "))+
        theme(legend.title=element_blank())+
        geom_point(data=services_2016_TT, color="black",stroke = 1.5)
      
    }
    else if((input$bubledates=="2016")&(input$rd_services=="All services except travel and transport")){
      ggplot(services_2016_WT, aes(x=Exports, y=Imports, size=Weight,fill= Service)) +
        geom_point(alpha=0.8,stroke=0) +
        scale_fill_manual(values=other_colors)+
        #  geom_label(label=services_1996$Item, nudge_x=0.25, nudge_y =0.25,size=5) +
        geom_abline(intercept = 0,color="#8E0000")+
        xlim(0,5000)+
        ylim(0,4000)+
        scale_size(range = c(3, 12))+
        theme(axis.text.x= element_text(family='Arial',size=11,color='#505050')) +
        theme(axis.text.y= element_text(family='Arial',size=11,color='#505050')) +
        ggtitle(paste("Year",input$bubledates,sep=" "))+
        theme(legend.title=element_blank())
      
    }
    else if((input$bubledates=="2017")&(input$rd_services=="All types of services")){
      ggplot(services_2017, aes(x=Exports, y=Imports, size=Weight,fill= Service)) +
        geom_point(alpha=0.8,stroke=0)+
        scale_fill_manual(values=my_colors)+
        #theme(legend.position="none")+
        geom_abline(intercept = 0,color="#8E0000")+
        xlim(0,17000)+
        ylim(0,5000)+
        scale_size(range = c(3, 12))+
        theme(axis.text.x= element_text(family='Arial',size=11,color='#505050')) +
        theme(axis.text.y= element_text(family='Arial',size=11,color='#505050')) +
        ggtitle(paste("Year",input$bubledates,sep=" "))+
        theme(legend.title=element_blank())+
        geom_point(data=services_2017_TT, color="black",stroke = 1.5)
      
    }
    else if((input$bubledates=="2017")&(input$rd_services=="All services except travel and transport")){
      ggplot(services_2017_WT, aes(x=Exports, y=Imports, size=Weight,fill= Service)) +
        geom_point(alpha=0.8,stroke=0) +
        scale_fill_manual(values=other_colors)+
        #  geom_label(label=services_1996$Item, nudge_x=0.25, nudge_y =0.25,size=5) +
        geom_abline(intercept = 0,color="#8E0000")+
        xlim(0,5000)+
        ylim(0,4000)+
        scale_size(range = c(3, 12))+
        theme(axis.text.x= element_text(family='Arial',size=11,color='#505050')) +
        theme(axis.text.y= element_text(family='Arial',size=11,color='#505050')) +
        ggtitle(paste("Year",input$bubledates,sep=" "))+
        theme(legend.title=element_blank())
      
    }
    else if((input$bubledates=="2018")&(input$rd_services=="All types of services")){
      ggplot(services_2018, aes(x=Exports, y=Imports, size=Weight,fill= Service)) +
        geom_point(alpha=0.8,stroke=0)+
        scale_fill_manual(values=my_colors)+
        #geom_text(aes(label=ifelse(Exports>6000,as.character(Item),'')), size = 4, colour = "black", vjust = 1,hjust=1)+
        # theme(legend.position="none")+
        geom_abline(intercept = 0,color="#8E0000")+
        xlim(0,17000)+
        ylim(0,5000)+
        scale_size(range = c(3, 12))+
        theme(axis.text.x= element_text(family='Arial',size=11,color='#505050')) +
        theme(axis.text.y= element_text(family='Arial',size=11,color='#505050')) +
        ggtitle(paste("Year",input$bubledates,sep=" "))+  
        theme(legend.title=element_blank())+
        geom_point(data=services_2018_TT, color="black",stroke = 1.5)
      
      
    }
    
    
    else if((input$bubledates=="2018")&(input$rd_services=="All services except travel and transport")){
      ggplot(services_2018_WT, aes(x=Exports, y=Imports, size=Weight,fill= Service)) +
        geom_point(alpha=0.8,stroke=0) +
        scale_fill_manual(values=other_colors)+
        #  geom_label(label=services_1996$Item, nudge_x=0.25, nudge_y =0.25,size=5) +
        geom_abline(intercept = 0,color="#8E0000")+
        xlim(0,5000)+
        ylim(0,4000)+
        scale_size(range = c(3, 12))+
        theme(axis.text.x= element_text(family='Arial',size=11,color='#505050')) +
        theme(axis.text.y= element_text(family='Arial',size=11,color='#505050')) +
        ggtitle(paste("Year",input$bubledates,sep=" "))+
        theme(legend.title=element_blank())
      
      
    }
    
    
  }) 
  
  observeEvent(input$reset4V, {
    reset("bubledates")
    reset("rd_services")
  })
  
  
  output$international <-   renderPlotly({
    
    ax <- list(
      title = "",
      showgrid = TRUE,
      gridcolor = toRGB("white"),
      ticks = 'outside'
    )
    
    
    x <- c('BE','BG','CZ','DK','DE','EE','IE','GR','ES','FR','HR','IT','CY','LV','LT','LU','HU','MT','NL','AT','PL','PT','RO','SI','SK','FI','SE','GB')
    
    
    if((input$internationaldates=="2018")&(input$rd=="All European countries")){
      
      
      
      MIP_Y2018$Country <- factor( MIP_Y2018$Country, levels = unique( MIP_Y2018$Country)[order(MIP_Y2018$GDP, decreasing = TRUE)])
      plot_ly(MIP_Y2018, x=~Country, y=~GDP, type = 'bar', name = 'GDP',
              marker = list(color=c('rgb(204,198,204)','rgb(204,198,204)','rgb(204,198,204)','rgb(204,198,204)','rgb(204,198,204)',
                                    'rgb(204,198,204)','rgb(204,198,204)','rgb(204,198,204)','rgb(204,198,204)','rgb(204,198,204)',
                                    'rgb(204,198,204)','rgb(204,198,204)','rgb(204,198,204)','rgb(204,198,204)','rgb(204,198,204)',
                                    'rgb(204,198,204)','rgb(204,198,204)','rgb(204,198,204)','rgb(204,198,204)','rgb(204,198,204)',
                                    'rgb(204,198,204)','rgb(142,0,0)','rgb(204,198,204)','rgb(204,198,204)','rgb(204,198,204)',
                                    'rgb(204,198,204)','rgb(204,198,204)','rgb(204,198,204)')),
              customdata = MIP_Y2018$FullName,
              hovertemplate = 'ISO Code: %{x}, Value: %{y}, Country: %{customdata}')%>%
        layout(title = paste("Year",input$internationaldates,sep=" "))%>%
        #     layout(yaxis = list(range = c(-21.5,10.5))) %>%
        layout(xaxis = ax, yaxis = ax)%>%
        layout(plot_bgcolor='#EBEBEB')%>%
        layout(
          xaxis = list(tickfont = list(size = 14)),
          yaxis = list(tickfont = list(size = 14) )
        )
      
    }
    
    
    else if((input$internationaldates=="2018")&(input$rd=="Italy, Greece and Spain")){
      
      
      MIP_Y2018_P$Country <- factor( MIP_Y2018_P$Country, levels = unique( MIP_Y2018_P$Country)[order(MIP_Y2018_P$GDP, decreasing = TRUE)])
      plot_ly(MIP_Y2018_P, x=~Country, y=~GDP, type = 'bar', name = 'GDP',
              marker = list(color=c('rgb(204,198,204)',
                                    'rgb(204,198,204)','rgb(204,198,204)','rgb(142,0,0)')),
              customdata = MIP_Y2018_P$FullName,
              hovertemplate = 'ISO Code: %{x}, Value: %{y}, Country: %{customdata}')%>%
        #      layout(yaxis = list(range = c(-14.5,3))) %>%
        layout(title = paste("Year",input$internationaldates,sep=" "))%>%
        layout(xaxis = ax, yaxis = ax)%>%
        layout(plot_bgcolor='#EBEBEB')%>%
        layout(
          xaxis = list(tickfont = list(size = 14)),
          yaxis = list(tickfont = list(size = 14) )
        )
      
    }
    
    
    
    else if((input$internationaldates=="2018")&(input$rd=="EU entry after 2004")){
      
      
      MIP_Y2018_04$Country <- factor( MIP_Y2018_04$Country, levels = unique( MIP_Y2018_04$Country)[order(MIP_Y2018_04$GDP, decreasing = TRUE)])
      plot_ly(MIP_Y2018_04, x=~Country, y=~GDP, type = 'bar', name = 'GDP',
              marker = list(color=c('rgb(204,198,204)',
                                    'rgb(204,198,204)','rgb(204,198,204)','rgb(204,198,204)','rgb(204,198,204)',
                                    'rgb(204,198,204)','rgb(204,198,204)','rgb(204,198,204)','rgb(204,198,204)',
                                    'rgb(204,198,204)','rgb(142,0,0)','rgb(204,198,204)','rgb(204,198,204)','rgb(204,198,204)',
                                    'rgb(204,198,204)')),
              customdata = MIP_Y2018_04$FullName,
              hovertemplate = 'ISO Code: %{x}, Value: %{y}, Country: %{customdata}')%>%
        #     layout(yaxis = list(range = c(-21.5,9))) %>%
        layout(title = paste("Year",input$internationaldates,sep=" "))%>%
        layout(xaxis = ax, yaxis = ax)%>%
        layout(plot_bgcolor='#EBEBEB')%>%
        layout(
          xaxis = list(tickfont = list(size = 14)),
          yaxis = list(tickfont = list(size = 14) )
        )
      
    }
    
    
    
    else if(input$internationaldates=="2017"&(input$rd=="All European countries")){
      MIP_Y2017$Country <- factor( MIP_Y2017$Country, levels = unique( MIP_Y2017$Country)[order(MIP_Y2017$GDP, decreasing = TRUE)])
      plot_ly(MIP_Y2017, x=~Country, y=~GDP, type = 'bar', name = 'GDP',
              marker = list(color=c('rgb(204,198,204)','rgb(204,198,204)','rgb(204,198,204)','rgb(204,198,204)','rgb(204,198,204)',
                                    'rgb(204,198,204)','rgb(204,198,204)','rgb(204,198,204)','rgb(204,198,204)','rgb(204,198,204)',
                                    'rgb(204,198,204)','rgb(204,198,204)','rgb(204,198,204)','rgb(204,198,204)','rgb(204,198,204)',
                                    'rgb(204,198,204)','rgb(204,198,204)','rgb(204,198,204)','rgb(204,198,204)','rgb(204,198,204)',
                                    'rgb(204,198,204)','rgb(142,0,0)','rgb(204,198,204)','rgb(204,198,204)','rgb(204,198,204)',
                                    'rgb(204,198,204)','rgb(204,198,204)','rgb(204,198,204)')),
              customdata = MIP_Y2018$FullName,
              hovertemplate = 'ISO Code: %{x}, Value: %{y}, Country: %{customdata}')%>%         
        layout(title = paste("Year",input$internationaldates,sep=" "))%>%
        #     layout(yaxis = list(range = c(-21.5,10.5))) %>%
        layout(xaxis = ax, yaxis = ax)%>%
        layout(plot_bgcolor='#EBEBEB')%>%
        layout(
          xaxis = list(tickfont = list(size = 14)),
          yaxis = list(tickfont = list(size = 14) )
        )
      
    }
    
    else if((input$internationaldates=="2017")&(input$rd=="Italy, Greece and Spain")){
      
      
      MIP_Y2017_P$Country <- factor( MIP_Y2017_P$Country, levels = unique( MIP_Y2017_P$Country)[order(MIP_Y2017_P$GDP, decreasing = TRUE)])
      plot_ly(MIP_Y2017_P, x=~Country, y=~GDP, type = 'bar', name = 'GDP',
              marker = list(color=c('rgb(204,198,204)',
                                    'rgb(204,198,204)','rgb(204,198,204)','rgb(142,0,0)')),
              customdata = MIP_Y2018_P$FullName,
              hovertemplate = 'ISO Code: %{x}, Value: %{y}, Country: %{customdata}')%>%
        #      layout(yaxis = list(range = c(-14.5,3))) %>%
        layout(title = paste("Year",input$internationaldates,sep=" "))%>%
        layout(xaxis = ax, yaxis = ax)%>%
        layout(plot_bgcolor='#EBEBEB')%>%
        layout(
          xaxis = list(tickfont = list(size = 14)),
          yaxis = list(tickfont = list(size = 14) )
        )
      
    }
    
    
    else if((input$internationaldates=="2017")&(input$rd=="EU entry after 2004")){
      
      
      MIP_Y2017_04$Country <- factor( MIP_Y2017_04$Country, levels = unique( MIP_Y2017_04$Country)[order(MIP_Y2017_04$GDP, decreasing = TRUE)])
      plot_ly(MIP_Y2017_04, x=~Country, y=~GDP, type = 'bar', name = 'GDP',
              marker = list(color=c('rgb(204,198,204)',
                                    'rgb(204,198,204)','rgb(204,198,204)','rgb(204,198,204)','rgb(204,198,204)',
                                    'rgb(204,198,204)','rgb(204,198,204)','rgb(204,198,204)','rgb(204,198,204)',
                                    'rgb(204,198,204)','rgb(142,0,0)','rgb(204,198,204)','rgb(204,198,204)','rgb(204,198,204)',
                                    'rgb(204,198,204)')),
              customdata = MIP_Y2018_04$FullName,
              hovertemplate = 'ISO Code: %{x}, Value: %{y}, Country: %{customdata}')%>%
        #    layout(yaxis = list(range = c(-21.5,9))) %>%
        layout(title = paste("Year",input$internationaldates,sep=" "))%>%
        layout(xaxis = ax, yaxis = ax)%>%
        layout(plot_bgcolor='#EBEBEB')%>%
        layout(
          xaxis = list(tickfont = list(size = 14)),
          yaxis = list(tickfont = list(size = 14) )
        )
      
    }
    
    else if(input$internationaldates=="2016"&(input$rd=="All European countries")){
      MIP_Y2016$Country <- factor( MIP_Y2016$Country, levels = unique( MIP_Y2016$Country)[order(MIP_Y2016$GDP, decreasing = TRUE)])
      plot_ly(MIP_Y2016, x=~Country, y=~GDP, type = 'bar', name = 'GDP',
              marker = list(color=c('rgb(204,198,204)','rgb(204,198,204)','rgb(204,198,204)','rgb(204,198,204)','rgb(204,198,204)',
                                    'rgb(204,198,204)','rgb(204,198,204)','rgb(204,198,204)','rgb(204,198,204)','rgb(204,198,204)',
                                    'rgb(204,198,204)','rgb(204,198,204)','rgb(204,198,204)','rgb(204,198,204)','rgb(204,198,204)',
                                    'rgb(204,198,204)','rgb(204,198,204)','rgb(204,198,204)','rgb(204,198,204)','rgb(204,198,204)',
                                    'rgb(204,198,204)','rgb(142,0,0)','rgb(204,198,204)','rgb(204,198,204)','rgb(204,198,204)',
                                    'rgb(204,198,204)','rgb(204,198,204)','rgb(204,198,204)')),
              customdata = MIP_Y2018$FullName,
              hovertemplate = 'ISO Code: %{x}, Value: %{y}, Country: %{customdata}')%>%         
        layout(title = paste("Year",input$internationaldates,sep=" "))%>%
        #    layout(yaxis = list(range = c(-21.5,10.5)))  %>%
        layout(xaxis = ax, yaxis = ax)%>%
        layout(plot_bgcolor='#EBEBEB')%>%
        layout(
          xaxis = list(tickfont = list(size = 14)),
          yaxis = list(tickfont = list(size = 14) )
        )
      
    }
    
    else if((input$internationaldates=="2016")&(input$rd=="Italy, Greece and Spain")){
      
      
      MIP_Y2016_P$Country <- factor( MIP_Y2016_P$Country, levels = unique( MIP_Y2016_P$Country)[order(MIP_Y2016_P$GDP, decreasing = TRUE)])
      plot_ly(MIP_Y2016_P, x=~Country, y=~GDP, type = 'bar', name = 'GDP',
              marker = list(color=c('rgb(204,198,204)',
                                    'rgb(204,198,204)','rgb(204,198,204)','rgb(142,0,0)')),
              customdata = MIP_Y2018_P$FullName,
              hovertemplate = 'ISO Code: %{x}, Value: %{y}, Country: %{customdata}')%>%
        #       layout(yaxis = list(range = c(-14.5,3))) %>%
        layout(title = paste("Year",input$internationaldates,sep=" "))%>%
        layout(xaxis = ax, yaxis = ax)%>%
        layout(plot_bgcolor='#EBEBEB')%>%
        layout(
          xaxis = list(tickfont = list(size = 14)),
          yaxis = list(tickfont = list(size = 14) )
        )
      
    }
    
    
    else if((input$internationaldates=="2016")&(input$rd=="EU entry after 2004")){
      
      
      MIP_Y2016_04$Country <- factor( MIP_Y2016_04$Country, levels = unique( MIP_Y2016_04$Country)[order(MIP_Y2016_04$GDP, decreasing = TRUE)])
      plot_ly(MIP_Y2016_04, x=~Country, y=~GDP, type = 'bar', name = 'GDP',
              marker = list(color=c('rgb(204,198,204)',
                                    'rgb(204,198,204)','rgb(204,198,204)','rgb(204,198,204)','rgb(204,198,204)',
                                    'rgb(204,198,204)','rgb(204,198,204)','rgb(204,198,204)','rgb(204,198,204)',
                                    'rgb(204,198,204)','rgb(142,0,0)','rgb(204,198,204)','rgb(204,198,204)','rgb(204,198,204)',
                                    'rgb(204,198,204)')),
              customdata = MIP_Y2018_04$FullName,
              hovertemplate = 'ISO Code: %{x}, Value: %{y}, Country: %{customdata}')%>%
        #     layout(yaxis = list(range = c(-21.5,9))) %>%
        layout(title = paste("Year",input$internationaldates,sep=" "))%>%
        layout(xaxis = ax, yaxis = ax)%>%
        layout(plot_bgcolor='#EBEBEB')%>%
        layout(
          xaxis = list(tickfont = list(size = 14)),
          yaxis = list(tickfont = list(size = 14) )
        )
      
    }
    
    
    else if(input$internationaldates=="2015"&(input$rd=="All European countries")){
      MIP_Y2015$Country <- factor( MIP_Y2015$Country, levels = unique( MIP_Y2015$Country)[order(MIP_Y2015$GDP, decreasing = TRUE)])
      plot_ly(MIP_Y2015, x=~Country, y=~GDP, type = 'bar', name = 'GDP',
              marker = list(color=c('rgb(204,198,204)','rgb(204,198,204)','rgb(204,198,204)','rgb(204,198,204)','rgb(204,198,204)',
                                    'rgb(204,198,204)','rgb(204,198,204)','rgb(204,198,204)','rgb(204,198,204)','rgb(204,198,204)',
                                    'rgb(204,198,204)','rgb(204,198,204)','rgb(204,198,204)','rgb(204,198,204)','rgb(204,198,204)',
                                    'rgb(204,198,204)','rgb(204,198,204)','rgb(204,198,204)','rgb(204,198,204)','rgb(204,198,204)',
                                    'rgb(204,198,204)','rgb(142,0,0)','rgb(204,198,204)','rgb(204,198,204)','rgb(204,198,204)',
                                    'rgb(204,198,204)','rgb(204,198,204)','rgb(204,198,204)')),
              customdata = MIP_Y2018$FullName,
              hovertemplate = 'ISO Code: %{x}, Value: %{y}, Country: %{customdata}')%>%         
        #       layout(yaxis = list(range = c(-21.5,10.5)))  %>%
        layout(title = paste("Year",input$internationaldates,sep=" "))%>%
        layout(xaxis = ax, yaxis = ax)%>%
        layout(plot_bgcolor='#EBEBEB')%>%
        layout(
          xaxis = list(tickfont = list(size = 14)),
          yaxis = list(tickfont = list(size = 14) )
        )
      
    }
    
    else if((input$internationaldates=="2015")&(input$rd=="Italy, Greece and Spain")){
      
      
      MIP_Y2015_P$Country <- factor( MIP_Y2015_P$Country, levels = unique( MIP_Y2015_P$Country)[order(MIP_Y2015_P$GDP, decreasing = TRUE)])
      plot_ly(MIP_Y2015_P, x=~Country, y=~GDP, type = 'bar', name = 'GDP',
              marker = list(color=c('rgb(204,198,204)',
                                    'rgb(204,198,204)','rgb(204,198,204)','rgb(142,0,0)')),
              customdata = MIP_Y2018_P$FullName,
              hovertemplate = 'ISO Code: %{x}, Value: %{y}, Country: %{customdata}')%>%
        #    layout(yaxis = list(range = c(-14.5,3))) %>%
        layout(title = paste("Year",input$internationaldates,sep=" "))%>%
        layout(xaxis = ax, yaxis = ax)%>%
        layout(plot_bgcolor='#EBEBEB')%>%
        layout(
          xaxis = list(tickfont = list(size = 14)),
          yaxis = list(tickfont = list(size = 14) )
        )
      
    }
    
    
    else if((input$internationaldates=="2015")&(input$rd=="EU entry after 2004")){
      
      
      MIP_Y2015_04$Country <- factor( MIP_Y2015_04$Country, levels = unique( MIP_Y2015_04$Country)[order(MIP_Y2015_04$GDP, decreasing = TRUE)])
      plot_ly(MIP_Y2015_04, x=~Country, y=~GDP, type = 'bar', name = 'GDP',
              marker = list(color=c('rgb(204,198,204)',
                                    'rgb(204,198,204)','rgb(204,198,204)','rgb(204,198,204)','rgb(204,198,204)',
                                    'rgb(204,198,204)','rgb(204,198,204)','rgb(204,198,204)','rgb(204,198,204)',
                                    'rgb(204,198,204)','rgb(142,0,0)','rgb(204,198,204)','rgb(204,198,204)','rgb(204,198,204)',
                                    'rgb(204,198,204)')),
              customdata = MIP_Y2018_04$FullName,
              hovertemplate = 'ISO Code: %{x}, Value: %{y}, Country: %{customdata}')%>%
        #      layout(yaxis = list(range = c(-21.5,9))) %>%
        layout(title = paste("Year",input$internationaldates,sep=" "))%>%
        layout(xaxis = ax, yaxis = ax)%>%
        layout(plot_bgcolor='#EBEBEB')%>%
        layout(
          xaxis = list(tickfont = list(size = 14)),
          yaxis = list(tickfont = list(size = 14) )
        )
      
    }
    
    else if(input$internationaldates=="2014"&(input$rd=="All European countries")){
      MIP_Y2014$Country <- factor( MIP_Y2014$Country, levels = unique( MIP_Y2014$Country)[order(MIP_Y2014$GDP, decreasing = TRUE)])
      plot_ly(MIP_Y2014, x=~Country, y=~GDP, type = 'bar', name = 'GDP',
              marker = list(color=c('rgb(204,198,204)','rgb(204,198,204)','rgb(204,198,204)','rgb(204,198,204)','rgb(204,198,204)',
                                    'rgb(204,198,204)','rgb(204,198,204)','rgb(204,198,204)','rgb(204,198,204)','rgb(204,198,204)',
                                    'rgb(204,198,204)','rgb(204,198,204)','rgb(204,198,204)','rgb(204,198,204)','rgb(204,198,204)',
                                    'rgb(204,198,204)','rgb(204,198,204)','rgb(204,198,204)','rgb(204,198,204)','rgb(204,198,204)',
                                    'rgb(204,198,204)','rgb(142,0,0)','rgb(204,198,204)','rgb(204,198,204)','rgb(204,198,204)',
                                    'rgb(204,198,204)','rgb(204,198,204)','rgb(204,198,204)')),
              customdata = MIP_Y2018$FullName,
              hovertemplate = 'ISO Code: %{x}, Value: %{y}, Country: %{customdata}')%>%   
        #       layout(yaxis = list(range = c(-21.5,10.5)))  %>%
        layout(title = paste("Year",input$internationaldates,sep=" "))%>%
        layout(xaxis = ax, yaxis = ax)%>%
        layout(plot_bgcolor='#EBEBEB')%>%
        layout(
          xaxis = list(tickfont = list(size = 14)),
          yaxis = list(tickfont = list(size = 14) )
        )
      
    }
    
    else if((input$internationaldates=="2014")&(input$rd=="Italy, Greece and Spain")){
      
      
      MIP_Y2014_P$Country <- factor( MIP_Y2014_P$Country, levels = unique( MIP_Y2014_P$Country)[order(MIP_Y2014_P$GDP, decreasing = TRUE)])
      plot_ly(MIP_Y2014_P, x=~Country, y=~GDP, type = 'bar', name = 'GDP',
              marker = list(color=c('rgb(204,198,204)',
                                    'rgb(204,198,204)','rgb(204,198,204)','rgb(142,0,0)')),
              customdata = MIP_Y2018_P$FullName,
              hovertemplate = 'ISO Code: %{x}, Value: %{y}, Country: %{customdata}')%>%
        #       layout(yaxis = list(range = c(-14.5,3))) %>%
        layout(title = paste("Year",input$internationaldates,sep=" "))%>%
        layout(xaxis = ax, yaxis = ax)%>%
        layout(plot_bgcolor='#EBEBEB')%>%
        layout(
          xaxis = list(tickfont = list(size = 14)),
          yaxis = list(tickfont = list(size = 14) )
        )
      
    }
    
    
    else if((input$internationaldates=="2014")&(input$rd=="EU entry after 2004")){
      
      
      MIP_Y2014_04$Country <- factor( MIP_Y2014_04$Country, levels = unique( MIP_Y2014_04$Country)[order(MIP_Y2014_04$GDP, decreasing = TRUE)])
      plot_ly(MIP_Y2014_04, x=~Country, y=~GDP, type = 'bar', name = 'GDP',
              marker = list(color=c('rgb(204,198,204)',
                                    'rgb(204,198,204)','rgb(204,198,204)','rgb(204,198,204)','rgb(204,198,204)',
                                    'rgb(204,198,204)','rgb(204,198,204)','rgb(204,198,204)','rgb(204,198,204)',
                                    'rgb(204,198,204)','rgb(142,0,0)','rgb(204,198,204)','rgb(204,198,204)','rgb(204,198,204)',
                                    'rgb(204,198,204)')),
              customdata = MIP_Y2018_04$FullName,
              hovertemplate = 'ISO Code: %{x}, Value: %{y}, Country: %{customdata}')%>%
        #       layout(yaxis = list(range = c(-21.5,9))) %>%
        layout(title = paste("Year",input$internationaldates,sep=" "))%>%
        layout(xaxis = ax, yaxis = ax)%>%
        layout(plot_bgcolor='#EBEBEB')%>%
        layout(
          xaxis = list(tickfont = list(size = 14)),
          yaxis = list(tickfont = list(size = 14) )
        )
      
    }
    
    else if(input$internationaldates=="2013"&(input$rd=="All European countries")){
      
      MIP_Y2013$Country <- factor( MIP_Y2013$Country, levels = unique( MIP_Y2013$Country)[order(MIP_Y2013$GDP, decreasing = TRUE)])
      plot_ly(MIP_Y2013, x=~Country, y=~GDP, type = 'bar', name = 'GDP'    ,
              marker = list(color=c('rgb(204,198,204)','rgb(204,198,204)','rgb(204,198,204)','rgb(204,198,204)','rgb(204,198,204)',
                                    'rgb(204,198,204)','rgb(204,198,204)','rgb(204,198,204)','rgb(204,198,204)','rgb(204,198,204)',
                                    'rgb(204,198,204)','rgb(204,198,204)','rgb(204,198,204)','rgb(204,198,204)','rgb(204,198,204)',
                                    'rgb(204,198,204)','rgb(204,198,204)','rgb(204,198,204)','rgb(204,198,204)','rgb(204,198,204)',
                                    'rgb(204,198,204)','rgb(142,0,0)','rgb(204,198,204)','rgb(204,198,204)','rgb(204,198,204)',
                                    'rgb(204,198,204)','rgb(204,198,204)','rgb(204,198,204)')),
              customdata = MIP_Y2018$FullName,
              hovertemplate = 'ISO Code: %{x}, Value: %{y}, Country: %{customdata}')%>%       
        #       layout(yaxis = list(range = c(-21.5,10.5)))  %>%
        layout(title = paste("Year",input$internationaldates,sep=" "))%>%
        layout(xaxis = ax, yaxis = ax)%>%
        layout(plot_bgcolor='#EBEBEB')%>%
        layout(
          xaxis = list(tickfont = list(size = 14)),
          yaxis = list(tickfont = list(size = 14) )
        )
    }
    
    else if((input$internationaldates=="2013")&(input$rd=="Italy, Greece and Spain")){
      
      
      MIP_Y2013_P$Country <- factor( MIP_Y2013_P$Country, levels = unique( MIP_Y2013_P$Country)[order(MIP_Y2013_P$GDP, decreasing = TRUE)])
      plot_ly(MIP_Y2013_P, x=~Country, y=~GDP, type = 'bar', name = 'GDP',
              marker = list(color=c('rgb(204,198,204)',
                                    'rgb(204,198,204)','rgb(204,198,204)','rgb(142,0,0)')),
              customdata = MIP_Y2018_P$FullName,
              hovertemplate = 'ISO Code: %{x}, Value: %{y}, Country: %{customdata}')%>%
        #      layout(yaxis = list(range = c(-14.5,3))) %>%
        layout(title = paste("Year",input$internationaldates,sep=" "))%>%
        layout(xaxis = ax, yaxis = ax)%>%
        layout(plot_bgcolor='#EBEBEB')%>%
        layout(
          xaxis = list(tickfont = list(size = 14)),
          yaxis = list(tickfont = list(size = 14) )
        )
      
    }
    
    
    else if((input$internationaldates=="2013")&(input$rd=="EU entry after 2004")){
      
      
      MIP_Y2013_04$Country <- factor( MIP_Y2013_04$Country, levels = unique( MIP_Y2013_04$Country)[order(MIP_Y2013_04$GDP, decreasing = TRUE)])
      plot_ly(MIP_Y2013_04, x=~Country, y=~GDP, type = 'bar', name = 'GDP',
              marker = list(color=c('rgb(204,198,204)',
                                    'rgb(204,198,204)','rgb(204,198,204)','rgb(204,198,204)','rgb(204,198,204)',
                                    'rgb(204,198,204)','rgb(204,198,204)','rgb(204,198,204)','rgb(204,198,204)',
                                    'rgb(204,198,204)','rgb(142,0,0)','rgb(204,198,204)','rgb(204,198,204)','rgb(204,198,204)',
                                    'rgb(204,198,204)')),
              customdata = MIP_Y2018_04$FullName,
              hovertemplate = 'ISO Code: %{x}, Value: %{y}, Country: %{customdata}')%>%
        #     layout(yaxis = list(range = c(-21.5,9))) %>%
        layout(title = paste("Year",input$internationaldates,sep=" "))%>%
        layout(xaxis = ax, yaxis = ax)%>%
        layout(plot_bgcolor='#EBEBEB')%>%
        layout(
          xaxis = list(tickfont = list(size = 14)),
          yaxis = list(tickfont = list(size = 14) )
        )
      
    }
    
    
    else if(input$internationaldates=="2012"&(input$rd=="All European countries")){
      
      MIP_Y2012$Country <- factor( MIP_Y2012$Country, levels = unique( MIP_Y2012$Country)[order(MIP_Y2012$GDP, decreasing = TRUE)])
      plot_ly(MIP_Y2012, x=~Country, y=~GDP, type = 'bar', name = 'GDP',
              marker = list(color=c('rgb(204,198,204)','rgb(204,198,204)','rgb(204,198,204)','rgb(204,198,204)','rgb(204,198,204)',
                                    'rgb(204,198,204)','rgb(204,198,204)','rgb(204,198,204)','rgb(204,198,204)','rgb(204,198,204)',
                                    'rgb(204,198,204)','rgb(204,198,204)','rgb(204,198,204)','rgb(204,198,204)','rgb(204,198,204)',
                                    'rgb(204,198,204)','rgb(204,198,204)','rgb(204,198,204)','rgb(204,198,204)','rgb(204,198,204)',
                                    'rgb(204,198,204)','rgb(142,0,0)','rgb(204,198,204)','rgb(204,198,204)','rgb(204,198,204)',
                                    'rgb(204,198,204)','rgb(204,198,204)','rgb(204,198,204)')),
              customdata = MIP_Y2018$FullName,
              hovertemplate = 'ISO Code: %{x}, Value: %{y}, Country: %{customdata}')%>%           
        #     layout(yaxis = list(range = c(-21.5,10.5)))  %>%
        layout(title = paste("Year",input$internationaldates,sep=" "))%>%
        layout(xaxis = ax, yaxis = ax)%>%
        layout(plot_bgcolor='#EBEBEB')%>%
        layout(
          xaxis = list(tickfont = list(size = 14)),
          yaxis = list(tickfont = list(size = 14) )
        )
    }
    
    else if((input$internationaldates=="2012")&(input$rd=="Italy, Greece and Spain")){
      
      
      MIP_Y2012_P$Country <- factor( MIP_Y2012_P$Country, levels = unique( MIP_Y2012_P$Country)[order(MIP_Y2012_P$GDP, decreasing = TRUE)])
      plot_ly(MIP_Y2012_P, x=~Country, y=~GDP, type = 'bar', name = 'GDP',
              marker = list(color=c('rgb(204,198,204)',
                                    'rgb(204,198,204)','rgb(204,198,204)','rgb(142,0,0)')),
              customdata = MIP_Y2018_P$FullName,
              hovertemplate = 'ISO Code: %{x}, Value: %{y}, Country: %{customdata}')%>%
        #    layout(yaxis = list(range = c(-14.5,3))) %>%
        layout(title = paste("Year",input$internationaldates,sep=" "))%>%
        layout(xaxis = ax, yaxis = ax)%>%
        layout(plot_bgcolor='#EBEBEB')%>%
        layout(
          xaxis = list(tickfont = list(size = 14)),
          yaxis = list(tickfont = list(size = 14) )
        )
      
    }
    
    
    else if((input$internationaldates=="2012")&(input$rd=="EU entry after 2004")){
      
      
      MIP_Y2012_04$Country <- factor( MIP_Y2012_04$Country, levels = unique( MIP_Y2012_04$Country)[order(MIP_Y2012_04$GDP, decreasing = TRUE)])
      plot_ly(MIP_Y2012_04, x=~Country, y=~GDP, type = 'bar', name = 'GDP',
              marker = list(color=c('rgb(204,198,204)',
                                    'rgb(204,198,204)','rgb(204,198,204)','rgb(204,198,204)','rgb(204,198,204)',
                                    'rgb(204,198,204)','rgb(204,198,204)','rgb(204,198,204)','rgb(204,198,204)',
                                    'rgb(204,198,204)','rgb(142,0,0)','rgb(204,198,204)','rgb(204,198,204)','rgb(204,198,204)',
                                    'rgb(204,198,204)')),
              customdata = MIP_Y2018_04$FullName,
              hovertemplate = 'ISO Code: %{x}, Value: %{y}, Country: %{customdata}')%>%
        #   layout(yaxis = list(range = c(-21.5,9))) %>%
        layout(title = paste("Year",input$internationaldates,sep=" "))%>%
        layout(xaxis = ax, yaxis = ax)%>%
        layout(plot_bgcolor='#EBEBEB')%>%
        layout(
          xaxis = list(tickfont = list(size = 14)),
          yaxis = list(tickfont = list(size = 14) )
        )
      
    }
    
    else if(input$internationaldates=="2011"&(input$rd=="All European countries")){
      MIP_Y2011$Country <- factor( MIP_Y2011$Country, levels = unique( MIP_Y2011$Country)[order(MIP_Y2011$GDP, decreasing = TRUE)])
      plot_ly(MIP_Y2011, x=~Country, y=~GDP, type = 'bar', name = 'GDP',
              marker = list(color=c('rgb(204,198,204)','rgb(204,198,204)','rgb(204,198,204)','rgb(204,198,204)','rgb(204,198,204)',
                                    'rgb(204,198,204)','rgb(204,198,204)','rgb(204,198,204)','rgb(204,198,204)','rgb(204,198,204)',
                                    'rgb(204,198,204)','rgb(204,198,204)','rgb(204,198,204)','rgb(204,198,204)','rgb(204,198,204)',
                                    'rgb(204,198,204)','rgb(204,198,204)','rgb(204,198,204)','rgb(204,198,204)','rgb(204,198,204)',
                                    'rgb(204,198,204)','rgb(142,0,0)','rgb(204,198,204)','rgb(204,198,204)','rgb(204,198,204)',
                                    'rgb(204,198,204)','rgb(204,198,204)','rgb(204,198,204)')),
              customdata = MIP_Y2018$FullName,
              hovertemplate = 'ISO Code: %{x}, Value: %{y}, Country: %{customdata}')%>%           
        #      layout(yaxis = list(range = c(-21.5,10.5)))  %>%
        layout(title = paste("Year",input$internationaldates,sep=" "))%>%
        layout(xaxis = ax, yaxis = ax)%>%
        layout(plot_bgcolor='#EBEBEB')%>%
        layout(
          xaxis = list(tickfont = list(size = 14)),
          yaxis = list(tickfont = list(size = 14) )
        )
      
    }
    
    else if((input$internationaldates=="2011")&(input$rd=="Italy, Greece and Spain")){
      
      
      MIP_Y2011_P$Country <- factor( MIP_Y2011_P$Country, levels = unique( MIP_Y2011_P$Country)[order(MIP_Y2011_P$GDP, decreasing = TRUE)])
      plot_ly(MIP_Y2011_P, x=~Country, y=~GDP, type = 'bar', name = 'GDP',
              marker = list(color=c('rgb(204,198,204)',
                                    'rgb(204,198,204)','rgb(204,198,204)','rgb(142,0,0)')),
              customdata = MIP_Y2018_P$FullName,
              hovertemplate = 'ISO Code: %{x}, Value: %{y}, Country: %{customdata}')%>%
        #     layout(yaxis = list(range = c(-14.5,3))) %>%
        layout(title = paste("Year",input$internationaldates,sep=" "))%>%
        layout(xaxis = ax, yaxis = ax)%>%
        layout(plot_bgcolor='#EBEBEB')%>%
        layout(
          xaxis = list(tickfont = list(size = 14)),
          yaxis = list(tickfont = list(size = 14) )
        )
      
    }
    
    
    else if((input$internationaldates=="2011")&(input$rd=="EU entry after 2004")){
      
      
      MIP_Y2011_04$Country <- factor( MIP_Y2011_04$Country, levels = unique( MIP_Y2011_04$Country)[order(MIP_Y2011_04$GDP, decreasing = TRUE)])
      plot_ly(MIP_Y2011_04, x=~Country, y=~GDP, type = 'bar', name = 'GDP',
              marker = list(color=c('rgb(204,198,204)',
                                    'rgb(204,198,204)','rgb(204,198,204)','rgb(204,198,204)','rgb(204,198,204)',
                                    'rgb(204,198,204)','rgb(204,198,204)','rgb(204,198,204)','rgb(204,198,204)',
                                    'rgb(204,198,204)','rgb(142,0,0)','rgb(204,198,204)','rgb(204,198,204)','rgb(204,198,204)',
                                    'rgb(204,198,204)')),
              customdata = MIP_Y2018_04$FullName,
              hovertemplate = 'ISO Code: %{x}, Value: %{y}, Country: %{customdata}')%>%
        #    layout(yaxis = list(range = c(-21.5,9))) %>%
        layout(title = paste("Year",input$internationaldates,sep=" "))%>%
        layout(xaxis = ax, yaxis = ax)%>%
        layout(plot_bgcolor='#EBEBEB')%>%
        layout(
          xaxis = list(tickfont = list(size = 14)),
          yaxis = list(tickfont = list(size = 14) )
        )
      
    }
    
    
    else if(input$internationaldates=="2010"&(input$rd=="All European countries")){
      
      MIP_Y2010$Country <- factor( MIP_Y2010$Country, levels = unique( MIP_Y2010$Country)[order(MIP_Y2010$GDP, decreasing = TRUE)])
      plot_ly(MIP_Y2010, x=~Country, y=~GDP, type = 'bar', name = 'GDP' ,
              marker = list(color=c('rgb(204,198,204)','rgb(204,198,204)','rgb(204,198,204)','rgb(204,198,204)','rgb(204,198,204)',
                                    'rgb(204,198,204)','rgb(204,198,204)','rgb(204,198,204)','rgb(204,198,204)','rgb(204,198,204)',
                                    'rgb(204,198,204)','rgb(204,198,204)','rgb(204,198,204)','rgb(204,198,204)','rgb(204,198,204)',
                                    'rgb(204,198,204)','rgb(204,198,204)','rgb(204,198,204)','rgb(204,198,204)','rgb(204,198,204)',
                                    'rgb(204,198,204)','rgb(142,0,0)','rgb(204,198,204)','rgb(204,198,204)','rgb(204,198,204)',
                                    'rgb(204,198,204)','rgb(204,198,204)','rgb(204,198,204)')),
              customdata = MIP_Y2018$FullName,
              hovertemplate = 'ISO Code: %{x}, Value: %{y}, Country: %{customdata}')%>%          
        #    layout(yaxis = list(range = c(-21.5,10.5)))  %>%
        layout(title = paste("Year",input$internationaldates,sep=" "))%>%
        layout(xaxis = ax, yaxis = ax)%>%
        layout(plot_bgcolor='#EBEBEB')%>%
        layout(
          xaxis = list(tickfont = list(size = 14)),
          yaxis = list(tickfont = list(size = 14) )
        )
    }
    
    else if((input$internationaldates=="2010")&(input$rd=="Italy, Greece and Spain")){
      
      
      MIP_Y2010_P$Country <- factor( MIP_Y2010_P$Country, levels = unique( MIP_Y2010_P$Country)[order(MIP_Y2010_P$GDP, decreasing = TRUE)])
      plot_ly(MIP_Y2010_P, x=~Country, y=~GDP, type = 'bar', name = 'GDP',
              marker = list(color=c('rgb(204,198,204)',
                                    'rgb(204,198,204)','rgb(204,198,204)','rgb(142,0,0)')),
              customdata = MIP_Y2018_P$FullName,
              hovertemplate = 'ISO Code: %{x}, Value: %{y}, Country: %{customdata}')%>%
        #    layout(yaxis = list(range = c(-14.5,3))) %>%
        layout(title = paste("Year",input$internationaldates,sep=" "))%>%
        layout(xaxis = ax, yaxis = ax)%>%
        layout(plot_bgcolor='#EBEBEB')%>%
        layout(
          xaxis = list(tickfont = list(size = 14)),
          yaxis = list(tickfont = list(size = 14) )
        )
      
    }
    
    
    else if((input$internationaldates=="2010")&(input$rd=="EU entry after 2004")){
      
      
      MIP_Y2010_04$Country <- factor( MIP_Y2010_04$Country, levels = unique( MIP_Y2010_04$Country)[order(MIP_Y2010_04$GDP, decreasing = TRUE)])
      plot_ly(MIP_Y2010_04, x=~Country, y=~GDP, type = 'bar', name = 'GDP',
              marker = list(color=c('rgb(204,198,204)',
                                    'rgb(204,198,204)','rgb(204,198,204)','rgb(204,198,204)','rgb(204,198,204)',
                                    'rgb(204,198,204)','rgb(204,198,204)','rgb(204,198,204)','rgb(204,198,204)',
                                    'rgb(204,198,204)','rgb(142,0,0)','rgb(204,198,204)','rgb(204,198,204)','rgb(204,198,204)',
                                    'rgb(204,198,204)')),
              customdata = MIP_Y2018_04$FullName,
              hovertemplate = 'ISO Code: %{x}, Value: %{y}, Country: %{customdata}')%>%
        #       layout(yaxis = list(range = c(-21.5,9))) %>%
        layout(title = paste("Year",input$internationaldates,sep=" "))%>%
        layout(xaxis = ax, yaxis = ax)%>%
        layout(plot_bgcolor='#EBEBEB')%>%
        layout(
          xaxis = list(tickfont = list(size = 14)),
          yaxis = list(tickfont = list(size = 14) )
        )
      
    }
    
    
    else if(input$internationaldates=="2009"&(input$rd=="All European countries")){
      
      MIP_Y2009$Country <- factor( MIP_Y2009$Country, levels = unique( MIP_Y2009$Country)[order(MIP_Y2009$GDP, decreasing = TRUE)])
      plot_ly(MIP_Y2009, x=~Country, y=~GDP, type = 'bar', name = 'GDP',
              marker = list(color=c('rgb(204,198,204)','rgb(204,198,204)','rgb(204,198,204)','rgb(204,198,204)','rgb(204,198,204)',
                                    'rgb(204,198,204)','rgb(204,198,204)','rgb(204,198,204)','rgb(204,198,204)','rgb(204,198,204)',
                                    'rgb(204,198,204)','rgb(204,198,204)','rgb(204,198,204)','rgb(204,198,204)','rgb(204,198,204)',
                                    'rgb(204,198,204)','rgb(204,198,204)','rgb(204,198,204)','rgb(204,198,204)','rgb(204,198,204)',
                                    'rgb(204,198,204)','rgb(142,0,0)','rgb(204,198,204)','rgb(204,198,204)','rgb(204,198,204)',
                                    'rgb(204,198,204)','rgb(204,198,204)','rgb(204,198,204)')),
              customdata = MIP_Y2018$FullName,
              hovertemplate = 'ISO Code: %{x}, Value: %{y}, Country: %{customdata}')%>%           
        #        layout(yaxis = list(range = c(-21.5,10.5)))  %>%
        layout(title = paste("Year",input$internationaldates,sep=" "))%>%
        layout(xaxis = ax, yaxis = ax)%>%
        layout(plot_bgcolor='#EBEBEB')%>%
        layout(
          xaxis = list(tickfont = list(size = 14)),
          yaxis = list(tickfont = list(size = 14) )
        )
    }
    
    else if((input$internationaldates=="2009")&(input$rd=="Italy, Greece and Spain")){
      
      
      MIP_Y2009_P$Country <- factor( MIP_Y2009_P$Country, levels = unique( MIP_Y2009_P$Country)[order(MIP_Y2009_P$GDP, decreasing = TRUE)])
      plot_ly(MIP_Y2009_P, x=~Country, y=~GDP, type = 'bar', name = 'GDP',
              marker = list(color=c('rgb(204,198,204)',
                                    'rgb(204,198,204)','rgb(204,198,204)','rgb(142,0,0)')),
              customdata = MIP_Y2018_P$FullName,
              hovertemplate = 'ISO Code: %{x}, Value: %{y}, Country: %{customdata}')%>%
        #    layout(yaxis = list(range = c(-14.5,3))) %>%
        layout(title = paste("Year",input$internationaldates,sep=" "))%>%
        layout(xaxis = ax, yaxis = ax)%>%
        layout(plot_bgcolor='#EBEBEB')%>%
        layout(
          xaxis = list(tickfont = list(size = 14)),
          yaxis = list(tickfont = list(size = 14) )
        )
      
    }
    
    
    else if((input$internationaldates=="2009")&(input$rd=="EU entry after 2004")){
      
      
      MIP_Y2009_04$Country <- factor( MIP_Y2009_04$Country, levels = unique( MIP_Y2009_04$Country)[order(MIP_Y2009_04$GDP, decreasing = TRUE)])
      plot_ly(MIP_Y2009_04, x=~Country, y=~GDP, type = 'bar', name = 'GDP',
              marker = list(color=c('rgb(204,198,204)',
                                    'rgb(204,198,204)','rgb(204,198,204)','rgb(204,198,204)','rgb(204,198,204)',
                                    'rgb(204,198,204)','rgb(204,198,204)','rgb(204,198,204)','rgb(204,198,204)',
                                    'rgb(204,198,204)','rgb(142,0,0)','rgb(204,198,204)','rgb(204,198,204)','rgb(204,198,204)',
                                    'rgb(204,198,204)')),
              customdata = MIP_Y2018_04$FullName,
              hovertemplate = 'ISO Code: %{x}, Value: %{y}, Country: %{customdata}')%>%
        #      layout(yaxis = list(range = c(-21.5,9))) %>%
        layout(title = paste("Year",input$internationaldates,sep=" "))%>%
        layout(xaxis = ax, yaxis = ax)%>%
        layout(plot_bgcolor='#EBEBEB')%>%
        layout(
          xaxis = list(tickfont = list(size = 14)),
          yaxis = list(tickfont = list(size = 14) )
        )
      
    }
    
    
    else if(input$internationaldates=="2008"&(input$rd=="All European countries")){
      
      MIP_Y2008$Country <- factor( MIP_Y2008$Country, levels = unique( MIP_Y2008$Country)[order(MIP_Y2008$GDP, decreasing = TRUE)])
      plot_ly(MIP_Y2008, x=~Country, y=~GDP, type = 'bar', name = 'GDP',
              marker = list(color=c('rgb(204,198,204)','rgb(204,198,204)','rgb(204,198,204)','rgb(204,198,204)','rgb(204,198,204)',
                                    'rgb(204,198,204)','rgb(204,198,204)','rgb(204,198,204)','rgb(204,198,204)','rgb(204,198,204)',
                                    'rgb(204,198,204)','rgb(204,198,204)','rgb(204,198,204)','rgb(204,198,204)','rgb(204,198,204)',
                                    'rgb(204,198,204)','rgb(204,198,204)','rgb(204,198,204)','rgb(204,198,204)','rgb(204,198,204)',
                                    'rgb(204,198,204)','rgb(142,0,0)','rgb(204,198,204)','rgb(204,198,204)','rgb(204,198,204)',
                                    'rgb(204,198,204)','rgb(204,198,204)','rgb(204,198,204)')),
              customdata = MIP_Y2018$FullName,
              hovertemplate = 'ISO Code: %{x}, Value: %{y}, Country: %{customdata}')%>%          
        #       layout(yaxis = list(range = c(-21.5,10.5)))  %>%
        layout(title = paste("Year",input$internationaldates,sep=" "))%>%
        layout(xaxis = ax, yaxis = ax)%>%
        layout(plot_bgcolor='#EBEBEB')%>%
        layout(
          xaxis = list(tickfont = list(size = 14)),
          yaxis = list(tickfont = list(size = 14) )
        )
    }
    
    else if((input$internationaldates=="2008")&(input$rd=="Italy, Greece and Spain")){
      
      
      MIP_Y2008_P$Country <- factor( MIP_Y2008_P$Country, levels = unique( MIP_Y2008_P$Country)[order(MIP_Y2008_P$GDP, decreasing = TRUE)])
      plot_ly(MIP_Y2008_P, x=~Country, y=~GDP, type = 'bar', name = 'GDP',
              marker = list(color=c('rgb(204,198,204)',
                                    'rgb(204,198,204)','rgb(204,198,204)','rgb(142,0,0)')),
              customdata = MIP_Y2018_P$FullName,
              hovertemplate = 'ISO Code: %{x}, Value: %{y}, Country: %{customdata}')%>%
        #      layout(yaxis = list(range = c(-14.5,3))) %>%
        layout(title = paste("Year",input$internationaldates,sep=" "))%>%
        layout(xaxis = ax, yaxis = ax)%>%
        layout(plot_bgcolor='#EBEBEB')%>%
        layout(
          xaxis = list(tickfont = list(size = 14)),
          yaxis = list(tickfont = list(size = 14) )
        )
      
    }
    
    
    else if((input$internationaldates=="2008")&(input$rd=="EU entry after 2004")){
      
      
      MIP_Y2008_04$Country <- factor( MIP_Y2008_04$Country, levels = unique( MIP_Y2008_04$Country)[order(MIP_Y2008_04$GDP, decreasing = TRUE)])
      plot_ly(MIP_Y2008_04, x=~Country, y=~GDP, type = 'bar', name = 'GDP',
              marker = list(color=c('rgb(204,198,204)',
                                    'rgb(204,198,204)','rgb(204,198,204)','rgb(204,198,204)','rgb(204,198,204)',
                                    'rgb(204,198,204)','rgb(204,198,204)','rgb(204,198,204)','rgb(204,198,204)',
                                    'rgb(204,198,204)','rgb(142,0,0)','rgb(204,198,204)','rgb(204,198,204)','rgb(204,198,204)',
                                    'rgb(204,198,204)')),
              customdata = MIP_Y2018_04$FullName,
              hovertemplate = 'ISO Code: %{x}, Value: %{y}, Country: %{customdata}')%>%
        #      layout(yaxis = list(range = c(-21.5,9))) %>%
        layout(title = paste("Year",input$internationaldates,sep=" "))%>%
        layout(xaxis = ax, yaxis = ax)%>%
        layout(plot_bgcolor='#EBEBEB')%>%
        layout(
          xaxis = list(tickfont = list(size = 14)),
          yaxis = list(tickfont = list(size = 14) )
        )
      
    }
    
    
    else if(input$internationaldates=="2007"&(input$rd=="All European countries")){
      
      MIP_Y2007$Country <- factor( MIP_Y2007$Country, levels = unique( MIP_Y2007$Country)[order(MIP_Y2007$GDP, decreasing = TRUE)])
      plot_ly(MIP_Y2007, x=~Country, y=~GDP, type = 'bar', name = 'GDP',
              marker = list(color=c('rgb(204,198,204)','rgb(204,198,204)','rgb(204,198,204)','rgb(204,198,204)','rgb(204,198,204)',
                                    'rgb(204,198,204)','rgb(204,198,204)','rgb(204,198,204)','rgb(204,198,204)','rgb(204,198,204)',
                                    'rgb(204,198,204)','rgb(204,198,204)','rgb(204,198,204)','rgb(204,198,204)','rgb(204,198,204)',
                                    'rgb(204,198,204)','rgb(204,198,204)','rgb(204,198,204)','rgb(204,198,204)','rgb(204,198,204)',
                                    'rgb(204,198,204)','rgb(142,0,0)','rgb(204,198,204)','rgb(204,198,204)','rgb(204,198,204)',
                                    'rgb(204,198,204)','rgb(204,198,204)','rgb(204,198,204)')),
              customdata = MIP_Y2018$FullName,
              hovertemplate = 'ISO Code: %{x}, Value: %{y}, Country: %{customdata}')%>%         
        #    layout(yaxis = list(range = c(-21.5,10.5)))  %>%
        layout(title = paste("Year",input$internationaldates,sep=" "))%>%
        layout(xaxis = ax, yaxis = ax)%>%
        layout(plot_bgcolor='#EBEBEB')%>%
        layout(
          xaxis = list(tickfont = list(size = 14)),
          yaxis = list(tickfont = list(size = 14) )
        )
    }
    
    
    else if((input$internationaldates=="2007")&(input$rd=="Italy, Greece and Spain")){
      
      
      MIP_Y2007_P$Country <- factor( MIP_Y2007_P$Country, levels = unique( MIP_Y2007_P$Country)[order(MIP_Y2007_P$GDP, decreasing = TRUE)])
      plot_ly(MIP_Y2007_P, x=~Country, y=~GDP, type = 'bar', name = 'GDP',
              marker = list(color=c('rgb(204,198,204)',
                                    'rgb(204,198,204)','rgb(204,198,204)','rgb(142,0,0)')),
              customdata = MIP_Y2018_P$FullName,
              hovertemplate = 'ISO Code: %{x}, Value: %{y}, Country: %{customdata}')%>%
        #   layout(yaxis = list(range = c(-14.5,3))) %>%
        layout(title = paste("Year",input$internationaldates,sep=" "))%>%
        layout(xaxis = ax, yaxis = ax)%>%
        layout(plot_bgcolor='#EBEBEB')%>%
        layout(
          xaxis = list(tickfont = list(size = 14)),
          yaxis = list(tickfont = list(size = 14) )
        )
      
    }
    
    
    else if((input$internationaldates=="2007")&(input$rd=="EU entry after 2004")){
      
      
      MIP_Y2007_04$Country <- factor( MIP_Y2007_04$Country, levels = unique( MIP_Y2007_04$Country)[order(MIP_Y2007_04$GDP, decreasing = TRUE)])
      plot_ly(MIP_Y2007_04, x=~Country, y=~GDP, type = 'bar', name = 'GDP',
              marker = list(color=c('rgb(204,198,204)',
                                    'rgb(204,198,204)','rgb(204,198,204)','rgb(204,198,204)','rgb(204,198,204)',
                                    'rgb(204,198,204)','rgb(204,198,204)','rgb(204,198,204)','rgb(204,198,204)',
                                    'rgb(204,198,204)','rgb(142,0,0)','rgb(204,198,204)','rgb(204,198,204)','rgb(204,198,204)',
                                    'rgb(204,198,204)')),
              customdata = MIP_Y2018_04$FullName,
              hovertemplate = 'ISO Code: %{x}, Value: %{y}, Country: %{customdata}')%>%
        #     layout(yaxis = list(range = c(-21.5,9))) %>%
        layout(title = paste("Year",input$internationaldates,sep=" "))%>%
        layout(xaxis = ax, yaxis = ax)%>%
        layout(plot_bgcolor='#EBEBEB')%>%
        layout(
          xaxis = list(tickfont = list(size = 14)),
          yaxis = list(tickfont = list(size = 14) )
        )
      
    }
    
    
  })
  
  observeEvent(input$reset5V, {
    reset("internationaldates")
    reset("rd")
  })
  
  output$video <- renderUI({
    tags$video(src = "DataVizMovie.mp4", type = "video/mp4", autoplay = NA, controls = NA,height = '450px', width = '100%', align = "center")
  })
  
  
}


### Kickstarter
shinyApp(ui, server)

