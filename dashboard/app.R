library(shiny)
library(bslib)
library(tidyverse)
library(leaflet)
library(sf)

source("map.R")

city_map  <- readRDS("city_chars.rds")
city_word <- readRDS("city_word.rds")

page_to_navbtn <- c(
  intro_p1   = "nav_intro_p1",  intro_p2   = "nav_intro_p2",
  sentiment  = "nav_sentiment", event      = "nav_event",
  did        = "nav_did",       conclusion = "nav_conclusion",
  authors    = "nav_authors"
)

navbtn_to_page <- c(
  nav_intro_p1   = "intro_p1",  nav_intro_p2   = "intro_p2",
  nav_sentiment  = "sentiment", nav_event      = "event",
  nav_did        = "did",       nav_conclusion = "conclusion",
  nav_authors    = "authors"
)

custom_css <- "
  /* ── Brand colours ── */
  :root { --maroon: #7a003c; --maroon-dark: #5c0030; --maroon-light: #f0e6ec; }

  /* ── Navbar ── */
  .navbar, .bslib-page-title { background-color: var(--maroon) !important; color: white !important; }
  .navbar-brand { color: white !important; font-size: 1.05rem; font-weight: 700; }

  /* ── Sidebar nav — shared base ── */
  .nav-btn, .nav-btn-sub {
    display: block; width: 100%; text-align: left;
    background: transparent; border: none; border-radius: 4px;
    cursor: pointer; margin-bottom: 2px; transition: background 0.15s, color 0.15s;
  }
  .nav-btn      { padding: 0.5rem 0.75rem; font-size: 0.875rem; color: #495057; }
  .nav-btn-sub  { padding: 0.4rem 0.75rem 0.4rem 1.4rem; font-size: 0.82rem; color: #6c757d; }
  .nav-btn:hover, .nav-btn-sub:hover { background: var(--maroon-light); color: var(--maroon); }
  .nav-btn.active  { background: var(--maroon); color: white; font-weight: 600; }
  .nav-btn-sub.active {
    background: var(--maroon-light); color: var(--maroon); font-weight: 600;
    border-left: 3px solid var(--maroon); padding-left: calc(1.4rem - 3px);
  }
  .nav-section-label {
    font-size: 0.65rem; text-transform: uppercase; letter-spacing: 0.08em;
    color: #6c757d; padding: 0 0.25rem; margin-bottom: 0.3rem;
  }

  /* ── Typography ── */
  .section-title {
    font-size: 1.55rem; font-weight: 700; color: var(--maroon);
    border-bottom: 2px solid var(--maroon); padding-bottom: 0.4rem; margin-bottom: 1.4rem;
  }
  .section-text { font-size: 0.93rem; line-height: 1.8; color: #343a40; }
  .section-text p { margin-bottom: 0.9rem; }

  /* ── Maroon left-border pattern (shared by intro, takeaway, anno-card) ── */
  .section-intro, .takeaway-box, .anno-card {
    border-left-width: 4px; border-left-style: solid;
    border-radius: 0 8px 8px 0; padding: 0.9rem 1.1rem;
    font-size: 0.93rem; line-height: 1.8;
  }
  .section-intro  { border-color: #c8506e; background: #faf6f8; color: #495057; margin-bottom: 1.6rem; }
  .takeaway-box   { border-color: var(--maroon); background: #fff8f9; color: #343a40; margin: 1.6rem 0; border-left-width: 5px; }
  .anno-card      { border-color: var(--maroon); background: #fff; border: 1px solid #e9ecef; border-left: 4px solid var(--maroon); color: #495057; height: 100%; font-size: 0.88rem; }
  .takeaway-box .takeaway-label, .anno-card .anno-title {
    font-size: 0.78rem; text-transform: uppercase; letter-spacing: 0.09em;
    color: var(--maroon); font-weight: 700; margin-bottom: 0.4rem;
  }
  .anno-card ul { margin: 0.4rem 0 0 0; padding-left: 1.1rem; }
  .anno-card ul li { margin-bottom: 0.45rem; }

  /* ── Layout helpers ── */
  .plot-row { margin-bottom: 1.8rem; }
  .bslib-sidebar-layout > .main { overflow-y: auto !important; max-height: 100vh; }

  /* ── Stat grid (About card) ── */
  .stat-grid { display: grid; grid-template-columns: 1fr 1fr; gap: 0.4rem; margin-top: 0.4rem; }
  .stat-cell { background: #f8f9fa; border: 1px solid #dee2e6; border-radius: 4px; padding: 0.45rem 0.6rem; }
  .stat-cell .stat-label { font-size: 0.6rem; text-transform: uppercase; color: #6c757d; }
  .stat-cell .stat-value { font-size: 0.9rem; font-weight: 600; }
  .stat-cell.treated .stat-value { color: #C0392B; }
  .stat-cell.control .stat-value { color: #2980B9; }
  .stat-cell.other   .stat-value { color: #6c757d; }

  /* ── Next page button ── */
  .next-btn-wrap { display: flex; align-items: center; justify-content: flex-end; margin-top: 2rem; padding-top: 1rem; border-top: 1px solid #e9ecef; }
  .next-page-btn { background: var(--maroon); color: white; border: none; border-radius: 6px; padding: 0.55rem 1.4rem; font-size: 0.9rem; font-weight: 600; cursor: pointer; transition: background 0.15s; }
  .next-page-btn:hover { background: var(--maroon-dark); }
  .page-counter { font-size: 0.78rem; color: #adb5bd; margin-right: auto; }

  /* ── Overview hero ── */
  .overview-hero { background: linear-gradient(135deg, var(--maroon) 0%, #a0004e 100%); border-radius: 10px; color: white; padding: 2.2rem 2rem; margin-bottom: 1.6rem; }
  .overview-hero h2 { font-size: 1.6rem; font-weight: 700; margin-bottom: 0.7rem; }
  .overview-hero p  { font-size: 0.97rem; line-height: 1.8; opacity: 0.92; margin: 0; }

  /* ── Author card ── */
  .author-card { background: white; border: 1px solid #e9ecef; border-top: 4px solid var(--maroon); border-radius: 8px; padding: 1.2rem 1.4rem; margin-bottom: 1rem; }
  .author-name  { font-size: 1.05rem; font-weight: 700; color: #2c2c2c; margin-bottom: 0.2rem; }
  .author-contact { font-size: 0.83rem; color: #495057; line-height: 1.7; border-top: 1px solid #f0e6ec; padding-top: 0.6rem; margin-top: 0.6rem; }
  .author-contact-label { font-size: 0.65rem; text-transform: uppercase; letter-spacing: 0.08em; color: var(--maroon); font-weight: 700; margin-bottom: 0.2rem; }
"

# UI
ui <- page_sidebar(
  title    = "Asylum Seekers and Canadian Voters: Examining the 2015 Refugee Crisis",
  theme    = bs_theme(version = 5, bootswatch = "flatly", primary = "#7a003c"),
  fillable = FALSE,
  tags$head(tags$style(HTML(custom_css))),
  
  sidebar = sidebar(
    width = 256, open = "open",
    div(class = "nav-section-label", "Introduction"),
    actionButton("nav_intro_p1", "Overview", class="nav-btn-sub active"),
    actionButton("nav_intro_p2", "Background & Lit Review", class="nav-btn-sub"),
    tags$hr(style = "margin: 0.5rem 0;"),
    div(class = "nav-section-label", style = "margin-top:0.2rem;", "Study"),
    actionButton("nav_sentiment", "Immigration Sentiment",class = "nav-btn"),
    actionButton("nav_event","Event Study",class = "nav-btn"),
    actionButton("nav_did","DiD Model & Analysis",class = "nav-btn"),
    actionButton("nav_conclusion", "Conclusion",class = "nav-btn"),
    actionButton("nav_authors","Authors",class = "nav-btn"),
    conditionalPanel(
      condition = "input.active_section === 'event'",
      tags$hr(),
      card(
        card_header(
          "Filter by Status"),
          checkboxGroupInput(
            "checkGroup", label = NULL,
            choices  = c("Treated", "Control", "other"),
            selected = c("Treated", "Control", "other"))
      )
    )
  ),
  
  # 
  div(
    id = "event_page_wrapper",
    style = "visibility:hidden; height:0; overflow:hidden;",
    div(class = "section-title", "Event Study: The 2015 Syrian Refugee Crisis"),
    div(class = "section-intro",
        "Distribution of Syrian Government-Assisted Refugees across Lower Mainland BC. Click any municipality to change the profile."),
    layout_columns(
      col_widths = c(5, 7), fill = FALSE,
      card(card_header("About: "),
           uiOutput("about_municipality_ui")),
      card(full_screen = TRUE,
           card_header("GAR Settlement Distribution"),
           leafletOutput("city_map_out", height = "500px"))
    ),
    div(class = "plot-row",
        layout_columns(
          col_widths = c(7, 5), fill = FALSE,
          card(card_header("Syrian Refugee Crisis Background"),
               div(class = "section-text",
                   tags$ul(
                     tags$li("The Syrian Refugee crisis began in March 2011 after a violent government crackdown on protests in Daraa, escalating into a civil war that forced millions to flee."
                    ),
                     tags$li("In 2015, it became internationally recognized, and Canada responded by rapidly resettling 25,000 refugees, with over 100,000 admitted to date."
                    ),
                     tags$li("There have been differing views, with some criticizing restrictive policies and others calling for stricter immigration amid pressures on housing and public services."
                    ),
                     tags$li("Despite Canada being seen as highly welcoming in 2019, by 2024 immigration became a top issue for many Canadians."
                    ),
                     tags$li("Recent debates, including Bill C-12, have increased concerns about refugee rights and risks of persecution and precarity."
                    )
                  )
                )
              ),
          anno_card(
            "Geographic Patterns",
            "Between November 2015 and December 2016, British Columbia received over 3,600 Syrian government-assisted refugees.",
            "1,881 settled in the Lower Mainland.",
            "Surrey received 1,082 GARs (about 43% of BC arrivals) due to affordable housing, available services, and established immigrant communities, making it our treated municipality.",
            "Abbotsford, Chilliwack, Maple Ridge, and Richmond received fewer GARs due to housing and community limitations, which are our control municipalities."
          )
        )
    ),
    takeaway(tags$p("Settlement was concentrated in a handful of larger cities, creating a meaningful treatment variation for an experiment.")),
    next_btn("next_event", "DiD Model & Analysis ›")
  ),
  
  uiOutput("main_content"),
  
  tags$script(HTML("
    // Attach Leaflet click handlers after map initialises
    $(document).on('shiny:connected', function() {
      var checkMap = setInterval(function() {
        var mapEl = document.getElementById('city_map_out');
        if (mapEl && mapEl._leaflet_map) {
          clearInterval(checkMap);
          var lmap = mapEl._leaflet_map;
          lmap.eachLayer(function(layer) {
            if (layer.on) {
              layer.on('click', function(e) {
                var id = e.layer ? e.layer.options.layerId
                                 : (e.target ? e.target.options.layerId : null);
                if (id) {
                  Shiny.setInputValue('city_map_out_shape_click',
                    {id: id, lat: e.latlng.lat, lng: e.latlng.lng},
                    {priority: 'event'});
                }
              });
            }
          });
        }
      }, 500);
    });

    Shiny.addCustomMessageHandler('setActiveNav', function(msg) {
      document.querySelectorAll('.nav-btn, .nav-btn-sub').forEach(function(el) {
        el.classList.remove('active');
      });
      var btn = document.getElementById(msg.btn);
      if (btn) btn.classList.add('active');
      Shiny.setInputValue('active_section', msg.section);
      var ep = document.getElementById('event_page_wrapper');
      var mc = document.getElementById('main_content');
      if (ep && mc) {
        if (msg.section === 'event') {
          ep.style.visibility = 'visible';
          ep.style.height = '';
          ep.style.overflow = '';
          mc.style.display = 'none';
        } else {
          ep.style.visibility = 'hidden';
          ep.style.height = '0';
          ep.style.overflow = 'hidden';
          mc.style.display = '';
        }
      }
    });
  "))
)

# SERVER
server <- function(input, output, session) {
  
  page <- reactiveVal("intro_p1")
  
  go_to <- function(pg) {
    page(pg)
    btn <- page_to_navbtn[[pg]]
    sec <- if (pg %in% c("intro_p1", "intro_p2")) "intro" else pg
    session$sendCustomMessage("setActiveNav", list(btn = btn, section = sec))
  }
  
  lapply(names(navbtn_to_page), function(btn) {
    observeEvent(input[[btn]], { go_to(navbtn_to_page[[btn]]) }, ignoreInit = TRUE)
  })
  
  observeEvent(input$next_intro_p1,   { go_to("intro_p2") })
  observeEvent(input$next_intro_p2,   { go_to("sentiment") })
  observeEvent(input$next_sentiment,  { go_to("event") })
  observeEvent(input$next_event,      { go_to("did") })
  observeEvent(input$next_did,        { go_to("conclusion") })
  observeEvent(input$next_conclusion, { go_to("authors") })
  
  # Map 
  selected_city <- reactiveVal(sort(unique(city_map$name))[1])
  
  observeEvent(input$city_map_out_shape_click, {
    click <- input$city_map_out_shape_click
    if (!is.null(click$id) && click$id %in% city_map$name)
      selected_city(click$id)
  })
  
  observe({
    req(selected_city())
    match_row <- city_map %>% filter(name == selected_city())
    if (nrow(st_drop_geometry(match_row)) == 0) return()
    leafletProxy("city_map_out") %>%
      clearGroup("highlight") %>%
      addPolygons(data = match_row, group = "highlight",
                  fillColor = "transparent", color = "#E67E22",
                  weight = 3, opacity = 1, fillOpacity = 0)
  })
  
  output$city_map_out <- renderLeaflet({
    make_map(city_map %>% filter(did_status %in% input$checkGroup))
  })
  
  # About
  output$about_municipality_ui <- renderUI({
    city <- selected_city()
    req(city)
    row <- city_map %>% filter(name == city) %>% slice(1)
    if (nrow(st_drop_geometry(row)) == 0) return(NULL)
    
    status_label <- if (is.na(row$did_status)) "Other" else row$did_status
    colour <- switch(tolower(status_label),
                     treated = "#C0392B", control = "#2980B9", "#6c757d")
    
    tagList(
      div(style = paste0("font-size:1.1rem; font-weight:700; color:#2c2c2c;",
                         "margin-bottom:0.2rem;"), city),
      div(style = paste0("font-size:0.78rem; text-transform:uppercase;",
                         "letter-spacing:0.07em; color:", colour,
                         "; font-weight:600; margin-bottom:0.8rem;"), status_label),
      div(class = "stat-grid",
          div(class = "stat-cell",
              div(class = "stat-label", "GAR Count"),
              div(class = "stat-value", formatC(row$GAR, format = "d", big.mark = ","))
          ),
          div(class = "stat-cell",
              div(class = "stat-label", "% of Population"),
              div(class = "stat-value", paste0(round(row$gar_pct, 3), "%"))
          )
      ),
      tags$hr(style = "margin:0.8rem 0;"),
      div(style = "font-size:0.88rem; line-height:1.8; color:#343a40;",
          make_city_narrative(city, city_word)
      )
    )
  })
  outputOptions(output, "about_municipality_ui", suspendWhenHidden = FALSE)

  # main
  output$main_content <- renderUI({
    
    pg <- page()
    
    if (pg == "intro_p1") {
      tagList(
        div(class = "section-title", "Overview"),
        div(class = "overview-hero",
            tags$h2("Asylum Seekers and Canadian Voters: Examining the 2015 Refugee Crisis")
        ),
        layout_columns(
          col_widths = c(6, 6), fill = FALSE,
          card(card_header("What We Are Doing"),
               div(class = "section-text",
                   tags$p("Our Capstone project examines the relationship between federal voting and immigration sentiment at two levels: Canada wide and in Lower Mainland, BC."
                  ),
                   tags$p("We look into whether anti-immigrant sentiment is associated with Conservative vote intention using sentiment analysis, as a view on if immigration attitudes are linked to voting behaviour. Use the 2015 Syrian Refugee Crisis as a quasi-experimental shock to test whether refugee exposure causally increases Conservative support, hypothesizing that negative sentiment is associated with Conservative voting and that refugee influx leads to greater right-wing support."
                  )
                )
          ),
          card(card_header("Our Goal"),
               div(class = "section-text",
                   tags$p("Given that our Capstone theme is on extremism, our goal is to explore:"),
                   tags$ul(
                     tags$li("Social and political mechanisms of right-wing extremism."),
                     tags$li("Extremism through shifts in Conservative vote intention and anti-immigrant sentiment, similar to Carter (2018)."),
                     tags$li("Whether a systematic association between negative immigration attitudes and support for anti-immigrant parties reflects xenophobic political mobilization.")
                   )))
        ),
        layout_columns(
          col_widths = c(4, 4, 4), fill = FALSE,
          card(card_header("Research Questions"),
               div(class = "section-text",
                   tags$ul(
                     tags$li("What is the relationship between immigration and partisanship?"),
                     tags$li("What is the impact of large refugee flows on voting intentions in Canada?")
                   ))),
          card(card_header("Data Sources"),
               div(class = "section-text",
                   tags$p("Innovative Research Group's surveys in April from 2012 to 2018"),
                   tags$p("Innovative Research Group's November 2025 survey"),
                   tags$p("Immigrant Services Society of B.C, 2017"))),
          card(card_header("Our Method"),
               div(class = "section-text",
                   tags$ul(
                     tags$li("A review of how to define right-wing extremism."),
                     tags$li("An immigrant sentiment analysis on vote shares."),
                     tags$li("A difference-in-difference analysis on the Syrian Refugee Crisis.")
                   )))
        ),
        next_btn("next_intro_p1", "Background & Literature >", "Page 1 of 2")
      )
      
    } else if (pg == "intro_p2") {
      tagList(
        div(class = "section-title", "Background & Literature Review"),
        layout_columns(
          col_widths = c(6, 6), fill = FALSE,
          card(card_header("Background"),
               div(class = "section-text",
                   tags$p("In 2015, the Syrian civil war caused one of the biggest refugee crises worldwide in recent years. In response, the Canadian government initiated a major refugee resettlement program and welcomed a significant number of Government-Assisted Refugees (GARs)."),
                   tags$p("This policy aimed to provide safe haven for displaced persons and promote their long-term integration into Canadian society. However, the rapid inflow of refugees led to extensive public discussions about the possible effects of immigration on local communities, public services, and political views."),
                   tags$p("Immigration has become an important political issue in many advanced democracies because the change in population composition may influence public opinion and electoral results. Many studies show that sudden refugee inflow is related to a change in political views."))),
          card(card_header("Literature Review"),
               div(class = "section-text",
                   tags$p("Elizabeth Cater (2018) redefined right-wing extremism/radicalism using Mudde's 1995 study as a foundation, arguing for a definition encompassing authoritarianism, anti-democracy and exclusionary nationalism."),
                   tags$p("Salathé  et al (2025) reviewed 123 quantitative studies; their analysis showed that refugee shocks tend to be associated with an increase in votes for the radical right."),
                   tags$p("Dinas et al. (2019) and Altindăg and Neeraj (2021) found that refugee arrivals did fuel support for right-wing politics, although with small effects on voters and a negligible effect on actual voting outcomes."),
                   tags$p("Results across studies, including Campo et al. (2024) and multiple studies in Germany, showed that while the refugee crisis significantly increased support for anti-immigration parties, the impact is small in magnitude.")))
        ),
        next_btn("next_intro_p2", "Immigration Sentiment >", "Page 2 of 2")
      )
      
    } else if (pg == "sentiment") {
      tagList(
        div(class = "section-title", "Immigration Sentiment Analysis"),
        div(class = "section-intro",
            "To explore the association between federal voting and immigration sentiment, we use a statement where respondents were asked whether they agreed, disagreed or neither that Canada has admitted too many new immigrants in the last five years. Figure 1, which shows the distribution of responses within each federal party's voter base, and used an ordinal logistic regression to test variation in federal party support and other associated characteristics."),
        plot_row(
          "barboy.png",
          "Anti-Immigrant Sentiment Across Party Voters",
          "Immigration sentiment varies with federal party support.",
          "Left-leaning voters (NDP, Green) mostly disagree with the statement. Liberal voters disagree at 62% while 38% agree.",
          "Conservative voters show a positive association, with only 14% who disagree and 42% who agree, indicating negative attitudes on immigration are concentrated among right-leaning voters.",
          "People Party (PPC) is inconsistent due to its small sample size of 21 observations, but the overall party choice varies with views on immigration."
        ),
        plot_row(
          "linesboyyy.png",
          "Variation In Immigration Support",
          "A weighted ordinal logistic regression tests the relationship between immigration sentiment and federal party vote intention, with Liberals as the reference category and controls for age, income, education, gender, and province.", "Conservative voters show 2.49 times higher odds of agreeing than Liberal voters (p<0.001).",
          "NDP voters show 34% lower odds (p<0.1).", "Green and PPC voters are not statistically significant, but Green voters are less likely and PPC voters more likely to agree. Staying inline with party support variation"
        ),
        div(class = "plot-row",
            layout_columns(
              col_widths = c(7, 5), fill = FALSE,
              card(tags$img(src = "table1.png",
                            style = "width:100%; border-radius:4px; display:block;"
                          )
                        ),
              anno_card(
                "Ordinal Logistic Regression",
                "Several individual-level characteristics are independently associated with immigration sentiment beyond party affiliation.", "Lower education levels are associated with higher odds of agreeing (47 - 56% higher than university-educated individuals).", "Respondents aged 35 to 54 show 43% higher odds than those aged 18 to 34.", "Residents of Quebec, British Columbia, and Atlantic provinces show lower odds than Ontario residents, showing a regional variation."
              )
            )),
        takeaway(tags$p("Immigration sentiment in Canada is not randomly distributed but associated with both political affiliation and socioeconomic characteristics. Conservative supporters show significantly higher odds of agreeing, while NDP supporters show the opposite preference, representing the shift in sentiment as we move from left to right on a political spectrum.")),
        next_btn("next_sentiment", "Event Study >")
      )
      
    } else if (pg == "did") {
      tagList(
        div(class = "section-title", "Difference-in-Differences Model & Analysis"),
        div(class = "section-intro",
            "Having established that immigration attitudes vary systematically by party, we now examine whether refugee exposure shifted voting intentions using a Difference-in-Differences (DiD) approach to identify causal effects. We compare Surrey to similar BC cities with lower refugee intake and track how differences evolve after 2015 relative to the pre-2015 period to isolate the impact of resettlement."),
        plot_row(
          "precheck.png",
          "Parallel Trends Check",
          "Before interpreting results, we assess whether Surrey and the control cities are comparable, ensuring any post-2015 differences reflect resettlement effects rather than pre-existing trends.",
          "The control cities act as a counterfactual for what Surrey would have looked like without large-scale refugee resettlement.",
          "From 2012 to 2015, Surrey consistently had higher Conservative support than the control cities.", "Trend patterns are mixed, with similar movement in 2012 - 2013 and slight divergence in 2013 - 2014."
        ),
        plot_row(
          "event.png",
          "Event Study Estimates",
          "The event study tests whether pre-treatment differences between Surrey and the control cities were due to chance and whether they followed similar trends before resettlement.",
          "Before 2015, none of the gaps are statistically significant, confidence intervals include zero, indicating similar pre-treatment trends.", "After 2015, all estimates are negative, indicating a decline in Conservative support in Surrey relative to control cities.", "However, post-treatment results are not statistically significant, so the pattern may be due to chance."
        ),
        plot_row(
          "results.png",
          "Actual vs Counterfactual",
          "Figure 6 compares Surrey's actual Conservative support to a counterfactual trend based on control cities, illustrating the estimated impact of refugee resettlement.",
          "The green line shows Surrey's actual Conservative support; the blue dashed line represents the counterfactual.", "The main estimate shows a -0.067 coefficient, indicating an average decline of about 6.7 percentage points in Conservative vote intention.", "This result is not statistically significant, reflecting uncertainty due to limited data.", "Despite this, the model rules out large increases in Conservative support."
        ),
        plot_row(
          "table2.png",
          "DiD Regression Results",
          "The DiD results show that the large-scale arrival of Syrian refugees in Surrey was not associated with a statistically significant increase in Conservative vote intention.",
          "The estimated effect is a 6.7 percentage point drop in Conservative support.", "The absence of backlash may be explained by the humanitarian framing of the crisis rather than a security or economic threat.", "Canada's structured resettlement approach likely reduced visible disruption.", "Surrey's existing infrastructure and diverse community may have further limited negative reactions."
        ),
        takeaway(
          tags$p("DiD results indicate that the large-scale arrival of Syrian refugees in Surrey had no statistically significant effect on Conservative vote intention.")
        ),
        next_btn("next_did", "Conclusion >")
      )
      
    } else if (pg == "conclusion") {
      tagList(
        div(class = "section-title", "Conclusion"),
        layout_columns(
          col_widths = c(6, 6), fill = FALSE,
          card(card_header("Conclusion"),
               div(class = "section-text",
                   tags$ul(
                     tags$li("Right-wing extremism can manifest through anti-democratic values, with secondary forms such as xenophobia emerging during perceived crises. This study examines whether refugee arrivals shape voting behaviour in Canada."
                    ),
                     tags$li("Consistent with European studies, the findings show no statistically significant effect of refugee arrivals on right-wing voting."
                    ),
                     tags$li("Sentiment analysis reveals a positive correlation between Conservative support and anti-immigrant attitudes, shaped by socioeconomic factors and regional differences."
                    ),
                     tags$li("The DiD results fail to reject the null hypothesis, suggesting refugee influx did not significantly affect Conservative support."
                    ),
                     tags$li("The humanitarian framing of the Syrian crisis likely reduced threat perceptions and limited political backlash."
                    ),
                     tags$li("While no electoral backlash is found, anti-immigrant attitudes remain present, indicating that immigration's political impact may evolve with changing economic and social conditions."
                    )
                   ))),
          card(card_header("Policy Implications"),
               div(class = "section-text",
                   tags$ul(
                     tags$li("Large-scale resettlement did not lead to increased right-wing support, and while effects on voting are uncertain, substantial backlash can be ruled out."
                    ),
                     tags$li("Immigration attitudes in 2025 remain linked to Conservative support, showing that immigration can become politically salient under certain conditions."),
                     tags$li("Economic pressures, such as rising costs of living, may shape how immigration is perceived compared to the 2015 context."
                    ),
                     tags$li("Policymakers can reduce polarization by maintaining strong settlement systems and addressing broader economic concerns."
                    ),
                     tags$li("Compared to mixed findings in Europe, the Canadian case aligns with evidence showing limited electoral effects, though changing conditions may alter future outcomes."
                    )
                   )))
        ),
        takeaway(tags$p("While our findings suggest that there was no significant electoral backlash in Canada during the 2015 refugee crisis, our sentiment analysis indicates that anti-immigrant attitudes remain associated with conservative support. Suggesting an evolved political landscape surrounding immigration, opening up to future research as economic and social conditions continue to shape public opinion.")),
        next_btn("next_conclusion", "Authors >")
      )
      
    } else if (pg == "authors") {
      tagList(
        div(class = "section-title", "Authors"),
        layout_columns(
          col_widths = c(6, 6), fill = FALSE,
          author_card("Cho Yi Ho", "Department of Global Asia", "cyh21@sfu.ca "),
          author_card("Jason Zhou", "Department of Economics", "jza311@sfu.ca ")
        ),
        layout_columns(
          col_widths = c(6, 6), fill = FALSE,
          author_card("Kennedy Jokonya", "Department of Economics", "zvikomborero_jokonya@sfu.ca"),
          author_card("Thwin Than Thar Nway", "Department of Political Science & International Studies", "thwin_nway@sfu.ca")
        ),
        div(style = "margin-top: 1.5rem;",
            div(style = paste0(
              "background: #fff8f9; border: 1px solid #f0e6ec;",
              "border-top: 4px solid #7a003c; border-radius: 8px;",
              "padding: 1.4rem 1.8rem; text-align: center;"
            ),
            div(style = "font-size:1rem; font-weight:700; color:#7a003c; margin-bottom:0.6rem;",
                "Acknowledgement"),
            div(style = "font-size:0.9rem; line-height:1.8; color:#495057;",
                tags$p("We would like to thank Kevin Schnepel and Steven Weldon, our instructors in SDA 490 for their guidance, feedback, and
                        support throughout this project."),
                tags$p("And thank you for your time and interest in reading our research.")
            ))
        )
      )
    }
  })
}

shinyApp(ui, server)