library(shiny)
library(deSolve)
library(tidyverse)
ui <- pageWithSidebar(
  headerPanel("The SIR model"),
  #The sidebar for parameter input
  sidebarPanel(
    #Sliders:
    sliderInput("R0", "R0:", 2,
                min = 0.5, max = 20),
    sliderInput("R0_r", "R0_r:", 2,
                min = 0.5, max = 20),
    sliderInput("infper", "Infectious period (days)", 5,
                min = 1, max = 100),
    sliderInput("b", "birth rate (yr^-1):", 5,
                min = 0, max = 100),
    sliderInput("T", "Time range:",
                min = 0, max = 1, value = c(0,1)),
  ),
  #Main panel for figures and equations
  mainPanel(
    #Multiple tabs in main panel
    tabsetPanel(
      #Tab 1: Time plot (plot1 from server)
      tabPanel("Time", plotOutput("plot1")),
  tabPanel("Time", plotOutput("plot_rats")),
)))

server <- function(input, output) {
  #Gradient function for SIR model
  sirmod = function(t, x, parameters){
    # S_h=x[1]
    # I_h=x[2]
    # R_h=x[3]
    # S_r=x[4]
    # I_r=x[5]
    # R_r=x[6]
    # Fl=x[7]
    # H=x[8]


    #D=x[9]
    #D_r=x[10]
    #R0=parameters["R0"]
    #R0_r=parameters["R0_r"]
    # N_h=parameters["N"]
    # alpha=parameters["alpha"]#flea searching efficiency
    # gamma=parameters["gamma"]#1/gamma=infectious period, gamma=
    # #N_r=parameters["N_r"]
    # #N=parameters["N"]
    # d_h=parameters["d_h"]#death rate
    # b_h=parameters["b_h"]#birth rate
    # g_h=parameters["g_h"]#probability of recovery
    # #gamma_r=parameters["gamma_r"]#
    # g_r=parameters["g_r"]
    # r_f=parameters["r_f"]#growth rate of fleas
    # K_f=parameters["K_f"]#flea carrying capacity (per rat)
    # d_f=parameters["d_f"]#flea lifespan
    # b_r=parameters["b_r"]
    # #beta=R0*(gamma+b)
    #beta_r=R0_r*(gamma_r+b_r)#okay so solve this for R0_r with given beta_r to get range
    #1.248/((1/5.15)+0.0014)=6.38

    with(as.list(c(x, parameters)),{
    #N_h = S_h + I_h + R_h
    N_r = S_r + I_r + R_r

    # human dynamics
   # dS_h = r_h * (S_h + R_h) - d_h * S_h - beta_h * S_h * Fl * exp(-1 * alpha * N_r) # ignores density dependence
  #  dI_h = beta_h * S_h * Fl * exp(-1 * alpha * N_r) - (d_h + m_h) * I_h
   # dR_h = m_h * g_h * I_h - d_h * R_h
    #dD = (1-g)*gamma * I

    # rat dynamics
    dS_r = r_r * S_r * (1 - N_r / K_r) + r_r * R_r * (1 - p) - d_r * S_r - beta_r * S_r / N_r * Fl * (1 - exp(-1* alpha * N_r))
    dI_r = beta_r * S_r / N_r * Fl * (1 - exp(-1 * alpha * N_r)) - (d_r + m_r) * I_r
    dR_r = r_r * R_r * (p - N_r / K_r) + m_r * g_r * I_r - d_r * R_r
    #dD_r = (1-g_r)*gamma_r * I_r

    # flea dynamics
    dH = r_f * H * (1 - H / K_f) + d_f / N_r  * Fl * (1 - exp(-1 * alpha * N_r)) # should thislast part be included?
    dFl = (d_r + m_r * (1 - g_r)) * I_r * H - d_f * Fl

    return(list(c(#dS_h, dI_h, dR_h,
                  dS_r, dI_r, dR_r, dH, dFl)))
    })
  }

  #S_0 <- 5000 # Initial condition- number of humans
  #N_r0 <- S_0 # Initial condition- number of rats

  #Plot1: renderPlot to be passed to UI tab 1
  output$plot1 <- renderPlot({

    times  = seq(0, 40, by = 0.01)

    parameters = c(r_r = 5, # rat reproductive rate -- 5 / 365 for days?
                   p = 0.975, # probability of inherited resistance -- or 0.65 or 0.975
                   K_r = 2500, # rat's carrying capacity
                   d_r = 0.2, # death rate of rats
                   beta_r = 4.7 , # transmission rate -- 1.248 [0–3.67] fleas-1 days-1, keeling has 4.7 "per year"!
                   m_r = 20, # infectious period ^-1
                   g_r = 0.02, # probability of recovery -- 0.06 [0.0–0.37], .02 in keeling, 0.1 dean
                 #  mu_r = 0.03 , # movement rate of rats
                   alpha =  0.004, # flea searching efficiency -- 3.0/Sr0 dean, 0.004 keeling
                   r_f = 20, # flea's reproductive rate, .0084 (white) to 20/365 = 0.05479452 white translation of keeling, but other translation is 20/day which is reasonable
                   d_f = 10, # death rate of fleas -- elsewhere 1/5 days, 10/day (?)
                   K_f = 6.57#, # flea carrying capacity per rat
                #   mu_f = 0.008, # movement rate of fleas
                #   r_h = 0.0455, # reproductive rate of humans
                #   d_h = 0.04 , # death rate of humans
                 #  beta_h = 0.01, # transmission rate to humans
                #   m_h = 26, # infectious period ^-1, 3-10 days?
                 #  g_h = 0.4 # probability of recovery-- 0.4 dean -- 0.34 [0.30, 0.40] white, .1 keeling
    )

    start = c(#S_h = 5000, I_h = 0, R_h = 0,
      S_r = 2500, I_r = 1, R_r = 0, H = 1, Fl = 0)

    out <- ode(start, times, sirmod, parameters) %>%
      as.data.frame() %>%
      as_tibble() %>%
      mutate(#N_h = S_h + R_h + I_h,
             N_r = S_r + R_r + I_r,
             lambda = Fl * exp(-parameters['alpha'] * N_r)) %>%
      select(time, I_r, lambda) %>%
      pivot_longer(-time)

    ggplot(out, aes(time, value, color = name)) +
      geom_line() +
      scale_y_log10()
  })

}
shinyApp(ui=ui, server=server)
