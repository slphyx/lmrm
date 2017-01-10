#shiny version of Lao Model
library(deSolve)
library(shiny)
library(TSA)


ui <- fluidPage(
  tabsetPanel(
    id="panels",
    tabPanel(title = strong("Intervention parameters"),
             column(3,
                    wellPanel(
                      numericInput(inputId="timei", label = "timing of intervention", value = 2017.5),
                      numericInput(inputId="scalei", label = "number of years required to reach intervention targets", value = 1),
                      checkboxInput(inputId="EDATon", label = "EDAT", value = TRUE),
                      checkboxInput(inputId="ITNon", label = "ITN", value = TRUE),
                      checkboxInput(inputId="RCDon", label = "RCD    ", value = TRUE),
                      checkboxInput(inputId="CHWon", label = "CHW  ", value = TRUE),
                      checkboxInput(inputId="MASSon", label = "MDA plus mass RTS,S", value = TRUE)
                    ),
                    sliderInput(inputId="covEDATi", label = "proportion of all clinical cases that receive treatment", value = 0.7, min=0,max=1),
                    sliderInput(inputId="covITNi", label = "coverage of ITN", value = 0.9, min=0,max=1),
                    sliderInput(inputId="effITN", label = "efficacy of ITN in reducing force of infection", value = 0.15, min=0,max=1)
                    ),
             column(3,
                    #numericInput(inputId="durITN", label = "duration of efficay of ITN", value = 3),
                    sliderInput(inputId="covRCDi", label = "coverage of RCD", value = 0.75, min=0,max=1),
                    sliderInput(inputId="effRCD", label = "RCD impact: additional clinical cases detected", value = 1.0, min=0,max=1),
                    sliderInput(inputId="covCHWi", label = "CHW coverage (%)", value = 0.9, min=0,max=1),
                    sliderInput(inputId="effCHW", label = "efficacy of f extended role CHW in reducing fatigue ", value = 0.9, min=0,max=1),
                    sliderInput(inputId="cm_1", label = "coverage of first MVDA", value = 0.8, min=0,max=1)
                    ),
             column(3,
                    sliderInput(inputId="cm_2", label = "coverage of second MVDA", value = 0.95, min=0,max=1),
                    sliderInput(inputId="cm_3", label = "coverage of third MVDA", value = 0.95, min=0,max=1),
                    numericInput(inputId="dm", label = "months to complete each round of MVDA", value = 1/12),
                    numericInput(inputId="lossd", label = "duration of protection MVDA (days)", value = 365/10),
                    sliderInput(inputId="cmda_1", label = "effective population coverage of focal MDA in round 1 - means the proportion of the ENTIRE pop covered with MDA if focal MDA deployed in around 30% of the villages", value = 0.5, min=0,max=1)
                    ),
             column(3,
                    sliderInput(inputId="cmda_2", label = "effective population coverage of focal MDA in round 2", value = 0.5, min=0,max=1),
                    sliderInput(inputId="cmda_3", label = "effective population coverage of focal MDA in round 3", value = 0.5, min=0,max=1),
                    sliderInput(inputId="effv_1", label = "protective efficacy of a single dose of RTS,S over one year", value = 0.05, min=0,max=1),
                    sliderInput(inputId="effv_2", label = "protective efficacy of two doses of RTS, over one year", value = 0.2, min=0,max=1),
                    sliderInput(inputId="effv_3", label = "protective efficacy of three doses of RTS,S over one year", value = 0.5, min=0,max=1),
                    numericInput(inputId="dv", label = "duration of vaccine protection", value = 1)
                    )
             
             ),
    tabPanel(title = strong("Typology parameters"),
             column(3,
                    numericInput(inputId="R0", label = "basic reproduction number", value = 3.0),
                    sliderInput(inputId="eta", label = "proportion of infections transmitted in the forest", value = 0.5, min=0,max=1),
                    sliderInput(inputId="covEDAT0", label = "baseline proportion of all clinical cases that receive treatment", value = 0.3, min=0,max=1),
                    numericInput(inputId="nuTr", label = "recovery under successful treatement with ACT", value = 365/3)
                    ),
             column(3,
                    sliderInput(inputId="covITN0", label = "baseline coverage of ITN", value = 0.5, min=0,max=1),
                    sliderInput(inputId="covRCD0", label = "baseline coverage of RCD (reactive case detection)", value = 0.0, min=0,max=1),
                    sliderInput(inputId="covCHW0", label = "baseline coverage of CHWs", value = 0.0, min=0,max=1),
                    numericInput(inputId="amp", label = "relative amplitude seasonality", value = 0.7)
                    ),
             column(3,
                    numericInput(inputId="phi", label = "phase angle seasonality", value = 0.5),
                    numericInput(inputId="muC", label = "rate of in-migration of clinical cases = number of cases per person (in current pop) per year", value = 0.00),
                    numericInput(inputId="muA", label = "rate of in-migration of patent, asymptomatic infections = number of new people per person (in current pop) per year", value = 0.0),
                    numericInput(inputId="muU", label = "rate of in-migration of sub-microscopic asymptomatic infections = number of new people per person (in current pop) per year", value = 0.0)
                    ),
             column(3,
                    numericInput(inputId="kf", label = "maximum fatigue due to low proportion testing positive (0 to 1)", value = 0.9),
                    numericInput(inputId="presa", label = "proportion of all infections resistant to artemisinin at start of simulation", value = 0.025),
                    numericInput(inputId="presp", label = "proportion of all infections resistant to partner drug 1 at start of simulation must be less than presa if double resistance", value = 0.0025),
                    checkboxInput(inputId="double", label = "is the partner drug 1 resistance double resistance? Tick if yes?", value = TRUE)
             )
    ),
    tabPanel(title = strong("Biological parameters"),
             column(3,
                    numericInput(inputId="omega", label = "rate of loss of immunity = 1/(average duration of immunity in years)", value = 1/2),
                    numericInput(inputId="nuC", label = "rate of loss of symptoms in the absence of treatment (days)", value = 365/10),
                    numericInput(inputId="nuA", label = "1/(duration of patent asymptomatic infection)", value = 365/30)
             ),
             column(3,
                    numericInput(inputId="nuU", label = "1/(duration of sub-microscopic asymptomatic infection)", value = 365/120),
                    numericInput(inputId="rhoa", label = "relative infectivity of patent asymptomatic infections compared with clinical infections", value = 0.7)
             ),
             column(3,
                    numericInput(inputId="rhou", label = "relative infectivity of sub-microscopic asymptomatic infections compared with clinical infections", value = 0.3),
                    sliderInput(inputId="ps", label = "proportion of all new infections in non-immune hosts that are clinical", value = 0.9, min=0,max=1),
                    sliderInput(inputId="pr", label = "proportion of all new infections in immune  hosts that are clinical", value = 0.2, min=0,max=1)
             ),
             column(3,
                    numericInput(inputId="mu", label = "birth/death rate", value = 1/50),
                    numericInput(inputId="incnm", label = "incidence of non-malaria febrile illness", value = 2)
             )
    ),
    tabPanel(title = strong("Resistance spread parameters"),
             column(3,
                    sliderInput(inputId="fa", label = "proportion of cases that fail on artemisinin mono therapy", value = 0.1, min=0,max=1),
                    numericInput(inputId="ka", label = "speed of fixation of artemisinin resistance with respect to cumulative exposure", value = 5),
                    numericInput(inputId="ha", label = "number of exposures to artemisinin mono therapy required for a selective sweeep for artemisinin resistance", value = 1e+09)  
             ),
             column(3,
                    sliderInput(inputId="fp", label = "proportion of partner drug 1 resistant cases that fail", value = 0.3, min=0,max=1),
                    numericInput(inputId="kp", label = "speed of fixation of partner drug 1 resistance", value = 5),
                    numericInput(inputId="hp", label = "number of exposures to ACT 1 required for a selective sweeep for partner drug 1 resistance", value = 1e+08)
             ),
             column(3,
                    numericInput(inputId="hap", label = "number of exposures to ACT 1 required for a selective sweeep for double resistance given artemisnin resistance", value = 1e+07),
                    numericInput(inputId="hpa", label = "number of exposures to ACT 1 required for a selective sweeep for double resistance given partner drug 1 resistance", value = 1e+08)
             ),
             column(3,
                    sliderInput(inputId="fap", label = "proportion of ACT1 resistant cases that fail to clear infection", value = 0.9, min=0,max=1),
                    numericInput(inputId="yatoap", label = "timing of switch from mono to ACT1", value = 2000)
             )
    )
  ),
  fluidRow(plotOutput(outputId = "lao")),
  br(),
  br(),
  br(),
  br(),
  br(),
  br(),
  br(),
  br(),
  br(),
  br(),
  br(),
  br(),
  br(),
  br(),
  br(),
  br(),
  br(),
  br(),
  br(),
  br(),
  hr(),
  fluidRow(h4("Legend. Black lines: “do nothing more”. Red lines: model result with the selected parameter values."))
  
)

server <- function(input, output) {
  output$lao <- renderPlot({
    # define the number of weeks to run the model
    dt<-1/12
    startyear<-2016
    stopyear<-2021
    maxt<-stopyear-startyear
    times <- seq(0, maxt, by = dt)
    tsteps<-length(times)
    
    # initial proportions resistant
    # presa <- 0.025       # proportion of all infections resistant to artemisin at start of simulation
    # presp <- 0.0025     # proportion of all infections resistant to partner drug 1 at start of simulation must be less than presa if double resisatnce
    # double <- 1         # is the partner drug 1 resistance double resistance? y=1, n=0 
    presa = input$presa
    presp = input$presp
    double = input$double
    
    # initial prevalence
    initprev<-0.1
    
    
    if (presp*double>presa){
      print("ERROR: chose a lower value for presp")
    }
    
    
    # ENTER COUNTERFACTUAL AND INTERVENTION SCENARIOS
    # 1 = time to start intervention
    # 2 = time taken to scale up EDAT, ITN, RCD and/or CHW to target coverage
    # 3 = include EDAT scaleup 1-yes, 0-no
    # 4 = include ITN scaleup 1-yes, 0-no
    # 5 = include MDA plus mass-RDT 1-yes, 0-no
    # 6 = include RCD scaleup 1-yes, 0-no
    # 7 = include CHW scaleup 1-yes, 0-no
    scenario_0<-c(input$timei,input$scalei,0,0,0,0,0)
    scenario_i<-c(input$timei,input$scalei,input$EDATon,input$ITNon,input$MASSon,input$RCDon,input$CHWon)
    
    runLAO<-function(scenario) 
    {
      
      #MODEL PARAMETERS
      parameters <- c(
        # intervention parameters
        timei = scenario[1],         # timing of intervention
        scalei = scenario[2],        # number of years required to reach intervention targets
        EDATon = scenario[3],        # switch on scale up of EDAT
        ITNon = scenario[4],         # switch on scale up of ITN
        RCDon = scenario[6],         # switch on scale up of RCD    
        CHWon = scenario[7],         # switch on scale up of CHW  
        MASSon = scenario[5],        # switch on MDA plus mass RTS,S
        # covEDATi = 0.7,      # intervention proportion of all clinical cases that receive treatment
        # covITNi = 0.9,       # intervention coverage of ITN
        # effITN = 0.15,       # efficacy of ITN in reducing force of infection
        # durITN = 3,          # duration of efficay of ITN
        # covRCDi = 0.75,      # intervention coverage of RCD
        # effRCD = 1.0,        # efficacy of RCD measured as the avereage number of additional clinical cases detected per clinical case reported 
        # covCHWi = 0.9,       # intervention coverage of extended role VMWs
        # effCHW = 0.9,        # efficacy of f extended role VMWs in reducing fatigue 
        # 
        # cm_1 = 0.8,           # coverage of first mass intervention round 
        # cm_2 = 0.95,          # coverage of second mass intervention round
        # cm_3 = 0.95,          # coverage of third mass intervention round
        covEDATi = input$covEDATi,
        covITNi = input$covITNi,
        effITN = input$effITN,
        #durITN = input$durITN,
        covRCDi = input$covRCDi,
        effRCD = input$effRCD,
        covCHWi = input$covCHWi,
        effCHW = input$effCHW,
        cm_1 = input$cm_1,
        cm_2 = input$cm_2,
        cm_3 = input$cm_3,
        
        
        tm_1 = scenario[1]+(0/12),   # timing of first round
        tm_2 = scenario[1]+(1/12),   # timing of second round
        tm_3 = scenario[1]+(2/12),   # timing of third round
        
        # 
        # dm = 1/12,            # time taken to reach coverage in each round: coverage means screening and vaccination coverage NOT population MDA coverage
        # 
        # lossd = 365/10,       # rate of loss of profilactic effect following MDA
        # cmda_1 = 0.5,         # effective population coverage of focal MDA in round 1 - means the proportion of the ENTIRE pop covered with MDA if focal MDA deployed in around 30% of the villages
        # cmda_2 = 0.5,         # effective population coverage of focal MDA in round 2
        # cmda_3 = 0.5,         # effective population coverage of focal MDA in round 3
        # 
        # effv_1 = 0.05,        # protective efficacy of a single dose of RTS,S
        # effv_2 = 0.2,         # protective efficacy of two doses of RTS,S
        # effv_3 = 0.5,         # protective efficacy of three doses of RTS,S
        # dv = 1,               # duration of vaccine protection
        dm = input$dm,
        lossd = input$lossd,
        cmda_1 = input$cmda_1,
        cmda_2 = input$cmda_2,
        cmda_3 = input$cmda_3,
        effv_1 = input$effv_1,
        effv_2 = input$effv_2,
        effv_3 = input$effv_3,
        dv = input$dv,
        
        # setting typology parameters
        # R0 =  3.0,           # basic reproduction number
        # eta = 0.5,           # proportion of all infections that are caught in the forrest
        # covEDAT0 = 0.3,      # baseline proportion of all clinical cases that receive treatment
        # nuTr = 365/3,        # recovery under successful treatement with ACT #1
        # covITN0 = 0.5,       # baseline coverage of ITN
        # covRCD0 = 0.0,       # baseline coverage of RCD (reactive case detection)
        # covCHW0 = 0.0,       # baseline coverage of extended role VMWs
        # amp = 0.7,           # relative amplitude seasonality
        # phi = 0.5,           # phase angle seasonality
        # muC = 0.00,          # rate of in-migration of clincial cases = number of cases per person (in current pop) per year
        # muA = 0.0,           # rate of in-migration of super-microscopic asymptomatic infections = number of new people per person (in current pop) per year
        # muU = 0.0,           # rate of in-migration of sub-microscopic asymptomatic infections = number of new people per person (in current pop) per year
        # kf = 0.9,            # maximum fatigue due to low proportion testing positive (0 to 1)
        R0 = input$R0,
        eta = input$eta,
        covEDAT0 = input$covEDAT0,
        nuTr = input$nuTr,
        covITN0 = input$covITN0,
        covRCD0 = input$covRCD0,
        covCHW0 = input$covCHW0,
        amp = input$amp,
        phi = input$phi,
        muC = input$muC,
        muA = input$muA,
        muU = input$muU,
        kf = input$kf,
        
        
        # biological parameters
        # omega = 1/2,         # rate of loss of immunity = 1/(average duration of immunity)
        # nuC = 365/10,        # rate of loss of symptoms in the absence of treatment
        # nuA = 365/30,        # 1/(duration of super-microscopic asymtomatic infection)
        # nuU = 365/120,       # 1/(duration of sub-microscopic asymtomatic infection)
        # rhoa = 0.7,          # relative infectivity of super-microscopic asymptomatic infections compared with clinical infections
        # rhou = 0.3,          # relative infectivity of sub-microscopic asymptomatic infections compared with clinical infections
        # ps = 0.9,            # proportion of all non-immune new infections that are clinical
        # pr = 0.2,            # proportion of all immune new infections that are clinical
        # mu = (1/50),         # birth/death
        # incnm = 2,           # incidence of non-malaria febrile illness
        omega = input$omega,
        nuC = input$nuC,
        nuA = input$nuA,
        nuU = input$nuU,
        rhoa = input$rhoa,
        rhou = input$rhou,
        ps = input$ps,
        pr = input$pr,
        mu = input$mu,
        incnm = input$incnm,
        
        # resistance spread parameters
        # fa = 0.1,           # proportion of artemisinin resistant cases that fail
        # ka = 5,             # speed of fixation of artemisinin resistance with respect to cumulative exposure
        # ha =  1e9,          # number of exposures to artemisinin mono therapy required for a selective sweeep for artemisinin resistance                
        # fp = 0.3,           # proportion of partner drug 1 resistant cases that fail
        # kp = 5,             # speed of fixation of partner drug 1 resistance
        # hp = 1E8,           # number of exposures to ACT 1 required for a selective sweeep for partner drug 1 resistance                
        # hap = 1E7,          # number of exposures to ACT 1 required for a selective sweeep for double resistance given artemisnin resistance                
        # hpa = 1E8,          # number of exposures to ACT 1 required for a selective sweeep for double resistance given partner drug 1 resistance                
        # fap = 0.9,          # proportion of ACT1 resistant cases that fail 
        # yatoap = 2000       # timing of switch from mono to ACT1
        fa = input$fa,
        ka = input$ka,
        ha = input$ha,
        fp = input$fp,
        kp = input$kp,
        hp = input$hp,
        hap = input$hap,
        hpa = input$hpa,
        fap = input$fap,
        yatoap = input$yatoap
      )
      
      
      # MODEL INITIAL CONDITIONS
      # population size
      initP<-10000 
      
      initS_0<-0.5*(1-initprev)*initP
      initIC_0<-0
      initIA_0<-initprev*initP
      initIU_0<-0
      initR_0<-0.5*(1-initprev)*initP
      initTr_0<-0
      
      
      # adjust cumulative exposure to reflect current proportions resistant
      initCa<-0
      if (presa>0){
        initCa<-as.numeric(parameters["ha"])*(1-(log((1-presa)/presa))/as.numeric(parameters["ka"]))
      }
      
      
      initCap<-0
      if (presp*(1-double)>0){
        initCap<-as.numeric(parameters["hap"])*(1-(log((1-presp)/presp))/as.numeric(parameters["kp"]))
      }
      if (presp*double>0){
        initCap<-as.numeric(parameters["hp"])*(1-(log((1-(presp/presa))/(presp/presa)))/as.numeric(parameters["kp"]))
      }
      
      
      initCf<-0
      
      
      state <- c(Y = 0, Ca = initCa, Cap = initCap, Cf = initCf, 
                 S_0 = initS_0, IC_0 = initIC_0, IA_0 = initIA_0, IU_0 = initIU_0, R_0 = initR_0, Tr_0 = initTr_0, Sm_0 = 0, Rm_0 = 0,
                 S_1 = 0, IC_1 = 0, IA_1 = 0, IU_1 = 0, R_1 = 0, Tr_1 = 0, Sm_1 = 0, Rm_1 = 0,
                 S_2 = 0, IC_2 = 0, IA_2 = 0, IU_2 = 0, R_2 = 0, Tr_2 = 0, Sm_2 = 0, Rm_2 = 0,
                 S_3 = 0, IC_3 = 0, IA_3 = 0, IU_3 = 0, R_3 = 0, Tr_3 = 0, Sm_3 = 0, Rm_3 = 0
      )
      
      # set up a function to solve the model
      modLAO<-function(t, state, parameters) 
      {
        with(as.list(c(state, parameters)),
             {
               
               # swtich on interventions
               covEDATi <- EDATon*covEDATi+(1-EDATon)*covEDAT0
               covITNi <- ITNon*covITNi+(1-ITNon)*covITN0
               covRCDi <- RCDon*covRCDi+(1-RCDon)*covRCD0
               covCHWi <- CHWon*covCHWi+(1-CHWon)*covCHW0
               
               sS <- S_0+S_1+S_2+S_3
               sR <- R_0+R_1+R_2+R_3
               sIC <- IC_0+IC_1+IC_2+IC_3
               sIA <- IA_0+IA_1+IA_2+IA_3
               sIU <- IU_0+IU_1+IU_2+IU_3
               sTr <- Tr_0+Tr_1+Tr_2+Tr_3
               sSm <- Sm_0+Sm_1+Sm_2+Sm_3
               sRm <- Rm_0+Rm_1+Rm_2+Rm_3
               
               # define variables
               P <- (sS+sR+sIC+sIA+sIU+sTr+sSm+sRm)
               seas<-1+amp*cos(2*3.14159*(Y-phi))
               nu <- 1/((1/nuC)+(1/nuA)+(1/nuU))
               beta<-R0*(mu+nu)*seas
               mu_out <- mu+muC+muA+muU
               
               
               timei<-timei-startyear
               yatoap<-yatoap-startyear
               
               wsi<-(1-(Y<=timei))*(Y<=(timei+scalei))*((Y-timei)/scalei)+1*(Y>=(timei+scalei))
               
               covEDAT<-(1-wsi)*covEDAT0+wsi*covEDATi
               covITN<-(1-wsi)*covITN0+wsi*covITNi
               covRCD<-(1-wsi)*covRCD0+wsi*covRCDi
               covCHW<-(1-wsi)*covCHW0+wsi*covCHWi
               
               
               lam <- (1-effITN*covITN)*beta*(sIC+rhoa*sIA+rhou*sIU)/P
               
               # vaccine effects
               
               v_1<- MASSon*(Y>(tm_1-startyear))*(Y<=(tm_1+dv-startyear))*effv_1
               v_2<- MASSon*(Y>(tm_2-startyear))*(Y<=(tm_2+dv-startyear))*effv_2
               v_3<- MASSon*(Y>(tm_3-startyear))*(Y<=(tm_3+dv-startyear))*effv_3
               
               lam_1 <- (1-v_1)*lam
               lam_2 <- (1-v_2)*lam
               lam_3 <- (1-v_3)*lam
               
               incm <- lam*(ps*sS+pr*(sR+sIA+sIU))
               ptp <- incm/(incnm*P+incm)
               fatig<- kf*(1-covCHW*effCHW)*(1-ptp)
               
               tau <- min(1,covEDAT*(1-fatig)*(1+(1-eta)*effRCD*covRCD))
               
               resa<-1/(1+exp(-ka*(Ca-ha)/ha))
               resp<-1/(1+exp(-kp*(Cap-hp)/hp))
               
               resap<-resa/(1+exp(-kp*(Cap-hap)/hap))+resp/(1+exp(-ka*(Cap-hpa)/hpa))
               
               
               faila <- fa*resa
               failp <- fp*resp
               failap <- fap*resap
               
               fail <- (Y<yatoap)*faila+(Y>=yatoap)*max(faila,failp)
               
               
               # MDA and RTS,S rounds
               m_1<- MASSon*(Y>(tm_1-startyear))*(Y<=(tm_1+dm-startyear))*(-log((1-cm_1))/dm) 
               m_2<- MASSon*(Y>(tm_2-startyear))*(Y<=(tm_2+dm-startyear))*(-log((1-cm_2))/dm) 
               m_3<- MASSon*(Y>(tm_3-startyear))*(Y<=(tm_3+dm-startyear))*(-log((1-cm_3))/dm) 
               m_4<-0
               
               treat <- ((ps*tau*lam*sS+pr*tau*lam*sR+pr*tau*lam*sIU+pr*tau*lam*sIA)
                         +m_1*cmda_1*(IC_0+IA_0+IU_0)
                         +m_2*cmda_2*(IC_1+IA_1+IU_1)
                         +m_3*cmda_3*(IC_2+IA_2+IU_2)
               )
               
               
               # rate of change
               dY <- 1
               
               dCa <-   (Y<yatoap)*treat                                                                                                                              #3
               dCap <-  (Y>=yatoap)*treat                                                                                                                             #4
               dCf <-  fail*nuTr*sTr                                                                                                                                  #5  
               
               dS_0 <- mu*P-mu_out*S_0+omega*R_0-lam*S_0+lossd*Sm_0-m_1*S_0                                                                                           #6
               dIC_0 <- muC*P-mu_out*IC_0+ps*(1-tau)*lam*S_0+pr*(1-tau)*lam*R_0+pr*(1-tau)*lam*IU_0+pr*(1-tau)*lam*IA_0-nuC*IC_0-m_1*IC_0                             #7 
               dIA_0 <- muA*P-mu_out*IA_0+(1-ps)*lam*S_0+(1-pr)*lam*R_0+(1-pr)*lam*IU_0-pr*lam*IA_0+nuC*IC_0-nuA*IA_0+fail*nuTr*Tr_0-m_1*IA_0                         #8
               dIU_0 <- muU*P-mu_out*IU_0-lam*IU_0-nuU*IU_0+nuA*IA_0-m_1*IU_0                                                                                         #9
               dR_0 <- -mu_out*R_0-omega*R_0-lam*R_0+nuU*IU_0+lossd*Rm_0-m_1*R_0                                                                                      #10
               dTr_0 <- -mu_out*Tr_0+ps*tau*lam*S_0+pr*tau*lam*R_0+pr*tau*lam*IU_0+pr*tau*lam*IA_0-nuTr*Tr_0-m_1*Tr_0                                                 #11
               dSm_0 <- -mu_out*Sm_0+omega*Rm_0-lossd*Sm_0-m_1*Sm_0                                                                                                   #12
               dRm_0 <- -mu_out*Rm_0-omega*Rm_0+(1-fail)*nuTr*Tr_0-lossd*Rm_0-m_1*Rm_0                                                                                #13
               
               
               dS_1 <- -mu_out*S_1+omega*R_1-lam_1*S_1+lossd*Sm_1+(1-cmda_1)*m_1*S_0-m_2*S_1                                                                          #14
               dIC_1 <- -mu_out*IC_1+ps*(1-tau)*lam_1*S_1+pr*(1-tau)*lam_1*R_1+pr*(1-tau)*lam_1*IU_1+pr*(1-tau)*lam_1*IA_1-nuC*IC_1+(1-cmda_1)*m_1*IC_0-m_2*IC_1      #15
               dIA_1 <- -mu_out*IA_1+(1-ps)*lam_1*S_1+(1-pr)*lam_1*R_1+(1-pr)*lam_1*IU_1-pr*lam_1*IA_1+nuC*IC_1-nuA*IA_1+fail*nuTr*Tr_1+(1-cmda_1)*m_1*IA_0-m_2*IA_1  #16
               dIU_1 <- -mu_out*IU_1-lam_1*IU_1-nuU*IU_1+nuA*IA_1+(1-cmda_1)*m_1*IU_0-m_2*IU_1                                                                        #17
               dR_1 <- -mu_out*R_1-omega*R_1-lam_1*R_1+nuU*IU_1 +lossd*Rm_1+(1-cmda_1)*m_1*R_0-m_2*R_1                                                                #18
               dTr_1 <- -mu_out*Tr_1+ps*tau*lam_1*S_1+pr*tau*lam_1*R_1+pr*tau*lam_1*IU_1+pr*tau*lam_1*IA_1-nuTr*Tr_1+m_1*(cmda_1*(IC_0+IA_0+IU_0)+Tr_0)-m_2*Tr_1      #19
               dSm_1 <- -mu_out*Sm_1+omega*Rm_1-lossd*Sm_1+m_1*(cmda_1*S_0+Sm_0)-m_2*Sm_1                                                                             #20
               dRm_1 <- -mu_out*Rm_1-omega*Rm_1+(1-fail)*nuTr*Tr_1-lossd*Rm_1+m_1*(cmda_1*R_0+Rm_0)-m_2*Rm_1                                                          #21
               
               dS_2 <- -mu_out*S_2+omega*R_2-lam_2*S_2+lossd*Sm_2+(1-cmda_2)*m_2*S_1-m_3*S_2                                                                          #22
               dIC_2 <- -mu_out*IC_2+ps*(1-tau)*lam_2*S_2+pr*(1-tau)*lam_2*R_2+pr*(1-tau)*lam_2*IU_2+pr*(1-tau)*lam_2*IA_2-nuC*IC_2+(1-cmda_2)*m_2*IC_1-m_3*IC_2      #23
               dIA_2 <- -mu_out*IA_2+(1-ps)*lam_2*S_2+(1-pr)*lam_2*R_2+(1-pr)*lam_2*IU_2-pr*lam_2*IA_2+nuC*IC_2-nuA*IA_2+fail*nuTr*Tr_2+(1-cmda_2)*m_2*IA_1-m_3*IA_2  #24
               dIU_2 <- -mu_out*IU_2-lam_2*IU_2-nuU*IU_2+nuA*IA_2+(1-cmda_2)*m_2*IU_1-m_3*IU_2                                                                        #25
               dR_2 <- -mu_out*R_2-omega*R_2-lam_2*R_2+nuU*IU_2 +lossd*Rm_2+(1-cmda_2)*m_2*R_1-m_3*R_2                                                                #26
               dTr_2 <- -mu_out*Tr_2+ps*tau*lam_2*S_2+pr*tau*lam_2*R_2+pr*tau*lam_2*IU_2+pr*tau*lam_2*IA_2-nuTr*Tr_2+m_2*(cmda_2*(IC_1+IA_1+IU_1)+Tr_1)-m_3*Tr_2      #27
               dSm_2 <- -mu_out*Sm_2+omega*Rm_2-lossd*Sm_2+m_2*(cmda_2*S_1+Sm_1)-m_3*Sm_2                                                                             #28
               dRm_2 <- -mu_out*Rm_2-omega*Rm_2+(1-fail)*nuTr*Tr_2-lossd*Rm_2+m_2*(cmda_2*R_1+Rm_1)-m_3*Rm_2                                                          #29
               
               dS_3 <- -mu_out*S_3+omega*R_3-lam_3*S_3+lossd*Sm_3+(1-cmda_3)*m_3*S_2-m_4*S_3                                                                          #30
               dIC_3 <- -mu_out*IC_3+ps*(1-tau)*lam_3*S_3+pr*(1-tau)*lam_3*R_3+pr*(1-tau)*lam_3*IU_3+pr*(1-tau)*lam_3*IA_3-nuC*IC_3+(1-cmda_3)*m_3*IC_2-m_4*IC_3      #31
               dIA_3 <- -mu_out*IA_3+(1-ps)*lam_3*S_3+(1-pr)*lam_3*R_3+(1-pr)*lam_3*IU_3-pr*lam_3*IA_3+nuC*IC_3-nuA*IA_3+fail*nuTr*Tr_3+(1-cmda_3)*m_3*IA_2-m_4*IA_3  #32
               dIU_3 <- -mu_out*IU_3-lam_3*IU_3-nuU*IU_3+nuA*IA_3+(1-cmda_3)*m_3*IU_2-m_4*IU_3                                                                        #33
               dR_3 <- -mu_out*R_3-omega*R_3-lam_3*R_3+nuU*IU_3 +lossd*Rm_3+(1-cmda_3)*m_3*R_2-m_4*R_3                                                                #34
               dTr_3 <- -mu_out*Tr_3+ps*tau*lam_3*S_3+pr*tau*lam_3*R_3+pr*tau*lam_3*IU_3+pr*tau*lam_3*IA_3-nuTr*Tr_3+m_3*(cmda_3*(IC_2+IA_2+IU_2)+Tr_2)-m_4*Tr_3      #35
               dSm_3 <- -mu_out*Sm_3+omega*Rm_3-lossd*Sm_3+m_3*(cmda_3*S_2+Sm_2)-m_4*Sm_3                                                                             #36
               dRm_3 <- -mu_out*Rm_3-omega*Rm_3+(1-fail)*nuTr*Tr_3-lossd*Rm_3+m_3*(cmda_3*R_2+Rm_2)-m_4*Rm_3                                                          #37
               
               
               
               # return the rate of change
               list(c(dY,dCa, dCap, dCf, dS_0, dIC_0, dIA_0, dIU_0, dR_0, dTr_0, dSm_0, dRm_0, 
                      dS_1, dIC_1, dIA_1, dIU_1, dR_1, dTr_1, dSm_1, dRm_1,
                      dS_2, dIC_2, dIA_2, dIU_2, dR_2, dTr_2, dSm_2, dRm_2,
                      dS_3, dIC_3, dIA_3, dIU_3, dR_3, dTr_3, dSm_3, dRm_3
               ))
             }
        ) 
        
      }
      
      out <- ode(y = state, times = times, func = modLAO, parms = parameters)
      
      
      # MODEL OUTPUTS
      ipop <- 6:37
      iinc <- 3:5
      iprev <- c(7,8,9,11,15,16,17,19,23,24,25,27,31,32,33,35)
      ifail <- 5
      
      # population
      times<-out[,1]+startyear
      pop<-rowSums(out[,ipop])
      
      
      # clinical incidence per 1000 per month
      tci <- rowSums(out[,iinc])
      clinmonth <- tci
      clinmonth[1] <- 0
      clinmonth[2:length(times)] <- 1000*(tci[2:length(times)] - tci[1:(length(times)-1)])/pop
      # percentage prevalence
      prevalence <- 100*rowSums(out[,iprev])/pop
      # treatment failure rates 
      failmonth <- out[,ifail]
      failmonth[2:length(times)] <- 1000*(out[2:length(times),ifail] - out[1:(length(times)-1),ifail])/pop
      percfail <- 100*failmonth/clinmonth
      
      LAOout<-matrix(NA,nrow=length(times),ncol=7)
      LAOout[,1]<-times
      LAOout[,2]<-clinmonth
      LAOout[,3]<-prevalence
      LAOout[,4]<-percfail
      
      return(LAOout)
    }
    scenario<-scenario_0
    LAOout0<-runLAO(scenario)
    scenario<-scenario_i
    LAOouti<-runLAO(scenario)
    
    times<-LAOout0[,1]
    clinmonth<-cbind(LAOout0[,2],LAOouti[,2])
    prevalence<-cbind(LAOout0[,3],LAOouti[,3])
    percfail<-cbind(LAOout0[,4],LAOouti[,4])
    
    
    # PLOTTING
    par(mfrow=c(1,2))
    maxy<-10
    matplot(times,clinmonth, type='l',lty=1,xlab = "Time",ylab="incidence per 1000 per month",main="Incidence of treated cases",ylim=c(0,maxy),lwd=2)
    lines(times,maxy*(times-scenario[1])/scenario[2],col="dark grey",lty=3, lwd=2)
    maxy<-30
    matplot(times,prevalence, type='l',lty=1,xlab = "Time",ylab="% prevalence",main="Population prevalence by U-PCR",ylim=c(0,maxy),lwd=2)
    lines(times,maxy*(times-scenario[1])/scenario[2],col="dark grey",lty=3, lwd=2)
  })
}

shinyApp(ui = ui, server = server)