
library(crayon)
library(SHELF)
library(rdrop2)

is_local <- Sys.getenv('SHINY_PORT') == ""
if (is_local) {
  setwd('H:/ED_app')
}

######## functions ########

f_nexttab<-function(a)
  #(actionButton("next_tab",a,width='120px'))
  (actionButton("next_tab",a,width='120px', style="background-color: lightgrey"))
  #(actionButton("next_tab",a,width='120px', style="color: #fff; background-color: #337ab7; border-color: #2e6da4"))

f_text_minimax<-function(a=NA,b=NA)  #a==elic$mini,b=elic$maxi
  ( p(#h4("Range"),
    p("I believe that on average, it's very unlikely to take:"), 
    tags$li(div(style="display: inline-block;vertical-align:middle; width: 100px;",HTML("less than")),
            div(style="display: inline-block;vertical-align:top; width: 75px;",numericInput("min",NULL,a,min=0,max=100)),
            div(style="display: inline-block;vertical-align:middle; width: 10px;",HTML("")),
            div(style="display: inline-block;vertical-align:middle; width: 120px;",HTML("minutes,"))),
    #br(),
    tags$li(div(style="display: inline-block;vertical-align:middle; width: 100px;",HTML("more than")),
            div(style="display: inline-block;vertical-align:top; width: 75px;",numericInput("max",NULL,b,min=0,max=100)),
            div(style="display: inline-block;vertical-align:middle; width: 10px;",HTML("")),
            div(style="display: inline-block;vertical-align:middle; width: 120px;",HTML("minutes.")))))

f_text_props<-function(a=NA,b=NA)  #a==elic$mini,b=elic$maxi
  ( p(#h4("Range"),
    p("I believe that it's very unlikely that:"), 
    tags$li(div(style="display: inline-block;vertical-align:middle; width: 210px;",HTML("the proportion is less than")),
            div(style="display: inline-block;vertical-align:top; width: 75px;",numericInput("prop_min",NULL,a,min=0,max=100)),
            div(style="display: inline-block;vertical-align:middle; width: 10px;",HTML("")),
            div(style="display: inline-block;vertical-align:middle; width: 120px;",HTML("percent,"))),
    #br(),
    tags$li(div(style="display: inline-block;vertical-align:middle; width: 210px;",HTML("the proportion is greater than")),
            div(style="display: inline-block;vertical-align:top; width: 75px;",numericInput("prop_max",NULL,b,min=0,max=100)),
            div(style="display: inline-block;vertical-align:middle; width: 10px;",HTML("")),
            div(style="display: inline-block;vertical-align:middle; width: 120px;",HTML("percent.")))
    ))


f_text_grid<-function(a)    #a=hist$nchip
  p(hr(),
    p("Please add",a,"chips to the grid below to express your uncertainty. The more chips you place in a particular bin the more certain you are the proportion lies in that bin."))

f_text_count<-function(chips,height)
  (p("You can use",strong(chips-height), " more chips."))

f_text1<-function(a,b,e)
  (paste("There is 98% probability that ", e, " between ",
     a," and ",b," minutes.", sep=""))

f_text2<-function(a,b,c,d,e)
  (paste("The average ", e, " is equally likely to take between ",
         a," and ",b," minutes, as it is to take between ",c, " and ",d," minutes.", sep=""))

f_text4<-function(a,b,c,e)
  (paste("There is ",a,"% probability that a ", e, " between ",
         b," and ",c," minutes.", sep=""))

f_questions2<-function(a,b,c,d) #a=median, b=lower quartile, c=task, d=task simplified
  (p(strong("If ", c, a,
            "minutes, what proportion of ",d," take up to ",b," minutes?", sep="")))

f_text_fback_hist<-function(a,b,c,d,e){#a=height,b=nbins,c=lbins,d=rbins,e=vec=rep(NA,5),f=valss<-rep(NA,10)
  valss<-rep(0,7)
  x<-which(a!=0)#length of non-zeros
  a<-a[x]
  b<-length(a)
  c<-c[x]
  d<-d[x]
  
  y<-which(a==max(a))
  z<-which(diff(y)==1)
  mmode<-y[1:ifelse(length(z)==0,1,length(z)+1)]
  l_mode<-c[mmode[1]]
  r_mode<-d[mmode[length(mmode)]]
  p_mode<-sum(a[mmode])/sum(a)
  p_less<-ifelse(mmode[1]>1,sum(a[1:(mmode[1]-1)])/sum(a),0)
  p_more<-ifelse(mmode[length(mmode)]<b,sum(a[-(1:mmode[length(mmode)])])/sum(a),0)
  
  if(p_mode==1&length(mmode)==1){
    vec<-1
    valss[1]<-l_mode
    valss[2]<-r_mode
    wording<-tags$li(f_text1(l_mode,r_mode,e))
  }
  
  if(p_mode==1&length(mmode)!=1){
    
    if(is.integer(length(mmode)/2)){
      vec<-2
      t_ratio<-length(mmode)/2
      valss[1]<-sum(a[1:t_ratio])/sum(a)
      valss[2]<-c[1]
      valss[3]<-d[t_ratio]
      valss[4]<-d[b]
      
      temptext1<-f_text1(l_mode,r_mode,e)
      temptext2<-f_text2(c[1],d[t_ratio],d[t_ratio],d[b],e)
      wording<-tagList(div(tags$li(temptext1),
                           tags$li(temptext2)))
      
    }else{
      vec<-3
      t_ratio<-ceiling(length(mmode)/2)
      valss[1]<-round(sum(a[1:t_ratio])/sum(a),digits=2)
      valss[2]<-c[1]
      valss[3]<-d[t_ratio]
      valss[4]<-d[b]
      
      temptext1<-f_text1(l_mode,r_mode,e)
      temptext2<-f_text4((valss[1]*100),c[1],d[t_ratio],e)
      
      wording<-tagList(div(tags$li(temptext1),
                           tags$li(temptext2)))
    }
    
  }
  
  if(p_mode!=1&(p_less==0|p_more==0)){
    
    temptext1<-f_text4(round(p_mode*100),l_mode,r_mode,e)
    
    if(p_less!=0){
      temptext2<-f_text4(round(p_less*100),c[1],l_mode,e)
    }else{
      temptext2<-f_text4(round(p_more*100),r_mode,d[b],e)
    }
    
    wording<-tagList(div(tags$li(temptext1),
                         tags$li(temptext2)))
    
  }
  
  if(p_less!=0&p_more!=0){
    
    temptext2<-f_text4(round(p_mode*100),l_mode,r_mode,e)
    
    temptext1<-f_text4(round(p_less*100),c[1],l_mode,e)
    
    temptext3<-f_text4(round(p_more*100),r_mode,d[b],e)
    
    wording<-tagList(div(tags$li(temptext1),
                         tags$li(temptext2),
                         tags$li(temptext3)))
    
  }
  
  wording
  
}

f_que2_fback<-function(quant1a, quant1b, quant2a, quant2b, que, x1, x3){
  
  p(
    div(
      paste("You have expressed that ",quant1a,"% to ",quant1b, "% of ", que, " take less than ", x1, " minutes. This implies that ",
        quant2a, "% to ", quant2b, "% of ", que, " take more than ", x3, " minutes.", sep=""),
      #p("- You have expressed that at least ", quant1, "% of ", que, " last less than ", x1," minutes. This implies that", strong("approximately 5%"), "of ", que, " (or 1 in 20) last ",strong(paste(" more than ", x2a, " minutes.")),sep=""),br(),
      #p("- You have expressed that up to ", quant2, "% of ", que, " last less than ", x1," minutes. This implies that", strong("approximately 5%"), "of ", que, " (or 1 in 20) last ",strong(" more than ", x2b, " minutes."),sep=""),
      #br(),
      selected = NULL, inline = TRUE, style='padding-left:30px;'), br(), br(),
    "If this summary statement does not represent your beliefs, you can enter new values above and click on 'Enter' to update the summaries.", br(), br()
  )
  
}

f_props_too_high<-function(que, quant2){
  p("One or more of the values you have entered is too high for ", que, "to take, on average, ",quant2, " minutes. Please revise your values.", sep="")
}

f_erf <- function(x) 2 * pnorm(x * sqrt(2)) - 1
f_erf_inv<- function (x) qnorm((1 + x)/2)/sqrt(2)
f_quadratic<-function(a,b,c){
  sol <- (-b + sqrt (b^2 - 4 * a * c)) / (2 * a)
  print(sol)
}

# f_lnorm_pars<-function(mn_nat,x1_nat,plnorm){
#   
#   mn_log<-log(mn_nat)
#   x1_log<-log(x1_nat)
#   erf_val<-f_erf_inv(2*plnorm-1) #https://stackoverflow.com/questions/29067916/error-function-erfz
#   sdev_lnorm <- f_quadratic(0.5, -1 * sqrt(2) * erf_val, x1_log-mn_log)
#   mean_lnorm <- mn_log - (sdev_lnorm^2) / 2
#   
#   tot<-list(mean_lnorm,sdev_lnorm)
#   return(tot)
#   
# }

ini_shapes<-c(seq(10,1,-0.1),1/seq(2,10,0.1))
ini_scales<-1/ini_shapes


f_trial1<-function(eli_mean,q_elicit,pr_elic1,ini_shapes,ini_scales){
  
  mat1<-matrix(c(eli_mean*ini_shapes, ini_scales),length(ini_shapes),2)
  vec1<-rep(NA,nrow(mat1))
  
  for(i in 1:nrow(mat1)){
    vec1[i] <- pgamma (q_elicit, shape = mat1[i, 1], scale = mat1[i, 2])
  }
  
  if(sum(vec1-pr_elic1<0)==length(vec1)|sum(vec1-pr_elic1>0)==length(vec1)){
    
    r1<-NA
    r2<-NA
    v1<-"y"
    
    gamma_par1<-c(r1,r2,v1)
    
  } else {
    
    r1<-which(vec1-pr_elic1<0)[sum(vec1-pr_elic1<0)]
    r2<-r1+1
    v1<-ifelse(abs(vec1[r1]-vec1[r2])<0.001, "y", "n")
    
    if(v1=="y"){
      
      g_par1<-c(r1,r2)[which.min(abs(vec1[c(r1,r2)]-pr_elic1))]
      gamma_par1<-c(mat1[g_par1,], v1)
      gamma_par1
      
    } else {
      
      c(ini_shapes[r2], ini_shapes[r1], v1)
      
    }
    
  }
  
}

#temp<-f_trial1(13,11,0.25,ini_shapes,ini_scales)

f_trial2<-function(temp, ini_shapes, eli_mean, q_elicit, pr_elic1){
  
  start1<-as.numeric(temp[1])
  start2<-as.numeric(temp[2])
  #v2<-temp[3]
  
  new_shapes<-seq(start1,start2,(start2-start1)/100)
  new_scales<-1/new_shapes
  
  mat2<-matrix(c(eli_mean*new_shapes, new_scales),length(new_shapes),2)
  vec2<-rep(NA,nrow(mat2))
  
  for(i in 1:nrow(mat2)){
    vec2[i] <- pgamma (q_elicit, shape = mat2[i, 1], scale = mat2[i, 2])
  }
  
  r3<-which(vec2-pr_elic1>0)[sum(vec2-pr_elic1>0)]
  r4<-r3+1
  v2<-ifelse(abs(vec2[r3]-vec2[r4])<0.001, "y", "n")
  
  if(v2=="y"){
    
    g_par1<-c(r3,r4)[which.min(abs(vec2[c(r3,r4)]-pr_elic1))]
    gamma_par2<-c(mat2[g_par1,],v2)
    gamma_par2
    
  } else {
    
    c(new_shapes[r3],new_shapes[r4], v2)
    
  }
  
  
}

#f_trial2(temp_fun1, ini_shapes, eli_mean)

f_gamma_pars<-function(eli_mean,q_elicit,pr_elic1,ini_shapes,ini_scales){
  
  temp_fun1<-f_trial1(eli_mean,q_elicit,pr_elic1,ini_shapes,ini_scales)
  
  if(temp_fun1[3]=="y" & !is.na(temp_fun1[1])) {
    
    as.numeric(temp_fun1[1:2])
    
  } else if (temp_fun1[3]=="y" & is.na(temp_fun1[1])) {
    
    rep(NA,2)
    
  } else {
    
    temp_fun2<-f_trial2(temp_fun1, ini_shapes, eli_mean, q_elicit, pr_elic1)
    
    if(temp_fun2[3]=="y"){
      
      as.numeric(temp_fun2[1:2])
      
    } else {
      
      temp_fun3<-f_trial2(temp_fun2, new_shapes, eli_mean, q_elicit, pr_elic1)
      
      as.numeric(temp_fun3[1:2])
      
    }
    
  }
  
}

f_plot_median<-function(height, nchips, rbins, lbins){
  
  temp1<-which(cumsum(height)==nchips/2)
  if (length(temp1)!=0) {
    median<-rbins[temp1]
  } else {
    temp2<-which(cumsum(height)>nchips/2)[1]
    median<-ceiling((lbins[temp2-1]+lbins[temp2])/2)
  }
  
  return(median)
}


# mean(rlnorm(10000,lnorm_par1, lnorm_par2))
# round(qlnorm(0.95, lnorm_par1, lnorm_par2), digits=0)
# 
# f_lnorm_pars(mn_nat,x1_nat,elic$prop_min/100)
# f_lnorm_pars(mn_nat,x1_nat,elic$prop_max/100)

#### saving to dropbox ####
#token <- drop_auth()
#saveRDS(token, "droptoken.rds")

token <- readRDS("droptoken.rds")
drop_acc(dtoken = token)
outputDir<-"Ed_app_save"

password_saves<-round(runif(1,0,1000000),digits=0)

f_saveData <- function(data,name1) {
  data <- t(data)
  # Create a unique file name
  fileName <- sprintf(name1, as.integer(Sys.time()), digest::digest(data))
  # Write the data to a temporary file locally
  filePath <- file.path(tempdir(), fileName)
  write.csv(data, filePath, row.names = FALSE, quote = TRUE)
  # Upload the file to Dropbox
  drop_upload(filePath, path = outputDir, dtoken = token)
}

f_loadData <- function(filename) {
  
  filesInfo <- subset(drop_dir(outputDir), name=filename)
  temp <- filesInfo$path_display
  filePaths <- temp[which(temp==paste0("/",outputDir,"/",filename))]
  if(length(filePaths) == 0) {
    data <- 999999
    data
  } else {
    data <- drop_read_csv(filePaths, dtoken = token)
    data
  }

}


#### plot functions ####

bins<-c(1,2,5,10)

fwidth<-function(mins,maxs)
  (bins[which.min(abs(((maxs-mins)/bins)-10))])

flower<-function(mins,width)
  (ifelse(((floor(mins/width)*width)-width)>=0,((floor(mins/width)*width)-width),(floor(mins/width)*width)))

fupper<-function(maxs,width)
  ((ceiling(maxs/width)*width)+width)
#(ifelse(((ceiling(a/b)*b)+b)<=100,((ceiling(a/b)*b)+b),(ceiling(a/b)*b)))

fnbins<-function(mins,maxs,width)
  ((maxs-mins)/width)

flbins<-function(mins,maxs,width)  
  (seq(from=mins,to=maxs-width,by=width))

frbins<-function(mins,maxs,width)
  (seq(from=mins+width,to=maxs,by=width))

fnchip<-function(nbins)
  (nbins*2)

######## text ########

t_questions1<-c("How long, on average, does it take to take a blood test? This
                includes ordering the blood test on the computer, taking the blood sample (and any relevant preparation) and sending it off for analysis,
                but not waiting for the results.",#1
                "How long, on average, does it take to get blood test results, once the sample has been sent off for analysis?",#2
                "How long, on average, does it take from sending a patient for x-ray to receiving the results?",#3
                "How long, on average, does it take to perform an ECG?",#4
                "How long, on average, does it take from sending a patient for a scan (including ultrasound, MRI, computerised 
                tomography and genitourinary contrast; excluding x-ray) to receiving the results?",#5
                "How long, on average, does it take to interpret the results of a scan?",#6
                "How long, on average, does it take to obtain patient observations each time ?",#7
                "How long, on average, does it take to interpret patient observations?",#8
                "How long, on average, does it take to get the results of a urine (incl. pregnancy) test from the time it is provided within the A&E?",#9
                "How long, on average, does it take a doctor to undertake an initial patient evaluation (excluding diagnostic investigations/ treatments)?",#10a
                "How long, on average, does it take a nurse to undertake an initial patient evaluation (excluding diagnostic investigations/ treatments)?",#10b
                "How long, on average, does it take to undertake a post-investigation/ treatment evaluation of the patient?",#11
                "How long, on average, does it take to take to remove a foreign body from a patient? It is recognised that this is 
                highly variable, depending on what is being removed and where it is being removed from. This can be captured within 
                the questions about patient variation. Please give your best estimate about the average time in this section.",#12
                "How long, on average, does it take to undertake lavage/ emesis/ charcoal/ eye irrigation?",#13
                "How long, on average, does it take to insert a urinary catheter?",#14
                "How long, on average, does it take to undertake defibrillation? This could be due to cardiac arrest or for an arrhythmia.",#15
                "How long, on average, does it take to undertake resuscitation?",#16
                "How long, on average, does it take to undertake minor surgery?",#17
                "How long, on average, does it take for epistaxis control?",#18
                "How long, on average, does it take to provide supplemental oxygen?",#19
                "How long, on average, does it take to provide continuous positive airways pressure/nasal intermittent positive pressure ventilation/bag valve mask?",#20
                "How long, on average, does it take to insert an arterial line?",#21
                "How long, on average, does it take to set up active rewarming of the hypothermic patient?")#22

n_que<-length(t_questions1)
 
#the following two no longer fit with the questions2 format
#if the average ... takes c minutes, what proportion, etc.
#One or more of the values you have entered is too high for the average ... to take

#there is xx probability that ... between...
t_questions2<-c("taking a blood test, on average, takes",#1
                "blood test results, on average, take",#2
                "receiving x-ray results, on average, takes",#3
                "performing an ECG, on average, takes",#4
                "scan results, on average, take",#5
                "interpreting scan results, on average, takes",#6
                "obtaining patient observations, on average, takes",#7
                "interpreting patient observations, on average, takes",#8
                "receiving urine test results, on average, takes",#9
                "a patient evaluation (by doctor), on average, takes",#10a
                "a patient evaluation (by nurse), on average, takes",#10b
                "such patient evaluation, on average, takes",#11
                "removing a foreign body, on average, takes",#12
                "the stated tasks, on average, take",#13
                "inserting a urinary catheter, on average, takes",#14
                "defibrillation, on average, takes",#15
                "resuscitation, on average, takes",#16
                "undertaking minor surgery, on average, takes",#17
                "epistaxis control, on average, takes",#18
                "provision of supplemental oxygen, on average, takes",#19
                "these tasks, on average, take",#20
                "inserting an arterial line, on average, takes",#21
                "active rewarming, on average, takes")#22

#there is xx probability that the average ... takes between...
t_questions3a<-c("the average blood test",
                 "the average blood test result",
                 "the average x-ray",
                 "the average scan")

#what proportion of .. takes up to c minutes?
#You have expressed that x% to y% of ... take less than
t_questions3b<-c("blood tests", #1
                 "blood tests results", #2
                 "x-ray results", #3
                 "ECGs", #4
                 "scan results", #5
                 "scan results (interpretation)", #6
                 "patient observations",#7
                 "patient observations (interpretation)", #8
                 "urine test results", #9
                 "patient evaluations", #10a
                 "patient evaluations", #10b
                 "such patient evaluations", #11
                 "foreign body removals", #12
                 "these tasks",#13
                 "urinary catheter insertions", #14
                 "defibrillations", #15
                 "resuscitations", #16
                 "minor surgeries", #17
                 "epistaxis controls", #18
                 "supplemental oxygen provisions", #19
                 "these tasks", #20
                 "arterial line insertions", #21
                 "active rewarmings") #22


t_welcome<-
  p(br(),
    strong(h4("Your data")),br(),
    "Before we start, please fill in the consent form below.",br(),br(),
    "We will be collecting data from you today. All the data we keep will be anonymised. Data 
    will be stored for up to 2 year in a secure folder on a networked computer.",br(),br(),
    "The anonymised data will be analysed, and results may be presented at conferences and 
    disseminated in peer-reviewed publications.",br(),br(),
    "Do you consent for us to use your data for the abovementioned purposes?",br(),
    radioButtons("consent"," ",c("Yes"=1, "No"=2),selected=2),
    fluidRow(
      column(9,"Note that you must consent in order to proceed."),
      column(2,f_nexttab("Next"))))

t_about_you<-
  p(br(),
    strong(h4("About you")),br(),
    "Please select your role from the list, then click on Next.",
    radioButtons("role"," ",c(
      "ED Consultant" = 1,
      "Associate specialist doctor" = 2,
      "Middle grade doctor" = 3,
      "Junior doctor" = 4,
      "ED specialist nurse" = 5,
      "Senior nurse" = 6,
      "Junior nurse" = 7,
      "Support worker/technician" = 8,
      "Other (splease specify)"= 9), selected=integer(0)),
    textInput("role_other", "", value="", width='710px'), br(), br(),
    fluidRow(column(2,offset = 9, f_nexttab("Next"))))

t_introduction<-
  p(br(),
    strong(h4("Introduction")),br(),
    "The University of Sheffield is undertaking modelling work to look at how A&E 
    services in Yorkshire and the Humber region are delivered. The majority of data 
    used in the model are based on primary patient level data collected from the A&E 
    departments themselves.",br(), br(),
    "However, there are several inputs for which data are unavailable, and as an 
    alternative we require clinical input to inform these estimates. We are requesting 
    your help with this.", br(), br(),
    "Here we will ask you a series of questions about the time taken to undertake key 
    activities in A&E.", br(), br(),
    "You are not expected to know the answer to all of these questions. 
    You will be asked if you carry out the activity we are asking about as part of your 
    regular job. If you do not, you will be asked to skip over that question.",br(), br(),
    "Some of the questions will be about things where you can express your answers relatively 
    easily, but others may require some thinking. Just express how uncertain you are 
    about the questions in your response (we will show you how to do this shortly). ",
    br(), br(),
    fluidRow(
      column(9,"Please click on Next to continue."),
      column(2,f_nexttab("Next"))))


t_session_timeout<-p(
  br(), br(),
  "Is this the first time you are attempting this exercise?",
  radioButtons("radio_skip_que1"," ",c("Yes"=1, "No"=2),selected=integer(0)),
  fluidRow(column(1,offset=2,actionButton("new_expert", "Enter", width='120px', style="background-color: lightgrey")))
)

t_session_timeout_new2<-p(
  br(),
  "Please enter your unique pass code to proceed from where you left off. This is the code you were provided at the start of your first session.", br(),br(),
  "If you do not have the password, please email dina.jankovic@york.ac.uk with subject line 'ED elicitation' or start the exercise from the beginning.",
  textInput("password", "", value="", width='710px'), 
  fluidRow(column(2,offset = 9, f_nexttab("Next"))),
  br(), br()
  
)

f_session_timeout_new1<-function(a)
  
    (p("Your unique pass code for this session is ",strong(a),". Please write it down, as you will need to enter it if you need to sign in again.", sep=""))
    

# t_session_timeout<-p(
#   br(), br(),
#   fluidRow(
#     column(9, "If you had already started completing the exercise but the web page had 'expired', please click here to continue from where you left off."),
#     column(2, actionButton("session_timeout","Reload",width='120px', style="background-color: lightgrey"))), br()
#   )

t_part1<-p(
  br(),
  strong(h4("PART 1: Average duration for tasks and expressing your uncertainty about this")),br(),br(),
  fluidRow(
    column(8,"This section takes approximately 20 minutes to complete."),
    column(2,actionButton("next_tab","Begin Part 1",width='150px', style="background-color: lightgrey")
                  ))
)

t_part2<-p(
  br(),
  strong(h4("PART 2: Patient variation")),br(),br(),
  fluidRow(
    column(8, "This section takes up to 20 minutes to complete."),
    column(2,actionButton("next_tab","Begin Part 2",width='150px', style="background-color: lightgrey")
                  ))
)
    
 # t_uncertainty0<-p(   
 #    strong(h4("What do we mean by uncertainty?")),br(),
 #    "Uncertainty refers to how (un)sure you are about an answer to a question. Imagine explaining the average duration of a GP appointment in England. An average appointment can be 11minutes, but it could also be 
 #    as low as 9 minutes or as high as 20. This is uncertainty! It describes a range of plausible answers to a question.",br(),br(),
 #    "This is different to thinking about variation between individual patients. For example, one patient may require a 15 minute appointment with their GP, another may only require 5 minutes, while some patients may 
 #    take as long as half an hour. This isn't uncertainty because it refers to variation between patients, instead of a range of plausible values for the same type of or average patient.",br(),br(),
 #    fluidRow(
 #      column(9,"Please click on Next to continue."),
 #      column(2,f_nexttab("Next"))))

t_type_of_que<-p(
  br(),
  strong(h4("What types of questions will we ask you?")),br(),
  "We are going to ask two types of questions:",br(),br(),
  div(
    "1.	Questions about average duration of specific tasks (i.e. time required to do them).",br(),
    "2.	Questions about the proportion of patients who take a specific amount of time to complete a specific task.", br(),
    selected = NULL, inline = TRUE, style='padding-left:15px;'), br(),
  "We have split the exercise into two parts (one for each type of question), and we will provide more information on each as we go through it.", br(), br(),
  fluidRow(
    column(9,"Please click on Next to continue."),
    column(2,f_nexttab("Next")))
)

t_uncertainty1<-
  p(br(),
    h4("What types of questions will we ask you?"),br(),br(),
    "In this section, we will ask questions about average duration of specific tasks 
    (i.e. time required to do them).",br(), br(),
    "For example, what is the average duration of a GP appointment in England?",br(), br(),
   "Here, we are interested in your uncertainty around the duration of ",
          strong("an average GP appointment."),br(), br(),
    hr(), br(),
    strong(h4("What do we mean by uncertainty?")),br(),
    "Uncertainty refers to how (un)sure you are about an answer to a question. For example, 
    the average duration of a GP appointment can be 11minutes, but it could also be as low 
    as 9 minutes or as high as 20. This is uncertainty! It describes a range of plausible 
    answers to a question.",br(),br(),
    "This is different to thinking about variation between individual patients. For example, 
    one patient may require a 15 minute appointment with their GP, another may only require 5 
    minutes, while some patients may take as long as half an hour. This isn't uncertainty 
    because it refers to variation between patients, instead of a range of plausible values 
    for the same type of or average patient.",br(),br(),
   "We will come back to variability between individual patients in Part 2.", br(), br(),
    hr(), br(),
    strong(h4("How will you express your uncertainty?")),br(),br(),
    "In order to express your uncertainty, first you will be asked to give a range of possible values. For example, if we are asking you for the average duration of a GP appointment, the plausible range represents", br(), br(),
    div(
      "-	Your lowest plausible duration (minimum) - a value such that you believe that there is a 1% probability that the duration is less than that value.",br(),
      "-	Your highest plausible duration (maximum) - a value, such that you believe that there is a 1% probability that the duration is more than that value.",br(),
      "-	So you believe that there is 98% probability between the lowest and the highest durations.", style='padding-left:15px;'),
   br(),
    "Test your range by imagining that somebody gives a value that is outside your plausible range. Your reaction should be that the person has misunderstood or misremembered, i.e. you are very confident that you have chosen the right range!", br(), br(),
    "Let's say that the average duration is unlikely to be less than 9, or longer than 17 minutes.", br(), br(),hr(),br(),
   "After giving your plausible range, you will then be required to fill in a grid, like the one below. The range of possible values that appear along the bottom are determined by the range you specify - in this case 8-18 minutes (slightly wider than the specified range of 9 to 17). Note that the grid is split into columns (we call these bins) - the example below has ten bins (8-9, 9-10, 10-11 minutes, and so on). ", br(), br(),
   "To tell us how plausible you think each of these values in this range are, you will be given a number of chips to place in the bins. There are a different number of “chips” depending on the range you give.", br(), br())
   
t_uncertainty2<-
  p("Use this to indicate how confident you feel that each of the values on the horizontal axis is the correct average value. The higher the red bar in the grid, the more confident you are.", br(), br(),
   "For example, if you are completely uncertain, you place the same number of chips in each of the bins, so the height of red bars is the same for all possible values, as shown below.", br(), br())
   
t_uncertainty3<-   
   p("Similarly, if you are absolutely certain of the value, you place all chips in one bin, as shown below.",br(), br())
  
t_uncertainty4<-     
   p("You will need to be mindful about what your response says about the degree of certainty in your belief. For example, twice as many coloured boxes on one value means you believe that the value is twice as likely to be the answer.",br(), br(),
   "Another example is shown below.", br(), br())

t_uncertainty5<-     
   p("It's important to realise that, because we are asking you about the average and the uncertainty around this, there is no rationale for breaks in the bins, like in the grid below.", br(), br())
   
t_uncertainty6<-     
   p(br(),"You can practice with the grid below.", br(), br())

t_enter_plot<-fluidRow(
  column(9, p(style="font-size:90%;","When you are happy with your answers please click 'Enter', then scroll down.")),
  column(1, actionButton("enter_plot", "Enter", width='120px', style="background-color: lightgrey")))

t_skip_que1<-
  p("Is this a task that you normally conduct?",
    radioButtons("radio_skip_que1"," ",c("Yes"=1, "No"=2),selected=integer(0)),
    fluidRow(column(1,offset=2,actionButton("skip_que1", "Enter", width='120px', style="background-color: lightgrey"))))

#fluidRow(
#column(9, p(style="font-size:90%;","If this is not a task that you normally conduct, you can click on 'Skip Question'")),
#column(1, actionButton("skip_que1", "Skip question", width='120px')))

t_que2_intro<-p(
  br(),
  h4("What types of questions will we ask you?"),br(),br(),
  "In this section, we will ask questions about variation between individual patients. For example, if a GP 
  appointment lasts, on average, 13 minutes, there may still be some variation between patients - some may be 
  very quick and only take 5 minutes, while others may take a long time - as long as 40 minutes. This could 
  be caused by patient characteristics such as condition severity and age, time of day, or other factors.", br(), br(),
  "We want to know more about the variation between individual tasks.", br(), br(),
  "First, we will provide the average duration for that task, based on your answers in part 1. Let's say that a GP appointment takes, on average, 13 minutes.", br(), br(),
  "Then, to capture this variation between patients, we will ask what proportion of patients take a specific amount of time to complete a specific task. For example, 
  assuming the duration of a GP appointment is, on average, 13 minutes, what proportion of appointments do you think will last up to 11 minutes? See some examples in the graph below.", br(), br(),
  "We are interested in your uncertainty about this proportion. We will ask you to give your plausible range, so you believe that there is 98% 
  probability that the proportion 
  is between the lowest and the highest value. This sounds tricky but see the example below.", br(), br(),
)

t_que2_intro1<-p(
  br(),
  h4("What types of questions will we ask you?"),br(),br(),
  "In this section, we will ask questions about variation between individual patients. For example, if a GP 
  appointment lasts, on average, 13 minutes, there may still be some variation between patients - some may be 
  very quick and only take 5 minutes, while others may take a long time - as long as 40 minutes. This could 
  be caused by patient characteristics such as condition severity and age, time of day, or other factors.", br(), br(),
  "We want to know more about the variation between individual tasks.", br(), br(),
  "First, we will provide the average duration for that task, based on your answers in part 1. Let's say that a GP appointment takes, on average, 13 minutes.", br(), br(),
  "Then, to capture this variation between patients, we will ask what proportion of patients take a specific amount of time to complete a specific task. For example, 
  assuming the duration of a GP appointment is, on average, 13 minutes, what proportion of appointments do you think will last up to 11 minutes? See some examples in the graph below.", br())

t_que2_intro2<-p(
  "We are interested in your uncertainty about this proportion. We will ask you to give your plausible range, so you believe that there is 98% 
  probability that the proportion 
  is between the lowest and the highest value. This sounds tricky but see the example below.", br(), br(),
)

t_fback_que2<-fluidRow(
  column(9, p(style="font-size:90%;","When you are happy with your answers please click 'Enter', then scroll down.")),
  column(1, actionButton("fback_que2", "Enter", width='120px', style="background-color: lightgrey")))

t_enter_prop<-fluidRow(
  column(9, p(style="font-size:100%;","Please select which of the two statements best represents your beliefs, then click on next.")),
  column(1,f_nexttab("Next")
         
         #actionButton("saves", "Next", width='120px')
         
  ))

# t_que_type<-
#   p(br(),br(),
#     strong(h4("What types of questions will we ask of you?")),br(),
#     "We are going to ask two types of questions:",br(),br(),
#     div(
#       "1.	Questions about average duration of specific tasks (i.e. time required to do them).",br(),
#       "2.	Questions about the proportion of patients who take a specific amount of time to complete a specific task.", br(),
#       selected = NULL, inline = TRUE, style='padding-left:15px;'), br(),
#     "We have already talked about how we ask you question 1, the average duration of specific tasks, when we talked about GP appointments:",br(),br(),
#     strong("What is the average duration of a GP appointment in England?"),br(),br(),
#     "For the second type of question, we will take the average duration from your answer - let's say that's 13 minutes.", br(), br(),
#     strong("Assuming the duration of an average GP appointment is 13 minutes, what proportion of appointments do you think will last up to 11 minutes?"), br(), br(),
#     "We are interested in your uncertainty about this proportion, so we will ask you to give your plausible range, so you believe that there is 98% probability that the proportion is between the lowest and the highest value. This sounds a bit tricky but see the example below.")
    
t_que2_instructions<-
  p(hr(),br(),
    "Your judgments about the proportion of GP appointments that last less than 11 minutes, can be 
    used to approximate the proportion of appointment that are greater than the average duration.", br(), br(),
    "For example, if 20% of GP appointments last less than 11 minutes, and a GP appointment is, on 
    average, 13 minutes long, then we would expect approximately 19% of appointments to last more than 15 minutes. See some examples in the graph below.", br(), br(),
    "For every question about proportions, we will feedback what your values imply about longer appointments, as shown below.", br(), br(),
     div(
       "- You have expressed that 20% to 40% of GP appointments last less than 11 minutes. This implies that 19% to 31% of GP appointments last",strong("more than 15 minutes."),br(),br(),
       selected = NULL, inline = TRUE, style='padding-left:30px;'), br(),
    "If these summary statements do not represent your beliefs, you will be able to modify your inputs.",
    hr(), br(),
    
    fluidRow(
      column(9, p(style="font-size:100%;","Once you are satisfied with these instructions, click on the Next button to start the first question. Note you will be able to look at these instructions again at any point in the exercise.")),
      column(1,f_nexttab("Next")))
  )

t_que2_instructions1<-
  p(hr(),br(),
    "Your judgments about the proportion of GP appointments that last less than 11 minutes, can be 
    used to approximate the proportion of appointment that are greater than the average duration.", br(), br(),
    "For example, if 20% of GP appointments last less than 11 minutes, and a GP appointment is, on 
    average, 13 minutes long, then we would expect approximately 19% of appointments to last more than 15 minutes.", br())

t_que2_instructions2<-
  p(
    "For every question about proportions, we will feedback what your values imply about longer appointments, as shown below.", br(), br(),
    div(
      "- You have expressed that 20% to 40% of GP appointments last less than 11 minutes. This implies that 19% to 31% of GP appointments last",strong("more than 15 minutes."),br(),br(),
      selected = NULL, inline = TRUE, style='padding-left:30px;'), br(),
    "If these summary statements do not represent your beliefs, you will be able to modify your inputs.",
    hr(), br(),
    
    fluidRow(
      column(9, p(style="font-size:100%;","Once you are satisfied with these instructions, click on the Next button to start the first question. Note you will be able to look at these instructions again at any point in the exercise.")),
      column(1,f_nexttab("Next")))
  )

   
t_text_hist<-fluidRow(  
  column(8, p(style="font-size:90%;","When you are happy with your answers please click on 'Continue'.")),
  column(1, offset=1, actionButton("show_plot", "Continue",width='120px', style="background-color: lightgrey")), br())

t_summary_update<-("If these summary statements do not represent your beliefs you can modify the grid.")

t_next_ques1<-p(
  "If you have any additional comments about your answer, please state these here.",
  textInput("comment_que1", "", value="", width='710px'), br(), br(),
  fluidRow( 
  column(8, p(style="font-size:90%;", "Once you are satisfied that the statements represent your beliefs, click on 'Continue'.")),
  column(1, offset=1, actionButton("next_que1", "Continue",width='120px', style="background-color: lightgrey")), br())
)

t_next_ques2<-p(
  br(), hr(),
  "If you have any additional comments about your answer, please state these here.",
  textInput("comment_que2", "", value="", width='710px'), br(), br(),
  fluidRow( 
    column(8, p(style="font-size:90%;", "Once you are satisfied that the statements represent your beliefs, click on 'Continue'.")),
    column(1, offset=1, actionButton("next_que2", "Continue",width='120px', style="background-color: lightgrey")), br())
)

t_text_update<-fluidRow(
  column(8,p(style="font-size:90%;", "You can update your range by entering the new values and clicking 'Update range'.")),
  column(1,offset=1,actionButton("show_plot", "Update range", style="background-color: lightgrey")))

t_text_chip<-p("Please make sure you use all the available chips before proceeding.")

t_text_nohist<-p("Please make sure you've entered the minimum and the maximum, that both values are non-negative, and that the minimum is lower than the maximum.")

#t_next_que2<-fluidRow( 
#  column(8, p(style="font-size:90%;", "Once you are satisfied that the statements represent your beliefs, click on 'Next' to continue onto the next question.")),
#  column(1, offset=1, actionButton("next_tab", "Next",width='120px')), br())

t_text_noprops<-p("Please make sure you've entered both values, that both values are between 0 and 100, and that the minimum is lower than the maximum.")

######## server function ##########

shinyServer(function(input, output, clientData, session) {
  
  # location / {
  #   app_idle_timeout 0;
  # }
  
  #session$allowReconnect(TRUE)
  
  buttons<-reactiveValues(next_tab=-1, enter_minimax=0, enter_plot=0, show_plot=0, next_que1=0, fback_que2=0, saves=0, skip_que1=rep(0,n_que), first_time=0, old_password=0)
  elic<-reactiveValues(mini=9, maxi=17, height=rep(0,10), value=9:18, probs=rep(0,10), conds=0, quant1=rep(0,n_que), quant2=rep(0,n_que), quant3=rep(0,n_que), prop_min=20, prop_max=40, gamma1=rep(0,2), gamma2=rep(0,2), upper1=0, upper2=0, que1=1, que2=1, part2=1)
  hist<-reactiveValues(lower=8, upper=18, nhigh=20, nbins=10, lbins=seq(8,17,1), rbins=seq(9,18,1), width=1, nchip=20)
  cond<-reactiveValues(all_chips_1=0, all_chips_2=0, prop_min_max=0, good_fit1=1, good_fit2=1, mid_props=0)
  pros<-reactiveValues(password = round(runif(1,0,1000000),digits=0), correct = 0)

  
  observeEvent(input$new_expert,{#?????make sure they can't go forward without choosing or they start from the beginning if they don't enter password
    
    if (length(input$radio_skip_que1)==1) {
      
      buttons$first_time<-ifelse(input$radio_skip_que1==1,1,2)
      
    }
    
  })
  
  observeEvent(input$next_tab,{
    
    if(buttons$next_tab==-1){
      
      if(buttons$first_time==2){
        
        buttons$old_password<-input$password
        pros$password<-buttons$old_password
        temp<-unlist(f_loadData(paste0(pros$password,"_last_tab.csv")))#download last tab
        pros$correct <- ifelse(temp==999999,999999,0)
        
        if(temp[1]!=999999){ 
          
          if(temp[1] <= 3){
            
            buttons$next_tab <- temp[1]
            
            } else if(temp[1] == 7){
                 
                 elic$que1 <-   temp[2] + 1
                 
                 if(elic$que1>n_que){
                   
                   buttons$next_tab <- 8
                   
                 } else {
                   
                   buttons$next_tab <-  7
                   buttons$enter_minimax<-0
                   buttons$enter_plot<-0
                   buttons$show_plot<-0
                   elic$mini<-integer(0)
                   elic$maxi<-integer(0)          
                   
                 }
                 
                 means_for_part2 <-unlist(f_loadData(paste0(pros$password, "_means_for_part2.csv")))
                 lows_for_part2 <-unlist(f_loadData(paste0(pros$password, "_lows_for_part2.csv")))
                 highs_for_part2 <-unlist(f_loadData(paste0(pros$password, "_highs_for_part2.csv")))
                 relevant_tasks <-unlist(f_loadData(paste0(pros$password, "_relevant_tasks.csv")))
                 
                 if(length(means_for_part2)==0){
                   elic$quant2[1:n_que] <- rep(0,n_que)
                 } else {
                   elic$quant2[1:n_que] <- means_for_part2
                 }
                 
                 if(length(lows_for_part2)==0){
                   elic$quant1[1:n_que] <- rep(0,n_que)
                 } else {
                   elic$quant1[1:n_que] <- lows_for_part2
                 }
                 
                 if(length(highs_for_part2)==0){
                   elic$quant3[1:n_que] <- rep(0,n_que)
                 } else {
                   elic$quant3[1:n_que] <- highs_for_part2
                 }
                 
                 if(length(relevant_tasks)==0){
                   buttons$skip_que1[1:n_que] <- rep(0,n_que)
                 } else {
                   buttons$skip_que1[1:n_que] <- relevant_tasks
                 }
                 
               } else if (temp[1]>=8){
                 
                 elic$quant2 <- unlist(f_loadData(paste0(pros$password, "_means_for_part2.csv")))
                 elic$quant1 <- unlist(f_loadData(paste0(pros$password, "_lows_for_part2.csv")))
                 elic$quant3 <- unlist(f_loadData(paste0(pros$password, "_highs_for_part2.csv")))
                 buttons$skip_que1 <-unlist(f_loadData(paste0(pros$password, "_relevant_tasks.csv")))
                 
                 elic$que2 <- which(buttons$skip_que1==1&(1:n_que)>temp[2])[1]
                 elic$part2<-sum(buttons$skip_que1[1:elic$que2])
                 
                 if(elic$que2>n_que){
                   
                   buttons$next_tab <- 12
                   
                 } else {
                   
                   buttons$next_tab <- temp[1]
                   elic$prop_min <- integer(0)
                   elic$prop_max <- integer(0)
                   elic$upper1 <- 0
                   elic$upper2 <- 0
                   cond$good_fit2 <- 0
                   
                 }
                 
               }
          
        }
        
        temptemp<-0

        
      } else {
        
        temptemp<-1
        f_saveData(c(0,0), paste(pros$password,"_last_tab.csv", sep=""))
        
      }
      
      } else if (buttons$next_tab==0|buttons$next_tab>2){
        
      temptemp<-1
      if(buttons$next_tab==0){
        f_saveData(c(1,0), paste(pros$password,"_last_tab.csv", sep=""))
      }
      
    } else if (buttons$next_tab==1) {
      
      if(input$consent==1){
        temptemp<-1
        f_saveData(c(2,0), paste(pros$password,"_last_tab.csv", sep=""))
      } else {
        temptemp<-0
      }
      
    } else if (buttons$next_tab==2) {
      
      if (length(input$role)>0){
        f_saveData(c(input$role,input$role_other), paste(pros$password,"_role.csv",sep=""))
        f_saveData(c(3,0), paste(pros$password,"_last_tab.csv", sep=""))
        temptemp<-1
      } else {
        temptemp<-0
      }
      
    }

    
    if(temptemp==1){
      
      temp<-buttons$next_tab; buttons$next_tab<-temp+1
      
      if(temp>5){
        buttons$enter_minimax<-0
        buttons$enter_plot<-0
        buttons$show_plot<-0
        cond$all_chips_1<-0
        cond$all_chips_2<-0
        elic$mini<-integer(0)
        elic$maxi<-integer(0)
      }
      
      if(temp==8){
        f_saveData(c(8,0), paste(pros$password,"_last_tab.csv", sep=""))
      }
      
      if(temp>9){
        elic$prop_min<-integer(0)
        elic$prop_max<-integer(0)
        elic$upper1<-0
        elic$upper2<-0
      }
      
    }

    })
  
  # observeEvent(input$enter_minimax,{
  #   temp<-buttons$enter_minimax; buttons$enter_minimax<-temp+1
  #   elic$mini<-input$min; elic$maxi<-input$max
  #   })
  
  observeEvent(input$enter_plot,{
    
    temp<-buttons$enter_plot; buttons$enter_plot<-temp+1
    
    if(round(sum(elic$height),digits=0)==hist$nchip){
      
      cond$all_chips_1<-1
      cond$all_chips_2<-1
      
    } else {
      
      cond$all_chips_1<-0
      cond$all_chips_2<-0
      
    }
    
  }) #xxxxx ideally buttons$enter_plot goes to 0 when chips are removed.
  
  observeEvent(input$show_plot,{
    temp<-buttons$show_plot; buttons$show_plot<-temp+1
    buttons$enter_plot<-0
    #buttons$show_que2<-0
  })
  
  observeEvent(input$skip_que1,{
    
    if(length(input$radio_skip_que1)==1){
      
      if(input$radio_skip_que1==1){
        
        buttons$skip_que1[elic$que1]<-1
        f_saveData(buttons$skip_que1,paste(pros$password,"_relevant_tasks.csv", sep=""))
        
      } else if (input$radio_skip_que1==2){
        
        if(elic$que1>=n_que){
          temp2<-buttons$next_tab; buttons$next_tab<-temp2+1
          elic$que2<-which(buttons$skip_que1==1)[1]
          elic$part2<-sum(buttons$skip_que1[1:elic$que2])
        } else{
          temp1<-buttons$next_que1; buttons$next_que1<-temp1+1;
          temp2<-elic$que1; elic$que1<-temp2+1
          cond$all_chips_2<-1
        }
      }
      
    }
    
  })
  
  observeEvent(input$next_que1,{
    
    cond$all_chips_1<-0
    
    if(round(sum(elic$height),digits=0)==hist$nchip){
      cond$all_chips_2<-1
    } else {
      cond$all_chips_2<-0
    }
    
    if(cond$all_chips_2==1){
      
      f_saveData(c(elic$mini, elic$maxi, elic$value, elic$height),paste(pros$password, "_part1_que", elic$que1,".csv", sep=""))
      f_saveData(c(7,elic$que1), paste(pros$password,"_last_tab.csv", sep=""))
      
      if(length(input$comment_que1)>0){
        if(input$comment_que1!=""){
          f_saveData(input$comment_que1,paste(pros$password, "_comment_part1_que", elic$que1,".csv", sep=""))
        }
      }
      
      f_saveData(elic$quant2,paste(pros$password,"_means_for_part2.csv", sep=""))
      f_saveData(elic$quant1,paste(pros$password,"_lows_for_part2.csv", sep=""))
      f_saveData(elic$quant3,paste(pros$password,"_highs_for_part2.csv", sep=""))
      
      temp1<-buttons$next_que1
      
      if(elic$que1>=n_que){
        temp2<-buttons$next_tab; buttons$next_tab<-temp2+1
        elic$que2<-which(buttons$skip_que1==1)[1]
        elic$part2<-sum(buttons$skip_que1[1:elic$que2])
      } else{
        
        buttons$next_que1<-temp1+1
        temp3<-elic$que1; elic$que1<-temp3+1
        
        buttons$enter_minimax<-0
        buttons$enter_plot<-0
        buttons$show_plot<-0
        #buttons$skip_que1<-0
        elic$mini<-integer(0)
        elic$maxi<-integer(0)
      }
      
    }
    
  })
  
  observeEvent(input$next_que2,{

    f_saveData(c(elic$prop_min, elic$prop_max),paste(pros$password, "_part2_que", elic$que2,".csv", sep=""))
    f_saveData(c(11,elic$que2), paste(pros$password,"_last_tab.csv", sep=""))
    
    if(length(input$comment_que2)>0){
      if(input$comment_que2!=""){
        f_saveData(input$comment_que2,paste(pros$password, "_comment_part2_que", elic$que2,".csv", sep=""))
      }
    }
        
    if(elic$que2>=n_que){
      
      temp2<-buttons$next_tab; buttons$next_tab<-temp2+1
      
    } else if (sum(buttons$skip_que1[(elic$que2+1):n_que]>0)==0){
      
      temp2<-buttons$next_tab; buttons$next_tab<-temp2+1
      
    } else {
      
      temp3<-elic$que2
      elic$que2<-which(buttons$skip_que1 == 1 & 1:n_que > temp3)[1]
      elic$part2<-sum(buttons$skip_que1[1:elic$que2])
      
      #temp3<-elic$que2; elic$que2<-temp3+1
      
      elic$prop_min<-integer(0)
      elic$prop_max<-integer(0)
      elic$upper1<-0
      elic$upper2<-0
      cond$good_fit2 <- 0
      
    }
    
    
  })
  
  observeEvent(input$fback_que2,{
    
    temp<-buttons$fback_que2; buttons$fback_que2<-temp+1
    elic$prop_min<-input$prop_min
    elic$prop_max<-input$prop_max
    
    temp1<-ifelse(input$prop_min!="" & input$prop_max!="" & input$prop_min<input$prop_max & input$prop_min>=0 & input$prop_max<100, 1, 0)
    cond$prop_min_max<-ifelse(!is.na(temp1) & temp1==1,1,0) 
    #cond$prop_small<-ifelse(elic$prop_max<10,0,1)
    #cond$prop_large<-ifelse(elic$prop_max==100,0,1)
    
    #if(cond$prop_min_max==1&cond$prop_small==1&cond$prop_large==1){
    if(cond$prop_min_max==1){#
      
      temp2<-f_gamma_pars(elic$quant2[elic$que2], elic$quant1[elic$que2], elic$prop_min/100, ini_shapes,ini_scales)
      elic$gamma1<-c(temp2[[1]],temp2[[2]])
      temp3<-f_gamma_pars(elic$quant2[elic$que2], elic$quant1[elic$que2], elic$prop_max/100, ini_shapes,ini_scales)
      elic$gamma2<-c(temp3[[1]],temp3[[2]])
      
      if (is.na(elic$gamma1[1])){
        cond$good_fit1 <- 0
      } else {
        cond$good_fit1 <- 1
        elic$upper1<-100-round(pgamma(elic$quant3[elic$que2], shape = elic$gamma1[1], scale = elic$gamma1[2])*100,digits=0)
      }
      
      if (is.na(elic$gamma2[1])){
        cond$good_fit2 <- 0
      } else {
        cond$good_fit2 <- 1
        elic$upper2<-100-round(pgamma(elic$quant3[elic$que2], shape = elic$gamma2[1], scale = elic$gamma2[2])*100,digits=0)
      }
      
    }#
    
    # } else if (cond$prop_min_max==1 & cond$prop_small==0){
    #   
    #   elic$quant1<-round(0.5 * (elic$quant2+elic$mini),digits=1)
    #   elic$quant1<-elic$quant1*1.25
    #   elic$prop_min<-integer(0)
    #   elic$prop_max<-integer(0)
    #   buttons$fback_que2<-0
    #   
    # } else if (cond$prop_min_max==1 & cond$prop_large==0){
    #   
    #   elic$quant1<-round(0.5 * (elic$quant2+elic$mini),digits=1)
    #   elic$quant1<-elic$quant1*0.5
    #   elic$prop_min<-integer(0)
    #   elic$prop_max<-integer(0)
    #   buttons$fback_que2<-0
    #   
    # }
    

  })
  
  observeEvent(input$saves,{
    temp<-buttons$saves; buttons$saves<-temp+1
  })
  
    output$blank_grid<-renderImage({
      return(list(src="www/blank_grid.png"))
   }, deleteFile = FALSE)
    output$flat_prior<-renderImage({
      return(list(src="www/flat_prior.png"))
    }, deleteFile = FALSE)
    output$cert_prior<-renderImage({
      return(list(src="www/cert_prior.png"))
    }, deleteFile = FALSE)
    output$norm_prior<-renderImage({
      return(list(src="www/norm_prior.png"))
    }, deleteFile = FALSE)
    output$bimo_prior<-renderImage({
      return(list(src="www/bimo_prior.png"))
    }, deleteFile = FALSE)
    
    output$gamma_p_20<-renderImage({
      return(list(src="www/gamma_p_20.jpeg"))
    }, deleteFile = FALSE)
    output$gamma_p_40<-renderImage({
      return(list(src="www/gamma_p_40.jpeg"))
    }, deleteFile = FALSE)
    output$gamma_p_20_outliers<-renderImage({
      return(list(src="www/gamma_p_20_outliers.jpeg"))
    }, deleteFile = FALSE)
    output$gamma_p_40_outliers<-renderImage({
      return(list(src="www/gamma_p_40_outliers.jpeg"))
    }, deleteFile = FALSE)
    
    ###### create plot #####
    
    observeEvent(input$show_plot,{
      
      temp2<-ifelse(input$min!="" & input$max!="" & input$min<input$max & input$min>=0, 1, 0)
      elic$conds<-ifelse(!is.na(temp2) & temp2==1,1,0) 
      elic$mini<-input$min
      elic$maxi<-input$max
      
      if(elic$conds==1){
        
        hist$width<-fwidth(elic$mini,elic$maxi)
        hist$lower<-flower(elic$mini,hist$width)
        hist$upper<-fupper(elic$maxi,hist$width)
        hist$nbins<-fnbins(hist$lower,hist$upper,hist$width)
        hist$nchip<-fnchip(hist$nbins)
        hist$nhigh<-fnchip(hist$nbins)
        hist$lbins<-flbins(hist$lower,hist$upper,hist$width)
        hist$rbins<-frbins(hist$lower,hist$upper,hist$width)
        elic$value<-hist$rbins
        elic$height<-rep(0,hist$nbins)
        elic$probs<-rep(0,hist$nbins) 
        
      }
      
      elic$quant2[elic$que1]<-round(0.5 * (elic$mini+elic$maxi),digits=0)
      elic$quant1[elic$que1]<-round(0.5 * (elic$quant2[elic$que1]+elic$mini),digits=0)
      elic$quant3[elic$que1]<-round(0.5 * (elic$quant2[elic$que1]+elic$maxi),digits=0)
      
      if(elic$quant2[elic$que1]-elic$quant1[elic$que1]<2){
        temp<-elic$quant1
        if(temp-1>0){
          elic$quant1<-temp-1 
        }
      }
      
    })
  
    output$plot0<- renderPlot({
      par(ps=12,mar=c(4,0,0,0))  #ps: integer; the point size of text (but not symbols); mar: c(bottom, left, top, right)
      plot(c(hist$lower,hist$upper),c(0,0),
           xlim=c(hist$lower,hist$upper),
           ylim=c(-1,hist$nhigh),type="l",
           ylab="",
           xlab="Time (minutes) ",
           xaxp=c(hist$lower,hist$upper,hist$nbins),
           yaxt="n")
      
      for(i in 1:hist$nbins){
        lines(c(hist$lbins[i],hist$lbins[i]),
              c(0,hist$nhigh),lty=3,col=8)
      }
      
      lines(c(hist$rbins[hist$nbins],hist$rbins[hist$nbins]),
            c(0,hist$nhigh),lty=3,col=8)
      
      for(i in 1:hist$nhigh){
        lines(c(hist$lower,hist$upper),c(i,i), lty=3,col=8)
      }
      
      for(i in 1:hist$nbins){
        if(elic$height[i]>0){
          rect(rep(hist$lbins[i],elic$height[i]),c(0:(elic$height[i]-1)),
               rep(hist$rbins[i],elic$height[i]),c(1:elic$height[i]),col=2)
        } 
      }
    }, height = 330, width = 400)#530
    
    
    observeEvent(input$location0, {
      
      for (i in 1:hist$nbins){
        
        if(input$location0$x>hist$lbins[i] & input$location0$x<hist$rbins[i]){
          temp<-elic$height[i]
          
          if(input$location0$y>temp & input$location0$y<hist$nchip & round(sum(elic$height),digits=0)<hist$nchip){
            elic$height[i]<-temp+1
          }
          if(input$location0$y<temp & round(sum(elic$height),digits=0)>0){
            elic$height[i]<-temp-1
          }
        }
      }
    })
    
    output$plot<- renderPlot({
      par(ps=12,mar=c(4,0,0,0))  #ps: integer; the point size of text (but not symbols); mar: c(bottom, left, top, right)
      plot(c(hist$lower,hist$upper),c(0,0),
           xlim=c(hist$lower,hist$upper),
           ylim=c(-1,hist$nhigh),type="l",
           ylab="",
           xlab="Time (minutes) ",
           xaxp=c(hist$lower,hist$upper,hist$nbins),
           yaxt="n")
      
      for(i in 1:hist$nbins){
        lines(c(hist$lbins[i],hist$lbins[i]),
              c(0,hist$nhigh),lty=3,col=8)
      }
      
      lines(c(hist$rbins[hist$nbins],hist$rbins[hist$nbins]),
            c(0,hist$nhigh),lty=3,col=8)
      
      for(i in 1:hist$nhigh){
        lines(c(hist$lower,hist$upper),c(i,i), lty=3,col=8)
      }
      
      for(i in 1:hist$nbins){
        if(elic$height[i]>0){
          rect(rep(hist$lbins[i],elic$height[i]),c(0:(elic$height[i]-1)),
               rep(hist$rbins[i],elic$height[i]),c(1:elic$height[i]),col=2)
        } 
      }
    }, height = 330, width = 530)#530
    
    
    observeEvent(input$location, {
      
      for (i in 1:hist$nbins){
        
        if(input$location$x>hist$lbins[i] & input$location$x<hist$rbins[i]){
          temp<-elic$height[i]
          
          if(input$location$y>temp & input$location$y<hist$nchip & round(sum(elic$height),digits=0)<hist$nchip){
            elic$height[i]<-temp+1
          }
          if(input$location$y<temp & round(sum(elic$height),digits=0)>0){
            elic$height[i]<-temp-1
          }
        }
      }
    })
    
    output$questions1<-renderUI({
      
      tagList(div(
        strong(paste("Question ", elic$que1," of ",n_que, sep="")), br(), br(),
        strong(t_questions1[elic$que1]),br(),br(),#elic$que1,n_que,buttons$next_tab,cond$all_chips_2,
        ifelse(buttons$skip_que1[elic$que1]==0,
               tagList(div(t_skip_que1)),
               tagList(div(
                 f_text_minimax(elic$mini, elic$maxi),br(),
                 ifelse(buttons$show_plot==0,
                        tagList(div(t_text_hist)),
                        ifelse(elic$conds==0,
                               tagList(div(t_text_hist, br(), br(), t_text_nohist)),
                               tagList(div(
                                 t_text_update,
                                 f_text_grid(hist$nchip),br(),br(),
                                 f_text_count(hist$nchip, round(sum(elic$height),digits=0)),
                                 plotOutput("plot", click="location"),
                                 ifelse(buttons$enter_plot==0,#|round(sum(elic$height),digits=0)!=hist$nchip,
                                        tagList(div(t_enter_plot,br(),br(),br())),
                                        ifelse(cond$all_chips_1==1,
                                               tagList(div(
                                                 hr(),strong("Summary"),br(),br(),
                                                 "Your answers imply that",br(),br(),
                                                 div(f_text_fback_hist(elic$height, hist$nbins, hist$lbins, hist$rbins, t_questions2[elic$que1]),
                                                     style='width:700px; padding-left:45px;'), br(),br(),
                                                 t_summary_update,br(),
                                                 hr(),t_next_ques1)),
                                               tagList(div(t_enter_plot, br(),t_text_chip,br(),br(),br()))
                                        )
                                 )
                               )))
                 )
               )))

      ))
    })

    output$questions2<-renderUI({
      
      tagList(div(
        
        tagList(div(
          strong(paste("Question ", elic$part2," of ", sum(buttons$skip_que1) ,sep="")), br(), br(),
          f_questions2(elic$quant2[elic$que2],elic$quant1[elic$que2],t_questions2[elic$que2],t_questions3b[elic$que2]),br(),
                    f_text_props(elic$prop_min,elic$prop_max),br(),br(),
                    t_fback_que2,
                    ifelse(buttons$fback_que2==0,
                           tagList(div(br(),br(),br())),
                           ifelse(cond$prop_min_max==0,
                                  tagList(div(br(), br(), t_text_noprops)),
                                  ifelse(cond$good_fit2==0,
                                         tagList(div(br(), br(),
                                                     f_props_too_high(t_questions3b[elic$que2], elic$quant2[elic$que2]))),
                                         tagList(div(
                                           br(), hr(),
                                           strong("Summary"),br(),br(),
                                           f_que2_fback(elic$prop_min, elic$prop_max, elic$upper1, elic$upper2, t_questions3b[elic$que2], elic$quant1[elic$que2], elic$quant3[elic$que2]),
                                           t_next_ques2, br(), br(),
                                         ))
                                  )
                           )
                    )
        ))
        
        
      ))
      
    })
        
    
  output$intro_text<-renderUI({
    
    if(buttons$next_tab==-1){
      tagList(div(
        ifelse(buttons$first_time==0,
               tagList(div(t_session_timeout,)),
               ifelse(buttons$first_time==1,
                      tagList(div(br(),br(),
                                  fluidRow(column(9,f_session_timeout_new1(pros$password)),
                                           column(2,f_nexttab("Next"))))),
                      tagList(div(t_session_timeout_new2,
                                  ifelse(pros$correct==0,
                                         "",
                                         tagList(div("The pass code you have entered in incorrect. Please enter the code again. If this is the first time you are attempting the exercise, 
                                                     please close the window and start again."))))))),
        style='width:800px; padding-left:50px;'
      ))
    } else if (buttons$next_tab==0){
      tagList(div(t_introduction,
                
                  #Sys.getenv('SHINY_PORT'),
                  #environment(),
                  #t_session_timeout,
                  style='width:800px; padding-left:50px;'))
    } else if (buttons$next_tab==1){
      tagList(div(t_welcome,
                  style='width:800px; padding-left:50px;'))
    } else if (buttons$next_tab==2){
      tagList(div(t_about_you,style='width:800px; padding-left:50px;'))
    } else if (buttons$next_tab==3){
      tagList(div(
        t_type_of_que,
        style='width:800px; padding-left:50px;'))
    } else if (buttons$next_tab==4){
      tagList(div(
        t_part1,
        style='width:800px; padding-left:50px;'))
    } else if (buttons$next_tab==5){
      tagList(div(
        t_uncertainty1,
        HTML("<div style='height: 360px;'>"),
        imageOutput("blank_grid"),
        HTML("</div>"),
        t_uncertainty2,
        HTML("<div style='height: 360px;'>"),
        imageOutput("flat_prior"),
        HTML("</div>"),
        t_uncertainty3,
        HTML("<div style='height: 360px;'>"),
        imageOutput("cert_prior"), 
        HTML("</div>"),
        t_uncertainty4,
        HTML("<div style='height: 360px;'>"),
        imageOutput("norm_prior"), 
        HTML("</div>"),
        t_uncertainty5,
        HTML("<div style='height: 360px;'>"),
        imageOutput("bimo_prior"),
        HTML("</div>"),
        t_uncertainty6,
        fluidRow(
          column(7,br(),
                 f_text_count(hist$nchip, round(sum(elic$height),digits=0)),
                 HTML("<div style='height: 380px;'>"),
                 plotOutput("plot0", click="location0"),
                 HTML("</div>")),
          column(5,br(),br(),br(),
                 strong("Reminder:"),br(),br(),
                 "The horizontal line represents plausible values for the appointment duration split into bins, 
                 and will depend on the range you specified.", br(), br(),
                 "You can add chips to each bin by clicking anywhere within that bin.", br(), br(),
                 "You can remove a chip from a bin by clicking on any of the chips in that bin.", br(), br(),
                 "Each click adds/removes one chip.")
          ),
        ifelse(buttons$enter_plot==0,#|round(sum(elic$height),digits=0)!=hist$nchip,
               tagList(div(t_enter_plot,br(),br(),br())),
               ifelse(cond$all_chips_1==1,
                      tagList(div(
                        hr(),"Once you have placed all the chips on the grid, you will be presented with a summary of your response, like this:", br(), br(),
                        div(f_text_fback_hist(elic$height, hist$nbins, hist$lbins, hist$rbins, "a GP apointment, on average, takes"),
                            style='width:700px; padding-left:45px;'), br(),br(),
                        t_summary_update,br(),
                        hr(),
                        fluidRow( 
                          column(8, p(style="font-size:90%;", "Once you are satisfied that the statements represent your beliefs, click on 'Continue'.")),
                          column(1, offset=1, actionButton("next_tab", "Continue", width='120px', style="background-color: lightgrey")), br(),br(),br()))),
                      tagList(div(t_enter_plot, br(),t_text_chip,br(),br(),br()))
               )
        ),
        style='width:800px; padding-left:50px;'))
      } else if (buttons$next_tab==6) {
      tagList(div(br(),br(),
                  p("Please note that for all of these questions we are interested in duration 
                  timings of the investigation or treatment. This", strong("should not include the waiting 
                  time"), "unless otherwise stated.",br(),br(),
                    "Patient evaluation times will be assessed separately to the duration 
                    of investigations and treatments.",br(), br(),
                    "Please assume that you are considering only adults in the duration timings, 
                    unless otherwise specified."),
                  br(),
                  fluidRow(column(2,offset=9,
                                  #f_nexttab("Begin questions")
                                  actionButton("next_tab","Begin questions",width='150px', style="background-color: lightgrey")
                                  )),
        style='width:800px; padding-left:50px; font-size:15px;'))
    } else if(buttons$next_tab==7){
      
      tagList(div(br(),
                  uiOutput("questions1"),
                  br(),
                  style='width:800px; padding-left:50px;'))
      
    } else if(buttons$next_tab==8) {
      
      tagList(div(br(),br(),
                  t_part2,
                  br(),
                  style='width:800px; padding-left:50px;'))
      
    } else if(buttons$next_tab==9) {
      
         tagList(div(
           #t_que2_intro,
           t_que2_intro1, br(),
           HTML("<div style='height: 380px;'>"),
           imageOutput("gamma_p_20"),
           HTML("</div>"),
           HTML("<div style='height: 380px;'>"),
           imageOutput("gamma_p_40"),
           HTML("</div>"),
           br(),
           t_que2_intro2,
           f_questions2(13,11,"a GP appointment lasts, on average,","GP appointments"), br(),
           f_text_props(elic$prop_min,elic$prop_max),
           #t_que2_instructions,
           t_que2_instructions1, br(), 
           # fluidRow(
           #   column(6,
           #          HTML("<div style='height: 350px;'>"),
           #          imageOutput("gamma_p_20_outliers"), 
           #          HTML("</div>")),
           #   column(6,
           #          HTML("<div style='height: 350px;'>"),
           #          imageOutput("gamma_p_40_outliers"), 
           #          HTML("</div>"))),
           HTML("<div style='height: 380px;'>"),
           imageOutput("gamma_p_20_outliers"),
           HTML("</div>"),
           HTML("<div style='height: 380px;'>"),
           imageOutput("gamma_p_40_outliers"),
           HTML("</div>"),
           br(),
           t_que2_instructions2,
           br(), br(), br(),
           style='width:800px; padding-left:50px;'))
    } else if(buttons$next_tab==10) {
      
      tagList(div(br(),br(),
                  p("As before, all of these questions are about duration timings of the investigation or treatment. They", strong("should not include the waiting 
                  time"), "unless otherwise stated.",br(),br(),
                    "The duration of time required to evaluate a patient will be assessed separately to the 
                    duration of investigations and treatments.",br(), br(),
                    "Please assume that you are considering only adults in the duration timings, 
                    unless otherwise specified."),
                  br(),
                  fluidRow(column(2,offset=9,
                                  #f_nexttab("Begin questions")
                                  actionButton("next_tab","Begin questions",width='150px', style="background-color: lightgrey")
                                  )),
                  style='width:800px; padding-left:50px; font-size:15px;'))
      
    } else if(buttons$next_tab==11) {
      
      tagList(div(br(),
                  uiOutput("questions2"),
                  br(),
                  style='width:800px; padding-left:50px;'))
      
    } else if(buttons$next_tab==12) {
      
      tagList(div(br(),br(),
                  "Thank you for completing this exercise, you may close the window now.",
                  br(),
                  style='width:800px; padding-left:50px;'))
      
    }
    
  })
  

  
  
  
  
})