library("shiny")
library("ggplot2")
library("dplyr")
library("tidyr")
library("shinythemes")

music <- read.csv("data/responses.csv", stringsAsFactors=FALSE)
rawData <- read.csv("data/responses.csv")
data <- select(rawData, 141, 145, 2, 133, 147, (3:19))

first_page <- tabPanel(
  "Project Overview",
  mainPanel(
    tags$img(src = "Logo.jpg", height = 200, width = 200), h1("App Introduction", style = "display: inline"),
    h3("Things we want our audience to explore:"), 
    tags$p("Our application helps visualize the relationships between different data factors and the choice of various genres of music. The possible data factors that might correlate with the genres of music are: age, gender, education level, and time spent on the internet every day. For table & plots, there are some widgets that users could interact with to find out the elements that play a crucial role in users' music tastes."),
    tags$a(href="https://www.kaggle.com/boltmaud/musics-depending-on-demographic-data/data?select=rules.json", "Click here to view data source!"),
    
    h3("About the data:"), 
    tags$p("The dataset we have been working on is a survey intended to explore the music preferences and opinions of young people in Slovakia. All participants were of Slovakian nationality, aged between 15-30. (* the data contains a few missing values)"),
    
    h3("Variables in the dataset:"), 
    tags$div(tags$b("DEMOGRAPHICS"), tags$br(), "Age", tags$br(), "Gender: Female / Male", tags$br(), "Education: highest education achieved", tags$br(), "Currently a Primary school pupil / Primary school / Secondary school / College or Bachelor degree"),
    tags$br(),
    tags$div(tags$b("PERSONALITY TRAITS, VIEWS ON LIFE & OPINIONS"), tags$br(), "Internet.usage: time spent online per day", tags$br(), "No time at all / Less than an hour a day / Few hours a day / Most of the day"),
    tags$br(),
    tags$div(tags$b("MUSIC PREFERENCES"), tags$br(), "Slow.songs.or.fast.songs: prefer slow paced music 1; perfer fast paced music 5 (rated with 1-5)", tags$br(), "Preferences on 12 different genres of music, rated between score 1 - 5 (integer). Score 1 stands for 'don't enjoy at all'; 5 stands for 'enjoy very much'", 
             tags$br(), "Dance, Disco, Funk music", tags$br(), "Folk", tags$br(), "Country", tags$br(), "Classical", tags$br(), "Musicals", tags$br(), "Pop", tags$br(), "Rock", tags$br(), "Metal, Hard rock", tags$br(), "Punk", tags$br(), "Hip hop, Rap", tags$br(), "Reggae, Ska", tags$br(), "Swing, Jazz", tags$br(), "Rock n Roll",
             tags$br(), "Alternative music", tags$br(), "Latin", tags$br(), "Techno, Trance", tags$br(), "Opera"),
  )
)

sixth_page <- tabPanel(
  "About Us",
  mainPanel(
    h3("Group Members"),
    h4("Brandon Bu"),
    tags$p("Male, Education Level: college, Internet usage: most of the day, Favorite song: Dancing with your ghost"),
    tags$img(src = "song1.png", height = 200, width = 150),
    h4("Mina Gao"),
    tags$p("Female, Education Level: college, Internet usage: most of the day, Favorite song: Good Day - Surfaces"),
    tags$img(src = "song2.png", height = 200, width = 150),
    h4("Zihan Lin"),
    tags$p("Male, Education Level: college, Internet usage: most of the day, Favorite song: Leave the door open (Jazz, pop)"),
    tags$img(src = "song3.png", height = 200, width = 150),
    h4("Vivian Yu"),
    tags$p("Female, Education Level: college, Internet usage: most of the day, Favorite song: 2002, If I can't have you, 10000 hours... (basically pop songs, bossa nova, Jazz)"),
    tags$img(src = "song4.png", height = 200, width = 150),
  )
)

second_page <- tabPanel(
  "Test Your Genre",
  sidebarLayout(
    sidebarPanel(
      uiOutput("Gender"),
      uiOutput("Education"),
      uiOutput("Internet")
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(
        tabPanel("Genre analysis",
                 br(),
                 textOutput("genre_description"),
                 br(),
                 plotOutput("genre_bar"),
                 htmlOutput("bar_message")),
        tabPanel("Pie",
                 br(),
                 textOutput("pie_description"),
                 br(),
                 plotOutput("music_pie"),
                 textOutput("pie_message"))
      )
    )
  )
)

third_page <- 
  tabPanel(
  "Male/Female Interest towards Genres",
  sidebarLayout(
  sidebarPanel(
    uiOutput("Genre")
  ),
  # Show a plot of the generated distribution
  mainPanel(
    plotOutput("genre_line")
  )
)
)


fourth_page <- tabPanel(
  "Searching Table",
  fluidRow(
    column(4,
           selectInput("age",
                       "Age:",
                       c("All",
                         unique(as.character(data$Age))))
    ),
    column(4,
           selectInput("gender",
                       "Gender:",
                       c("All",
                         unique(as.character(data$Gender))))
    ),
    column(4,
           selectInput("speed",
                       "Tempo preference(1-5):",
                       c("All",
                         unique(as.character(data$Slow.songs.or.fast.songs))))
    ),
    column(4,
           selectInput("internet",
                       "Hours spend on internet:",
                       c("All",
                         unique(as.character(data$Internet.usage))))
    ),
    column(4,
           selectInput("education",
                       "Education level:",
                       c("All",
                         unique(as.character(data$Education))))
    )
  ),
  textOutput("table_description"),
  br(),
  DT::dataTableOutput("table"),
)

fifth_page <- tabPanel(
  "Summary",
  mainPanel(
    plotOutput("summary_graph"),
    h3("Broader implications of the insights"),
    tags$p("According to the results from our table, we realized that regardless of the gender and internet usage, 
       college students like rock music the most. Additionally, at the age of 25, there is a large difference in gender of
       listening classical music. Furthermore, as the pie chart shows, no matter how we filter the sample, we noticed that
       most of the participants enjoy listening to music."),
    h3("Data quality"),
    tags$p("As for our data quality, the sample size of our data is about 1000 and about 150 columns contains different 
           information, like music type and demographics. It contains reasonable high quality information and there are 
           only a few missing data . However, there are some bias because the data is collected from young people in Slovakian. 
           These bias of age and locations would means “rock” may not be the most popular music type in other region or at other 
           ages. In addition, gender column only includes male and female. This mould exclude some people from the participants. 
           Since the participants are friends invited from students of Stats class in FSEV UK, these students are more likely 
           to invite friends who have a similar music preference. This could also cause a bias."),
    h3("Future ideas"),
    tags$p("Besides the factors like gender, education level, and internet usage, there are more interesting
       factors collected from the dataset that could be implemented into our visualization to show the users
       what other factors that could have correlation with the music tastes. These include: whether one has cheated in school,
       whether one has siblings, or how often they drink etc. The detailed information we consider, the more features we 
       could provide to the users. We would like to advance our project by offering users the name of the songs they might like
       when they input their personal information in our 'Test Your Genre' feature."),
  )
)

ui <- fluidPage(theme = shinytheme("flatly"),
                navbarPage("Music Preference Analysis",
                           first_page,
                           second_page,
                           third_page,
                           fourth_page,
                           fifth_page,
                           sixth_page
                )
)