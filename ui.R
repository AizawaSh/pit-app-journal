library(shiny)
library(shinythemes)

# Определение пользовательского интерфейса
ui <- fluidPage(
  theme = shinytheme("flatly"),
  
  # Заголовок приложения
  titlePanel("Журнал оценок"),
  
  # Табы для различных функций
  tabsetPanel(
    type = "tabs",
    
    # Вкладка для загрузки данных
    tabPanel("Загрузка данных",
             sidebarLayout(
               sidebarPanel(
                  fileInput("file1","Выберите файл",
                         accept = c(".csv",".txt",".xlsx")),
                  actionButton("loadButton", "Загрузить")),
                  mainPanel(dataTableOutput("contents")),
              )
    ),
    
    # Вкладка для создания/редактирования журнала
    tabPanel("Журнал",
             sidebarLayout(
               sidebarPanel(
                 numericInput("rowToEdit", "Номер строки для редактирования", value = NA),
                 textInput("studentName", "ФИО ученика"),
                 textInput("studentClass", "Класс"),
                 numericInput("mark1", "Оценка (Информатика)", min = 1, max = 5, value = NA),
                 numericInput("mark2", "Оценка (Физика)", min = 1, max = 5, value = NA),
                 numericInput("mark3", "Оценка (Математика)", min = 1, max = 5, value = NA),
                 numericInput("mark4", "Оценка (Литература)", min = 1, max = 5, value = NA),
                 numericInput("mark5", "Оценка (Музыка)", min = 1, max = 5, value = NA),
                 actionButton("addRecord", "Добавить запись"),
                 actionButton("editRecord", "Редактировать запись"),
                 numericInput("rowToDelete", "Номер строки для удаления", value = NA),
                 actionButton("deleteRow", "Удалить строку"),
                 br(),
                 tags$div(style="font-size: 16px; font-weight: bold; margin-top: 10px;", 
                          "Сохранить журнал:"),
                 br(),
                 downloadButton("downloadCsv", "Скачать CSV"),
                 downloadButton("downloadXlsx", "Скачать XLSX"),
                 downloadButton("downloadTxt", "Скачать TXT"),
                 dataTableOutput("journalTable")
               ),
               mainPanel(
                 dataTableOutput("journalTable")
               )
             )
    ),
    
    # Вкладка для вывода статистики в табличном виде
    tabPanel("Статистика (Таблица)",
             sidebarLayout(
               sidebarPanel(
                 selectInput("tableType", "Тип таблицы", choices = c("Для каждого класса", "Для всех классов")),
               ),
               mainPanel(
                 dataTableOutput("tableStats")
               )
             )
    ),
    
    # Вкладка для вывода статистики в графическом виде
    tabPanel("Статистика (График)",
             sidebarLayout(
               sidebarPanel(
                 selectInput("graphType", "Тип графика", choices = c("Для каждого класса", "Для всех классов"))
               ),
               mainPanel(
                 plotOutput("graphStats")
               )
             )
    ),
    
    # Вкладка "Помощь"
    tabPanel("Помощь",
             h3("Вкладка \"Загрузка данных\""),
             p("Эта вкладка позволяет пользователям загружать оценки из файлов различных форматов."),
             p("Кнопка загрузки файла: пользователи могут выбрать файл для загрузки в формате .csv, .txt или .xlsx."),
             p("Пример файла: Файл должен содержать следующие столбцы:"),
             tags$ul(
               tags$li("name: ФИО ученика"),
               tags$li("class: Класс ученика (например, \"10 А\")"),
               tags$li("informatics: Оценка по информатике (числовое значение)"),
               tags$li("physics: Оценка по физике (числовое значение)"),
               tags$li("mathematics: Оценка по математике (числовое значение)"),
               tags$li("literature: Оценка по литературе (числовое значение)"),
               tags$li("music: Оценка по музыке (числовое значение)")
             ),
             h3("Вкладка \"Журнал\""),
             p("Эта вкладка предназначена для создания и редактирования записей о учениках."),
             p("Таблица для просмотра имеющихся значений: пользователи могут видеть текущие записи в журнале оценок."),
             p("Поля для ввода ФИО и оценок ученика: здесь пользователи могут вводить информацию о новом ученике или редактировать существующую."),
             p("Кнопка удаления записи: Позволяет пользователям удалять записи отдельных учеников из журнала."),
             p("Кнопка редактирования записей: пользователи могут редактировать записи выбранного ученика, обновляя информацию о ФИО и оценках."),
             p("Кнопка для скачивания журнала: пользователи могут ввести жедаемое имя файла и выбрать удобный формат скачивания"),
             h3(" Вкладка \"Статистика (Таблица)\""),
             p("Эта вкладка выводит статистику оценок в виде таблицы."),
             p("Статистика для каждого класса и предмета: Для каждого класса выводится средняя оценка, медиана, количество и процент учеников для каждой оценки по всем предметам."),
             p("Статистика по всем ученикам: для всех классов выводится средняя оценка, медиана, количество и процент учеников с каждой оценкой по каждому предмету."),
             h3("Вкладка \"Статистика (График)\""),
             p("Эта вкладка позволяет визуализировать статистику оценок в графическом виде")
    ),
    
    # Вкладка "О программе"
    tabPanel("О разработчике",
             h3("Разработчик"),
             p("Жданова Виктория Алексеевна, E-mail: vika-jdanova@rambler.ru"),
             p("Связаться со мной в Telegram: ", 
               tags$a(href = "https://t.me/AizawaSh", "@AizawaSh"),  # Замените "ваш_пользователь" на ваш Telegram username
               "."),
             img(src = "image.jpg", height = 200, width = 200)
    )
  )
)