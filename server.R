library(readr)
library(readxl)
library(DT)
library(dplyr)
library(ggplot2)
library(openxlsx)

server <- function(input, output) {

  # Создаем переменную для хранения данных таблицы
  journalData <- reactiveValues(data = data.frame(name = character(), 
                                                  class = character(),
                                                  informatics = numeric(),
                                                  physics = numeric(),
                                                  mathemathics = numeric(),
                                                  literature = numeric(),
                                                  music = numeric()))
  
  observeEvent(input$loadButton, {
    inFile <- input$file1
    if (is.null(inFile)) {
      showNotification("Файл не выбран", type = "error")
      return()
    }
    
    tryCatch({
      if (tools::file_ext(inFile$datapath) == "csv") {
        journalData$data <- read_csv2(inFile$datapath)
      } else if (tools::file_ext(inFile$datapath) == "txt") {
        journalData$data <- read_delim(inFile$datapath, delim = ";")
      } else if (tools::file_ext(inFile$datapath) == "xlsx") {
        journalData$data <- read_excel(inFile$datapath)
      } else {
        showNotification("Неподдерживаемый формат файла", type = "error")
        return()
      }
      showNotification("Файл успешно загружен", type = "message")
    }, error = function(e) {
      showNotification(paste("Ошибка при загрузке файла:", e$message), type = "error")
    })
  })
  
  
  # Отображаем содержимое таблицы в обеих вкладках
  output$contents <- renderDataTable({
    req(journalData$data)  # Проверяем, что данные доступны
    datatable(journalData$data, options = list(lengthChange = FALSE))
  })
  
  # Используем renderDT вместо renderTable для редактируемой таблицы
  output$journalTable <- renderDT({
    req(journalData$data) # Проверяем, что данные доступны
    datatable(journalData$data, editable = TRUE, options = list(lengthChange = FALSE))
  }, server = FALSE)
  
  # Добавляем функциональность удаления строк
  observeEvent(input$deleteRow, {
    data <- journalData$data
    if (!is.null(data) && !is.na(input$rowToDelete) && input$rowToDelete <= nrow(data)) {
      data <- data[-input$rowToDelete, ]
      journalData$data <- data
      updateNumericInput(inputId="rowToDelete", value=NA);
    } else {
      showNotification("Неправильный номер строки", type = "error")
    }
  })
  
  observeEvent(input$addRecord, {
    data <- journalData$data
    if (!is.null(data) && !is.na(input$studentName) && !is.na(input$studentClass) &&
        !is.na(input$mark1) && !is.na(input$mark2) && !is.na(input$mark3) &&
        !is.na(input$mark4) && !is.na(input$mark5)) {
    newRow <- setNames(data.frame(ФИО = input$studentName, Класс = input$studentClass,
                                  informatics = input$mark1, physics = input$mark2,
                                  mathemathics = input$mark3, literature = input$mark4,
                                  music = input$mark5,
                                  stringsAsFactors = FALSE), names(data))
        journalData$data <- rbind(data, newRow) # Добавляем новую строку в таблицу
        updateTextInput(inputId="studentName", value="")
        updateTextInput(inputId="studentClass", value="")
        
        updateNumericInput(inputId="mark1", value=NA);
        updateNumericInput(inputId="mark2", value=NA);
        updateNumericInput(inputId="mark3", value=NA);
        updateNumericInput(inputId="mark4", value=NA);
        updateNumericInput(inputId="mark5", value=NA);
        showNotification("Запись добавлена", type = "message")

    } else {
      showNotification("Неправильные данные", type = "error")
    }
  })
    
    
    observeEvent(input$editRecord, {
      data <- journalData$data
      
      if (!is.null(data) && !is.na(input$rowToEdit) && input$rowToEdit <= nrow(data)) {
        
        # Update the selected row with user inputs
        if (!is.na(input$studentName) && nchar(input$studentName) > 0) {
          data[input$rowToEdit, ]$name <- input$studentName;
        }
        if (!is.na(input$studentClass) && nchar(input$studentClass) > 0) {
          data[input$rowToEdit, ]$class <- input$studentClass;
        }
        if (!is.na(input$mark1)) {
          data[input$rowToEdit, ]$informatics <- input$mark1;
        }
        if (!is.na(input$mark2)) {
          data[input$rowToEdit, ]$physics <- input$mark2;
        }
        if (!is.na(input$mark3)) {
          data[input$rowToEdit, ]$mathemathics <- input$mark3;
        }
        if (!is.na(input$mark4)) {
          data[input$rowToEdit, ]$literature <- input$mark4;
        }
        if (!is.na(input$mark5)) {
          data[input$rowToEdit, ]$music <- input$mark5;
        }
        
        updateNumericInput(inputId="rowToEdit", value=NA);
        
        updateTextInput(inputId="studentName", value="")
        updateTextInput(inputId="studentClass", value="")
        
        updateNumericInput(inputId="mark1", value=NA);
        updateNumericInput(inputId="mark2", value=NA);
        updateNumericInput(inputId="mark3", value=NA);
        updateNumericInput(inputId="mark4", value=NA);
        updateNumericInput(inputId="mark5", value=NA);
        # Update the reactive data with modified row
        journalData$data <- data 
        
        showNotification("Запись обновлена!", type="message");
      } else {
        showNotification("Неправильный номер строки или нет данных для редактирования.", type="error");
      }
    })
  
    # Обработчик для CSV
    output$downloadCsv <- downloadHandler(
      filename = function() {
        "journal.csv"
      },
      content = function(file) {
        data <- journalData$data
        if (!is.null(data) && nrow(data) > 0) {
          write_delim(data, file, delim = ";")
        }
      }
    )
    
    # Обработчик для XLSX
    output$downloadXlsx <- downloadHandler(
      filename = function() {
        "journal.xlsx"
      },
      content = function(file) {
        data <- journalData$data
        if (!is.null(data) && nrow(data) > 0) {
          write.xlsx(data, file)
        }
      }
    )
    
    # Обработчик для TXT
    output$downloadTxt <- downloadHandler(
      filename = function() {
        "journal.txt"
      },
      content = function(file) {
        data <- journalData$data
        if (!is.null(data) && nrow(data) > 0) {
          write_delim(data, file, delim = ";")
        }
      }
    )
  
  output$tableStats <- DT::renderDataTable({
    req(journalData$data)  # Проверяем, что данные загружены
    req(input$tableType)    # Проверяем, что выбран тип таблицы
    
    data <- journalData$data
    
    if (input$tableType == "Для каждого класса" && !is.null(data) && nrow(data) > 0) {
      return(generateClassTable())
    } else if (input$tableType == "Для всех классов" && !is.null(data) && nrow(data) > 0) {
      return(generateSubjectTable())
    }
  })
  

  output$graphStats <- renderPlot({
    req(journalData$data)  # Проверяем, что данные загружены
    req(input$graphType)   # Проверяем, что выбран тип графика
    
    data <- journalData$data
    
    if (input$graphType == "Для каждого класса" && !is.null(data) && nrow(data) > 0) {
      plotByClass()
    } else if (input$graphType == "Для всех классов" && !is.null(data) && nrow(data) > 0) {
      plotForAllClasses()
    }
  })

  # Функция для генерации таблицы по классам
  generateClassTable <- function() {
    data <- journalData$data
    
    subject_columns <- c("informatics", "physics", "mathemathics", "literature", "music")
    class_column <- "class"
    
    # Инициализация пустого датафрейма для статистики
    stats_data <- data.frame(
      Class = character(),
      Subject = character(),
      Grade = numeric(),
      Average = numeric(),
      Median = numeric(),
      Count = numeric(),
      Percentage = numeric(),
      stringsAsFactors = FALSE
    )
    
    # Вычисляем статистику для каждого класса и предмета
    for (class in unique(data[[class_column]])) {
      for (subject in subject_columns) {
        subject_data <- data[data[[class_column]] == class, subject, drop = TRUE]
        
        for (grade in unique(subject_data)) {
          count <- sum(subject_data == grade, na.rm = TRUE)
          percentage <- (count / length(subject_data)) * 100
          
          # Добавляем статистику с округлением
          stats_data <- rbind(stats_data, data.frame(
            Class = class,
            Subject = subject,
            Grade = grade,
            Average = round(mean(subject_data, na.rm = TRUE), 2),
            Median = round(median(subject_data, na.rm = TRUE), 2),
            Count = count,
            Percentage = round(percentage, 2),
            stringsAsFactors = FALSE
          ))
        }
      }
    }
    
    # Сортируем по классу, предмету и оценке
    stats_data <- stats_data[order(stats_data$Class, stats_data$Subject, stats_data$Grade), ]
    
    # Возвращаем датафрейм как DataTable
    DT::datatable(stats_data, options = list(pageLength = 10))
  }
  
  # Функция для генерации таблицы по предметам
  generateSubjectTable <- function() {
    data <- journalData$data
    
    subject_columns <- c("informatics", "physics", "mathemathics", "literature", "music")
    
    # Инициализация пустого датафрейма для статистики
    stats_data <- data.frame(
      Subject = character(),
      Grade = numeric(),
      Average = numeric(),
      Median = numeric(),
      Count = numeric(),
      Percentage = numeric(),
      stringsAsFactors = FALSE
    )
    
    # Вычисляем статистику для каждого предмета
    for (subject in subject_columns) {
      subject_scores <- data[[subject]]
      
      for (grade in unique(subject_scores)) {
        grade_count <- sum(subject_scores == grade, na.rm = TRUE)
        grade_percentage <- (grade_count / length(subject_scores)) * 100
        
        # Добавляем статистику с округлением
        stats_data <- rbind(stats_data, data.frame(
          Subject = subject,
          Grade = grade,
          Average = round(mean(subject_scores, na.rm = TRUE), 2),
          Median = round(median(subject_scores, na.rm = TRUE), 2),
          Count = grade_count,
          Percentage = round(grade_percentage, 2),
          stringsAsFactors = FALSE
        ))
      }
    }
    
    # Сортируем по предмету и оценке
    stats_data <- stats_data[order(stats_data$Subject, stats_data$Grade), ]
    
    # Возвращаем датафрейм как DataTable
    DT::datatable(stats_data, options = list(pageLength = 10))
  }
  
# Функция для построения графика по классам
  plotByClass <- function() {
    data <- journalData$data
    
    subject_columns <- c("informatics", "physics", "mathemathics", "literature", "music")
    class_column <- "class"
    
    stats_data <- data.frame(
      Class = character(),
      Subject = character(),
      Grade = numeric(),
      Average = numeric(),
      Median = numeric(),
      Count = numeric(),
      Percentage = numeric(),
      stringsAsFactors = FALSE
    )
    
    for (class in unique(data[[class_column]])) {
      for (subject in subject_columns) {
        subject_data <- data[data[[class_column]] == class, subject, drop = TRUE]
        
        if (length(subject_data) > 0) {  
          for (grade in unique(subject_data)) {
            count <- sum(subject_data == grade, na.rm = TRUE)
            percentage <- count / length(subject_data) * 100
            
            stats_data <- rbind(stats_data, data.frame(
              Class = class,
              Subject = subject,
              Grade = grade,
              Average = round(mean(subject_data, na.rm = TRUE), 2),
              Median = round(median(subject_data, na.rm = TRUE), 2),
              Count = count,
              Percentage = round(percentage, 2),
              stringsAsFactors = FALSE
            ))
          }
        }
      }
    }
    
    stats_data <- stats_data[order(stats_data$Class, stats_data$Subject, stats_data$Grade), ]
    
    ggplot(stats_data, aes(x = Grade, y = Count, fill = Subject)) +
      geom_bar(stat="identity", position="dodge") +
      facet_wrap(~ Class) +
      labs(x="Оценка", y="Количество", fill="Предмет") +
      theme_bw()
  }
  
  # Функция для построения графика для всех классов
  plotForAllClasses <- function() {
    data <- journalData$data
    
    subject_columns <- c("informatics", "physics", "mathemathics", "literature", "music")
    
    stats_data <- data.frame(Subject=character(), Grade=numeric(),
                             Average=numeric(), Median=numeric(),
                             Count=numeric(), Percentage=numeric(),
                             stringsAsFactors=FALSE)
    
    for (subject in subject_columns) {
      subject_scores <- data[[subject]]
      
      if (length(subject_scores) > 0) {  
        for (grade in unique(subject_scores)) {
          count <- sum(subject_scores == grade, na.rm=TRUE)
          percentage <- (count / length(subject_scores)) * 100;
          
          stats_data <- rbind(stats_data, data.frame(
            Subject=subject,
            Grade=grade,
            Average=mean(subject_scores, na.rm=TRUE),
            Median=median(subject_scores, na.rm=TRUE),
            Count=count,
            Percentage=percentage,
            stringsAsFactors=FALSE
          ))
        }
      }
    }
    
    stats_data <- stats_data[order(stats_data$Subject, stats_data$Grade), ]
    
    ggplot(stats_data, aes(x=Grade, y=Count, fill=Subject)) +
      geom_bar(stat="identity", position="dodge") +
      labs(x="Оценка", y="Количество", fill="Предмет") +
      theme_bw()
  }
  
}