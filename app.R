library(shiny)
library(shinydashboard)
library(readr)
library(readxl)
library(DT)
library(dplyr)
library(ggplot2)

ui <- dashboardPage(
  skin = "purple",
  dashboardHeader(title = "Журнал оценок"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Загрузка журнала", tabName = "upload", icon = icon("download")),
      menuItem("Редактировать журнала", tabName = "edit-section", icon = icon("pen")),
      menuItem("Статистика(таблица)", tabName = "table-stats", icon = icon("table")),
      menuItem("Статистика(график)", tabName = "graph-stats", icon = icon("chart-simple")),
      menuItem("Помощь", tabName = "help", icon = icon("circle-question")),
      menuItem("О программе", tabName = "about", icon = icon("circle-info"))
    )
  ),
  dashboardBody(
    tabItems(
      
      tabItem(#Вкладка "О программе"
        tabName = "about",
        box(
          status = "info",
          width = 15,
          HTML("
            <h2>О программе</h2>
            <p>ФИО разработчика: Зинаков Никита Андреевич </p>
            <p> Контактные данные:</p>
            <p> VK <a href='https://YourKeonOne'> Зинаков Никита </a> </p>
            <p> Telegram <a href='https://t.me/YourKeonOne'> @YourKeonOne </a> </p>"),
          img(src = "developer.jpg", height = 500, with = 100)
        )
      ),
      
      tabItem(# Вкладка "Помощь"
        tabName="help",
        box(
          status = "info",
          width = 15,
          HTML("
            <h2>Помощь</h2>
            <p>Электронный дневник предлагает следующие функции, распределенные по вкладкам:</p>
            <h3>Загрузить оценки</h3>
            <p>На этой вкладке вы можете загрузить файлы с оценками в форматах .csv, .txt или .xlsx. Для начала загрузки нажмите на кнопку *Выбрать и загрузить файл*. После выбора файла он будет автоматически обработан и отображен в виде таблицы на странице.</p>
            <h3>Редактировать журнал</h3>
            <p>Эта вкладка позволяет добавлять и изменять записи оценок учеников. Для добавления или редактирования информации заполните форму данными ученика и нажмите кнопку *Сохранить*.</p>
            <h3>Статистика (таблица)</h3>
            <p>В этом разделе вы можете увидеть статистику по классам и предметам: средний балл, медиану оценок, количество и процентное распределение оценок по каждому предмету.</p>
            <h3>Статистика (график)</h3>
            <p>Здесь представлена статистика в графическом виде. Графики могут помочь вам лучше визуализировать распределение оценок и успеваемость учеников.</p>
            <h3>О программе</h3>
            <p>На этой вкладке содержится информация о создателях электронного дневника, их контактные данные и другие сведения о проекте.</p>
          ")
        )
      ),
      
      tabItem(# Вкладка "Загрузка файла"
        tabName="upload",
        fileInput("upload","Выберете файл для загрузки", 
                  buttonLabel = "Выберете файл",
                  width = 500,
                  placeholder = "Файл не найден",
                  accept = c(".csv",".txt",".xlsx")),
        box( title = "Просмотр содержимого", 
             status = "info", 
             height = "545",
             width = "14",
             solidHeader = T, 
             column(width = 12,
                    tableOutput("uploadTable"),
                    style = "height:485px; overflow-y: scroll;overflow-x: scroll;")
        )
      ),
      tabItem(# Вкладка "Редактирование"
        tabName = "edit-section",
        box(
          title = "Редактирование журнала",
          status = "primary",
          solidHeader = TRUE,
          collapsible = TRUE,
          width = 30,
          DTOutput("editTable"),
          numericInput("rowToDelete", "Номер строки для удаления", value = NA),
          actionButton("deleteRow", "Удалить строку"),
          actionButton("addRow", "Добавить строку"), # Новая кнопка для добавления строки
          actionButton("saveAsCsv", "Сохранить как CSV"), # Новая кнопка для сохранения в CSV
          actionButton("saveAsXlsx", "Сохранить как XLSX"), # Новая кнопка для сохранения в XLSX
          actionButton("saveAsTxt", "Сохранить как TXT"), # Новая кнопка для сохранения в TXT
          textInput("fileName", "Введите название файла для сохранения", value = "table"),
        )
      ),
      tabItem(# Вкладка "статистика таблицей"
        tabName = "table-stats",
        tabBox(
          id = "tabset1",
          title = "Статистика",
          width = 12,
          tabPanel(
            "По классам и предметам",
            DT::dataTableOutput("statsTable")
          ),
          tabPanel(
            "По предметам",
            DT::dataTableOutput("subjectStatsTable")
          )
        )
      ),
      tabItem(#Вкладка "статистика графиком"
        tabName = "graph-stats",
        tabBox(
          id = "tabset1",
          title = "Статистика",
          width = 12,
          tabPanel(
            "По классам и предметам",
            plotOutput("classSubjectStatsPlot")
          ),
          tabPanel(
            "По предметам",
            plotOutput("subjectStatsPlot")
          )
        )
      )
    )
  )
)

server <- function(input, output) {
  
  # Создаем переменную для хранения данных таблицы
  tableData <- reactiveVal()
  
  # Загружаем файл и сохраняем данные в переменной tableData
  observeEvent(input$upload, {
    file <- input$upload
    ext <- tools::file_ext(file$datapath)
    
    if (ext == "csv") {
      data <- read_csv2(file$datapath)
    } else if (ext == "txt") {
      data <- read_delim(file$datapath, delim = ";")
    } else if (ext == "xlsx") {
      data <- read_excel(file$datapath)
    } else {
      showNotification("Неподдерживаемый формат файла", type = "error")
      return()
    }
    
    tableData(data)
  })
  
  # Отображаем содержимое таблицы в обеих вкладках
  output$uploadTable <- renderTable({
    tableData()
  })
  
  # Используем renderDT вместо renderTable для редактируемой таблицы
  output$editTable <- renderDT({
    req(tableData()) # Проверяем, что данные доступны
    datatable(tableData(), editable = TRUE, options = list(lengthChange = FALSE))
  }, server = FALSE)
  
  # Добавляем функциональность удаления строк
  observeEvent(input$deleteRow, {
    data <- tableData()
    if (!is.null(data) && !is.na(input$rowToDelete) && input$rowToDelete <= nrow(data)) {
      data <- data[-input$rowToDelete, ]
      tableData(data)
    } else {
      showNotification("Неправильный номер строки", type = "error")
    }
  })
  
  #Добавляем пустую строку
  observeEvent(input$addRow, {
    data <- tableData()
    if (!is.null(data)) {
      newRow <- rep(NA, ncol(data)) # Создаем новую пустую строку
      tableData(rbind(data, newRow)) # Добавляем новую строку в таблицу
    } else {
      showNotification("Нет данных для добавления строки", type = "error")
    }
  })
  
  # Сохраняем изменения в таблице
  observeEvent(input$editTable_cell_edit, {
    info <- input$editTable_cell_edit
    str(info)
    data <- tableData()
    data[info$row, info$col] <- info$value
    tableData(data)
  })
  
  # Отслеживаем изменения в таблице и сохраняем их в переменной tableData
  observeEvent(input$editTable_cell_edit, {
    info <- input$editTable_cell_edit
    data <- tableData()
    data[info$row, info$col] <- info$value
    tableData(data)
  })
  #Сохранить как ксв
  observeEvent(input$saveAsCsv, {
    data <- tableData()
    if (!is.null(data)) {
      write_csv(data, paste0(input$fileName, ".csv"))
      showNotification("Таблица сохранена как CSV", type = "message")
    } else {
      showNotification("Нет данных для сохранения", type = "error")
    }
  })
  #сохранить как xlsx
  observeEvent(input$saveAsXlsx, {
    data <- tableData()
    if (!is.null(data)) {
      write_excel_csv(data, paste0(input$fileName, ".xlsx"))
      showNotification("Таблица сохранена как XLSX", type = "message")
    } else {
      showNotification("Нет данных для сохранения", type = "error")
    }
  })
  #сохранить как тхт
  observeEvent(input$saveAsTxt, {
    data <- tableData()
    if (!is.null(data)) {
      write_delim(data, paste0(input$fileName, ".txt"), delim = ";")
      showNotification("Таблица сохранена как TXT", type = "message")
    } else {
      showNotification("Нет данных для сохранения", type = "error")
    }
  })
  
  output$statsTable <- DT::renderDataTable({
    data <- tableData()
    
    if (!is.null(data)) {
      # Выберем только колонки с предметами и классом
      subject_columns <- c("informatics", "physics", "mathemathics", "literature", "music")
      class_column <- "class"
      
      # Создадим пустой датафрейм для хранения статистики
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
      
      # Вычислим статистику для каждого класса и предмета
      for (class in unique(data[[class_column]])) {
        for (subject in subject_columns) {
          subject_data <- data[data[[class_column]] == class, subject, drop = TRUE]
          for (grade in unique(subject_data)) {
            count <- sum(subject_data == grade)
            percentage <- count / length(subject_data) * 100
            stats_data <- rbind(stats_data, data.frame(
              Class = class,
              Subject = subject,
              Grade = grade,
              Average = mean(subject_data, na.rm = TRUE),
              Median = median(subject_data, na.rm = TRUE),
              Count = count,
              Percentage = percentage,
              stringsAsFactors = FALSE
            ))
          }
        }
      }
      
      # Отсортируем данные по классу и предмету
      stats_data <- stats_data[order(stats_data$Class, stats_data$Subject, stats_data$Grade), ]
      
      # Вернем датафрейм в виде таблицы
      DT::datatable(stats_data, options = list(pageLength = 10))
    } else {
      DT::datatable(NULL)
    }
  })
  
  output$subjectStatsTable <- DT::renderDataTable({
    data <- tableData()
    
    if (!is.null(data)) {
      # Выберем только колонки с предметами и классом
      subject_columns <- c("informatics", "physics", "mathemathics", "literature", "music")
      
      # Создадим пустой датафрейм для хранения статистики
      stats_data <- data.frame(
        Subject = character(),
        Average = numeric(),
        Median = numeric(),
        Count = numeric(),
        Percentage = numeric(),
        stringsAsFactors = FALSE
      )
      
      # Вычислим статистику для каждого предмета
      for (subject in subject_columns) {
        subject_data <- data[[subject]]
        for (grade in unique(subject_data)) {
          count <- sum(subject_data == grade)
          percentage <- count / length(subject_data) * 100
          stats_data <- rbind(stats_data, data.frame(
            Subject = subject,
            Grade = grade,
            Average = mean(subject_data, na.rm = TRUE),
            Median = median(subject_data, na.rm = TRUE),
            Count = count,
            Percentage = percentage,
            stringsAsFactors = FALSE
          ))
        }
      }
      
      # Отсортируем данные по предмету
      stats_data <- stats_data[order(stats_data$Subject, stats_data$Grade), ]
      
      # Вернем датафрейм в виде таблицы
      DT::datatable(stats_data, options = list(pageLength = 10))
    } else {
      DT::datatable(NULL)
    }
  })
  
  output$classSubjectStatsPlot <- renderPlot({
    data <- tableData()
    
    if (!is.null(data)) {
      # Выберем только колонки с предметами и классом
      subject_columns <- c("informatics", "physics", "mathemathics", "literature", "music")
      class_column <- "class"
      
      # Создадим пустой датафрейм для хранения статистики
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
      
      # Вычислим статистику для каждого класса и предмета
      for (class in unique(data[[class_column]])) {
        for (subject in subject_columns) {
          subject_data <- data[data[[class_column]] == class, subject, drop = TRUE]
          for (grade in unique(subject_data)) {
            count <- sum(subject_data == grade)
            percentage <- count / length(subject_data) * 100
            stats_data <- rbind(stats_data, data.frame(
              Class = class,
              Subject = subject,
              Grade = grade,
              Average = mean(subject_data, na.rm = TRUE),
              Median = median(subject_data, na.rm = TRUE),
              Count = count,
              Percentage = percentage,
              stringsAsFactors = FALSE
            ))
          }
        }
      }
      
      # Отсортируем данные по классу и предмету
      stats_data <- stats_data[order(stats_data$Class, stats_data$Subject, stats_data$Grade), ]
      
      # Создадим график
      ggplot(stats_data, aes(x = Grade, y = Count, fill = Subject)) +
        geom_bar(stat = "identity", position = "dodge") +
        facet_wrap(~ Class) +
        labs(x = "Оценка", y = "Количество", fill = "Предмет") +
        theme_minimal()
    } else {
      plot(0, 0, type = "n", axes = FALSE, xlab = "", ylab = "")
    }
  })
  
  output$subjectStatsPlot <- renderPlot({
    data <- tableData()
    
    if (!is.null(data)) {
      # Выберем только колонки с предметами и классом
      subject_columns <- c("informatics", "physics", "mathemathics", "literature", "music")
      
      # Создадим пустой датафрейм для хранения статистики
      stats_data <- data.frame(
        Subject = character(),
        Grade = numeric(),
        Average = numeric(),
        Median = numeric(),
        Count = numeric(),
        Percentage = numeric(),
        stringsAsFactors = FALSE
      )
      
      # Вычислим статистику для каждого предмета
      for (subject in subject_columns) {
        subject_data <- data[[subject]]
        for (grade in unique(subject_data)) {
          count <- sum(subject_data == grade)
          percentage <- count / length(subject_data) * 100
          stats_data <- rbind(stats_data, data.frame(
            Subject = subject,
            Grade = grade,
            Average = mean(subject_data, na.rm = TRUE),
            Median = median(subject_data, na.rm = TRUE),
            Count = count,
            Percentage = percentage,
            stringsAsFactors = FALSE
          ))
        }
      }
      
      # Отсортируем данные по предмету
      stats_data <- stats_data[order(stats_data$Subject, stats_data$Grade), ]
      
      # Создадим график
      ggplot(stats_data, aes(x = Grade, y = Count, fill = Subject)) +
        geom_bar(stat = "identity", position = "dodge") +
        labs(x = "Оценка", y = "Количество", fill = "Предмет") +
        theme_minimal()
    } else {
      plot(0, 0, type = "n", axes = FALSE, xlab = "", ylab = "")
    }
  })
  
}

shinyApp(ui, server)
