library(tidyverse)
library(gt)

#  Data available from https://www.mockaroo.com/6c4871a0
resp <- read_csv("data/respiratory.csv")


# Group data by hospital and pathogen
resp_hosp_group <- resp %>% 
    group_by(hospital, condition) %>% 
    tally() %>% 
    arrange(desc(n)) %>% 
    pivot_wider(names_from = condition, values_from = n) %>% 
    ungroup() 

#  Make a nice table
resp_hosp_group %>% 
    gt() %>% 
    tab_header(
        title = "Common respiratory pathogens in fictional hospitals between 1 Jan 2021 and 30 June 2022",
        subtitle = md("Data generated from [mockeroo](https://www.mockaroo.com/6c4871a0)")
        ) %>% 
    opt_align_table_header(align = "left") %>%
    cols_label(
        `Respiratory syncytial virus` = "RSV",
        `Parainfluenza virus` = "Parainfluenza",
        hospital = "Hospital"
    ) %>% 
    tab_footnote(
        footnote = "Respiratory syncytial virus",
        locations = cells_column_labels(
            columns = c(`Respiratory syncytial virus`)
        )
    ) %>%
    tab_footnote(
        footnote = "Includes human parainfluenza subtypes 1, 2, and 3",
        locations = cells_column_labels(
            columns = c(
                `Parainfluenza virus`
            )
        )
    ) %>%
    opt_footnote_marks(marks = c("*", "#", "^",  "~")) %>% 
    cols_align(align = c("left"), columns = everything()) %>%
    cols_align(align = c("right"), columns = where(is.numeric)) %>% 
    cols_width(hospital ~ px(200),
        everything() ~ px(120)) %>% 
    tab_style(
        locations = cells_column_labels(),
        style = list(
            cell_fill("#183C5D"),
            cell_text("#FFFFFF", weight = 500)
        ) 
    ) %>% 
    tab_style(
        locations = cells_body(
            columns = hospital
        ),
        style = list(
            cell_fill("#0F5CA2"),
            cell_text("#FFFFFF", weight = 500)
        ) 
    ) %>% 
    tab_style(
       locations = cells_body(
           style = list(
               cell_text(size = 9)
           )
       ) 
    )

