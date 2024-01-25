#remove objects 
rm(list =ls(all = T))

#set working environment 
setwd("E:/Archives/The Point Sets")

#libraries 
library(tidyverse)
library(janitor)
library(showtext)
library(ggtext)

#add fonts
font_add_google("Quattrocento Sans", "raleway")
showtext_auto()

#get the fonts 
font_family1 = 'Quattrocento Sans'
font_family2 = 'raleway'

highlight_color = "#28331b"
highlight_color2 = "#131613"
highlight_color3 = "gray12"
highlight_color4 = "#eeb41c"

caption_label = 'Data Visualizations by Point Analytics\n Data Source:Job Adverts in Kenya\n Email: datapowereddecisons@gmail.com \n X: @_PointAnalytics Instagram:_pointanalytics'
subtitle = "\n**The Kenyan job market landscape:** Sales, Marketing, and Business Development claim the top spots, <br/>
followed closely by Accounting & Finance and Information Communication Technology. <span style='color:#eeb41c; '>**Data, AI, 
<br/>and Business Analytics**</span>  occupy 11th place with <span style='color:#eeb41c; '>**6,024 job postings**</span>, representing a significant  <span style='color:#eeb41c; '>**3.73%** </span><br/>
of the market, signifying a burgeoning demand for AI-related skills."

subtitle2 = "\nThe Kenyan job market landscape: Sales, Marketing, and Business Development claim the top spots,\nfollowed closely by Accounting & Finance and Information Communication Technology. Data, AI, \nand Business Analytics  occupy 11th place with 6,024 job postings, representing a significant 3.73% \nof the market, signifying a burgeoning demand for AI-related skills."

print("While traditional sectors like Sales and Accounting dominate Kenya's job market, a new star is rising: Data, AI, and Business Analytics. Already in 11th place with a significant 3.7%, this category surpasses established fields like Law and Marketing, showcasing a surging demand for AI skills across diverse industries. Kenya's job market is not just keeping pace with the global data revolution, it's actively embracing it.")

#print numerics 
options(digits = 3)
options(scipen = 999)

## read data 
df = readr::read_csv("Main Job Categories.csv") %>% 
  dplyr::select(-link) %>% 
  dplyr::arrange_at("totals", desc) %>% 
  dplyr::group_by(category) %>% 
  dplyr::summarise(totals = sum(totals)) %>% 
  dplyr::mutate(percent = totals/sum(totals)) %>% 
  dplyr::arrange_at("totals", desc) %>% 
  dplyr::mutate(#category = gsub(" ", "", category), 
                category = case_when(category == "Sales / Marketing / Retail / Business Development"~"Sales, Marketing and Business Development", 
                                     category == "Finance / Accounting / Audit"~"Audit, Accounting and Finance", 
                                     category == "Administration / Secretarial"~"Administration and Secretarial",
                                     category == "ICT / Computer"~"Information Communication Technology", 
                                     category == "Medical / Healthcare"~"Medical and Healthcare",
                                     category == "Engineering / Technical"~"Engineering and Technical", 
                                     category == "Education / Teaching"~"Education and Teaching", 
                                     category == "NGO/Non-Profit"~"NGO and Non-Profit", 
                                     category == "Media / Advertising / Branding"~"Branding, Media and Advertising", 
                                     category == "Data, Business Analysis and AI"~"Data, Artificial Intelligence and Business Analysis", 
                                     category == "Human Resources / HR"~"Human Resource",
                                     category == "Internships / Volunteering"~"Internships", 
                                     category == "Procurement / Store-keeping / Supply Chain"~"Procurement and Supply Chain", 
                                     category == "Law / Legal"~"Law", 
                                     category == "Agriculture / Agro-Allied"~"Agriculture", 
                                     category == "Hospitality / Hotel / Restaurant"~"Hospitality", 
                                     category == "Catering / Confectionery"~"Confectionery and Catering", 
                                     category == "Security / Intelligence"~"Intelligence and Security", 
                                     TRUE~category))  %>% 
  head(25)

data = df %>% 
  dplyr::filter(category == "Data, Artificial Intelligence and Business Analysis")

#plot the data 

df %>% 
  ggplot() +
  geom_hline(aes(yintercept = y), data.frame(y = c(0:3) * 5000),color = "grey90") +
  geom_col(aes(x = reorder(str_wrap(category, 5), totals),y = totals),
    position = "dodge2",show.legend = TRUE,alpha = .9, fill = highlight_color ) +
  geom_col(data = data, aes(x = reorder(str_wrap(category, 5), totals),y = totals),
           position = "dodge2",show.legend = TRUE,alpha = .9, fill = highlight_color4) +
  # Lollipop shaft for mean gain per region
  geom_segment(aes(x = reorder(str_wrap(category, 5), totals),
                   y = 0, xend = reorder(str_wrap(category, 5), totals),yend = 17000),
    linetype = "dashed",color = "#af8458")+
  # Make it circular!
  coord_polar()+
  labs(title = "\nTop Job Categories Advertised in Kenya",
       subtitle = paste0(subtitle2),
       caption = caption_label)+
  annotate(x = 1.8, 
           y = 10500,
           label = "Total Number of Jobs Posted",
           geom = "text",
           angle = 70,
           color = highlight_color2,
           size = 3.5,
           family = font_family2) +
  annotate(x = 2.6,
           y = 5700,
           label = "5,000",
           geom = "text",
           size = 3,
           color = highlight_color2,
           family = font_family2) +
  annotate(x = 2.4,
           y = 10500,
           label = "10,000",
           geom = "text",
           size = 3,
           color = highlight_color2,
           family = font_family2) +
  annotate(x = 2.3,
           y =15500,
           label = "15,000",
           geom = "text",
           size = 3,
           color = highlight_color2,
           family = font_family2) +
  theme(# Remove axis ticks and text
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    axis.text.y = element_blank(),
    axis.text.x = element_text(color = highlight_color, family = font_family2, size = 10),
    plot.background = element_rect(fill = "#fafafa", colour = "#fafafa"),
    plot.title = element_text(family = font_family2, hjust = 0.5, size = 16, face = 'bold', color = highlight_color2),
    # plot.subtitle = element_markdown(family = 'raleway',face = 'plain',  
    #                                  # margin = margin(b = 0.3, unit = "cm"),
    #                                  hjust = 0.5,
    #                                  size = 8, 
    #                                  lineheight = unit(1.25, "cm"), 
    #                                  color = highlight_color),
    plot.subtitle = element_text(family = font_family2,face = 'plain',  size = 11, color = highlight_color2),
    panel.background = element_rect(fill = "#fafafa", colour = "#fafafa"),
    plot.caption = element_text(size = 9, face = 'italic', color = 'grey30', family = font_family2))

