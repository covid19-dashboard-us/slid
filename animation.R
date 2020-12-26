rm(list = ls())

library(devtools)
#install_github('covid19-dashboard-us/slid')
library(slid)
library(ggplot2)
library(plotly)
library(gapminder)

pop.state$State

region.state = pop.state

features.state=read.csv("data/region.csv", header=T)

features.state$pop=pop.state$population

names(features.state)

save(features.state, file = "features.state.rda")

I.new.state = inner_join(I.state, features.state, by = "State")
dim(I.new.state)

I.new.state.long <- gather(I.new.state, DATE, Infected, X2020.12.11:X2020.01.22, factor_key=TRUE)
dim(I.new.state.long)

D.state.long <- gather(D.state, DATE, Death, X2020.12.11:X2020.01.22, factor_key=TRUE)
dim(D.state.long)

state.long = I.new.state.long
state.long$Death = D.state.long$Death
names(state.long)
save(state.long, file = 'data/state.long.rda')

data(state.long)
names(state.long)

state.long$DATE = as.Date(substring(state.long$DATE, 2), "%Y.%m.%d") 
state.long.DEC = state.long[state.long$DATE > as.Date("2020-11-30"), ]

state.long.DEC$log.Infected = log10(state.long.DEC$Infected)
state.long.DEC$log.Death = log10(state.long.DEC$Death)

gg <- ggplot(state.long.DEC, aes(log.Infected, log.Death, color = State)) +
  geom_point(aes(size = population, frame = as.numeric(DATE), ids = State)) +
  scale_x_log10() 
ggplotly(gg)


base <- state.long.DEC %>%
  plot_ly(x = ~log.Infected, y = ~log.Death, size = ~population, 
          text = ~State, hoverinfo = "text") %>%
  layout(xaxis = list(type = "log"))

base %>%
  add_markers(color = ~State, frame = ~DATE, ids = ~State) %>%
  animation_opts(1000, easing = "elastic", redraw = FALSE) %>%
  animation_button(
    x = 1, xanchor = "right", y = 0, yanchor = "bottom"
  ) %>%
  animation_slider(
    currentvalue = list(type = "date", font = list(color="red"))
  )

xaxis <- list(title = "", showline = FALSE, showticklabels = TRUE,
              showgrid = TRUE, type='date', tickformat = '%m/%d', 
              range = c(as.Date('2020-01-20'), date.update + 12))

library(gapminder)
library(ggplot2)
library(plotly)

data(gapminder, package = "gapminder")

gapminder[1,]


# Libraries
library(ggplot2)
library(dplyr)

library(plotly)
df <- data.frame(
  Date = seq(as.Date("2017-01-01"), as.Date("2017-01-05"), by = 1),
  Hour = rep(1:24, 5),
  Value = rnorm(24 * 5)
)
p1 <- ggplot(aes(Hour, Value, frame = Date), data = df) +
  geom_line() # not working
ggplotly(p1)
p2 <- ggplot(aes(Hour, Value, frame = as.numeric(Date)), data = df) +
  geom_line() # working
ggplotly(p2)


gg <- ggplot(state.long.DEC, aes(Infected, Death, color = State)) +
  geom_point(aes(size = population, frame = as.numeric(DATE), ids = State)) +
  scale_x_log10() 
ggplotly(gg)

gg <- ggplot(gapminder, aes(gdpPercap, lifeExp, color = continent)) +
  geom_point(aes(size = pop, frame = year, ids = country)) +
  scale_x_log10()
ggplotly(gg)

as.numeric(state.long$DATE)



meanLife <- with(gapminder, tapply(lifeExp, INDEX = continent, mean))
gapminder$continent <- factor(
  gapminder$continent, levels = names(sort(meanLife))
)

base <- gapminder %>%
  plot_ly(x = ~gdpPercap, y = ~lifeExp, size = ~pop, 
          text = ~country, hoverinfo = "text") %>%
  layout(xaxis = list(type = "log"))

base %>%
  add_markers(color = ~continent, frame = ~year, ids = ~country) %>%
  animation_opts(1000, easing = "elastic", redraw = FALSE) %>%
  animation_button(
    x = 1, xanchor = "right", y = 0, yanchor = "bottom"
  ) %>%
  animation_slider(
    currentvalue = list(prefix = "YEAR ", font = list(color="red"))
  )

base %>%
  add_markers(data = gapminder, frame = ~continent) %>%
  hide_legend() %>%
  animation_opts(frame = 1000, transition = 0, redraw = FALSE)




