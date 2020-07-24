# set mapping colour for each outbreak
covid_col = "#cd4652"
covid_other_col = "#000000"
covid_test_col = '#43abb6'
fc_tests <- colorRampPalette(c('#ffffff', '#43abb6', "#602B59"))
fc_cases <- colorRampPalette(c('#ffffff', "#cd4652"))

label_func = function(y) {
    suffixes = c("", "K", "M", "G")

    recurseY = function(val, i = 1) {
        if (!is.na(val) & val >= 1000 & i < length(suffixes)) {
            return(recurseY(val / 1000, i + 1))
        } else {
            return(paste0(val, suffixes[i]))
        }
    }

    return(lapply(y, recurseY))
}

default_theme = function() {
    theme(
        legend.title = element_blank(),
        legend.position = "",
        plot.title = element_text(size=10),
        plot.margin = margin(5, 12, 5, 5)
    )
}

comma = function(number) {
    format(number, big.mark=",")
}

# function to plot cumulative cases by date
cumulative_plot = function(cv_aggregated, plot_date, current_case_count) {
  plot_df = subset(cv_aggregated, date<=plot_date & region %in% c("Global", "Deaths"))
  g1 = ggplot(plot_df, aes(x = date, y = cases, color = region)) + geom_line() + geom_point(size = 1, alpha = 0.8) +
    ylab("Reported numbers\n(cumulative)") + 
    xlab("") +
    theme_bw() + 
    scale_colour_manual(labels = c('Positive tests', 'Deaths'), values=c(covid_col, covid_other_col)) +
    xlim(c(cv_min_date,current_date)) + 
    scale_x_date(date_labels = "%b %Y") +
    scale_y_continuous(trans = 'log', limits=c(10,current_case_count+10000), breaks = c(100, 1000, 1000000),labels = c('0.1K', '1K', '1M')) +
    theme(legend.title = element_blank(), 
          legend.position = c(0.7, 0.25),
          legend.background = element_blank(),
          plot.title = element_text(size=10), 
          plot.margin = margin(5, 12, 5, 5))
  g1
}

# function to plot cumulative tests by date
cumulative_test_plot = function(cv_test_aggregated, plot_date, current_test_count) {
  plot_df = subset(cv_test_aggregated, date<=plot_date & region =="Global")
  g1 = ggplot(plot_df, aes(x = date, y = cum_tests, color = region)) + geom_line() + geom_point(size = 1, alpha = 0.8) +
    ylab("Reported\ntests performed\n(cumulative)") +
    xlab("") +
    theme_bw() + 
    scale_colour_manual(values=c(covid_test_col)) +
    xlim(c(cv_min_date,current_date + 1)) + 
    scale_x_date(date_labels = "%b %Y") +
    scale_y_continuous(trans = 'log', limits=c(10,current_test_count+10000), breaks = c(100, 1000, 1000000), labels = c('0.1K', '1K', '1M')) +
    theme(legend.title = element_blank(), legend.position = "", plot.title = element_text(size=10), 
          plot.margin = margin(5, 12, 5, 5))
  g1
}


# function to plot new cases by date
new_cases_plot = function(cv_aggregated, plot_date) {
    plot_df_new = subset(cv_aggregated, date<=plot_date & region!="Global")

    g1 = ggplot(plot_df_new, aes(x = date, y = new, fill = region)) +
        geom_bar(position="stack", stat="identity") +
        ylab("New cases") + theme_bw() +
        xlab("Date") +
        scale_fill_manual(values=c(covid_col, covid_other_col)) +
        xlim(c(cv_min_date,current_date)) +
        scale_x_date(date_labels = "%b %Y") +
        scale_y_continuous(limits=c(0,23000), labels = label_func) +
        default_theme()

    if (plot_date >= as.Date("2020-02-14")) {
        g1 + annotate(
            "text",
            x = as.Date("2020-02-01"),
            y = 20000,
            label = "new diagnostic criteria",
            size=3
        ) +
        annotate(
            "point",
            x = as.Date("2020-02-14"),
            y = 20000,
            colour = "black",
            size = 3,
            shape = 4
        )
    } else {
        g1
    }
}

# function to plot new tests by date
new_tests_plot = function(cv_test_aggregated, plot_date) {
	plot_df_new = subset(cv_test_aggregated, date<=plot_date & region =="Global")

    g1 = ggplot(plot_df_new, aes(x = date, y = new, fill = region)) +
        geom_bar(position="stack", stat="identity") +
        ylab("New tests") + theme_bw() +
        scale_fill_manual(values=c(covid_test_col)) +
        xlim(c(cv_min_date,current_date+2)) +
        scale_x_date(date_labels = "%b %Y") +
        scale_y_continuous(limits=c(0,70000), labels = label_func) +
        default_theme()

	if (plot_date >= as.Date("2020-02-14")) {
		g1 + annotate(
            "text",
            x = as.Date("2020-02-01"),
            y = 70000,
            label = "new diagnostic criteria",
            size=3
        ) +
		annotate(
            "point",
            x = as.Date("2020-02-14"),
            y = 70000,
            colour = "black",
            size = 3,
            shape = 4
        )
	} else {
        g1
    }
}

bubble_plot = function(data, latest, country_cols) {
	p = ggplot(
        data,
        aes(
            x=casesPer100k,
            y=testsPer100k,
            text=paste(
                "Country: ", country,
                "\nPopulation: ", comma(population),
                "\nTests/100k population: ", comma(testsPer100k),
                "\nCases/100k population: ", comma(casesPer100k),
                "\nDeaths/100k population: ", comma(deathsPer100k)
            )
        )
    ) +
        geom_point(aes(col=country, size=deathsPer100k)) +
        theme_bw() +
        theme(
            #plot.title = element_text(size=10),
            plot.margin = margin(5, 12, 5, 5),
            legend.position="right",
            legend.title = element_text(size = 8),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank()
        ) +
        scale_colour_manual(values=country_cols, name = "Colour: Country", guide="legend") +
        scale_x_continuous(limits=c(0, max(latest$casesPer100k)), labels=scales::comma) +
        scale_y_continuous(limits=c(0, max(latest$testsPer100k)), labels=scales::comma) +
        scale_size_continuous(name="Size: Deaths/100k of population") +
        labs(title="COVID-19 cases vs. tests per 100k of population", x="Cases/100k population", y="Tests/100k population") 
        
    ggplotly(p, tooltip="text")
}

# function to render plotly of depending on selected outcome
comparison_plot = function(epi_comp, comparison) {
    epi_comp$outcome = epi_comp[,comparison]
    epi_comp = epi_comp[order(epi_comp$outcome),]
    epi_comp$outbreak = factor(epi_comp$outbreak, levels=epi_comp$outbreak)

    p1 <- ggplot(
        epi_comp,
        aes(
            x = outbreak,
            y = outcome,
            fill=outbreak,
            text = paste0(outbreak, ": ",comma(outcome))
        )
    ) +
        geom_bar(alpha = 0.8, stat="identity") +
        xlab("") +
        ylab("N") +
        theme_bw() +
        scale_fill_manual(values=c("2019-Covid"=covid_col, "2003-SARS"=sars_col, "2014-Ebola"=ebola_col,"2009-H1N1 (swine flu)"=h1n1_col)) +
        theme(legend.position = "")

  if (comparison == "cfr") {
      p1 = p1 + ylab("%")
  }

  if (comparison == "deaths") {
      p1 = p1 + scale_y_continuous(labels = label_func)
  }

  if (comparison == "cases") {
      p1 = p1 + scale_y_continuous(
          trans='log10',
          limits = c(1,1e8),
          breaks=c(1,1000,1e6,1e9),
          labels = label_func
      )
  }

  ggplotly(p1 + coord_flip(), tooltip = c("text")) %>% layout(showlegend = FALSE)
}

country_cases_plot = function(cv_cases, country_cols) {
    g1 = ggplot(
        cv_cases,
        aes(
            x = date,
            y = new_outcome,
            fill = country,
            text = paste0(format(date, "%d %B %Y"), "\n", country, ": ",comma(new_outcome))
        )
    ) +
        geom_bar(position="stack", stat="identity") +
        ylab(paste0("Number of ", cv_cases$label_name, "  per day")) +
        xlab("Date") +
        theme_bw() +
        scale_fill_manual(values=country_cols) +
        xlim(c(cv_min_date,current_date+1)) +
        scale_x_date(date_labels = "%b %Y") +
        scale_y_continuous(labels=scales::comma) +
        default_theme()

    ggplotly(g1, tooltip = c("text")) %>% layout(legend = list(font = list(size=11)))
}

country_cases_plot_empty = function(text) {

    g1 = ggplot() +
        annotate("text", x = 4, y = 25, size=8, label = text) +
        theme_bw() +
        theme(
            panel.grid.major=element_blank(),
            panel.grid.minor=element_blank(),
            line = element_blank(),
            text = element_blank(),
            title = element_blank()
        )
}




country_cases_cumulative = function(cv_cases, country_cols) {
    g1 = ggplot(
        cv_cases,
        aes(
            x = date,
            y = outcome,
            colour = country,
            group = 1,
            text = paste0(format(date, "%d %B %Y"), "\n", country, ": ",comma(outcome))
        )
    ) +
        geom_line(alpha=0.8) +
        geom_point(size = 1, alpha = 0.8) +
        ylab(paste0("Cumulative number of ", cv_cases$label_name)) +
        xlab("Date") +
        theme_bw() +
        scale_colour_manual(values=country_cols) +
        xlim(c(cv_min_date,current_date+1)) +
        scale_x_date(date_labels = "%b %Y") +
        scale_y_continuous(labels=scales::comma) +
        default_theme()

    ggplotly(g1, tooltip = c("text")) %>% layout(legend = list(font = list(size=11)))
}

get_country_select_options = function(order, countries, cv_today) {
  if (is.null(order)) {
    return()
  }
  
  switch(order,
         "Total Tests"={
           intersect(as.character(cv_today[order(-cv_today$tests_cumulative),]$country),UN_list)
         },
         "Tests/100k"={
           intersect(as.character(cv_today[order(-cv_today$testsPer100k),]$country),UN_list)
         },
         "Total Cases"={
           intersect(as.character(cv_today[order(-cv_today$cases),]$country),UN_list)
         },
         "Cases/100k"={
           intersect(as.character(cv_today[order(-cv_today$casesPer100k),]$country),UN_list)
         },
         "Alphabetical"={
           sort(intersect(as.character(cv_today$country),UN_list))
         },
         "Population"={
           intersect(as.character(cv_today[order(-cv_today$population),]$country),UN_list)
         }
  )
}