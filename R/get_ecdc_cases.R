get_ecdc_cases <- function (countries = NULL)
{
  # Get latest update
  base_url <- "https://www.ecdc.europa.eu/sites/default/files/documents/COVID-19-geographic-disbtribution-worldwide.csv"
  d <- readr::read_csv(base_url) %>%
    dplyr::mutate(date = as.Date(DateRep, format = "%d/%m/%Y")) %>%
    rename(geoid = GeoId, country = `Countries and territories`,
           cases = Cases, death = Deaths,
           population_2018 = 'Pop_Data.2018') %>%
    select(-DateRep) %>%
    dplyr::arrange(date) %>% dplyr::mutate(cases = ifelse(cases <
                                                            0, 0, cases))
  if (!is.null(countries)) {
    d <- d %>% dplyr::filter(country %in% countries)
  }
  return(d)
}
