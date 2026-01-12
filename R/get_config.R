#' @export
get_config <- function() {
  # chemin relatif depuis le projet package
  yaml::read_yaml(file.path("data-raw", "config.yaml"))
}
