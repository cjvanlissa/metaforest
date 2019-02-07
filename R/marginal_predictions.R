#' @import data.table
create_marginal_preds <- function (data, vars, n, model, percentile_interval, moderator = NULL, mod_levels = NULL, ...){
  mp <- create_integration_grid_mod(data, vars, n, moderator, mod_levels)
  mp[, ("preds") := predict(object = model, data = .SD)$predictions]
  mp[,names(mp)[-which(names(mp) %in% c(vars, "preds"))]:=NULL]
  #mp[, .SD, .SDcols = c("preds", vars), drop = FALSE]
  #agfun <- function(x){ c(sum(x)/length(x), quantile(x, c((.5 * (1 - percentile_interval)), 1 - (.5 * (1 - percentile_interval))))) }
  #out <- mp[, as.list(unlist(lapply(.SD, agfun))), by = vars]
  if(is.null(percentile_interval)) percentile_interval <- .95
  out <- mp[, as.list(unlist(lapply(.SD, quantile, probs = c(.5, (.5 * (1 - percentile_interval)), 1 - (.5 * (1 - percentile_interval)))))), by = vars]
  setnames(out, (ncol(out)-2):ncol(out), c("preds", "lower", "upper"))
  out
}

#' @importFrom data.table setcolorder
create_integration_grid_mod <- function (data, vars, n, moderator = NULL, mod_levels = NULL) {

  points <- sapply(vars, function(x) {
    seq_unif(data[[x]], length.out = n[1])
  }, simplify = FALSE)
  if(length(moderator) > 0 & !is.null(mod_levels)){
    points[[which(names(points) == moderator)]] <- mod_levels
  }

  points <- data.table(id = 1, expand.grid(points))
  int.points <-
    data.table(id = 1, data[sample(seq_len(nrow(data)), min(n[2], nrow(data))), !colnames(data) %in% vars, drop = FALSE])
  out = merge(int.points,
              points,
              all = TRUE,
              allow.cartesian = TRUE)[,!"id", with = FALSE]
  setcolorder(out, names(data))
  if(!is.null(moderator) & !is.null(mod_levels)){
    if(inherits(data[[moderator]], "factor")){
      out[, (moderator) := factor(out[[moderator]], levels = levels(data[[moderator]]))]
    } else {
      #if(length(names(mod_levels)) > 0){
        out[, (moderator) := factor(out[[moderator]], labels = names(mod_levels))]
      #} else {
      #  out[, (moderator) := factor(out[[moderator]])]
      #}

    }
  }
  out
}

create_integration_grid <- function (data, vars, n) {
  points <- data.table(id = 1, expand.grid(sapply(vars, function(x) {
    seq_unif(data[[x]], length.out = n[1])
  }, simplify = FALSE)))

  int.points <-
    data.table(id = 1, data[sample(seq_len(nrow(data)), min(n[2], nrow(data))), !colnames(data) %in% vars, drop = FALSE])
  out = merge(int.points,
              points,
              all = TRUE,
              allow.cartesian = TRUE)[,!"id", with = FALSE]
  setcolorder(out, names(data))
  out
}

seq_unif <- function(x, length.out){
  UseMethod("seq_unif", x)
}
seq_unif.numeric <- function(x, length.out){
  seq(min(x, na.rm = TRUE), max(x, na.rm = TRUE), length.out = length.out)
}

seq_unif.integer <- function (x, length.out) {
  min.x = min(x, na.rm = TRUE)
  max.x = max(x, na.rm = TRUE)
  x.length = max.x - min.x
  if (length.out > length(unique(x))) {
    sort(x)
  }
  else {
    as.integer(round(seq.int(min.x, max.x, length.out = length.out)),
               0)
  }
}

seq_unif.character <- function(x, length.out){
  x.length = length(unique(x))
  if (length.out < x.length) {
     warning("length.out is less than the number of unique values")
  }
  sample(unique(x), size = min(length.out, x.length))
}

seq_unif.factor <- function(x, length.out) {
  x.length = length(unique(x))
  if (length.out >= x.length) {
    sort(unique(x))
  }
  else {
    if (is.ordered(x)) {
      unique(x)[seq_unif(seq_len(x.length), length.out)]
    }
    else {
      warning("length.out is less than the number of levels")
      sort(sample(unique(x), size = length.out))
    }
  }
}
