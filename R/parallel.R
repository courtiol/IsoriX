## Internal (not exported) functions used to run some computations in parallel.
## They should not be called by the user but are instead called by other IsoriX functions.
##
## Parallel computations rely on socket (PSOCK) clusters from the base package
## parallel since it is the only approach which works identically on Windows,
## macOS and Linux (forking, as used by `parallel::mclapply()`, is not
## available on Windows and is not safe in all situations on macOS).
##
## Workers are fresh R sessions, which implies that:
##  - the IsoriX options and some internal states must be sent to the workers;
##  - objects from terra (SpatRaster and SpatVector) must be packed before being
##    sent to or retrieved from the workers, since they point to data stored
##    outside R;
##  - what workers print and the messages and warnings they produce must be
##    captured and displayed by the main R session.


.check_Ncpu <- function(Ncpu) {
  ## This function should not be called by the user.
  ## It checks the argument `Ncpu` and returns it as an integer.
  valid <- is.numeric(Ncpu) && length(Ncpu) == 1L && !is.na(Ncpu) &&
    Ncpu >= 1 && Ncpu == round(Ncpu)
  if (!valid) {
    stop("The argument `Ncpu` must be a single positive integer (e.g. `Ncpu = 2`).")
  }
  as.integer(Ncpu)
}


.lapply_IsoriX <- function(X, FUN, shared = list(), Ncpu = 1L) {
  ## This function should not be called by the user.
  ## It applies `FUN(x, shared)` to each element `x` of the list `X`, either
  ## sequentially (`Ncpu = 1` or a single element in `X`) or in parallel using
  ## a socket cluster of `min(Ncpu, length(X))` workers.
  ##
  ## Args:
  ##   X: a list
  ##   FUN: a function with two arguments: an element of `X` and `shared`;
  ##        to avoid sending unnecessary objects to the workers, it should be
  ##        a function defined in the IsoriX namespace (not a closure)
  ##   shared: a list of objects needed by all calls to `FUN`, which is only
  ##           sent once to each worker
  ##   Ncpu: the number of CPUs to use
  ##
  ## Returns:
  ##   A list of the same length as `X`
  ##
  Ncpu <- min(.check_Ncpu(Ncpu), length(X))
  if (Ncpu <= 1L) {
    return(lapply(X, FUN, shared))
  }

  cl <- parallel::makePSOCKcluster(Ncpu)
  on.exit(parallel::stopCluster(cl), add = TRUE)
  .setup_workers(cl)
  parallel::clusterCall(cl, .store_shared_in_worker, shared = shared)
  results <- parallel::parLapplyLB(cl, X, .run_in_worker, task_fn = FUN)

  ## display what the workers printed or signalled, and return the values
  lapply(results, function(res) {
    if (length(res$output) > 0) {
      cat(res$output, sep = "\n")
    }
    for (cnd in res$conditions) {
      if (inherits(cnd, "warning")) {
        warning(cnd)
      } else {
        message(cnd)
      }
    }
    if (inherits(res$value, "error")) {
      stop(res$value)
    }
    res$value
  })
}


.setup_workers <- function(cl) {
  ## This function should not be called by the user.
  ## It prepares the workers of the cluster `cl` so that they behave like the
  ## main R session: same libraries, same version of IsoriX (including the one
  ## loaded with `pkgload::load_all()` during development), same options and
  ## same location for the temporary files created by terra (so that these
  ## files are not deleted when the workers stop).
  ##
  ## The functions sent to the workers before IsoriX is loaded in them must not
  ## be defined in the IsoriX namespace, otherwise the workers would try to load
  ## IsoriX when receiving them.
  parallel::clusterCall(cl, base::.libPaths, .libPaths())

  ns <- asNamespace("IsoriX")
  if (exists(".__DEVTOOLS__", envir = ns, inherits = FALSE)) { ## IsoriX loaded with pkgload::load_all()
    load_dev <- function(path) {
      loadNamespace("pkgload")$load_all(path, export_all = FALSE, helpers = FALSE, quiet = TRUE)
      invisible(NULL)
    }
    environment(load_dev) <- globalenv()
    parallel::clusterCall(cl, load_dev, path = getNamespaceInfo(ns, "path"))
  } else {
    parallel::clusterCall(cl, base::loadNamespace, package = "IsoriX")
  }

  parallel::clusterCall(cl, .set_worker_state,
    options = options_IsoriX(),
    default_model_msg_shown = isTRUE(.data_IsoriX$default_model_msg_shown),
    tempdir = terra::terraOptions(print = FALSE)$tempdir
  )
  invisible(NULL)
}


.set_worker_state <- function(options, default_model_msg_shown, tempdir) {
  ## This function should not be called by the user.
  ## It is run within each worker to copy the state of the main R session.
  options_IsoriX(options)
  .data_IsoriX$default_model_msg_shown <- default_model_msg_shown
  terra::terraOptions(tempdir = tempdir, print = FALSE)
  invisible(NULL)
}


.store_shared_in_worker <- function(shared) {
  ## This function should not be called by the user.
  ## It is run within each worker to store the objects needed by all tasks.
  .data_IsoriX$worker_shared <- shared
  invisible(NULL)
}


.run_in_worker <- function(x, task_fn) {
  ## This function should not be called by the user.
  ## It is run within each worker to compute `task_fn(x, shared)` while capturing
  ## what is printed, the messages, the warnings and the errors
  ## (the function is not called `FUN` since it would clash with an argument of parLapplyLB()).
  ## The calls stored in the conditions are dropped since they may contain large
  ## objects (e.g. those created by `do.call()`).
  conditions <- list()
  store_condition <- function(cnd) {
    cnd$call <- NULL
    conditions[[length(conditions) + 1L]] <<- cnd
  }
  output <- utils::capture.output(
    value <- withCallingHandlers(
      tryCatch(task_fn(x, .data_IsoriX$worker_shared),
        error = function(e) {
          e$call <- NULL
          e
        }
      ),
      warning = function(w) {
        store_condition(w)
        invokeRestart("muffleWarning")
      },
      message = function(m) {
        store_condition(m)
        invokeRestart("muffleMessage")
      }
    )
  )
  list(value = value, output = output, conditions = conditions)
}


.pack_terra <- function(x) {
  ## This function should not be called by the user.
  ## It packs a terra object so that it can be sent to another R session.
  if (inherits(x, "SpatRaster") || inherits(x, "SpatVector")) terra::wrap(x) else x
}


.unpack_terra <- function(x) {
  ## This function should not be called by the user.
  ## It unpacks a terra object packed by `.pack_terra()`.
  if (inherits(x, "PackedSpatRaster") || inherits(x, "PackedSpatVector")) terra::unwrap(x) else x
}
