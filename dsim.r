require(data.table)

dsimdbg <- 1
dprint <- function(x) {
  if (exists("dsimdbg")) {
    if(dsimdbg == 1) {
      print(x)
    }
  }
}


model <- setRefClass(
  'model',
  fields=c('clock', 'event.list', 'obj.list', 'obj.num'),
  methods = list(
    initialize = function(clock, event.list, event.log) {
      clock <<- 0
      event.list <<- data.frame()
      obj.list <<- list()
      obj.num <<- 0
    },
    add.obj = function(obj) {
      dprint("class:model, method:add.obj")
      obj$parent <- .self
      obj.list <<- c(obj.list, obj)
      obj.num <<- obj.num + 1
      obj$id <- obj.num
      return(obj)
    }
  )
)

obj <- setRefClass(
  'obj',
  fields <- c("id", "parent", "prev.obj", "next.obj", "items"),
  methods = list(
    initialized = function() {
      parent <<- list(parent)
      prev.obj <<- NA
      next.obj <<- NA
      items <<- data.frame()      
    },
    run = function() {      
    },
    pass = function() {
      return()
      
    }
  )
)



mod <- model$new()
obj1 <- mod$add.obj(obj$new())
obj2 <- mod$add.obj(obj$new())
obj3 <- mod$add.obj(obj$new())
obj4 <- mod$add.obj(obj$new())

