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
  fields=c('clock', 'event.list', 'event.log', 'objects'),
  methods = list(
    initialize = function(clock, event.list, event.log) {
      clock <<- 0
      event.list <<- data.table()
      event.log <<- data.table()
      objects <<- list()
    }    
  )
)

obj <- setRefClass(
  'obj',
  #general methods: run...
)

obj.queue <- setRefClass(
  'obj.queue',
  contains = 'obj',
  fields = c("size", "wait.fun", "next", "prev", "items"),
  methods = list(
    initialize = function(size=0, wait.fun=function(){NA}) {
      dprint(paste(size, wait.fun()))
      size <<- size
      wait.fun <<- wait.fun
    }
  )
)

queue <- obj.queue$new()
