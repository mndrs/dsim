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
  fields=c('clock', 'event.list', 'obj.list', 'obj.num', 'run.length'),
  methods = list(
    initialize = function(clock, event.list, event.log) {
      clock <<- 0
      event.list <<- data.frame()
      obj.list <<- list()
      obj.num <<- 0
      event.list <<- data.frame(event_id=-1, obj_id=-1, time=0, status=-1)
      run.length <<- 0
    },
    add.obj = function(obj) {
      dprint("class:model, method:add.obj")
      obj$parent <- .self
      obj.list <<- c(obj.list, obj)
      obj.num <<- obj.num + 1
      obj$id <- obj.num
      return(obj)
    },
    add.event = function(id, time) {
      dprint(paste("class:model, method:add.event", "object/time:", id, time))
      event.list <<- rbind(event.list, data.frame(event_id=max(event.list$event_id) + 1, obj_id=id, time, status=0))
      return(1)
    },
    run = function() {
      dprint("class:model, method:run")
      if (clock == 0) {
        dprint("class:model, method:run, Initializing")
        for (i in obj.list) {
          dprint(paste("Object name:", i$name))
          i$run()
        }
      }
      
      while (clock < run.length) {
        print(paste("Event clock:", clock))
        el <- event.list[event.list$status == 0 & event.list$time <= clock,]$event_id
        if (length(el) > 0) {
          for (j in el) {
            i <- event.list[event.list$event_id == j,]
            dprint(paste(j, i$obj_id))
            dprint(event.list)
            obj.list[[i$obj_id]]$run()
            dprint(paste("Event/object/time/clock:", i$event_id, i$obj_id, i$time, clock))
            event.list[event.list$event_id == i$event_id,]$status <<- 1           
          }
        }
        newclock <- min(event.list[event.list$status == 0,]$time, run.length)
        dprint(paste("Changing clock:", newclock))
        clock <<- newclock
      }
    }
  )
)

obj <- setRefClass(
  'obj',
  fields <- c("id", "parent", "prev.obj", "next.obj", "items", "name"),
  methods = list(
    initialize = function(name="Name not asigned") {
      parent <<- list(parent)
      prev.obj <<- NA
      next.obj <<- NA
      items <<- data.frame()
      name <<- name
    },
    run = function() {      
    },
    pass = function() {
      return()
      
    }
  )
)

create_obj <- setRefClass(
  'create_obj',
  fields <- c("create.fun", "name"),
  contains = "obj",
  methods = list(
    initialize = function(name="Name not assigned: create_obj") {
#      parent <<- list(parent)
#      prev.obj <<- NA
#      next.obj <<- NA
#      items <<- data.frame()
      create.fun <<- NA
      name <<- name
    },
    run = function() {
      parent$add.event(id, parent$clock + create.fun())
    },
    pass = function() {
      return()
      
    }
  )
)


#test <- function() {

#obj1 <- mod$add.obj(obj$new())
#obj2 <- mod$add.obj(obj$new())
#obj3 <- mod$add.obj(obj$new())
#obj4 <- mod$add.obj(obj$new())

mod <- model$new()
mod$run.length <- 100

obj5 <- mod$add.obj(create_obj$new("CreateA"))
obj6 <- mod$add.obj(create_obj$new("CreateB"))

obj5$create.fun <- function() {5}
obj6$create.fun <- function() {12}

mod$run()

#obj5$run()
#obj6$run()

print(mod$event.list)
#}