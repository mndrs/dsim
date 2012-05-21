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
  fields=c('clock', 'event.list', 'event.num', 'obj.list', 'obj.num', 'run.length', 'item.list', 'item.num'),
  methods = list(
    initialize = function(clock, event.list, event.log) {
      clock <<- 0
      event.list <<- data.frame()
      obj.list <<- list()
      obj.num <<- 0
      event.list <<- data.frame(event_id=-1, obj_id=-1, time=0, status=-1)
      event.num <<- 0
      item.list <<- data.frame()
      item.num <<- 0
      
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
    add.item = function(orig) {
      dprint("class:model, method:add.item")
      item.num <<- item.num + 1
      item.list <<- rbind(item.list, data.frame(id=item.num, enter=clock, exit=NA, orig.blk=orig,exit.blk=NA))

      return(item.num)      
    },
    connect = function(obja, objb) {
      if (class(obja$next.obj) != "list") {
        obja$next.obj <- list(objb)  
      } else {
        obja$next.obj <- list(obja$next.obj, objb)
      }
      
      if (class(objb$prev.obj) != "list") {
        objb$prev.obj <- list(obja)
      } else {
        objb$prev.obj <- list(objb$prev.obj, obja)
      }
    },
    run = function() {
      dprint("class:model, method:run")
      if (clock == 0) {
        dprint("class:model, method:run, Initializing")
        for (i in obj.list) {
          dprint(paste("Object name:", i$name))
          i$init()
        }
      }
      
      while (clock < run.length) {
        print(paste("Event clock:", clock))
        el <- event.list[event.list$status == 0 & event.list$time <= clock,]$event_id
        if (length(el) > 0) {
          for (j in el) {
            i <- event.list[event.list$event_id == j,]
            #dprint(paste(j, i$obj_id))
            #dprint(event.list)
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
  fields <- c("id", "parent", "prev.obj", "next.obj", "item.list", "item.num", "name", "event.list", "event.num"),
  methods = list(
    initialize = function(name="Name not assigned") {
      parent <<- list(parent)
      item.list <<- data.frame()
      item.num <<- 0
      event.list <<- data.frame()
      event.num <<- 0
      name <<- name
    },
    init = function() {
      dprint("obj/init")
    },
    run = function() {      
    },
    pass = function() {
      return()
    },
    add.event = function(event.type, event.time) {
      if (event.time < parent$run.length) {
        parent$add.event(id, event.time)
        event.num <<- event.num + 1
        event.list <<- rbind(event.list, data.frame(event.id=event.num, event.type=event.type, event.time=event.time))
      }
    },
    req.item = function(view=FALSE) {
      for(tmp.obj in prev.obj) {
        dprint(paste("Previous object:", tmp.obj$name))
        
        item <- tmp.obj$item.list[order(tmp.obj$item.list$enter),][1,]
        if (view != TRUE) {
          tmp.obj$item.list <- tmp.obj$item.list[!(tmp.obj$item.list$global.id == item$global.id),]
        }
      
        # Return first matching item...
        return(item)
      }
      
    }
  )
)

create_obj <- setRefClass(
  'create_obj',
  fields <- c("create.fun", "name"),
  contains = "obj",
  methods = list(
    initialize = function(name="Name not assigned: create_obj") {
      create.fun <<- NA
      name <<- name
      event.num <<- 0
      event.list <<- data.frame()
      item.list <<- data.frame()
      item.num <<- 0
    },
    init = function() {
      dprint("create_obj/init")
      event.time <- parent$clock + create.fun()
      add.event("create", event.time)
    },
    run = function() {
      dprint("create_obj/run")
      run.event.id <- event.list[event.list$event.time == parent$clock,]$event.id
      if (length(run.event.id) > 0) {
        for (i in run.event.id) {
          run.event <- event.list[event.list$event.id == i,]
          if (run.event$event.type == "create") {
            dprint(paste("create_obj/run/event:", i))
            item.id <- parent$add.item(id)
            item.num <<- item.num + 1
            
            item.list <<- rbind(item.list, data.frame(local.id=item.num, global.id=item.id, enter=parent$clock, exit=NA, status=1))
            # Create next event
            event.time <- parent$clock + create.fun()
            add.event("create", event.time)
          }
        }
      }
    },
    pass = function() {
      return()
    },
    add.event = function(event.type, event.time) {
      if (event.time < parent$run.length) {
        parent$add.event(id, event.time)
        event.num <<- event.num + 1
        event.list <<- rbind(event.list, data.frame(event.id=event.num, event.type=event.type, event.time=event.time))
      }
    }
    
  )
)


if (test) {
#test <- function() {

#obj1 <- mod$add.obj(obj$new())
#obj2 <- mod$add.obj(obj$new())
#obj3 <- mod$add.obj(obj$new())
#obj4 <- mod$add.obj(obj$new())

mod <- model$new()
mod$run.length <- 100

obj5 <- mod$add.obj(create_obj$new("CreateA"))
#obj6 <- mod$add.obj(create_obj$new("CreateB"))
obj6 <- mod$add.obj(obj$new("CreateB"))
mod$connect(obj5, obj6)

obj5$create.fun <- function() {5}
#obj6$create.fun <- function() {12}

mod$run()

#obj5$run()
#obj6$run()

print(mod$event.list)
print(obj6$event.list)
}