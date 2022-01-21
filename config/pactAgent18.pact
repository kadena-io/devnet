(module pactAgent18 GOVERNANCE

  (use fungible-util)
  (use util.guards)

  (defconst ROW_KEY_SEPARATOR:string "/")
  (defconst MILLION:decimal 1000000.0)
  (defconst CRANKK01_BANK:string 'crankk01-bank)

  (defcap GOVERNANCE ()
  (with-read paAdmin18 "admin"
    { "aguard" := aguard }
    (enforce-guard aguard)
  ))

  (defcap INTERNAL ()
    "mark some functions as internal only"
    true
  )

  (defun init (aguard:guard)
    "Create Bank and set admin...."
    (insert paAdmin18 "admin" {
      "admin": (at "sender" (chain-data)),
      "aguard": aguard
      } )
  )

  (defschema admin
    admin:string
    aguard:guard
  )

  (deftable paAdmin18:{admin})

  (defschema node
     address: string
     lastAction: time
  )    

  (deftable nodes18:{node})

  (defun reactivate-node ()
    (with-capability (INTERNAL)
       (with-default-read nodes18 (at "sender" (chain-data)) 
           {"address": "", "lastAction": (get-time) } 
           {"address":= readAddress, "lastAction":= readLastAction}
           (if (> (length readAddress) 0)
              (update nodes18 readAddress {"lastAction": (get-time)})
              (insert nodes18 (at "sender" (chain-data)) {"address": (at "sender" (chain-data)), "lastAction": (get-time)})
           )
          (if (> (get-time) (add-time readLastAction 540))
              (award-reactivation) 
              ""    
          )    
       )
    )
  )

  (defun award-reactivation ()
    (require-capability (INTERNAL))
    (install-capability (free.crankk01.TRANSFER CRANKK01_BANK (at "sender" (chain-data)) 0.001))
    (free.crankk01.transfer CRANKK01_BANK (at "sender" (chain-data)) 0.001)
  )    

  (defschema crankstats
    day:string
    count:integer
  )

  (deftable crankstats18:{crankstats})

  (defun update-daily-cranks ()
    (let
      ((day (format-time "%F" (get-time))))

      (with-default-read crankstats18 day
        {"day": "", "count": 0}
        {"day":= readDay, "count":= count}
        (if (> (length readDay) 0)
           (update crankstats18 day {"count": (+ count 1)})
           (insert crankstats18 day {"day": day, "count": (+ count 1)})
        )          
      )
    )
  )

  (defun get-todays-cranks ()
    (let
      ((day (format-time "%F" (get-time))))

      (with-default-read crankstats18 day
        {"count": 0}
        {"count":= count}
        (format "{}" [count])
      )
    )
  )

  (defschema nodestats
    day:string
    count:integer
  )

  (deftable nodestats18:{nodestats})

  (defun update-daily-nodes ()
    (let*
      ((day (format-time "%F" (get-time)))
       (start (time (+ day "T00:00:00Z")))
       (nodes (select nodes18 (where 'lastAction (<= start))))
       (count (length nodes))
      )

      (with-default-read nodestats18 day
        {"day": ""}
        {"day":= readDay}
        (if (> (length readDay) 0)
           (update nodestats18 day {"count": count})
           (insert nodestats18 day {"day": day, "count": count})
        )          
      )
    )
  )

  (defun substring (str:string from:integer to:integer)
    (let*
      ((strlist (str-to-list str))
       (res (fold (substr) {"i":0, "ss":"", "from":from, "to":to} strlist))
      )

      (at "ss" res)    
    )
  )

  (defschema substrcarry
     i: integer
     ss: string
     from: integer
     to: integer
  )   

  (defun substr:object{substrcarry} (sf:object{substrcarry} char:string)
    (bind sf {
      "i":= i,
      "ss":= ss,
      "from":= from,
      "to":= to
      }
      (let 
        ((i (+ i 1)))

          (if (and (>= i from) (<= i to))
            {"i":i, "ss":(+ ss char), "from": from, "to": to}
            {"i":i, "ss":ss, "from": from, "to": to}
          )
      )
    )
  )

  (defschema modules
    pactModule:string
    sender:string
    sguard:guard
    crank:bool
    frequency:integer
    net: time
    executor: string
    createdAt:time
  )

  (defun register-module (pactModule frequency)
    (insert modules18 pactModule {
      "pactModule": pactModule,
      "frequency": frequency,
      "sender": (at "sender" (chain-data)),
      "sguard": (read-keyset "keyset"),
      "crank": true,
      "net": (get-time),
      "executor": "",
      "createdAt": (get-time)}
    )
  )

  (defun set-module-next (pactModule)
    (with-read modules18 pactModule {
        "frequency":= frequency} 
        (update modules18 pactModule {
          "net": (add-time (get-time) frequency),
          "executor": (select-executor)}    
        )
        (update-daily-cranks)
    )
  )

  (deftable modules18:{modules})

  (defschema scripts
    pactModule:string
    scriptId:string
    sender:string
    sguard:guard
    frequency:integer
    minExecutors:integer
    maxExecutors:integer
    percent:integer
    function:string
    input:string
    createdAt:time
  )

  (deftable scripts18:{scripts})

  (defschema tasks
    taskId:string
    scriptId:string
    sender:string
    sguard:guard
    input:string
    resultConsensus:string
    executions:integer
    active:bool
    net:time
    runCount:integer
    maxRuns:integer
    stopResult:string
    createdAt:time
  )

  (deftable tasks18:{tasks})

  (defschema taskResult
    executionId:string
    result:string
  )

  (defschema resultCount
    result:string
    count:integer
  )

  (defschema executions
    executionId:string
    scriptId:string
    taskId:string
    executor:string
    result:string
    createdAt:time
  )

  (deftable executions18:{executions})

  (defun register-crank() 
    (register-module "free.pactAgent18" 600)
  )

  (defun crank()
    (update-daily-nodes)
    (set-module-next "free.pactAgent18")
  )

  (defun get-time:time ()
    "Get block time"
    (at "block-time" (chain-data))
  )

  (defun get-formattedTime:string (t:time)
    "Get formatted time"
    (format-time "%Y-%m-%dT%H:%M:%S" t)
  )

  (defun create-script (pactModule scriptId frequency minExecutors maxExecutors percent function input)
    "Create a script"
    (insert scripts18 (compound-key2 pactModule scriptId)
            { "pactModule" : pactModule,
              "scriptId": scriptId,
              "sender": (at "sender" (chain-data)),
              "sguard": (read-keyset "keyset"),
              "frequency": frequency,
              "minExecutors": minExecutors,
              "maxExecutors": maxExecutors,
              "percent": percent,
              "function": function,
              "input": input,
              "createdAt": (get-time)
            }
    )
  )

  (defun update-script (pactModule scriptId input)
    "Update a script"
    ;Only the script creator can change it
    (with-read scripts18 (compound-key2 pactModule scriptId)
      { "sguard":= sguard }
    ;   (enforce-guard sguard)
        (update scripts18 (compound-key2 pactModule scriptId)
                { 
                  "input": input
                }
        )
    )
  )

  (defun create-task (pactModule scriptId taskId input maxRuns stopResult)
    "Create a task"
    ;Only the script creator can create a task for it - is that good?
    (with-read scripts18 (compound-key2 pactModule scriptId) 
      { "sguard":= sguard }
    ;   (enforce-guard sguard)
        (insert tasks18 (compound-key4 pactModule scriptId taskId "1")
                { 
                  "scriptId": scriptId,
                  "taskId": taskId,
                  "sender": (at "sender" (chain-data)),
                  "sguard": (read-keyset "keyset"),
                  "executions": 0, 
                  "resultConsensus": "",
                  "input": input,
                  "active": true,
                  "runCount": 1,
                  "maxRuns" : maxRuns,
                  "stopResult": stopResult,
                  "net": (get-time),
                  "createdAt": (get-time)
                }
        )
    )
  )

  (defun create-execution (pactModule:string scriptId:string taskId:string runCount:string result:string)
    "Create an execution"
    ;Balance check of executor will come here 
    (with-read tasks18 (compound-key4 pactModule scriptId taskId runCount)
        {"net":= net,
         "stopResult":= stopResult,
         "active":= active}
        (enforce (= active true) "Only active task can be executed")
        (enforce (>= (get-time) net) "Not earlier than must be enforced")
        ; change key later on to allow one execution per task per executor
        (insert executions18 (compound-key5 pactModule scriptId taskId runCount (at "sender" (chain-data)))
                { "executionId": (compound-key4 pactModule scriptId taskId runCount),
                  "taskId": taskId,
                  "scriptId": scriptId,
                  "executor": (at "sender" (chain-data)),
                  "result": result,
                  "createdAt": (get-time)
                }
        )
        (with-read scripts18 (compound-key2 pactModule scriptId)  
          {"maxExecutors":= maxExecutors,
           "frequency":= frequency}
            (let*
              (
                ;all executions for the same task
                (execId (compound-key4 pactModule scriptId taskId runCount))
                (all (select executions18 ['executionId] 
                  (where 'executionId (= execId))))
                (executions (length all))
                ; deactivate if enough executions
                (active (if (>= executions maxExecutors) false true))
                ; copy result if enough executions
                (resultConsensus (if (>= executions maxExecutors) (get-topResult execId) ""))
              )     
    
                ;Increment executions, deactivate if needed, update consensus result
              (update tasks18 (compound-key4 pactModule scriptId taskId runCount)
                {
                    "executions": executions,
                    "active": active,
                    "resultConsensus": resultConsensus
                }
              )
    
              ;If deactivated, a new one can be created
               (with-read tasks18 (compound-key4 pactModule scriptId taskId runCount)
                {"sender":= sender,
                 "sguard":= sguard,
                 "runCount":= runCount,
                 "maxRuns" := maxRuns,
                 "input":= input}
                ;Being deactivated; runCount allows, result not satisfied 
                (if (= active false)
                (if (!= stopResult resultConsensus)
                (if (or (< runCount maxRuns) (= 0 maxRuns))
                   ;insert task, decrement runCount, add frequency 
                    (insert tasks18 (compound-key4 pactModule scriptId taskId (int-to-str 10 (+ runCount 1)))
                        { "taskId": taskId,
                          "scriptId": scriptId,
                          "sender": sender,
                          "sguard": sguard,
                          "executions": 0, 
                          "resultConsensus": "",
                          "input": input,
                          "active": true,
                          "runCount": (+ runCount 1),
                          "maxRuns": maxRuns,
                          "stopResult": stopResult,
                          "net": (add-time (get-time) frequency),
                          "createdAt": (get-time)
                        }
                    )
                "" )
                "" )
                "" )
              )
            )
        )
    )
  )        

  (defun get-scripts ()
    "Get all scripts"
    (select scripts18 (constantly true))
  )

  (defun get-tasks ()
    "Get all tasks"
    (select tasks18 (constantly true))
  )

  (defun get-result (taskId)
    "Get consensus result for a tasks"
    (let
      (
        (finished (reverse (sort ['createdAt]
            (select tasks18 ['resultConsensus 'scriptId 'net 'createdAt] 
                (and?
                (where 'taskId (= taskId))
                (where 'active (= false))
                )
            )
        )))
      )
      ;Return latest consensus result    
      (if (> (length finished) 0) (at "resultConsensus" (at 0 finished)) "")
    )
  )

  (defun get-executions ()
    "Get all executions"
    (select executions18 (constantly true))
  )

  (defun get-nodes ()
    "Get all nodes"
    (select nodes18 (constantly true))
  )

  (defun get-nodestats ()
    "Get all nodestats"
    (select nodestats18 (constantly true))
  )

  (defun get-crankstats ()
    "Get all crankstats"
    (select crankstats18 (constantly true))
  )

  (defun select-executor ()
    "Select executor"
    (let*
      ((activeNodes (get-activeNodes))
      (activeNodeCount (length activeNodes)) 
      (randomIndex (ceiling (* (/ (str-to-int (format-time "%v" (get-time))) MILLION) activeNodeCount))))
      
      (if (> activeNodeCount 0)
         (at "address" (at (- randomIndex 1) activeNodes))
         ""
      )
    )
  )

  (defschema mapped-node
     index:integer
     address: string
  )    

  (defun select-executors (count:integer)
    "Select 'count' number of executors"
    (let*
      ((activeNodes (get-activeNodesAddress))
      (activeNodeCount (length activeNodes)) 
      (randomIndex (ceiling (* (/ (str-to-int (format-time "%v" (get-time))) MILLION) activeNodeCount)))
      (tail (take (- randomIndex activeNodeCount) activeNodes)))

      (if (< count (length tail))
        (take count tail)
        (if (> count (length tail)) 
          tail
          tail
        )  
      )
    ;   (fold (map-node) [{"index":randomIndex, "address":""}] activeNodes)
    )
  )

;   (defun map-node:[object:{mapped-node}] (sf:[object:{mapped-node}] node:object{node})
;     (bind mapped-node { 
;       "index" := index }
;     (bind node { 
;       "address" := address }
;       (+ sf [{"index":0, "address":address}]) 
;     )
;     )
;   )


  (defun get-activeNodes ()
    "Get active nodes"
    (select nodes18 (where 'lastAction (<= (add-time (get-time) -900))))
  )

  (defun get-activeNodesAddress ()
    "Get active nodes"
    (select nodes18 ['address] (where 'lastAction (<= (add-time (get-time) -900))))
  )

  (defun get-taskExecutions (taskId)
    "Get all executions for a task"
    (select executions18 (where 'taskId (= taskId)))
  )

  (defun count-items:object{resultCount} (sf:object{resultCount} taskResult:object{taskResult})
    "Count items"
    (bind sf { 
       "result" := oldResult,
       "count" := count }
    (bind taskResult { 
       "executionId" := executionId,
       "result" := result }
       (let*
         (     
           (results (select executions18
             (and?
             (where 'executionId (= executionId))
             (where 'result (= result))
             )
           ))
           (records (length results))
         )
         (if (> records count) {"result":result, "count":records} sf)
       )
    )
    )
  )

  (defun get-topResult (execId)
    "Get top result for a task"
    (let*
      (
        (all (select executions18 ['executionId 'result] (where 'executionId (= execId))))
        (dist (distinct all))   
        (res (fold (count-items) {"result":"", "count":0} dist))
      )     
      (at "result" res)
    )
  )

  (defun get-activeTasks ()
    "Get active tasks"
    (select tasks18 (where 'active (= true)))
  )

  (defun get-activeModules ()
    "Get active modules"
    (select modules18 (where 'crank (= true)))
  )

  (defun get-cranks ()
    "Get cranks"
    (let*
        ((all (select modules18 
          (and?
          (where 'crank (= true))
          (where 'net (>= (get-time)))))
         )
        (mine (filter (where 'executor (= (at "sender" (chain-data)))) all)))

        (if (> (length mine) 0) mine all)
    )
  )

  (defun deactivate-activeTasks ()
    "Deactivate active tasks"
      (fold (deactivate-item) "" (keys tasks18))
  )

  (defun deactivate-item (sf key)
    (update tasks18 key
      {"active": false} 
    )  
  )

  (defun get-taskKeys ()
    (keys tasks18)
  )

  (defun get-scriptKeys ()
    (keys scripts18)
  )

  (defun update-finishedTasks ()
    "Update finished tasks"
    (select tasks18 (where 'active (= true)))
  )

  (defun read-admin ()
    "Read admin guard"
    (with-read paAdmin18 "admin"
      { "admin":= admin,
      "aguard":= aguard }
    (format "Admin guard is {}, admin {}" [aguard admin]))
  )

  (defun test-admin ()
    "Test admin guard"
  (with-read paAdmin18 "admin"
    { "aguard" := aguard }
    (enforce-guard aguard)
      (format "Admin guard success! {}" [aguard] )
    )
  )

  (defun compound-key2:string
    ( part1:string
      part2:string )
    (format "{}{}{}" [part1 ROW_KEY_SEPARATOR part2])
  )

  (defun compound-key3:string
    ( part1:string
      part2:string
      part3:string )
    (format "{}{}{}{}{}" [part1 ROW_KEY_SEPARATOR part2 ROW_KEY_SEPARATOR part3])
  )

  (defun compound-key4:string
    ( part1:string
      part2:string
      part3:string
      part4:string )
    (format "{}{}{}{}{}{}{}" [part1 ROW_KEY_SEPARATOR part2 ROW_KEY_SEPARATOR part3 ROW_KEY_SEPARATOR part4])
  )

  (defun compound-key5:string
    ( part1:string
      part2:string
      part3:string
      part4:string 
      part5:string )
    (format "{}{}{}{}{}{}{}{}{}" [part1 ROW_KEY_SEPARATOR part2 ROW_KEY_SEPARATOR part3 ROW_KEY_SEPARATOR part4 ROW_KEY_SEPARATOR part5])
  )

  (defun show-msg ()
    (read-msg)
  )
  
  (defun init-bank ()
    (free.crankk01.create-account CRANKK01_BANK (create-module-guard "CRANKK01"))
  )

)
; create-table
(create-table paAdmin18)
(create-table modules18)
(create-table scripts18)
(create-table tasks18)
(create-table nodes18)
(create-table executions18)
(create-table nodestats18)

