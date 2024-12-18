;; Task Tracker Smart Contract

(define-map tasks
  { id: uint }
  {
    title: (string-utf8 100),
    description: (optional (string-utf8 500)),
    deadline: uint,
    completed: bool,
    creator: principal
  }
)
;; Store the next task ID
(define-data-var next-task-id uint u0)

;; Store the list of task IDs
(define-data-var task-ids (list 2000 uint) (list))
(define-constant MAX-TASK-IDS 1000)

;; Create a new task
(define-public (create-task 
  (title (string-utf8 100))
  (description (optional (string-utf8 500)))
  (deadline uint)
)
  (let 
    (
      (task-id (var-get next-task-id))
      (current-tasks (var-get task-ids))
    )
    ;; Check if we've reached the list capacity
    (asserts! (< (len current-tasks) u1000) (err u500))
    
    ;; Increment the task ID for the next task
    (var-set next-task-id (+ task-id u1))
    
    ;; Create the task in the map
    (map-set tasks 
      { id: task-id }
      {
        title: title,
        description: description,
        deadline: deadline,
        completed: false,
        creator: tx-sender
      }
    )
    
    ;; Append the new task ID to the list
    (var-set task-ids (unwrap! (as-max-len? (append current-tasks task-id) u1000) (err u500)))
    
    ;; Return the task ID
    (ok task-id)
  )
)

;; Mark a task as completed
(define-public (complete-task (task-id uint))
  (let 
    (
      (task (unwrap! (map-get? tasks { id: task-id }) (err u404)))
    )
    ;; Ensure only the task creator can mark it as completed
    (asserts! (is-eq tx-sender (get creator task)) (err u403))
    
    ;; Update the task's completed status
    (map-set tasks 
      { id: task-id }
      (merge task { completed: true })
    )
    
    (ok true)
  )
)

;; Get task details
(define-read-only (get-task (task-id uint))
  (map-get? tasks { id: task-id })
)

;; ;; List all tasks created by the caller
;; (define-read-only (get-user-tasks)
;;   (filter is-user-task (map-keys tasks))
;; )

(define-read-only (get-user-tasks)
  (filter is-user-task (var-get task-ids))
)

;; Helper function to filter tasks by the current user
(define-private (is-user-task (task-id uint))
  (match (map-get? tasks { id: task-id })
    task (is-eq tx-sender (get creator task))
    false
  )
)