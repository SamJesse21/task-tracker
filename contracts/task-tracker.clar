;; Task Tracker Smart Contract

;; Update task structure
(define-map tasks
  { id: uint }
  {
    title: (string-utf8 100),
    description: (optional (string-utf8 500)),
    deadline: uint,
    completed: bool,
    creator: principal,
    priority: uint
  }
)
;; Store the next task ID
(define-data-var next-task-id uint u0)

;; Store the list of task IDs
(define-data-var task-ids (list 2000 uint) (list))
(define-constant MAX-TASK-IDS 1000)

;; Update create-task function
(define-public (create-task 
  (title (string-utf8 100))
  (description (optional (string-utf8 500)))
  (deadline uint)
  (priority uint)
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
        creator: tx-sender,
        priority: priority
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


(define-map comments
  { task-id: uint, comment-id: uint }
  {
    commenter: principal,
    comment: (string-utf8 500)
  }
)

(define-data-var next-comment-id uint u0)

(define-public (add-comment (task-id uint) (comment (string-utf8 500)))
  (let 
    (
      (comment-id (var-get next-comment-id))
    )
    ;; Increment the comment ID for the next comment
    (var-set next-comment-id (+ comment-id u1))
    
    ;; Add the comment to the map
    (map-set comments 
      { task-id: task-id, comment-id: comment-id }
      {
        commenter: tx-sender,
        comment: comment
      }
    )
    
    (ok comment-id)
  )
)



(define-public (extend-deadline (task-id uint) (new-deadline uint))
  (let 
    (
      (task (unwrap! (map-get? tasks { id: task-id }) (err u404)))
    )
    ;; Ensure only the task creator can extend the deadline
    (asserts! (is-eq tx-sender (get creator task)) (err u403))
    
    ;; Update the task's deadline
    (map-set tasks 
      { id: task-id }
      (merge task { deadline: new-deadline })
    )
    
    (ok true)
  )
)


;; Add assignee field to tasks map
(define-map task-assignments
  { task-id: uint }
  { assignee: principal }
)

(define-public (assign-task (task-id uint) (assignee principal))
  (let ((task (unwrap! (map-get? tasks { id: task-id }) (err u404))))
    (asserts! (is-eq tx-sender (get creator task)) (err u403))
    (ok (map-set task-assignments { task-id: task-id } { assignee: assignee }))
  )
)


(define-map task-progress
  { task-id: uint }
  { 
    percentage: uint,
    last-updated: uint
  }
)

(define-public (update-progress (task-id uint) (percentage uint))
  (let ((task (unwrap! (map-get? tasks { id: task-id }) (err u404))))
    (asserts! (<= percentage u100) (err u401))
    (ok (map-set task-progress 
        { task-id: task-id }
        { 
          percentage: percentage,
          last-updated: block-height
        }))
  )
)


(define-map task-dependencies
  { task-id: uint }
  { dependent-on: (list 10 uint) }
)

(define-public (add-dependency (task-id uint) (dependency-id uint))
  (let (
    (current-deps (default-to (list) (get dependent-on (map-get? task-dependencies { task-id: task-id }))))
  )
    (ok (map-set task-dependencies 
        { task-id: task-id }
        { dependent-on: (unwrap! (as-max-len? (append current-deps dependency-id) u10) (err u500)) }))
  )
)



(define-map task-reminders
  { task-id: uint }
  { 
    reminder-time: uint,
    reminder-set: bool
  }
)

(define-public (set-reminder (task-id uint) (reminder-time uint))
  (ok (map-set task-reminders 
      { task-id: task-id }
      { 
        reminder-time: reminder-time,
        reminder-set: true
      }))
)


(define-map priority-history
  { task-id: uint, update-id: uint }
  {
    old-priority: uint,
    new-priority: uint,
    update-time: uint
  }
)

(define-data-var next-priority-update-id uint u0)

(define-public (update-priority (task-id uint) (new-priority uint))
  (let (
    (task (unwrap! (map-get? tasks { id: task-id }) (err u404)))
    (update-id (var-get next-priority-update-id))
  )
    (var-set next-priority-update-id (+ update-id u1))
    (map-set priority-history
      { task-id: task-id, update-id: update-id }
      {
        old-priority: (get priority task),
        new-priority: new-priority,
        update-time: block-height
      }
    )
    (ok (map-set tasks { id: task-id } (merge task { priority: new-priority })))
  )
)


(define-map task-tags
  { task-id: uint }
  { tags: (list 5 (string-utf8 20)) }
)

(define-public (add-tag (task-id uint) (tag (string-utf8 20)))
  (let (
    (current-tags (default-to (list) (get tags (map-get? task-tags { task-id: task-id }))))
  )
    (ok (map-set task-tags
      { task-id: task-id }
      { tags: (unwrap! (as-max-len? (append current-tags tag) u5) (err u500)) }))
  )
)
