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



;; Add category mapping
(define-map task-categories
  { category-id: uint }
  { 
    name: (string-utf8 50),
    created-by: principal
  }
)

(define-data-var next-category-id uint u0)

(define-public (create-category (name (string-utf8 50)))
  (let ((category-id (var-get next-category-id)))
    (var-set next-category-id (+ category-id u1))
    (ok (map-set task-categories 
        { category-id: category-id }
        { name: name, created-by: tx-sender }))
  )
)


(define-map priority-labels
  { priority-level: uint }
  { label: (string-utf8 20) }
)

(define-public (set-priority-label (level uint) (label (string-utf8 20)))
  (ok (map-set priority-labels 
      { priority-level: level }
      { label: label }))
)



(define-map shared-tasks
  { task-id: uint, shared-with: principal }
  { can-edit: bool }
)

(define-public (share-task (task-id uint) (user principal) (can-edit bool))
  (let ((task (unwrap! (map-get? tasks { id: task-id }) (err u404))))
    (asserts! (is-eq tx-sender (get creator task)) (err u403))
    (ok (map-set shared-tasks 
        { task-id: task-id, shared-with: user }
        { can-edit: can-edit }))
  )
)



(define-map user-statistics
  { user: principal }
  {
    tasks-completed: uint,
    tasks-created: uint,
    on-time-completion: uint
  }
)

(define-public (update-user-stats (completed bool))
  (let ((current-stats (default-to 
        { tasks-completed: u0, tasks-created: u0, on-time-completion: u0 }
        (map-get? user-statistics { user: tx-sender }))))
    (ok (map-set user-statistics
        { user: tx-sender }
        (merge current-stats 
          { tasks-completed: (+ (get tasks-completed current-stats) u1) })))
  )
)



(define-map task-templates
  { template-id: uint }
  {
    name: (string-utf8 100),
    description: (optional (string-utf8 500)),
    creator: principal,
    default-priority: uint
  }
)

(define-data-var next-template-id uint u0)

(define-public (create-template 
    (name (string-utf8 100))
    (description (optional (string-utf8 500)))
    (default-priority uint))
  (let ((template-id (var-get next-template-id)))
    (var-set next-template-id (+ template-id u1))
    (ok (map-set task-templates
        { template-id: template-id }
        {
          name: name,
          description: description,
          creator: tx-sender,
          default-priority: default-priority
        }))
  )
)



(define-map archived-tasks
  { task-id: uint }
  { 
    archive-date: uint,
    archived-by: principal
  }
)

(define-public (archive-task (task-id uint))
  (let ((task (unwrap! (map-get? tasks { id: task-id }) (err u404))))
    (asserts! (is-eq tx-sender (get creator task)) (err u403))
    (ok (map-set archived-tasks
        { task-id: task-id }
        { 
          archive-date: block-height,
          archived-by: tx-sender
        }))
  )
)



(define-map task-time-logs
  { task-id: uint, log-id: uint }
  {
    start-time: uint,
    end-time: uint,
    logged-by: principal
  }
)

(define-data-var next-log-id uint u0)

(define-public (log-task-time (task-id uint) (start-time uint) (end-time uint))
  (let ((log-id (var-get next-log-id)))
    (var-set next-log-id (+ log-id u1))
    (ok (map-set task-time-logs
        { task-id: task-id, log-id: log-id }
        {
          start-time: start-time,
          end-time: end-time,
          logged-by: tx-sender
        }))
  )
)



(define-map recurring-tasks
  { task-id: uint }
  {
    frequency: (string-utf8 20), ;; daily, weekly, monthly
    last-created: uint,
    active: bool
  }
)

(define-public (set-task-recurrence (task-id uint) (frequency (string-utf8 20)))
  (let ((task (unwrap! (map-get? tasks { id: task-id }) (err u404))))
    (asserts! (is-eq tx-sender (get creator task)) (err u403))
    (ok (map-set recurring-tasks
        { task-id: task-id }
        {
          frequency: frequency,
          last-created: block-height,
          active: true
        }))
  )
)

;; Define status map
(define-map task-status
    { task-id: uint }
    { status: (string-utf8 20) }  ;; "IN_PROGRESS", "BLOCKED", "REVIEW", etc.
)

(define-public (update-task-status (task-id uint) (new-status (string-utf8 20)))
    (let ((task (unwrap! (map-get? tasks { id: task-id }) (err u404))))
        (asserts! (is-eq tx-sender (get creator task)) (err u403))
        (ok (map-set task-status 
            { task-id: task-id }
            { status: new-status }))
    )
)


(define-map subtasks
    { parent-id: uint, subtask-id: uint }
    {
        title: (string-utf8 100),
        completed: bool,
        created-by: principal
    }
)

(define-data-var next-subtask-id uint u0)

(define-public (create-subtask (parent-id uint) (title (string-utf8 100)))
    (let 
        ((subtask-id (var-get next-subtask-id))
         (parent-task (unwrap! (map-get? tasks { id: parent-id }) (err u404))))
        (var-set next-subtask-id (+ subtask-id u1))
        (ok (map-set subtasks
            { parent-id: parent-id, subtask-id: subtask-id }
            {
                title: title,
                completed: false,
                created-by: tx-sender
            }))
    )
)


(define-map task-votes
    { task-id: uint, voter: principal }
    { rating: uint }  ;; 1-5 rating
)

(define-public (vote-on-task (task-id uint) (rating uint))
    (begin
        (asserts! (and (>= rating u1) (<= rating u5)) (err u401))
        (ok (map-set task-votes
            { task-id: task-id, voter: tx-sender }
            { rating: rating })))
)


(define-map task-attachments
    { task-id: uint, attachment-id: uint }
    {
        url: (string-utf8 200),
        description: (string-utf8 100),
        added-by: principal
    }
)

(define-data-var next-attachment-id uint u0)

(define-public (add-attachment (task-id uint) (url (string-utf8 200)) (description (string-utf8 100)))
    (let ((attachment-id (var-get next-attachment-id)))
        (var-set next-attachment-id (+ attachment-id u1))
        (ok (map-set task-attachments
            { task-id: task-id, attachment-id: attachment-id }
            {
                url: url,
                description: description,
                added-by: tx-sender
            }))
    )
)


(define-map task-favorites
    { user: principal, task-id: uint }
    { favorited: bool }
)

(define-public (toggle-favorite (task-id uint))
    (let ((current-status (default-to false (get favorited (map-get? task-favorites { user: tx-sender, task-id: task-id })))))
        (ok (map-set task-favorites
            { user: tx-sender, task-id: task-id }
            { favorited: (not current-status) }))
    )
)


(define-map task-difficulty
    { task-id: uint }
    { 
        level: uint,  ;; 1-Easy, 2-Medium, 3-Hard
        estimated-hours: uint
    }
)

(define-public (set-task-difficulty (task-id uint) (level uint) (hours uint))
    (begin
        (asserts! (and (>= level u1) (<= level u3)) (err u401))
        (ok (map-set task-difficulty
            { task-id: task-id }
            { 
                level: level,
                estimated-hours: hours
            })))
)


(define-map task-checklist
    { task-id: uint, item-id: uint }
    {
        item: (string-utf8 100),
        checked: bool
    }
)

(define-data-var next-checklist-item-id uint u0)

(define-public (add-checklist-item (task-id uint) (item (string-utf8 100)))
    (let ((item-id (var-get next-checklist-item-id)))
        (var-set next-checklist-item-id (+ item-id u1))
        (ok (map-set task-checklist
            { task-id: task-id, item-id: item-id }
            {
                item: item,
                checked: false
            }))
    )
)

(define-public (toggle-checklist-item (task-id uint) (item-id uint))
    (let ((current-item (unwrap! (map-get? task-checklist { task-id: task-id, item-id: item-id }) (err u404))))
        (ok (map-set task-checklist
            { task-id: task-id, item-id: item-id }
            (merge current-item { checked: (not (get checked current-item)) })))
    )
)
