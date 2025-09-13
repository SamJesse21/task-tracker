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




(define-public (duplicate-task (task-id uint))
  (let 
    (
      (task (unwrap! (map-get? tasks { id: task-id }) (err u404)))
      (new-task-id (var-get next-task-id))
      (current-tasks (var-get task-ids))
    )
    (asserts! (< (len current-tasks) u1000) (err u500))
    (var-set next-task-id (+ new-task-id u1))
    
    (map-set tasks 
      { id: new-task-id }
      {
        title: (get title task),
        description: (get description task),
        deadline: (get deadline task),
        completed: false,
        creator: tx-sender,
        priority: (get priority task)
      }
    )
    
    (var-set task-ids (unwrap! (as-max-len? (append current-tasks new-task-id) u1000) (err u500)))
    
    (ok new-task-id)
  )
)


(define-map task-labels
    { task-id: uint }
    { color: (string-utf8 20) }
)

(define-public (set-task-label (task-id uint) (color (string-utf8 20)))
    (let ((task (unwrap! (map-get? tasks { id: task-id }) (err u404))))
        (asserts! (is-eq tx-sender (get creator task)) (err u403))
        (ok (map-set task-labels 
            { task-id: task-id }
            { color: color }))
    )
)


(define-map priority-queue
    { priority-level: uint }
    { task-ids: (list 100 uint) }
)

(define-public (add-to-priority-queue (task-id uint) (priority-level uint))
    (let ((current-queue (default-to (list) (get task-ids (map-get? priority-queue { priority-level: priority-level })))))
        (ok (map-set priority-queue
            { priority-level: priority-level }
            { task-ids: (unwrap! (as-max-len? (append current-queue task-id) u100) (err u500)) }))
    )
)


(define-map task-timers
    { task-id: uint }
    {
        start-time: uint,
        duration: uint,
        breaks-taken: uint
    }
)

(define-public (start-task-timer (task-id uint) (duration uint))
    (ok (map-set task-timers
        { task-id: task-id }
        {
            start-time: block-height,
            duration: duration,
            breaks-taken: u0
        }))
)


(define-map task-graph
    { task-id: uint }
    {
        blocked-by: (list 50 uint),
        blocking: (list 50 uint)
    }
)

(define-public (add-task-dependency (task-id uint) (depends-on uint))
    (let 
        (
            (current-blocked-by (default-to (list) (get blocked-by (map-get? task-graph { task-id: task-id }))))
            (current-blocking (default-to (list) (get blocking (map-get? task-graph { task-id: depends-on }))))
        )
        (map-set task-graph
            { task-id: task-id }
            { blocked-by: (unwrap! (as-max-len? (append current-blocked-by depends-on) u50) (err u500)),
              blocking: (get blocking (default-to { blocked-by: (list), blocking: (list) } (map-get? task-graph { task-id: task-id }))) })
        (ok (map-set task-graph
            { task-id: depends-on }
            { blocked-by: (get blocked-by (default-to { blocked-by: (list), blocking: (list) } (map-get? task-graph { task-id: depends-on }))),
              blocking: (unwrap! (as-max-len? (append current-blocking task-id) u50) (err u500)) }))
    )
)


(define-map task-scores
    { task-id: uint }
    {
        importance: uint,
        urgency: uint,
        effort: uint,
        total-score: uint
    }
)

(define-public (set-task-scores (task-id uint) (importance uint) (urgency uint) (effort uint))
    (ok (map-set task-scores
        { task-id: task-id }
        {
            importance: importance,
            urgency: urgency,
            effort: effort,
            total-score: (+ (+ importance urgency) effort)
        }))
)



(define-map projects
    { project-id: uint }
    {
        name: (string-utf8 100),
        tasks: (list 100 uint),
        owner: principal
    }
)

(define-data-var next-project-id uint u0)

(define-public (create-project (name (string-utf8 100)))
    (let ((project-id (var-get next-project-id)))
        (var-set next-project-id (+ project-id u1))
        (ok (map-set projects
            { project-id: project-id }
            {
                name: name,
                tasks: (list),
                owner: tx-sender
            }))
    )
)


(define-map task-collaborators
    { task-id: uint }
    {
        members: (list 10 principal),
        roles: (list 10 (string-utf8 20))
    }
)

(define-public (add-collaborator (task-id uint) (member principal) (role (string-utf8 20)))
    (let 
        (
            (current-members (default-to (list) (get members (map-get? task-collaborators { task-id: task-id }))))
            (current-roles (default-to (list) (get roles (map-get? task-collaborators { task-id: task-id }))))
        )
        (ok (map-set task-collaborators
            { task-id: task-id }
            {
                members: (unwrap! (as-max-len? (append current-members member) u10) (err u500)),
                roles: (unwrap! (as-max-len? (append current-roles role) u10) (err u500))
            }))
    )
)


(define-map task-analytics
    { task-id: uint }
    {
        views: uint,
        time-spent: uint,
        revisions: uint,
        completion-rate: uint
    }
)

(define-public (update-task-analytics (task-id uint) (view-count uint) (time-spent uint))
    (let ((current-analytics (default-to { views: u0, time-spent: u0, revisions: u0, completion-rate: u0 } 
                            (map-get? task-analytics { task-id: task-id }))))
        (ok (map-set task-analytics
            { task-id: task-id }
            {
                views: (+ (get views current-analytics) view-count),
                time-spent: (+ (get time-spent current-analytics) time-spent),
                revisions: (+ (get revisions current-analytics) u1),
                completion-rate: (if (> time-spent u0) (/ (* u100 (get time-spent current-analytics)) time-spent) u0)
            }))
    )
)


(define-map task-milestones
    { task-id: uint, milestone-id: uint }
    {
        title: (string-utf8 100),
        target-date: uint,
        completed: bool,
        reward-points: uint
    }
)

(define-data-var next-milestone-id uint u0)

(define-public (create-milestone (task-id uint) (title (string-utf8 100)) (target-date uint) (reward-points uint))
    (let 
        ((milestone-id (var-get next-milestone-id))
         (task (unwrap! (map-get? tasks { id: task-id }) (err u404))))
        (asserts! (is-eq tx-sender (get creator task)) (err u403))
        (var-set next-milestone-id (+ milestone-id u1))
        (ok (map-set task-milestones
            { task-id: task-id, milestone-id: milestone-id }
            {
                title: title,
                target-date: target-date,
                completed: false,
                reward-points: reward-points
            }))
    )
)

(define-public (complete-milestone (task-id uint) (milestone-id uint))
    (let ((milestone (unwrap! (map-get? task-milestones { task-id: task-id, milestone-id: milestone-id }) (err u404))))
        (ok (map-set task-milestones
            { task-id: task-id, milestone-id: milestone-id }
            (merge milestone { completed: true })))
    )
)


(define-map workflow-states
    { state-id: uint }
    {
        name: (string-utf8 50),
        allowed-transitions: (list 10 uint)
    }
)

(define-map task-workflow
    { task-id: uint }
    {
        current-state: uint,
        state-history: (list 50 uint),
        last-modified: uint
    }
)

(define-public (create-workflow-state (state-id uint) (name (string-utf8 50)) (transitions (list 10 uint)))
    (ok (map-set workflow-states
        { state-id: state-id }
        {
            name: name,
            allowed-transitions: transitions
        }))
)

(define-public (transition-task-state (task-id uint) (new-state uint))
    (let 
        ((current-workflow (unwrap! (map-get? task-workflow { task-id: task-id }) (err u404)))
         (current-state (get current-state current-workflow))
         (state-def (unwrap! (map-get? workflow-states { state-id: current-state }) (err u404))))
        
        (asserts! (is-some (index-of (get allowed-transitions state-def) new-state)) (err u403))
        (ok (map-set task-workflow
            { task-id: task-id }
            {
                current-state: new-state,
                state-history: (unwrap! (as-max-len? (append (get state-history current-workflow) current-state) u50) (err u500)),
                last-modified: block-height
            }))
    )
)

(define-map smart-priority-engine
  { user: principal }
  {
    deadline-weight: uint,
    difficulty-weight: uint,
    hours-weight: uint,
    preference-weight: uint,
    auto-update: bool
  }
)

(define-map task-smart-scores
  { task-id: uint }
  {
    deadline-score: uint,
    difficulty-score: uint,
    hours-score: uint,
    combined-score: uint,
    rank-position: uint,
    last-calculated: uint
  }
)

(define-map priority-suggestions
  { user: principal }
  { suggested-order: (list 50 uint) }
)

(define-data-var global-priority-weights 
  { deadline: uint, difficulty: uint, hours: uint, preference: uint }
  { deadline: u40, difficulty: u30, hours: u20, preference: u10 }
)

(define-public (initialize-priority-engine 
    (deadline-weight uint) 
    (difficulty-weight uint) 
    (hours-weight uint) 
    (preference-weight uint))
  (begin
    (asserts! (is-eq (+ (+ (+ deadline-weight difficulty-weight) hours-weight) preference-weight) u100) (err u401))
    (ok (map-set smart-priority-engine
      { user: tx-sender }
      {
        deadline-weight: deadline-weight,
        difficulty-weight: difficulty-weight,
        hours-weight: hours-weight,
        preference-weight: preference-weight,
        auto-update: true
      }))
  )
)

(define-public (calculate-smart-priority (task-id uint))
  (let 
    (
      (task (unwrap! (map-get? tasks { id: task-id }) (err u404)))
      (difficulty-data (map-get? task-difficulty { task-id: task-id }))
      (engine-config (unwrap! (map-get? smart-priority-engine { user: tx-sender }) (err u405)))
      (current-block block-height)
      (task-deadline (get deadline task))
      (blocks-until-deadline (if (> task-deadline current-block) (- task-deadline current-block) u0))
      (deadline-score (if (is-eq blocks-until-deadline u0) u100 
                       (if (< blocks-until-deadline u144) u80
                         (if (< blocks-until-deadline u1008) u60
                           (if (< blocks-until-deadline u4032) u40 u20)))))
      (difficulty-level (default-to u2 (get level difficulty-data)))
      (estimated-hours (default-to u4 (get estimated-hours difficulty-data)))
      (difficulty-score (* difficulty-level u25))
      (hours-score (if (> estimated-hours u8) u80
                    (if (> estimated-hours u4) u60
                      (if (> estimated-hours u2) u40 u20))))
      (preference-score (get priority task))
      (weighted-deadline (* deadline-score (get deadline-weight engine-config)))
      (weighted-difficulty (* difficulty-score (get difficulty-weight engine-config)))
      (weighted-hours (* hours-score (get hours-weight engine-config)))
      (weighted-preference (* preference-score (get preference-weight engine-config)))
      (combined-score (/ (+ (+ (+ weighted-deadline weighted-difficulty) weighted-hours) weighted-preference) u100))
    )
    (ok (map-set task-smart-scores
      { task-id: task-id }
      {
        deadline-score: deadline-score,
        difficulty-score: difficulty-score,
        hours-score: hours-score,
        combined-score: combined-score,
        rank-position: u0,
        last-calculated: current-block
      }))
  )
)

(define-public (generate-priority-suggestions)
  (let 
    (
      (user-tasks (filter is-user-task (var-get task-ids)))
      (scored-tasks (unwrap! (as-max-len? (map calculate-task-score-pair user-tasks) u50) (err u500)))
      (sorted-tasks (sort-tasks-by-score scored-tasks))
    )
    (ok (map-set priority-suggestions
      { user: tx-sender }
      { suggested-order: (map extract-task-id (unwrap! (as-max-len? sorted-tasks u50) (err u500))) }))
  )
)

(define-public (update-engine-weights 
    (deadline-weight uint) 
    (difficulty-weight uint) 
    (hours-weight uint) 
    (preference-weight uint))
  (begin
    (asserts! (is-eq (+ (+ (+ deadline-weight difficulty-weight) hours-weight) preference-weight) u100) (err u401))
    (let ((current-config (unwrap! (map-get? smart-priority-engine { user: tx-sender }) (err u405))))
      (ok (map-set smart-priority-engine
        { user: tx-sender }
        (merge current-config 
          {
            deadline-weight: deadline-weight,
            difficulty-weight: difficulty-weight,
            hours-weight: hours-weight,
            preference-weight: preference-weight
          })))
    )
  )
)

(define-public (auto-recalculate-priorities)
  (let ((user-tasks (filter is-user-task (var-get task-ids))))
    (fold recalculate-single-task user-tasks (ok true))
  )
)

(define-private (calculate-task-score-pair (task-id uint))
  { task-id: task-id, score: (get-task-smart-score task-id) }
)

(define-private (get-task-smart-score (task-id uint))
  (default-to u0 (get combined-score (map-get? task-smart-scores { task-id: task-id })))
)

(define-private (sort-tasks-by-score (task-pairs (list 50 { task-id: uint, score: uint })))
  task-pairs
)

(define-private (extract-task-id (task-pair { task-id: uint, score: uint }))
  (get task-id task-pair)
)

(define-private (recalculate-single-task (task-id uint) (previous-result (response bool uint)))
  (match previous-result
    success (calculate-smart-priority task-id)
    error (err error)
  )
)

(define-read-only (get-priority-suggestions)
  (map-get? priority-suggestions { user: tx-sender })
)

(define-read-only (get-task-smart-score-details (task-id uint))
  (map-get? task-smart-scores { task-id: task-id })
)

(define-read-only (get-engine-config)
  (map-get? smart-priority-engine { user: tx-sender })
)

;; Task Delegation Network with Skill Matching
;; Enables users to delegate tasks to skilled community members

;; User skill profiles and availability
(define-map user-skills
  { user: principal }
  {
    skills: (list 10 (string-utf8 30)),
    hourly-rate: uint,
    availability-hours: uint,
    timezone: (string-utf8 10),
    active: bool,
    last-updated: uint
  }
)

;; Delegation requests posted by task creators
(define-map delegation-requests
  { request-id: uint }
  {
    task-id: uint,
    delegator: principal,
    required-skills: (list 5 (string-utf8 30)),
    max-reward: uint,
    deadline: uint,
    description: (string-utf8 300),
    status: (string-utf8 20), ;; "open", "assigned", "completed", "cancelled"
    created-at: uint
  }
)

;; Active delegation contracts between users
(define-map delegation-contracts
  { contract-id: uint }
  {
    request-id: uint,
    delegator: principal,
    delegatee: principal,
    agreed-reward: uint,
    start-time: uint,
    expected-completion: uint,
    status: (string-utf8 20), ;; "active", "completed", "disputed", "cancelled"
    performance-rating: uint
  }
)

;; User reputation and performance metrics
(define-map user-reputation
  { user: principal }
  {
    total-delegations: uint,
    completed-delegations: uint,
    average-rating: uint,
    total-earnings: uint,
    reliability-score: uint,
    skill-endorsements: uint
  }
)

;; Skill endorsements from other users
(define-map skill-endorsements
  { endorser: principal, endorsed: principal, skill: (string-utf8 30) }
  { endorsement-strength: uint, timestamp: uint }
)

;; Delegation marketplace bids
(define-map delegation-bids
  { request-id: uint, bidder: principal }
  {
    proposed-reward: uint,
    estimated-completion: uint,
    proposal-message: (string-utf8 200),
    bid-timestamp: uint
  }
)

;; Skill matching scores for recommendations
(define-map skill-matches
  { request-id: uint, candidate: principal }
  {
    skill-score: uint,
    availability-score: uint,
    reputation-score: uint,
    total-match-score: uint,
    last-calculated: uint
  }
)

;; Data variables for ID management
(define-data-var next-delegation-request-id uint u0)
(define-data-var next-delegation-contract-id uint u0)

;; Register user skills and availability
(define-public (register-skills 
    (skills (list 10 (string-utf8 30)))
    (hourly-rate uint)
    (availability-hours uint)
    (timezone (string-utf8 10)))
  (begin
    ;; Validate input parameters
    (asserts! (> hourly-rate u0) (err u401))
    (asserts! (<= availability-hours u168) (err u402)) ;; Max 168 hours per week
    (asserts! (> (len skills) u0) (err u403))
    
    (ok (map-set user-skills
      { user: tx-sender }
      {
        skills: skills,
        hourly-rate: hourly-rate,
        availability-hours: availability-hours,
        timezone: timezone,
        active: true,
        last-updated: block-height
      }))
  )
)

;; Create a delegation request for a task
(define-public (create-delegation-request
    (task-id uint)
    (required-skills (list 5 (string-utf8 30)))
    (max-reward uint)
    (deadline uint)
    (description (string-utf8 300)))
  (let 
    (
      (request-id (var-get next-delegation-request-id))
      (task (unwrap! (map-get? tasks { id: task-id }) (err u404)))
    )
    ;; Verify task ownership
    (asserts! (is-eq tx-sender (get creator task)) (err u403))
    ;; Validate parameters
    (asserts! (> max-reward u0) (err u401))
    (asserts! (> deadline block-height) (err u405))
    (asserts! (> (len required-skills) u0) (err u406))
    
    ;; Increment request ID
    (var-set next-delegation-request-id (+ request-id u1))
    
    (ok (map-set delegation-requests
      { request-id: request-id }
      {
        task-id: task-id,
        delegator: tx-sender,
        required-skills: required-skills,
        max-reward: max-reward,
        deadline: deadline,
        description: description,
        status: u"open",
        created-at: block-height
      }))
  )
)

;; Submit a bid for a delegation request
(define-public (submit-delegation-bid
    (request-id uint)
    (proposed-reward uint)
    (estimated-completion uint)
    (proposal-message (string-utf8 200)))
  (let ((request (unwrap! (map-get? delegation-requests { request-id: request-id }) (err u404))))
    ;; Validate request is still open
    (asserts! (is-eq (get status request) u"open") (err u407))
    ;; Validate bid parameters
    (asserts! (<= proposed-reward (get max-reward request)) (err u408))
    (asserts! (<= estimated-completion (get deadline request)) (err u409))
    ;; Prevent self-bidding
    (asserts! (not (is-eq tx-sender (get delegator request))) (err u410))
    
    (ok (map-set delegation-bids
      { request-id: request-id, bidder: tx-sender }
      {
        proposed-reward: proposed-reward,
        estimated-completion: estimated-completion,
        proposal-message: proposal-message,
        bid-timestamp: block-height
      }))
  )
)

;; Accept a delegation bid and create contract
(define-public (accept-delegation-bid (request-id uint) (chosen-bidder principal))
  (let 
    (
      (request (unwrap! (map-get? delegation-requests { request-id: request-id }) (err u404)))
      (bid (unwrap! (map-get? delegation-bids { request-id: request-id, bidder: chosen-bidder }) (err u411)))
      (contract-id (var-get next-delegation-contract-id))
    )
    ;; Verify request ownership and status
    (asserts! (is-eq tx-sender (get delegator request)) (err u403))
    (asserts! (is-eq (get status request) u"open") (err u407))
    
    ;; Create delegation contract
    (var-set next-delegation-contract-id (+ contract-id u1))
    (map-set delegation-contracts
      { contract-id: contract-id }
      {
        request-id: request-id,
        delegator: tx-sender,
        delegatee: chosen-bidder,
        agreed-reward: (get proposed-reward bid),
        start-time: block-height,
        expected-completion: (get estimated-completion bid),
        status: u"active",
        performance-rating: u0
      })
    
    ;; Update request status
    (map-set delegation-requests
      { request-id: request-id }
      (merge request { status: u"assigned" }))
    
    (ok contract-id)
  )
)

;; Complete delegation and release payment
(define-public (complete-delegation (contract-id uint) (performance-rating uint))
  (let ((contract (unwrap! (map-get? delegation-contracts { contract-id: contract-id }) (err u404))))
    ;; Validate contract ownership and status
    (asserts! (is-eq tx-sender (get delegator contract)) (err u403))
    (asserts! (is-eq (get status contract) u"active") (err u412))
    ;; Validate rating range
    (asserts! (and (>= performance-rating u1) (<= performance-rating u5)) (err u413))
    
    ;; Update contract status and rating
    (map-set delegation-contracts
      { contract-id: contract-id }
      (merge contract { 
        status: u"completed",
        performance-rating: performance-rating
      }))
    
    ;; Update delegatee reputation
    (update-user-reputation (get delegatee contract) performance-rating (get agreed-reward contract))
    
    (ok true)
  )
)

;; Calculate skill matching score for a user and request
(define-public (calculate-skill-match (request-id uint) (candidate principal))
  (let 
    (
      (request (unwrap! (map-get? delegation-requests { request-id: request-id }) (err u404)))
      (user-profile (unwrap! (map-get? user-skills { user: candidate }) (err u414)))
      (reputation (default-to 
        { total-delegations: u0, completed-delegations: u0, average-rating: u0, 
          total-earnings: u0, reliability-score: u0, skill-endorsements: u0 }
        (map-get? user-reputation { user: candidate })))
      (required-skills (get required-skills request))
      (user-skills-list (get skills user-profile))
      (skill-overlap (calculate-skill-overlap required-skills user-skills-list))
      (skill-score (* skill-overlap u20)) ;; Max 100 if all skills match
      (availability-score (if (get active user-profile) u25 u0))
      (reputation-score (if (> (get average-rating reputation) u25) u25 (get average-rating reputation)))
      (total-score (+ (+ skill-score availability-score) reputation-score))
    )
    (ok (map-set skill-matches
      { request-id: request-id, candidate: candidate }
      {
        skill-score: skill-score,
        availability-score: availability-score,
        reputation-score: reputation-score,
        total-match-score: total-score,
        last-calculated: block-height
      }))
  )
)

;; Endorse a user's skill
(define-public (endorse-skill (endorsed-user principal) (skill (string-utf8 30)) (strength uint))
  (begin
    ;; Validate endorsement parameters
    (asserts! (not (is-eq tx-sender endorsed-user)) (err u415))
    (asserts! (and (>= strength u1) (<= strength u5)) (err u416))
    
    (ok (map-set skill-endorsements
      { endorser: tx-sender, endorsed: endorsed-user, skill: skill }
      { endorsement-strength: strength, timestamp: block-height }))
  )
)

;; Update user availability status
(define-public (update-availability (active bool))
  (let ((current-profile (unwrap! (map-get? user-skills { user: tx-sender }) (err u414))))
    (ok (map-set user-skills
      { user: tx-sender }
      (merge current-profile { active: active, last-updated: block-height })))
  )
)

;; Private helper function to update user reputation
(define-private (update-user-reputation (user principal) (rating uint) (earned-amount uint))
  (let 
    ((current-rep (default-to 
      { total-delegations: u0, completed-delegations: u0, average-rating: u0, 
        total-earnings: u0, reliability-score: u0, skill-endorsements: u0 }
      (map-get? user-reputation { user: user }))))
    (map-set user-reputation
      { user: user }
      {
        total-delegations: (+ (get total-delegations current-rep) u1),
        completed-delegations: (+ (get completed-delegations current-rep) u1),
        average-rating: (/ (+ (* (get average-rating current-rep) (get total-delegations current-rep)) rating) 
                          (+ (get total-delegations current-rep) u1)),
        total-earnings: (+ (get total-earnings current-rep) earned-amount),
        reliability-score: (if (> (+ (get reliability-score current-rep) u5) u100) u100 (+ (get reliability-score current-rep) u5)),
        skill-endorsements: (get skill-endorsements current-rep)
      })
  )
)

;; Private helper to calculate skill overlap percentage
(define-private (calculate-skill-overlap (required (list 5 (string-utf8 30))) (available (list 10 (string-utf8 30))))
  (fold check-skill-match required u0)
)

;; Helper function for skill matching
(define-private (check-skill-match (skill (string-utf8 30)) (count uint))
  count ;; Simplified implementation - would need more complex matching logic
)

;; Read-only functions for querying delegation data
(define-read-only (get-user-skills (user principal))
  (map-get? user-skills { user: user })
)

(define-read-only (get-delegation-request (request-id uint))
  (map-get? delegation-requests { request-id: request-id })
)

(define-read-only (get-delegation-contract (contract-id uint))
  (map-get? delegation-contracts { contract-id: contract-id })
)

(define-read-only (get-user-reputation (user principal))
  (map-get? user-reputation { user: user })
)

(define-read-only (get-delegation-bid (request-id uint) (bidder principal))
  (map-get? delegation-bids { request-id: request-id, bidder: bidder })
)

(define-read-only (get-skill-match-score (request-id uint) (candidate principal))
  (map-get? skill-matches { request-id: request-id, candidate: candidate })
)

(define-map user-rewards
  { user: principal }
  {
    total-points: uint,
    available-points: uint,
    tasks-completed-today: uint,
    current-streak: uint,
    longest-streak: uint,
    last-activity-day: uint
  }
)

(define-map reward-store
  { item-id: uint }
  {
    name: (string-utf8 50),
    description: (string-utf8 200),
    cost: uint,
    available: bool,
    creator: principal
  }
)

(define-map user-purchases
  { user: principal, purchase-id: uint }
  {
    item-id: uint,
    purchase-date: uint,
    points-spent: uint
  }
)

(define-map reward-multipliers
  { action-type: (string-utf8 20) }
  { multiplier: uint }
)

(define-data-var next-reward-item-id uint u0)
(define-data-var next-purchase-id uint u0)

(define-public (initialize-user-rewards)
  (let ((existing-rewards (map-get? user-rewards { user: tx-sender })))
    (if (is-none existing-rewards)
      (ok (map-set user-rewards
        { user: tx-sender }
        {
          total-points: u0,
          available-points: u0,
          tasks-completed-today: u0,
          current-streak: u0,
          longest-streak: u0,
          last-activity-day: u0
        }))
      (ok true)
    )
  )
)

(define-public (award-task-completion-points (task-id uint))
  (let 
    (
      (task (unwrap! (map-get? tasks { id: task-id }) (err u404)))
      (user-reward (unwrap! (map-get? user-rewards { user: tx-sender }) (err u405)))
      (base-points (* (get priority task) u10))
      (streak-bonus (calculate-streak-bonus tx-sender))
      (total-points (+ (+ base-points u1) streak-bonus))
      (current-day (/ block-height u144))
    )
    (asserts! (is-eq tx-sender (get creator task)) (err u403))
    (asserts! (get completed task) (err u406))
    
    (let 
      (
        (is-same-day (is-eq (get last-activity-day user-reward) current-day))
        (is-consecutive-day (is-eq (get last-activity-day user-reward) (- current-day u1)))
        (new-streak (if is-consecutive-day (+ (get current-streak user-reward) u1) u1))
        (new-longest (if (> new-streak (get longest-streak user-reward)) new-streak (get longest-streak user-reward)))
        (daily-tasks (if is-same-day (+ (get tasks-completed-today user-reward) u1) u1))
      )
      (ok (map-set user-rewards
        { user: tx-sender }
        {
          total-points: (+ (get total-points user-reward) total-points),
          available-points: (+ (get available-points user-reward) total-points),
          tasks-completed-today: daily-tasks,
          current-streak: new-streak,
          longest-streak: new-longest,
          last-activity-day: current-day
        }))
    )
  )
)



(define-private (calculate-streak-bonus (user principal))
  (match (map-get? user-rewards { user: user })
    rewards
      (let ((streak (get current-streak rewards)))
        (if (>= streak u7)
          u50
          (if (>= streak u3)
            u20
            u0
          )
        )
      )
    u0
  )
)

(define-public (create-reward-item (name (string-utf8 50)) (description (string-utf8 200)) (cost uint))
  (let ((item-id (var-get next-reward-item-id)))
    (var-set next-reward-item-id (+ item-id u1))
    (ok (map-set reward-store
      { item-id: item-id }
      {
        name: name,
        description: description,
        cost: cost,
        available: true,
        creator: tx-sender
      }))
  )
)

(define-public (purchase-reward-item (item-id uint))
  (let 
    (
      (item (unwrap! (map-get? reward-store { item-id: item-id }) (err u404)))
      (user-reward (unwrap! (map-get? user-rewards { user: tx-sender }) (err u405)))
      (purchase-id (var-get next-purchase-id))
    )
    (asserts! (get available item) (err u407))
    (asserts! (>= (get available-points user-reward) (get cost item)) (err u408))
    
    (var-set next-purchase-id (+ purchase-id u1))
    
    (map-set user-rewards
      { user: tx-sender }
      (merge user-reward 
        { available-points: (- (get available-points user-reward) (get cost item)) }
      )
    )
    
    (ok (map-set user-purchases
      { user: tx-sender, purchase-id: purchase-id }
      {
        item-id: item-id,
        purchase-date: block-height,
        points-spent: (get cost item)
      }))
  )
)

(define-public (award-milestone-points (task-id uint) (milestone-id uint))
  (let 
    (
      (milestone (unwrap! (map-get? task-milestones { task-id: task-id, milestone-id: milestone-id }) (err u404)))
      (user-reward (unwrap! (map-get? user-rewards { user: tx-sender }) (err u405)))
      (points (get reward-points milestone))
    )
    (asserts! (get completed milestone) (err u406))
    
    (ok (map-set user-rewards
      { user: tx-sender }
      {
        total-points: (+ (get total-points user-reward) points),
        available-points: (+ (get available-points user-reward) points),
        tasks-completed-today: (get tasks-completed-today user-reward),
        current-streak: (get current-streak user-reward),
        longest-streak: (get longest-streak user-reward),
        last-activity-day: (get last-activity-day user-reward)
      }))
  )
)

(define-public (set-reward-multiplier (action-type (string-utf8 20)) (multiplier uint))
  (ok (map-set reward-multipliers
    { action-type: action-type }
    { multiplier: multiplier }
  ))
)




(define-read-only (get-user-rewards (user principal))
  (map-get? user-rewards { user: user })
)

(define-read-only (get-reward-item (item-id uint))
  (map-get? reward-store { item-id: item-id })
)

(define-read-only (get-user-rank (user principal))
  (match (map-get? user-rewards { user: user })
    rewards
      (let ((total-points (get total-points rewards)))
        (if (>= total-points u1000)
          "EXPERT"
          (if (>= total-points u500)
            "ADVANCED"
            (if (>= total-points u100)
              "INTERMEDIATE"
              "BEGINNER"
            )
          )
        )
      )
    "UNRANKED"
  )
)

(define-read-only (calculate-daily-bonus (user principal))
  (match (map-get? user-rewards { user: user })
    rewards
      (let 
        (
          (tasks-today (get tasks-completed-today rewards))
          (current-day (/ block-height u144))
          (last-day (get last-activity-day rewards))
        )
        (if (is-eq current-day last-day)
          (if (>= tasks-today u5)
            u100
            (if (>= tasks-today u3)
              u50
              (* tasks-today u10)
            )
          )
          u0
        )
      )
    u0
  )
)

(define-public (claim-daily-bonus)
  (let 
    (
      (user-reward (unwrap! (map-get? user-rewards { user: tx-sender }) (err u405)))
      (bonus-points (calculate-daily-bonus tx-sender))
      (current-day (/ block-height u144))
    )
    (asserts! (> bonus-points u0) (err u409))
    (asserts! (is-eq (get last-activity-day user-reward) current-day) (err u410))
    
    (ok (map-set user-rewards
      { user: tx-sender }
      (merge user-reward 
        {
          total-points: (+ (get total-points user-reward) bonus-points),
          available-points: (+ (get available-points user-reward) bonus-points)
        }
      )
    ))
  )
)

(define-read-only (get-leaderboard-position (user principal))
  (match (map-get? user-rewards { user: user })
    rewards (get total-points rewards)
    u0
  )
)

;; Task Performance Forecasting Engine
;; Predicts task completion times based on historical data and user patterns

;; Historical performance tracking per user and difficulty level
(define-map performance-history
  { user: principal, difficulty-level: uint }
  {
    total-tasks: uint,
    total-time-spent: uint,
    average-completion-time: uint,
    accuracy-rate: uint,
    last-updated: uint
  }
)

;; Current workload analysis
(define-map workload-analysis
  { user: principal }
  {
    active-tasks: uint,
    estimated-total-hours: uint,
    overdue-tasks: uint,
    capacity-utilization: uint,
    forecast-accuracy: uint,
    last-calculated: uint
  }
)

;; Task completion forecasts
(define-map task-forecasts
  { task-id: uint }
  {
    estimated-completion-time: uint,
    confidence-level: uint,
    risk-factors: (list 3 (string-utf8 30)),
    forecast-method: (string-utf8 20),
    created-at: uint
  }
)

;; Performance patterns and trends
(define-map performance-trends
  { user: principal }
  {
    productivity-trend: uint, ;; 0-declining, 50-stable, 100-improving
    peak-performance-hours: uint,
    burnout-risk: uint,
    optimal-task-load: uint,
    trend-direction: (string-utf8 10)
  }
)

;; Update performance history when tasks are completed
(define-public (update-performance-history (task-id uint) (actual-time-spent uint))
  (let 
    (
      (task (unwrap! (map-get? tasks { id: task-id }) (err u404)))
      (difficulty-data (map-get? task-difficulty { task-id: task-id }))
      (difficulty-level (default-to u2 (get level difficulty-data)))
      (user tx-sender)
      (current-history (default-to
        { total-tasks: u0, total-time-spent: u0, average-completion-time: u0, accuracy-rate: u100, last-updated: u0 }
        (map-get? performance-history { user: user, difficulty-level: difficulty-level })))
      (new-total-tasks (+ (get total-tasks current-history) u1))
      (new-total-time (+ (get total-time-spent current-history) actual-time-spent))
      (new-average (if (> new-total-tasks u0) (/ new-total-time new-total-tasks) u0))
    )
    (asserts! (is-eq tx-sender (get creator task)) (err u403))
    (asserts! (get completed task) (err u406))
    (asserts! (> actual-time-spent u0) (err u401))
    
    (ok (map-set performance-history
      { user: user, difficulty-level: difficulty-level }
      {
        total-tasks: new-total-tasks,
        total-time-spent: new-total-time,
        average-completion-time: new-average,
        accuracy-rate: (calculate-forecast-accuracy user difficulty-level),
        last-updated: block-height
      }))
  )
)

;; Generate completion time forecast for a new task
(define-public (forecast-task-completion (task-id uint))
  (let 
    (
      (task (unwrap! (map-get? tasks { id: task-id }) (err u404)))
      (difficulty-data (map-get? task-difficulty { task-id: task-id }))
      (difficulty-level (default-to u2 (get level difficulty-data)))
      (estimated-hours (default-to u4 (get estimated-hours difficulty-data)))
      (user (get creator task))
      (history (map-get? performance-history { user: user, difficulty-level: difficulty-level }))
      (workload (map-get? workload-analysis { user: user }))
      (base-estimate (match history
        hist (get average-completion-time hist)
        estimated-hours))
      (workload-factor (match workload
        load (if (> (get capacity-utilization load) u80) u120 u100)
        u100))
      (adjusted-estimate (/ (* base-estimate workload-factor) u100))
      (confidence (calculate-forecast-confidence user difficulty-level))
      (risk-factors (identify-risk-factors task-id user))
    )
    (asserts! (is-eq tx-sender user) (err u403))
    
    (ok (map-set task-forecasts
      { task-id: task-id }
      {
        estimated-completion-time: adjusted-estimate,
        confidence-level: confidence,
        risk-factors: risk-factors,
        forecast-method: u"historical-pattern",
        created-at: block-height
      }))
  )
)

;; Analyze current workload and capacity
(define-public (analyze-workload-capacity)
  (let 
    (
      (user tx-sender)
      (user-tasks (filter is-user-task (var-get task-ids)))
      (active-tasks (filter is-active-incomplete-task user-tasks))
      (active-count (len active-tasks))
      (total-estimated-hours (fold sum-task-hours active-tasks u0))
      (overdue-count (fold count-overdue-tasks active-tasks u0))
      (capacity-percent (if (> total-estimated-hours u0) 
                         (let ((calc-percent (/ (* total-estimated-hours u100) u40)))
                           (if (> calc-percent u100) u100 calc-percent)) u0))
      (current-accuracy (calculate-overall-accuracy user))
    )
    (ok (map-set workload-analysis
      { user: user }
      {
        active-tasks: active-count,
        estimated-total-hours: total-estimated-hours,
        overdue-tasks: overdue-count,
        capacity-utilization: capacity-percent,
        forecast-accuracy: current-accuracy,
        last-calculated: block-height
      }))
  )
)

;; Generate performance trend analysis
(define-public (analyze-performance-trends)
  (let 
    (
      (user tx-sender)
      (recent-tasks (get-recent-completed-tasks user))
      (productivity-score (calculate-productivity-trend user))
      (peak-hours (analyze-peak-performance-hours user))
      (burnout-score (calculate-burnout-risk user))
      (optimal-load (calculate-optimal-task-load user))
      (trend-dir (if (> productivity-score u60) u"improving" 
                   (if (< productivity-score u40) u"declining" u"stable")))
    )
    (ok (map-set performance-trends
      { user: user }
      {
        productivity-trend: productivity-score,
        peak-performance-hours: peak-hours,
        burnout-risk: burnout-score,
        optimal-task-load: optimal-load,
        trend-direction: trend-dir
      }))
  )
)

;; Private helper functions for forecasting

(define-private (calculate-forecast-accuracy (user principal) (difficulty-level uint))
  (match (map-get? performance-history { user: user, difficulty-level: difficulty-level })
    hist (let ((new-rate (+ (get accuracy-rate hist) u5)))
           (if (> new-rate u100) u100 new-rate))
    u75
  )
)

(define-private (calculate-forecast-confidence (user principal) (difficulty-level uint))
  (match (map-get? performance-history { user: user, difficulty-level: difficulty-level })
    hist (if (> (get total-tasks hist) u10) u90
           (if (> (get total-tasks hist) u5) u75
             (if (> (get total-tasks hist) u2) u60 u40)))
    u30
  )
)

(define-private (identify-risk-factors (task-id uint) (user principal))
  (let 
    (
      (workload (map-get? workload-analysis { user: user }))
      (high-capacity (match workload load (> (get capacity-utilization load) u85) false))
      (has-overdue (match workload load (> (get overdue-tasks load) u2) false))
    )
    (if high-capacity
      (if has-overdue (list u"high-workload" u"overdue-tasks" u"time-pressure") (list u"high-workload"))
      (if has-overdue (list u"overdue-tasks") (list))
    )
  )
)

(define-private (is-active-incomplete-task (task-id uint))
  (match (map-get? tasks { id: task-id })
    task (and (not (get completed task)) (is-eq tx-sender (get creator task)))
    false
  )
)

(define-private (sum-task-hours (task-id uint) (accumulator uint))
  (let ((difficulty-data (map-get? task-difficulty { task-id: task-id })))
    (+ accumulator (default-to u4 (get estimated-hours difficulty-data)))
  )
)

(define-private (count-overdue-tasks (task-id uint) (count uint))
  (match (map-get? tasks { id: task-id })
    task (if (and (not (get completed task)) (< (get deadline task) block-height))
           (+ count u1)
           count)
    count
  )
)

(define-private (get-recent-completed-tasks (user principal))
  (filter is-recent-completed-task (filter is-user-task (var-get task-ids)))
)

(define-private (is-recent-completed-task (task-id uint))
  (match (map-get? tasks { id: task-id })
    task (and (get completed task) (< (- block-height u1008) block-height)) ;; Last week
    false
  )
)

(define-private (calculate-productivity-trend (user principal))
  (match (map-get? user-rewards { user: user })
    rewards (let ((trend-score (+ (* (get current-streak rewards) u10) u30)))
              (if (> trend-score u100) u100 trend-score))
    u50
  )
)

(define-private (analyze-peak-performance-hours (user principal))
  u14 ;; Simplified: assume 2 PM is peak (block 14 of day)
)

(define-private (calculate-burnout-risk (user principal))
  (match (map-get? workload-analysis { user: user })
    load (if (> (get capacity-utilization load) u90) u80
           (if (> (get capacity-utilization load) u70) u50 u20))
    u20
  )
)

(define-private (calculate-optimal-task-load (user principal))
  (match (map-get? performance-trends { user: user })
    trends (if (> (get burnout-risk trends) u60) u3
             (if (< (get productivity-trend trends) u40) u5 u8))
    u5
  )
)

(define-private (calculate-overall-accuracy (user principal))
  (let 
    (
      (easy-history (map-get? performance-history { user: user, difficulty-level: u1 }))
      (medium-history (map-get? performance-history { user: user, difficulty-level: u2 }))
      (hard-history (map-get? performance-history { user: user, difficulty-level: u3 }))
      (easy-acc (match easy-history hist (get accuracy-rate hist) u0))
      (medium-acc (match medium-history hist (get accuracy-rate hist) u0))
      (hard-acc (match hard-history hist (get accuracy-rate hist) u0))
      (total-histories (+ (if (is-some easy-history) u1 u0)
                          (+ (if (is-some medium-history) u1 u0)
                             (if (is-some hard-history) u1 u0))))
    )
    (if (> total-histories u0)
      (/ (+ (+ easy-acc medium-acc) hard-acc) total-histories)
      u75) ;; Default accuracy if no history
  )
)

;; Read-only functions for forecasting insights

(define-read-only (get-task-forecast (task-id uint))
  (map-get? task-forecasts { task-id: task-id })
)

(define-read-only (get-workload-analysis (user principal))
  (map-get? workload-analysis { user: user })
)

(define-read-only (get-performance-trends (user principal))
  (map-get? performance-trends { user: user })
)

(define-read-only (get-capacity-recommendation (user principal))
  (match (map-get? workload-analysis { user: user })
    analysis
      (let ((utilization (get capacity-utilization analysis)))
        (if (> utilization u85)
          u"reduce-workload"
          (if (< utilization u50)
            u"can-take-more"
            u"optimal-load"
          )
        )
      )
    u"no-data"
  )
)
