#lang racket/gui

(require racket/date
         racket/format
         json)

(define macos-divider-color (make-object color% 209 209 214))

;; 数据结构定义
(struct task (id text due-date completed? list-name created-at) #:transparent)
(struct todo-list (name color) #:transparent)

;; 全局状态
(define current-vault-path (make-parameter "todo-vault"))
(define all-tasks (make-parameter '()))
(define all-lists (make-parameter (list (todo-list "工作" "blue")
                                        (todo-list "生活" "green"))))
(define current-filter (make-parameter "工作"))
(define current-view (make-parameter "list")) ; "list", "today", "planned", "all", "completed"

;; 日期格式化函数
(define (normalize-date-string date-str)
  (let ([trimmed-str (string-trim date-str)])
    (if (equal? trimmed-str "")
        #f
        (let ([parts (string-split trimmed-str "-")])
          (if (= (length parts) 3)
              (let* ([year-str (list-ref parts 0)]
                     [month-str (list-ref parts 1)]
                     [day-str (list-ref parts 2)]
                     [year-num (string->number year-str)]
                     [month-num (string->number month-str)]
                     [day-num (string->number day-str)])
                (if (and year-num month-num day-num
                         (<= 1 month-num 12)
                         (<= 1 day-num 31)
                         (<= 1900 year-num 9999))
                    (format "~a-~a-~a"
                            (if (< year-num 1000) 
                                (format "0~a" year-num) 
                                (~a year-num))
                            (if (< month-num 10) 
                                (format "0~a" month-num) 
                                (~a month-num))
                            (if (< day-num 10) 
                                (format "0~a" day-num) 
                                (~a day-num)))
                    #f))
              #f)))))

;; 文件操作函数
(define (ensure-vault-directory)
  (unless (directory-exists? (current-vault-path))
    (make-directory* (current-vault-path)))
  (unless (directory-exists? (build-path (current-vault-path) "logs"))
    (make-directory* (build-path (current-vault-path) "logs"))))

(define (tasks-file-path)
  (build-path (current-vault-path) "tasks.json"))

;; 任务管理函数
(define (toggle-task! id-to-toggle)
  (with-handlers ([exn:fail? (lambda (e)
                               (printf "切换任务状态错误: ~a\n" (exn-message e)))])
    (define updated-tasks
      (map (lambda (t)
             (if (equal? (task-id t) id-to-toggle)
                 (struct-copy task t [completed? (not (task-completed? t))])
                 t))
           (all-tasks)))
    (all-tasks updated-tasks)
    (save-tasks!)))

(define (delete-task! id-to-delete)
  (with-handlers ([exn:fail? (lambda (e)
                               (printf "删除任务错误: ~a\n" (exn-message e)))])
    (all-tasks (filter (lambda (t) (not (equal? (task-id t) id-to-delete))) (all-tasks)))
    (save-tasks!)))

(define (edit-task! id-to-edit new-text new-due-date)
  (with-handlers ([exn:fail? (lambda (e)
                               (printf "编辑任务错误: ~a\n" (exn-message e)))])
    (define updated-tasks
      (map (lambda (t)
             (if (equal? (task-id t) id-to-edit)
                 (struct-copy task t [text new-text] [due-date new-due-date])
                 t))
           (all-tasks)))
    (all-tasks updated-tasks)
    (save-tasks!)))

;; 创建列表分组标题组件
(define list-group-header%
  (class horizontal-panel%
    (init-field list-name task-count)
    
    (super-new (stretchable-height #f)
               (stretchable-width #t)
               (spacing 8)
               (border 8))
    
    (define title-msg (new message%
                           [parent this]
                           [label (format "~a (~a)" list-name task-count)]
                           [font (make-font #:size 12 #:weight 'bold #:family 'modern)]))
    
    (define separator-canvas (new canvas%
                                  [parent this]
                                  [stretchable-width #t]
                                  [min-height 1]
                                  [stretchable-height #f]
                                  [paint-callback
                                   (lambda (canvas dc)
                                     (define-values (w h) (send canvas get-size))
                                     (send dc set-pen macos-divider-color 1 'solid)
                                     (send dc draw-line 0 (/ h 2) w (/ h 2)))]))
    
    (void)))

(define task-item%
  (class horizontal-panel%
    (init-field task-data)
    
    (super-new (stretchable-height #f)
               (stretchable-width #t)
               (style '(border))
               (spacing 8)
               (border 5))
    
    (when task-data
      (define current-task-id (task-id task-data))
      (define editing? #f)

      (define checkbox-panel (new panel%
                                  [parent this]
                                  [min-width 30]
                                  [stretchable-width #f]))
      
      (define checkbox (new check-box%
                            [parent checkbox-panel]
                            [label ""]
                            [value (task-completed? task-data)]
                            [callback (lambda (cb evt)
                                        (toggle-task! current-task-id)
                                        (refresh-task-list!))]))

      (define text-date-panel (new vertical-panel%
                                   [parent this]
                                   [stretchable-width #t]
                                   [alignment '(left top)]
                                   [spacing 2]))
      
      (define task-text-msg (new message%
                                 [parent text-date-panel]
                                 [stretchable-width #t]
                                 [label (task-text task-data)]
                                 [font (if (task-completed? task-data)
                                           (make-font #:family 'modern)
                                           (make-font #:family 'modern))]))
      
      (define task-text-field #f)
      (define task-date-field #f)
      
      (define due-label
        (if (task-due-date task-data)
            (new message%
                 [parent text-date-panel]
                 [label (format-date (task-due-date task-data))]
                 [font (make-font #:size 9 #:family 'modern)])
            #f))

      (define button-panel (new horizontal-panel%
                                [parent this]
                                [stretchable-width #f]
                                [spacing 4]))
      
      (define edit-btn (new button%
                            [parent button-panel]
                            [label "✎"]
                            [min-width 20]
                            [min-height 24]
                            [callback (lambda (btn evt)
                                        (if editing?
                                            (finish-edit)
                                            (start-edit)))]))
      
      (define delete-btn (new button%
                              [parent button-panel]
                              [label "×"]
                              [min-width 20]
                              [min-height 24]
                              [callback (lambda (btn evt)
                                          (delete-task! current-task-id)
                                          (refresh-task-list!))]))

      (define (start-edit)
        (set! editing? #t)
        (send edit-btn set-label "✓")
        (send task-text-msg show #f)
        (when due-label (send due-label show #f))
        
        (set! task-text-field 
              (new text-field%
                   [parent text-date-panel]
                   [label "任务:"]
                   [init-value (task-text task-data)]
                   [stretchable-width #t]))
        
        (set! task-date-field
              (new text-field%
                   [parent text-date-panel]
                   [label "日期:"]
                   [init-value (if (task-due-date task-data) 
                                   (task-due-date task-data) 
                                   "")]
                   [stretchable-width #t]
                   [callback (lambda (tf evt)
                               (when (eq? (send evt get-event-type) 'text-field-enter)
                                 (finish-edit)))]))
        (send task-text-field focus))

      (define (finish-edit)
        (when (and task-text-field task-date-field)
          (define new-text (send task-text-field get-value))
          (define new-date (send task-date-field get-value))
          
          (when (not (equal? (string-trim new-text) ""))
            (define normalized-date
              (if (not (equal? (string-trim new-date) ""))
                  (normalize-date-string (string-trim new-date))
                  #f))
            
            (if (or (equal? (string-trim new-date) "") normalized-date)
                (begin
                  (edit-task! current-task-id new-text normalized-date)
                  (send task-text-msg set-label new-text)
                  
                  (when due-label
                    (send text-date-panel delete-child due-label)
                    (set! due-label #f))
                  
                  (when normalized-date
                    (set! due-label
                          (new message%
                               [parent text-date-panel]
                               [label (format-date normalized-date)]
                               [font (make-font #:size 9 #:family 'modern)])))
                  
                  (send task-text-field show #f)
                  (send task-date-field show #f)
                  (send text-date-panel delete-child task-text-field)
                  (send text-date-panel delete-child task-date-field)
                  (set! task-text-field #f)
                  (set! task-date-field #f)
                  (send task-text-msg show #t)
                  (when due-label (send due-label show #t))
                  (set! editing? #f)
                  (send edit-btn set-label "✎"))
                
                (message-box "日期格式错误" 
                             "请输入正确的日期格式 (YYYY-MM-DD),例如: 2025-08-07,或留空表示无截止日期" 
                             frame '(ok))))))

      (void))))

;; 保存和加载函数
(define (save-tasks!)
  (ensure-vault-directory)
  (define data (hash 'tasks (map task->hash (all-tasks))
                     'lists (map todo-list->hash (all-lists))))
  (call-with-output-file (tasks-file-path)
    (lambda (out)
      (write-json data out))
    #:exists 'replace))

(define (load-tasks!)
  (ensure-vault-directory)
  (when (file-exists? (tasks-file-path))
    (with-handlers ([exn:fail? (lambda (e)
                                 (printf "错误:无法读取任务文件,使用默认设置\n")
                                 (printf "错误信息:~a\n" (exn-message e)))])
      (define data (call-with-input-file (tasks-file-path)
                     (lambda (in)
                       (define content (read-json in))
                       (if (eof-object? content)
                           (hash 'tasks '() 'lists '())
                           content))))
      (when (hash? data)
        (all-tasks (map hash->task (hash-ref data 'tasks '())))
        (when (hash-has-key? data 'lists)
          (all-lists (map hash->todo-list (hash-ref data 'lists '()))))))))

(define (task->hash t)
  (hash 'id (task-id t)
        'text (task-text t)
        'due-date (if (task-due-date t) (task-due-date t) "")
        'completed (task-completed? t)
        'list-name (task-list-name t)
        'created-at (task-created-at t)))

(define (hash->task h)
  (task (hash-ref h 'id 0)
        (hash-ref h 'text "")
        (let ([due (hash-ref h 'due-date "")])
          (if (equal? due "") #f due))
        (hash-ref h 'completed #f)
        (hash-ref h 'list-name "默认")
        (hash-ref h 'created-at (hash-ref h 'created-at (current-seconds)))))

(define (todo-list->hash tl)
  (hash 'name (todo-list-name tl)
        'color (todo-list-color tl)))

(define (hash->todo-list h)
  (todo-list (hash-ref h 'name "默认")
             (hash-ref h 'color "blue")))

(define (generate-task-id)
  (current-milliseconds))

(define (add-task! text due-date list-name)
  (define new-task (task (generate-task-id)
                         text
                         due-date
                         #f
                         list-name
                         (current-seconds)))
  (all-tasks (cons new-task (all-tasks)))
  (save-tasks!))

(define (add-todo-list! name color)
  (define new-list (todo-list name color))
  (all-lists (cons new-list (all-lists)))
  (save-tasks!))

(define (delete-todo-list! name)
  (all-lists (filter (lambda (tl) (not (equal? (todo-list-name tl) name))) (all-lists)))
  (all-tasks (filter (lambda (t) (not (equal? (task-list-name t) name))) (all-tasks)))
  (when (equal? (current-filter) name)
    (if (not (empty? (all-lists)))
        (let ([first-list (todo-list-name (first (all-lists)))])
          (current-filter first-list)
          (send title-label set-label first-list))
        (begin
          (current-filter "")
          (send title-label set-label "无列表"))))
  (save-tasks!))

;; 改进的任务排序函数
(define (sort-tasks-by-date tasks)
  (sort tasks
        (lambda (t1 t2)
          (define date1 (task-due-date t1))
          (define date2 (task-due-date t2))
          (cond
            [(and date1 date2) (string<? date1 date2)]
            [date1 #t]  ; 有日期的排在前面
            [date2 #f]  ; 有日期的排在前面
            [else (< (task-created-at t1) (task-created-at t2))]))))  ; 都没日期按创建时间

;; 按列表分组任务
(define (group-tasks-by-list tasks)
  (define groups (make-hash))
  
  ;; 将任务按列表分组
  (for ([task tasks])
    (define list-name (task-list-name task))
    (hash-set! groups list-name 
               (cons task (hash-ref groups list-name '()))))
  
  ;; 对每个分组内的任务按日期排序,并返回有序的分组列表
  (define sorted-groups '())
  (for ([list-obj (all-lists)])
    (define list-name (todo-list-name list-obj))
    (when (hash-has-key? groups list-name)
      (define group-tasks (hash-ref groups list-name))
      (define sorted-tasks (sort-tasks-by-date group-tasks))
      (set! sorted-groups (cons (cons list-name sorted-tasks) sorted-groups))))
  
  (reverse sorted-groups))

;; 改进的列表过滤逻辑
(define (filter-tasks)
  (with-handlers ([exn:fail? (lambda (e)
                               (printf "过滤任务错误: ~a\n" (exn-message e))
                               '())])
    (case (current-view)
      [("today")
       (let* ([today-struct (current-date)]
              [year (date-year today-struct)]
              [month (date-month today-struct)]
              [day (date-day today-struct)]
              [today-str (format "~a-~a-~a"
                                 (~a year)
                                 (if (< month 10) (format "0~a" month) (~a month))
                                 (if (< day 10) (format "0~a" day) (~a day)))])
         (filter (lambda (t)
                   (and (task-due-date t)
                        (string? (task-due-date t))
                        (equal? (task-due-date t) today-str)
                        (not (task-completed? t))))
                 (all-tasks)))]
      [("planned")
       (filter (lambda (t)
                 (and (task-due-date t)
                      (string? (task-due-date t))
                      (not (equal? (task-due-date t) ""))
                      (not (task-completed? t))))
               (all-tasks))]
      [("all")
       (filter (lambda (t) (not (task-completed? t))) (all-tasks))]
      [("completed") (filter task-completed? (all-tasks))]
      [else
       (let ([filter-val (current-filter)])
         (if (and filter-val (not (equal? filter-val "")))
             (filter (lambda (t)
                       (and (task-list-name t)
                            (equal? (task-list-name t) filter-val)
                            (not (task-completed? t))))
                     (all-tasks))
             (filter (lambda (t) (not (task-completed? t))) (all-tasks))))])))

(define (format-date date-str)
  (if (and date-str (string? date-str) (not (equal? date-str "")))
      (let ([parts (string-split date-str "-")])
        (if (= (length parts) 3)
            (let ([year (string->number (list-ref parts 0))]
                  [month (string->number (list-ref parts 1))]
                  [day (string->number (list-ref parts 2))])
              (if (and year month day)
                  (format "~a月~a日" month day)
                  date-str))
            date-str))
      ""))

(define frame (new frame%
                   [label "RReminder"]
                   [min-width 850]
                   [min-height 650]
                   ))

(define main-panel (new horizontal-panel% 
                        [parent frame] 
                        [spacing 0] 
                        [border 0]))

(define sidebar (new vertical-panel%
                     [parent main-panel]
                     [min-width 120]
                     [spacing 4]
                     [border 4]
                     [stretchable-width #f]
                     ))

(define divider (new canvas%
                     [parent main-panel]
                     [min-width 1]
                     [stretchable-width #f]
                     [stretchable-height #t]
                     [paint-callback
                      (lambda (canvas dc)
                        (define-values (w h) (send canvas get-size))
                        (send dc set-pen macos-divider-color 1 'solid)
                        (send dc draw-line 0 0 0 h))]))

(define content-panel (new vertical-panel% 
                           [parent main-panel] 
                           [spacing 4] 
                           [border 4]
                           ))

(define filter-panel (new vertical-panel%
                          [parent sidebar]
                          [stretchable-height #f]
                          [spacing 4]
                          [border 4]))

(define filter-row1 (new horizontal-panel%
                         [parent filter-panel]
                         [stretchable-height #f]
                         [spacing 4]))

(define today-btn (new button%
                       [parent filter-row1]
                       [label "今天"]
                       [min-width 50]
                       [min-height 30]
                       [stretchable-width #t]
                       [callback (lambda (btn evt)
                                   (current-view "today")
                                   (send title-label set-label "今天")
                                   (refresh-task-list!))]))

(define planned-btn (new button%
                         [parent filter-row1]
                         [label "计划"]
                         [min-width 50]
                         [min-height 30]
                         [stretchable-width #t]
                         [callback (lambda (btn evt)
                                     (current-view "planned")
                                     (send title-label set-label "计划")
                                     (refresh-task-list!))]))

(define filter-row2 (new horizontal-panel%
                         [parent filter-panel]
                         [stretchable-height #f]
                         [spacing 4]))

(define all-btn (new button%
                     [parent filter-row2]
                     [label "所有"]
                     [min-width 50]
                     [min-height 30]
                     [stretchable-width #t]
                     [callback (lambda (btn evt)
                                 (current-view "all")
                                 (send title-label set-label "所有")
                                 (refresh-task-list!))]))

(define completed-btn (new button%
                           [parent filter-row2]
                           [label "完成"]
                           [min-width 50]
                           [min-height 30]
                           [stretchable-width #t]
                           [callback (lambda (btn evt)
                                       (current-view "completed")
                                       (send title-label set-label "完成")
                                       (refresh-task-list!))]))

(define label-panel (new horizontal-panel%
                         [parent sidebar]
                         [stretchable-height #f]
                         [spacing 4]))

(define my-lists-label (new message%
                            [parent label-panel]
                            [label "我的列表"]
                            [vert-margin 8]
                            [font (make-font #:weight 'bold #:family 'modern #:size 14)]))

(define lists-panel (new vertical-panel% [parent sidebar] [spacing 2]))
(define list-buttons '())

(define (refresh-list-buttons!)
  (send lists-panel change-children (lambda (children) '()))
  (set! list-buttons '())
  (for ([lst (all-lists)])
    (define btn (new button%
                     [parent lists-panel]
                     [label (todo-list-name lst)]
                     [min-width 140]
                     [min-height 28]
                     [callback (lambda (btn evt)
                                 (current-filter (todo-list-name lst))
                                 (current-view "list")
                                 (refresh-task-list!)
                                 (send title-label set-label (todo-list-name lst)))]))
    (set! list-buttons (cons btn list-buttons))))

(define spacer (new panel% [parent sidebar]))

(define list-management-panel (new horizontal-panel%
                                   [parent sidebar]
                                   [stretchable-height #f]
                                   [spacing 4]))

(define add-list-btn (new button%
                          [parent list-management-panel]
                          [label "+ 新建列表"]
                          [min-width 65]
                          [min-height 32]
                          [callback (lambda (btn evt)
                                      (show-add-list-dialog))]))

(define delete-list-btn (new button%
                             [parent list-management-panel]
                             [label "- 删除列表"]
                             [min-width 65]
                             [min-height 32]
                             [callback (lambda (btn evt)
                                         (when (not (equal? (current-filter) ""))
                                           (define result
                                             (message-box "确认删除"
                                                          (format "确定要删除列表 \"~a\" 及其所有任务吗?"
                                                                  (current-filter))
                                                          #f
                                                          '(yes-no)))
                                           (when (eq? result 'yes)
                                             (delete-todo-list! (current-filter))
                                             (refresh-list-buttons!)
                                             (refresh-task-list!))))]))

(define title-panel (new horizontal-panel%
                         [parent content-panel]
                         [stretchable-height #f]))

(define title-label (new message%
                         [parent title-panel]
                         [label "工作"]
                         [vert-margin 12]
                         [font (make-font #:size 18 #:weight 'bold #:family 'modern)]))

(define task-scroll (new panel%
                         [parent content-panel]
                         [style '(vscroll)]))

(define task-list-panel (new vertical-panel%
                             [parent task-scroll]
                             [min-width 1]
                             [stretchable-height #t]
                             [stretchable-width #t]
                             [spacing 2]))

(define bottom-panel (new horizontal-panel%
                          [parent content-panel]
                          [stretchable-height #f]))

(define add-task-btn (new button%
                          [parent bottom-panel]
                          [label "+ 新提醒事项"]
                          [min-height 32]
                          [callback (lambda (btn evt)
                                      (show-add-task-dialog))]))

;; 改进的任务列表刷新函数
(define (refresh-task-list!)
  (send task-list-panel change-children (lambda (children) '()))
  (define filtered-tasks (filter-tasks))
  
  ;; 判断是否需要分组显示
  (cond
    [(or (equal? (current-view) "planned") 
         (equal? (current-view) "all")
         (equal? (current-view) "completed"))
     ;; 分组显示
     (define grouped-tasks (group-tasks-by-list filtered-tasks))
     (for ([group grouped-tasks])
       (define list-name (car group))
       (define group-tasks (cdr group))
       (when (not (empty? group-tasks))
         ;; 添加分组标题
         (new list-group-header% 
              [parent task-list-panel] 
              [list-name list-name] 
              [task-count (length group-tasks)])
         ;; 添加该分组的任务
         (for ([task-data group-tasks])
           (when task-data
             (new task-item% [parent task-list-panel] [task-data task-data])))))]
    [else
     ;; 普通显示(今天、单个列表等)
     (define sorted-tasks (sort-tasks-by-date filtered-tasks))
     (for ([task-data sorted-tasks])
       (when task-data
         (new task-item% [parent task-list-panel] [task-data task-data])))]))

(define (show-add-task-dialog)
  (define dialog (new dialog%
                      [label "添加新任务"]
                      [parent frame]
                      [width 400]
                      [height 500]
                      [stretchable-width #t]
                      [stretchable-height #f]))
  (define dialog-panel (new vertical-panel% [parent dialog] [spacing 8] [border 12]))
  (new message% [parent dialog-panel] [label "任务描述:"] [stretchable-width #t])
  (define text-field (new text-field% [parent dialog-panel] [label ""] [init-value ""]))
  (new message% [parent dialog-panel] [label "截止日期 (YYYY-MM-DD, 可选):"] [stretchable-width #t])
  (define date-field (new text-field% [parent dialog-panel] [label ""] [init-value ""]))
  (define button-panel (new horizontal-panel% [parent dialog-panel] [spacing 8]))
  (define ok-btn (new button%
                      [parent button-panel]
                      [label "确定"]
                      [min-width 60]
                      [callback (lambda (btn evt)
                                  (define text (send text-field get-value))
                                  (define date (send date-field get-value))
                                  (when (not (equal? text ""))
                                    (define normalized-date-str
                                      (if (not (equal? (string-trim date) ""))
                                          (normalize-date-string (string-trim date))
                                          #f))
                                    (if (or (not (string-trim date))
                                            (equal? (string-trim date) "")
                                            normalized-date-str)
                                        (let ([target-list
                                               (cond
                                                 [(and (equal? (current-view) "list")
                                                       (not (equal? (current-filter) "")))
                                                  (current-filter)]
                                                 [(not (empty? (all-lists)))
                                                  (todo-list-name (first (all-lists)))]
                                                 [else "默认"])])
                                          (when (equal? target-list "默认")
                                            (add-todo-list! "默认" "blue"))
                                          (add-task! text
                                                     normalized-date-str
                                                     target-list)
                                          (current-view "list")
                                          (current-filter target-list)
                                          (send title-label set-label target-list)
                                          (refresh-task-list!)
                                          (send dialog show #f))
                                        (message-box "日期格式错误"
                                                     "请输入正确的日期格式 (YYYY-MM-DD),例如: 2025-08-07"
                                                     dialog
                                                     '(ok)))))]))
  (define cancel-btn (new button%
                          [parent button-panel]
                          [label "取消"]
                          [min-width 60]
                          [callback (lambda (btn evt)
                                      (send dialog show #f))]))
  (send text-field focus)
  (send dialog show #t))

(define (show-add-list-dialog)
  (define dialog (new dialog%
                      [label "添加新列表"]
                      [parent frame]
                      [width 320]
                      [height 160]))
  (define dialog-panel (new vertical-panel% [parent dialog] [spacing 8] [border 12]))
  (new message% [parent dialog-panel] [label "列表名称:"])
  (define name-field (new text-field% [parent dialog-panel] [label ""] [init-value ""]))
  (define button-panel (new horizontal-panel% [parent dialog-panel] [spacing 8]))
  (define ok-btn (new button%
                      [parent button-panel]
                      [label "确定"]
                      [min-width 60]
                      [callback (lambda (btn evt)
                                  (define name (send name-field get-value))
                                  (when (not (equal? name ""))
                                    (add-todo-list! name "blue")
                                    (refresh-list-buttons!)
                                    (send dialog show #f)))]))
  (define cancel-btn (new button%
                          [parent button-panel]
                          [label "取消"]
                          [min-width 60]
                          [callback (lambda (btn evt)
                                      (send dialog show #f))]))
  (send name-field focus)
  (send dialog show #t))

(define (init-app!)
  (load-tasks!)
  (when (empty? (all-lists))
    (all-lists (list (todo-list "工作" "blue")
                     (todo-list "生活" "green")))
    (save-tasks!))
  (when (not (empty? (all-lists)))
    (current-filter (todo-list-name (first (all-lists))))
    (send title-label set-label (current-filter)))
  (refresh-list-buttons!)
  (refresh-task-list!))

(init-app!)
(send frame center)
(send frame show #t)