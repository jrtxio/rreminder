#lang racket/gui

(require racket/date
         racket/format
         json)

;; 数据结构定义
(struct task (id text due-date completed? list-name created-at) #:transparent)
(struct todo-list (name color) #:transparent)

;; 全局状态
(define current-vault-path (make-parameter "todo-vault"))
(define all-tasks (make-parameter '()))
(define all-lists (make-parameter (list (todo-list "工作" "blue")
                                        (todo-list "生活" "green"))))
(define current-filter (make-parameter "工作"))
(define current-view (make-parameter "list")) ; "list", "today", "completed"

;; 修复问题1:日期格式化问题
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
                    ;; 使用简单的格式化方法
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

;; 修复问题2:添加编辑任务功能
(define (edit-task! id-to-edit new-text)
  (with-handlers ([exn:fail? (lambda (e)
                               (printf "编辑任务错误: ~a\n" (exn-message e)))])
    (define updated-tasks
      (map (lambda (t)
             (if (equal? (task-id t) id-to-edit)
                 (struct-copy task t [text new-text])
                 t))
           (all-tasks)))
    (all-tasks updated-tasks)
    (save-tasks!)))

;; 修复问题2和3:改进的任务项UI组件
(define task-item%
  (class horizontal-panel%
    (init-field task-data)
    
    (super-new (stretchable-height #f)
               (stretchable-width #t)
               (style '(border))
               (spacing 8) ; 统一间距
               (border 5)) ; 统一边框
    
    (when task-data
      (define current-task-id (task-id task-data))
      (define editing? #f)

      ;; 修复问题3:使用固定宽度的复选框确保跨平台一致性
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

      ;; 文本和日期面板
      (define text-date-panel (new vertical-panel%
                                   [parent this]
                                   [stretchable-width #t]
                                   [alignment '(left top)]
                                   [spacing 2]))
      
      ;; 任务文本显示/编辑控件
      (define task-text-msg (new message%
                                 [parent text-date-panel]
                                 [stretchable-width #t]
                                 [label (task-text task-data)]
                                 [font (if (task-completed? task-data)
                                           (make-font #:style 'italic #:family 'modern)
                                           (make-font #:family 'modern))]))
      
      (define task-text-field #f) ; 编辑时使用的文本框

      ;; 显示截止日期
      (define due-label
        (when (task-due-date task-data)
          (new message%
               [parent text-date-panel]
               [label (format-date (task-due-date task-data))]
               [font (make-font #:size 9 #:family 'modern #:style 'italic)])))

      ;; 修复问题3:按钮面板,确保按钮大小一致
      (define button-panel (new horizontal-panel%
                                [parent this]
                                [stretchable-width #f]
                                [spacing 4]))
      
      ;; 编辑按钮
      (define edit-btn (new button%
                            [parent button-panel]
                            [label "✎"]
                            [min-width 32]
                            [min-height 24]
                            [callback (lambda (btn evt)
                                        (if editing?
                                            (finish-edit)
                                            (start-edit)))]))
      
      ;; 删除按钮
      (define delete-btn (new button%
                              [parent button-panel]
                              [label "×"]
                              [min-width 32]
                              [min-height 24]
                              [callback (lambda (btn evt)
                                          (delete-task! current-task-id)
                                          (refresh-task-list!))]))

      ;; 开始编辑模式
      (define (start-edit)
        (set! editing? #t)
        (send edit-btn set-label "✓")
        (send task-text-msg show #f)
        (set! task-text-field 
              (new text-field%
                   [parent text-date-panel]
                   [label ""]
                   [init-value (task-text task-data)]
                   [stretchable-width #t]
                   [callback (lambda (tf evt)
                               (when (eq? (send evt get-event-type) 'text-field-enter)
                                 (finish-edit)))]))
        (send task-text-field focus))

      ;; 完成编辑
      (define (finish-edit)
        (when task-text-field
          (define new-text (send task-text-field get-value))
          (when (not (equal? (string-trim new-text) ""))
            (edit-task! current-task-id new-text)
            (send task-text-msg set-label new-text))
          (send task-text-field show #f)
          (send text-date-panel delete-child task-text-field)
          (set! task-text-field #f)
          (send task-text-msg show #t))
        (set! editing? #f)
        (send edit-btn set-label "✎"))

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

;; 列表过滤逻辑
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

;; 修复问题3:主窗口布局优化,确保跨平台一致性
(define frame (new frame%
                   [label "RReminder"]
                   [width 850]  ; 稍微增加宽度以适应新的布局
                   [height 650]
                   [style '(no-resize-border)]))

(define main-panel (new horizontal-panel% 
                        [parent frame] 
                        [spacing 8] 
                        [border 8])) ; 统一间距和边框

(define sidebar (new vertical-panel%
                     [parent main-panel]
                     [min-width 160]  ; 稍微增加侧边栏宽度
                     [spacing 8]
                     [border 8]
                     [stretchable-width #f]
                     [style '(border)]))

;; 修复问题3:统一按钮大小和间距
(define filter-panel (new horizontal-panel%
                          [parent sidebar]
                          [stretchable-height #f]
                          [spacing 4]))

(define today-btn (new button%
                       [parent filter-panel]
                       [label "今天"]
                       [min-width 65]
                       [min-height 32]
                       [callback (lambda (btn evt)
                                   (current-view "today")
                                   (send title-label set-label "今天")
                                   (refresh-task-list!))]))

(define completed-btn (new button%
                           [parent filter-panel]
                           [label "完成"]
                           [min-width 65]
                           [min-height 32]
                           [callback (lambda (btn evt)
                                       (current-view "completed")
                                       (send title-label set-label "完成")
                                       (refresh-task-list!))]))

(define my-lists-label (new message%
                             [parent sidebar]
                             [label "我的列表"]
                             [vert-margin 12]
                             [font (make-font #:weight 'bold #:family 'modern #:size 11)]))

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
                     [min-height 28]  ; 统一按钮高度
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
                          [label "+ 列表"]
                          [min-width 65]
                          [min-height 32]
                          [callback (lambda (btn evt)
                                      (show-add-list-dialog))]))

(define delete-list-btn (new button%
                             [parent list-management-panel]
                             [label "- 删除"]
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

(define content-panel (new vertical-panel% 
                           [parent main-panel] 
                           [spacing 8] 
                           [border 8] 
                           [style '(border)]))

(define title-panel (new horizontal-panel%
                         [parent content-panel]
                         [stretchable-height #f]))

(define title-label (new message%
                         [parent title-panel]
                         [label "工作"]
                         [vert-margin 12]
                         [font (make-font #:size 13 #:weight 'bold #:family 'modern)]))

(define task-scroll (new panel%
                        [parent content-panel]
                        [style '(vscroll)]))

(define task-list-panel (new vertical-panel%
                             [parent task-scroll]
                             [min-width 1]
                             [stretchable-height #t]
                             [stretchable-width #t]
                             [spacing 2])) ; 任务项之间的间距

(define bottom-panel (new horizontal-panel%
                          [parent content-panel]
                          [stretchable-height #f]))

(define add-task-btn (new button%
                          [parent bottom-panel]
                          [label "+ 新提醒事项"]
                          [min-height 32]
                          [callback (lambda (btn evt)
                                      (show-add-task-dialog))]))

(define (refresh-task-list!)
  (send task-list-panel change-children (lambda (children) '()))
  (define filtered-tasks (filter-tasks))
  (for ([task-data filtered-tasks])
    (when task-data
      (new task-item% [parent task-list-panel] [task-data task-data]))))

(define (show-add-task-dialog)
  (define dialog (new dialog%
                      [label "添加新任务"]
                      [parent frame]
                      [width 450]
                      [height 220]))
  (define dialog-panel (new vertical-panel% [parent dialog] [spacing 8] [border 12]))
  (new message% [parent dialog-panel] [label "任务描述:"])
  (define text-field (new text-field% [parent dialog-panel] [label ""] [init-value ""]))
  (new message% [parent dialog-panel] [label "截止日期 (YYYY-MM-DD, 可选):"])
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
                                    (if normalized-date-str
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