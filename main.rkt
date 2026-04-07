#lang racket/gui

(require csv-reading)
(require (only-in srfi/19 string->date date->string))

;;; configuration --------------------------------------------------------------

(define agenda-file "~/Documents/org/life/todos.org")
(define agenda-command "a")
(define agenda-span "day")

(define refresh-interval-seconds 60)

(define-values (frame-width frame-height) (values 200 200))

(define margin 8)
(define bullet-width 12)
(define date-height 24)
(define date-top-padding 8)
(define line-height 12)
(define extra-bullet-height 4)
(define scrollbar-width 12) ;; idk man seems about right

(define big-font (make-object font% 14 "Terminus" 'roman 'normal 'bold #f 'unsmoothed #t 'unaligned))
(define small-font (make-object font% 12 "Terminus" 'roman 'normal 'normal #f 'unsmoothed #t 'unaligned))

(define background-image "bro33-rotated.jpg")

;;; agenda reading -------------------------------------------------------------

(struct task [category ;; category of this item
              head ;; headline, without TODO kwd, TAGS and PRIORITY
              type ;; type of the agenda entry. one of: todo, tagsmatch, diary, deadline, scheduled, timestamp, closed, upcoming-deadline, past-scheduled, block
              todo ;; todo keyword, if any
              tags ;; all tags including inherited
              date ;; the relevant date
              time ;; the relevant time
              extra ;; string with extra planning info (deadline)
              priority-1 ;; priority letter if one was given
              priority-n ;; computed numerical priority
              agenda-day ;; day in the agenda where this is listed
              ]
  #:transparent)

(define (string->date/auto s)
  (string->date s "~Y-~m-~d"))

(define (list->task l)
  (match (map (λ (s) (if (string=? s "") #f s)) l)
    [(list category head type todo tags date time extra priority-1 priority-n agenda-day)
     (task category head type todo tags (string->date/auto date) time extra priority-1 priority-n (string->date/auto agenda-day))]
    [_ (error (format "not a valid entry: ~e" l))]))

(define (get-agenda command file span)
  ;; use emacs to get agenda as csv
  (define agenda-csv
    (with-output-to-string
      (lambda ()
        (system (format "emacs -Q -batch -eval '(org-batch-agenda-csv \"~a\" org-agenda-files (quote (\"~a\")) org-agenda-span (quote ~a))'"
                        command
                        file
                        span)))))
  (csv-map list->task agenda-csv))

;;; get the list of tasks ------------------------------------------------------

(define tasks (box #f))

(define (update-tasks!)
  (set-box! tasks (get-agenda agenda-command agenda-file agenda-span)))

(define (group-tasks-by-deadline tasks)
  (map (λ (l) (cons (task-extra (first l)) l))
       (group-by task-extra tasks)))

;;; drawing helpers ------------------------------------------------------------

;; scale the background to the desired size
(define background
  (let* ([background-unscaled (make-object bitmap% background-image)]
         [src-width (send background-unscaled get-width)]
         [scale (/ src-width frame-width)])
    ;; return a new bitmap from the same image with the correct backing scale
    (make-object bitmap% background-image 'unknown #f #f scale)))

;; find the first index where the string needs to wrap given the max width,
;; counting up from start
;; first-wrap-idx : string natural natural font -> natural
(define (first-wrap-idx text start max-width dc)
  (define-values (width _height _base _extra) (send dc get-text-extent (substring text 0 start)))
  (cond
    [(>= start (string-length text)) #f]
    [(> width max-width) start]
    [else (first-wrap-idx text (+ start 1) max-width dc)]))

;; split the text into a list of strings to wrap it at max-width
;; wrap-text : string natural font -> (list of string)
(define (wrap-text text max-width dc)
  (define wrap-idx (first-wrap-idx text 0 max-width dc))
  (if wrap-idx
      (cons (substring text 0 wrap-idx)
            (wrap-text (substring text wrap-idx) max-width dc))
      (list text)))

;; returns the y-position of the bottom of the agenda, because why not
(define (draw-agenda! dc)
  (define tasks/by-deadline (group-tasks-by-deadline (unbox tasks)))
  
  (for/fold ([date-group-y margin])
            ([deadline-and-tasks tasks/by-deadline])
    (match-define (cons deadline tasks) deadline-and-tasks)
    ;; draw date with a big font
    (send dc set-font big-font)
    (send dc draw-text
          (string-downcase #;(date->string date "~a, ~B ~e")
                           (string-normalize-spaces deadline))
          margin (+ date-group-y))
    ;; draw each of the bullets with a small font
    (send dc set-font small-font)
    (+ (for/fold ([bullet-y (+ date-group-y date-height)])
                 ([task tasks])
         (define text (task-head task))
         (define lines (wrap-text text (- frame-width (* 2 margin) bullet-width scrollbar-width) dc))
         (send dc draw-text ">" margin bullet-y)
         (for ([line lines]
               [i (in-naturals)])
           (send dc draw-text line (+ margin bullet-width) (+ bullet-y (* line-height i))))
         (+ bullet-y (* line-height (length lines)) extra-bullet-height))
       date-top-padding)))

;; draw to a dummy dc to get the agenda height
(define (get-agenda-height)
  (draw-agenda! (new record-dc%)))

(module+ main
  (update-tasks!)

  ;; special frame that stops current-timer on close
  (define current-timer (box #f))
  (define frame%/close-handler
    (class frame%
      (super-new)
      (define/augment (on-close)
        (define timer (unbox current-timer))
        (when timer
          (send timer stop)))))

  (define frame
    (new frame%/close-handler
         [label "agenda"]
         [width frame-width]
         [height frame-height]
         [border 0]
         [style '(no-caption)]
         [min-width frame-width]
         [min-height frame-height]
         [stretchable-width #f]
         [stretchable-height #f]))

  (define canvas
    (new canvas%
         [parent frame]
         [style '(vscroll)]
         [paint-callback
          (λ (_canvas dc)
            ;; tile the background vertically
            (define background-height (send background get-height))
            (define agenda-height (get-agenda-height))
            (let loop ([tile-y 0])
              (when (< tile-y agenda-height)
                (send dc draw-bitmap background 0 tile-y)
                (loop (+ tile-y background-height -1)))) ;; -1 to prevent gap
            ;; draw the agenda
            (send dc set-smoothing 'unsmoothed)
            (draw-agenda! dc))]))

  (define timer
    (new timer%
         [interval (* 1000 refresh-interval-seconds)]
         [notify-callback
          (λ ()
            (displayln "updating tasks...")
            (update-tasks!)
            (send canvas init-auto-scrollbars #f (get-agenda-height) 0 0)
            (send canvas refresh-now))]) )

  (set-box! current-timer timer) ;; so that the timer can be stopped on close

  (send canvas init-auto-scrollbars #f (get-agenda-height) 0 0)
  (send frame show #t))
