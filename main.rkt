#lang racket/gui

(require csv-reading)
(require racket/runtime-path)
(require (only-in srfi/19 string->date date->string))

;;; constants ------------------------------------------------------------------

(define-runtime-path default-background-image "backgrounds/bro33-rotated.jpg")

;; I don't think the user should change these? prob shouldn't be configurable
(define agenda-command "a")
(define agenda-span "day")

;;; configuration --------------------------------------------------------------

;; TODO move this all to an ini file or something
(define emacs-bin
  (find-executable-path "emacs"))
(define agenda-file
  (expand-user-path "~/Documents/org/life/todos.org"))
(define custom-background-image #f #;(expand-user-path "./backgrounds/bro255.jpg"))

(define refresh-interval-seconds 60)

(define-values (frame-width frame-height) (values 200 200))
(define margin 8)
(define date-height 24)
(define date-spacing 8)
(define line-height 12)
(define list-item-indent 12)
(define list-item-spacing 4)
(define bullet-string ">")
(define scrollbar-width 12) ;; idk man seems about right

(define big-font (make-object font% 14 "Terminus" 'default 'normal 'bold #f 'unsmoothed #t 'unaligned))
(define small-font (make-object font% 12 "Terminus" 'default 'normal 'normal #f 'unsmoothed #t 'unaligned))

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

(define path-to-existent-file/c
  (and/c path? file-exists?))

(define/contract (get-agenda emacs-bin command file span)
  (path-to-existent-file/c string? path-to-existent-file/c string? . -> . (listof task?))

  ;; surround a string with double quotes
  (define (add-double-quotes s)
    (format "\"~a\"" s))

  ;; emacs lisp command to run
  (define agenda-command
    (~a `(progn
          (setq org-agenda-skip-unavailable-files t)
          (org-batch-agenda-csv
           ,(add-double-quotes command)
           org-agenda-files (quote (,(add-double-quotes file)))
           org-agenda-span (quote ,span)))))

  ;; run the command
  (define agenda-csv
    (with-output-to-string
      (thunk (system (format "~a -Q -batch -eval '~a'"
                             emacs-bin
                             agenda-command)))))

  ;; map the returned sexp to a list of tasks
  (csv-map list->task agenda-csv))

;;; get the list of tasks ------------------------------------------------------

(define tasks (box #f))

(define (update-tasks!)
  (set-box! tasks (get-agenda emacs-bin agenda-command agenda-file agenda-span)))

(define (group-tasks-by-deadline tasks)
  (map (λ (l) (cons (task-extra (first l)) l))
       (group-by task-extra tasks)))

;;; drawing helpers ------------------------------------------------------------

;; scale the background to the desired size
(define/contract (make-background background-image-file-path)
  (path-to-existent-file/c . -> . (object/c))
  (let* ([background-unscaled (make-object bitmap% background-image-file-path)]
         [src-width (send background-unscaled get-width)]
         [scale (/ src-width frame-width)])
    ;; return a new bitmap from the same image with the correct backing scale
    (define background
      (make-object bitmap%
                   background-image-file-path
                   'unknown #f #f
                   scale))
    ;; verify loaded successfully
    (unless (send background ok?)
      (error (format "error encountered when trying to load background image: ~a" background-image-file-path)))
    background))

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

;; wrap text at a character width
(define (wrap-text-mono text max-char-width)
  (if (> (string-length text) max-char-width)
      (cons (substring text 0 max-char-width)
            (wrap-text-mono (substring text max-char-width) max-char-width))
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
         (define lines (wrap-text text (- frame-width (* 2 margin) list-item-indent scrollbar-width) dc))
         (send dc draw-text bullet-string margin bullet-y)
         (for ([line lines]
               [i (in-naturals)])
           (send dc draw-text line (+ margin list-item-indent) (+ bullet-y (* line-height i))))
         (+ bullet-y (* line-height (length lines)) list-item-spacing))
       date-spacing)))

;; draw to a dummy dc to get the agenda height
(define (get-agenda-height)
  (draw-agenda! (new record-dc%)))

(define (fail-with-dialog e)
  (define dialog
    (new dialog%
         [label "oops"]))
  (define hpane
    (new horizontal-pane% [parent dialog]))
  (new message% [label 'stop]
       [parent hpane])
  (new text-field%
       [label "oh no, stupid-agenda ran into an error..."]
       [font small-font]
       [init-value
        (string-join (wrap-text-mono (exn-message e) 60)
                     "\n")]
       [style '(multiple vertical-label)]
       [enabled false]
       [parent hpane])
  (send dialog show #t)
  (exit 1))

(module+ main
  (with-handlers ([exn:fail? fail-with-dialog])
    ;; default to the provided background image if no custom supplied
    (define background-image
      (or custom-background-image default-background-image))
    
    ;; setup checks
    (unless (and emacs-bin (file-exists? emacs-bin))
      (error (format "emacs binary does not exist at path ~a" emacs-bin)))
    (unless (and agenda-file (file-exists? agenda-file))
      (error (format "agenda file does not exist at path ~a" agenda-file)))
    (unless (and background-image (file-exists? background-image))
      (error (format "background image does not exist at path ~a" background-image)))
    
    ;; update the list of tasks
    (update-tasks!)

    (define frame
      (new frame%
           [label "agenda"]
           [width frame-width]
           [height frame-height]
           [border 0]
           [style '(no-caption)]
           [min-width frame-width]
           [min-height frame-height]
           [stretchable-width #f]
           [stretchable-height #f]))
    
    ;; we already checked the file exists with a nicer message, but it might also
    ;; fail for another reason (e.g., wrong file format)
    (define background (make-background background-image))
    
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
              (send canvas refresh-now))]))
    
    (send canvas init-auto-scrollbars #f (get-agenda-height) 0 0)
    (send frame show #t)
    (send timer stop)))
