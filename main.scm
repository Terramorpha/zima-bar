#!/usr/bin/env -S guile
!#

(define-module (main)
  #:use-module (rnrs bytevectors)
  #:use-module (ice-9 threads)
  #:use-module (ice-9 suspendable-ports)
  #:use-module (web client)
  #:use-module (ice-9 binary-ports)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 match)
  #:use-module (ice-9 popen)
  #:use-module (json)
  #:use-module (srfi srfi-41)

  #:use-module (oop goops)
  #:use-module (g-golf)

  #:duplicates (merge-generics
		        replace
		        warn-override-core
		        warn
		        last))

(install-suspendable-ports!)

;; Require the typelibs first
(g-irepository-require "GLib")
(g-irepository-require "Gio")
(g-irepository-require "Gtk" #:version "4.0")
(g-irepository-require "Gdk" #:version "4.0")
(g-irepository-require "Gtk4LayerShell" #:version "1.0")

;; Import Gio classes
;; (for-each (lambda (name)
;;             (gi-import-by-name "Gio" name))
;;           '())

;; Import Gtk classes
(for-each (lambda (name)
            (gi-import-by-name "Gtk" name))
          '("Application"
            "ApplicationWindow"
            "Box"
            "Label"
            "Button"
            "CssProvider"
            "StyleContext"
            "Picture"
            "GestureClick"
            "GestureDrag"
            "Popover"
            "Scale"
            "Calendar"
            "ProgressBar"
            "EventControllerMotion"))

;; Import Gdk classes
(for-each (lambda (name)
            (gi-import-by-name "Gdk" name))
          '("Display"))

;; Import gtk-layer-shell
(gi-import "Gtk4LayerShell" #:version "1.0")

(define style
  "window {
                   font-family: monospace;
                   font-size: 12px;
                 }
                 button {
                   background: none;
                   border: none;
                   box-shadow: none;
                   padding: 0px 0px;
                   margin: 0px 2px;
                   min-height: 0;
                   min-width: 0;
                 }
                 button:hover {
                   background-color: rgba(255, 255, 255, 0.1);
                 }
                 button:active {
                   background-color: rgba(255, 255, 255, 0.2);
                 }")



(define (setup-css)
  (let ((provider (make <gtk-css-provider>))
        (display (gdk-display-get-default)))

    (load-from-data provider
                    style
                    -1)
    (gtk-style-context-add-provider-for-display display provider 600)))

(define* (click-popover! #:key parent child)
  (define popover
    (make <gtk-popover>))

  (set-parent popover parent)

  (set-child popover child)

  (define gesture
    (make <gtk-gesture-click>
      #:button 1))

  (add-controller parent gesture)

  (connect gesture 'pressed
           (lambda _
             (popup popover))))

(define (nonblockify! port)
  (let ((flags (fcntl port F_GETFL)))
    (fcntl port F_SETFL (logior O_NONBLOCK flags))
    port))

(define (hyprland-socket-path)
  (string-append (getenv "XDG_RUNTIME_DIR")
                 "/hypr/"
                 (getenv "HYPRLAND_INSTANCE_SIGNATURE")
                 "/.socket2.sock"))

(define (connect-to-unix-socket path)
  (let ((sock (socket PF_UNIX SOCK_STREAM 0)))
    (connect sock AF_UNIX path)
    sock))

(define hyprland-port
  (nonblockify!
   (connect-to-unix-socket (hyprland-socket-path))))

(define hyprland-socket-hook (make-hook 2))

(define (hyprland-handle-events)
  (let ((line (read-line hyprland-port)))
    (unless (eof-object? line)
      (let* ((parts (string-split line #\>))
             (type (string->symbol (car parts)))
             (rest (string-join (cddr parts) ">")))
        (run-hook
         hyprland-socket-hook
         type
         rest))
      (hyprland-handle-events))))

(define glib-prompt-tag (make-prompt-tag))

(define (glib-read-waiter port)
  (abort-to-prompt glib-prompt-tag (list 'read port)))

(define (with-glib-io thunk)
  (call-with-prompt glib-prompt-tag
    (lambda ()
     (parameterize ((current-read-waiter glib-read-waiter))
       (thunk)))
    (lambda (kont msg)
      (match msg
        (('read port)
         (let* ((fd (port->fdes port))
                (channel (g-io-channel-unix-new fd))
                (source (g-io-create-watch channel '(in))))

           (g-source-set-closure
            source
            (!g-closure
             (make <closure>
               #:function
               (lambda (fd cond)
                 ;; When the file descriptor is ready to be read from, we resume
                 ;; the callback (again, in the same kind of handling block).
                 (g-source-destroy source)
                 (with-glib-io kont))
               #:return-type 'boolean
               #:param-types '(int ;; fd
                               uint32))))
           (g-source-attach source #f)))
        (_ (error "don't know what that is" msg))))))

(define (set-box-children! box new-children)
  ;; Remove all existing children
  (let loop ()
    (let ((child (get-first-child box)))
      (when child
        (remove box child)
        (loop))))
  ;; Add new children
  (for-each (lambda (child)
              (append box child))
            new-children))

(define (hyprctl-get-workspace-states)
  (let* ((pipe (open-input-pipe "hyprctl workspaces -j"))
         (val (json->scm pipe)))
    (close-pipe pipe)
    val))

(define (hyprctl-get-active-workspace)
  (let* ((pipe (open-input-pipe "hyprctl activeworkspace -j"))
         (val (json->scm pipe)))
    (close-pipe pipe)
    val))

(define* (workspaces)
  (define box (make <gtk-box>
                #:orientation 'horizontal
                #:spacing 5))

  (define (make-children active states)
    "Create an up to date version of the list of labels to display in the box."
    (define active-id (assoc-ref active "id"))
    (define (workspace-state->widget state)
      (define id (assoc-ref state "id"))
      (define active? (eq? id active-id))
      (define (bold s)
        (string-append "<b>" s "</b>"))
      (define label (make <gtk-label>
                      #:use-markup #t
                      #:label (if active?
                                  (bold
                                   (assoc-ref state "name"))
                                  (assoc-ref state "name"))))
      (define button (make <gtk-button>
                       #:child label))

      (connect button 'clicked
               (lambda (btn)
                 (system* "hyprctl" "dispatch" "workspace" (number->string id))))

      button)
    (let ((sorted (sort (vector->list states)
                        (lambda (a b)
                          (< (assoc-ref a "id")
                             (assoc-ref b "id"))))))
      (map workspace-state->widget sorted)))

  (add-hook! hyprland-socket-hook
             (match-lambda*
               (('workspacev2 rest)
                (let ((states (hyprctl-get-workspace-states))
                      (active (hyprctl-get-active-workspace)))

                  (set-box-children! box (make-children active states)))
                #f)
               (_ #f)))
  (set-box-children! box
                     (make-children
                      (hyprctl-get-active-workspace)
                      (hyprctl-get-workspace-states)))

  box)

(define* (window-name)
  (let ((label (make <gtk-label>)))
    (add-hook!
     hyprland-socket-hook
     (lambda (type rest)
       (match type
         ('activewindow
          (match (string-split rest #\,)
            ((short long . long*)
             (set-label label (string-join (cons long long*) ",")))))
         (_ #f))))
    label))


(define pactl-port
  (nonblockify!
   (open-input-pipe "pactl subscribe")))

(define pactl-hook (make-hook 1))

(define (pactl-handle-events)
  (let* ((line (read-line pactl-port))
         (parts (string-split line #\'))
         (type (string->symbol (cadr parts))))
    (unless (eof-object? line)
      (run-hook pactl-hook type)
      (pactl-handle-events))))

(define* (debounce f #:key (delay-ms 100))
  (let ((pending-timeout #f)
        (pending-args #f))
    (lambda args
      ;; Cancel any pending call
      (when pending-timeout
        (g-source-remove pending-timeout)
        (set! pending-timeout #f))

      ;; Store the arguments
      (set! pending-args args)

      ;; Schedule the actual call
      (set! pending-timeout
        (g-timeout-add delay-ms
                       (lambda ()
                         (apply f pending-args)
                         (set! pending-timeout #f)
                         #f))))))

(define (set-volume vol)
  (system* "wpctl" "set-volume" "@DEFAULT_AUDIO_SINK@" (format #f "~,2f" vol)))

(define (get-volume)
    (let* ((pipe (open-input-pipe "wpctl get-volume @DEFAULT_AUDIO_SINK@"))
           (output (read-line pipe)))
      (close-pipe pipe)

      (if (eof-object? output)
          #f
          (let ((parts (string-split output #\space)))
            (string->number (cadr parts))))))

(define volume-hook (make-hook 1))
(add-hook! pactl-hook
           (lambda (type)
             (when (eq? type 'change)
               (let ((volume (get-volume)))
                 (run-hook volume-hook volume)))))

(define* (volume)
  (define init-volume (get-volume))
  (define volume-label
    (make <gtk-label>
      #:label (number->string init-volume)))


  (define volume-slider
    (make <gtk-scale>
      #:orientation 'vertical
      #:inverted #t
      #:draw-value #f
      #:height-request 200))

  ;; Track whether user is currently dragging
  (define user-dragging? #f)

  ;; Create a gesture to detect drag state
  (define drag-gesture
    (make <gtk-gesture-drag>))

  (define debounced-set-volume (debounce set-volume))

  (add-controller volume-slider drag-gesture)

  (connect drag-gesture 'drag-begin
           (lambda (gesture x y)
             (set! user-dragging? #t)))

  (connect drag-gesture 'drag-end
           (lambda (gesture x y)
             (set! user-dragging? #f)))

  (set-range volume-slider 0.0 1.0)
  (set-value volume-slider init-volume)
  ;; (set-increments volume-slider 0.01 0.1)

  (connect volume-slider 'value-changed
           (lambda (scale)
             (debounced-set-volume (get-value scale))))

  (click-popover!
   #:parent volume-label
   #:child volume-slider)

  (add-hook! volume-hook
             (lambda (volume)
               (set-label volume-label (format #f "~,2f" volume))
               (unless user-dragging?
                (set-value volume-slider volume))))

  volume-label)

(define (readonly-calendar)
  (make <gtk-calendar>
    #:can-target #f))

(define* (clock)
  (define (format-date)
    (strftime "%Y-%m-%d" (localtime (current-time))))
  (define (format-time)
    (strftime "%H:%M:%S" (localtime (current-time))))

  (define time-label
    (make <gtk-label>
      #:label (format-time)))

  (define date-label
    (make <gtk-label>
      #:label (format-date)))

  (define clock-box
    (make <gtk-box>
      #:orientation 'horizontal
      #:spacing 10))

  (append clock-box date-label)
  (append clock-box time-label)

  (click-popover!
   #:parent clock-box
   #:child (readonly-calendar))

  (g-timeout-add 1000
                 (lambda ()
                   (set-label time-label (format-time))
                   #t))
  (g-timeout-add (* 24 60 60 1000)
                 (lambda ()
                   (set-label date-label (format-date))
                   #t))                 ; Return #t to keep timer running
  clock-box)

(define (playerctl command)
  (system* "playerctl" command))

(define playerctl-cmd
  "playerctl metadata --follow --format '{\"status\":\"{{status}}\",\"artist\":\"{{artist}}\",\"title\":\"{{title}}\",\"album\":\"{{album}}\",\"position\":{{position}},\"length\":{{mpris:length}}, \"artUrl\":\"{{mpris:artUrl}}\"}'")

(define playerctl-port
  (nonblockify! (open-input-pipe
                 playerctl-cmd)))

(define playerctl-hook (make-hook 1))

(define (playerctl-handle-events)
  (let ((line (read-line playerctl-port)))
    (if (string=? (string-trim-both line) "")
        (playerctl-handle-events)
        (let ((obj (json-string->scm line)))
          (run-hook
           playerctl-hook
           obj)
          (playerctl-handle-events)))))

;; (add-hook! playerctl-hook
;;            (lambda (obj) (format #t "playerctl: ~a\n" obj)))

(define file-cache (make-hash-table))

(define (download-file url path)
  (system* "curl" "-o" path url))

(define* (get-scale-image url #:key size)
  (or (hash-ref file-cache url #f)
      (let* ((t (current-time))
             (path (string-append (getenv "HOME") "/.cache/zima-bar/" (number->string t))))
        (unless (file-exists? (dirname path))
          (mkdir (dirname path)))
        (download-file url path)
        (let* ((pixbuf (gdk-pixbuf-new-from-file-at-size path size size))
               (texture (gdk-texture-new-for-pixbuf pixbuf)))
          (hash-set! file-cache url texture)
          texture))))

(define* (player-card)
  (define big-box (make <gtk-box>
                    #:orientation 'vertical
                    #:spacing 10))
  (set-size-request big-box 200 -1)


  (define album-art
    (make <gtk-picture>
      #:can-shrink? #f
      #:content-fit 'scale-down))
  (append big-box album-art)

  (define media-label
    (make <gtk-label>
      #:label "No media playing"
      #:ellipsize 'end
      #:max-width-chars 20))
  (append big-box media-label)

  (define buttons-box
    (make <gtk-box>
      #:orientation 'horizontal
      #:spacing 5
      #:halign 'center
      #:homogeneous #t))
  (append big-box buttons-box)

  (define prev-button (make <gtk-button> #:icon-name "media-skip-backward"))
  (define play-pause-button (make <gtk-button> #:icon-name "media-playback-start"))

  (define next-button (make <gtk-button> #:icon-name "media-skip-forward"))

  (connect prev-button 'clicked
           (lambda (button)
             (playerctl "previous")))  ; or whatever your previous function is
  (connect play-pause-button 'clicked
           (lambda (button)
             (playerctl "play-pause")))  ; or whatever your play-pause function is
  (connect next-button 'clicked
           (lambda (button)
             (playerctl "next")))

  (append buttons-box prev-button)
  (append buttons-box play-pause-button)
  (append buttons-box next-button)

  (define progress (make <gtk-progress-bar>))
  (append big-box progress)

  (add-hook! playerctl-hook
             (lambda (metadata)
               (let ((artist (assoc-ref metadata "artist"))
                     (title (assoc-ref metadata "title"))
                     (art-url (assoc-ref metadata "artUrl"))
                     (position (assoc-ref metadata "position"))
                     (length (assoc-ref metadata "length"))
                     (status (assoc-ref metadata "status")))
                 ;; Update label
                 (when (and artist title)
                   (set-label media-label
                              (format #f "~a - ~a"
                                      (if (vector? artist)
                                          (vector-ref artist 0)
                                          artist)
                                      title)))
                 ;; Update album art
                 (when art-url
                   (let ((texture (get-scale-image art-url #:size 200)))
                     (set-paintable album-art texture)))

                 (when (and position length)
                   (let ((song-progress (/ position length)))
                     (set-fraction progress song-progress)))

                 (match status
                   ("Playing" (set-icon-name play-pause-button "media-playback-pause"))
                   ("Paused" (set-icon-name play-pause-button "media-playback-start"))
                   (_ #f))
                 )))

  big-box)

(define* (player)
  (define title-label
    (make <gtk-label>))

  (click-popover!
   #:parent title-label
   #:child (player-card))

  (add-hook! playerctl-hook
             (lambda (event)
               (let ((title (assoc-ref event "title"))
                     (artist (assoc-ref event "artist")))
                 (if (and (string? title) (string? artist))
                     (set-label title-label
                                (format #f "~a - ~a" title artist))))))

  title-label)

(define* (bar-contents)
  (define main-box
    (make <gtk-box>
      #:orientation 'horizontal
      #:spacing 0))
  (define left-box
    (make <gtk-box>
      #:orientation 'horizontal
      #:spacing 10
      #:halign 'start
      #:margin-start 10))
  (define center-box
    (make <gtk-box>
      #:orientation 'horizontal
      #:spacing 10
      #:halign 'center
      #:hexpand #t))  ; This expands to fill space
  (define right-box
    (make <gtk-box>
      #:orientation 'horizontal
      #:spacing 10
      #:halign 'end
      #:margin-end 10))

  (append left-box (workspaces))

  ;; (append center-box (window-name))
  (append center-box (player))

  (append right-box (volume))
  (append right-box (clock))


  (append main-box
          left-box)
  (append main-box
          center-box)
  (append main-box
          right-box)

  main-box)



(define (activated app)
  (let* ((p (promise)))

    p))

(define (main)
  (define app
    (make <gtk-application>
      #:application-id "org.terramorpha.StatusBar"))

  (connect app 'activate
           (lambda (_)
             (define window
               (make <gtk-application-window>
                 #:application app))
             (define height 20)


             ;; Initialize layer shell BEFORE setting any properties
             (gtk-layer-init-for-window window)

             ;; Set layer
             (gtk-layer-set-layer window 'bottom)

             ;; Anchor to edges
             (gtk-layer-set-anchor window 'top #t)
             (gtk-layer-set-anchor window 'left #t)
             (gtk-layer-set-anchor window 'right #t)

             ;; IMPORTANT: Set the size/margin for the anchored edge
             (gtk-layer-set-margin window 'top 0)
             (gtk-layer-set-margin window 'left 0)
             (gtk-layer-set-margin window 'right 0)

             ;; Set exclusive zone (reserves space so other windows don't overlap)
             (gtk-layer-set-exclusive-zone window height)

             ;; Set keyboard mode
             (gtk-layer-set-keyboard-mode window 'none)

             ;; Set namespace
             (gtk-layer-set-namespace window "status-bar")

             ;; Set the actual height
             (set-default-size window -1 height)

             ;; Build the status bar
             (set-child window (bar-contents))

             (setup-css)

             (with-glib-io
              hyprland-handle-events)

             (with-glib-io
              pactl-handle-events)

             (with-glib-io
              playerctl-handle-events)

             ;; zero values returned
             (show window)
             ;; ca a l'air que j'ai besoin de ça pour pas que ça crash
             #f))

  (exit (run app (command-line))))

(main)
