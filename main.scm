#!/usr/bin/env -S guile
!#

(define-module (main)
  #:use-module (rnrs bytevectors)
  #:use-module (ice-9 threads)
  #:use-module (ice-9 suspendable-ports)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 match)
  #:use-module (ice-9 popen)
  #:use-module (json)

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
            "StyleContext"))

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

(define (get-volume)
    (let* ((pipe (open-input-pipe "wpctl get-volume @DEFAULT_AUDIO_SINK@"))
           (output (read-line pipe)))
      (close-pipe pipe)

      (if (eof-object? output)
          #f
          (let ((parts (string-split output #\space)))
            (string->number (cadr parts))))))

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

(define* (volume)
  (define volume-label
    (make <gtk-label>
      #:label (number->string (get-volume))))

  (add-hook! pactl-hook
             (match-lambda
               ('change
                (set-label volume-label (format #f "~,2f" (get-volume))))
               (_ #f)))

  volume-label)

(define* (clock)
  (define (format-clock)
    (strftime "%Y-%m-%d  %H:%M:%S" (localtime (current-time))))
  (define clock-label
    (make <gtk-label>
      #:label (format-clock)))
  (g-timeout-add 1000
                 (lambda ()
                   (set-label clock-label (format-clock))
                   #t))  ; Return #t to keep timer running
  clock-label)

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

  (append center-box (window-name))

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

             ;; zero values returned
             (show window)
             ;; ca a l'air que j'ai besoin de ça pour pas que ça crash
             #f))

  (exit (run app (command-line))))

(main)


